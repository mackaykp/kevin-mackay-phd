# Session management: create, join, participants.
# Store is persisted to a file so all clients (and all R processes/workers) see the same state.

`%||%` <- function(x, y) if (is.null(x)) y else x

.sessions <- new.env(parent = emptyenv())
.sessions$store <- list()

#' Path to the shared session store file (same for all processes so everyone sees the same state).
#' Default avoids the project directory: OneDrive/sync folders often deny R read/write and break
#' multi-tab sync. Override with options(belay_buddy_store_path = "/path/to/session_store.rds").
store_path <- function() {
  opt <- getOption("belay_buddy_store_path", default = NULL)
  if (is.character(opt) && length(opt) == 1L && nzchar(trimws(opt[1L]))) {
    return(trimws(opt[1L]))
  }
  if (.Platform$OS.type == "windows") {
    la <- Sys.getenv("LOCALAPPDATA", unset = "")
    if (nzchar(la)) {
      d <- file.path(la, "BelayBuddy")
      dir.create(d, recursive = TRUE, showWarnings = FALSE)
      return(file.path(d, "session_store.rds"))
    }
  }
  d <- path.expand("~/.local/share/BelayBuddy")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  if (file.exists(path.expand("~"))) {
    return(file.path(d, "session_store.rds"))
  }
  file.path(getwd(), "session_store.rds")
}

#' Load store from file (so we see updates made by other clients/processes).
load_store <- function() {
  path <- store_path()
  if (!file.exists(path)) return(list())
  for (attempt in seq_len(8L)) {
    x <- tryCatch(readRDS(path), error = function(e) NULL)
    if (is.list(x) && (length(x) >= 0L)) return(x)
    Sys.sleep(0.04 * attempt)
  }
  warning("Belay Buddy: could not read session store (try closing other apps using the file).\n  path: ",
          path, call. = FALSE)
  list()
}

#' Save store to file (atomic on Windows: avoids corrupt reads while writing).
#' Returns TRUE if written. On FALSE, other tabs will not see updates.
save_store <- function(store) {
  path <- store_path()
  d <- dirname(path)
  if (!dir.create(d, recursive = TRUE, showWarnings = FALSE) && !dir.exists(d)) {
    warning("Belay Buddy: cannot create directory: ", d, call. = FALSE)
    return(FALSE)
  }
  tmp <- file.path(d, ".session_store_writing.rds")
  ok <- tryCatch(
    {
      saveRDS(store, tmp)
      if (file.exists(path)) {
        un <- suppressWarnings(unlink(path, force = TRUE))
        if (un != 0L || file.exists(path)) {
          if (!file.copy(tmp, path, overwrite = TRUE)) stop("session file in use or locked; close other copies of the app")
          unlink(tmp, force = TRUE)
          return(TRUE)
        }
      }
      if (!file.rename(tmp, path)) {
        if (!file.copy(tmp, path, overwrite = TRUE)) stop("could not write session file")
        unlink(tmp, force = TRUE)
      }
      TRUE
    },
    error = function(e) {
      warning(
        "Belay Buddy: save_store failed — category/round changes will NOT sync across tabs.\n  ",
        conditionMessage(e), "\n  path: ", path,
        call. = FALSE
      )
      unlink(tmp, force = TRUE)
      FALSE
    }
  )
  isTRUE(ok)
}

#' App calls bb_register_shared_stamp(reactiveValues(stamp=0)) so every browser session re-renders together.
.sessions$bb_stamp_rv <- NULL
bb_register_shared_stamp <- function(rv) {
  .sessions$bb_stamp_rv <- rv
}

#' Every mutation bumps stamp + optional file (multi-process still uses disk).
bump_sync_stamp <- function() {
  .sessions$sync_gen <- (.sessions$sync_gen %||% 0L) + 1L
  rv <- .sessions$bb_stamp_rv
  if (!is.null(rv)) {
    # `later::later()` runs outside Shiny's reactive consumer; isolate avoids
    # "Can't access reactive value outside of reactive consumer" errors.
    rv$stamp <- (shiny::isolate(rv$stamp) %||% 0L) + 1L
  }
  f <- file.path(dirname(store_path()), "belay_buddy_sync.txt")
  tryCatch(
    {
      dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
      writeLines(format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"), f)
    },
    error = function(e) NULL
  )
}

sync_stamp_mtime <- function() {
  f <- file.path(dirname(store_path()), "belay_buddy_sync.txt")
  if (!file.exists(f)) return("0")
  paste(as.numeric(file.mtime(f)), file.size(f), sep = ":")
}

session_poll_key <- function() {
  paste(.sessions$sync_gen %||% 0L, sync_stamp_mtime(), sep = ":")
}

#' Generate a short random code (room code or participant id)
#' @param n length
#' @param chars character set
rand_code <- function(n = 5L, chars = c(letters, LETTERS, 0:9)) {
  paste0(sample(chars, n, replace = TRUE), collapse = "")
}

#' Create a new session. Returns the room code.
create_session <- function() {
  disk <- load_store()
  for (nm in names(disk)) {
    if (is.null(.sessions$store[[nm]])) .sessions$store[[nm]] <- disk[[nm]]
  }
  code <- rand_code(5L)
  while (code %in% names(.sessions$store)) code <- rand_code(5L)
  .sessions$store[[code]] <- list(
    room_code = code,
    created_at = Sys.time(),
    participants = list(),
    round = 1L,
    last_action = list(),
    pair_history = list(),
    started = FALSE,  # pairings shown only after someone presses Start
    roster_dirty = FALSE,  # roster changed; apply starting next round
    share_needed = FALSE,  # host should download/share updated schedule
    share_from_round = 1L, # round number the new schedule should start at
    roster_events = list(), # chronological list of add/remove events (for export)
    # Upcoming-round burst buffer (host UX: always show next 5 rounds).
    burst_size = 5L,
    burst_from_round = 1L,        # round number of burst_pairings[[1]]
    burst_pairings = NULL,        # list[[k]] = pairings list for round (burst_from_round + k - 1)
    burst_sig = "",               # signature to know when to rebuild
    # Legacy schedule fields (kept for backwards compatibility; UI now uses bursts).
    schedule_active = FALSE,
    schedule_rounds = 5L,
    schedule_pairings = NULL,
    schedule_state_upto = 0L,
    schedule_precompute_in_progress = FALSE
  )
  if (!save_store(.sessions$store)) warning("Create session may not persist.", call. = FALSE)
  bump_sync_stamp()
  code
}

#' Start the session (enables round 1 and pairings). Returns TRUE on success.
start_session <- function(room_code) {
  s <- get_session(room_code)
  if (is.null(s)) return(FALSE)
  s$started <- TRUE
  s$schedule_active <- FALSE
  # Reset round state on (re)start.
  s$round <- as.integer(s$round %||% 1L)
  if (is.na(s$round) || s$round < 1L) s$round <- 1L
  s$last_action <- s$last_action %||% list()
  s$pair_history <- s$pair_history %||% list()
  # Clear any prior burst/schedule buffers.
  s$burst_pairings <- NULL
  s$burst_from_round <- s$round
  s$burst_sig <- ""
  s$schedule_pairings <- NULL
  s$schedule_state_upto <- 0L
  .sessions$store[[room_code]] <- s
  save_store(.sessions$store)
  bump_sync_stamp()
  TRUE
}

#' Get session: shared in-memory copy for this R process (all tabs). Disk only fills missing rooms.
#' Previously every read reloaded the whole file and overwrote fresh updates when save lagged/failed.
get_session <- function(room_code) {
  if (!is.null(.sessions$store[[room_code]])) {
    return(.sessions$store[[room_code]])
  }
  disk <- load_store()
  if (!is.null(disk[[room_code]])) {
    .sessions$store[[room_code]] <- disk[[room_code]]
    return(.sessions$store[[room_code]])
  }
  NULL
}

#' Add a participant to a session. Returns participant id.
#' @param room_code session room code
#' @param name display name
#' @param category "boulder" | "top_rope" | "lead"
#' TRUE if another participant already uses this display name (case-insensitive trim).
session_display_name_taken <- function(room_code, name) {
  s <- get_session(room_code)
  if (is.null(s)) return(FALSE)
  target <- tolower(trimws(as.character(name %||% "")))
  if (!nzchar(target)) return(FALSE)
  for (pid in names(s$participants)) {
    p <- s$participants[[pid]]
    if (isFALSE(p$active)) next
    nm <- p$name %||% ""
    if (tolower(trimws(as.character(nm))) == target) return(TRUE)
  }
  FALSE
}

join_session <- function(room_code, name, category = "top_rope") {
  s <- get_session(room_code)
  if (is.null(s)) return(NULL)
  pid <- rand_code(8L)
  while (pid %in% names(s$participants)) pid <- rand_code(8L)
  s$participants[[pid]] <- list(
    id = pid,
    name = name,
    category = category,
    active = TRUE,
    joined_at = Sys.time()
  )
  if (isTRUE(s$started %||% FALSE)) {
    eff <- as.integer(s$round %||% 1L) + 1L
    s$roster_events <- c(s$roster_events %||% list(), list(list(
      type = "add",
      name = as.character(name),
      participant_id = pid,
      effective_round = eff,
      ts = Sys.time()
    )))
  }
  # Roster change applies starting next round.
  s$roster_dirty <- TRUE
  # If the session is not started, keep buffers empty/invalid; if started, we keep the
  # current round stable and rebuild after advancing.
  if (!isTRUE(s$started %||% FALSE)) {
    s$burst_pairings <- NULL
    s$burst_sig <- ""
  }
  s$schedule_pairings <- NULL
  s$schedule_state_upto <- 0L
  .sessions$store[[room_code]] <- s
  save_store(.sessions$store)
  bump_sync_stamp()
  pid
}

#' Update a participant's category
update_category <- function(room_code, participant_id, category) {
  s <- get_session(room_code)
  if (is.null(s) || is.null(s$participants[[participant_id]])) return(FALSE)
  s$participants[[participant_id]]$category <- category
  .sessions$store[[room_code]] <- s
  save_store(.sessions$store)
  bump_sync_stamp()
  TRUE
}

#' Remove a participant from the session
leave_session <- function(room_code, participant_id) {
  s <- get_session(room_code)
  if (is.null(s)) return(FALSE)
  if (!is.null(s$participants[[participant_id]])) {
    nm <- as.character(s$participants[[participant_id]]$name %||% participant_id)
    s$participants[[participant_id]]$active <- FALSE
    s$participants[[participant_id]]$left_at <- Sys.time()
    if (isTRUE(s$started %||% FALSE)) {
      eff <- as.integer(s$round %||% 1L) + 1L
      s$roster_events <- c(s$roster_events %||% list(), list(list(
        type = "remove",
        name = nm,
        participant_id = participant_id,
        effective_round = eff,
        ts = Sys.time()
      )))
    }
  }
  s$last_action[[participant_id]] <- NULL
  # Roster change applies starting next round.
  s$roster_dirty <- TRUE
  if (!isTRUE(s$started %||% FALSE)) {
    s$burst_pairings <- NULL
    s$burst_sig <- ""
  }
  s$schedule_pairings <- NULL
  s$schedule_state_upto <- 0L
  .sessions$store[[room_code]] <- s
  save_store(.sessions$store)
  bump_sync_stamp()
  TRUE
}

#' Return session state for display (list of participants, round, pairings)
#' Pairings are computed by pairing.R
session_state <- function(room_code) {
  s <- get_session(room_code)
  if (is.null(s)) return(NULL)
  started <- isTRUE(s$started %||% FALSE)
  burst_pairings <- s$burst_pairings %||% NULL
  burst_size <- as.integer(s$burst_size %||% 5L)
  if (is.na(burst_size) || burst_size < 1L) burst_size <- 5L
  list(
    room_code = s$room_code,
    participants = s$participants,
    round = s$round,
    last_action = s$last_action,
    pair_history = s$pair_history %||% list(),
    started = started,
    roster_dirty = s$roster_dirty %||% FALSE,
    share_needed = s$share_needed %||% FALSE,
    share_from_round = s$share_from_round %||% (s$burst_from_round %||% (s$round %||% 1L)),
    roster_events = s$roster_events %||% list(),
    burst_size = burst_size,
    burst_from_round = s$burst_from_round %||% (s$round %||% 1L),
    burst_pairings = burst_pairings,
    # UI compatibility: present the burst as the "schedule" (5-round burst).
    schedule_active = started && !is.null(burst_pairings) && length(burst_pairings) >= 1L,
    schedule_rounds = burst_size,
    schedule_pairings = burst_pairings,
    schedule_state_upto = s$schedule_state_upto %||% 0L,
    schedule_precompute_in_progress = s$schedule_precompute_in_progress %||% FALSE
  )
}

#' Clear the share-needed flag (host has downloaded/shared the updated schedule).
clear_share_needed <- function(room_code) {
  s <- get_session(room_code)
  if (is.null(s)) return(FALSE)
  s$share_needed <- FALSE
  .sessions$store[[room_code]] <- s
  save_store(.sessions$store)
  bump_sync_stamp()
  TRUE
}

#' Signature for burst rebuild: round + roster + history length + burst size.
.burst_signature <- function(s, ids, burst_size) {
  paste0(
    as.integer(s$round %||% 1L), "|",
    as.integer(length(s$pair_history %||% list())), "|",
    paste(sort(as.character(ids)), collapse = ","), "|",
    as.integer(burst_size)
  )
}

#' Build or refresh the upcoming-round burst buffer (default: 5 rounds).
#' Returns TRUE if burst is available (or FALSE if session missing / not started / too few people).
ensure_burst <- function(room_code, burst_size = 5L) {
  s <- get_session(room_code)
  if (is.null(s)) return(FALSE)
  if (!isTRUE(s$started %||% FALSE)) return(FALSE)
  burst_size <- as.integer(burst_size %||% (s$burst_size %||% 5L))
  if (is.na(burst_size) || burst_size < 1L) burst_size <- 5L

  ids <- top_rope_ids(s$participants %||% list())
  if (length(ids) < 2L) return(FALSE)

  sig <- .burst_signature(s, ids, burst_size)
  # If roster changed mid-round, keep the current burst (so Round X stays stable).
  # The next `advance_round_from_burst()` clears the burst, and then we rebuild using the new roster.
  if (isTRUE(s$roster_dirty %||% FALSE) &&
      !is.null(s$burst_pairings) &&
      identical(s$burst_from_round %||% NA_integer_, s$round %||% NA_integer_) &&
      length(s$burst_pairings) >= 1L) {
    return(TRUE)
  }
  if (!is.null(s$burst_pairings) &&
      is.list(s$burst_pairings) &&
      length(s$burst_pairings) == burst_size &&
      identical(s$burst_from_round %||% NA_integer_, s$round %||% NA_integer_) &&
      identical(s$burst_sig %||% "", sig)) {
    return(TRUE)
  }

  sim <- list(
    round = as.integer(s$round %||% 1L),
    last_action = s$last_action %||% list(),
    pair_history = s$pair_history %||% list(),
    participants = s$participants %||% list()
  )
  out <- vector("list", burst_size)
  for (k in seq_len(burst_size)) {
    out[[k]] <- compute_pairings(ids, sim$last_action, sim$round, sim$participants, sim$pair_history)
    sim <- apply_pairings_to_state(sim, out[[k]])
  }

  s$burst_size <- burst_size
  s$burst_from_round <- as.integer(s$round %||% 1L)
  s$burst_pairings <- out
  s$burst_sig <- sig
  s$roster_dirty <- FALSE
  .sessions$store[[room_code]] <- s
  save_store(.sessions$store)
  bump_sync_stamp()
  TRUE
}

#' Advance one round using the current burst round 1, then invalidate burst so it rebuilds.
advance_round_from_burst <- function(room_code) {
  s <- get_session(room_code)
  if (is.null(s)) return(FALSE)
  if (!isTRUE(s$started %||% FALSE)) return(FALSE)
  roster_dirty_before <- isTRUE(s$roster_dirty %||% FALSE)
  burst_size <- as.integer(s$burst_size %||% 5L)
  if (!isTRUE(ensure_burst(room_code, burst_size = burst_size))) return(FALSE)
  s2 <- get_session(room_code)
  if (is.null(s2) || is.null(s2$burst_pairings) || length(s2$burst_pairings) < 1L) return(FALSE)
  pairings <- s2$burst_pairings[[1L]]
  ok <- advance_round(room_code, pairings)
  if (!isTRUE(ok)) return(FALSE)
  s3 <- get_session(room_code)
  if (is.null(s3)) return(FALSE)
  s3$burst_pairings <- NULL
  s3$burst_from_round <- as.integer(s3$round %||% 1L)
  s3$burst_sig <- ""
  if (roster_dirty_before) {
    # Roster changes apply starting this new round; prompt host to re-share.
    s3$share_needed <- TRUE
    s3$share_from_round <- as.integer(s3$round %||% 1L)
  }
  .sessions$store[[room_code]] <- s3
  save_store(.sessions$store)
  bump_sync_stamp()
  TRUE
}

#' Apply pairings to a session state object (pure, does not touch disk).
#' Mirrors logic in `advance_round()` but works on an in-memory `s`.
apply_pairings_to_state <- function(s, pairings) {
  if (is.null(s)) return(NULL)
  round_pairs <- list()
  for (p in pairings) {
    if (p$type == "pair") {
      s$last_action[[p$climbs_first]] <- "belay"
      s$last_action[[p$belays_first]] <- "climb"
      round_pairs[[length(round_pairs) + 1L]] <- sort(c(p$climbs_first, p$belays_first))
    } else if (p$type == "trio") {
      order_ids <- p$climb_order
      s$last_action[[order_ids[3L]]] <- "climb"
      s$last_action[[order_ids[1L]]] <- "belay"
      s$last_action[[order_ids[2L]]] <- "rest"
      round_pairs[[length(round_pairs) + 1L]] <- sort(c(order_ids[1L], order_ids[2L]))
      round_pairs[[length(round_pairs) + 1L]] <- sort(c(order_ids[1L], order_ids[3L]))
      round_pairs[[length(round_pairs) + 1L]] <- sort(c(order_ids[2L], order_ids[3L]))
    }
  }
  s$pair_history <- c(s$pair_history %||% list(), list(round_pairs))
  s$round <- s$round + 1L
  s
}

#' Resolved path to schedule_templates.rds (for loading and error messages).
schedule_templates_path <- function() {
  opt <- getOption("belay_buddy_schedule_templates_path", default = NULL)
  if (!is.null(opt) && nzchar(trimws(opt))) {
    return(trimws(opt))
  }
  root <- getOption("belay_buddy_project_root", default = NULL)
  if (!is.null(root) && nzchar(trimws(root))) {
    return(file.path(trimws(root), "precomputed", "schedule_templates.rds"))
  }
  file.path(getwd(), "precomputed", "schedule_templates.rds")
}

.fail_start_session <- function(room_code, schedule_rounds, msg) {
  s <- get_session(room_code)
  if (is.null(s)) return(msg)
  s$started <- FALSE
  s$schedule_active <- FALSE
  s$schedule_rounds <- schedule_rounds
  s$schedule_pairings <- NULL
  s$schedule_state_upto <- 0L
  s$last_action <- list()
  s$pair_history <- list()
  s$round <- 1L
  .sessions$store[[room_code]] <- s
  save_store(.sessions$store)
  bump_sync_stamp()
  msg
}

#' Load precomputed schedule templates from disk.
#' File format (from scripts/precompute_schedule_templates.R):
#'   list(metadata = ..., templates = list("4"=list(set1, set2, ...), ...))
load_schedule_templates <- function() {
  opt <- schedule_templates_path()
  mtime <- if (file.exists(opt)) file.mtime(opt) else as.POSIXct(NA)
  cached <- .sessions$schedule_templates_cache
  if (!is.null(cached) &&
      identical(.sessions$schedule_templates_cache_path %||% "", opt) &&
      identical(.sessions$schedule_templates_cache_mtime %||% as.POSIXct(NA), mtime)) {
    return(cached)
  }
  .sessions$schedule_templates_cache <- NULL
  .sessions$schedule_templates_cache_path <- NULL
  .sessions$schedule_templates_cache_mtime <- NULL

  if (!file.exists(opt)) {
    return(NULL)
  }
  x <- tryCatch(readRDS(opt), error = function(e) NULL)
  if (!is.null(x) && !is.null(x$templates)) {
    .sessions$schedule_templates_cache <- x
    .sessions$schedule_templates_cache_path <- opt
    .sessions$schedule_templates_cache_mtime <- mtime
    return(x)
  }
  NULL
}

#' Remap template IDs ("1".."n") to actual participant IDs.
remap_pairings_template_ids <- function(pairings, template_to_participant) {
  if (is.null(pairings) || length(pairings) == 0L) return(list())
  out <- vector("list", length(pairings))
  for (i in seq_along(pairings)) {
    p <- pairings[[i]]
    if (p$type == "pair") {
      out[[i]] <- list(
        type = "pair",
        climbs_first = template_to_participant[[p$climbs_first]],
        belays_first = template_to_participant[[p$belays_first]]
      )
    } else if (p$type == "trio") {
      out[[i]] <- list(
        type = "trio",
        climb_order = vapply(p$climb_order, function(id) template_to_participant[[id]], character(1))
      )
    } else {
      out[[i]] <- p
    }
  }
  out
}

#' Build a mapped schedule segment for the current participants using templates.
build_mapped_schedule_segment <- function(ids, templates_for_n) {
  n <- length(ids)
  if (n < 4L) return(NULL)
  template_sets <- templates_for_n
  if (is.null(template_sets) || length(template_sets) < 1L) return(NULL)

  set_index <- sample(seq_along(template_sets), 1L)
  segment <- template_sets[[set_index]]
  if (is.null(segment) || length(segment) < 1L) return(NULL)

  template_ids <- as.character(seq_len(n))
  # Random mapping to reduce chance that "couples" land together repeatedly.
  pid_shuffled <- sample(ids, length(ids), replace = FALSE)
  template_to_participant <- setNames(pid_shuffled, template_ids)

  # Remap every round.
  out <- vector("list", length(segment))
  for (k in seq_along(segment)) {
    out[[k]] <- remap_pairings_template_ids(segment[[k]], template_to_participant)
  }
  list(
    set_index = set_index,
    schedule_segment = out
  )
}

#' Append one mapped schedule segment onto the existing `schedule_pairings`.
append_schedule_segment_from_templates <- function(room_code) {
  s <- get_session(room_code)
  if (is.null(s)) return(FALSE)
  if (!isTRUE(s$started %||% FALSE)) return(FALSE)
  ids <- top_rope_ids(s$participants)
  n <- length(ids)
  if (n < 4L) return(FALSE)

  tpl <- load_schedule_templates()
  if (is.null(tpl) || is.null(tpl$templates) || is.null(tpl$templates[[as.character(n)]])) {
    return(FALSE)
  }

  built <- build_mapped_schedule_segment(ids = ids, templates_for_n = tpl$templates[[as.character(n)]])
  if (is.null(built) || is.null(built$schedule_segment) || length(built$schedule_segment) < 1L) return(FALSE)

  if (is.null(s$schedule_pairings)) s$schedule_pairings <- list()
  s$schedule_pairings <- c(s$schedule_pairings, built$schedule_segment)
  s$schedule_active <- TRUE

  .sessions$store[[room_code]] <- s
  save_store(.sessions$store)
  bump_sync_stamp()
  TRUE
}

#' Build and persist a multi-round schedule (rounds 1..n) immediately on Start.
#' After this, the session state is advanced to round (n+1) so additional rounds can continue.
start_session_and_build_schedule <- function(room_code, schedule_rounds = 15L) {
  s <- get_session(room_code)
  if (is.null(s)) {
    return("Session not found.")
  }

  schedule_rounds <- as.integer(schedule_rounds %||% 15L)
  if (schedule_rounds < 1L) schedule_rounds <- 1L

  ids <- top_rope_ids(s$participants)
  n <- length(ids)
  # Precomputed templates must exist; we never compute schedules in real-time.
  if (n < 4L) {
    return(.fail_start_session(
      room_code, schedule_rounds,
      paste0("Need at least 4 people on top rope to start (you have ", n, ").")
    ))
  }

  tpl_path <- schedule_templates_path()
  if (!file.exists(tpl_path)) {
    return(.fail_start_session(
      room_code, schedule_rounds,
      paste0(
        "Schedule file not found at:\n", tpl_path,
        "\nSet options(belay_buddy_project_root = \"<your app folder>\") or options(belay_buddy_schedule_templates_path = \"...\"), ",
        "or run the app from the project directory."
      )
    ))
  }

  tpl <- load_schedule_templates()
  if (is.null(tpl) || is.null(tpl$templates)) {
    return(.fail_start_session(
      room_code, schedule_rounds,
      paste0("Could not read schedule templates from:\n", tpl_path)
    ))
  }
  if (is.null(tpl$templates[[as.character(n)]])) {
    keys <- names(tpl$templates)
    sizes <- suppressWarnings(sort(unique(as.integer(keys))))
    sizes <- sizes[!is.na(sizes)]
    avail <- if (length(sizes)) paste(sizes, collapse = ", ") else paste(keys, collapse = ", ")
    return(.fail_start_session(
      room_code, schedule_rounds,
      paste0(
        "No precomputed templates for ", n, " top-rope participants. ",
        "This file includes group sizes: ", avail, ". ",
        "Re-run scripts/precompute_schedule_templates.R with --min_n/--max_n covering your group."
      )
    ))
  }

  s$started <- TRUE
  s$schedule_active <- TRUE
  s$schedule_rounds <- schedule_rounds
  s$schedule_pairings <- list()
  s$schedule_state_upto <- 0L

  # Must persist before append_schedule_segment_from_templates (it requires started == TRUE).
  .sessions$store[[room_code]] <- s
  save_store(.sessions$store)
  bump_sync_stamp()

  # Build at least the initial revealed rounds; templates should include the full segment.
  ok <- TRUE
  while (length(s$schedule_pairings) < schedule_rounds && ok) {
    ok <- append_schedule_segment_from_templates(room_code)
    s <- get_session(room_code)
  }
  if (!ok || is.null(s$schedule_pairings) || length(s$schedule_pairings) < schedule_rounds) {
    return(.fail_start_session(
      room_code, schedule_rounds,
      "Could not build enough precomputed rounds (template data may be incomplete)."
    ))
  }

  # Apply revealed rounds to keep state variables consistent.
  s$last_action <- list()
  s$pair_history <- list()
  s$round <- 1L
  for (k in seq_len(schedule_rounds)) {
    s <- apply_pairings_to_state(s, s$schedule_pairings[[k]])
  }
  s$schedule_state_upto <- schedule_rounds

  .sessions$store[[room_code]] <- s
  save_store(.sessions$store)
  bump_sync_stamp()
  TRUE
}

#' Append precomputed rounds to the schedule (same simulation as start). Keeps schedule_active TRUE.
#' Persisted last_action, pair_history, and round advance as if those rounds completed.
extend_session_schedule <- function(room_code, additional_rounds = 5L, update_display_rounds = TRUE) {
  s <- get_session(room_code)
  if (is.null(s)) return(FALSE)
  additional_rounds <- as.integer(additional_rounds %||% 5L)
  if (additional_rounds < 1L) additional_rounds <- 1L
  if (is.null(s$schedule_pairings) || length(s$schedule_pairings) < 1L) return(FALSE)

  target_total <- length(s$schedule_pairings) + additional_rounds
  ok <- TRUE
  while (length(s$schedule_pairings) < target_total && ok) {
    ok <- append_schedule_segment_from_templates(room_code)
    s <- get_session(room_code)
  }
  if (!ok) return(FALSE)

  if (isTRUE(update_display_rounds)) {
    s$schedule_rounds <- min(length(s$schedule_pairings), s$schedule_rounds %||% length(s$schedule_pairings))
  }
  s$schedule_active <- TRUE
  .sessions$store[[room_code]] <- s
  save_store(.sessions$store)
  bump_sync_stamp()
  TRUE
}

#' Set the number of scheduled rounds to display (reveal, without recomputing).
set_schedule_display_rounds <- function(room_code, schedule_rounds) {
  s <- get_session(room_code)
  if (is.null(s)) return(FALSE)
  schedule_rounds <- as.integer(schedule_rounds %||% 5L)
  if (schedule_rounds < 1L) schedule_rounds <- 1L
  if (is.null(s$schedule_pairings) || length(s$schedule_pairings) < schedule_rounds) return(FALSE)

  upto <- s$schedule_state_upto %||% 0L
  if (schedule_rounds > upto) {
    for (k in seq(upto + 1L, schedule_rounds)) {
      s <- apply_pairings_to_state(s, s$schedule_pairings[[k]])
    }
    s$schedule_state_upto <- schedule_rounds
  }

  s$schedule_rounds <- schedule_rounds
  s$schedule_active <- TRUE

  .sessions$store[[room_code]] <- s
  save_store(.sessions$store)
  bump_sync_stamp()
  TRUE
}

#' Pre-calculate the next block of scheduled rounds in the background.
#' Appends to `schedule_pairings` but does NOT change `schedule_rounds` (display stays the same).
schedule_precompute_next_block <- function(room_code, block_size = 5L) {
  s <- get_session(room_code)
  if (is.null(s)) return(FALSE)
  if (!isTRUE(s$schedule_active %||% FALSE)) return(FALSE)
  if (is.null(s$schedule_pairings) || length(s$schedule_pairings) < 1L) return(FALSE)

  block_size <- as.integer(block_size %||% 5L)
  if (block_size < 1L) block_size <- 1L

  display_rounds <- as.integer(s$schedule_rounds %||% 5L)
  total_precomputed <- length(s$schedule_pairings)
  target_total <- display_rounds + block_size

  if (total_precomputed >= target_total) return(FALSE)
  if (isTRUE(s$schedule_precompute_in_progress %||% FALSE)) return(FALSE)

  # Mark in-progress so other tabs don't duplicate the expensive computation.
  s$schedule_precompute_in_progress <- TRUE
  .sessions$store[[room_code]] <- s
  save_store(.sessions$store)
  bump_sync_stamp()

  missing <- target_total - total_precomputed
  ok <- extend_session_schedule(room_code, additional_rounds = missing, update_display_rounds = FALSE)

  # Clear flag (even if compute failed, so UI doesn't get stuck).
  s2 <- get_session(room_code)
  if (!is.null(s2)) {
    s2$schedule_precompute_in_progress <- FALSE
    .sessions$store[[room_code]] <- s2
    save_store(.sessions$store)
    bump_sync_stamp()
  }

  isTRUE(ok)
}

#' Toggle schedule display mode (used for showing round 1..n schedule, then continuing at n+1).
set_schedule_active <- function(room_code, active) {
  s <- get_session(room_code)
  if (is.null(s)) return(FALSE)
  s$schedule_active <- isTRUE(active)
  .sessions$store[[room_code]] <- s
  save_store(.sessions$store)
  bump_sync_stamp()
  TRUE
}

#' Control auto-advancing state.
#' Returns TRUE if the flag was set to the requested value (i.e., caller should schedule).
set_auto_running <- function(room_code, auto_running, auto_until_round = NULL) {
  s <- get_session(room_code)
  if (is.null(s)) return(FALSE)
  # If it's already running and caller wants running, avoid double-scheduling.
  if (isTRUE(s$auto_running) && isTRUE(auto_running)) return(FALSE)
  s$auto_running <- isTRUE(auto_running)
  if (!is.null(auto_until_round)) s$auto_until_round <- as.integer(auto_until_round)
  .sessions$store[[room_code]] <- s
  save_store(.sessions$store)
  bump_sync_stamp()
  TRUE
}

#' Advance to next round: update last_action from current pairings, record pair_history, increment round.
#' Trio: use climb_order; last climber = "climb", other two = "belay".
advance_round <- function(room_code, pairings) {
  s <- get_session(room_code)
  if (is.null(s)) return(FALSE)
  round_pairs <- list()
  for (p in pairings) {
    if (p$type == "pair") {
      # After swap: climbs_first belayed last, belays_first climbed last
      s$last_action[[p$climbs_first]] <- "belay"
      s$last_action[[p$belays_first]] <- "climb"
      round_pairs[[length(round_pairs) + 1L]] <- sort(c(p$climbs_first, p$belays_first))
    } else if (p$type == "trio") {
      # climb_order: who climbs in slot 1, 2, 3. Slot 1: ord[1] climbs, ord[2] belays, ord[3] rests; etc.
      # After round: ord[3] climbed last, ord[1] belayed last, ord[2] rested last.
      order_ids <- p$climb_order
      s$last_action[[order_ids[3L]]] <- "climb"
      s$last_action[[order_ids[1L]]] <- "belay"
      s$last_action[[order_ids[2L]]] <- "rest"
      round_pairs[[length(round_pairs) + 1L]] <- sort(c(order_ids[1L], order_ids[2L]))
      round_pairs[[length(round_pairs) + 1L]] <- sort(c(order_ids[1L], order_ids[3L]))
      round_pairs[[length(round_pairs) + 1L]] <- sort(c(order_ids[2L], order_ids[3L]))
    }
  }
  s$pair_history <- c(s$pair_history %||% list(), list(round_pairs))
  s$round <- s$round + 1L
  .sessions$store[[room_code]] <- s
  save_store(.sessions$store)
  bump_sync_stamp()
  TRUE
}
