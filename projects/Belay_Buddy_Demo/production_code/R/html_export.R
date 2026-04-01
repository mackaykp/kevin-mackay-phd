# HTML schedule backup: visual replica of in-app schedule (Bootstrap Flatly + embedded custom.css + collapses).

bb_custom_css_path <- function() {
  root <- getOption("belay_buddy_project_root", default = NULL)
  candidates <- character(0)
  if (!is.null(root) && nzchar(trimws(root))) {
    candidates <- c(candidates, file.path(trimws(root), "www", "custom.css"))
  }
  candidates <- c(candidates, file.path(getwd(), "www", "custom.css"))
  up <- tryCatch(normalizePath(file.path(getwd(), ".."), winslash = "/", mustWork = FALSE), error = function(e) "")
  if (nzchar(up)) {
    candidates <- c(candidates, file.path(up, "www", "custom.css"))
  }
  for (p in candidates) {
    if (file.exists(p)) return(p)
  }
  NULL
}

bb_read_custom_css_embed <- function() {
  path <- bb_custom_css_path()
  if (is.null(path)) return("")
  tryCatch(
    paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
    error = function(e) ""
  )
}

bb_export_esc <- function(x) {
  htmltools::htmlEscape(as.character(x %||% ""), attribute = FALSE)
}

bb_export_pairing_rows_html <- function(display_obj) {
  parts <- character(length(display_obj))
  for (i in seq_along(display_obj)) {
    d <- display_obj[[i]]
    label <- if (d$type == "trio") "Trio" else paste0("Pair ", i)
    if (d$type == "pair") {
      parts[[i]] <- paste0(
        "<div class=\"pairing-row pairing-row-pair\">",
        "<div class=\"pairing-label\">", bb_export_esc(label), "</div>",
        "<div class=\"pairing-pair-line\">",
        "<div class=\"pairing-visual\">",
        "<div class=\"pairing-slot pairing-slot-climb\">",
        "<span class=\"pairing-name\">", bb_export_esc(d$climbs_first), "</span>",
        "<i class=\"bi bi-arrow-up-circle-fill pairing-icon pairing-icon-climb\" title=\"Climbs first\"></i>",
        "</div>",
        "<span class=\"pairing-sep\">·</span>",
        "<div class=\"pairing-slot pairing-slot-belay\">",
        "<span class=\"pairing-name\">", bb_export_esc(d$belays_first), "</span>",
        "<i class=\"bi bi-link-45deg pairing-icon pairing-icon-belay\" title=\"Belays first\"></i>",
        "</div>",
        "</div>",
        "<div class=\"pairing-swap\">",
        "<i class=\"bi bi-arrow-left-right pairing-icon-swap\" aria-hidden=\"true\"></i>",
        "<span class=\"pairing-swap-text\">then swap</span>",
        "</div>",
        "</div></div>"
      )
    } else {
      n1 <- d$climb_order[1L]
      n2 <- d$climb_order[2L]
      n3 <- d$climb_order[3L]
      slot <- function(a, b, c) {
        paste0(
          "<div class=\"pairing-trio-slot\">",
          "<i class=\"bi bi-arrow-up-circle-fill pairing-icon pairing-icon-climb\"></i>",
          "<span class=\"pairing-name\">", bb_export_esc(a), "</span>",
          "<i class=\"bi bi-link-45deg pairing-icon pairing-icon-belay\"></i>",
          "<span class=\"pairing-name\">", bb_export_esc(b), "</span>",
          "<i class=\"bi bi-pause-circle pairing-icon pairing-icon-rest\"></i>",
          "<span class=\"pairing-name\">", bb_export_esc(c), "</span>",
          "</div>"
        )
      }
      parts[[i]] <- paste0(
        "<div class=\"pairing-row pairing-row-trio\">",
        "<div class=\"pairing-label\">", bb_export_esc(label), "</div>",
        "<div class=\"pairing-trio\">",
        slot(n1, n2, n3),
        slot(n2, n3, n1),
        slot(n3, n1, n2),
        "</div></div>"
      )
    }
  }
  paste(parts, collapse = "\n")
}

bb_export_one_round_card_html <- function(k, spp, participants, round_start = 1L) {
  disp <- pairings_for_display(spp[[k]], participants)
  body_inner <- bb_export_pairing_rows_html(disp)
  rs <- suppressWarnings(as.integer(round_start))
  rs <- if (length(rs) >= 1L && !is.na(rs[1L])) rs[1L] else 1L
  abs_round <- rs + (k - 1L)
  ac <- ((abs_round - 1L) %% 5L) + 1L
  paste0(
    "<div class=\"card bb-card bb-round-card bb-accent-", ac, " mb-1\">",
    "<div class=\"card-header bb-round-header\">",
    "<span class=\"bb-round-badge bb-round-badge-", ac, "\">", abs_round, "</span>",
    "<div class=\"bb-round-header-text\">",
    "<span class=\"bb-round-title\">Round ", abs_round, "</span>",
    "<span class=\"bb-round-sub\">Top-rope</span>",
    "</div></div>",
    "<div class=\"card-body bb-pairings-body\">", body_inner, "</div></div>"
  )
}

bb_export_participants_html <- function(participants) {
  if (is.null(participants) || length(participants) < 1L) {
    return("")
  }
  ids <- names(participants)
  # Export should reflect the roster for these rounds (active only).
  ids <- ids[!vapply(ids, function(id) isFALSE(participants[[id]]$active), logical(1))]
  if (length(ids) < 1L) {
    return("")
  }
  ord <- order(tolower(vapply(ids, function(id) {
    trimws(as.character(participants[[id]]$name %||% ""))
  }, character(1))))
  ids <- ids[ord]
  chips <- vapply(ids, function(pid) {
    nm <- trimws(as.character(participants[[pid]]$name %||% ""))
    paste0(
      "<div class=\"participant-chip-row participant-chip-row-single\">",
      "<div class=\"participant-chip participant-chip-other\">", bb_export_esc(nm), "</div></div>"
    )
  }, character(1))
  paste0(
    "<div class=\"card bb-card bb-participants-card mb-1\">",
    "<div class=\"card-header bb-card-header-accent\">",
    "<i class=\"bi bi-people-fill me-2\" aria-hidden=\"true\"></i>Participants (", length(ids), ")",
    "</div>",
    "<div class=\"card-body bb-card-body-tight\">",
    "<div class=\"participant-board\"><div class=\"participant-simple-list\">",
    paste(chips, collapse = ""),
    "</div></div></div></div>"
  )
}

#' Event POSIXct or NA (for stable sort when `ts` missing on old sessions).
bb_export_roster_event_epoch <- function(ev) {
  if (!is.list(ev)) return(NA_real_)
  t <- ev[["ts"]]
  if (inherits(t, "POSIXct") && length(t) >= 1L && !is.na(t[1L])) return(as.numeric(t[1L]))
  NA_real_
}

#' Roster change summary grouped by effective round (newest round first).
bb_export_roster_changes_html <- function(roster_events) {
  if (is.null(roster_events) || length(roster_events) < 1L) return("")

  n <- length(roster_events)
  types <- character(0)
  names <- character(0)
  effs <- integer(0)
  epochs <- numeric(0)
  seq_i <- integer(0)

  for (i in seq_len(n)) {
    ev <- roster_events[[i]]
    if (!is.list(ev)) next
    typ <- ev$type %||% ""
    if (!identical(typ, "add") && !identical(typ, "remove")) next
    nm <- trimws(as.character(ev$name %||% ""))
    if (!nzchar(nm)) next
    eff <- suppressWarnings(as.integer(ev$effective_round %||% NA_integer_))
    if (length(eff) < 1L || is.na(eff[1L]) || eff[1L] < 1L) eff <- 1L else eff <- eff[1L]
    types <- c(types, typ)
    names <- c(names, nm)
    effs <- c(effs, eff)
    epochs <- c(epochs, bb_export_roster_event_epoch(ev))
    seq_i <- c(seq_i, i) # fallback recency when `ts` missing
  }

  if (length(names) < 1L) return("")

  rounds <- sort(unique(effs), decreasing = TRUE)

  make_chip <- function(nm, typ) {
    cls <- if (identical(typ, "add")) "participant-chip-added" else "participant-chip-removed"
    paste0(
      "<div class=\"participant-chip participant-chip-other ", cls, "\">",
      bb_export_esc(nm),
      "</div>"
    )
  }

  groups <- vapply(rounds, function(rnd) {
    idx <- which(effs == rnd)
    epoch2 <- epochs[idx]
    epoch2[is.na(epoch2)] <- -Inf
    # Newest first within the same effective round: prefer real timestamps; then fallback by list order.
    ord <- order(is.na(epochs[idx]), -epoch2, -seq_i[idx])
    idx2 <- idx[ord]

    # Keep “added” and “removed” separately so each can be ordered newest->oldest.
    idx_add <- idx2[types[idx2] == "add"]
    idx_rem <- idx2[types[idx2] == "remove"]

    chips_add <- if (length(idx_add)) paste(vapply(idx_add, function(j) make_chip(names[j], types[j]), character(1)), collapse = "") else ""
    chips_rem <- if (length(idx_rem)) paste(vapply(idx_rem, function(j) make_chip(names[j], types[j]), character(1)), collapse = "") else ""

    chips_all <- paste0(chips_add, chips_rem)
    if (!nzchar(chips_all)) return("")

    paste0(
      "<div class=\"bb-roster-change-group d-flex flex-wrap align-items-center gap-2 mb-0\">",
      "<span class=\"bb-roster-change-round\">Eff ", bb_export_esc(rnd), "</span>",
      "<div class=\"bb-roster-change-chiplist\">",
      chips_all,
      "</div>",
      "</div>"
    )
  }, character(1))

  groups <- groups[nzchar(groups)]
  if (length(groups) < 1L) return("")

  paste0(
    "<div class=\"card bb-card mb-1 bb-roster-changes-card\">",
    "<div class=\"card-header bb-card-header-accent\">",
    "<i class=\"bi bi-arrow-repeat me-2\" aria-hidden=\"true\"></i>Roster changes",
    "<span class=\"text-muted small ms-1\">(grouped by effective round)</span>",
    "</div>",
    "<div class=\"card-body bb-card-body-tight\">",
    paste(groups, collapse = "\n"),
    "</div></div>"
  )
}

#' Rich HTML snapshot matching in-app schedule layout (CDN Bootstrap Flatly + icons; embedded custom.css).
build_schedule_html_export <- function(
    state,
    room_code,
    primary_rounds = 5L,
    second_tier_end = 10L,
    round_start = 1L) {
  participants <- state$participants
  spp <- state$schedule_pairings
  if (is.null(spp) || length(spp) < 1L) {
    return("<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\"><title>Belay Buddy</title></head><body><p>No schedule data.</p></body></html>")
  }

  n_all <- length(spp)
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M")

  css_embed <- bb_read_custom_css_embed()
  rc_safe <- gsub("[^a-zA-Z0-9]", "", room_code)
  id1 <- paste0("bbExport", rc_safe, "T610")
  id2 <- paste0("bbExport", rc_safe, "T11")

  # Include every loaded round in the file (same as precomputed buffer).
  primary_n <- min(as.integer(primary_rounds), n_all)
  tier1_from <- primary_n + 1L
  tier1_to <- min(as.integer(second_tier_end), n_all)
  tier1_block <- if (tier1_to >= tier1_from) {
    from_abs <- as.integer(round_start) + (tier1_from - 1L)
    to_abs <- as.integer(round_start) + (tier1_to - 1L)
    cards <- vapply(seq(tier1_from, tier1_to), function(k) {
      bb_export_one_round_card_html(k, spp, participants, round_start = round_start)
    }, character(1))
    paste0(
      "<button class=\"bb-extra-rounds-toggle\" type=\"button\" data-bs-toggle=\"collapse\" ",
      "data-bs-target=\"#", id1, "\" aria-expanded=\"false\" aria-controls=\"", id1, "\">",
      "<i class=\"bi bi-chevron-down bb-extra-rounds-chevron\" aria-hidden=\"true\"></i>",
      "<span class=\"bb-extra-rounds-label\">Rounds ", from_abs, "–", to_abs, "</span>",
      "<span class=\"bb-extra-rounds-hint text-muted\">Tap to expand</span></button>",
      "<div id=\"", id1, "\" class=\"collapse bb-extra-rounds-panel\">",
      paste(cards, collapse = "\n"),
      "</div>"
    )
  } else {
    ""
  }

  tier2_block <- if (n_all > as.integer(second_tier_end)) {
    t2_from <- as.integer(second_tier_end) + 1L
    t2_to <- n_all
    from_abs <- as.integer(round_start) + (t2_from - 1L)
    to_abs <- as.integer(round_start) + (t2_to - 1L)
    cards <- vapply(seq(t2_from, t2_to), function(k) {
      bb_export_one_round_card_html(k, spp, participants, round_start = round_start)
    }, character(1))
    paste0(
      "<button class=\"bb-extra-rounds-toggle\" type=\"button\" data-bs-toggle=\"collapse\" ",
      "data-bs-target=\"#", id2, "\" aria-expanded=\"false\" aria-controls=\"", id2, "\">",
      "<i class=\"bi bi-chevron-down bb-extra-rounds-chevron\" aria-hidden=\"true\"></i>",
      "<span class=\"bb-extra-rounds-label\">Rounds ", from_abs, "–", to_abs, "</span>",
      "<span class=\"bb-extra-rounds-hint text-muted\">Tap to expand</span></button>",
      "<div id=\"", id2, "\" class=\"collapse bb-extra-rounds-panel\">",
      paste(cards, collapse = "\n"),
      "</div>"
    )
  } else {
    ""
  }

  primary_cards <- if (primary_n >= 1L) {
    paste(vapply(seq_len(primary_n), function(k) {
      bb_export_one_round_card_html(k, spp, participants, round_start = round_start)
    }, character(1)), collapse = "\n")
  } else {
    ""
  }

  part_html <- bb_export_participants_html(participants)
  roster_html <- bb_export_roster_changes_html(state$roster_events %||% list())

  banner <- paste0(
    "<div class=\"card bb-card mb-1\">",
    "<div class=\"card-body py-2 px-3\">",
    "<p class=\"small text-muted mb-0\"><strong>Belay Buddy</strong> · Rounds ",
    bb_export_esc(as.integer(round_start)), "–", bb_export_esc(as.integer(round_start) + n_all - 1L),
    "</p>",
    "</div></div>"
  )

  extra_css <- "
body.bb-html-export { font-family: \"DM Sans\", system-ui, -apple-system, sans-serif !important; }
.bb-html-export .container-fluid.bb-export-shell { max-width: 26rem; margin-left: auto; margin-right: auto; }
.bb-html-export .bb-card-header-accent .bi { vertical-align: -0.125em; }
.bb-html-export .participant-chip-added { background: rgba(5, 150, 105, 0.12); border: 1px solid rgba(5, 150, 105, 0.35); color: #065f46; }
.bb-html-export .participant-chip-removed { background: rgba(220, 38, 38, 0.10); border: 1px solid rgba(220, 38, 38, 0.30); color: #991b1b; }
"

  paste0(
    "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n",
    "<meta charset=\"utf-8\">\n",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
    "<title>Belay Buddy - schedule</title>\n",
    "<link rel=\"preconnect\" href=\"https://fonts.googleapis.com\">\n",
    "<link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin>\n",
    "<link href=\"https://fonts.googleapis.com/css2?family=DM+Sans:ital,opsz,wght@0,9..40,400;0,9..40,600;0,9..40,700;1,9..40,400&display=swap\" rel=\"stylesheet\">\n",
    "<link href=\"https://cdn.jsdelivr.net/npm/bootswatch@5.3.3/dist/flatly/bootstrap.min.css\" rel=\"stylesheet\">\n",
    "<link href=\"https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css\" rel=\"stylesheet\">\n",
    "<style>\n", css_embed, "\n", extra_css, "\n</style>\n",
    "</head>\n<body class=\"bb-html-export\">\n",
    "<div class=\"container-fluid bb-export-shell py-2\">\n",
    banner,
    roster_html,
    part_html,
    "<div class=\"bb-session-stack bb-tight-stack\">\n",
    primary_cards, "\n",
    tier1_block, "\n",
    tier2_block, "\n",
    "</div>\n",
    "</div>\n",
    "<script src=\"https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js\"></script>\n",
    "</body>\n</html>"
  )
}
