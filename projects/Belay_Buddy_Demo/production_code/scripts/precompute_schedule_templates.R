# Precompute round schedules (sequence of pairings) for group sizes 4..20.
#
# This does *not* change the runtime algorithm; it just moves the expensive work
# (compute_pairings across many rounds) into an offline script.
#
# Output:
#   precomputed/schedule_templates.rds
#
# Usage examples:
#   Rscript scripts/precompute_schedule_templates.R
#   Rscript scripts/precompute_schedule_templates.R --min_n=4 --max_n=20 --rounds=30 --num_sets=1
#   Rscript scripts/precompute_schedule_templates.R --num_sets=5 --seed_base=1000

args <- commandArgs(trailingOnly = TRUE)
parse_arg <- function(prefix, default) {
  hit <- args[startsWith(args, prefix)]
  if (length(hit) == 0L) return(default)
  as.character(sub(prefix, "", hit[1L]))
}

min_n <- as.integer(parse_arg("--min_n=", "4"))
max_n <- as.integer(parse_arg("--max_n=", "20"))
rounds <- as.integer(parse_arg("--rounds=", "30"))
num_sets <- as.integer(parse_arg("--num_sets=", "1"))
seed_base <- as.integer(parse_arg("--seed_base=", "12345"))
out_path <- parse_arg("--out=", file.path("precomputed", "schedule_templates.rds"))
workers <- as.integer(parse_arg("--workers=", NA_character_))

if (min_n < 2L) min_n <- 2L
if (max_n < min_n) stop("--max_n must be >= --min_n")
if (rounds < 1L) rounds <- 1L
if (num_sets < 1L) num_sets <- 1L
if (is.na(workers) || workers < 1L) {
  # Leave 1 core free by default (best effort; can be overridden).
  workers <- max(1L, parallel::detectCores() - 1L)
}

if (workers > 1L) {
  cat("Using parallel workers:", workers, "\n", sep = " ")
}

# Source the existing matching code (prefer project folder even if cwd is one level off).
root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(root, "R", "pairing.R"))) {
  up <- normalizePath(file.path(root, ".."), winslash = "/", mustWork = FALSE)
  if (file.exists(file.path(up, "R", "pairing.R"))) root <- up
}
source(file.path(root, "R", "pairing.R"))
source(file.path(root, "R", "session.R"))

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

template_ids_by_n <- function(n) {
  as.character(seq_len(n))
}

# participants aren't used in compute_pairings directly today, but we pass a realistic
# structure to keep the signatures stable.
make_participants <- function(ids) {
  out <- setNames(vector("list", length(ids)), ids)
  for (id in ids) {
    out[[id]] <- list(category = "top_rope", name = id)
  }
  out
}

# Compute one schedule for a fixed N and RNG seed.
compute_schedule_for_n <- function(n, set_index = 1L) {
  ids <- template_ids_by_n(n)
  participants <- make_participants(ids)

  set.seed(seed_base + (n * 1000L) + set_index)

  s_sim <- list(
    round = 1L,
    last_action = list(),
    pair_history = list(),
    participants = participants
  )

  schedule <- vector("list", rounds)
  for (k in seq_len(rounds)) {
    pairings <- compute_pairings(
      ids,
      s_sim$last_action,
      k,
      s_sim$participants,
      s_sim$pair_history
    )
    schedule[[k]] <- pairings
    s_sim <- apply_pairings_to_state(s_sim, pairings)
  }
  schedule
}

compute_schedule_for_n_worker <- function(n, set_index = 1L) {
  # Same logic as compute_schedule_for_n, but exists only for clarity.
  compute_schedule_for_n(n = n, set_index = set_index)
}

existing <- NULL
if (file.exists(out_path)) {
  existing <- tryCatch(readRDS(out_path), error = function(e) NULL)
}

templates <- list()
if (!is.null(existing) && !is.null(existing$templates)) {
  templates <- existing$templates
}

meta <- list(
  min_n = min_n,
  max_n = max_n,
  rounds = rounds,
  num_sets = num_sets,
  seed_base = seed_base,
  generated_at = Sys.time()
)

for (n in seq(min_n, max_n)) {
  key <- as.character(n)
  if (!is.null(templates[[key]]) && length(templates[[key]]) >= num_sets) {
    cat("Skipping N=", n, " (already have ", length(templates[[key]]), " sets)\n", sep = "")
    next
  }

  cat("Computing N=", n, " (", num_sets, " set(s), ", rounds, " rounds)\n", sep = "")

  if (workers <= 1L) {
    templates[[key]] <- vector("list", num_sets)
    for (set_index in seq_len(num_sets)) {
      cat("  Set ", set_index, "/", num_sets, "\n", sep = "")
      templates[[key]][[set_index]] <- compute_schedule_for_n(n, set_index = set_index)
    }
  } else {
    cl <- parallel::makeCluster(workers)

    # Ensure every worker has the same matching functions.
    pairing_file <- file.path(root, "R", "pairing.R")
    session_file <- file.path(root, "R", "session.R")
    parallel::clusterExport(
      cl,
      varlist = c("pairing_file", "session_file", "seed_base", "rounds"),
      envir = environment()
    )
    parallel::clusterEvalQ(cl, {
      source(pairing_file)
      source(session_file)
      NULL
    })

    # Define helpers and the compute function on each worker.
    parallel::clusterEvalQ(cl, {
      template_ids_by_n <- function(n) as.character(seq_len(n))
      make_participants <- function(ids) {
        out <- setNames(vector("list", length(ids)), ids)
        for (id in ids) out[[id]] <- list(category = "top_rope", name = id)
        out
      }
      compute_schedule_for_n_worker <- function(n, set_index, seed_base, rounds) {
        ids <- template_ids_by_n(n)
        participants <- make_participants(ids)
        set.seed(seed_base + (n * 1000L) + set_index)
        s_sim <- list(round = 1L, last_action = list(), pair_history = list(), participants = participants)
        schedule <- vector("list", rounds)
        for (k in seq_len(rounds)) {
          pairings <- compute_pairings(ids, s_sim$last_action, k, s_sim$participants, s_sim$pair_history)
          schedule[[k]] <- pairings
          s_sim <- apply_pairings_to_state(s_sim, pairings)
        }
        schedule
      }
      NULL
    })

    n_value <- n
    parallel::clusterExport(cl, varlist = c("n_value"), envir = environment())

    set_indices <- seq_len(num_sets)
    results <- parallel::parLapply(cl, set_indices, function(set_index) {
      compute_schedule_for_n_worker(
        n = n_value,
        set_index = set_index,
        seed_base = seed_base,
        rounds = rounds
      )
    })
    parallel::stopCluster(cl)
    templates[[key]] <- results
  }

  saveRDS(
    list(
      metadata = meta,
      templates = templates
    ),
    out_path
  )
}

cat("Done. Wrote templates to:\n", out_path, "\n", sep = "")

