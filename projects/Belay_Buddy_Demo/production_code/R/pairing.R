# Top-rope pairing: form pairs (and one trio if odd).
#
# Even-sized groups: minimum-repeat pairing uses min-weight perfect matching (lpSolve)
# instead of enumerating all (n-1)!! matchings. Bipartite (alternation) blocks use a
# linear assignment solve. Install: install.packages("lpSolve")
#
# Matching priorities (high to low):
# 1) Diversity - minimize total repeat pairings (pair_count) across the round.
# 2) Even n - alternation: among minimum-repeat matchings, prefer no double climb/belay when tie;
#    only allow doubles if unconstrained matching has strictly lower repeat total.
# 3) Odd n - trio avoidance: among minimum-repeat trio+pair partitions, minimize how many people
#    in the new trio were already in a trio in a past round; repeat trios only when every
#    minimum-diversity option needs at least one such person (then pick the smallest count).
# 4) Role assignment - climb/belay/rest from preferences (belayed last → climb first, etc.).
#
# Trio: 1 climber, 1 belayer, 1 rest; rotate so each climbs once, belays once, rests once.

#' Get top-rope participant ids from session state
top_rope_ids <- function(participants) {
  ids <- character(0)
  for (pid in names(participants)) {
    p <- participants[[pid]]
    if (isFALSE(p$active)) next
    if (p$category == "top_rope")
      ids <- c(ids, pid)
  }
  # UX fallback: if session data contains older/alternate category values and
  # nobody matches "top_rope", we still want Start to produce a schedule.
  if (length(ids) == 0L && length(participants) > 0L) {
    ids <- names(participants)
  }
  ids
}

#' Count how many times two ids have been in the same pair/trio (from pair_history)
pair_count <- function(id1, id2, pair_history) {
  if (length(pair_history) == 0) return(0L)
  n <- 0L
  for (round_pairs in pair_history) {
    for (pr in round_pairs) {
      if (length(pr) >= 2L && ((pr[1L] == id1 && pr[2L] == id2) || (pr[1L] == id2 && pr[2L] == id1)))
        n <- n + 1L
    }
  }
  n
}

#' Ids who were in the trio in any past round (in odd rounds each trio member appears in two pairs that round).
prior_trio_participant_ids <- function(pair_history) {
  if (length(pair_history) == 0L) return(character(0))
  out <- character(0)
  for (round_pairs in pair_history) {
    deg <- list()
    for (pr in round_pairs) {
      if (length(pr) < 2L) next
      a <- pr[1L]
      b <- pr[2L]
      deg[[a]] <- (if (is.null(deg[[a]])) 0L else deg[[a]]) + 1L
      deg[[b]] <- (if (is.null(deg[[b]])) 0L else deg[[b]]) + 1L
    }
    for (nm in names(deg)) {
      if (deg[[nm]] >= 2L) out <- c(out, nm)
    }
  }
  unique(out)
}

#' Number of chosen trio members who were in a trio before (0–3).
trio_prior_repeat_count <- function(trio, prior_trio_ids) {
  if (length(prior_trio_ids) == 0L) return(0L)
  sum(vapply(trio, function(id) id %in% prior_trio_ids, logical(1)))
}

#' Integer symmetric matrix of pair_count for all pairs in ids (single pass over history).
pair_count_matrix <- function(ids, pair_history) {
  n <- length(ids)
  M <- matrix(0L, n, n)
  if (length(pair_history) == 0L) {
    dimnames(M) <- list(ids, ids)
    return(M)
  }
  for (round_pairs in pair_history) {
    for (pr in round_pairs) {
      if (length(pr) < 2L) next
      i <- match(pr[1L], ids, nomatch = 0L)
      j <- match(pr[2L], ids, nomatch = 0L)
      if (i == 0L || j == 0L) next
      M[i, j] <- M[i, j] + 1L
      M[j, i] <- M[j, i] + 1L
    }
  }
  dimnames(M) <- list(ids, ids)
  M
}

#' Numeric weights from count matrix; optional symmetric jitter breaks ties at random.
pair_weight_from_matrix <- function(M, jitter = FALSE) {
  n <- nrow(M)
  W <- matrix(as.numeric(M), n, n)
  diag(W) <- 0
  if (jitter) {
    for (i in seq_len(n - 1L)) {
      for (j in (i + 1L):n) {
        u <- stats::runif(1L, 0, 1e-6)
        W[i, j] <- W[i, j] + u
        W[j, i] <- W[j, i] + u
      }
    }
  }
  W
}

#' Minimum-weight perfect matching via binary LP (one variable per edge). Returns list of sorted pairs or NULL.
min_weight_perfect_matching_lpsolve <- function(ids, W) {
  n <- length(ids)
  if (n == 0L) return(list())
  if (n == 2L) return(list(sort(c(ids[1L], ids[2L]))))
  stopifnot(n %% 2L == 0L)
  m <- as.integer(n * (n - 1L) / 2L)
  obj <- numeric(m)
  con <- matrix(0, n, m)
  col <- 0L
  for (ii in seq_len(n - 1L)) {
    for (jj in (ii + 1L):n) {
      col <- col + 1L
      obj[col] <- W[ii, jj]
      con[ii, col] <- 1
      con[jj, col] <- 1
    }
  }
  res <- lpSolve::lp(
    "min",
    obj,
    con,
    rep("=", n),
    rep(1, n),
    binary.vec = seq_len(m),
    presolve = 0
  )
  if (res$status != 0) return(NULL)
  sol <- res$solution
  out <- list()
  col <- 0L
  for (ii in seq_len(n - 1L)) {
    for (jj in (ii + 1L):n) {
      col <- col + 1L
      if (!is.na(sol[col]) && sol[col] > 0.5) {
        out[[length(out) + 1L]] <- sort(c(ids[ii], ids[jj]))
      }
    }
  }
  if (length(out) != n %/% 2L) return(NULL)
  out
}

#' Linear assignment (square cost matrix): return permutation p with p[i] = assigned column for row i.
assignment_min_cost_lpsolve <- function(cost) {
  m <- nrow(cost)
  if (m == 0L) return(integer(0))
  if (m == 1L) return(1L)
  stopifnot(ncol(cost) == m)
  nvar <- m * m
  obj <- numeric(nvar)
  for (i in seq_len(m)) {
    for (j in seq_len(m)) {
      obj[(i - 1L) * m + j] <- cost[i, j]
    }
  }
  con <- matrix(0, 2L * m, nvar)
  for (i in seq_len(m)) {
    for (j in seq_len(m)) {
      con[i, (i - 1L) * m + j] <- 1
    }
  }
  for (j in seq_len(m)) {
    for (i in seq_len(m)) {
      con[m + j, (i - 1L) * m + j] <- 1
    }
  }
  res <- lpSolve::lp(
    "min",
    obj,
    con,
    rep("=", 2L * m),
    rep(1, 2L * m),
    binary.vec = seq_len(nvar),
    presolve = 0
  )
  if (res$status != 0) return(NULL)
  sol <- res$solution
  perm <- integer(m)
  for (i in seq_len(m)) {
    for (j in seq_len(m)) {
      k <- (i - 1L) * m + j
      if (!is.na(sol[k]) && sol[k] > 0.5) perm[i] <- j
    }
  }
  if (any(perm == 0L)) return(NULL)
  perm
}

#' Bipartite assignment: min sum cost[i, p[i]]. Uses lpSolve; falls back to full permutation scan if m small.
assignment_min_cost <- function(cost) {
  m <- nrow(cost)
  if (m == 0L) return(integer(0))
  if (m == 1L) return(1L)
  stopifnot(ncol(cost) == m)
  if (requireNamespace("lpSolve", quietly = TRUE)) {
    p <- assignment_min_cost_lpsolve(cost)
    if (!is.null(p)) return(p)
  }
  if (m > 8L) return(NULL)
  best_p <- NULL
  best_v <- Inf
  for (perm in all_permutations(seq_len(m))) {
    v <- 0
    for (i in seq_len(m)) v <- v + cost[i, perm[i]]
    if (v < best_v) {
      best_v <- v
      best_p <- perm
    }
  }
  best_p
}

#' Generate all perfect matchings of a set of ids (list of list of pairs; each pair is sorted length-2 vector).
all_matchings <- function(ids) {
  n <- length(ids)
  if (n == 0L) return(list(list()))
  if (n == 2L) return(list(list(sort(ids))))
  res <- list()
  for (j in 2:n) {
    pair <- sort(c(ids[1L], ids[j]))
    rest <- ids[-c(1L, j)]
    for (m in all_matchings(rest)) {
      res[[length(res) + 1L]] <- c(list(pair), m)
    }
  }
  res
}

#' Score a matching (list of pairs, each pair = length-2 char vector) by total pair_count.
matching_score <- function(matching, pair_history) {
  total <- 0L
  for (pr in matching) {
    total <- total + pair_count(pr[1L], pr[2L], pair_history)
  }
  total
}

#' Best matching for even n: among all perfect matchings, the one with minimum total pair_count.
#' Optional count_matrix + count_ids slice avoids rebuilding counts from history (e.g. odd-n trio loop).
best_matching_even <- function(ids, pair_history, count_matrix = NULL, count_ids = NULL) {
  n <- length(ids)
  if (n == 0L) return(NULL)
  if (n == 2L) return(list(sort(ids)))
  if (n %% 2L == 1L) return(NULL)
  M <- NULL
  if (!is.null(count_matrix) && !is.null(count_ids)) {
    ii <- match(ids, count_ids)
    if (!anyNA(ii)) {
      M <- count_matrix[ii, ii, drop = FALSE]
      dimnames(M) <- list(ids, ids)
    }
  }
  if (is.null(M)) M <- pair_count_matrix(ids, pair_history)
  W <- pair_weight_from_matrix(M, jitter = TRUE)
  if (requireNamespace("lpSolve", quietly = TRUE)) {
    mm <- min_weight_perfect_matching_lpsolve(ids, W)
    if (!is.null(mm)) return(mm)
  }
  matchings <- all_matchings(ids)
  best <- NULL
  best_score <- Inf
  for (m in matchings) {
    sc <- matching_score(m, pair_history)
    if (sc < best_score) {
      best_score <- sc
      best <- m
    }
  }
  best
}

#' All permutations of a vector (for alternation-valid matchings).
all_permutations <- function(x) {
  n <- length(x)
  if (n <= 1L) return(list(x))
  res <- list()
  for (i in seq_len(n)) {
    rest <- x[-i]
    for (p in all_permutations(rest)) {
      res[[length(res) + 1L]] <- c(x[i], p)
    }
  }
  res
}

#' Best matching that respects alternation: each pair has one "want climb first" (pref 1) and one "want belay first" (pref -1).
#' A = pref 1 (belayed last), B = pref -1 (climbed last). When |A| == |B| == n/2 we can have zero doubles.
#' Returns list of role pairs (climbs_first, belays_first), or NULL to fall back to unconstrained.
best_matching_even_alternation <- function(ids, pair_history, pref, count_matrix = NULL, count_ids = NULL) {
  A <- ids[pref[ids] == 1L]
  B <- ids[pref[ids] == -1L]
  C <- ids[pref[ids] == 0L]
  nA <- length(A)
  nB <- length(B)
  n <- length(ids)
  stopifnot(n %% 2L == 0L)
  npairs <- n %/% 2L
  M <- if (!is.null(count_matrix) && identical(count_ids, ids)) {
    count_matrix
  } else {
    pair_count_matrix(ids, pair_history)
  }

  # Perfect alternation: min-cost assignment A <-> B (same objective as enumerating all bijections).
  if (nA == npairs && nB == npairs && length(C) == 0L) {
    cost <- matrix(0, npairs, npairs)
    ia <- match(A, rownames(M))
    jb <- match(B, colnames(M))
    for (i in seq_len(npairs)) {
      for (j in seq_len(npairs)) {
        cost[i, j] <- M[ia[i], jb[j]] + stats::runif(1L, 0, 1e-6)
      }
    }
    perm <- assignment_min_cost(cost)
    if (is.null(perm)) return(NULL)
    return(lapply(seq_len(npairs), function(i) {
      list(climbs_first = A[i], belays_first = B[perm[i]])
    }))
  }

  k <- min(nA, nB)
  if (k == 0L) return(NULL)
  remainder <- c(
    if (nA > k) A[seq(k + 1L, nA)] else character(0),
    if (nB > k) B[seq(k + 1L, nB)] else character(0),
    C
  )
  A_k <- A[seq_len(k)]
  B_k <- B[seq_len(k)]
  ia <- match(A_k, rownames(M))
  jb <- match(B_k, colnames(M))
  cost <- matrix(0, k, k)
  for (i in seq_len(k)) {
    for (j in seq_len(k)) {
      cost[i, j] <- M[ia[i], jb[j]] + stats::runif(1L, 0, 1e-6)
    }
  }
  perm <- assignment_min_cost(cost)
  if (is.null(perm)) return(NULL)
  ab_pairs <- lapply(seq_len(k), function(i) {
    list(climbs_first = A_k[i], belays_first = B_k[perm[i]])
  })

  if (length(remainder) == 0L) {
    return(ab_pairs)
  }
  if (length(remainder) == 2L) {
    a <- remainder[1L]
    b <- remainder[2L]
    if (pref[a] == 1L && pref[b] != 1L) {
      extra <- list(list(climbs_first = a, belays_first = b))
    } else if (pref[b] == 1L && pref[a] != 1L) {
      extra <- list(list(climbs_first = b, belays_first = a))
    } else {
      extra <- list(list(climbs_first = a, belays_first = b))
    }
    return(c(ab_pairs, extra))
  }
  rem_match <- best_matching_even(remainder, pair_history, count_matrix = M, count_ids = ids)
  if (is.null(rem_match)) return(NULL)
  rem_role <- lapply(rem_match, function(pr) {
    a <- pr[1L]
    b <- pr[2L]
    if (pref[a] == 1L && pref[b] != 1L) list(climbs_first = a, belays_first = b)
    else if (pref[b] == 1L && pref[a] != 1L) list(climbs_first = b, belays_first = a)
    else list(climbs_first = a, belays_first = b)
  })
  c(ab_pairs, rem_role)
}

#' Sum of M[u,v] over edges in a matching (pairs are sorted id character vectors).
matching_score_matrix <- function(pair_matching, M, ids) {
  total <- 0L
  for (pr in pair_matching) {
    u <- match(pr[1L], ids)
    v <- match(pr[2L], ids)
    total <- total + M[u, v]
  }
  total
}

#' Best partition for odd n: one trio + pairs. Trio score = sum of pair_count over the 3 pairs in the trio.
#' Among ties on total diversity, prefer trios with fewer people who were already in a past trio.
best_matching_odd <- function(ids, pair_history) {
  n <- length(ids)
  if (n < 3L) return(NULL)
  M <- pair_count_matrix(ids, pair_history)
  prior_trio_ids <- prior_trio_participant_ids(pair_history)
  best_trio <- NULL
  best_pairs <- NULL
  best_score <- Inf
  best_repeat <- Inf
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (j <= i) next
      for (k in seq_len(n)) {
        if (k <= j) next
        trio <- c(ids[i], ids[j], ids[k])
        rest <- setdiff(ids, trio)
        trio_score <- M[i, j] + M[i, k] + M[j, k]
        pair_matching <- best_matching_even(rest, pair_history, count_matrix = M, count_ids = ids)
        if (is.null(pair_matching)) next
        total <- trio_score + matching_score_matrix(pair_matching, M, ids)
        n_repeat <- trio_prior_repeat_count(trio, prior_trio_ids)
        if (total < best_score || (total == best_score && n_repeat < best_repeat)) {
          best_score <- total
          best_repeat <- n_repeat
          best_trio <- trio
          best_pairs <- pair_matching
        }
      }
    }
  }
  if (is.null(best_trio)) return(NULL)
  list(trio = best_trio, pairs = best_pairs)
}

#' Compute pairings for one round: best matching (min repeats), then assign climb/belay order.
#' last_action: "climb" | "belay" | "rest". Pref: belay->1 (climb first), climb->-1 (belay first), rest->0.
compute_pairings <- function(ids, last_action, round_number, participants, pair_history = list()) {
  n <- length(ids)
  if (n == 0) return(list())
  if (n == 1) return(list(list(type = "pair", climbs_first = ids[1], belays_first = NA_character_)))

  pref <- integer(length(ids))
  names(pref) <- ids
  for (i in seq_along(ids)) {
    la <- last_action[[ids[i]]]
    if (is.null(la) || length(la) == 0L || la == "" || la == "rest") pref[i] <- 0L
    else if (la == "belay") pref[i] <- 1L
    else pref[i] <- -1L
  }

  pairings <- list()

  if (n %% 2L == 1L) {
    # Odd: best trio + pairs
    out <- best_matching_odd(ids, pair_history)
    if (is.null(out)) return(list())
    trio_ids <- out$trio
    # Climb order: 1st = most wants to climb (pref 1), 3rd = most wants to belay (pref -1), 2nd = rest
    order_by_pref <- trio_ids[order(-pref[trio_ids])]
    pairings[[length(pairings) + 1L]] <- list(type = "trio", climb_order = order_by_pref)
    # Pairs with role assignment
    for (pr in out$pairs) {
      a <- pr[1L]
      b <- pr[2L]
      if (pref[a] == 1L && pref[b] != 1L)
        pairings[[length(pairings) + 1L]] <- list(type = "pair", climbs_first = a, belays_first = b)
      else if (pref[b] == 1L && pref[a] != 1L)
        pairings[[length(pairings) + 1L]] <- list(type = "pair", climbs_first = b, belays_first = a)
      else
        pairings[[length(pairings) + 1L]] <- list(type = "pair", climbs_first = a, belays_first = b)
    }
  } else {
    # Use alternation-valid (no doubles) whenever it achieves same or better diversity; only allow doubles when unconstrained has strictly fewer repeats.
    M_round <- pair_count_matrix(ids, pair_history)
    alt <- best_matching_even_alternation(ids, pair_history, pref, count_matrix = M_round, count_ids = ids)
    best_unconstrained <- best_matching_even(ids, pair_history, count_matrix = M_round, count_ids = ids)
    score_alt <- if (!is.null(alt)) {
      sum(vapply(alt, function(p) M_round[match(p$climbs_first, ids), match(p$belays_first, ids)], integer(1)))
    } else Inf
    score_unc <- if (!is.null(best_unconstrained)) matching_score_matrix(best_unconstrained, M_round, ids) else Inf
    use_alt <- !is.null(alt) && (is.null(best_unconstrained) || score_alt <= score_unc)
    if (use_alt) {
      for (p in alt) {
        pairings[[length(pairings) + 1L]] <- list(type = "pair", climbs_first = p$climbs_first, belays_first = p$belays_first)
      }
    } else if (!is.null(best_unconstrained)) {
      for (pr in best_unconstrained) {
        a <- pr[1L]
        b <- pr[2L]
        if (pref[a] == 1L && pref[b] != 1L)
          pairings[[length(pairings) + 1L]] <- list(type = "pair", climbs_first = a, belays_first = b)
        else if (pref[b] == 1L && pref[a] != 1L)
          pairings[[length(pairings) + 1L]] <- list(type = "pair", climbs_first = b, belays_first = a)
        else
          pairings[[length(pairings) + 1L]] <- list(type = "pair", climbs_first = a, belays_first = b)
      }
    }
  }

  pairings
}

#' Format pairings for display. Trio: 1. A climbs, B belays, C rests; 2. B climbs, C belays, A rests; 3. C climbs, A belays, B rests.
pairings_for_display <- function(pairings, participants) {
  get_name <- function(id) {
    if (is.na(id)) return("-")
    p <- participants[[id]]
    if (is.null(p)) return(id)
    p$name
  }
  out <- list()
  for (p in pairings) {
    if (p$type == "pair") {
      out[[length(out) + 1L]] <- list(
        type = "pair",
        climbs_first = get_name(p$climbs_first),
        belays_first = get_name(p$belays_first),
        climbs_first_id = p$climbs_first,
        belays_first_id = p$belays_first,
        climb_order = NULL
      )
    } else {
      ord <- p$climb_order
      # Slot 1: ord[1] climbs, ord[2] belays, ord[3] rests; slot 2: ord[2] climbs, ord[3] belays, ord[1] rests; slot 3: ord[3] climbs, ord[1] belays, ord[2] rests
      out[[length(out) + 1L]] <- list(
        type = "trio",
        climb_order = vapply(ord, get_name, character(1)),
        climb_order_ids = ord,
        climbs_first = get_name(ord[1L]),
        belays_first = get_name(ord[2L]),
        rest_first = get_name(ord[3L]),
        climbs_first_id = ord[1L],
        belays_first_id = ord[2L]
      )
    }
  }
  out
}
