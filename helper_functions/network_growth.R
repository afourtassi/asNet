# Network growth helpers (replaces the rwebppl/espeak pipeline in the old 03_networks.Rmd)
#
# Networks are built over a fixed word set (CDI:WS nouns), so TD and ASD share the
# same semantic and phonological networks; only the order of acquisition (AoA) differs.

# Per-item AoA: age at which a logistic fit of P(produces) ~ age crosses .5.
# d: one row per administration x item, with columns definition, age, produces (0/1)
estimate_aoa <- function(d) {
  d %>%
    group_by(definition) %>%
    group_modify(function(x, key) {
      m <- suppressWarnings(glm(produces ~ age, family = binomial, data = x))
      b <- coef(m)
      tibble(intercept = b[1], slope = b[2], aoa = -b[1] / b[2],
             prop_produced = mean(x$produces), n = nrow(x))
    }) %>%
    ungroup()
}

# Undirected adjacency matrix (0/1) over `words`, from free-association cue/target pairs
assoc_adjacency <- function(words, lemmas, cue_target) {
  ct <- cue_target %>% filter(cue %in% lemmas, target %in% lemmas, cue != target)
  idx <- setNames(seq_along(words), lemmas)
  A <- matrix(0, length(words), length(words), dimnames = list(words, words))
  A[cbind(idx[ct$cue], idx[ct$target])] <- 1
  A <- pmax(A, t(A))
  diag(A) <- 0
  A
}

# Undirected adjacency matrix: words linked if IPA edit distance <= threshold
phono_adjacency <- function(words, ipa, threshold = 2) {
  D <- adist(ipa)
  A <- (D <= threshold) * 1
  diag(A) <- 0
  dimnames(A) <- list(words, words)
  A
}

# Growth data: for each month, every word not yet learned before that month is a
# candidate; `learned` = 1 if its (rounded) AoA falls in that month.
# PAC = degree in the full network. PAT = mean degree, within the network of
# already-known words, of the known words the candidate is linked to (0 if none).
make_growth <- function(aoa_month, nets, months) {
  words <- names(aoa_month)
  out <- vector("list", length(months))
  for (i in seq_along(months)) {
    m <- months[i]
    cand <- words[aoa_month >= m]
    known <- words[aoa_month < m]
    if (!length(cand)) next
    feats <- lapply(nets, function(A) {
      pac <- rowSums(A[cand, , drop = FALSE])
      if (length(known)) {
        deg_known <- rowSums(A[known, known, drop = FALSE])
        L <- A[cand, known, drop = FALSE]
        nlinks <- rowSums(L)
        pat <- ifelse(nlinks > 0, as.vector(L %*% deg_known) / pmax(nlinks, 1), 0)
      } else pat <- rep(0, length(cand))
      list(PAC = pac, PAT = pat)
    })
    g <- tibble(age = m, definition = cand, learned = as.integer(aoa_month[cand] == m))
    for (n in names(feats)) {
      g[[paste0(n, "_PAC")]] <- feats[[n]]$PAC
      g[[paste0(n, "_PAT")]] <- feats[[n]]$PAT
    }
    out[[i]] <- g
  }
  bind_rows(out)
}

# Softmax choice model (the likelihood the old webppl models optimized):
# each month, every learned word is a draw from softmax(X %*% alpha) over the candidates.
# Fit by maximum likelihood; SEs from the Hessian.
fit_growth <- function(g, preds) {
  X <- as.matrix(g[, preds])
  month <- g$age
  learned <- g$learned == 1
  groups <- split(seq_len(nrow(g)), month)
  nll <- function(a) {
    u <- as.vector(X %*% a)
    -sum(vapply(groups, function(ix) {
      ul <- u[ix]
      mx <- max(ul)
      sum(ul[learned[ix]]) - sum(learned[ix]) * (mx + log(sum(exp(ul - mx))))
    }, numeric(1)))
  }
  fit <- optim(rep(0, length(preds)), nll, method = "BFGS", hessian = TRUE)
  se <- sqrt(diag(solve(fit$hessian)))
  ll0 <- -nll(rep(0, length(preds)))
  tibble(predictor = preds, estimate = fit$par, se = se,
         lower = fit$par - 1.96 * se, upper = fit$par + 1.96 * se,
         logLik = -fit$value, logLik_null = ll0,
         mcfadden_r2 = 1 - (-fit$value) / ll0,
         n_learned = sum(learned))
}
