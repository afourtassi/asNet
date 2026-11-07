# Profile likelihood ratios: how much does a child's word-by-word CDI profile,
# at their estimated ability, look like the ASD-sample DIF pattern rather than
# the Wordbank item bank? Builds on anchored_dif.R.

source("helper_functions/anchored_dif.R")

# Empirical-Bayes shrinkage of DIF estimates toward 0: delta * tau^2 / (tau^2 + se^2),
# with tau^2 = var(delta) - mean(se^2) estimated across informative items.
shrink_delta <- function(res) {
  inf <- res$informative
  tau2 <- max(var(res$delta[inf]) - mean(res$se[inf]^2), .01)
  ifelse(inf, res$delta * tau2 / (tau2 + res$se^2), 0)
}

# EAP ability from anchor items only, with the group's own N(mu, sigma) prior
anchor_theta <- function(X, items, anchors) {
  items <- items[match(colnames(X), items$definition), ]
  fit_ability(loglik_matrix(X, items$a, items$d, which(anchors)))$eap
}

# log LR = sum over items of log P(x | theta, a, d + delta) - log P(x | theta, a, d)
profile_llr <- function(X, items, theta, delta, use = rep(TRUE, ncol(X))) {
  items <- items[match(colnames(X), items$definition), ]
  eta0 <- outer(theta, items$a) + matrix(items$d, nrow(X), ncol(X), byrow = TRUE)
  eta1 <- eta0 + matrix(delta, nrow(X), ncol(X), byrow = TRUE)
  ll <- function(eta) ifelse(X == 1, plogis(eta, log.p = TRUE), plogis(-eta, log.p = TRUE))
  contrib <- ll(eta1) - ll(eta0)
  contrib[is.na(contrib)] <- 0
  drop(contrib[, use, drop = FALSE] %*% rep(1, sum(use)))
}

# AUC by the rank (Mann-Whitney) formula; higher score = more ASD-like
auc <- function(score, is_case) {
  r <- rank(score); n1 <- sum(is_case); n0 <- sum(!is_case)
  (sum(r[is_case]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

# For each case, the k nearest controls on theta (with replacement), within a caliper
match_on_theta <- function(theta_case, theta_ctrl, k = 3, caliper = .1) {
  o <- order(theta_ctrl); s <- theta_ctrl[o]
  map(theta_case, function(t) {
    j <- findInterval(t, s); cand <- max(1, j - k):min(length(s), j + k + 1)
    cand <- cand[order(abs(s[cand] - t))][1:k]
    o[cand[abs(s[cand] - t) <= caliper]]
  })
}
