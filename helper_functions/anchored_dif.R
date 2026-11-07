# DIF against a fixed Wordbank 2PL item bank.
#
# Item parameters (a, d) come from the Wordbank English calibration
# (in_files/wordbank_2pl_items.csv, see export_wordbank_params.R) and are held
# fixed. The ASD sample's ability distribution (mean, sd on the Wordbank scale)
# is estimated by EM over a quadrature grid, using anchor items only. For each
# item, DIF is then estimated by maximising the expected log-likelihood under
# each child's posterior over ability:
#   uniform DIF     P(y = 1 | theta) = logistic(a * theta + d + delta)
#   non-uniform DIF P(y = 1 | theta) = logistic((1 + gamma) * a * theta + d + delta)
# delta < 0 means the item is harder for the ASD sample than Wordbank children
# of the same ability. Anchor items use a leave-item-out posterior so an item's
# own response does not inform the ability it is judged against.
# Anchors are purified iteratively: items flagged for DIF are dropped from the
# anchor set and the sample is re-scored until the anchor set stops changing.

suppressPackageStartupMessages(library(tidyverse))

QUAD <- seq(-6, 6, length.out = 121)

# Long NDAR data -> response matrix (rows = administrations, cols = Wordbank
# definitions). WG lists "in" and "inside" separately; WS (and the item bank)
# has one item "inside/in", produced if either is produced.
response_matrix <- function(d_long, items) {
  d_long <- d_long %>%
    mutate(definition = ifelse(definition %in% c("in", "inside"), "inside/in", definition)) %>%
    group_by(row, definition) %>%
    summarise(value = if (all(is.na(value))) NA else any(value, na.rm = TRUE), .groups = "drop")
  w <- d_long %>% filter(definition %in% items$definition) %>%
    pivot_wider(names_from = definition, values_from = value)
  X <- as.matrix(w[, -1]) * 1
  rownames(X) <- w$row
  X
}

# log P(y | theta_q) summed over the given items: n x Q matrix
loglik_matrix <- function(X, a, d, cols = seq_len(ncol(X))) {
  X <- X[, cols, drop = FALSE]; a <- a[cols]; d <- d[cols]
  eta <- outer(d, rep(1, length(QUAD))) + outer(a, QUAD)   # I x Q
  lp1 <- plogis(eta, log.p = TRUE); lp0 <- plogis(-eta, log.p = TRUE)
  obs <- !is.na(X); X1 <- ifelse(obs, X, 0); X0 <- obs - X1
  X1 %*% lp1 + X0 %*% lp0
}

row_softmax <- function(L) { L <- L - apply(L, 1, max); W <- exp(L); W / rowSums(W) }

# EM for the sample's ability distribution N(mu, sigma) with items fixed
fit_ability <- function(LL, max_iter = 200, tol = 1e-6) {
  mu <- 0; sigma <- 1
  for (it in seq_len(max_iter)) {
    W <- row_softmax(sweep(LL, 2, dnorm(QUAD, mu, sigma, log = TRUE), "+"))
    eap <- drop(W %*% QUAD)
    psd2 <- drop(W %*% QUAD^2) - eap^2
    mu_new <- mean(eap); sigma_new <- sqrt(mean(psd2 + (eap - mu_new)^2))
    done <- abs(mu_new - mu) + abs(sigma_new - sigma) < tol
    mu <- mu_new; sigma <- sigma_new
    if (done) break
  }
  W <- row_softmax(sweep(LL, 2, dnorm(QUAD, mu, sigma, log = TRUE), "+"))
  list(mu = mu, sigma = sigma, W = W, eap = drop(W %*% QUAD),
       psd = sqrt(pmax(drop(W %*% QUAD^2) - drop(W %*% QUAD)^2, 0)))
}

# Newton-Raphson on the posterior-weighted log-likelihood of one item.
# A, B: expected counts of produced / not produced at each quadrature point.
fit_item_dif <- function(A, B, a, d, nonuniform = FALSE) {
  par <- c(0, 0); k <- if (nonuniform) 2 else 1
  ll <- function(p) { eta <- (1 + p[2]) * a * QUAD + d + p[1]
                      sum(A * plogis(eta, log.p = TRUE) + B * plogis(-eta, log.p = TRUE)) }
  for (it in 1:50) {
    eta <- (1 + par[2]) * a * QUAD + d + par[1]
    p <- plogis(eta); r <- A - (A + B) * p; w <- (A + B) * p * (1 - p)
    x <- cbind(1, a * QUAD)[, 1:k, drop = FALSE]
    g <- colSums(x * r); H <- crossprod(x, x * w)
    step <- tryCatch(solve(H, g), error = function(e) rep(0, k))
    step <- pmax(pmin(step, 2), -2)
    par[1:k] <- par[1:k] + step
    if (max(abs(step)) < 1e-6) break
  }
  eta <- (1 + par[2]) * a * QUAD + d + par[1]; p <- plogis(eta); w <- (A + B) * p * (1 - p)
  x <- cbind(1, a * QUAD)[, 1:k, drop = FALSE]
  se <- sqrt(diag(solve(crossprod(x, x * w))))
  list(delta = par[1], gamma = par[2], se_delta = se[1],
       se_gamma = if (nonuniform) se[2] else NA_real_, loglik = ll(par), loglik0 = ll(c(0, 0)))
}

# DIF for every item given the anchor set
dif_all_items <- function(X, items, anchors, fit) {
  a <- items$a; d <- items$d
  obs <- !is.na(X); X1 <- ifelse(obs, X, 0); X0 <- obs - X1
  lprior <- dnorm(QUAD, fit$mu, fit$sigma, log = TRUE)
  LL_anchor <- loglik_matrix(X, a, d, which(anchors))
  map_dfr(seq_len(ncol(X)), function(i) {
    L <- LL_anchor
    if (anchors[i]) {  # leave the item's own response out of its ability posterior
      eta <- d[i] + a[i] * QUAD
      L <- L - outer(X1[, i], plogis(eta, log.p = TRUE)) - outer(X0[, i], plogis(-eta, log.p = TRUE))
    }
    W <- row_softmax(sweep(L, 2, lprior, "+"))
    A <- colSums(W * X1[, i]); B <- colSums(W * X0[, i])
    u <- fit_item_dif(A, B, a[i], d[i])
    nu <- fit_item_dif(A, B, a[i], d[i], nonuniform = TRUE)
    # average change in P(produce) across the ASD sample's ability posterior
    th <- colSums(W[obs[, i], , drop = FALSE]) / sum(obs[, i])
    dp <- sum(th * (plogis(a[i] * QUAD + d[i] + u$delta) - plogis(a[i] * QUAD + d[i])))
    tibble(definition = colnames(X)[i], n = sum(obs[, i]), prop = mean(X[, i], na.rm = TRUE),
           delta = u$delta, se = u$se_delta, z = u$delta / u$se_delta,
           p = 2 * pnorm(-abs(u$delta / u$se_delta)), dprob = dp,
           gamma = nu$gamma, lr_nonuniform = 2 * (nu$loglik - u$loglik),
           p_nonuniform = pchisq(2 * (nu$loglik - u$loglik), 1, lower.tail = FALSE))
  })
}

# Iterative anchor purification. Purify liberally, flag conservatively:
#   dropped from anchors: BH-adjusted p < alpha and |delta| > purify_delta
#   flagged (large DIF):  BH-adjusted p < alpha and |delta| > flag_delta
# (delta = 1 logit is an odds ratio of 2.7 at fixed ability)
anchored_dif <- function(X, items, alpha = .05, purify_delta = .5, flag_delta = 1,
                         max_iter = 20) {
  items <- items[match(colnames(X), items$definition), ]
  # items nobody / everybody in the sample produces carry no DIF information
  informative <- colSums(X, na.rm = TRUE) > 0 & colSums(1 - X, na.rm = TRUE) > 0
  anchors <- informative
  history <- integer(0)
  for (it in seq_len(max_iter)) {
    fit <- fit_ability(loglik_matrix(X, items$a, items$d, which(anchors)))
    res <- dif_all_items(X, items, anchors, fit) %>%
      mutate(q = p.adjust(p, "BH"), informative = informative,
             size = case_when(!informative | q >= alpha ~ "none",
                              abs(delta) > flag_delta ~ "large",
                              abs(delta) > purify_delta ~ "moderate",
                              TRUE ~ "negligible"),
             flagged = size == "large")
    new_anchors <- informative & !(res$size %in% c("large", "moderate"))
    history <- c(history, sum(new_anchors))
    if (identical(new_anchors, anchors)) break
    anchors <- new_anchors
  }
  list(items = res %>% mutate(anchor = anchors), mu = fit$mu, sigma = fit$sigma,
       theta = tibble(row = as.integer(rownames(X)), theta = fit$eap, theta_se = fit$psd),
       iterations = it, anchor_history = history)
}

# Ability -> "language age": the age at which the Wordbank median ability equals
# theta (linear interpolation over monthly medians; NA outside the norm range).
language_age <- function(theta, norms) {
  nm <- norms %>% filter(n >= 50) %>% arrange(age)
  m <- cummax(nm$q50)  # monotone for interpolation
  approx(m, nm$age, xout = theta, ties = "ordered")$y
}
