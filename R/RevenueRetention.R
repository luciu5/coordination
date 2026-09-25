# Exact retained-profit equations for heterogeneous product revenue retention.
# Effective costs stay in mcPre/mcPost; ownership and firm roles stay economic.

.coord_retention <- function(object, preMerger = TRUE) {
  antitrust::getRetention(object, preMerger = preMerger)
}

.coord_mixed_retention <- function(object, preMerger = TRUE, subset = NULL) {
  n <- length(object@shares)
  if (is.null(subset)) subset <- if (preMerger) rep(TRUE, n) else object@subset
  owner <- if (preMerger) object@firmOwnerPre else object@firmOwnerPost
  r <- .coord_retention(object, preMerger)
  mixed <- any(vapply(split(r[subset], owner[subset]), function(z)
    any(abs(log(z) - log(z[1L])) > 1e-10), logical(1)))
  if (mixed && !isTRUE(object@output)) stop("heterogeneous revenue retention requires an output market")
  mixed
}

.coord_retained_demand <- function(object, prices, subset) {
  ix <- which(subset)
  if (methods::is(object, "CES")) {
    d <- .cf_ces_demand(object, prices, subset)
    q <- d$quantities[ix]; s <- d$revenue[ix]; p <- prices[ix]
    g <- object@slopes$gamma
    J <- outer(q, (g - 1) * s / p)
    diag(J) <- diag(J) - g * q / p
    normalized <- matrix(rep((g - 1) * q / object@mktSize, each = length(q)), length(q))
    diag(normalized) <- diag(normalized) - g / p
    b <- (g - 1) / (g * (1 + (g - 1) * (1 - sum(s))))
    K <- -b * outer(p, p) / object@mktSize
    diag(K) <- diag(K) - p / (g * q)
  } else {
    s <- .cf_logit_shares(object, prices, subset)[ix]
    q <- object@mktSize * s
    J <- object@mktSize * object@slopes$alpha * (diag(s, length(s)) - tcrossprod(s))
    normalized <- object@slopes$alpha * (diag(length(s)) - matrix(rep(s, each = length(s)), length(s)))
    K <- matrix(1 / (object@slopes$alpha * object@mktSize * (1 - sum(s))), length(s), length(s))
    diag(K) <- diag(K) + 1 / (object@slopes$alpha * q)
  }
  list(q = q, J = J, normalized = normalized, shares = s,
    K = if (object@conduct == "cournot") K else NULL,
    prices = prices[ix])
}

.coord_retained_margins <- function(object, preMerger = TRUE, level = FALSE) {
  n <- length(object@shares)
  subset <- if (preMerger) rep(TRUE, n) else object@subset
  ix <- which(subset)
  p <- if (preMerger) object@pricePre else object@pricePost
  owner <- if (preMerger) object@firmOwnerPre else object@firmOwnerPost
  r <- .coord_retention(object, preMerger)[ix]
  d <- .coord_retained_demand(object, p, subset)
  O <- outer(owner[ix], owner[ix], "==") * 1
  is_core <- methods::is(object, "CoreFringeLogit") || methods::is(object, "CoreFringeCES")
  core <- if (is_core) {
    if (preMerger) object@corePre else object@corePost
  } else unique(owner)
  fringe <- is_core & !(owner[ix] %in% core)
  if (any(fringe)) {
    # Atomistic fringe ignores diversion, including within a common firm ID.
    O[fringe, ] <- 0
    diag(O)[fringe] <- 1
  }
  B <- sweep(O, 2L, r, "*")
  if (object@conduct == "bertrand") {
    A <- B * d$normalized
    rhs <- -r
    if (any(fringe)) {
      A[fringe, ] <- 0
      perceived <- if (methods::is(object, "CES")) {
        -object@slopes$gamma / p[ix]
      } else rep(object@slopes$alpha, length(ix))
      diag(A)[fringe] <- r[fringe] * perceived[fringe]
    }
    margin <- as.vector(solve(A, rhs))
  } else {
    margin <- -as.vector((B * t(d$K)) %*% d$q) / r
    if (any(fringe)) margin[fringe] <- if (methods::is(object, "CES")) {
      p[ix][fringe] / object@slopes$gamma
    } else -1 / object@slopes$alpha
  }
  if (!is_core) {
    leaders <- if (preMerger) object@leadersPre else object@leadersPost
    li <- which(owner[ix] %in% leaders); fi <- which(!(owner[ix] %in% leaders))
    if (length(li) && length(fi)) {
      # At the candidate demand, invert follower costs from their weighted
      # Nash equations, then differentiate those equations at fixed costs.
      tmp <- object
      costs <- p
      costs[ix] <- p[ix] - margin
      if (preMerger) tmp@mcPre <- costs else tmp@mcPost <- costs
      R <- if (methods::is(object, "CES")) {
        .ces_implicit_response_at(tmp, preMerger, subset)
      } else .sk_implicit_response_at(tmp, preMerger, subset)
      D <- if (object@conduct == "bertrand") d$J else d$K
      reduced <- D[, li, drop = FALSE] + D[, fi, drop = FALSE] %*% R
      if (object@conduct == "bertrand") {
        A <- B[li, li, drop = FALSE] * sweep(t(reduced[li, , drop = FALSE]), 1L, d$q[li], "/")
        margin[li] <- as.vector(solve(A, -r[li]))
      } else {
        margin[li] <- -as.vector((B[li, , drop = FALSE] * t(reduced)) %*% d$q) / r[li]
      }
    }
  }
  out <- rep(NA_real_, n)
  out[ix] <- if (level) margin else margin / p[ix]
  names(out) <- object@labels
  out
}

# The implicit-function response uses a row-scaled exact follower Hessian.
# Scaling avoids loss of rank when a high tariff makes one product's demand
# tiny. Unlike differencing raw profit gradients, it remains informative for
# near-zero (but positive) imported quantities.
.coord_retained_response <- function(object, preMerger, subset) {
  st <- .sk_state(object, preMerger, subset); ix <- which(subset)
  owner <- st$owner[ix]; r <- .coord_retention(object, preMerger)[ix]
  p <- st$prices[ix]; cost <- st$costs[ix]; mu <- p - cost
  d <- .coord_retained_demand(object, st$prices, subset)
  q <- d$q; s <- d$shares; n <- length(ix)
  O <- outer(owner, owner, "==") * 1
  li <- which(owner %in% st$leaders); fi <- which(!(owner %in% st$leaders))
  H <- matrix(0, n, n)
  if (methods::is(object, "CES")) {
    g <- object@slopes$gamma; h <- g - 1; E <- object@mktSize
    if (object@conduct == "bertrand") {
      for (i in seq_len(n)) {
        own <- O[i, ] > 0; profit <- sum(r[own] * mu[own] * q[own])
        v <- 1 - g * mu[i] / p[i] + h * profit / (r[i] * E)
        dh <- q * (own * r * (1 - g * mu / p) + h * profit / E)
        H[i, ] <- h * dh / (r[i] * E) + v * h * s / p
        H[i, i] <- H[i, i] - g * cost[i] / p[i]^2 - v * g / p[i]
      }
    } else {
      s0 <- 1 - sum(s); C <- 1 + h * s0; b <- h / (g * C)
      for (i in seq_len(n)) {
        T <- sum(O[i, ] * r * s)
        v <- h / g - b * T / r[i]
        H[i, ] <- q[i] * p / E * (-b * v -
          T / r[i] * h^3 * s0 / (g * C^3) -
          b / r[i] * (h / g * O[i, ] * r - b * T))
        H[i, i] <- H[i, i] - v / g
      }
    }
  } else {
    beta <- object@slopes$alpha; M <- object@mktSize; s0 <- 1 - sum(s)
    if (object@conduct == "bertrand") {
      for (i in seq_len(n)) {
        own <- O[i, ] > 0; profit <- sum(r[own] * mu[own] * s[own])
        v <- 1 + beta * (mu[i] - profit / r[i])
        dh <- s * (own * r * (1 + beta * mu) - beta * profit)
        H[i, ] <- beta * (-dh / r[i] - v * s)
        H[i, i] <- H[i, i] + beta * (1 + v)
      }
    } else {
      for (i in seq_len(n)) {
        T <- sum(O[i, ] * r * s)
        H[i, ] <- q[i] / (M * s0) * (1 + O[i, ] * r / r[i] + T / (r[i] * s0))
        H[i, i] <- H[i, i] + 1
      }
    }
  }
  out <- if (length(fi) && length(li)) -solve(H[fi, fi, drop = FALSE], H[fi, li, drop = FALSE]) else matrix(numeric(), length(fi), length(li))
  dimnames(out) <- list(object@labels[ix[fi]], object@labels[ix[li]])
  out
}

.coord_retained_root <- function(object, preMerger, subset) {
  n <- length(object@shares)
  costs <- if (preMerger) object@mcPre else object@mcPost
  start <- if (preMerger) object@priceStart else object@pricePost
  if (length(start) != n || any(!is.finite(start[subset])) || any(start[subset] <= 0)) start <- object@prices
  foc <- function(z) {
    p <- rep(NA_real_, n); p[subset] <- exp(z)
    if (any(!is.finite(p[subset]))) return(rep(1e6, sum(subset)))
    tmp <- object
    if (preMerger) tmp@pricePre <- p else { tmp@pricePost <- p; tmp@subset <- subset }
    m <- try(.coord_retained_margins(tmp, preMerger, TRUE), silent = TRUE)
    if (inherits(m, "try-error") || any(!is.finite(m[subset]))) return(rep(1e6, sum(subset)))
    (p[subset] - costs[subset] - m[subset]) / pmax(p[subset], 1)
  }
  z <- log(pmax(start[subset], .Machine$double.eps^0.25))
  ctl <- list(ftol = 1e-11, xtol = 1e-11, maxit = as.integer(object@control.equ$maxit %||% 500L))
  sol <- try(nleqslv::nleqslv(z, foc, method = "Broyden", control = ctl), silent = TRUE)
  if (!inherits(sol, "try-error") && all(is.finite(sol$x))) z <- sol$x
  if (max(abs(foc(z))) > 2e-8) {
    bb <- try(BB::BBsolve(z, foc, quiet = TRUE,
      control = list(tol = 1e-12, maxit = ctl$maxit)), silent = TRUE)
    if (!inherits(bb, "try-error") && all(is.finite(bb$par))) z <- bb$par
  }
  if (any(!is.finite(foc(z))) || max(abs(foc(z))) > 2e-8) {
    stop("retained-profit equilibrium residual exceeds tolerance")
  }
  exp(z)
}

.coord_retained_slopes <- function(object) {
  if (!isTRUE(object@output)) stop("heterogeneous revenue retention requires an output market")
  n <- length(object@shares); p <- object@prices; s <- object@shares
  object@pricePre <- object@pricePost <- p
  object@mktSize <- object@insideSize / sum(s)
  observed <- object@diagnostics$observedMargins
  w <- object@weights
  use <- is.finite(observed) & observed > 0 & is.finite(w) & w > 0
  if (methods::is(object, "CES")) {
    at <- function(g) {
      obj <- object
      obj@slopes <- list(alpha = 1 / sum(s) - 1, gamma = g,
        meanval = (s / (1 - sum(s))) * (p / obj@priceOutside)^(g - 1))
      obj
    }
    objective <- function(g) {
      mm <- try(.coord_retained_margins(at(g), TRUE), silent = TRUE)
      if (inherits(mm, "try-error") || any(!is.finite(mm[use])) || any(mm[use] <= 0)) return(Inf)
      sum(w[use] * (log(observed[use] / mm[use]))^2)
    }
    fixed <- is.finite(object@gammaFixed)
    if (!fixed && !any(use)) stop("no usable margins; supply a fixed gamma")
    if (fixed) g <- object@gammaFixed else {
      lo <- object@control.slopes$lower %||% object@control.slopes$lowerGamma %||% (1 + 1e-6)
      hi <- object@control.slopes$upper %||% object@control.slopes$upperGamma %||% 100
      grid <- exp(seq(log(lo - 1), log(hi - 1), length.out = 61L)) + 1
      values <- vapply(grid, objective, numeric(1))
      best <- which.min(values)
      opt <- stats::optimize(objective, c(grid[max(1L, best - 1L)], grid[min(length(grid), best + 1L)]), tol = 1e-9)
      g <- opt$minimum
    }
    object <- at(g); parameter <- g
    fit <- list(gamma = g, objective = objective(g), usable = use)
  } else {
    object@slopes <- list(alpha = -1,
      meanval = log(s / (1 - sum(s))) + (p - object@priceOutside))
    h <- .coord_retained_margins(object, TRUE, TRUE)
    fixed <- is.finite(object@alphaFixed)
    if (!fixed && !any(use)) stop("no usable margins; supply a fixed alpha")
    a <- if (fixed) object@alphaFixed else {
      x <- h / p
      sum(w[use] * x[use]^2) / sum(w[use] * x[use] * observed[use])
    }
    if (!is.finite(a) || a <= 0) stop("retained-profit margins do not identify positive alpha")
    object@slopes <- list(alpha = -a,
      meanval = log(s / (1 - sum(s))) + a * (p - object@priceOutside))
    parameter <- a; fit <- list(alpha = a, usable = use)
  }
  implied <- .coord_retained_margins(object, TRUE)
  object@diagnostics <- modifyList(object@diagnostics,
    list(demandparam = parameter, impliedMargins = implied,
      residuals = observed - implied, usable = use, calibration = fit,
      retention = .coord_retention(object, TRUE)))
  if (methods::is(object, "CES")) {
    object@diagnostics$gamma <- parameter
    object@diagnostics$fixedGamma <- fixed
  } else {
    object@diagnostics$alpha <- parameter
    object@diagnostics$beta <- -parameter
    object@diagnostics$fixedAlpha <- fixed
  }
  object
}

.coord_retained_followers <- function(object, leaderActions, preMerger, start, subset) {
  st <- .sk_state(object, preMerger, subset)
  active <- which(subset); n <- length(subset)
  li <- which(subset & st$owner %in% st$leaders)
  fi <- which(subset & !(st$owner %in% st$leaders))
  if (length(leaderActions) == n) leaderActions <- leaderActions[li]
  if (length(leaderActions) != length(li) || any(!is.finite(leaderActions)) || any(leaderActions <= 0)) stop("invalid leader actions")
  base <- .coord_retained_demand(object, st$prices, subset)
  if (is.null(start)) start <- if (object@conduct == "bertrand") st$prices[fi] else base$q[match(fi, active)]
  candidate <- function(z) {
    a <- rep(0, n); a[li] <- leaderActions; a[fi] <- exp(z)
    if (object@conduct == "bertrand") {
      p <- a
      q <- rep(0, n); q[active] <- .coord_retained_demand(object, p, subset)$q
    } else {
      q <- a; s <- q / object@mktSize
      if (sum(s[active]) >= 1) return(list(residual = rep(1e6, length(fi))))
      p <- object@priceOutside + (log(s / (1 - sum(s[active]))) - object@slopes$meanval) / object@slopes$alpha
    }
    raw <- .sk_raw_foc(object, a[active], preMerger, subset)[match(fi, active)]
    scale <- .coord_retention(object, preMerger)[fi] * if (object@conduct == "bertrand") q[fi] else pmax(abs(p[fi]), 1)
    list(residual = raw / pmax(scale, .Machine$double.xmin), raw = raw,
      prices = p, quantities = q, choices = a)
  }
  z <- log(start)
  if (length(fi)) {
    sol <- nleqslv::nleqslv(z, function(z) candidate(z)$residual,
      control = list(ftol = 1e-12, xtol = 1e-12, maxit = 500L))
    z <- sol$x
  }
  got <- candidate(z)
  if (any(!is.finite(got$residual)) || max(c(0, abs(got$residual))) > 1e-8) stop("retained-profit follower residual exceeds tolerance")
  list(choices = got$choices, prices = got$prices, quantities = got$quantities,
    shares = got$quantities / object@mktSize, residuals = got$raw,
    normalizedResiduals = got$residual,
    leaderProducts = li, followerProducts = fi, converged = TRUE)
}
