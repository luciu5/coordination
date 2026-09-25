#' @rdname StackelbergLogit
#' @exportClass StackelbergCES
# Noncooperative Stackelberg games with CES demand.
#
# This file contains the CES specialization of the common Stackelberg API.
# The equations here use unconditional revenue shares and antitrust's CES
# expenditure normalization.  In particular, no Logit quantity-share formula
# is reused for CES calibration or strategic derivatives.

setClass(
  Class = "StackelbergCES",
  contains = "CES",
  representation = representation(
    conduct = "character",
    leadersPre = "character",
    leadersPost = "character",
    firmOwnerPre = "character",
    firmOwnerPost = "character",
    diagnostics = "list",
    gammaFixed = "numeric"
  ),
  prototype = prototype(
    conduct = "bertrand",
    leadersPre = character(),
    leadersPost = character(),
    firmOwnerPre = character(),
    firmOwnerPost = character(),
    diagnostics = list(),
    gammaFixed = NA_real_
  ),
  validity = function(object) {
    n <- length(object@shares)
    if (length(object@conduct) != 1L || !object@conduct %in% c("bertrand", "cournot")) {
      return("'conduct' must be 'bertrand' or 'cournot'")
    }
    if (!isTRUE(object@output)) return("StackelbergCES currently supports output markets only")
    if (length(object@firmOwnerPre) != n || length(object@firmOwnerPost) != n) {
      return("'firmOwnerPre' and 'firmOwnerPost' must have one ID per product")
    }
    if (anyNA(object@firmOwnerPre) || anyNA(object@firmOwnerPost) ||
        any(!nzchar(object@firmOwnerPre)) || any(!nzchar(object@firmOwnerPost))) {
      return("firm ownership IDs must be non-missing and non-empty")
    }
    if (any(!object@leadersPre %in% unique(object@firmOwnerPre)) ||
        any(!object@leadersPost %in% unique(object@firmOwnerPost))) {
      return("leader IDs must occur in the corresponding firm ownership vector")
    }
    if (length(object@gammaFixed) != 1L ||
        (!is.na(object@gammaFixed) &&
         (!is.finite(object@gammaFixed) || object@gammaFixed <= 1))) {
      return("'gammaFixed' must be greater than one or NA before calibration")
    }
    if (length(object@priceOutside) != 1L || !is.finite(object@priceOutside) ||
        object@priceOutside <= 0) return("priceOutside must be positive for StackelbergCES")
    TRUE
  }
)

.ces_gamma <- function(object) {
  g <- object@slopes$gamma
  if (length(g) != 1L || !is.finite(g) || g <= 1) stop("StackelbergCES elasticity is unavailable")
  as.numeric(g)
}

.ces_active_state <- function(object, preMerger = TRUE, subset = NULL) {
  subset <- .sk_active(object, preMerger, subset)
  st <- .sk_state(object, preMerger, subset)
  list(subset = subset, owner = st$owner, leaders = st$leaders,
       prices = st$prices, costs = st$costs, roles = st$roles,
       gamma = .ces_gamma(object), E = as.numeric(object@mktSize),
       p0 = as.numeric(object@priceOutside))
}

# Demand evaluated from prices, returning revenue shares, quantity shares,
# quantities, and the outside revenue share.  Inactive products are omitted
# from the demand system while retaining their original vector positions.
.ces_demand_from_prices <- function(object, prices, subset) {
  n <- length(object@shares)
  g <- .ces_gamma(object); h <- g - 1
  p <- as.numeric(prices)
  if (length(p) != n || any(!is.finite(p[subset])) || any(p[subset] <= 0)) {
    stop("CES prices must be finite and positive on active products")
  }
  A <- as.numeric(object@slopes$meanval)
  if (length(A) != n || any(!is.finite(A[subset])) || any(A[subset] <= 0)) {
    stop("CES mean values are unavailable on active products")
  }
  z <- A[subset] * p[subset]^(-h)
  outz <- object@priceOutside^(-h)
  den <- outz + sum(z)
  r <- rep(0, n); r[subset] <- z / den
  r0 <- outz / den
  q <- rep(0, n); q[subset] <- object@mktSize * r[subset] / p[subset]
  sden <- r0 / object@priceOutside + sum(r[subset] / p[subset])
  s <- rep(0, n); s[subset] <- (r[subset] / p[subset]) / sden
  list(prices = p, revenue = r, quantityShare = s, quantities = q, r0 = r0,
       quantityOutside = (r0 / object@priceOutside) / sden)
}

.ces_demand_from_quantities <- function(object, quantities, subset) {
  n <- length(object@shares)
  g <- .ces_gamma(object); h <- g - 1
  q <- as.numeric(quantities)
  if (length(q) != n || any(!is.finite(q[subset])) || any(q[subset] <= 0)) {
    stop("CES quantities must be finite and positive on active products")
  }
  A <- as.numeric(object@slopes$meanval)
  if (length(A) != n || any(!is.finite(A[subset])) || any(A[subset] <= 0)) {
    stop("CES mean values are unavailable on active products")
  }
  ## The scalar w inversion is done in log space.  It has a unique root for
  ## every finite positive quantity vector because nominal expenditure is E.
  lse <- function(z) {
    m <- max(z)
    m + log(sum(exp(z - m)))
  }
  logB <- lse((1 / g) * log(A[subset]) + (h / g) * log(q[subset]))
  logc0 <- (1 - g) * log(object@priceOutside)
  E <- object@mktSize
  f <- function(y) {
    lse(c(logc0 + g * y, logB + y)) - log(E)
  }
  hi <- min((log(E) - logc0) / g, log(E) - logB) + log(2)
  while (f(hi) < 0) hi <- hi + log(2)
  lo <- hi - log(2)
  while (f(lo) > 0) lo <- lo - log(2)
  root <- stats::uniroot(f, c(lo, hi), tol = 1e-14)$root
  logw <- root
  logp <- logw + (log(A[subset]) - log(q[subset])) / g
  p <- rep(NA_real_, n); p[subset] <- exp(logp)
  ## Revenue shares follow directly from p*q/E and are more stable than
  ## re-evaluating a nearly singular power expression.
  r <- rep(0, n); r[subset] <- p[subset] * q[subset] / E
  logr0 <- logc0 + g * logw - log(E)
  r0 <- exp(logr0)
  if (!is.finite(r0) || r0 <= 0) stop("CES inverse demand produced an invalid outside share")
  rsum <- sum(r[subset])
  if (!is.finite(rsum) || abs(r0 + rsum - 1) > 2e-8) stop("CES inverse demand expenditure identity failed")
  sden <- r0 / object@priceOutside + sum(r[subset] / p[subset])
  s <- rep(0, n); s[subset] <- (r[subset] / p[subset]) / sden
  list(prices = p, revenue = r, quantityShare = s, quantities = q,
       r0 = r0, quantityOutside = (r0 / object@priceOutside) / sden,
       w = exp(logw))
}

.ces_multipliers <- function(r, owner, leaders, gamma, conduct, subset) {
  if (!any(subset)) stop("CES requires at least one active product")
  rr <- r[subset]
  own <- owner[subset]
  firms <- unique(own)
  R <- vapply(firms, function(f) sum(rr[own == f]), numeric(1)); names(R) <- firms
  h <- gamma - 1
  r0 <- 1 - sum(rr)
  if (!is.finite(r0) || r0 <= 0) stop("CES requires a positive outside revenue share")
  H <- 1 + h * r0
  mult <- rep(NA_real_, length(owner)); names(mult) <- names(owner)
  if (conduct == "bertrand") {
    af <- setNames(numeric(length(R)), firms)
    for (f in firms) {
      y <- R[f]
      af[f] <- h * y / (1 - y + h * (1 - y + y^2))
    }
    ff <- setdiff(firms, leaders)
    B <- if (length(ff)) sum(R[ff] * af[ff]) else 0
    if (!is.finite(B) || B >= 1) stop("invalid CES follower-sector response multiplier")
    phi <- 1 / (1 - B)
    for (f in ff) mult[subset & owner == f] <- 1 / (1 + h * (1 - R[f]))
    for (f in intersect(leaders, firms)) mult[subset & owner == f] <-
      1 / (gamma - h * phi * R[f])
    list(margins = mult, R = R, r0 = r0, H = H, a = af, B = B, phi = phi)
  } else {
    b <- h / (gamma * H)
    af <- setNames(numeric(length(R)), firms)
    for (f in firms) {
      y <- R[f]
      af[f] <- h * (H - 2 * y + gamma * h * r0 * y / H) /
        (H * (H + (gamma - 2) * y))
    }
    ff <- setdiff(firms, leaders)
    C <- if (length(ff)) sum(R[ff] * af[ff]) else 0
    if (!is.finite(1 + C) || 1 + C <= 0) stop("invalid CES follower-sector quantity response multiplier")
    for (f in ff) mult[subset & owner == f] <- 1 / gamma + b * R[f]
    for (f in intersect(leaders, firms)) mult[subset & owner == f] <-
      1 / gamma + b * R[f] / (1 + C)
    list(margins = mult, R = R, r0 = r0, H = H, b = b, a = af, C = C)
  }
}

.ces_calibration <- function(object, prices, margins, owner, leaders, weights,
                              fixed = NULL, control = list()) {
  n <- length(prices)
  usable <- is.finite(margins) & margins > 0 & is.finite(weights) & weights > 0 &
    is.finite(prices) & prices > 0
  if (is.null(fixed) && !any(usable)) stop("no usable positive observed margins; supply a positive fixed 'gamma'")
  objective <- function(gamma) {
    if (!is.finite(gamma) || gamma <= 1) return(Inf)
    z <- .ces_multipliers(object@shares, owner, leaders, gamma, object@conduct, rep(TRUE, n))$margins
    if (!any(usable)) return(0)
    sum(weights[usable] * (log(1 / margins[usable]) - log(1 / z[usable]))^2)
  }
  if (!is.null(fixed)) {
    g <- as.numeric(fixed)
    return(list(gamma = g, objective = objective(g), usable = usable,
                lower = g, upper = g, boundary = FALSE, expanded = FALSE,
                grid = g, gridObjective = objective(g)))
  }
  lower <- as.numeric(control$lower %||% control$lowerGamma %||% (1 + 1e-7))
  upper <- as.numeric(control$upper %||% control$upperGamma %||% 100)
  if (length(lower) != 1L || !is.finite(lower) || lower <= 1) lower <- 1 + 1e-7
  if (length(upper) != 1L || !is.finite(upper) || upper <= lower) upper <- max(100, lower * 2)
  explicitUpper <- !is.null(control$upper) || !is.null(control$upperGamma)
  maxUpper <- as.numeric(control$maxUpper %||% control$maxGamma %||% 1e6)
  if (!is.finite(maxUpper) || maxUpper <= upper) maxUpper <- upper
  gridN <- as.integer(control$grid %||% 121L)
  if (!is.finite(gridN) || gridN < 21) gridN <- 121L
  expanded <- FALSE
  repeat {
    gy <- seq(log(lower - 1), log(upper - 1), length.out = gridN)
    gg <- exp(gy) + 1
    go <- vapply(gg, objective, numeric(1))
    if (explicitUpper || which.min(go) != length(go) || upper >= maxUpper) break
    newUpper <- min(maxUpper, max(upper * 2, 1 + 2 * (upper - 1)))
    if (newUpper <= upper * (1 + 1e-12)) break
    upper <- newUpper; expanded <- TRUE
  }
  gy <- seq(log(lower - 1), log(upper - 1), length.out = gridN)
  gg <- exp(gy) + 1; go <- vapply(gg, objective, numeric(1))
  ## Refine every grid local minimum.  This keeps the bounded search
  ## transparent and does not rely on an unverified global-unimodality claim.
  idx <- which(go <= c(Inf, go[-length(go)]) & go <= c(go[-1], Inf))
  if (!length(idx)) idx <- which.min(go)
  candidates <- list()
  for (i in idx) {
    if (i == 1L || i == length(gg)) {
      candidates[[length(candidates) + 1L]] <- c(gg[i], go[i])
    } else {
      opt <- try(stats::optimize(function(y) objective(exp(y) + 1),
                                  c(gy[i - 1L], gy[i + 1L]),
                                  tol = as.numeric(control$reltol %||% 1e-11)), silent = TRUE)
      if (!inherits(opt, "try-error") && is.finite(opt$objective)) {
        candidates[[length(candidates) + 1L]] <- c(exp(opt$minimum) + 1, opt$objective)
      }
    }
  }
  cand <- do.call(rbind, candidates)
  win <- cand[which.min(cand[, 2]), ]
  boundary <- abs(win[1] - lower) <= 1e-8 * (1 + lower) ||
    abs(win[1] - upper) <= 1e-8 * (1 + upper)
  list(gamma = win[1], objective = win[2], usable = usable,
       lower = lower, upper = upper, boundary = boundary, expanded = expanded,
       grid = gg, gridObjective = go)
}

.ces_make_diagnostics <- function(object, gamma, implied, calibration, observed) {
  list(
    demandparam = gamma, gamma = gamma, observedMargins = observed,
    impliedMargins = implied, residuals = observed - implied,
    weights = object@weights, usable = calibration$usable,
    ownerPre = object@firmOwnerPre, ownerPost = object@firmOwnerPost,
    leadersPre = object@leadersPre, leadersPost = object@leadersPost,
    calibration = calibration,
    solverStatus = list(pre = "not-run", post = "not-run"),
    inadmissible = character()
  )
}

.stackelberg_ces_constructor <- function(prices, shares, margins, ownerPre,
                                         ownerPost, leadersPre, leadersPost,
                                         conduct, output, insideSize, normIndex,
                                         priceOutside, mcDelta, subset,
                                         priceStart, labels, weights, alpha,
                                         gamma, control.slopes, control.equ,
                                         ownerPostWasMissing = FALSE, dots = list(),
                                         revenueRetentionPre = rep(1, length(prices)),
                                         revenueRetentionPost = revenueRetentionPre) {
  if (length(dots)) {
    bad <- intersect(names(dots), c("diversions", "nests", "sigma", "mktElast"))
    if (length(bad)) stop("unsupported Stackelberg CES arguments: ", paste(bad, collapse = ", "))
  }
  .sk_stop_matrix(control.slopes, "control.slopes")
  .sk_stop_matrix(control.equ, "control.equ")
  if (!isTRUE(output)) stop("StackelbergCES currently supports output markets only")
  if (!is.null(alpha)) stop("'alpha' is only supported for demand='logit'; supply 'gamma' for CES")
  n <- length(prices)
  prices <- as.numeric(prices); shares <- as.numeric(shares); margins <- as.numeric(margins)
  if (length(prices) != n || length(shares) != n || length(margins) != n) {
    stop("prices, shares, and margins must have the same length")
  }
  if (any(!is.finite(prices)) || any(prices <= 0)) stop("prices must be finite and positive")
  if (any(!is.finite(shares)) || any(shares <= 0) || sum(shares) >= 1) {
    stop("CES shares must be positive unconditional revenue shares summing to less than one")
  }
  if (length(normIndex) != 1L || !is.na(normIndex)) stop("Stackelberg CES requires normIndex=NA and an explicit outside good")
  if (length(priceOutside) != 1L || !is.finite(priceOutside) || priceOutside <= 0) stop("Stackelberg CES requires a positive priceOutside")
  if (!is.finite(insideSize) || length(insideSize) != 1L || insideSize <= 0) stop("insideSize must be positive")
  ownerPre <- .sk_ids(ownerPre, "ownerPre", n); ownerPost <- .sk_ids(ownerPost, "ownerPost", n)
  leadersPre <- .sk_ids(leadersPre, "leadersPre")
  if (any(!leadersPre %in% unique(ownerPre))) stop("leadersPre contains an unknown firm ID")
  if (is.null(leadersPost)) {
    if (ownerPostWasMissing || identical(ownerPost, ownerPre)) leadersPost <- leadersPre
    else stop("leadersPost must be supplied when ownerPost changes the ownership partition")
  } else leadersPost <- .sk_ids(leadersPost, "leadersPost")
  if (any(!leadersPost %in% unique(ownerPost))) stop("leadersPost contains an unknown firm ID")
  if (!is.logical(subset) || length(subset) != n || anyNA(subset) || !any(subset)) stop("subset must be a logical vector with at least one TRUE and no NA values")
  mcDelta <- as.numeric(mcDelta)
  if (length(mcDelta) != n || any(!is.finite(mcDelta))) stop("mcDelta must be a finite vector matching prices")
  priceStart <- as.numeric(priceStart)
  if (length(priceStart) != n || any(!is.finite(priceStart)) || any(priceStart <= 0)) stop("priceStart must be finite and positive")
  labels <- as.character(labels); if (length(labels) != n) stop("labels must have one value per product")
  weights <- as.numeric(weights)
  if (length(weights) != n || any(!is.finite(weights)) || any(weights < 0)) stop("weights must be finite and non-negative")
  if (!is.null(gamma) && (length(gamma) != 1L || !is.finite(gamma) || gamma <= 1)) stop("'gamma' must be a finite elasticity greater than one")
  h0 <- .ces_multipliers(shares, ownerPre, leadersPre,
                         if (is.null(gamma)) 2 else as.numeric(gamma), conduct,
                         rep(TRUE, n))$margins
  impliedPlaceholder <- h0
  allMissing <- all(is.na(margins))
  if (allMissing && is.null(gamma)) stop("all margins are missing; supply a positive fixed 'gamma'")
  parentMargins <- margins
  if (allMissing) parentMargins <- impliedPlaceholder
  result <- suppressWarnings(new("StackelbergCES",
    prices = prices, shares = shares, margins = parentMargins,
    diversion = matrix(NA_real_, n, n), normIndex = NA_real_,
    ownerPre = .sk_owner_matrix(ownerPre), ownerPost = .sk_owner_matrix(ownerPost),
    insideSize = insideSize, output = TRUE, mcDelta = mcDelta,
    subset = subset, weights = weights, priceOutside = priceOutside,
    priceStart = priceStart, shareInside = sum(shares),
    mktSize = insideSize / sum(shares),
    labels = labels, conduct = conduct, leadersPre = leadersPre,
    leadersPost = leadersPost, firmOwnerPre = ownerPre,
    firmOwnerPost = ownerPost, diagnostics = list(observedMargins = margins),
    gammaFixed = if (is.null(gamma)) NA_real_ else as.numeric(gamma)
  ))
  if (length(control.slopes)) result@control.slopes <- control.slopes
  if (length(control.equ)) result@control.equ <- control.equ
  result@diagnostics$parentMargins <- parentMargins
  result@diagnostics$marginProvenance <- if (allMissing) "fixed_gamma_placeholder" else "observed"
  result <- antitrust::setRetention(result, revenueRetentionPre, revenueRetentionPost)
  result <- calcSlopes(result)
  pred <- result@diagnostics$impliedMargins
  result@mcPre <- prices * (1 - pred)
  if (any(!is.finite(result@mcPre)) || any(result@mcPre <= 0)) stop("baseline CES marginal costs must be finite and positive")
  result@mcPost <- result@mcPre * (1 + mcDelta)
  if (any(!is.finite(result@mcPost[subset]) | result@mcPost[subset] <= 0)) {
    stop("CES post marginal costs must be finite and positive on active products")
  }
  result@pricePre <- calcPrices(result, TRUE, subset = rep(TRUE, n))
  result@pricePost <- calcPrices(result, FALSE, subset = subset)
  tmp <- result; tmp@priceStart <- pmax(prices * 1.17 + 0.031, .Machine$double.eps^0.25)
  pchk <- try(calcPrices(tmp, TRUE, subset = rep(TRUE, n)), silent = TRUE)
  baselineErr <- if (inherits(pchk, "try-error")) Inf else max(abs(pchk - prices))
  baselineRel <- if (inherits(pchk, "try-error")) Inf else max(abs(pchk / prices - 1))
  if (!is.finite(baselineRel) || baselineRel > 2e-7) {
    stop("Stackelberg CES baseline reproduction exceeds tolerance")
  }
  result@diagnostics$baselineReproduction <- baselineErr
  result@diagnostics$baselineReproductionRelative <- baselineRel
  result@diagnostics$baselineReproductionPrices <- if (inherits(pchk, "try-error")) rep(NA_real_, n) else pchk
  result@diagnostics$solverStatus <- list(pre = "converged", post = "converged")
  result@diagnostics$residualsPre <- stackelberg_residuals(result, TRUE)
  result@diagnostics$residualsPost <- stackelberg_residuals(result, FALSE)
  implicitCheck <- isTRUE(control.equ$implicitCheck %||% TRUE)
  impPre <- if (implicitCheck) try(calcPrices(result, TRUE, method = "implicit"), silent = TRUE) else rep(NA_real_, n)
  impPost <- if (implicitCheck) try(calcPrices(result, FALSE, method = "implicit"), silent = TRUE) else rep(NA_real_, n)
  result@diagnostics$implicit <- list(
    pre = if (inherits(impPre, "try-error")) rep(NA_real_, n) else impPre,
    post = if (inherits(impPost, "try-error")) rep(NA_real_, n) else impPost,
    statusPre = if (!implicitCheck) "not-run" else if (inherits(impPre, "try-error")) "failed" else "converged",
    statusPost = if (!implicitCheck) "not-run" else if (inherits(impPost, "try-error")) "failed" else "converged",
    maxDifferencePre = if (!implicitCheck) NA_real_ else if (inherits(impPre, "try-error")) Inf else max(abs(impPre - result@pricePre), na.rm = TRUE),
    maxDifferencePost = if (!implicitCheck) NA_real_ else if (inherits(impPost, "try-error")) Inf else max(abs(impPost - result@pricePost), na.rm = TRUE)
  )
  result
}

#' @rdname StackelbergLogit
setMethod("calcSlopes", "StackelbergCES", function(object, ...) {
  if (.coord_mixed_retention(object, TRUE)) return(.coord_retained_slopes(object))
  n <- length(object@shares); prices <- object@prices
  if (length(object@gammaFixed) == 1L && is.finite(object@gammaFixed) && object@gammaFixed > 1) {
    fit <- .ces_calibration(object, prices, .sk_observed(object), object@firmOwnerPre,
                            object@leadersPre, object@weights, fixed = object@gammaFixed,
                            control = object@control.slopes)
  } else {
    fit <- .ces_calibration(object, prices, .sk_observed(object), object@firmOwnerPre,
                            object@leadersPre, object@weights, control = object@control.slopes)
  }
  g <- fit$gamma; h <- g - 1; r0 <- 1 - sum(object@shares)
  A <- (object@shares / r0) * (prices / object@priceOutside)^h
  names(A) <- object@labels
  object@slopes <- list(alpha = 1 / sum(object@shares) - 1, gamma = g, meanval = A)
  object@priceOutside <- as.numeric(object@priceOutside)
  object@mktSize <- object@insideSize / (1 - r0)
  implied <- .ces_multipliers(object@shares, object@firmOwnerPre, object@leadersPre,
                              g, object@conduct, rep(TRUE, n))$margins
  d <- .ces_make_diagnostics(object, g, implied, fit, .sk_observed(object))
  d$fixedGamma <- is.finite(object@gammaFixed) && object@gammaFixed > 1
  object@diagnostics <- modifyList(object@diagnostics, d)
  object
})

#' @rdname StackelbergLogit
setMethod("calcMC", "StackelbergCES", function(object, preMerger = TRUE) {
  mc <- if (preMerger) object@mcPre else object@mcPost
  if (!length(mc)) stop("Stackelberg CES marginal costs are not initialized")
  names(mc) <- object@labels; mc
})

#' @rdname StackelbergLogit
setMethod("calcMargins", "StackelbergCES", function(object, preMerger = TRUE, level = FALSE) {
  if (.coord_mixed_retention(object, preMerger)) return(.coord_retained_margins(object, preMerger, level))
  st <- .ces_active_state(object, preMerger)
  d <- .ces_demand_from_prices(object, st$prices, st$subset)
  m <- .ces_multipliers(d$revenue, st$owner, st$leaders, st$gamma,
                        object@conduct, st$subset)
  out <- m$margins
  out[!st$subset] <- NA_real_
  if (level) out <- out * st$prices
  names(out) <- object@labels; out
})

.ces_price_root <- function(object, preMerger, subset, start = NULL, ...) {
  st <- .ces_active_state(object, preMerger, subset)
  n <- length(object@shares); p0 <- st$prices
  if (is.null(start)) {
    start <- if (preMerger) object@priceStart else object@prices
    if (length(start) != n || any(!is.finite(start[subset])) || any(start[subset] <= 0)) start <- object@prices
  }
  start <- as.numeric(start)[subset]; costs <- st$costs[subset]
  if (length(start) != sum(subset) || any(!is.finite(start)) || any(start <= 0)) start <- object@prices[subset]
  target <- function(p) {
    pp <- rep(NA_real_, n); pp[subset] <- p
    d <- .ces_demand_from_prices(object, pp, subset)
    mm <- .ces_multipliers(d$revenue, st$owner, st$leaders, st$gamma, object@conduct, subset)$margins[subset]
    costs / pmax(1 - mm, 1e-12)
  }
  foc <- function(z) {
    p <- exp(z); tg <- try(target(p), silent = TRUE)
    if (inherits(tg, "try-error") || any(!is.finite(tg)) || any(tg <= 0)) return(rep(1e6, length(p)))
    log(p / tg)
  }
  ctl <- object@control.equ; maxit <- as.integer(ctl$maxit %||% 500L)
  if (!is.finite(maxit) || maxit < 20) maxit <- 500L
  tol <- as.numeric(ctl$tol %||% 1e-11); if (!is.finite(tol) || tol <= 0) tol <- 1e-11
  sol <- try(nleqslv::nleqslv(log(start), foc, method = "Broyden",
                              control = list(ftol = tol, xtol = tol, maxit = maxit)), silent = TRUE)
  z <- if (!inherits(sol, "try-error") && is.finite(sol$termcd) && sol$termcd <= 2) sol$x else NULL
  if (is.null(z)) {
    bb <- try(BB::BBsolve(log(start), foc, quiet = TRUE, control = ctl), silent = TRUE)
    if (!inherits(bb, "try-error") && is.finite(bb$convergence) && bb$convergence == 0) z <- bb$par
  }
  if (is.null(z) || any(!is.finite(z))) stop("Stackelberg CES price equilibrium solver failed")
  for (it in seq_len(10L)) {
    rr <- foc(z); if (all(is.finite(rr)) && max(abs(rr)) <= 1e-12) break
    JJ <- try(numDeriv::jacobian(foc, z), silent = TRUE)
    dz <- if (!inherits(JJ, "try-error")) try(solve(JJ, -rr), silent = TRUE) else structure("bad", class = "try-error")
    if (inherits(dz, "try-error") || any(!is.finite(dz))) break
    old <- sum(rr^2); step <- 1
    repeat {
      zn <- z + step * dz; rn <- foc(zn)
      if (all(is.finite(rn)) && sum(rn^2) <= old) { z <- zn; break }
      step <- step / 2; if (step < 1 / 128) break
    }
  }
  ans <- exp(z); rr <- foc(z)
  if (any(!is.finite(ans)) || max(abs(rr), na.rm = TRUE) > 2e-9) stop("Stackelberg CES price equilibrium residual exceeds tolerance")
  ans
}

#' @rdname StackelbergLogit
setMethod("calcPrices", "StackelbergCES", function(object, preMerger = TRUE,
                                                       isMax = FALSE, subset,
                                                       method = c("analytic", "implicit"), ...) {
  subset <- .sk_active(object, preMerger, subset); method <- match.arg(method)
  if (.coord_mixed_retention(object, preMerger, subset)) {
    out <- rep(NA_real_, length(subset)); out[subset] <- .coord_retained_root(object, preMerger, subset)
    names(out) <- object@labels; return(out)
  }
  p <- if (method == "implicit") .ces_implicit_price_root(object, preMerger, subset) else
    .ces_price_root(object, preMerger, subset, ...)
  out <- rep(NA_real_, length(object@shares)); out[subset] <- p
  if (preMerger) out[!subset] <- object@prices[!subset]
  names(out) <- object@labels; out
})

# Solve the leader equations after fully solving the follower Nash game at
# each proposed leader action.  This route deliberately uses the raw-profit
# Jacobian response, so it is an independent diagnostic of the closed-form
# joint price equations used by the analytic route.
.ces_implicit_price_root <- function(object, preMerger, subset) {
  st <- .ces_active_state(object, preMerger, subset); n <- length(object@shares)
  active <- which(subset); li <- active[st$owner[active] %in% st$leaders]
  fi <- active[!(st$owner[active] %in% st$leaders)]
  if (!length(li) || !length(fi)) return(.ces_price_root(object, preMerger, subset))
  if (object@conduct == "bertrand") {
    start <- if (preMerger) object@priceStart else object@pricePost
    if (length(start) != n || any(!is.finite(start[li])) || any(start[li] <= 0)) start <- object@prices
    start <- start[li]
  } else {
    dd <- .ces_demand_from_prices(object, if (preMerger) object@prices else st$prices, subset)
    start <- dd$quantities[li]
  }
  candidate <- function(a) {
    ff <- try(.ces_stackelberg_followers(object, a, preMerger = preMerger,
                                         subset = subset), silent = TRUE)
    if (inherits(ff, "try-error")) return(list(eq = rep(1e6, length(li)), p = rep(NA_real_, n), q = rep(NA_real_, n)))
    tmp <- object
    if (preMerger) tmp@pricePre <- ff$prices else tmp@pricePost <- ff$prices
    R <- try(.ces_implicit_response_at(tmp, preMerger, subset), silent = TRUE)
    if (inherits(R, "try-error")) return(list(eq = rep(1e6, length(li)), p = ff$prices, q = ff$quantities))
    g <- try(.ces_reduced_leader_foc(tmp, preMerger, subset, R = R), silent = TRUE)
    if (inherits(g, "try-error") || any(!is.finite(g))) return(list(eq = rep(1e6, length(li)), p = ff$prices, q = ff$quantities))
    scale <- if (object@conduct == "bertrand") ff$quantities[li] else ff$prices[li]
    list(eq = as.numeric(g) / pmax(scale, .Machine$double.xmin), p = ff$prices, q = ff$quantities)
  }
  foc <- function(z) candidate(exp(z))$eq
  ctl <- object@control.equ; maxit <- as.integer(ctl$maxit %||% 500L)
  if (!is.finite(maxit) || maxit < 20) maxit <- 500L
  sol <- try(nleqslv::nleqslv(log(start), foc, method = "Broyden",
                              control = list(ftol = 1e-9, xtol = 1e-9, maxit = maxit)), silent = TRUE)
  bad <- inherits(sol, "try-error") || any(!is.finite(sol$x)) || sol$termcd > 2
  if (!bad) {
    probe <- candidate(exp(sol$x)); bad <- any(!is.finite(probe$eq)) || max(abs(probe$eq)) > 2e-7
  }
  if (bad) {
    lo <- rep(log(.Machine$double.eps^0.25), length(start))
    hi <- rep(log(max(c(start, object@prices, 1), na.rm = TRUE) * 1e4), length(start))
    opt <- try(stats::optim(log(start), function(z) sum(foc(z)^2), method = "L-BFGS-B",
                            lower = lo, upper = hi, control = list(maxit = 1000L)), silent = TRUE)
    if (inherits(opt, "try-error") || any(!is.finite(opt$par))) stop("implicit Stackelberg CES leader solver failed")
    sol <- list(x = opt$par)
  }
  zg <- sol$x
  ## A few Newton-polish steps remove nested follower solver tolerance from
  ## the returned implicit root and keep analytic/implicit gates tight.
  for (it in seq_len(6L)) {
    rr <- foc(zg); if (all(is.finite(rr)) && max(abs(rr)) <= 1e-10) break
    JJ <- try(numDeriv::jacobian(foc, zg), silent = TRUE)
    dz <- if (!inherits(JJ, "try-error")) try(solve(JJ, -rr), silent = TRUE) else structure("bad", class = "try-error")
    if (inherits(dz, "try-error") || any(!is.finite(dz))) break
    old <- sum(rr^2); step <- 1
    repeat { zn <- zg + step * dz; rn <- foc(zn); if (all(is.finite(rn)) && sum(rn^2) <= old) { zg <- zn; break }; step <- step / 2; if (step < 1 / 128) break }
  }
  got <- candidate(exp(zg))
  if (any(!is.finite(got$eq)) || max(abs(got$eq)) > 2e-7) stop("implicit Stackelberg CES leader residual exceeds tolerance")
  got$p[subset]
}

.ces_raw_foc <- function(object, action, preMerger = TRUE, subset = NULL) {
  st <- .ces_active_state(object, preMerger, subset); n <- length(object@shares)
  subset <- st$subset; p <- rep(NA_real_, n)
  retention <- .coord_retention(object, preMerger)
  if (object@conduct == "bertrand") {
    p[subset] <- as.numeric(action); d <- .ces_demand_from_prices(object, p, subset)
    g <- numeric(sum(subset)); active <- which(subset)
    for (jj in seq_along(active)) {
      j <- active[jj]; own <- which(subset & st$owner == st$owner[j]);
      mu <- d$prices[own] - st$costs[own]
      dq <- d$quantities[own] / d$prices[j] * (-st$gamma * (own == j) + (st$gamma - 1) * d$revenue[j])
      g[jj] <- retention[j] * d$quantities[j] + sum(retention[own] * mu * dq)
    }
    return(g)
  }
  q <- rep(NA_real_, n); q[subset] <- as.numeric(action)
  d <- .ces_demand_from_quantities(object, q, subset)
  h <- st$gamma - 1; b <- h / (st$gamma * (1 + h * d$r0))
  g <- numeric(sum(subset)); active <- which(subset)
  for (jj in seq_along(active)) {
    j <- active[jj]; own <- which(subset & st$owner == st$owner[j]); Rf <- sum(retention[own] * d$revenue[own])
    g[jj] <- retention[j] * (d$prices[j] - st$costs[j] - d$prices[j] / st$gamma) - b * d$prices[j] * Rf
  }
  g
}

.ces_implicit_response_at <- function(object, preMerger = TRUE, subset = NULL) {
  st <- .ces_active_state(object, preMerger, subset); subset <- st$subset
  if (.coord_mixed_retention(object, preMerger, subset)) return(.coord_retained_response(object, preMerger, subset))
  active <- which(subset); li <- active[st$owner[active] %in% st$leaders]
  fi <- active[!(st$owner[active] %in% st$leaders)]
  out <- matrix(numeric(), nrow = length(fi), ncol = length(li),
                dimnames = list(object@labels[fi], object@labels[li]))
  if (!length(fi) || !length(li)) return(out)
  d <- if (object@conduct == "bertrand") .ces_demand_from_prices(object, st$prices, subset) else {
    q <- rep(NA_real_, length(object@shares)); q[subset] <-
      .ces_demand_from_prices(object, st$prices, subset)$quantities[subset]
    .ces_demand_from_quantities(object, q, subset)
  }
  action <- if (object@conduct == "bertrand") st$prices[subset] else d$quantities[subset]
  J <- numDeriv::jacobian(function(a) .ces_raw_foc(object, a, preMerger, subset), action)
  posF <- match(fi, active); posL <- match(li, active)
  ans <- try(-solve(J[posF, posF, drop = FALSE], J[posF, posL, drop = FALSE]), silent = TRUE)
  if (inherits(ans, "try-error") || any(!is.finite(ans))) stop("CES follower FOC Jacobian is singular")
  dimnames(ans) <- list(object@labels[fi], object@labels[li]); ans
}

.ces_direct_gradients <- function(object, preMerger = TRUE, subset = NULL) {
  st <- .ces_active_state(object, preMerger, subset); subset <- st$subset; active <- which(subset)
  d <- if (object@conduct == "bertrand") .ces_demand_from_prices(object, st$prices, subset) else {
    q <- rep(NA_real_, length(object@shares)); dd <- .ces_demand_from_prices(object, st$prices, subset)
    q[subset] <- dd$quantities[subset]; .ces_demand_from_quantities(object, q, subset)
  }
  firms <- intersect(st$leaders, unique(st$owner[active])); ans <- list()
  retention <- .coord_retention(object, preMerger)
  if (object@conduct == "bertrand") {
    for (f in firms) {
      own <- which(subset & st$owner == f); v <- numeric(length(active))
      for (jj in seq_along(active)) {
        j <- active[jj]
        dq <- d$quantities[own] / d$prices[j] * (-st$gamma * (own == j) + (st$gamma - 1) * d$revenue[j])
        v[jj] <- sum(retention[own] * ((own == j) * d$quantities[own] + (d$prices[own] - st$costs[own]) * dq))
      }
      ans[[f]] <- v
    }
  } else {
    h <- st$gamma - 1; b <- h / (st$gamma * (1 + h * d$r0));
    for (f in firms) {
      own <- which(subset & st$owner == f); v <- numeric(length(active))
      for (jj in seq_along(active)) {
        j <- active[jj]
        K <- -d$prices[own] / (st$gamma * d$quantities[own]) * (own == j) -
          b * d$prices[own] * d$prices[j] / st$E
        v[jj] <- sum(retention[own] * ((own == j) * (d$prices[own] - st$costs[own]) + d$quantities[own] * K))
      }
      ans[[f]] <- v
    }
  }
  list(values = ans, active = active, leaders = firms)
}

.ces_reduced_leader_foc <- function(object, preMerger = TRUE, subset = NULL, R = NULL) {
  st <- .ces_active_state(object, preMerger, subset); subset <- st$subset; active <- which(subset)
  li <- active[st$owner[active] %in% st$leaders]; fi <- active[!(st$owner[active] %in% st$leaders)]
  if (!length(li)) return(numeric(0))
  if (is.null(R)) R <- .ces_stackelberg_response(object, preMerger, "analytic")
  dg <- .ces_direct_gradients(object, preMerger, subset)$values
  out <- numeric(length(li)); names(out) <- object@labels[li]
  for (kk in seq_along(li)) {
    k <- li[kk]; f <- st$owner[k]; direct <- dg[[f]]
    out[kk] <- direct[match(k, active)]
    if (length(fi)) out[kk] <- out[kk] + sum(direct[match(fi, active)] * R[, kk])
  }
  out
}

.ces_stackelberg_followers <- function(object, leaderActions, preMerger = TRUE, start = NULL,
                                       subset = NULL) {
  st <- .ces_active_state(object, preMerger, subset); subset <- st$subset; n <- length(object@shares)
  active <- which(subset); li <- active[st$owner[active] %in% st$leaders]; fi <- active[!(st$owner[active] %in% st$leaders)]
  la <- as.numeric(leaderActions); if (length(la) == n) la <- la[li]
  if (length(la) != length(li) || any(!is.finite(la)) || any(la <= 0)) stop("leaderActions must be positive actions for every active leader product")
  if (object@conduct == "bertrand") {
    p <- rep(NA_real_, n); p[li] <- la
    if (!length(fi)) {
      d <- .ces_demand_from_prices(object, p, subset)
      return(list(choices = p, prices = p, quantities = d$quantities,
                  shares = d$quantityShare, residuals = numeric(), leaderProducts = li,
                  followerProducts = fi, converged = TRUE))
    }
    if (is.null(start)) {
      base <- if (preMerger) object@pricePre else object@pricePost
      start <- base[fi]; if (any(!is.finite(start)) || any(start <= 0)) start <- object@prices[fi]
    }
  } else {
    q <- rep(NA_real_, n); q[li] <- la
    if (!length(fi)) {
      d <- .ces_demand_from_quantities(object, q, subset)
      return(list(choices = q, prices = d$prices, quantities = q,
                  shares = d$quantityShare, residuals = numeric(), leaderProducts = li,
                  followerProducts = fi, converged = TRUE))
    }
    if (is.null(start)) {
      base <- .ces_demand_from_prices(object, st$prices, subset)$quantities
      start <- base[fi]
    }
  }
  start <- pmax(as.numeric(start), .Machine$double.eps^0.25)
  if (length(start) != length(fi)) stop("start must match follower products")
  fixed <- function(z) {
    a <- exp(z)
    if (object@conduct == "bertrand") {
      pp <- p; pp[fi] <- a
      raw <- .ces_raw_foc(object, pp[subset], preMerger, subset)[match(fi, active)]
      dd <- .ces_demand_from_prices(object, pp, subset)
      raw / pmax(dd$quantities[fi], .Machine$double.xmin)
    } else {
      qq <- q; qq[fi] <- a
      raw <- .ces_raw_foc(object, qq[subset], preMerger, subset)[match(fi, active)]
      dd <- .ces_demand_from_quantities(object, qq, subset)
      raw / pmax(dd$prices[fi], .Machine$double.xmin)
    }
  }
  sol <- try(nleqslv::nleqslv(log(start), fixed, method = "Broyden",
                              control = list(ftol = 1e-14, xtol = 1e-14, maxit = 500L)), silent = TRUE)
  if (inherits(sol, "try-error") || any(!is.finite(sol$x)) ||
      (sol$termcd > 2 && (is.null(sol$fvec) || max(abs(sol$fvec)) > 1e-8))) stop("CES follower equilibrium solver failed")
  zz <- sol$x
  for (it in seq_len(8L)) {
    rr <- fixed(zz); if (max(abs(rr), na.rm = TRUE) <= 1e-14) break
    JJ <- try(numDeriv::jacobian(fixed, zz), silent = TRUE)
    dz <- if (!inherits(JJ, "try-error")) try(solve(JJ, -rr), silent = TRUE) else structure("bad", class = "try-error")
    if (inherits(dz, "try-error") || any(!is.finite(dz))) break
    old <- sum(rr^2); step <- 1
    repeat { zn <- zz + step * dz; rn <- fixed(zn); if (all(is.finite(rn)) && sum(rn^2) <= old) { zz <- zn; break }; step <- step / 2; if (step < 1 / 128) break }
  }
  finalScaled <- fixed(zz)
  if (any(!is.finite(finalScaled)) || max(abs(finalScaled), na.rm = TRUE) > 2e-10) {
    stop("CES follower equilibrium normalized residual exceeds tolerance")
  }
  a <- exp(zz)
  if (object@conduct == "bertrand") {
    p[fi] <- a; d <- .ces_demand_from_prices(object, p, subset)
    r <- .ces_raw_foc(object, p[subset], preMerger, subset)[match(fi, active)]
    list(choices = p, prices = p, quantities = d$quantities, shares = d$quantityShare,
         residuals = r, leaderProducts = li, followerProducts = fi, converged = TRUE)
  } else {
    q[fi] <- a; d <- .ces_demand_from_quantities(object, q, subset)
    r <- .ces_raw_foc(object, q[subset], preMerger, subset)[match(fi, active)]
    list(choices = q, prices = d$prices, quantities = q, shares = d$quantityShare,
         residuals = r, leaderProducts = li, followerProducts = fi, converged = TRUE)
  }
}

.ces_stackelberg_response <- function(object, preMerger = TRUE, method = "analytic") {
  method <- match.arg(method, c("analytic", "implicit"))
  if (.coord_mixed_retention(object, preMerger)) method <- "implicit"
  st <- .ces_active_state(object, preMerger); subset <- st$subset; active <- which(subset)
  li <- active[st$owner[active] %in% st$leaders]; fi <- active[!(st$owner[active] %in% st$leaders)]
  out <- matrix(numeric(), nrow = length(fi), ncol = length(li),
                dimnames = list(object@labels[fi], object@labels[li]))
  if (!length(fi) || !length(li)) return(out)
  if (method == "implicit") return(.ces_implicit_response_at(object, preMerger, subset))
  d <- .ces_demand_from_prices(object, st$prices, subset); g <- st$gamma; h <- g - 1
  fs <- tapply(d$revenue[subset], st$owner[subset], sum); ff <- setdiff(names(fs), st$leaders)
  if (object@conduct == "bertrand") {
    af <- h * fs / (1 - fs + h * (1 - fs + fs^2)); B <- if (length(ff)) sum(fs[ff] * af[ff]) else 0; phi <- 1 / (1 - B)
    for (rr in seq_along(fi)) { f <- st$owner[fi[rr]]; out[rr, ] <- st$prices[fi[rr]] * af[f] * phi * d$revenue[li] / st$prices[li] }
  } else {
    H <- 1 + h * d$r0; b <- h / (g * H); a <- h * (H - 2 * fs + g * h * d$r0 * fs / H) / (H * (H + (g - 2) * fs)); C <- if (length(ff)) sum(fs[ff] * a[ff]) else 0
    q <- d$quantities
    for (rr in seq_along(fi)) { f <- st$owner[fi[rr]]; out[rr, ] <- -q[fi[rr]] * a[f] * st$prices[li] / (st$E * (1 + C)) }
  }
  out
}

.ces_stackelberg_residuals <- function(object, preMerger = TRUE, prices = NULL, quantities = NULL) {
  st <- .ces_active_state(object, preMerger); subset <- st$subset; n <- length(object@shares); active <- which(subset)
  tmp <- object
  if (object@conduct == "bertrand") {
    if (!is.null(quantities)) stop("quantities are unsupported for Bertrand residuals")
    if (is.null(prices)) prices <- st$prices[subset] else { prices <- as.numeric(prices); if (length(prices) == n) prices <- prices[subset]; if (length(prices) != length(active) || any(!is.finite(prices)) || any(prices <= 0)) stop("prices must be positive and have full or active-product length") }
    full <- rep(NA_real_, n); full[subset] <- prices; if (preMerger) tmp@pricePre <- full else tmp@pricePost <- full
    g <- .ces_raw_foc(tmp, prices, preMerger, subset)
  } else {
    if (!is.null(prices)) stop("prices are unsupported for Cournot residuals; supply quantities")
    if (is.null(quantities)) { dd <- .ces_demand_from_prices(object, st$prices, subset); quantities <- dd$quantities[subset] } else { quantities <- as.numeric(quantities); if (length(quantities) == n) quantities <- quantities[subset]; if (length(quantities) != length(active) || any(!is.finite(quantities)) || any(quantities <= 0)) stop("quantities must be positive and finite") }
    qfull <- rep(NA_real_, n); qfull[subset] <- quantities
    dd <- .ces_demand_from_quantities(tmp, qfull, subset)
    if (preMerger) tmp@pricePre <- dd$prices else tmp@pricePost <- dd$prices
    g <- .ces_raw_foc(tmp, quantities, preMerger, subset)
  }
  liFlag <- st$owner[subset] %in% st$leaders; leader <- .ces_reduced_leader_foc(tmp, preMerger, subset)
  follower <- g[!liFlag]; all <- rep(NA_real_, length(g)); all[liFlag] <- leader; all[!liFlag] <- follower
  if (object@conduct == "bertrand") {
    pp <- if (preMerger) tmp@pricePre else tmp@pricePost
    scaleFull <- .ces_demand_from_prices(tmp, pp, subset)$quantities
  } else {
    scaleFull <- .ces_demand_from_quantities(tmp, qfull, subset)$prices
  }
  scale <- pmax(abs(scaleFull[subset]), .Machine$double.xmin)
  normalizedLeader <- leader / scale[liFlag]
  normalizedFollower <- follower / scale[!liFlag]
  normalizedAll <- rep(NA_real_, length(g)); normalizedAll[liFlag] <- normalizedLeader; normalizedAll[!liFlag] <- normalizedFollower
  nv <- c(abs(normalizedLeader), abs(normalizedFollower)); nv <- nv[is.finite(nv)]
  av <- c(abs(leader), abs(follower)); av <- av[is.finite(av)]
  list(leader = leader, follower = follower, all = all,
       maxLeader = if (length(leader)) max(abs(leader), na.rm = TRUE) else 0,
       maxFollower = if (length(follower)) max(abs(follower), na.rm = TRUE) else 0,
       max = if (length(av)) max(av) else 0,
       normalizedLeader = normalizedLeader, normalizedFollower = normalizedFollower,
       normalizedAll = normalizedAll,
       maxNormalizedLeader = if (length(normalizedLeader)) max(abs(normalizedLeader), na.rm = TRUE) else 0,
       maxNormalizedFollower = if (length(normalizedFollower)) max(abs(normalizedFollower), na.rm = TRUE) else 0,
       maxNormalized = if (length(nv)) max(nv) else 0,
       leaderProducts = active[liFlag], followerProducts = active[!liFlag], status = "evaluated")
}

.ces_stackelberg_simulate <- function(object, ownerPost, leadersPost, mcDelta, subset, dots = list()) {
  if (length(dots)) stop("unsupported Stackelberg CES simulation arguments: ", paste(names(dots), collapse = ", "))
  n <- length(object@shares); same <- identical(as.character(ownerPost), object@firmOwnerPost)
  ownerPost <- .sk_ids(ownerPost, "ownerPost", n)
  if (is.null(leadersPost)) { if (!same) stop("leadersPost must be supplied when ownerPost changes the ownership partition"); leadersPost <- object@leadersPost } else leadersPost <- .sk_ids(leadersPost, "leadersPost")
  if (any(!leadersPost %in% unique(ownerPost))) stop("leadersPost contains an unknown firm ID")
  if (!is.logical(subset) || length(subset) != n || anyNA(subset) || !any(subset)) stop("subset must be a logical vector with at least one TRUE and no NA values")
  mcDelta <- as.numeric(mcDelta); if (length(mcDelta) != n || any(!is.finite(mcDelta))) stop("mcDelta must be a finite vector matching prices")
  out <- object; out@firmOwnerPost <- ownerPost; out@leadersPost <- leadersPost; out@ownerPost <- .sk_owner_matrix(ownerPost); out@mcDelta <- mcDelta; out@subset <- subset
  out@mcPost <- object@mcPre * (1 + mcDelta)
  if (any(!is.finite(out@mcPost[subset]) | out@mcPost[subset] <= 0)) {
    stop("CES post marginal costs must be finite and positive on active products")
  }
  out@pricePost <- calcPrices(out, FALSE, subset = subset)
  out@diagnostics$ownerPost <- ownerPost; out@diagnostics$leadersPost <- leadersPost; out@diagnostics$mcDelta <- mcDelta; out@diagnostics$subset <- subset
  out@diagnostics$counterfactual <- stackelberg_residuals(out, FALSE); out
}
