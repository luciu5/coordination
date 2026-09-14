# Noncooperative, firm-level Stackelberg games with flat Logit demand.
#
# This file deliberately defines a new class and new methods.  In particular,
# it does not alter antitrust's ordinary Logit, Cournot, or legacy Stackelberg
# classes.

setClass(
  Class = "StackelbergLogit",
  contains = "Logit",
  representation = representation(
    conduct = "character",
    leadersPre = "character",
    leadersPost = "character",
    firmOwnerPre = "character",
    firmOwnerPost = "character",
    diagnostics = "list",
    alphaFixed = "numeric"
  ),
  prototype = prototype(
    conduct = "bertrand",
    leadersPre = character(),
    leadersPost = character(),
    firmOwnerPre = character(),
    firmOwnerPost = character(),
    diagnostics = list(),
    alphaFixed = NA_real_
  ),
  validity = function(object) {
    n <- length(object@shares)
    if (length(object@conduct) != 1L || !object@conduct %in% c("bertrand", "cournot")) {
      return("'conduct' must be 'bertrand' or 'cournot'")
    }
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
    ## During construction an uncensored object carries NA here until
    ## calcSlopes calibrates alpha.  Publicly returned objects always have a
    ## finite positive demand parameter.
    if (length(object@alphaFixed) != 1L ||
        (!is.na(object@alphaFixed) &&
         (!is.finite(object@alphaFixed) || object@alphaFixed <= 0))) {
      return("'alphaFixed' must be a positive scalar or NA before calibration")
    }
    TRUE
  }
)

.sk_stop_matrix <- function(x, name) {
  if (is.matrix(x) || is.array(x)) {
    stop("'", name, "' matrix controls are unsupported; supply a named list of scalar controls")
  }
}

.sk_ids <- function(x, name, n = NULL) {
  if (is.null(x)) return(character())
  if (is.matrix(x) || is.array(x)) {
    stop("'", name, "' must be a firm-ID vector; partial-control matrices are unsupported")
  }
  if (!is.atomic(x) || is.list(x) || (length(x) && anyNA(x))) {
    stop("'", name, "' must be a non-missing firm-ID vector")
  }
  if (!is.null(n) && length(x) != n) stop("'", name, "' must have length ", n)
  as.character(x)
}

.sk_owner_matrix <- function(ids) {
  out <- outer(ids, ids, FUN = "==") * 1
  dimnames(out) <- list(ids, ids)
  out
}

.sk_roles <- function(owner, leaders, subset = rep(TRUE, length(owner))) {
  active <- unique(owner[subset])
  leaders <- intersect(as.character(leaders), active)
  list(
    active = active,
    leaders = leaders,
    followers = setdiff(active, leaders),
    isLeader = owner %in% leaders,
    isFollower = owner %in% setdiff(active, leaders)
  )
}

.sk_h <- function(shares, owner, leaders, conduct = c("bertrand", "cournot"),
                  subset = rep(TRUE, length(shares))) {
  conduct <- match.arg(conduct)
  shares <- as.numeric(shares)
  if (length(shares) != length(owner) || length(subset) != length(shares)) {
    stop("shares, ownership, and subset must have equal lengths")
  }
  if (any(!is.finite(shares[subset])) || any(shares[subset] <= 0) ||
      sum(shares[subset]) >= 1) {
    stop("Stackelberg Logit requires positive active shares with a positive outside share")
  }
  roles <- .sk_roles(owner, leaders, subset)
  s0 <- 1 - sum(shares[subset])
  firmS <- vapply(roles$active, function(f) sum(shares[subset & owner == f]), numeric(1))
  names(firmS) <- roles$active
  h <- rep(NA_real_, length(shares))
  if (conduct == "bertrand") {
    fs <- firmS[roles$followers]
    B <- if (length(fs)) sum(fs^2 / (1 - fs + fs^2)) else 0
    if (!is.finite(B) || B >= 1) stop("invalid follower-sector Bertrand response multiplier")
    phi <- 1 / (1 - B)
    for (f in roles$followers) h[subset & owner == f] <- 1 / (1 - firmS[f])
    for (f in roles$leaders) h[subset & owner == f] <- 1 / (1 - phi * firmS[f])
  } else {
    sf <- sum(firmS[roles$followers])
    for (f in roles$followers) h[subset & owner == f] <- 1 + firmS[f] / s0
    for (f in roles$leaders) h[subset & owner == f] <- 1 + firmS[f] / (s0 + sf)
  }
  if (any(!is.finite(h[subset])) || any(h[subset] <= 0)) {
    stop("Stackelberg Logit produced a non-positive action multiplier")
  }
  h
}

.sk_margin_from_h <- function(h, prices, alpha) h / (alpha * prices)

.sk_beta <- function(object) {
  beta <- object@slopes$alpha
  if (length(beta) != 1L || !is.finite(beta) || beta == 0) {
    stop("StackelbergLogit demand coefficient is unavailable")
  }
  as.numeric(beta)
}

.sk_active <- function(object, preMerger, subset) {
  n <- length(object@shares)
  if (missing(subset) || is.null(subset)) {
    subset <- if (preMerger) rep(TRUE, n) else object@subset
  }
  if (!is.logical(subset) || length(subset) != n || anyNA(subset) || !any(subset)) {
    stop("'subset' must be a logical vector with at least one active product")
  }
  subset
}

.sk_state <- function(object, preMerger = TRUE, subset) {
  subset <- .sk_active(object, preMerger, subset)
  if (preMerger) {
    owner <- object@firmOwnerPre
    leaders <- object@leadersPre
    prices <- object@pricePre
    if (length(prices) != length(owner)) prices <- object@prices
    costs <- object@mcPre
  } else {
    owner <- object@firmOwnerPost
    leaders <- object@leadersPost
    prices <- object@pricePost
    if (length(prices) != length(owner)) prices <- object@prices
    costs <- object@mcPost
  }
  if (length(prices) != length(owner)) stop("equilibrium prices are not initialized")
  list(subset = subset, owner = owner, leaders = leaders, prices = prices,
       costs = costs, roles = .sk_roles(owner, leaders, subset))
}

.sk_observed <- function(object) {
  z <- object@diagnostics$observedMargins
  if (is.null(z)) object@margins else z
}

.sk_calibration <- function(object, h) {
  prices <- object@prices
  obs <- .sk_observed(object)
  weights <- as.numeric(object@weights)
  if (!length(weights)) weights <- rep(1, length(prices))
  usable <- is.finite(obs) & obs > 0 & is.finite(weights) & weights > 0 &
    is.finite(prices) & prices > 0 & is.finite(h) & h > 0
  if (!any(usable)) stop("no usable positive observed margins; supply a positive 'alpha'")
  x <- h / prices
  den <- sum(weights[usable] * x[usable]^2)
  if (!is.finite(den) || den <= 0) stop("observed margins do not identify a positive Logit price coefficient")
  lambda <- sum(weights[usable] * x[usable] * obs[usable]) / den
  if (!is.finite(lambda) || lambda <= 0) stop("calibrated inverse price coefficient is not positive")
  list(alpha = 1 / lambda, usable = usable, lambda = lambda)
}

.sk_make_diagnostics <- function(object, alpha, h, implied, usable = logical()) {
  observed <- .sk_observed(object)
  list(
    demandparam = alpha,
    alpha = alpha,
    beta = if (object@output) -alpha else alpha,
    observedMargins = observed,
    impliedMargins = implied,
    residuals = observed - implied,
    weights = object@weights,
    usable = usable,
    ownerPre = object@firmOwnerPre,
    ownerPost = object@firmOwnerPost,
    leadersPre = object@leadersPre,
    leadersPost = object@leadersPost,
    solverStatus = list(pre = "not-run", post = "not-run"),
    inadmissible = character()
  )
}

#' Construct a firm-level noncooperative Stackelberg Logit model.
#'
#' `stackelberg()` is intentionally a namespaced coordination constructor;
#' antitrust's legacy linear/log-linear `stackelberg()` remains unchanged.
#' @export
stackelberg <- function(prices, shares, margins = rep(NA_real_, length(prices)),
                        ownerPre, ownerPost = ownerPre, leadersPre,
                        leadersPost = NULL, demand = c("logit", "ces"),
                        conduct = c("bertrand", "cournot"), output = TRUE,
                        insideSize = 1, normIndex = NA, priceOutside = 0,
                        mcDelta = rep(0, length(prices)),
                        subset = rep(TRUE, length(prices)), priceStart = prices,
                        labels = paste0("Prod", seq_along(prices)),
                        weights = rep(1, length(prices)), alpha = NULL,
                        gamma = NULL, control.slopes = list(),
                        control.equ = list(), ...) {
  ownerPostWasMissing <- missing(ownerPost)
  demand <- match.arg(demand)
  conduct <- match.arg(conduct)
  if (demand != "logit") {
    stop("Stackelberg CES is reserved for a later implementation; demand='ces' is unsupported")
  }
  dots <- list(...)
  if (length(dots)) {
    bad <- intersect(names(dots), c("diversions", "nests", "sigma", "mktElast"))
    if (length(bad)) stop("unsupported Stackelberg Logit arguments: ", paste(bad, collapse = ", "))
  }
  .sk_stop_matrix(control.slopes, "control.slopes")
  .sk_stop_matrix(control.equ, "control.equ")
  n <- length(prices)
  prices <- as.numeric(prices); shares <- as.numeric(shares)
  margins <- as.numeric(margins)
  if (length(prices) != n || length(shares) != n || length(margins) != n) {
    stop("prices, shares, and margins must have the same length")
  }
  if (any(!is.finite(prices)) || any(prices <= 0)) stop("prices must be finite and positive")
  if (any(!is.finite(shares)) || any(shares <= 0) || sum(shares) >= 1) {
    stop("shares must be positive unconditional shares summing to less than one")
  }
  if (length(normIndex) != 1L || !is.na(normIndex)) {
    stop("Stackelberg Logit requires an explicit outside good and normIndex=NA")
  }
  if (length(output) != 1L || is.na(output)) stop("'output' must be one logical value")
  if (length(priceOutside) != 1L || !is.finite(priceOutside) || priceOutside < 0) {
    stop("'priceOutside' must be one finite non-negative value")
  }
  ownerPre <- .sk_ids(ownerPre, "ownerPre", n)
  ownerPost <- .sk_ids(ownerPost, "ownerPost", n)
  leadersPre <- .sk_ids(leadersPre, "leadersPre")
  if (any(!leadersPre %in% unique(ownerPre))) stop("leadersPre contains an unknown firm ID")
  if (is.null(leadersPost)) {
    if (!ownerPostWasMissing && !identical(ownerPost, ownerPre)) {
      stop("leadersPost must be supplied when ownerPost changes the ownership partition")
    }
    leadersPost <- leadersPre
  } else {
    leadersPost <- .sk_ids(leadersPost, "leadersPost")
  }
  if (any(!leadersPost %in% unique(ownerPost))) stop("leadersPost contains an unknown firm ID")
  if (!is.logical(subset) || length(subset) != n || anyNA(subset) || !any(subset)) stop("subset must be a logical vector with at least one TRUE and no NA values")
  mcDelta <- as.numeric(mcDelta)
  if (length(mcDelta) != n || any(!is.finite(mcDelta))) stop("mcDelta must be a finite vector matching prices")
  priceStart <- as.numeric(priceStart)
  if (length(priceStart) != n || any(!is.finite(priceStart)) || any(priceStart <= 0)) stop("priceStart must be finite and positive")
  labels <- as.character(labels)
  if (length(labels) != n) stop("labels must have one value per product")
  weights <- as.numeric(weights)
  if (length(weights) != n || any(!is.finite(weights)) || any(weights < 0)) stop("weights must be finite and non-negative")
  if (!is.finite(insideSize) || length(insideSize) != 1L || insideSize <= 0) stop("insideSize must be positive")
  if (!is.null(gamma)) stop("'gamma' is reserved for CES and is unsupported with demand='logit'")
  if (!is.null(alpha) && (length(alpha) != 1L || !is.finite(alpha) || alpha <= 0)) stop("'alpha' must be a positive absolute Logit coefficient")

  hPre <- .sk_h(shares, ownerPre, leadersPre, conduct, rep(TRUE, n))
  impliedPlaceholder <- .sk_margin_from_h(hPre, prices, if (is.null(alpha)) 1 else alpha)
  allMissing <- all(is.na(margins))
  if (allMissing && is.null(alpha)) stop("all margins are missing; supply a positive fixed 'alpha'")
  parentMargins <- margins
  if (allMissing) parentMargins <- impliedPlaceholder
  ## Parent Logit's validity requires at least one non-missing margin.  The
  ## original observations remain in diagnostics and are never used as data
  ## when a fixed alpha was supplied.
  result <- suppressWarnings(new("StackelbergLogit",
    prices = prices, shares = shares, margins = parentMargins,
    diversion = matrix(NA_real_, n, n), normIndex = NA_real_,
    ownerPre = .sk_owner_matrix(ownerPre), ownerPost = .sk_owner_matrix(ownerPost),
    insideSize = insideSize, output = isTRUE(output), mcDelta = mcDelta,
    subset = subset, weights = weights, priceOutside = priceOutside,
    priceStart = priceStart, shareInside = sum(shares),
    mktSize = insideSize / sum(shares), labels = labels,
    conduct = conduct, leadersPre = leadersPre, leadersPost = leadersPost,
    firmOwnerPre = ownerPre, firmOwnerPost = ownerPost,
    diagnostics = list(observedMargins = margins),
    alphaFixed = if (is.null(alpha)) NA_real_ else as.numeric(alpha)
  ))
  if (length(control.slopes)) result@control.slopes <- control.slopes
  if (length(control.equ)) result@control.equ <- control.equ
  result <- calcSlopes(result)
  ## Costs are fixed at the baseline hard-role FOCs and then shocked
  ## proportionally.  No post-equilibrium FOC is used to reinvert costs.
  a <- result@diagnostics$demandparam
  result@mcPre <- if (isTRUE(output)) prices - hPre / a else prices + hPre / a
  if (any(!is.finite(result@mcPre))) stop("baseline marginal costs are not finite")
  result@mcPost <- result@mcPre * (1 + mcDelta)
  result@pricePre <- calcPrices(result, TRUE, subset = rep(TRUE, n))
  result@pricePost <- calcPrices(result, FALSE, subset = subset)
  ## Check convergence from an independently perturbed baseline start.  This
  ## diagnostic is informative and keeps all structural costs fixed.
  tmp <- result
  tmp@priceStart <- pmax(prices * 1.17 + 0.031, .Machine$double.eps^0.25)
  pchk <- try(calcPrices(tmp, TRUE, subset = rep(TRUE, n)), silent = TRUE)
  baselineErr <- if (inherits(pchk, "try-error")) Inf else max(abs(pchk - prices))
  result@diagnostics$baselineReproduction <- baselineErr
  result@diagnostics$baselineReproductionPrices <- if (inherits(pchk, "try-error")) rep(NA_real_, n) else pchk
  result@diagnostics$solverStatus <- list(pre = "converged", post = "converged")
  result@diagnostics$residualsPre <- stackelberg_residuals(result, TRUE)
  result@diagnostics$residualsPost <- stackelberg_residuals(result, FALSE)
  ## Run the independent implicit leader route as a diagnostic.  A failure is
  ## reported in diagnostics while preserving the analytic equilibrium as the
  ## constructor's result.
  impPre <- try(calcPrices(result, TRUE, method = "implicit"), silent = TRUE)
  impPost <- try(calcPrices(result, FALSE, method = "implicit"), silent = TRUE)
  result@diagnostics$implicit <- list(
    pre = if (inherits(impPre, "try-error")) rep(NA_real_, n) else impPre,
    post = if (inherits(impPost, "try-error")) rep(NA_real_, n) else impPost,
    statusPre = if (inherits(impPre, "try-error")) "failed" else "converged",
    statusPost = if (inherits(impPost, "try-error")) "failed" else "converged",
    maxDifferencePre = if (inherits(impPre, "try-error")) Inf else max(abs(impPre - result@pricePre), na.rm = TRUE),
    maxDifferencePost = if (inherits(impPost, "try-error")) Inf else max(abs(impPost - result@pricePost), na.rm = TRUE)
  )
  result
}

#' Calibrate the Logit coefficient for a Stackelberg model.
#' @export
setMethod("calcSlopes", "StackelbergLogit", function(object, ...) {
  h <- .sk_h(object@shares, object@firmOwnerPre, object@leadersPre,
             object@conduct, rep(TRUE, length(object@shares)))
  if (length(object@alphaFixed) == 1L && is.finite(object@alphaFixed) && object@alphaFixed > 0) {
    a <- as.numeric(object@alphaFixed)
    usable <- is.finite(.sk_observed(object)) & is.finite(object@weights) & object@weights > 0
  } else {
    fit <- .sk_calibration(object, h)
    a <- fit$alpha
    usable <- fit$usable
  }
  beta <- if (isTRUE(object@output)) -a else a
  meanval <- log(object@shares / (1 - sum(object@shares))) -
    beta * (object@prices - object@priceOutside)
  names(meanval) <- object@labels
  object@slopes <- list(alpha = beta, meanval = meanval)
  object@priceOutside <- as.numeric(object@priceOutside)
  implied <- .sk_margin_from_h(h, object@prices, a)
  d <- .sk_make_diagnostics(object, a, h, implied, usable)
  d$fixedAlpha <- is.finite(object@alphaFixed) && object@alphaFixed > 0
  object@diagnostics <- modifyList(object@diagnostics, d)
  object@mktSize <- object@insideSize / sum(object@shares)
  object
})

#' Return the persistent Stackelberg marginal-cost/value primitive.
#' @export
setMethod("calcMC", "StackelbergLogit", function(object, preMerger = TRUE) {
  mc <- if (preMerger) object@mcPre else object@mcPost
  if (!length(mc)) stop("Stackelberg marginal costs are not initialized")
  names(mc) <- object@labels
  mc <- as.numeric(mc)
  names(mc) <- object@labels
  mc
})

#' Compute hard-role Stackelberg multipliers or proportional margins.
#' @export
setMethod("calcMargins", "StackelbergLogit", function(object, preMerger = TRUE,
                                                        level = FALSE) {
  st <- .sk_state(object, preMerger)
  if (preMerger) st$prices <- object@pricePre
  shares <- calcShares(object, preMerger = preMerger, revenue = FALSE)
  h <- .sk_h(shares, st$owner, st$leaders, object@conduct, st$subset)
  a <- abs(.sk_beta(object))
  out <- .sk_margin_from_h(h, st$prices, a)
  out[!st$subset] <- NA_real_
  names(out) <- object@labels
  if (level) out <- out * st$prices
  names(out) <- object@labels
  out
})

.sk_price_root <- function(object, preMerger, subset, start = NULL, ...) {
  n <- length(object@shares)
  st <- .sk_state(object, preMerger, subset)
  if (is.null(start)) start <- if (preMerger) object@priceStart else {
    p <- object@pricePost
    if (length(p) != n || any(!is.finite(p[subset]) | p[subset] <= 0)) object@priceStart else p
  }
  start <- as.numeric(start)[subset]
  if (length(start) != sum(subset) || any(!is.finite(start)) || any(start <= 0)) start <- object@prices[subset]
  costs <- st$costs[subset]
  beta <- .sk_beta(object)
  owner <- st$owner
  leader <- st$leaders
  target <- function(p) {
    this <- object
    if (preMerger) this@pricePre <- {z <- rep(NA_real_, n); z[subset] <- p; z[!subset] <- object@prices[!subset]; z}
    else this@pricePost <- {z <- rep(NA_real_, n); z[subset] <- p; z[!subset] <- NA_real_; z}
    s <- calcShares(this, preMerger = preMerger, revenue = FALSE)
    h <- .sk_h(s, owner, leader, object@conduct, subset)
    sign <- if (isTRUE(object@output)) 1 else -1
    costs + sign * h[subset] / abs(beta)
  }
  foc <- function(z) {
    p <- exp(z)
    tg <- try(target(p), silent = TRUE)
    if (inherits(tg, "try-error") || any(!is.finite(tg)) || any(tg <= 0)) return(rep(1e6, length(p)))
    log(p / tg)
  }
  ctl <- object@control.equ
  maxit <- as.integer(ctl$maxit %||% 300L)
  if (!is.finite(maxit) || maxit < 20) maxit <- 300L
  tol <- as.numeric(ctl$tol %||% 1e-10)
  if (!is.finite(tol) || tol <= 0) tol <- 1e-10
  sol <- try(nleqslv::nleqslv(log(start), foc, method = "Broyden",
                              control = list(ftol = tol, maxit = maxit)), silent = TRUE)
  z <- if (!inherits(sol, "try-error") && is.finite(sol$termcd) && sol$termcd <= 2) sol$x else NULL
  if (is.null(z)) {
    bb <- try(BB::BBsolve(log(start), foc, quiet = TRUE, control = ctl), silent = TRUE)
    if (!inherits(bb, "try-error") && is.finite(bb$convergence) && bb$convergence == 0) z <- bb$par
  }
  if (is.null(z) || any(!is.finite(z))) stop("Stackelberg price equilibrium solver failed")
  ## Refine the positive price root with a few Newton steps.  This keeps the
  ## exposed FOC residual below the numerical gate even when Broyden stops on
  ## a very flat follower-share direction.
  for (it in seq_len(8L)) {
    rr <- foc(z)
    if (all(is.finite(rr)) && max(abs(rr), na.rm = TRUE) <= 1e-12) break
    JJ <- try(numDeriv::jacobian(foc, z), silent = TRUE)
    dz <- if (!inherits(JJ, "try-error")) try(solve(JJ, -rr), silent = TRUE) else structure("bad", class = "try-error")
    if (inherits(dz, "try-error") || any(!is.finite(dz))) break
    ## A short backtracking line search protects the positivity/domain gate.
    old <- if (all(is.finite(rr))) sum(rr^2) else Inf
    step <- 1
    repeat {
      zn <- z + step * dz
      rn <- foc(zn)
      if (all(is.finite(rn)) && sum(rn^2) <= old) { z <- zn; break }
      step <- step / 2
      if (step < 1 / 128) break
    }
  }
  p <- exp(z)
  r <- foc(z)
  if (any(!is.finite(p)) || max(abs(r), na.rm = TRUE) > 2e-10) {
    stop("Stackelberg price equilibrium residual exceeds tolerance")
  }
  p
}

.sk_implicit_price_root <- function(object, preMerger, subset) {
  st <- .sk_state(object, preMerger, subset)
  li <- which(subset & st$owner %in% st$leaders)
  fi <- which(subset & !(st$owner %in% st$leaders))
  n <- length(object@shares); M <- object@mktSize
  if (!length(li)) return(.sk_price_root(object, preMerger, subset))
  if (object@conduct == "bertrand") {
    p0 <- if (preMerger) object@priceStart else object@pricePost
    if (length(p0) != n || any(!is.finite(p0[li])) || any(p0[li] <= 0)) p0 <- object@prices
    start <- p0[li]
  } else {
    s0 <- calcShares(object, preMerger, revenue = FALSE)
    start <- M * s0[li]
  }
  candidate <- function(a) {
    z <- try({
      if (object@conduct == "bertrand") {
        ff <- stackelberg_followers(object, a, preMerger = preMerger)
        p <- ff$prices
        q <- ff$quantities
      } else {
        q <- .sk_cournot_follower_solution(object, a, preMerger, subset)
        s <- q / M
        p <- object@priceOutside + (log(s / (1 - sum(s))) - object@slopes$meanval) / .sk_beta(object)
      }
      tmp <- object
      if (preMerger) tmp@pricePre <- p else tmp@pricePost <- p
      R <- .sk_implicit_response_at(tmp, preMerger, subset)
      g <- .sk_leader_foc_with_response(tmp, preMerger, subset, R = R)
      ss <- calcShares(tmp, preMerger, revenue = FALSE)
      scale <- if (object@conduct == "bertrand") M * ss[li] else p[li]
      eq <- g / pmax(scale, 1e-10)
      list(eq = eq, p = p, q = q)
    }, silent = TRUE)
    if (inherits(z, "try-error") || any(!is.finite(z$eq))) {
      list(eq = rep(1e6, length(li)), p = rep(NA_real_, n), q = rep(NA_real_, n))
    } else z
  }
  foc <- function(z) candidate(exp(z))$eq
  sol <- try(nleqslv::nleqslv(log(start), foc, method = "Broyden",
                              control = list(ftol = 1e-9, maxit = 500L)), silent = TRUE)
  solNeedsFallback <- inherits(sol, "try-error") || sol$termcd > 2 || any(!is.finite(sol$x))
  if (!solNeedsFallback) {
    probe <- try(candidate(exp(sol$x)), silent = TRUE)
    solNeedsFallback <- inherits(probe, "try-error") || any(!is.finite(probe$eq)) ||
      max(abs(probe$eq), na.rm = TRUE) > 2e-7
  }
  if (solNeedsFallback) {
    ## The nested follower solve can make a Broyden step leave the admissible
    ## demand domain.  A bounded least-squares pass supplies a robust
    ## independent starting point for the same implicit equations.
    lo <- rep(log(.Machine$double.eps^0.25), length(start))
    hi <- rep(log(max(c(start, object@prices, 1), na.rm = TRUE) * 1e4), length(start))
    opt <- try(stats::optim(log(start), function(z) sum(foc(z)^2),
                            method = "L-BFGS-B", lower = lo, upper = hi,
                            control = list(maxit = 1000L, factr = 1e7)), silent = TRUE)
    if (inherits(opt, "try-error") || any(!is.finite(opt$par))) stop("implicit Stackelberg leader solver failed")
    sol <- list(x = opt$par, termcd = 1)
  }
  ## Refine the independent nested equations after either solver.  This is
  ## especially useful when the follower's Broyden solve is accurate in
  ## absolute but not relative terms.
  zg <- sol$x
  for (it in seq_len(8L)) {
    rr <- try(foc(zg), silent = TRUE)
    if (inherits(rr, "try-error") || any(!is.finite(rr)) || max(abs(rr), na.rm = TRUE) <= 1e-10) break
    JJ <- try(numDeriv::jacobian(foc, zg), silent = TRUE)
    dz <- if (!inherits(JJ, "try-error")) try(solve(JJ, -rr), silent = TRUE) else structure("bad", class = "try-error")
    if (inherits(dz, "try-error") || any(!is.finite(dz))) break
    old <- sum(rr^2); step <- 1
    repeat {
      zn <- zg + step * dz; rn <- try(foc(zn), silent = TRUE)
      if (!inherits(rn, "try-error") && all(is.finite(rn)) && sum(rn^2) <= old) { zg <- zn; break }
      step <- step / 2
      if (step < 1 / 128) break
    }
  }
  sol$x <- zg
  got <- candidate(exp(sol$x))
  if (max(abs(got$eq), na.rm = TRUE) > 2e-7) stop("implicit Stackelberg leader residual exceeds tolerance")
  got$p[subset]
}

`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x[[1L]]

#' Solve the closed-form Stackelberg price equations.
#' @export
setMethod("calcPrices", "StackelbergLogit", function(object, preMerger = TRUE,
                                                       isMax = FALSE, subset,
                                                       method = c("analytic", "implicit"), ...) {
  subset <- .sk_active(object, preMerger, subset)
  method <- match.arg(method)
  p <- if (method == "implicit") .sk_implicit_price_root(object, preMerger, subset) else .sk_price_root(object, preMerger, subset, ...)
  out <- rep(NA_real_, length(object@shares))
  out[subset] <- p
  if (preMerger) out[!subset] <- object@prices[!subset]
  names(out) <- object@labels
  out
})

.sk_raw_foc <- function(object, action, preMerger = TRUE, subset = NULL) {
  subset <- .sk_active(object, preMerger, subset)
  st <- .sk_state(object, preMerger, subset)
  n <- length(object@shares)
  owner <- st$owner
  leaders <- st$leaders
  beta <- .sk_beta(object)
  M <- object@mktSize
  sigma <- if (isTRUE(object@output)) 1 else -1
  if (object@conduct == "bertrand") {
    p <- rep(NA_real_, n); p[subset] <- action
    if (preMerger) p[!subset] <- object@prices[!subset]
    z <- object
    if (preMerger) z@pricePre <- p else z@pricePost <- p
    s <- calcShares(z, preMerger = preMerger, revenue = FALSE)
    mu <- p - st$costs
    g <- rep(NA_real_, n)
    for (f in unique(owner[subset])) {
      ix <- which(subset & owner == f)
      H <- sum(mu[ix] * s[ix])
      g[ix] <- sigma * M * s[ix] * (1 + beta * (mu[ix] - H))
    }
    return(g[subset])
  }
  q <- rep(NA_real_, n); q[subset] <- action
  if (preMerger) q[!subset] <- object@mktSize * object@shares[!subset] else q[!subset] <- 0
  if (any(!is.finite(q[subset])) || any(q[subset] <= 0)) return(rep(1e6, sum(subset)))
  s <- q / M; s0 <- 1 - sum(s[subset])
  if (!is.finite(s0) || s0 <= 0) return(rep(1e6, sum(subset)))
  meanval <- object@slopes$meanval
  p <- object@priceOutside + (log(s / s0) - meanval) / beta
  mu <- p - st$costs
  g <- rep(NA_real_, n)
  for (f in unique(owner[subset])) {
    ix <- which(subset & owner == f)
    sf <- sum(s[ix])
    ## The signed inverse-demand coefficient makes the quantity FOC
    ## mu + (1+S_f/s0)/beta = 0 for both output and input markets.
  ## This is the raw derivative with respect to actual q (no market-size
  ## rescaling); sigma changes the input-market payoff orientation.
  g[ix] <- sigma * (mu[ix] + (1 + sf / s0) / beta)
  }
  g[subset]
}

.sk_leader_foc_with_response <- function(object, preMerger = TRUE, subset = NULL, R = NULL) {
  subset <- .sk_active(object, preMerger, subset)
  st <- .sk_state(object, preMerger, subset)
  n <- length(object@shares)
  s <- calcShares(object, preMerger = preMerger, revenue = FALSE)
  M <- object@mktSize
  beta <- .sk_beta(object)
  sigma <- if (isTRUE(object@output)) 1 else -1
  li <- which(subset & st$owner %in% st$leaders)
  fi <- which(subset & !(st$owner %in% st$leaders))
  ans <- setNames(rep(NA_real_, n), object@labels)
  if (!length(li)) return(numeric(0))
  if (is.null(R)) {
    R <- if (length(fi)) stackelberg_response(object, preMerger = preMerger, method = "analytic") else matrix(numeric(0), 0, length(li))
  }
  mu <- st$prices - st$costs
  if (object@conduct == "bertrand") {
    for (f in unique(st$owner[li])) {
      own <- which(subset & st$owner == f)
      H <- sum(mu[own] * s[own])
      ## Derivative with respect to a foreign product's price has no direct
      ## revenue term: it is only -beta*s_j times the leader's weighted
      ## margin.  Own products additionally carry the direct s_k term.
      direct <- sigma * (-M * s * beta * H)
      direct[own] <- sigma * M * s[own] * (1 + beta * (mu[own] - H))
      for (k in own) {
        col <- match(k, li)
        ans[k] <- direct[k] + if (length(fi)) sum(direct[fi] * R[, col]) else 0
      }
    }
  } else {
    s0 <- 1 - sum(s[subset])
    K <- matrix(0, n, n)
    K[subset, subset] <- outer(which(subset), which(subset),
      ## p=(log(s/s0)-delta)/beta, so the inverse-demand derivative carries
      ## the signed coefficient directly (beta=-a for output).
      FUN = function(i, k) ((i == k) / s[i] + 1 / s0) / (beta * M))
    q <- M * s
    for (f in unique(st$owner[li])) {
      own <- which(subset & st$owner == f)
      direct <- sigma * as.numeric(crossprod(q[own], K[own, , drop = FALSE]))
      direct[own] <- direct[own] + sigma * mu[own]
      for (k in own) {
        col <- match(k, li)
        ans[k] <- direct[k] + if (length(fi)) sum(direct[fi] * R[, col]) else 0
      }
    }
  }
  ans[li]
}

.sk_reduced_leader_foc <- function(object, preMerger = TRUE, subset = NULL) {
  .sk_leader_foc_with_response(object, preMerger, subset, R = NULL)
}

.sk_implicit_response_at <- function(object, preMerger = TRUE, subset = NULL) {
  subset <- .sk_active(object, preMerger, subset)
  st <- .sk_state(object, preMerger, subset)
  fi <- which(subset & !(st$owner %in% st$leaders))
  li <- which(subset & st$owner %in% st$leaders)
  if (!length(fi) || !length(li)) return(matrix(numeric(0), nrow = length(fi), ncol = length(li),
                                                  dimnames = list(object@labels[fi], object@labels[li])))
  action <- if (object@conduct == "bertrand") st$prices[subset] else object@mktSize * calcShares(object, preMerger, revenue = FALSE)[subset]
  J <- numDeriv::jacobian(function(a) .sk_raw_foc(object, a, preMerger, subset), action)
  posF <- match(fi, which(subset)); posL <- match(li, which(subset))
  Jff <- J[posF, posF, drop = FALSE]; Jfl <- J[posF, posL, drop = FALSE]
  ans <- try(-solve(Jff, Jfl), silent = TRUE)
  if (inherits(ans, "try-error") || any(!is.finite(ans))) stop("follower FOC Jacobian is singular")
  dimnames(ans) <- list(object@labels[fi], object@labels[li])
  ans
}

.sk_cournot_follower_solution <- function(object, leaderQuantities,
                                          preMerger = TRUE, subset = NULL) {
  subset <- .sk_active(object, preMerger, subset)
  st <- .sk_state(object, preMerger, subset)
  n <- length(object@shares); M <- object@mktSize
  beta <- .sk_beta(object)
  li <- which(subset & st$owner %in% st$leaders)
  fi <- which(subset & !(st$owner %in% st$leaders))
  q <- rep(0, n)
  q[li] <- as.numeric(leaderQuantities)
  if (length(q[li]) != length(li) || any(!is.finite(q[li])) || any(q[li] <= 0)) {
    stop("leader quantities must be finite and positive")
  }
  leftover <- 1 - sum(q[li]) / M
  if (!is.finite(leftover) || leftover <= 0) {
    stop("leader quantities must leave a positive quantity share for followers and the outside good")
  }
  fs <- split(fi, st$owner[fi])
  tf <- numeric(length(fs)); names(tf) <- names(fs)
  logu <- rep(NA_real_, n)
  delta <- object@slopes$meanval
  for (f in names(fs)) {
    ix <- fs[[f]]
    arg <- delta[ix] + beta * (st$costs[ix] - object@priceOutside) - 1
    if (any(!is.finite(arg))) stop("non-finite follower inverse-demand primitive")
    ma <- max(arg)
    logA <- ma + log(sum(exp(arg - ma)))
    ## Solve log(t)+t=log(A) by safeguarded Newton.  This is the positive
    ## Lambert-W branch and avoids a multidimensional quantity root.
    t <- if (logA < 0) exp(logA) else max(logA - log1p(logA), 1e-8)
    for (it in seq_len(100L)) {
      step <- (log(t) + t - logA) / (1 / t + 1)
      tn <- t - step
      if (!is.finite(tn) || tn <= 0) tn <- t / 2
      if (abs(tn - t) <= 1e-13 * (1 + abs(t))) { t <- tn; break }
      t <- tn
    }
    if (!is.finite(t) || t <= 0 || abs(log(t) + t - logA) > 1e-8) stop("follower quantity closed form failed")
    tf[f] <- t
    logu[ix] <- arg - t
  }
  s0 <- leftover / (1 + sum(tf))
  q[fi] <- M * s0 * exp(logu[fi])
  if (any(!is.finite(q[fi])) || any(q[fi] <= 0) || sum(q) >= M) stop("follower solution leaves no positive outside share")
  q
}

#' Return follower choices at fixed leader actions.
#' @export
stackelberg_followers <- function(object, leaderActions, preMerger = TRUE, start = NULL) {
  if (!methods::is(object, "StackelbergLogit")) stop("object must be a StackelbergLogit")
  subset <- .sk_active(object, preMerger)
  st <- .sk_state(object, preMerger, subset)
  sigma <- if (isTRUE(object@output)) 1 else -1
  leaderProducts <- which(subset & st$owner %in% st$leaders)
  followerProducts <- which(subset & !(st$owner %in% st$leaders))
  if (length(leaderActions) == length(object@shares)) leaderActions <- leaderActions[leaderProducts]
  leaderActions <- as.numeric(leaderActions)
  if (length(leaderActions) != length(leaderProducts) || any(!is.finite(leaderActions)) || any(leaderActions <= 0)) {
    stop("leaderActions must be positive actions for every active leader product")
  }
  if (!length(followerProducts)) {
    if (object@conduct == "bertrand") {
      p <- rep(NA_real_, length(subset)); p[subset] <- 0; p[leaderProducts] <- leaderActions
      z <- object
      if (preMerger) z@pricePre <- p else z@pricePost <- p
      q <- calcQuantities(z, preMerger = preMerger)
    } else {
      q <- rep(NA_real_, length(subset)); q[subset] <- 0; q[leaderProducts] <- leaderActions
      s <- q / object@mktSize
      s0 <- 1 - sum(s[subset])
      if (!is.finite(s0) || s0 <= 0) stop("leader quantities must leave a positive outside share")
      p <- object@priceOutside + (log(s / s0) - object@slopes$meanval) / .sk_beta(object)
    }
    return(list(choices = if (object@conduct == "bertrand") p else q,
                prices = p, quantities = q,
                residuals = numeric(), leaderProducts = leaderProducts,
                followerProducts = followerProducts, converged = TRUE))
  }
  if (object@conduct == "cournot") {
    ## The audited inverse-demand FOCs reduce each follower firm to a scalar
    ## Lambert-W equation.  This remains well behaved for sizable leader
    ## deviations as long as leaders leave a positive outside share.
    q <- .sk_cournot_follower_solution(object, leaderActions, preMerger, subset)
    s <- q / object@mktSize
    p <- object@priceOutside + (log(s / (1 - sum(s))) - object@slopes$meanval) / .sk_beta(object)
    this <- object
    if (preMerger) this@pricePre <- p else this@pricePost <- p
    r <- .sk_raw_foc(this, q[subset], preMerger, subset)
    if (max(abs(r[match(followerProducts, which(subset))]), na.rm = TRUE) > 1e-6) {
      stop("follower equilibrium residual exceeds tolerance")
    }
    return(list(choices = q, prices = p, quantities = q,
                shares = s, residuals = r,
                leaderProducts = leaderProducts, followerProducts = followerProducts,
                converged = TRUE))
  }
  if (is.null(start)) start <- if (object@conduct == "bertrand") st$prices[followerProducts] else object@mktSize * calcShares(object, preMerger, revenue = FALSE)[followerProducts]
  start <- pmax(as.numeric(start), .Machine$double.eps^0.25)
  if (length(start) != length(followerProducts)) stop("start must match follower products")
  fixedRaw <- function(z) {
    a <- exp(z)
    if (object@conduct == "bertrand") {
      p <- rep(NA_real_, length(subset)); p[leaderProducts] <- leaderActions; p[followerProducts] <- a
      this <- object
      if (preMerger) this@pricePre <- p else this@pricePost <- p
      .sk_raw_foc(this, p[subset], preMerger, subset)[match(followerProducts, which(subset))]
    } else {
      q <- rep(0, length(subset)); q[leaderProducts] <- leaderActions; q[followerProducts] <- a
      this <- object
      .sk_raw_foc(this, q[subset], preMerger, subset)[match(followerProducts, which(subset))]
    }
  }
  ## Solve using a payoff-orientation-free equation; retain fixedRaw for
  ## diagnostics below.  Multiplying all follower rows by a common sign does
  ## not change the implicit reaction.
  fixed <- function(z) fixedRaw(z) / sigma
  sol <- try(nleqslv::nleqslv(log(start), function(z) fixed(z), method = "Broyden",
                              control = list(ftol = 1e-14, xtol = 1e-14, maxit = 400L)), silent = TRUE)
  if (inherits(sol, "try-error") || any(!is.finite(sol$x)) ||
      (sol$termcd > 2 && (is.null(sol$fvec) || max(abs(sol$fvec), na.rm = TRUE) > 1e-9))) {
    stop("follower equilibrium solver failed")
  }
  ## A high precision Newton polish is useful for finite-difference callers:
  ## the public follower map should not expose the root solver's stopping
  ## tolerance as apparent economic curvature.
  zz <- sol$x
  for (it in seq_len(6L)) {
    rr <- fixed(zz)
    if (all(is.finite(rr)) && max(abs(rr), na.rm = TRUE) <= 1e-12) break
    JJ <- try(numDeriv::jacobian(fixed, zz), silent = TRUE)
    dz <- if (!inherits(JJ, "try-error")) try(solve(JJ, -rr), silent = TRUE) else structure("bad", class = "try-error")
    if (inherits(dz, "try-error") || any(!is.finite(dz))) break
    old <- sum(rr^2); step <- 1
    repeat {
      zn <- zz + step * dz; rn <- try(fixed(zn), silent = TRUE)
      if (!inherits(rn, "try-error") && all(is.finite(rn)) && sum(rn^2) <= old) { zz <- zn; break }
      step <- step / 2
      if (step < 1 / 128) break
    }
  }
  sol$x <- zz
  followers <- exp(sol$x)
  if (object@conduct == "bertrand") {
    p <- rep(NA_real_, length(subset)); p[leaderProducts] <- leaderActions; p[followerProducts] <- followers
    this <- object; if (preMerger) this@pricePre <- p else this@pricePost <- p
    q <- calcQuantities(this, preMerger = preMerger)
    r <- fixedRaw(sol$x)
    choices <- p
  } else {
    q <- rep(0, length(subset)); q[leaderProducts] <- leaderActions; q[followerProducts] <- followers
    s <- q / object@mktSize
    p <- object@priceOutside + (log(s / (1 - sum(s))) - object@slopes$meanval) / .sk_beta(object)
    r <- fixed(sol$x)
    choices <- q
  }
  if (max(abs(r), na.rm = TRUE) > 1e-6) stop("follower equilibrium residual exceeds tolerance")
  list(choices = choices, prices = p, quantities = q,
       shares = q / object@mktSize, residuals = r,
       leaderProducts = leaderProducts, followerProducts = followerProducts,
       converged = TRUE)
}

#' Compute follower reactions by the audited formula or an implicit Jacobian.
#' @export
stackelberg_response <- function(object, preMerger = TRUE,
                                 method = c("analytic", "implicit")) {
  method <- match.arg(method)
  subset <- .sk_active(object, preMerger)
  st <- .sk_state(object, preMerger, subset)
  li <- which(subset & st$owner %in% st$leaders)
  fi <- which(subset & !(st$owner %in% st$leaders))
  out <- matrix(numeric(), nrow = length(fi), ncol = length(li),
                dimnames = list(object@labels[fi], object@labels[li]))
  if (!length(fi) || !length(li)) return(out)
  if (method == "analytic") {
    s <- calcShares(object, preMerger, revenue = FALSE)
    M <- object@mktSize
    if (object@conduct == "bertrand") {
      fs <- tapply(s[subset], st$owner[subset], sum)
      ff <- setdiff(names(fs), st$leaders)
      B <- if (length(ff)) sum(fs[ff]^2 / (1 - fs[ff] + fs[ff]^2)) else 0
      phi <- 1 / (1 - B)
      for (r in seq_along(fi)) {
        f <- st$owner[fi[r]]; d <- 1 - fs[f] + fs[f]^2
        out[r, ] <- (fs[f] / d) * phi * s[li]
      }
    } else {
      s0 <- 1 - sum(s[subset]); sf <- sum(s[fi])
      q <- M * s
      out[,] <- -q[fi] / (M * (s0 + sf))
    }
    return(out)
  }
  ## Raw-profit FOC derivatives hold costs fixed.  The resulting Jacobian is
  ## the independent implicit diagnostic for the closed-form response.
  .sk_implicit_response_at(object, preMerger, subset)
}

#' Expose leader and follower first-order-condition residuals.
#' @export
stackelberg_residuals <- function(object, preMerger = TRUE,
                                  prices = NULL, quantities = NULL) {
  subset <- .sk_active(object, preMerger)
  st <- .sk_state(object, preMerger, subset)
  n <- length(object@shares)
  tmp <- object
  if (object@conduct == "bertrand") {
    if (!is.null(quantities)) stop("quantities are unsupported for Bertrand residuals")
    if (is.null(prices)) prices <- st$prices[subset] else {
      prices <- as.numeric(prices)
      if (length(prices) == n) prices <- prices[subset]
      if (length(prices) != sum(subset) || any(!is.finite(prices)) || any(prices <= 0)) {
        stop("prices must be positive and have full or active-product length")
      }
    }
    full <- st$prices
    full[subset] <- prices
    if (preMerger) tmp@pricePre <- full else tmp@pricePost <- full
    g <- .sk_raw_foc(tmp, prices, preMerger, subset)
  } else {
    if (!is.null(prices)) stop("prices are unsupported for Cournot residuals; supply quantities")
    if (is.null(quantities)) quantities <- object@mktSize * calcShares(object, preMerger, revenue = FALSE)[subset] else {
      quantities <- as.numeric(quantities)
      if (length(quantities) == n) quantities <- quantities[subset]
      if (length(quantities) != sum(subset) || any(!is.finite(quantities)) || any(quantities <= 0)) {
        stop("quantities must be positive and have full or active-product length")
      }
    }
    qfull <- rep(0, n); qfull[subset] <- quantities
    s <- qfull / object@mktSize; s0 <- 1 - sum(s[subset])
    if (!is.finite(s0) || s0 <= 0) stop("quantities must leave a positive outside share")
    tmpPrice <- object@priceOutside + (log(s / s0) - object@slopes$meanval) / .sk_beta(object)
    if (preMerger) tmp@pricePre <- tmpPrice else tmp@pricePost <- tmpPrice
    g <- .sk_raw_foc(tmp, quantities, preMerger, subset)
  }
  li <- st$owner[subset] %in% st$leaders
  ## Followers use their raw Nash FOCs.  Leaders use reduced-profit FOCs,
  ## including the fixed-cost follower reaction.  This is the relevant
  ## Stackelberg stationarity diagnostic.
  leader <- .sk_reduced_leader_foc(tmp, preMerger, subset)
  follower <- g[!li]
  all <- rep(NA_real_, length(g))
  all[li] <- leader
  all[!li] <- follower
  maxLeader <- if (length(leader) && any(is.finite(leader))) max(abs(leader), na.rm = TRUE) else 0
  maxFollower <- if (length(follower) && any(is.finite(follower))) max(abs(follower), na.rm = TRUE) else 0
  list(leader = leader, follower = follower, all = all,
       maxLeader = maxLeader,
       maxFollower = maxFollower,
       max = max(c(if (length(leader)) abs(leader) else 0,
                   if (length(follower)) abs(follower) else 0), na.rm = TRUE),
       leaderProducts = which(subset)[li], followerProducts = which(subset)[!li],
       status = "evaluated")
}

#' Apply ownership and proportional marginal-cost counterfactuals.
#' @export
stackelberg_simulate <- function(object, ownerPost = object@firmOwnerPost,
                                 leadersPost = NULL, mcDelta = object@mcDelta,
                                 subset = object@subset, ...) {
  if (!methods::is(object, "StackelbergLogit")) stop("object must be a StackelbergLogit")
  n <- length(object@shares)
  ownerPostWasSame <- identical(as.character(ownerPost), object@firmOwnerPost)
  ownerPost <- .sk_ids(ownerPost, "ownerPost", n)
  if (is.null(leadersPost)) {
    if (!ownerPostWasSame) {
      stop("leadersPost must be supplied when ownerPost changes the ownership partition")
    }
    leadersPost <- object@leadersPost
  } else leadersPost <- .sk_ids(leadersPost, "leadersPost")
  if (any(!leadersPost %in% unique(ownerPost))) stop("leadersPost contains an unknown firm ID")
  if (!is.logical(subset) || length(subset) != n || anyNA(subset) || !any(subset)) stop("subset must be a logical vector with at least one TRUE and no NA values")
  mcDelta <- as.numeric(mcDelta)
  if (length(mcDelta) != n || any(!is.finite(mcDelta))) stop("mcDelta must be a finite vector matching prices")
  out <- object
  out@firmOwnerPost <- ownerPost
  out@leadersPost <- leadersPost
  out@ownerPost <- .sk_owner_matrix(ownerPost)
  out@mcDelta <- mcDelta
  out@subset <- subset
  out@mcPost <- object@mcPre * (1 + mcDelta)
  out@pricePost <- calcPrices(out, FALSE, subset = subset)
  out@diagnostics$ownerPost <- ownerPost
  out@diagnostics$leadersPost <- leadersPost
  out@diagnostics$mcDelta <- mcDelta
  out@diagnostics$subset <- subset
  out@diagnostics$counterfactual <- stackelberg_residuals(out, FALSE)
  out
}
