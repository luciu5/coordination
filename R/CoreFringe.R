# Deterministic exact core-fringe games.  These classes are deliberately
# separate from StackelbergLogit/StackelbergCES: every active product solves
# the same simultaneous hard-role equilibrium, with no sequential leader.

setClass(
  "CoreFringeLogit", contains = "Logit",
  representation = representation(
    conduct = "character", corePre = "character", corePost = "character",
    firmOwnerPre = "character", firmOwnerPost = "character",
    diagnostics = "list", alphaFixed = "numeric"
  ),
  prototype = prototype(conduct = "bertrand", corePre = character(),
                        corePost = character(), firmOwnerPre = character(),
                        firmOwnerPost = character(), diagnostics = list(),
                        alphaFixed = NA_real_),
  validity = function(object) {
    n <- length(object@shares)
    if (length(object@conduct) != 1L || !object@conduct %in% c("bertrand", "cournot"))
      return("'conduct' must be 'bertrand' or 'cournot'")
    if (length(object@firmOwnerPre) != n || length(object@firmOwnerPost) != n)
      return("ownership vectors must have one ID per product")
    if (anyNA(object@firmOwnerPre) || anyNA(object@firmOwnerPost) ||
        any(!nzchar(object@firmOwnerPre)) || any(!nzchar(object@firmOwnerPost)))
      return("firm ownership IDs must be non-missing and non-empty")
    if (any(!object@corePre %in% unique(object@firmOwnerPre)) ||
        any(!object@corePost %in% unique(object@firmOwnerPost)))
      return("core IDs must occur in the corresponding ownership vector")
    if (length(object@alphaFixed) != 1L ||
        (!is.na(object@alphaFixed) && (!is.finite(object@alphaFixed) || object@alphaFixed <= 0)))
      return("'alphaFixed' must be a positive scalar or NA before calibration")
    TRUE
  }
)

setClass(
  "CoreFringeCES", contains = "CES",
  representation = representation(
    conduct = "character", corePre = "character", corePost = "character",
    firmOwnerPre = "character", firmOwnerPost = "character",
    diagnostics = "list", gammaFixed = "numeric"
  ),
  prototype = prototype(conduct = "bertrand", corePre = character(),
                        corePost = character(), firmOwnerPre = character(),
                        firmOwnerPost = character(), diagnostics = list(),
                        gammaFixed = NA_real_),
  validity = function(object) {
    n <- length(object@shares)
    if (length(object@conduct) != 1L || !object@conduct %in% c("bertrand", "cournot"))
      return("'conduct' must be 'bertrand' or 'cournot'")
    if (!isTRUE(object@output)) return("CoreFringeCES currently supports output markets only")
    if (length(object@firmOwnerPre) != n || length(object@firmOwnerPost) != n)
      return("ownership vectors must have one ID per product")
    if (anyNA(object@firmOwnerPre) || anyNA(object@firmOwnerPost) ||
        any(!nzchar(object@firmOwnerPre)) || any(!nzchar(object@firmOwnerPost)))
      return("firm ownership IDs must be non-missing and non-empty")
    if (any(!object@corePre %in% unique(object@firmOwnerPre)) ||
        any(!object@corePost %in% unique(object@firmOwnerPost)))
      return("core IDs must occur in the corresponding ownership vector")
    if (length(object@gammaFixed) != 1L ||
        (!is.na(object@gammaFixed) && (!is.finite(object@gammaFixed) || object@gammaFixed <= 1)))
      return("'gammaFixed' must be greater than one or NA before calibration")
    if (length(object@priceOutside) != 1L || !is.finite(object@priceOutside) ||
        object@priceOutside <= 0) return("priceOutside must be positive for CoreFringeCES")
    TRUE
  }
)

.cf_ids <- function(x, name, n = NULL) {
  if (is.null(x)) return(character())
  if (is.matrix(x) || is.array(x) || !is.atomic(x) || is.list(x) ||
      (length(x) && anyNA(x))) stop("'", name, "' must be a non-missing ID vector")
  if (!is.null(n) && length(x) != n) stop("'", name, "' must have length ", n)
  as.character(x)
}

.cf_owner_matrix <- function(ids) {
  z <- outer(ids, ids, FUN = "==") * 1
  dimnames(z) <- list(ids, ids)
  z
}

.cf_subset <- function(subset, n) {
  if (!is.logical(subset) || length(subset) != n || anyNA(subset) || !any(subset))
    stop("subset must be a logical vector with at least one TRUE and no NA values")
  subset
}

.cf_stop_matrix <- function(x, name) {
  if (is.matrix(x) || is.array(x))
    stop("'", name, "' matrix controls are unsupported; supply a named list of scalar controls")
}

.cf_roles <- function(owner, core, subset) {
  active <- which(subset)
  cp <- active[owner[active] %in% core]
  fp <- active[!(owner[active] %in% core)]
  list(coreProducts = cp, fringeProducts = fp,
       coreFirms = intersect(core, unique(owner[active])),
       fringeFirms = setdiff(unique(owner[active]), core))
}

.cf_validate_inputs <- function(prices, shares, margins, n) {
  prices <- as.numeric(prices); shares <- as.numeric(shares); margins <- as.numeric(margins)
  if (length(prices) != n || length(shares) != n || length(margins) != n)
    stop("prices, shares, and margins must have the same length")
  if (any(!is.finite(prices)) || any(prices <= 0)) stop("prices must be finite and positive")
  if (any(!is.finite(shares)) || any(shares <= 0) || sum(shares) >= 1)
    stop("shares must be positive unconditional shares summing to less than one")
  list(prices = prices, shares = shares, margins = margins)
}

.cf_logit_shares <- function(object, prices, subset) {
  n <- length(object@shares); p <- as.numeric(prices)
  if (length(p) != n || any(!is.finite(p[subset])) || any(p[subset] <= 0))
    stop("Logit prices must be finite and positive on active products")
  beta <- as.numeric(object@slopes$alpha); d <- as.numeric(object@slopes$meanval)
  if (length(beta) != 1L || !is.finite(beta) || length(d) != n) stop("Logit slopes are unavailable")
  u <- d[subset] + beta * (p[subset] - object@priceOutside)
  m <- max(c(0, u)); eu <- exp(u - m); e0 <- exp(-m)
  s <- rep(0, n); s[subset] <- eu / (e0 + sum(eu))
  names(s) <- object@labels
  s
}

.cf_logit_h <- function(shares, owner, core, conduct, subset) {
  s <- as.numeric(shares); active <- which(subset); s0 <- 1 - sum(s[active])
  if (!is.finite(s0) || s0 <= 0) stop("Logit requires a positive outside share")
  firms <- unique(owner[active]); fs <- vapply(firms, function(f) sum(s[active][owner[active] == f]), numeric(1))
  names(fs) <- firms
  h <- rep(NA_real_, length(s)); h[active] <- 1
  cf <- intersect(core, firms)
  if (length(cf)) {
    inCore <- seq_along(owner) %in% active & owner %in% cf
    if (conduct == "bertrand") h[inCore] <- 1 / (1 - fs[owner[inCore]])
    else h[inCore] <- 1 + fs[owner[inCore]] / s0
  }
  if (any(!is.finite(h[active])) || any(h[active] <= 0)) stop("invalid core-fringe Logit action multiplier")
  list(h = h, firmShares = fs, outside = s0)
}

.cf_logit_margins <- function(shares, owner, core, conduct, subset, prices, alpha) {
  z <- .cf_logit_h(shares, owner, core, conduct, subset)
  z$h / (alpha * prices)
}

.cf_ces_demand <- function(object, prices, subset) {
  n <- length(object@shares); p <- as.numeric(prices); g <- as.numeric(object@slopes$gamma)
  A <- as.numeric(object@slopes$meanval); h <- g - 1
  if (length(p) != n || any(!is.finite(p[subset])) || any(p[subset] <= 0) ||
      length(A) != n || any(!is.finite(A[subset])) || any(A[subset] <= 0))
    stop("CES prices and mean values must be finite and positive on active products")
  z <- A[subset] * p[subset]^(-h); outz <- object@priceOutside^(-h)
  den <- outz + sum(z); r <- rep(0, n); r[subset] <- z / den
  q <- rep(0, n); q[subset] <- object@mktSize * r[subset] / p[subset]
  sden <- (1 - sum(r[subset])) / object@priceOutside + sum(r[subset] / p[subset])
  qs <- rep(0, n); qs[subset] <- (r[subset] / p[subset]) / sden
  list(prices = p, revenue = r, quantityShare = qs, quantities = q, outside = 1 - sum(r[subset]))
}

.cf_ces_margins <- function(revenue, owner, core, conduct, gamma, subset) {
  active <- which(subset); R <- vapply(unique(owner[active]), function(f)
    sum(revenue[active][owner[active] == f]), numeric(1)); names(R) <- unique(owner[active])
  r0 <- 1 - sum(revenue[active]); h <- gamma - 1; H <- 1 + h * r0
  out <- rep(NA_real_, length(owner)); out[active] <- 1 / gamma
  cf <- intersect(core, names(R))
  if (length(cf)) {
    inCore <- seq_along(owner) %in% active & owner %in% cf
    if (conduct == "bertrand") out[inCore] <-
      1 / (gamma - h * R[owner[inCore]])
    else {
      b <- h / (gamma * H)
      out[inCore] <- 1 / gamma + b * R[owner[inCore]]
    }
  }
  if (any(!is.finite(out[active])) || any(out[active] <= 0) || any(out[active] >= 1))
    stop("invalid CoreFringe CES Lerner margins")
  list(margins = out, firmRevenue = R, outside = r0, H = H)
}

.cf_logit_calibrate <- function(object, h, fixed = NULL) {
  obs <- object@diagnostics$observedMargins; p <- object@prices; w <- object@weights
  use <- is.finite(obs) & obs > 0 & is.finite(w) & w > 0 & is.finite(p) & p > 0 & is.finite(h) & h > 0
  if (is.null(fixed) && !any(use)) stop("no usable positive observed margins; supply a positive 'alpha'")
  if (!is.null(fixed)) return(list(alpha = as.numeric(fixed), usable = use, objective = NA_real_))
  x <- h / p; den <- sum(w[use] * x[use]^2); lam <- sum(w[use] * x[use] * obs[use]) / den
  if (!is.finite(lam) || lam <= 0) stop("observed margins do not identify a positive Logit price coefficient")
  list(alpha = 1 / lam, usable = use, objective = sum(w[use] * (obs[use] - x[use] * lam)^2))
}

.cf_ces_calibrate <- function(object, fixed = NULL) {
  obs <- object@diagnostics$observedMargins; w <- object@weights; p <- object@prices
  use <- is.finite(obs) & obs > 0 & is.finite(w) & w > 0 & is.finite(p) & p > 0
  if (is.null(fixed) && !any(use)) stop("no usable positive observed margins; supply a positive fixed 'gamma'")
  objective <- function(g) {
    if (!is.finite(g) || g <= 1) return(Inf)
    mm <- .cf_ces_margins(object@shares, object@firmOwnerPre, object@corePre,
                          object@conduct, g, rep(TRUE, length(p)))$margins
    if (!any(use)) return(0)
    sum(w[use] * (log(1 / obs[use]) - log(1 / mm[use]))^2)
  }
  if (!is.null(fixed)) return(list(gamma = as.numeric(fixed), usable = use, objective = objective(fixed)))
  lower <- as.numeric(object@control.slopes$lower %||% (1 + 1e-7)); upper <- as.numeric(object@control.slopes$upper %||% 100)
  if (length(lower) != 1L || !is.finite(lower) || lower <= 1) lower <- 1 + 1e-7
  if (length(upper) != 1L || !is.finite(upper) || upper <= lower) upper <- 100
  opt <- stats::optimize(objective, c(lower, upper), tol = as.numeric(object@control.slopes$reltol %||% 1e-10))
  list(gamma = opt$minimum, usable = use, objective = opt$objective,
       lower = lower, upper = upper)
}

.cf_state <- function(object, preMerger, subset = NULL) {
  n <- length(object@shares); if (is.null(subset)) subset <- if (preMerger) rep(TRUE, n) else object@subset
  subset <- .cf_subset(subset, n)
  state <- if (preMerger) {
    list(subset = subset, owner = object@firmOwnerPre, core = object@corePre,
         prices = object@pricePre, costs = object@mcPre)
  } else {
    list(subset = subset, owner = object@firmOwnerPost, core = object@corePost,
         prices = object@pricePost, costs = object@mcPost)
  }
  c(state, .cf_roles(state$owner, state$core, subset))
}

.cf_logit_root <- function(object, preMerger, subset, start = NULL) {
  st <- .cf_state(object, preMerger, subset); n <- length(object@shares); a <- abs(object@slopes$alpha)
  sigma <- if (isTRUE(object@output)) 1 else -1
  if (is.null(start)) start <- if (preMerger) object@priceStart else object@pricePost
  if (length(start) != n || any(!is.finite(start[subset])) || any(start[subset] <= 0)) start <- object@prices
  start <- pmax(as.numeric(start[subset]), .Machine$double.eps^0.25)
  target <- function(p) {
    full <- rep(NA_real_, n); full[subset] <- p
    s <- .cf_logit_shares(object, full, subset)
    z <- .cf_logit_h(s, st$owner, st$core, object@conduct, subset)
    st$costs[subset] + sigma * z$h[subset] / a
  }
  foc <- function(x) {
    p <- exp(x); tg <- try(target(p), silent = TRUE)
    if (inherits(tg, "try-error") || any(!is.finite(tg))) return(rep(1e6, length(p)))
    (p - tg) / pmax(abs(p), 1)
  }
  ctl <- object@control.equ; maxit <- as.integer(ctl$maxit %||% 500L); tol <- as.numeric(ctl$tol %||% 1e-11)
  if (!is.finite(maxit) || maxit < 20) maxit <- 500L; if (!is.finite(tol) || tol <= 0) tol <- 1e-11
  sol <- try(nleqslv::nleqslv(log(start), foc, method = "Broyden",
                              control = list(ftol = tol, xtol = tol, maxit = maxit)), silent = TRUE)
  z <- if (!inherits(sol, "try-error") && is.finite(sol$termcd) && sol$termcd <= 2) sol$x else NULL
  if (is.null(z)) {
    bb <- try(BB::BBsolve(log(start), foc, quiet = TRUE, control = ctl), silent = TRUE)
    if (!inherits(bb, "try-error") && is.finite(bb$convergence) && bb$convergence == 0) z <- bb$par
  }
  if (is.null(z) || any(!is.finite(z))) stop("CoreFringe Logit price equilibrium solver failed")
  for (i in seq_len(8L)) {
    rr <- foc(z); if (all(is.finite(rr)) && max(abs(rr)) <= 1e-12) break
    J <- try(numDeriv::jacobian(foc, z), silent = TRUE); dz <- if (!inherits(J, "try-error")) try(solve(J, -rr), silent = TRUE) else structure("bad", class = "try-error")
    if (inherits(dz, "try-error") || any(!is.finite(dz))) break
    old <- sum(rr^2); step <- 1
    repeat { zn <- z + step * dz; rn <- foc(zn); if (all(is.finite(rn)) && sum(rn^2) <= old) { z <- zn; break }; step <- step / 2; if (step < 1 / 128) break }
  }
  p <- exp(z); if (any(!is.finite(p)) || max(abs(foc(z)), na.rm = TRUE) > 2e-9)
    stop("CoreFringe Logit price equilibrium residual exceeds tolerance")
  p
}

.cf_ces_root <- function(object, preMerger, subset, start = NULL) {
  st <- .cf_state(object, preMerger, subset); n <- length(object@shares); g <- object@slopes$gamma
  if (is.null(start)) start <- if (preMerger) object@priceStart else object@pricePost
  if (length(start) != n || any(!is.finite(start[subset])) || any(start[subset] <= 0)) start <- object@prices
  start <- pmax(as.numeric(start[subset]), .Machine$double.eps^0.25)
  target <- function(p) {
    full <- rep(NA_real_, n); full[subset] <- p; d <- .cf_ces_demand(object, full, subset)
    m <- .cf_ces_margins(d$revenue, st$owner, st$core, object@conduct, g, subset)$margins[subset]
    st$costs[subset] / pmax(1 - m, 1e-12)
  }
  foc <- function(x) { p <- exp(x); tg <- try(target(p), silent = TRUE); if (inherits(tg, "try-error") || any(!is.finite(tg)) || any(tg <= 0)) rep(1e6, length(p)) else log(p / tg) }
  ctl <- object@control.equ; maxit <- as.integer(ctl$maxit %||% 500L); tol <- as.numeric(ctl$tol %||% 1e-11)
  if (!is.finite(maxit) || maxit < 20) maxit <- 500L; if (!is.finite(tol) || tol <= 0) tol <- 1e-11
  sol <- try(nleqslv::nleqslv(log(start), foc, method = "Broyden", control = list(ftol = tol, xtol = tol, maxit = maxit)), silent = TRUE)
  z <- if (!inherits(sol, "try-error") && is.finite(sol$termcd) && sol$termcd <= 2) sol$x else NULL
  if (is.null(z)) { bb <- try(BB::BBsolve(log(start), foc, quiet = TRUE, control = ctl), silent = TRUE); if (!inherits(bb, "try-error") && is.finite(bb$convergence) && bb$convergence == 0) z <- bb$par }
  if (is.null(z) || any(!is.finite(z))) stop("CoreFringe CES price equilibrium solver failed")
  for (i in seq_len(8L)) { rr <- foc(z); if (all(is.finite(rr)) && max(abs(rr)) <= 1e-12) break; J <- try(numDeriv::jacobian(foc, z), silent = TRUE); dz <- if (!inherits(J, "try-error")) try(solve(J, -rr), silent = TRUE) else structure("bad", class = "try-error"); if (inherits(dz, "try-error") || any(!is.finite(dz))) break; old <- sum(rr^2); step <- 1; repeat { zn <- z + step * dz; rn <- foc(zn); if (all(is.finite(rn)) && sum(rn^2) <= old) { z <- zn; break }; step <- step / 2; if (step < 1 / 128) break } }
  p <- exp(z); if (any(!is.finite(p)) || max(abs(foc(z)), na.rm = TRUE) > 2e-9) stop("CoreFringe CES price equilibrium residual exceeds tolerance")
  p
}

.cf_constructor <- function(kind, prices, shares, margins, ownerPre, ownerPost, corePre,
                             corePost, conduct, output, insideSize, normIndex, priceOutside,
                             mcDelta, subset, priceStart, labels, weights, alpha, gamma,
                             control.slopes, control.equ, ownerPostMissing, corePostMissing, dots) {
  n <- length(prices); z <- .cf_validate_inputs(prices, shares, margins, n)
  prices <- z$prices; shares <- z$shares; margins <- z$margins
  if (length(dots)) stop("unsupported core-fringe arguments: ", paste(names(dots), collapse = ", "))
  .cf_stop_matrix(control.slopes, "control.slopes")
  .cf_stop_matrix(control.equ, "control.equ")
  if (length(normIndex) != 1L || !is.na(normIndex)) stop("core-fringe requires normIndex=NA and an explicit outside good")
  if (!is.finite(insideSize) || length(insideSize) != 1L || insideSize <= 0) stop("insideSize must be positive")
  ownerPre <- .cf_ids(ownerPre, "ownerPre", n); ownerPost <- .cf_ids(ownerPost, "ownerPost", n)
  corePre <- .cf_ids(corePre, "corePre"); if (any(!corePre %in% unique(ownerPre))) stop("corePre contains an unknown firm ID")
  if (corePostMissing || is.null(corePost)) {
    if (ownerPostMissing || identical(ownerPost, ownerPre)) corePost <- corePre
    else stop("corePost must be supplied when ownerPost changes the ownership partition")
  } else corePost <- .cf_ids(corePost, "corePost")
  if (any(!corePost %in% unique(ownerPost))) stop("corePost contains an unknown firm ID")
  subset <- .cf_subset(subset, n); mcDelta <- as.numeric(mcDelta)
  if (length(mcDelta) != n || any(!is.finite(mcDelta))) stop("mcDelta must be a finite vector matching prices")
  priceStart <- as.numeric(priceStart); if (length(priceStart) != n || any(!is.finite(priceStart)) || any(priceStart <= 0)) stop("priceStart must be finite and positive")
  labels <- as.character(labels); if (length(labels) != n) stop("labels must have one value per product")
  weights <- as.numeric(weights); if (length(weights) != n || any(!is.finite(weights)) || any(weights < 0)) stop("weights must be finite and non-negative")
  if (kind == "logit") {
    if (length(priceOutside) != 1L || !is.finite(priceOutside) || priceOutside < 0) stop("priceOutside must be finite and non-negative")
    if (!is.null(gamma)) stop("'gamma' is only supported for demand='ces'")
    if (!is.null(alpha) && (length(alpha) != 1L || !is.finite(alpha) || alpha <= 0)) stop("'alpha' must be positive")
    parentMargins <- if (all(is.na(margins))) .cf_logit_margins(shares, ownerPre, corePre, conduct, rep(TRUE, n), prices, if (is.null(alpha)) 1 else alpha) else margins
    result <- suppressWarnings(new("CoreFringeLogit", prices = prices, shares = shares, margins = parentMargins,
      diversion = matrix(NA_real_, n, n), normIndex = NA_real_, ownerPre = .cf_owner_matrix(ownerPre), ownerPost = .cf_owner_matrix(ownerPost),
      insideSize = insideSize, output = isTRUE(output), mcDelta = mcDelta, subset = subset, weights = weights,
      priceOutside = priceOutside, priceStart = priceStart, shareInside = sum(shares), mktSize = insideSize / sum(shares), labels = labels,
      conduct = conduct, corePre = corePre, corePost = corePost, firmOwnerPre = ownerPre, firmOwnerPost = ownerPost,
      diagnostics = list(observedMargins = margins), alphaFixed = if (is.null(alpha)) NA_real_ else as.numeric(alpha)))
    if (length(control.slopes)) result@control.slopes <- control.slopes; if (length(control.equ)) result@control.equ <- control.equ
    result <- calcSlopes(result); a <- result@diagnostics$demandparam
    hp <- .cf_logit_h(shares, ownerPre, corePre, conduct, rep(TRUE, n))$h
    result@mcPre <- prices - if (isTRUE(output)) hp / a else -hp / a
    if (any(!is.finite(result@mcPre))) stop("baseline marginal costs are not finite")
    result@mcPost <- result@mcPre * (1 + mcDelta); if (any(!is.finite(result@mcPost[subset]) | result@mcPost[subset] <= 0)) stop("post marginal costs must be finite and positive on active products")
    result@pricePre <- calcPrices(result, TRUE, subset = rep(TRUE, n)); result@pricePost <- calcPrices(result, FALSE, subset = subset)
    tmp <- result; tmp@priceStart <- pmax(prices * 1.17 + .031, .Machine$double.eps^0.25); chk <- try(calcPrices(tmp, TRUE, subset = rep(TRUE, n)), silent = TRUE)
    rel <- if (inherits(chk, "try-error")) Inf else max(abs(chk / prices - 1)); if (!is.finite(rel) || rel > 2e-7) stop("CoreFringe Logit baseline reproduction exceeds tolerance")
    result@diagnostics$baselineReproduction <- if (inherits(chk, "try-error")) Inf else max(abs(chk - prices)); result@diagnostics$baselineReproductionRelative <- rel; result@diagnostics$baselineReproductionPrices <- if (inherits(chk, "try-error")) rep(NA_real_, n) else chk
    result@diagnostics$solverStatus <- list(pre = "converged", post = "converged"); result@diagnostics$counterfactual <- core_fringe_residuals(result, FALSE); result@diagnostics$baseline <- core_fringe_residuals(result, TRUE)
    result@diagnostics$maxCoreFOCResidual <- result@diagnostics$baseline$maxCoreFOCResidual
    result@diagnostics$maxFringeFOCResidual <- result@diagnostics$baseline$maxFringeFOCResidual
    result@diagnostics$maxCoreFOCResidualPre <- result@diagnostics$baseline$maxCoreFOCResidual
    result@diagnostics$maxFringeFOCResidualPre <- result@diagnostics$baseline$maxFringeFOCResidual
    result@diagnostics$residualsPre <- result@diagnostics$baseline
    result@diagnostics$residualsPost <- result@diagnostics$counterfactual
    result@diagnostics$activeRoles <- result@diagnostics$baseline$activeRoles
    result@diagnostics$rolesPre <- result@diagnostics$baseline$activeRoles
    result@diagnostics$rolesPost <- result@diagnostics$counterfactual$activeRoles
    result@diagnostics$maxCoreFOCResiduals <- c(pre = result@diagnostics$baseline$maxCoreFOCResidual,
                                                post = result@diagnostics$counterfactual$maxCoreFOCResidual)
    result@diagnostics$maxFringeFOCResiduals <- c(pre = result@diagnostics$baseline$maxFringeFOCResidual,
                                                  post = result@diagnostics$counterfactual$maxFringeFOCResidual)
    return(result)
  }
  if (!isTRUE(output)) stop("CoreFringeCES currently supports output markets only")
  if (!is.null(alpha)) stop("'alpha' is only supported for demand='logit'")
  if (length(priceOutside) != 1L || !is.finite(priceOutside) || priceOutside <= 0) stop("CoreFringe CES requires a positive priceOutside")
  if (is.null(gamma) && all(is.na(margins))) stop("all margins are missing; supply a positive fixed 'gamma'")
  if (!is.null(gamma) && (length(gamma) != 1L || !is.finite(gamma) || gamma <= 1)) stop("'gamma' must be greater than one")
  result <- suppressWarnings(new("CoreFringeCES", prices = prices, shares = shares, margins = if (all(is.na(margins))) rep(.5, n) else margins,
    diversion = matrix(NA_real_, n, n), normIndex = NA_real_, ownerPre = .cf_owner_matrix(ownerPre), ownerPost = .cf_owner_matrix(ownerPost), insideSize = insideSize,
    output = TRUE, mcDelta = mcDelta, subset = subset, weights = weights, priceOutside = priceOutside, priceStart = priceStart,
    shareInside = sum(shares), mktSize = insideSize / sum(shares), labels = labels, conduct = conduct, corePre = corePre, corePost = corePost,
    firmOwnerPre = ownerPre, firmOwnerPost = ownerPost, diagnostics = list(observedMargins = margins), gammaFixed = if (is.null(gamma)) NA_real_ else as.numeric(gamma)))
  if (length(control.slopes)) result@control.slopes <- control.slopes; if (length(control.equ)) result@control.equ <- control.equ
  result <- calcSlopes(result); g <- result@diagnostics$demandparam; mm <- .cf_ces_margins(shares, ownerPre, corePre, conduct, g, rep(TRUE, n))$margins
  result@mcPre <- prices * (1 - mm); if (any(!is.finite(result@mcPre) | result@mcPre <= 0)) stop("baseline CES marginal costs must be finite and positive")
  result@mcPost <- result@mcPre * (1 + mcDelta); if (any(!is.finite(result@mcPost[subset]) | result@mcPost[subset] <= 0)) stop("post marginal costs must be finite and positive on active products")
  result@pricePre <- calcPrices(result, TRUE, subset = rep(TRUE, n)); result@pricePost <- calcPrices(result, FALSE, subset = subset)
  tmp <- result; tmp@priceStart <- pmax(prices * 1.17 + .031, .Machine$double.eps^0.25); chk <- try(calcPrices(tmp, TRUE, subset = rep(TRUE, n)), silent = TRUE)
  rel <- if (inherits(chk, "try-error")) Inf else max(abs(chk / prices - 1)); if (!is.finite(rel) || rel > 2e-7) stop("CoreFringe CES baseline reproduction exceeds tolerance")
  result@diagnostics$baselineReproduction <- if (inherits(chk, "try-error")) Inf else max(abs(chk - prices)); result@diagnostics$baselineReproductionRelative <- rel; result@diagnostics$baselineReproductionPrices <- if (inherits(chk, "try-error")) rep(NA_real_, n) else chk
  result@diagnostics$solverStatus <- list(pre = "converged", post = "converged"); result@diagnostics$counterfactual <- core_fringe_residuals(result, FALSE); result@diagnostics$baseline <- core_fringe_residuals(result, TRUE)
  result@diagnostics$maxCoreFOCResidual <- result@diagnostics$baseline$maxCoreFOCResidual
  result@diagnostics$maxFringeFOCResidual <- result@diagnostics$baseline$maxFringeFOCResidual
  result@diagnostics$maxCoreFOCResidualPre <- result@diagnostics$baseline$maxCoreFOCResidual
  result@diagnostics$maxFringeFOCResidualPre <- result@diagnostics$baseline$maxFringeFOCResidual
  result@diagnostics$residualsPre <- result@diagnostics$baseline
  result@diagnostics$residualsPost <- result@diagnostics$counterfactual
  result@diagnostics$activeRoles <- result@diagnostics$baseline$activeRoles
  result@diagnostics$rolesPre <- result@diagnostics$baseline$activeRoles
  result@diagnostics$rolesPost <- result@diagnostics$counterfactual$activeRoles
  result@diagnostics$maxCoreFOCResiduals <- c(pre = result@diagnostics$baseline$maxCoreFOCResidual,
                                              post = result@diagnostics$counterfactual$maxCoreFOCResidual)
  result@diagnostics$maxFringeFOCResiduals <- c(pre = result@diagnostics$baseline$maxFringeFOCResidual,
                                                post = result@diagnostics$counterfactual$maxFringeFOCResidual)
  result
}

#' Construct an exact deterministic core-fringe game.
#' @export
core_fringe <- function(prices, shares, margins = rep(NA_real_, length(prices)), ownerPre,
                        ownerPost = ownerPre, corePre = character(), corePost = NULL,
                        demand = c("logit", "ces"), conduct = c("bertrand", "cournot"), output = TRUE,
                        insideSize = 1, normIndex = NA, priceOutside = NULL,
                        mcDelta = rep(0, length(prices)), subset = rep(TRUE, length(prices)),
                        priceStart = prices, labels = paste0("Prod", seq_along(prices)),
                        weights = rep(1, length(prices)), alpha = NULL, gamma = NULL,
                        control.slopes = list(), control.equ = list(), ...) {
  demand <- match.arg(demand); conduct <- match.arg(conduct); ownerPostMissing <- missing(ownerPost); corePostMissing <- missing(corePost)
  if (!is.logical(output) || length(output) != 1L || is.na(output)) stop("'output' must be one logical value")
  if (is.null(priceOutside)) priceOutside <- if (demand == "ces") 1 else 0
  .cf_constructor(demand, prices, shares, margins, ownerPre, ownerPost, corePre, corePost, conduct, output,
                  insideSize, normIndex, priceOutside, mcDelta, subset, priceStart, labels, weights, alpha, gamma,
                  control.slopes, control.equ, ownerPostMissing, corePostMissing, list(...))
}

setMethod("calcSlopes", "CoreFringeLogit", function(object, ...) {
  h <- .cf_logit_h(object@shares, object@firmOwnerPre, object@corePre, object@conduct, rep(TRUE, length(object@shares)))$h
  fit <- .cf_logit_calibrate(object, h, if (is.finite(object@alphaFixed)) object@alphaFixed else NULL); a <- fit$alpha; beta <- if (isTRUE(object@output)) -a else a
  object@slopes <- list(alpha = beta, meanval = log(object@shares / (1 - sum(object@shares))) - beta * (object@prices - object@priceOutside)); names(object@slopes$meanval) <- object@labels
  object@mktSize <- object@insideSize / sum(object@shares); object@diagnostics <- modifyList(object@diagnostics, list(demandparam = a, alpha = a, beta = beta, observedMargins = object@diagnostics$observedMargins, impliedMargins = h / (a * object@prices), residuals = object@diagnostics$observedMargins - h / (a * object@prices), usable = fit$usable, calibration = fit, fixedAlpha = is.finite(object@alphaFixed))); object
})

setMethod("calcSlopes", "CoreFringeCES", function(object, ...) {
  fit <- .cf_ces_calibrate(object, if (is.finite(object@gammaFixed)) object@gammaFixed else NULL); g <- fit$gamma; r0 <- 1 - sum(object@shares); h <- g - 1
  A <- (object@shares / r0) * (object@prices / object@priceOutside)^h; names(A) <- object@labels
  object@slopes <- list(alpha = 1 / sum(object@shares) - 1, gamma = g, meanval = A); object@mktSize <- object@insideSize / sum(object@shares)
  mm <- .cf_ces_margins(object@shares, object@firmOwnerPre, object@corePre, object@conduct, g, rep(TRUE, length(object@shares)))$margins
  object@diagnostics <- modifyList(object@diagnostics, list(demandparam = g, gamma = g, observedMargins = object@diagnostics$observedMargins, impliedMargins = mm, residuals = object@diagnostics$observedMargins - mm, usable = fit$usable, calibration = fit, fixedGamma = is.finite(object@gammaFixed))); object
})

setMethod("calcMC", "CoreFringeLogit", function(object, preMerger = TRUE) { z <- if (preMerger) object@mcPre else object@mcPost; if (!length(z)) stop("CoreFringe marginal costs are not initialized"); names(z) <- object@labels; z })
setMethod("calcMC", "CoreFringeCES", function(object, preMerger = TRUE) { z <- if (preMerger) object@mcPre else object@mcPost; if (!length(z)) stop("CoreFringe marginal costs are not initialized"); names(z) <- object@labels; z })

setMethod("calcMargins", "CoreFringeLogit", function(object, preMerger = TRUE, level = FALSE) {
  st <- .cf_state(object, preMerger); d <- .cf_logit_shares(object, st$prices, st$subset); a <- abs(object@slopes$alpha); out <- .cf_logit_h(d, st$owner, st$core, object@conduct, st$subset)$h / (a * st$prices); out[!st$subset] <- NA_real_; if (level) out <- out * st$prices; names(out) <- object@labels; out
})
setMethod("calcMargins", "CoreFringeCES", function(object, preMerger = TRUE, level = FALSE) {
  st <- .cf_state(object, preMerger); d <- .cf_ces_demand(object, st$prices, st$subset); out <- .cf_ces_margins(d$revenue, st$owner, st$core, object@conduct, object@slopes$gamma, st$subset)$margins; out[!st$subset] <- NA_real_; if (level) out <- out * st$prices; names(out) <- object@labels; out
})

setMethod("calcPrices", "CoreFringeLogit", function(object, preMerger = TRUE, isMax = FALSE, subset, ...) {
  if (isTRUE(isMax)) stop("isMax = TRUE is not implemented for CoreFringeLogit")
  if (missing(subset)) subset <- if (preMerger) rep(TRUE, length(object@shares)) else object@subset; subset <- .cf_subset(subset, length(object@shares)); p <- .cf_logit_root(object, preMerger, subset); out <- rep(NA_real_, length(object@shares)); out[subset] <- p; if (preMerger) out[!subset] <- object@prices[!subset]; names(out) <- object@labels; out
})
setMethod("calcPrices", "CoreFringeCES", function(object, preMerger = TRUE, isMax = FALSE, subset, ...) {
  if (isTRUE(isMax)) stop("isMax = TRUE is not implemented for CoreFringeCES")
  if (missing(subset)) subset <- if (preMerger) rep(TRUE, length(object@shares)) else object@subset; subset <- .cf_subset(subset, length(object@shares)); p <- .cf_ces_root(object, preMerger, subset); out <- rep(NA_real_, length(object@shares)); out[subset] <- p; if (preMerger) out[!subset] <- object@prices[!subset]; names(out) <- object@labels; out
})

# Simultaneous role FOC diagnostics.  The raw Logit derivatives are retained
# for Bertrand/Cournot, while CES reports the equivalent Lerner FOC wedge.
core_fringe_residuals <- function(object, preMerger = TRUE, prices = NULL) {
  if (!methods::is(object, "CoreFringeLogit") && !methods::is(object, "CoreFringeCES")) stop("object must be a CoreFringeLogit or CoreFringeCES")
  st <- .cf_state(object, preMerger); n <- length(object@shares); if (!is.null(prices)) { p <- as.numeric(prices); if (length(p) == n) p <- p[st$subset]; if (length(p) != sum(st$subset) || any(!is.finite(p)) || any(p <= 0)) stop("prices must be positive and have full or active-product length"); full <- st$prices; full[st$subset] <- p } else full <- st$prices
  if (methods::is(object, "CoreFringeLogit")) {
    s <- .cf_logit_shares(object, full, st$subset); z <- .cf_logit_h(s, st$owner, st$core, object@conduct, st$subset); a <- abs(object@slopes$alpha); sig <- if (isTRUE(object@output)) 1 else -1; mu <- full - st$costs; raw <- rep(NA_real_, n)
    if (object@conduct == "bertrand") for (f in unique(st$owner[st$subset])) {
      ix <- which(st$subset & st$owner == f)
      if (f %in% st$core) {
        H <- sum(mu[ix] * s[ix])
        raw[ix] <- sig * object@mktSize * s[ix] *
          (1 + (if (isTRUE(object@output)) -a else a) * (mu[ix] - H))
      } else {
        ## Fringe products are atomistic MonCom firms, including when an ID
        ## happens to label more than one such product.
        raw[ix] <- sig * (mu[ix] - sig * z$h[ix] / a)
      }
    } else raw[st$subset] <- sig * (mu[st$subset] - sig * z$h[st$subset] / a)
  } else {
    d <- .cf_ces_demand(object, full, st$subset); z <- .cf_ces_margins(d$revenue, st$owner, st$core, object@conduct, object@slopes$gamma, st$subset); raw <- full - st$costs - full * z$margins
  }
  core <- raw[st$coreProducts]; fringe <- raw[st$fringeProducts]; all <- raw[which(st$subset)]
  names(core) <- object@labels[st$coreProducts]; names(fringe) <- object@labels[st$fringeProducts]
  names(all) <- object@labels[which(st$subset)]; scale <- pmax(abs(full[st$subset]), .Machine$double.xmin); nr <- all / scale
  list(core = core, fringe = fringe, all = all,
       maxCore = if (length(core)) max(abs(core), na.rm = TRUE) else 0,
       maxFringe = if (length(fringe)) max(abs(fringe), na.rm = TRUE) else 0,
       max = if (length(all)) max(abs(all), na.rm = TRUE) else 0,
       maxCoreFOCResidual = if (length(core)) max(abs(core), na.rm = TRUE) else 0,
       maxFringeFOCResidual = if (length(fringe)) max(abs(fringe), na.rm = TRUE) else 0,
       normalizedAll = nr, maxNormalized = if (length(nr)) max(abs(nr), na.rm = TRUE) else 0,
       coreProducts = st$coreProducts, fringeProducts = st$fringeProducts,
       activeRoles = .cf_roles(st$owner, st$core, st$subset), status = "evaluated")
}

core_fringe_simulate <- function(object, ownerPost = object@firmOwnerPost, corePost = NULL,
                                  mcDelta = object@mcDelta, subset = object@subset, ...) {
  if (!methods::is(object, "CoreFringeLogit") && !methods::is(object, "CoreFringeCES")) stop("object must be a CoreFringeLogit or CoreFringeCES")
  if (length(list(...))) stop("unsupported core-fringe simulation arguments: ", paste(names(list(...)), collapse = ", "))
  n <- length(object@shares); ownerSame <- identical(as.character(ownerPost), object@firmOwnerPost); ownerPost <- .cf_ids(ownerPost, "ownerPost", n)
  if (is.null(corePost)) { if (!ownerSame) stop("corePost must be supplied when ownerPost changes the ownership partition"); corePost <- object@corePost } else corePost <- .cf_ids(corePost, "corePost")
  if (any(!corePost %in% unique(ownerPost))) stop("corePost contains an unknown firm ID")
  subset <- .cf_subset(subset, n); mcDelta <- as.numeric(mcDelta); if (length(mcDelta) != n || any(!is.finite(mcDelta))) stop("mcDelta must be a finite vector matching prices")
  out <- object; out@firmOwnerPost <- ownerPost; out@corePost <- corePost; out@ownerPost <- .cf_owner_matrix(ownerPost); out@subset <- subset; out@mcDelta <- mcDelta; out@mcPost <- object@mcPre * (1 + mcDelta)
  if (any(!is.finite(out@mcPost[subset]) | out@mcPost[subset] <= 0)) stop("post marginal costs must be finite and positive on active products")
  out@pricePost <- calcPrices(out, FALSE, subset = subset); out@diagnostics$ownerPost <- ownerPost; out@diagnostics$corePost <- corePost; out@diagnostics$mcDelta <- mcDelta; out@diagnostics$subset <- subset; out@diagnostics$solverStatus$post <- "converged"; out@diagnostics$counterfactual <- core_fringe_residuals(out, FALSE); out@diagnostics$residualsPost <- out@diagnostics$counterfactual; out@diagnostics$impliedMarginsPost <- calcMargins(out, FALSE); out@diagnostics$rolesPost <- out@diagnostics$counterfactual$activeRoles; out@diagnostics$activeRoles <- out@diagnostics$rolesPost; out@diagnostics$maxCoreFOCResidualPost <- out@diagnostics$counterfactual$maxCoreFOCResidual; out@diagnostics$maxFringeFOCResidualPost <- out@diagnostics$counterfactual$maxFringeFOCResidual; out@diagnostics$maxCoreFOCResiduals <- c(pre = out@diagnostics$maxCoreFOCResidualPre, post = out@diagnostics$maxCoreFOCResidualPost); out@diagnostics$maxFringeFOCResiduals <- c(pre = out@diagnostics$maxFringeFOCResidualPre, post = out@diagnostics$maxFringeFOCResidualPost); out
}
