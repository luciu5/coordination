## Explicit rate-domain support for input Logit Cournot games.  This file is
## intentionally independent of the S4 classes: the two game files consume
## the same stable firm-level quantity construction.

.coordination_price_domain <- function(price_domain) {
  match.arg(price_domain, c("positive", "real"))
}

.coordination_real_logit <- function(price_domain, output, conduct) {
  isTRUE(identical(price_domain, "real") &&
    !isTRUE(output) && identical(conduct, "cournot"))
}

.coordination_validate_price_domain <- function(price_domain, demand, output,
                                                conduct) {
  price_domain <- .coordination_price_domain(price_domain)
  if (identical(price_domain, "real") &&
      (!identical(demand, "logit") || isTRUE(output) ||
       !identical(conduct, "cournot"))) {
    stop("price_domain = 'real' supports only input Logit Cournot games.",
         call. = FALSE)
  }
  price_domain
}

.coordination_set_price_domain <- function(control.equ, price_domain) {
  ctl <- control.equ
  if (is.null(ctl)) ctl <- list()
  if (!is.list(ctl)) stop("'control.equ' must be a named list")
  ctl$price_domain <- price_domain
  ctl
}

.coordination_object_price_domain <- function(object) {
  ctl <- try(object@control.equ, silent = TRUE)
  if (inherits(ctl, "try-error") || !is.list(ctl) ||
      !identical(ctl$price_domain, "real")) {
    return("positive")
  }
  "real"
}

.coordination_real_object <- function(object) {
  identical(.coordination_object_price_domain(object), "real") &&
    isTRUE(!object@output) && identical(object@conduct, "cournot")
}

.coordination_solver_control <- function(control.equ) {
  if (is.null(control.equ) || !is.list(control.equ)) return(list())
  control.equ[setdiff(names(control.equ), "price_domain")]
}

.coordination_logsumexp <- function(x) {
  if (!length(x) || any(!is.finite(x))) stop("log-sum-exp inputs must be finite")
  m <- max(x)
  m + log(sum(exp(x - m)))
}

## Return W(exp(logA)) without constructing exp(logA).  The solved variable
## satisfies log(x) + x = logA and is always the positive real branch.
.coordination_log_w_exp <- function(logA) {
  if (length(logA) != 1L || !is.finite(logA)) {
    stop("Lambert-W log input must be one finite value")
  }
  if (logA < log(.Machine$double.xmin) + 1) {
    stop("Lambert-W scalar underflowed below machine precision")
  }
  x <- if (logA <= 1) exp(logA) else {
    max(logA - log(max(logA, 1)), .Machine$double.eps)
  }
  for (iter in seq_len(100L)) {
    if (!is.finite(x) || x <= 0) x <- .Machine$double.eps
    step <- (log(x) + x - logA) / (1 / x + 1)
    next_x <- x - step
    if (!is.finite(next_x) || next_x <= 0) next_x <- x / 2
    if (abs(next_x - x) <= 1e-13 * (1 + abs(x))) {
      x <- next_x
      break
    }
    x <- next_x
  }
  if (!is.finite(x) || x <= 0 ||
      abs(log(x) + x - logA) > 2e-12 * (1 + abs(logA))) {
    stop("Lambert-W scalar root failed")
  }
  x
}

.coordination_real_quantity_state <- function(object, costs, owner,
                                               strategic, subset,
                                               game = c("stackelberg",
                                                        "core_fringe")) {
  game <- match.arg(game)
  n <- length(object@shares)
  active <- which(subset)
  beta <- as.numeric(object@slopes$alpha)
  d <- as.numeric(object@slopes$meanval)
  if (length(beta) != 1L || !is.finite(beta) || beta <= 0 ||
      length(d) != n || any(!is.finite(d[active])) ||
      any(!is.finite(costs[active]))) {
    stop("real input Logit requires finite positive beta and primitives")
  }
  groups <- split(active, as.character(owner[active]), drop = TRUE)
  firms <- names(groups)
  logA <- rep(NA_real_, n)
  logA[active] <- d[active] + beta *
    (as.numeric(costs[active]) - as.numeric(object@priceOutside))
  logAfirm <- vapply(groups, function(ix) {
    .coordination_logsumexp(logA[ix])
  }, numeric(1))
  names(logAfirm) <- firms
  strategic <- intersect(as.character(strategic), firms)
  followers <- setdiff(firms, strategic)
  xfirm <- setNames(numeric(length(firms)), firms)
  if (identical(game, "core_fringe")) {
    if (length(followers)) {
      xfirm[followers] <- exp(logAfirm[followers] - 1)
    }
    if (length(strategic)) {
      xfirm[strategic] <- vapply(strategic, function(f) {
        .coordination_log_w_exp(logAfirm[[f]] - 1)
      }, numeric(1))
    }
    D <- 1
  } else if (length(followers)) {
    xfirm[followers] <- vapply(followers, function(f) {
      .coordination_log_w_exp(logAfirm[[f]] - 1)
    }, numeric(1))
  } else {
    D <- 1
  }
  if (identical(game, "stackelberg")) {
    D <- 1 + sum(xfirm[followers])
    if (!is.finite(D) || D <= 0) {
      stop("real input Logit follower scalar is invalid")
    }
    if (length(strategic)) {
      xfirm[strategic] <- vapply(strategic, function(f) {
        D * .coordination_log_w_exp(logAfirm[[f]] - 1 - log(D))
      }, numeric(1))
    }
  }
  if (any(!is.finite(xfirm)) || any(xfirm <= 0)) {
    stop("real input Logit firm scalar is invalid")
  }
  logxfirm <- log(xfirm)
  logden <- .coordination_logsumexp(c(0, logxfirm))
  s0 <- exp(-logden)
  shares <- rep(NA_real_, n)
  shares[active] <- exp(
    logxfirm[owner[active]] + logA[active] -
      logAfirm[owner[active]] - logden
  )
  if (any(!is.finite(shares[active])) || any(shares[active] <= 0) ||
      !is.finite(s0) || s0 <= 0) {
    stop("real input Logit shares are outside the finite interior domain")
  }
  hfirm <- setNames(rep(1, length(firms)), firms)
  if (identical(game, "stackelberg") && length(followers)) {
    hfirm[followers] <- 1 + xfirm[followers]
  }
  if (length(strategic)) {
    hfirm[strategic] <- if (identical(game, "core_fringe")) {
      1 + xfirm[strategic]
    } else {
      1 + xfirm[strategic] / D
    }
  }
  h <- rep(NA_real_, n)
  h[active] <- hfirm[owner[active]]
  prices <- rep(NA_real_, n)
  prices[active] <- as.numeric(costs[active]) - h[active] / beta
  if (any(!is.finite(prices[active]))) stop("real input Logit rates are not finite")
  list(prices = prices, shares = shares, outside = s0, x = xfirm,
       logA = logA, logAfirm = logAfirm, h = h, beta = beta, D = D,
       firms = firms, strategic = strategic, followers = followers,
       logden = logden)
}

.coordination_domain_obstruction <- function(rate, labels, preMerger,
                                             original_error, active_index = NULL) {
  rate <- as.numeric(rate)
  original <- attr(original_error, "condition")
  original_message <- if (!is.null(original)) {
    conditionMessage(original)
  } else {
    as.character(original_error)
  }
  i <- which.min(rate)
  index <- if (!is.null(active_index) && length(active_index) >= i) {
    active_index[[i]]
  } else {
    i
  }
  label <- if (length(labels) >= index) as.character(labels[[index]]) else as.character(index)
  msg <- paste0(
    original_message,
    "; positive price domain obstructed by a unique real Cournot root at ",
    "rate ", format(rate[[i]], digits = 8), " for product '", label, "'"
  )
  cnd <- structure(
    list(message = msg, call = NULL,
         category = "positive_domain_obstruction",
         domain = "positive", minimum_rate = rate[[i]],
         minimum_product = label, preMerger = isTRUE(preMerger)),
    class = c("coordination_domain_obstruction", "error", "condition")
  )
  stop(cnd)
}

.coordination_rethrow_or_classify <- function(error, real_root, object,
                                               preMerger, active_index = NULL) {
  if (inherits(real_root, "try-error")) {
    original <- attr(error, "condition")
    if (!is.null(original)) stop(original)
    stop(as.character(error), call. = FALSE)
  }
  finite <- is.finite(real_root)
  finite_root <- real_root[finite]
  finite_index <- if (is.null(active_index)) NULL else active_index[finite]
  if (length(finite_root) && any(finite_root <= 0) &&
      !.coordination_real_object(object)) {
    .coordination_domain_obstruction(
      finite_root, object@labels, preMerger, error, finite_index
    )
  }
  original <- attr(error, "condition")
  if (!is.null(original)) stop(original)
  stop(as.character(error), call. = FALSE)
}

.coordination_stop_try_error <- function(error) {
  original <- attr(error, "condition")
  if (!is.null(original)) stop(original)
  stop(as.character(error), call. = FALSE)
}
