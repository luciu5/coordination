\name{CoreFringe}
\alias{CoreFringeLogit-class}
\alias{CoreFringeCES-class}
\alias{core_fringe}
\alias{core_fringe_simulate}
\alias{core_fringe_residuals}
\alias{calcSlopes,CoreFringeLogit-method}
\alias{calcSlopes,CoreFringeCES-method}
\alias{calcMargins,CoreFringeLogit-method}
\alias{calcMargins,CoreFringeCES-method}
\alias{calcMC,CoreFringeLogit-method}
\alias{calcMC,CoreFringeCES-method}
\alias{calcPrices,CoreFringeLogit-method}
\alias{calcPrices,CoreFringeCES-method}
\title{Exact deterministic core-fringe Logit and CES games}
\description{
  Constructs a simultaneous hard-role core-fringe game.  Core firms use the
  specified multiproduct Bertrand or Cournot first-order condition and every
  fringe product uses the atomistic MonCom condition.  The two classes extend
  the corresponding \pkg{antitrust} demand classes and are separate from the
  sequential Stackelberg classes.
}
\usage{
core_fringe(prices, shares, margins = rep(NA_real_, length(prices)), ownerPre,
  ownerPost = ownerPre, corePre = character(), corePost = NULL,
  demand = c("logit", "ces"), conduct = c("bertrand", "cournot"),
  output = TRUE, insideSize = 1, normIndex = NA, priceOutside = NULL,
  mcDelta = rep(0, length(prices)), subset = rep(TRUE, length(prices)),
  priceStart = prices, labels = paste0("Prod", seq_along(prices)),
  weights = rep(1, length(prices)), alpha = NULL, gamma = NULL,
  control.slopes = list(), control.equ = list(), ...)
core_fringe_simulate(object, ownerPost = object@firmOwnerPost,
  corePost = NULL, mcDelta = object@mcDelta, subset = object@subset, ...)
core_fringe_residuals(object, preMerger = TRUE, prices = NULL)
}
\arguments{
  \item{prices}{Observed positive product prices; for residual diagnostics,
    an optional full or active price vector.}
  \item{shares}{Observed unconditional quantity shares for Logit or revenue
    shares for CES; their sum must be below one.}
  \item{margins}{Optional proportional observed margins. Missing moments are
    omitted from demand calibration.}
  \item{ownerPre,ownerPost}{Firm IDs for every product before and after the
    intervention.}
  \item{corePre,corePost}{Firm IDs in the deterministic core. A changed post
    ownership partition requires an explicit \code{corePost}; an empty vector
    is the k=0 fringe boundary.}
  \item{demand}{Either \code{"logit"} or \code{"ces"}.}
  \item{conduct}{Either \code{"bertrand"} or \code{"cournot"}.}
  \item{output}{Logical market orientation. CES currently supports output
    markets only; Logit retains the package input-market sign convention.}
  \item{insideSize}{Inside quantity for Logit or inside expenditure for CES.}
  \item{normIndex}{Must be \code{NA}; the game uses an explicit outside good.}
  \item{priceOutside}{Outside-good price. CES requires a positive value and
    defaults to one; Logit defaults to zero.}
  \item{mcDelta}{Proportional marginal-cost/value changes in the post state.}
  \item{subset}{Logical vector of products active in the post state.}
  \item{priceStart}{Positive starting prices for equilibrium solving.}
  \item{labels}{Product labels.}
  \item{weights}{Nonnegative weights for margin calibration.}
  \item{alpha}{Optional positive absolute Logit price coefficient.}
  \item{gamma}{Optional CES elasticity greater than one.}
  \item{control.slopes,control.equ}{Optional named calibration and solver
    controls.}
  \item{object}{A \code{CoreFringeLogit} or \code{CoreFringeCES} object.}
  \item{preMerger}{Whether to report the pre-intervention state.}
  \item{...}{Reserved arguments.}
}
\value{
  \code{core_fringe} returns a \code{CoreFringeLogit} or
  \code{CoreFringeCES} object. The simulation and residual helpers return an
  object or a list with equilibrium and role-specific diagnostics.
}
\seealso{\link[antitrust]{Logit-class}, \link[antitrust]{CES-class}}
