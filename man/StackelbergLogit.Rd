\name{StackelbergLogit}
\alias{StackelbergLogit-class}
\alias{stackelberg}
\alias{stackelberg_followers}
\alias{stackelberg_response}
\alias{stackelberg_residuals}
\alias{stackelberg_simulate}
\alias{calcSlopes,StackelbergLogit-method}
\alias{calcMargins,StackelbergLogit-method}
\alias{calcMC,StackelbergLogit-method}
\alias{calcPrices,StackelbergLogit-method}
\title{Firm-level Stackelberg Logit games}
\description{
  Constructs and analyzes a noncooperative firm-level Stackelberg game with
  flat Logit demand.  The class extends \code{antitrust}'s \code{Logit}
  class.  This API is separate from the legacy linear/log-linear
  \code{antitrust::stackelberg} constructor and from coordination's
  PriceLeadership model.
}
\usage{
stackelberg(prices, shares, margins = rep(NA_real_, length(prices)), ownerPre,
  ownerPost = ownerPre, leadersPre, leadersPost = NULL,
  demand = c("logit", "ces"), conduct = c("bertrand", "cournot"),
  output = TRUE, insideSize = 1, normIndex = NA, priceOutside = 0,
  mcDelta = rep(0, length(prices)), subset = rep(TRUE, length(prices)),
  priceStart = prices, labels = paste0("Prod", seq_along(prices)),
  weights = rep(1, length(prices)), alpha = NULL, gamma = NULL,
  control.slopes = list(), control.equ = list(), ...)
stackelberg_followers(object, leaderActions, preMerger = TRUE, start = NULL)
stackelberg_response(object, preMerger = TRUE,
  method = c("analytic", "implicit"))
stackelberg_residuals(object, preMerger = TRUE, prices = NULL,
  quantities = NULL)
stackelberg_simulate(object,
  ownerPost = object@firmOwnerPost, leadersPost = NULL,
  mcDelta = object@mcDelta, subset = object@subset, ...)
}
\arguments{
  \item{prices}{Observed product prices, or an active/full price vector for
    residual diagnostics.}
  \item{shares, margins}{Observed unconditional
    quantity shares, and proportional observed margins.}
  \item{ownerPre, ownerPost}{One firm ID per product before and after the
    intervention.  Partial-control matrices are rejected.}
  \item{leadersPre, leadersPost}{Firm IDs that choose in the first stage.
    Changed post ownership requires an explicit \code{leadersPost}.}
  \item{conduct}{Either \code{"bertrand"} or \code{"cournot"}.}
  \item{demand}{Currently only \code{"logit"}; CES is reserved for a later
    extension.}
  \item{output}{Whether the market is an output market; input margins use the
    signed inverse demand convention internally.}
  \item{insideSize}{Observed inside quantity used to scale product quantities.}
  \item{normIndex}{Must be \code{NA}; this API uses a positive outside share.}
  \item{priceOutside}{Fixed outside-good price.}
  \item{mcDelta}{Proportional marginal-cost/value changes for the post state.}
  \item{subset}{Products active in the post state.}
  \item{priceStart}{Positive starting prices for equilibrium solving.}
  \item{labels}{Product labels.}
  \item{alpha}{Optional positive absolute Logit price coefficient.}
  \item{gamma}{Reserved CES elasticity argument; unsupported for Logit.}
  \item{weights}{Nonnegative product weights used by WLS calibration.}
  \item{control.slopes, control.equ}{Optional named solver-control lists.}
  \item{object}{A \code{StackelbergLogit} object.}
  \item{leaderActions}{Prices for Bertrand or quantities for Cournot, one per
    active leader product.}
  \item{method}{Analytic or implicit-Jacobian follower response route.}
  \item{preMerger}{Whether the pre-intervention state is selected.}
  \item{start}{Optional follower-action starting values.}
  \item{quantities}{Optional active or full quantity vector for Cournot
    residual diagnostics.}
  \item{...}{Reserved solver and counterfactual arguments.}
}
\value{
  \code{stackelberg} returns a \code{StackelbergLogit} object.  The follower,
  response, and residual helpers return lists or matrices containing full
  choices, prices, quantities, responses, and diagnostics.
}
\seealso{\link[antitrust]{Logit-class}, \link[antitrust]{stackelberg}}
