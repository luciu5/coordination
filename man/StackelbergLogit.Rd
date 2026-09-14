\name{StackelbergLogit}
\alias{StackelbergLogit-class}
\alias{StackelbergCES-class}
\alias{stackelberg}
\alias{stackelberg_followers}
\alias{stackelberg_response}
\alias{stackelberg_residuals}
\alias{stackelberg_simulate}
\alias{calcSlopes,StackelbergLogit-method}
\alias{calcMargins,StackelbergLogit-method}
\alias{calcMC,StackelbergLogit-method}
\alias{calcPrices,StackelbergLogit-method}
\alias{calcSlopes,StackelbergCES-method}
\alias{calcMargins,StackelbergCES-method}
\alias{calcMC,StackelbergCES-method}
\alias{calcPrices,StackelbergCES-method}
\title{Firm-level Stackelberg Logit and CES games}
\description{
  Constructs and analyzes a noncooperative firm-level Stackelberg game with
  Logit or CES demand.  \code{StackelbergLogit} and \code{StackelbergCES}
  extend \code{antitrust}'s corresponding demand classes.  This API is separate from the legacy linear/log-linear
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
stackelberg_followers(object, leaderActions, preMerger = TRUE, start = NULL,
  subset = NULL)
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
  \item{demand}{Either \code{"logit"} or output-market \code{"ces"}.}
  \item{output}{Logical output-market indicator. CES requires \code{TRUE};
    Logit retains its output/input sign convention.}
  \item{insideSize}{Observed inside quantity for Logit and inside expenditure
    (nominal market size) for CES.}
  \item{normIndex}{Must be \code{NA}; this API uses an explicit outside good.}
  \item{priceOutside}{Fixed outside-good price; CES defaults to 1 and requires
    a strictly positive value when supplied.}
  \item{mcDelta}{Proportional marginal-cost/value changes for the post state.}
  \item{subset}{Products active in the post state.}
  \item{priceStart}{Positive starting prices for equilibrium solving.}
  \item{labels}{Product labels.}
  \item{alpha}{Optional positive absolute Logit price coefficient.}
  \item{gamma}{Optional CES elasticity, strictly greater than one. If omitted,
    it is calibrated from all usable positive observed margins.}
  \item{weights}{Nonnegative product weights used by WLS calibration.}
  \item{control.slopes, control.equ}{Optional named solver-control lists.
    Set \code{control.equ$implicitCheck = FALSE} to skip the optional nested
    implicit equilibrium diagnostic.}
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
  \code{stackelberg} returns a \code{StackelbergLogit} or \code{StackelbergCES}
  object.  The follower,
  response, and residual helpers return lists or matrices containing full
  choices, prices, quantities, responses, and diagnostics.
}
\seealso{\link[antitrust]{Logit-class}, \link[antitrust]{stackelberg}}
