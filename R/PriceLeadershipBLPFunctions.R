## Price Leadership with BLP (random-coefficients) demand.
##
## PriceLeadershipBLP extends antitrust's LogitBLP. The BLP mean-utility
## recovery (calcMeanval) and integration-rule selection (calcBLPintegration)
## are provided by antitrust as public API; this file adds only the
## coordinated-effects layer (supermarkup / timing / IC calibration) on top,
## reusing the PriceLeadership methods via inheritance/delegation.


#'@rdname PriceLeadership-Functions
#'@export
ple.blp <- function(
  prices,
  shares,
  margins = NULL,
  ownerPre,
  ownerPost = ownerPre,
  coalitionPre,
  coalitionPost = coalitionPre,
  timingParam = NA_real_,
  normIndex = ifelse(isTRUE(all.equal(sum(shares),1,check.names=FALSE,tolerance=1e-3)),1,NA),
  insideSize = NA_real_,
  output = TRUE,
  mcDelta = rep(0,length(prices)),
  subset = rep(TRUE,length(prices)),
  priceOutside = 0,
  priceStart = prices,
  isMax = FALSE,
  integration = c("auto", "gauss-hermite", "monte-carlo", "provided"),
  nNodes = NULL,
  nDraws = NULL,
  consDraws = NULL,
  integrationWeights = NULL,
  slopes,
  control.slopes,
  control.equ,
  labels = paste("Prod",1:length(prices),sep=""),
  ...
){

  nprods <- length(prices)
  integration_missing <- missing(integration)
  integration <- match.arg(integration)

  if (!is.list(slopes)) {
    stop("'slopes' must be a list of BLP demand parameters.")
  }
  if (!missing(nDraws) && identical(integration, "auto")) {
    ## Preserve the historical meaning of nDraws in the legacy public API.
    integration <- "monte-carlo"
  }
  if (!is.null(nDraws) && !identical(integration, "monte-carlo")) {
    stop("'nDraws' is only valid with integration = 'monte-carlo'; use 'nNodes' for Gauss-Hermite.")
  }
  if (!is.null(nNodes) && !identical(integration, "gauss-hermite")) {
    stop("'nNodes' is only valid with integration = 'gauss-hermite'.")
  }
  if (!is.null(consDraws) &&
      (!is.null(slopes$consDraws) || !is.null(slopes$draws))) {
    stop("BLP integration points were supplied both in 'slopes' and 'consDraws'.")
  }
  if (!is.null(integrationWeights) &&
      (!is.null(slopes$integrationWeights) || !is.null(slopes$drawWeights))) {
    stop("BLP integration weights were supplied both in 'slopes' and 'integrationWeights'.")
  }

  if (!is.null(consDraws)) slopes$consDraws <- consDraws
  if (!is.null(integrationWeights)) slopes$integrationWeights <- integrationWeights
  if (!is.null(nNodes)) slopes$nNodes <- nNodes
  if (!is.null(nDraws)) slopes$nDraws <- nDraws
  supplied_points <- if (!is.null(slopes$draws)) slopes$draws else slopes$consDraws
  if (!is.null(slopes$nDraws) && !is.null(supplied_points) &&
      slopes$nDraws != length(supplied_points)) {
    stop("'nDraws' must equal the number of supplied BLP integration points.")
  }
  if (!is.null(slopes$nDraws) && isTRUE(integration_missing) &&
      is.null(slopes$consDraws) && is.null(slopes$draws)) {
    integration <- "monte-carlo"
  }
  slopes$integration <- integration
  integration_result <- calcBLPintegration(slopes)
  slopes$consDraws <- integration_result$draws
  slopes$drawWeights <- integration_result$weights
  slopes$integrationWeights <- integration_result$weights
  slopes$integration <- integration_result$rule
  slopes$nNodes <- if (identical(integration_result$rule, "gauss-hermite")) {
    length(integration_result$draws)
  } else {
    NULL
  }
  nDraws <- length(integration_result$draws)

  ## Check post-merger coalition
  ## Logit's parent validity requires a finite margin vector even though
  ## PriceLeadershipBLP can infer its supermarkup from observed prices.
  ## Construct with a temporary placeholder, then restore the missing-margin
  ## state before calibration.
  marginsProvided <- !is.null(margins)
  marginsForClass <- if(marginsProvided) margins else rep(1 / nprods, nprods)

  ## Create object - validity method will check coalition, slopes, set defaults, etc.
  ## Note: slopes contains pre-calibrated BLP demand parameters
  result <- new("PriceLeadershipBLP",
                prices = prices,
                shares = shares,
                margins = marginsForClass,
                normIndex = normIndex,
                ownerPre = ownerPre,
                ownerPost = ownerPost,
                coalitionPre = coalitionPre,
                coalitionPost = coalitionPost,
                timingParam = timingParam,
                insideSize = insideSize,
                output = output,
                mcDelta = mcDelta,
                subset = subset,
                priceOutside = priceOutside,
                priceStart = priceStart,
                shareInside = ifelse(isTRUE(all.equal(sum(shares),1,check.names=FALSE,tolerance=1e-3)),1,sum(shares)),
                slopes = slopes,
                nDraws = nDraws,
                labels = labels)

  if(!marginsProvided){
    result@margins <- rep(NA_real_, nprods)
  }

  if(!missing(control.slopes)){
    result@control.slopes <- control.slopes
  }
  if(!missing(control.equ)){
    result@control.equ <- control.equ
  }

  ## Convert ownership vectors to ownership matrices
  result@ownerPre  <- ownerToMatrix(result, TRUE)
  result@ownerPost <- ownerToMatrix(result, FALSE)

  ## Convert coalition vectors to coalition matrices
  result@coalitionPre  <- ownerToMatrix(result, TRUE, control = TRUE)
  result@coalitionPost <- ownerToMatrix(result, FALSE, control = TRUE)

  ## Calculate Demand Slope Coefficients, marginal costs, and supermarkups
  ## (calcSlopes now handles both pre and post-merger calibration)
  result <- calcSlopes(result)

  ## Solve Non-Linear System for Price Changes
  result@pricePre  <- calcPrices(result, preMerger = TRUE, isMax = isMax, regime = "constrained", ...)
  result@pricePost <- calcPrices(result, preMerger = FALSE, isMax = isMax, subset = subset, regime = "constrained", ...)

  return(result)
}

#' @rdname Params-Methods
#' @export
setMethod(
  f = "calcSlopes",
  signature = "PriceLeadershipBLP",
  definition = function(object){

    ## For LogitBLP with price leadership:
    ## 1. BLP demand parameters are PRE-CALIBRATED (provided in slopes list)
    ## 2. Use BLP contraction mapping to ensure meanval is consistent with shares
    ## 3. Calibrate only price leadership parameters: supermarkup, timing
    ##
    ## This differs from PriceLeadership which calibrates both demand AND leadership params

    nprods <- length(object@prices)

    # Extract coalition indices (handle both vector and matrix forms)
    if(is.matrix(object@coalitionPre)){
      coalition <- which(rowSums(object@coalitionPre) > 1)
    } else {
      coalition <- object@coalitionPre
    }

    ## Validate BLP parameters (now in validity method)
    # BLP parameter validation moved to PriceLeadershipBLP validity method

  ## Run BLP contraction mapping to get meanval (delta) via shared method
  ## This ensures delta is consistent with observed shares given random coefficients
  object <- calcMeanval(object)

  ## BLP models do not obtain marginal cost from observed margins.  Recover
  ## it from the supplied demand system and observed prices before solving
  ## the leadership equilibria.
  object@pricePre <- object@prices
  object@mcPre <- calcMC(object, preMerger = TRUE)

    ## Set market size if not already set
    if(is.na(object@mktSize) || length(object@mktSize) == 0){
      shareInside <- object@shareInside
      object@mktSize <- object@insideSize / shareInside
    }

    ## First, solve for pure Bertrand equilibrium prices.  With BLP demand,
    ## the leadership supermarkup can be inferred from observed prices when
    ## margins are not supplied; supplied margins remain available for the
    ## legacy margin-based path.
    bertrandPrices <- calcPrices(object, preMerger = TRUE, regime = "bertrand")

    marginsProvided <- any(!is.na(object@margins))
    if(marginsProvided){
      ## Temporarily set prices to Bertrand equilibrium
      object@pricePre <- bertrandPrices

      ## Calculate Bertrand margins at Bertrand prices
      bertrandMargins <- calcMargins(object, preMerger = TRUE, regime = "bertrand")

      ## Supermarkup is the difference for coalition products
      supermarkupPre <- mean((object@margins - bertrandMargins)[coalition], na.rm = TRUE)
      object@supermarkupPre <- supermarkupPre
    }

    ## Calibrate timing parameters from IC constraints at the observed level of coordination
    # Solve for coordinated Bertrand prices (coalition coordinates with m=0)
    pricesColl <- calcPrices(object, preMerger = TRUE, isMax = FALSE, regime = "coordination")

    # Calculate price leadership parameters (timing parameters and IC slack at observed coordination)
    pleParams <- calcPriceLeadershipParams(object, preMerger = TRUE,
                                           pricesColl = pricesColl,
                                           coalition = coalition)

    if(!marginsProvided){
      object@supermarkupPre <- pleParams$supermarkup
    }

    # Store timing parameters calibrated from IC constraints
    object@timingParam <- pleParams$timingParam
    object@bindingFirm <- pleParams$bindingFirm
    object@slackValues <- pleParams$slackValues

    ## Calculate marginal costs using BLP-calibrated demand (after supermarkup calibration)
    object@mcPre <- calcMC(object, preMerger = TRUE)

    ## Calculate post-merger marginal costs
    object@mcPost <- calcMC(object, preMerger = FALSE)

    ## For BLP, post-merger supermarkup using pre-merger calibrated timing parameters
    # Get post-merger coalition
    if(is.matrix(object@coalitionPost)){
      coalitionPost <- which(rowSums(object@coalitionPost) > 1)
    } else {
      coalitionPost <- object@coalitionPost
    }

    ownerVec <- ownerToVec(object, preMerger = FALSE)
    ownerMat <- object@ownerPost
    coalitionFirms <- unique(ownerVec[coalitionPost])

    if(length(coalitionPost) == 0){
      # No post-merger coalition - no supermarkup
      object@supermarkupPost <- 0
    } else {
      # Check if full collusion is sustainable post-merger using pre-merger timing parameters

      # Calculate post-merger collusive profits (unconstrained price leader profits)
      pricesCollPost <- calcPrices(object, preMerger = FALSE, isMax = FALSE, regime = "coordination")
      object@pricePost <- pricesCollPost
      profitsCollPost <- calcProducerSurplus(object, preMerger = FALSE)

      # Calculate post-merger Bertrand profits
      pricesBertrandPost <- calcPrices(object, preMerger = FALSE, isMax = FALSE, regime = "bertrand")
      object@pricePost <- pricesBertrandPost
      profitsBertrandPost <- calcProducerSurplus(object, preMerger = FALSE)

      # Calculate optimal deviation profits for each coalition firm
      # For each firm, compute profits if it deviates from coordination
      # Method: Temporarily remove the firm from the coalition and solve for new equilibrium
      profitsDeviationPost <- sapply(coalitionFirms, function(firm){
        firmProducts <- which(ownerVec == firm)
        deviatingCoalitionProducts <- intersect(firmProducts, coalitionPost)

        if(length(deviatingCoalitionProducts) == 0) return(0)

        # Store original coalition matrix to restore later
        originalCoalitionMat <- object@coalitionPost

        # Create modified coalition matrix excluding this firm
        # Use ownerToMatrix with exclude parameter for cleaner implementation
        nprods <- length(ownerVec)
        excludeVec <- rep(FALSE, nprods)
        excludeVec[firmProducts] <- TRUE
        modifiedCoalitionMat <- ownerToMatrix(object, preMerger = FALSE,
                                             control = TRUE, exclude = excludeVec)

        # Temporarily set the modified coalition matrix
        object@coalitionPost <- modifiedCoalitionMat

        # Solve for deviation equilibrium (deviating firm competes, others coordinate)
        deviationProfit <- calcProducerSurplus(object, preMerger = FALSE,
                                              regime = "coordination")

        # Restore original coalition matrix
        object@coalitionPost <- originalCoalitionMat

        sum(deviationProfit[firmProducts])
      })

      names(profitsDeviationPost) <- as.character(coalitionFirms)

      # Aggregate profits by firm using ownership matrix
      allProfitsCollPostFirm <- ownerMat %*% profitsCollPost
      profitsCollPostFirm <- allProfitsCollPostFirm[coalitionFirms]
      names(profitsCollPostFirm) <- as.character(coalitionFirms)

      allProfitsBertrandPostFirm <- ownerMat %*% profitsBertrandPost
      profitsBertrandPostFirm <- allProfitsBertrandPostFirm[coalitionFirms]
      names(profitsBertrandPostFirm) <- as.character(coalitionFirms)

      # Check IC constraints using pre-merger timing parameters
      IC_slack <- sapply(coalitionFirms, function(firm){
        firmName <- as.character(firm)
        delta_f <- object@timingParam[firmName]

        # If no timing parameter was identified, use the immediate-payoff
        # constraint.  Avoid evaluating delta/(1-delta) at delta = 1, which
        # would turn a zero payoff difference into NaN.
        if(is.na(delta_f) || delta_f >= 1){
          return(profitsCollPostFirm[firmName] - profitsDeviationPost[firmName])
        }

        # IC_f: pi_PL_f - pi_D_f + delta_f/(1-delta_f) * (pi_PL_f - pi_B_f) >= 0
        slack_f <- (profitsCollPostFirm[firmName] - profitsDeviationPost[firmName]) +
          (delta_f / (1 - delta_f)) * (profitsCollPostFirm[firmName] - profitsBertrandPostFirm[firmName])

        return(slack_f)
      })

      # If all IC constraints are satisfied (slack >= 0), full collusion is sustainable
      if(all(IC_slack >= -1e-6)){
        # Back out supermarkup from margins: collusive margins - Bertrand margins
        # Set prices to collusive equilibrium for margin calculation
        object@pricePost <- pricesCollPost
        collusiveMargins <- calcMargins(object, preMerger = FALSE, regime = "coordination")

        # Set prices to Bertrand equilibrium for margin calculation
        object@pricePost <- pricesBertrandPost
        bertrandMargins <- calcMargins(object, preMerger = FALSE, regime = "bertrand")

        object@supermarkupPost <- mean((collusiveMargins - bertrandMargins)[coalitionPost], na.rm = TRUE)
      } else {
        # IC constraints violated - find maximum sustainable supermarkup
        object@supermarkupPost <- calcSupermarkup(object, preMerger = FALSE, constrained = TRUE)
      }
    }

    return(object)
  }
)

#' @keywords internal
setMethod(
  f = "calcPriceLeadershipParams",
  signature = "PriceLeadershipBLP",
  definition = function(object, preMerger = TRUE, pricesColl, coalition) {
    selectMethod("calcPriceLeadershipParams", "PriceLeadership")(object, preMerger, pricesColl, coalition)
  }
)

#' @rdname Ownership-methods
#' @export
setMethod(
  f = "ownerToMatrix",
  signature = "PriceLeadershipBLP",
  definition = function(object, preMerger = TRUE, control = FALSE, exclude = NULL){
    # Reuse PriceLeadership implementation - logic is identical
    # Both classes have coalitionPre/Post and ownerPre/Post slots
    selectMethod("ownerToMatrix", "PriceLeadership")(object, preMerger, control, exclude)
  }
)

#' @rdname PS-methods
#' @export
setMethod(
  f = "calcProducerSurplus",
  signature = "PriceLeadershipBLP",
  definition = function(object, preMerger = TRUE, regime = c("coordination", "bertrand"), ...){
    regime <- match.arg(regime)
    # Delegate to PriceLeadership implementation
    selectMethod("calcProducerSurplus", "PriceLeadership")(
      object, preMerger = preMerger, regime = regime, ...
    )
  }
)

#' @rdname Margins-Methods
#' @export
setMethod(
  f = "calcMargins",
  signature = "PriceLeadershipBLP",
  definition = function(object, preMerger = TRUE, level = FALSE,
                        regime = c("coordination", "bertrand", "deviation", "constrained")){
    regime <- match.arg(regime)
    # Delegate to PriceLeadership implementation
    selectMethod("calcMargins", "PriceLeadership")(
      object, preMerger = preMerger, level = level, regime = regime
    )
  }
)

#' @rdname Prices-Methods
#' @export
setMethod(
  f = "calcPrices",
  signature = "PriceLeadershipBLP",
  definition = function(object, preMerger = TRUE, isMax = FALSE, subset,
                        regime = c("coordination", "bertrand", "deviation", "constrained"), ...){
    regime <- match.arg(regime)
    # Delegate to PriceLeadership implementation
    if(missing(subset)){
      selectMethod("calcPrices", "PriceLeadership")(
        object, preMerger = preMerger, isMax = isMax, regime = regime, ...
      )
    } else {
      selectMethod("calcPrices", "PriceLeadership")(
        object, preMerger = preMerger, isMax = isMax,
        subset = subset, regime = regime, ...
      )
    }
  }
)

#' @rdname calcSupermarkup
#' @export
setMethod(
  f = "calcSupermarkup",
  signature = "PriceLeadershipBLP",
  definition = function(object, preMerger = TRUE, constrained = FALSE, ...){
    # Delegate to PriceLeadership implementation
    selectMethod("calcSupermarkup", "PriceLeadership")(object, preMerger, constrained, ...)
  }
)
