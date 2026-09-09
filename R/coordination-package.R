#' coordination: Coordinated Effects Models for Antitrust Analysis
#'
#' Coordinated-effects models for merger analysis built on the structural
#' demand and cost calibration infrastructure provided by the
#' \pkg{antitrust} package.
#'
#' @importClassesFrom antitrust Logit Bertrand
#' @importMethodsFrom antitrust calcPrices calcMargins calcSlopes calcMC
#'   calcProducerSurplus calcShares calcQuantities ownerToMatrix ownerToVec
#' @importFrom methods setClass setGeneric setMethod new prototype validObject
#'   callNextMethod selectMethod
#' @importFrom stats optim
#' @importFrom BB BBsolve
#' @importFrom nleqslv nleqslv
#' @importFrom numDeriv genD
#' @keywords internal
"_PACKAGE"
