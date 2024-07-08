#' @title calcFractionInputsUSDA
#' @description Calculates the factor factor shares for agricultural production from USDA'S Inputs shares.
#'
#' @return magpie object of the shares of the factor requirements in agriculture (capital, labor, materials, land).
#' @author Edna J. Molina Bacca, Debbora Leip
#' @importFrom dplyr  intersect
#' @importFrom magclass magpiesort
#' @importFrom magclass time_interpolate
#' @importFrom magclass collapseDim
#' @importFrom magclass collapseNames
#' @seealso [calcOutput()]
#' @examples
#' \dontrun{
#' a <- calcOutput("FractionInputsUSDA")
#' }
#'
calcFractionInputsUSDA <- function() {

  # value of animals is directly covered in MAgPIE
  tfpSharesRaw <- readSource("TFPUSDA")

  # assuming the same share in the middle of the decade
  tfpShares <- magpiesort(time_interpolate(tfpSharesRaw,
                                           interpolated_year = c((getYears(tfpSharesRaw, as.integer = TRUE) + 5)),
                                           extrapolation_type = "constant", integrate_interpolated_years = TRUE))

  # mappping of categories
  mapping <- data.frame("to" = c("Labor", "Land", "Materials", "Materials", "Capital", "Capital"), 
                        "from" = c("AG_Labour", "AG_Land", "Crop_Materials", "Animal_Materials", "Crop_Capital", "Animal_Capital"))

  tfpShares <- toolAggregate(tfpShares, rel = mapping, from = "from", to = "to", dim = 3)

  # vop as aggregation weight
  vopCrops <- dimSums(calcOutput("VoPcrops", aggregate = FALSE), dim = 3)
  vopLivst <- dimSums(calcOutput("VoPlivst", aggregate = FALSE), dim = 3)
  vop <- vopCrops + vopLivst

  # constant weight for missing years
  missingYears <- setdiff(getYears(tfpShares), getYears(vop))
  vop <- time_interpolate(vop, interpolated_year = missingYears, extrapolation_type = "constant",
                          integrate_interpolated_years = TRUE)

  # weight
  weight <- tfpShares
  weight[, , ] <- vop[, getYears(tfpShares), ]
  weight[tfpShares == 0] <- 0

  return(list(x = tfpShares,
              weight = weight,
              mixed_aggregation = NULL,
              unit = "fraction",
              description = "Factor shares for agricultural production from USDA TFP data"))

}
