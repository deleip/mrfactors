#' @title calcFractionInputsUSDA
#' @description Calculates the factor factor shares for crop production from USDA'S Inputs shares.
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
calcFractionInputsUSDA <- function(products = "kcr", keepConstantExtrpolation = TRUE, interpolate = TRUE) {
  # only looking at crop cost shares
  tfpShares <- readSource("TFPUSDA")

  # assuming the same share in the middle of the decade
  if (isTRUE(interpolate)) {
    tfpShares <- magpiesort(time_interpolate(tfpShares,
                                            interpolated_year = c((getYears(tfpShares, as.integer = TRUE) + 5)),
                                            extrapolation_type = "constant", integrate_interpolated_years = TRUE))
  }

  # remove constant extrapolation
  if (isFALSE(keepConstantExtrpolation)) {
    mapping <- toolGetMapping("caseStudiesUSDATFP.csv", where = "mrfactors", type = "regional")

    .cleanCaseStudy <- function(cs) {
      countries <- mapping[mapping$CaseStudy == cs, "ISO"]
      tmp <- tfpShares[countries, , ]
      tmpArray <- as.array(tmp)[1, , ]
      
      # remove casestudy if it has completely constant values (currently this is only SSA)
      if (all(sweep(tmpArray, MARGIN = 2, STATS = tmpArray[1, ], FUN = "=="))) {
        tmp[countries, , ] <- 0
        return(tmp)
      } 

      removeYears <- c()

      # extrapolation to past
      test <- FALSE
      i <- 1
      while (!test) {
        if (all(tmpArray[i, ] == tmpArray[i + 1, ])) {
          removeYears <- c(removeYears, i)
          i <- i + 1
        } else {
          test <- TRUE
        }
      }

      # extrapolation to past
      test <- FALSE
      i <- nrow(tmpArray)
      while (!test) {
        if (all(tmpArray[i, ] == tmpArray[i - 1, ])) {
          removeYears <- c(removeYears, i)
          i <- i - 1
        } else {
          test <- TRUE
        }
      }

      tmp[countries, removeYears, ] <- 0
      return(tmp)
    }

    caseStudies <- unique(mapping$CaseStudy)

    tfpShares <- magpiesort(mbind(lapply(caseStudies, .cleanCaseStudy)))

    # refill dropped countries with zeros
    tfpShares <- toolCountryFill(tfpShares, fill = 0)
  }


  # reads value of production
  voPAll <- readSource("FAO_online", "ValueOfProd")
  years <- intersect(getYears(tfpShares), getYears(voPAll))
  voPAll <- voPAll[, years, ]
  tfpShares <- tfpShares[, years, ]

  # mio. USD VoP (constant 2014_2016 thousand US$ has values before 1991, current_thousand_US$ does not)
  vopCrops <- voPAll[, ,  "2041|Crops.Gross_Production_Value_(constant_2014_2016_thousand_US$)_(1000_US$)"]/1000
  vopLivst <- voPAll[, , "2044|Livestock.Gross_Production_Value_(constant_2014_2016_thousand_US$)_(1000_US$)"]/1000
  ratio <- collapseDim(vopCrops / (vopCrops + vopLivst))

  # TODO: BETTER WAY TO FILL GAPS?
  # fill gaps in ratio with global average
  gloRatio <- dimSums(vopCrops, dim = 1) / (dimSums(vopCrops, dim = 1) + dimSums(vopLivst, dim = 1))
  for (y in getYears(ratio)) {
   ratio[where(is.na(ratio[, y, ]))$true$regions, y, ] <- gloRatio[, y, ]
  }

  # split labor costs between crops and livestock
  if (products == "kli") ratio <- 1 - ratio
  tfpShares[, , "AG_Labour"] <- tfpShares[, , "AG_Labour"] * ratio

  # mappping of categories
  if (products == "kcr") {
    mapping <- data.frame("to" = c("Labor", "Land", "Materials", "Capital"),
                          "from" = c("AG_Labour", "AG_Land", "Crop_Materials", "Crop_Machinery"))
  } else {
    mapping <- data.frame("to" = c("Labor", "Materials", "Capital"),
                          "from" = c("AG_Labour", "Animal_Materials", "Livestock"))
  }
  
  tfpShares <- toolAggregate(tfpShares[, , mapping$from], rel = mapping, from = "from", to = "to", dim = 3)
  tfpShares <- tfpShares / dimSums(tfpShares, dim = 3)
  tfpShares[!is.finite(tfpShares)] <- 0

  # vop as aggregation weight
  vop <- vopCrops + vopLivst
  weight <- tfpShares
  weight[, , ] <- vop[, getYears(tfpShares), ]
  weight[tfpShares == 0] <- 0

  return(list(x = tfpShares,
              weight = weight,
              mixed_aggregation = NULL,
              unit = "fraction",
              description = "Factor shares for crops from USDA TFP data"))

}
