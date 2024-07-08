#' @title calcLaborCosts
#' @description calculates total labor costs in mio. US$MER05 (coverage depends on source: crop, livestock and
#' fish production for USDA, and additionally forestry for GTAP and ILO)
#' @param datasource data source on which the labor costs should be based. Available are ILO, USDA (which also uses data
#' on VoP from FAO), and GTAP.
#' @param dataVersionILO If source is ILO, the version can be chosen. "" for the oldest version, or "monthYear" (e.g.
#' "Aug23") for a newer version)
#' @param subsectors boolean: should output be aggregated or split into available subsectors (crops, livst, forestry,
#' fishery)
#' @param inclFish boolean: should fish labor costs be included?
#' @param inclForest boolean: should forestry labor costs be included (only available for ILO and GTAP)?
#' @param otherLivst boolean: should other_livst category (i.e. beeswax, wool, silkworms, and honey) be included
#' in livestock (only relevant for datasource USDA)?
#' @param gtapVar variable name to use from GTAP (only relevant if source is "GTAP")
#' @param addSubsidies boolean: should subsidy data (from IFPRI) should be added to VoP before applying USDA cost
#' shares (only relevant if datasource is "USDA")
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Debbora Leip
#' @examples
#' \dontrun{
#' calcOutput("LaborCosts", datasource = "ILO")
#' }
#' @importFrom magclass setNames dimSums time_interpolate
#' @importFrom GDPuc convertGDP
#' @importFrom stringr str_split

calcLaborCosts <- function(datasource = "ILO", dataVersionILO = "Aug23", subsectors = TRUE, inclFish = FALSE,
                           inclForest = FALSE, otherLivst = TRUE, gtapVar = "NVFA", addSubsidies = FALSE) {

  # get data from specified source
  if (datasource == "ILO") {

    # ILO employment in agriculture (mio. people)
    iloEmpl <- calcOutput("AgEmplILO", aggregate = FALSE, subsectors = subsectors,
                          inclFish = inclFish, inclForest = inclForest, dataVersionILO = dataVersionILO)

    # ILO mean weekly hours actually worked per employed person in agriculture (h)
    iloWeeklyHours <- calcOutput("WeeklyHoursILO", aggregate = FALSE, dataVersionILO = dataVersionILO)

    # subset to joint years
    years <- intersect(getItems(iloEmpl, dim = 2), getItems(iloWeeklyHours, dim = 2))
    iloEmpl <- iloEmpl[, years, ]
    iloWeeklyHours <- iloWeeklyHours[, years, ]

    # mean nominal hourly labor cost per employee in agriculture (US$05MER/h) based on USDA_FAO calculation
    # and regression based on ILO raw data
    iloLaborCosts <- calcOutput("HourlyLaborCosts", dataVersionILO = dataVersionILO, datasource = datasource,
                                fillWithRegression = TRUE, projection = "SSP2", # scenario has no influence until 2020
                                aggregate = FALSE)[, years, ]

    # combine data sets to get total labor costs
    iloTotalHours <- 52.1429 * iloWeeklyHours[, , , drop = TRUE] * iloEmpl # mio. hours
    out <- iloTotalHours * iloLaborCosts[, , , drop = TRUE] # mio. US$MER05

  } else if (datasource == "USDA") {

    if (isTRUE(inclForest)) stop("Forest labor costs not available for this datasource")

    # Value of Production for livestock in US$MER2005 (including FAO livst categories not mapped to MAgPIE categories)
    vopLivst <- calcOutput("VoPlivst", other = otherLivst, fillGaps = TRUE, aggregate = FALSE) # mio. US$MER05
    vopLivst <- setNames(dimSums(vopLivst, dim = 3), "Livestock")

    # Value of Production for crops in US$MER2005
    vopCrops <- calcOutput("VoPcrops", fillGaps = TRUE, aggregate = FALSE) # mio. US$MER05
    vopCrops <- setNames(dimSums(vopCrops, dim = 3), "Crops")

    # no VoP data before 1991, data for 2019 incomplete (using fillGaps in calcVoP reduces years to 1991:2013 anyway)
    years <- setdiff(getItems(vopLivst, dim = 2), paste0("y", c(1960:1990, 2019)))

    # VoP of fish (reduces years)
    if (isTRUE(inclFish)) {
      vopFish <- calcOutput("VoPAFF", aggregate = FALSE)[, , "Fisheries"]
      years <- intersect(years, getItems(vopFish, dim = 2))
    }

    # add subsidies to VoP
    if (isTRUE(addSubsidies)) {
      subsidies <- calcOutput("IFPRIsubsidy", aggregate = FALSE)
      # to not loose years, we keep subsidies constant for past years which are not covered by the dataset
      subsidies <- time_interpolate(subsidies,
                                    interpolated_year = setdiff(years, getItems(subsidies, dim = 2)),
                                    integrate_interpolated_years = TRUE,
                                    extrapolation_type = "constant")

      vopCrops <- vopCrops[, years, ] + subsidies[, years, "Crops"]
      vopLivst <- vopLivst[, years, ] + subsidies[, years, "Livestock"]
    }

    # combine VoP for crops and livestock (and fish)
    vopAg <- mbind(vopLivst[, years, ], vopCrops[, years, ])
    if (isTRUE(inclFish)) vopAg <- mbind(vopAg, vopFish[, years, ])
    vopAg[!is.finite(vopAg)] <- 0

    # USDA labor cost shares
    shares <- calcOutput("FractionInputsUSDA", aggregate = FALSE)[, , "Labor"]

    # closest 5-year step before and after start of VoP data needed for interpolation of shares
    yearsInt <- as.integer(str_split(years, "y", simplify = TRUE)[, 2])
    y <- intersect(paste0("y", seq(min(yearsInt) - min(yearsInt) %% 5, max(yearsInt) - max(yearsInt) %% 5 + 5, 5)),
                   getItems(shares, dim = 2))

    # filling missing values with region average, using production as weight
    h12 <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mappingfolder")
    weight <- dimSums(collapseDim(calcOutput("Production", aggregate = FALSE)[, , "dm"]), dim = 3.1)
    weight <- time_interpolate(weight, interpolated_year = setdiff(y, getItems(weight, dim = 2)),
                               extrapolation_type = "constant", integrate_interpolated_years = TRUE)[, y, ]
    shares <- toolFillWithRegionAvg(shares[, y, ], valueToReplace = 0, weight = weight,
                                    regionmapping = h12, verbose = FALSE, warningThreshold = 1)

    # # for REF in 1990 no country has a value, so toolFillWithRegionAvg assigns NA. Use values from 1995 instead:
    # if ("y1990" %in% y) { # subsidy data starts only in 2005
    #   ref <- h12$CountryCode[h12$RegionCode == "REF"]
    #   costShares[ref, 1990, ]  <- costShares[ref, 1995, ]
    # }

    # interpolate between the five-year-steps
    shares <- time_interpolate(shares,
                               interpolated_year = setdiff(years, getYears(shares)),
                               extrapolation_type = "constant",
                               integrate_interpolated_years = TRUE)[, years, ]

    # estimate total labor costs as share of VoP
    out <- vopAg[, years, ] * collapseDim(shares)

    # aggregate if subsectors is FALSE, but only in cases where both crops and livestock are included
    if (isFALSE(subsectors)) {
      incl <- out[, , "Livestock"] * out[, , "Crops"]
      out <- dimSums(out, dim = 3)
      out[incl == 0] <- 0
    }

  } else if (datasource == "GTAP") {

    # gtap data in mio. current US$MER
    if (gtapVar == "NVFA") {
      gtap <- dimSums(readSource("GTAPv8v9", "9:SF01"), dim = c("dir", "purchvalue"))
    } else if (gtapVar == "VFM") {
      gtap <- readSource("GTAPv8v9", "9:VFM")
      getSets(gtap) <- c("reg", "year", "demd_comm", "prod_comm")
    } else if (gtapVar == "EVFA") {
      gtap <- readSource("GTAPv8v9", "9:AG03")
      getSets(gtap) <- c("reg", "year", "demd_comm", "prod_comm")
    } else {
      stop("This GTAP variable is not available for labor costs")
    }

    labor <- c("unsklab", "sklab")

    # https://www.gtap.agecon.purdue.edu/databases/contribute/detailedsector.asp
    kcr <- c("pdr", "wht", "gro", "v_f", "osd", "c_b", "pfb", "ocr")
    kli <- c("ctl", "oap", "rmk")
    fish <- c("fsh")
    forest <- c("frs")

    # get sectors according to function inputs
    sectors <- list(kcr, kli)
    sectorNames <- c("Crops", "Livestock")
    if (isTRUE(inclFish)) {
      sectors <- c(sectors, fish)
      sectorNames <- c(sectorNames, "Fisheries")
    }
    if (isTRUE(inclForest)) {
      sectors <- c(sectors, forest)
      sectorNames <- c(sectorNames, "Forestry")
    }

    .getSector <- function(items, name) {
      return(setNames(dimSums(gtap[, , list("demd_comm" = labor, "prod_comm" = items)],
                              dim = c("demd_comm", "prod_comm")), name))
    }

    laborCosts <- mbind(Map(.getSector, items = sectors, name = sectorNames))

    # aggregate if subsectors == FALSE
    if (isFALSE(subsectors)) laborCosts <- dimSums(laborCosts, dim = 3)

    # convert to USDMER05 (for countries missing the inflation factor, we assume no inflation)
    out <- convertGDP(laborCosts, unit_in = "current US$MER",
                      unit_out = "constant 2005 US$MER", replace_NAs = "no_conversion")

  } else {
    stop("Data source not available")
  }

  return(list(x = out,
              weight = NULL,
              unit = "mio. USD05MER",
              description = "labor costs in agriculture"))
}
