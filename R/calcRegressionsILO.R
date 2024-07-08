#' @title regressionsILO
#' @description Calculates regression coefficients used by calc functions for ILO data sets
#'
#' @author Debbora Leip
#' @param subtype specifies the regression type: "AgEmplShare" for a regression between the square root of the
#' share of people employed in agriculture (out of total population) and the log (base 10) of GDP pc PPP05.
#' "HourlyLaborCosts" for a regression between  mean nominal hourly labor cost per employee in
#' agriculture and GDP pc MER05.
#' @param dataVersionILO which version of the ILO input data and regression to use. "" for the oldest version and
#' old regression, or "monthYear" (e.g. "Aug23") for newer data with the new regression type
#' @param thresholdWage  only relevant for linear hourly labor cost regression: for low GDP pc MER, the regression
#' between hourly labor costs and GDP pc MER can lead to unreasonably low or even negative hourly labor costs.
#' Therefore, we set all hourly labor costs below this threshold to the threshold.
#' @param forceWageIntercept only relevant for linear hourly labor cost regression: If TRUE, the wage threshold is also
#' used as intercept of the regression. If FALSE, the intercept is determined by the regression
#' @param wageRegrType Only relevant for HourlyLaborCosts regression. If NULL, a linear regression will be used for the
#' oldest data (dataVersionILO: ""), and a loglog regression for all newer data. Can be overwritten by specifically
#' setting wageRegrType to "linear" or "loglog".
#' @param recalculate whether regression should be read from source folder, or recalculated from scratch. Recalculation
#' can lead to new regression coefficients if data changed, and result should always be checked.
#' @examples
#' \dontrun{
#'     a <- calcOutput("RegressionsILO", subtype = "HourlyLaborCosts")
#' }

calcRegressionsILO <- function(subtype = "AgEmplShare", dataVersionILO = "Aug23",
                               thresholdWage = 0.1, forceWageIntercept = TRUE,
                               wageRegrType = NULL, recalculate = FALSE) {

  if (isFALSE(recalculate)) {
    subtypeVersion <- ifelse(dataVersionILO == "", subtype, paste(subtype, dataVersionILO, sep = "_"))
    regCoeffs <- readSource("RegressionsILO", subtype = subtypeVersion)

    if (subtype == "AgEmplShare") {
      description <- paste0("Regression coeffcients for sqrt(ag. empl. share) ~ log(GDP pc PPP, base = 10) ",
                          "and a empl. share threshold")
    } else if (subtype == "HourlyLaborCosts") {
      if (dataVersionILO == "") {
        description <- paste0("Regression coeffcients for hourly labor costs ~ GDP pc MER, ",
                            "and a wage threshold")
      } else {
        description <- "Regression coeffcients of pooled OLS for log(hourly labor costs) ~ log(GDP pc MER)"
      }
    }

    return(list(x = regCoeffs,
                weight = NULL,
                unit = "",
                description = description))
  }

  cat(paste0("Note: You are recalculating this regression. If this should be the new default regression you",
             " need to manually save the resulting regression coefficients in the source folder."))

  if (subtype == "AgEmplShare") {# Regression to fill missing countries in ILO ag. employment data set

    cat(paste0("Note: In case underlying data changed (agricultural employment from ILO, historic population,",
                " or GDP pc PPP) you should double check the resulting regression."))
    description <- paste0("Regression coeffcients for sqrt(ag. empl. share) ~ log(GDP pc PPP, base = 10) ",
                          "and a empl. share threshold")

    # read original employment dataset from ILO (convert from thous. to mil.)
    dataType <- ifelse(dataVersionILO == "", "EmplByActivityModelled",
                       paste("EmplByActivityModelled", dataVersionILO, sep = "_"))
    iloEmpl <- readSource("ILOSTAT", dataType)[, , "Aggregate: Agriculture"] / 1000
    getNames(iloEmpl, dim = 2) <- "Agriculture, forestry and fishing"

    # set missing values to NA
    iloEmpl[iloEmpl == 0] <- NA

    # calculate share of people employed in agriculture
    pop <- calcOutput("PopulationPast", aggregate = FALSE) # million people
    pop <- pop[, intersect(getYears(pop), getYears(iloEmpl)), ]
    share <- iloEmpl[, getYears(pop), "Total", drop = TRUE] / pop[, , , drop = TRUE]
    getNames(share) <- "share_empl_ag"

    # GDP per capita as independent variable (sqrt(share of people in ag.) ~ log_10(GDPpcPPP))
    gdpPc <- calcOutput("GDPpcPast", GDPpcPast = "WDI-MI", unit = "constant 2005 Int$PPP", aggregate = FALSE)

    years <- intersect(getYears(share), getYears(gdpPc))
    gdpPc <- gdpPc[, years, ]
    share <- share[, years, ]

    getSets(gdpPc) <- c("region", "year", "data")
    getNames(gdpPc) <- "GDP_PPP_pc"

    # regression weighted with population (as countries with high population have an higher impact on the overall
    # employment number)
    weight <- pop / dimSums(pop, dim = 1)

    # combine data
    data <- mbind(share, gdpPc, weight)
    data <- as.data.frame(data)[, -1]
    data  <- reshape(data, idvar = c("Region", "Year"), timevar = "Data1", direction = "wide")
    colnames(data) <- c("Region", "Year", "shareEmplAg", "GDPpcPPP", "pop")

    # using a log scale for GDP and sqrt scale for share of people working in
    # agriculture leads to a approximately linear relationship
    data$transformedGDPpcPPP    <- log(data$GDPpcPPP, base = 10)
    data$transformedshareEmplAg <- sqrt(data$shareEmplAg)

    # regression
    reg       <- lm(transformedshareEmplAg ~ transformedGDPpcPPP, data = data, weights = pop)
    intercept <- reg$coefficients["(Intercept)"][[1]]
    slope     <- reg$coefficients["transformedGDPpcPPP"][[1]]

    # for very high GDP, this regression leads to a increasing share of people employed in agriculture -> we keep
    # the share constant after a certain threshold.
    # We calculated the default threshold as half of the lowest observed share (empl. data version from Aug 21)
    thresholdEmplShare <- 0.5 * min(share, na.rm = TRUE)
    thresholdGDP <- 10 ** ((sqrt(thresholdEmplShare) - intercept) / slope)

    # combine coefficients in MAgPIE object
    regCoeffs <- new.magpie(names = c("intercept", "slope", "threshold"))
    regCoeffs[, , "intercept"] <- intercept
    regCoeffs[, , "slope"] <- slope
    regCoeffs[, , "threshold"] <- thresholdGDP

  } else if (subtype == "HourlyLaborCosts") {

    ## HOURLY LABOR COSTS DATA FROM ILO

    # original hourly labor costs dataset (ILO data + data for India + data for China)
    hourlyLaborCosts <- calcOutput("HourlyLaborCosts", datasource = "ILO", dataVersionILO = dataVersionILO,
                                  fillWithRegression = FALSE, aggregate = FALSE)
    hourlyLaborCosts[hourlyLaborCosts == 0] <- NA

    ## GDP PER CAPITA IN US$MER AS DEPENDENT VARIABLE
    gdpPcMER <- calcOutput("GDPpcPast", GDPpcPast = "WDI-MI", unit = "constant 2005 US$MER", aggregate = FALSE)

    ## combining data
    years <- intersect(getItems(hourlyLaborCosts, dim = 2), getItems(gdpPcMER, dim = 2))
    data  <- mbind(hourlyLaborCosts[, years, ], gdpPcMER[, years, ])
    data <- as.data.frame(data)[, -1]
    data  <- reshape(data, idvar = c("Region", "Year"), timevar = "Data1", direction = "wide")
    colnames(data) <- c("Region", "Year", "LaborCosts", "gdpPcMER")

    if (is.null(wageRegrType)) {
      wageRegrType <- ifelse(dataVersionILO == "", "linear",  "loglog")
    } else {
      warning(paste0("You manually set the regression type of the hourly labor cost ",
                     "regression to ", wageRegrType, ". This option was included for ",
                     "testing purposes. In general, the linear regression should be ",
                     "only used for the oldest data version, and the log-log regression ",
                     "in all newer cases."))
    }

    if (wageRegrType == "linear") {
      if (thresholdWage != 0.1) warning(paste0("You changed the wage threshold. This option was included ",
                                              "for testing purposes. Might lead to inconsistencies with ",
                                              "functions using the default regression (e.g. calcLaborCosts ",
                                              "or calcValidHourlyLaborCosts)"))

      if (isFALSE(forceWageIntercept)) warning(paste0("You changed the wage intercept setting. This option was ",
                                              "included for testing purposes. Might lead to inconsistencies with ",
                                              "functions using the default regression (e.g. calcLaborCosts ",
                                              "or calcValidHourlyLaborCosts)"))

      cat(paste0("Note: In case underlying data changed (hourly labor costs from ILO, ",
                  "or GDP pc MER) you should double check the resulting regression."))
      description <- paste0("Regression coeffcients for hourly labor costs ~ GDP pc MER, ",
                          "and a wage threshold")

      # calculate regression (no regr weight, as we only have few countries and India would get too much influence)
      if (isTRUE(forceWageIntercept)) {
        reg <- lm(I(LaborCosts - thresholdWage) ~ 0 + gdpPcMER, data = data)
        intercept <- thresholdWage
      } else {
        reg <- lm(LaborCosts ~ gdpPcMER, data = data)
        intercept <- reg$coefficients["(Intercept)"]
      }

      slope <- reg$coefficients["gdpPcMER"]

      # create magclass object
      regCoeffs <- new.magpie(cells_and_regions = "GLO", names = c("intercept", "slope", "threshold"),
                                            fill = NA, sets = c("region", "year", "data"))
      regCoeffs[, , "slope"] <- slope
      regCoeffs[, , "intercept"] <- intercept
      regCoeffs[, , "threshold"] <- thresholdWage
    } else {
      description <- "Regression coeffcients of pooled OLS for log(hourly labor costs) ~ log(GDP pc MER)"
      cat(paste0("Note: In case underlying data changed (hourly labor costs from ILO, ",
                  "or GDP pc MER) you should double check the resulting regression."))

      reg <- lm(log(LaborCosts) ~ log(gdpPcMER), data = data)
      intercept <- reg$coefficients["(Intercept)"]
      slope <- reg$coefficients["log(gdpPcMER)"]

      # create magclass object
      regCoeffs <- new.magpie(cells_and_regions = "GLO", names = c("intercept", "slope"),
                              fill = NA, sets = c("region", "year", "data"))
      regCoeffs[, , "slope"] <- slope
      regCoeffs[, , "intercept"] <- intercept
    }
  }

  return(list(x = regCoeffs,
              weight = NULL,
              unit = "",
              description = description))
}
