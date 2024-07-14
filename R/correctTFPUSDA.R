#' @title correctTFPUSDA
#' @description Remove values that are just constant extrpolation
#' @param x magpie object provided by the read function
#' @return corrected mapie object on national-scale costs of production for 10
#' crops, disaggregated in 9 distinct cost elements
#' @author Debbora Leip
#' @seealso
#' [readTFPUSDA()]

correctTFPUSDA <- function(x) {

  # both TUN and MAR use data from the WANA case study, but land cost share for TUN is different, 
  # such that the sum of cost shares for TUN is not adding up to 1
  x["TUN", , ] <- x["MAR"]

  return(x)

}
