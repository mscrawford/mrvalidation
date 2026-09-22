#' @title calcValidTreeCoverLoss
#'
#' @description Observed annual tree cover loss by dominant driver, for model validation.
#'
#' @details This is UMD/Hansen tree cover loss, not FAO forest loss, with each 1 km cell assigned
#' one dominant driver. All eight classes are returned, although only shifting cultivation reaches
#' module 35 (\code{f35_forest_lost_share}): an order-of-magnitude and pattern check, not a target.
#'
#' @param datasource Currently only \code{"GFW"} (WRI/Google DeepMind drivers of tree cover
#' loss, Sims et al. 2025, crossed with UMD/Hansen annual loss via the GFW data-api).
#' @return list of magpie object with data and weight
#' @author Michael Crawford
#' @import magpiesets
#' @importFrom magclass getNames setNames mbind dimSums add_dimension
#' @importFrom madrat readSource
#' @examples
#' \dontrun{
#' calcOutput("ValidTreeCoverLoss", datasource = "GFW")
#' }

calcValidTreeCoverLoss <- function(datasource = "GFW") {

  if (datasource != "GFW") {
    stop("No validation data exists for the given datasource!")
  }

  unit <- "million ha/yr"
  x <- readSource("GFWLossByDriver", convert = TRUE)

  drivers <- x
  getNames(drivers) <- paste0("Resources|Tree Cover Loss|+|",
                              reportingnames(getNames(drivers)), " (", unit, ")")
  total <- setNames(dimSums(x, dim = 3), paste0("Resources|Tree Cover Loss (", unit, ")"))

  out <- mbind(total, drivers)
  out <- add_dimension(out, dim = 3.1, add = "scenario", nm = "historical")
  out <- add_dimension(out, dim = 3.2, add = "model", nm = "GFW/WRI")

  return(list(x = out,
              weight = NULL,
              unit = unit,
              description = paste("Annual tree cover loss by dominant driver, UMD/Hansen loss",
                                  "crossed with the WRI/Google DeepMind 1 km driver map of Sims",
                                  "et al. (2025), at a 30 per cent canopy density threshold.")))
}
