#' @title calcValidTreeCoverLoss
#'
#' @description Observed annual tree cover loss by dominant driver, for model validation.
#'
#' @details The full eight-class decomposition is returned on purpose, including the classes
#' MAgPIE does NOT take as an input. That is the point of putting this dataset here: module 35
#' consumes only the drivers that leave the land as forest and that MAgPIE does not already
#' decide for itself (shifting cultivation, wildfire, other natural disturbances), because the
#' parameter it feeds is an age-class reset with regrowth, not a land-use transition. The
#' remaining classes are the observational counterpart to quantities MAgPIE solves for
#' endogenously, and belong here rather than in the model input:
#'
#' \itemize{
#'   \item \emph{Permanent agriculture} and \emph{Hard commodities} - benchmark for the model's
#'         endogenous forest-to-cropland and forest-to-pasture transitions (\code{vm_lu_transitions}).
#'   \item \emph{Logging} - benchmark for endogenous timber harvest from natural vegetation
#'         (\code{v35_hvarea_*}, driven by module 73 demand).
#'   \item \emph{Settlements and infrastructure} - benchmark for the urban land pool (module 34).
#'   \item \emph{Unknown} - unattributed loss; kept so the classes sum to the total.
#' }
#'
#' The children sum exactly to the total, which is enforced upstream in
#' \code{mrland:::checkGFWLossByDriver()} against an independently aggregated country-total file.
#'
#' Note that this is UMD/Hansen \emph{tree cover} loss, not forest loss in the FAO sense, and
#' that the driver map assigns all loss in a 1 km cell to a single dominant class. Neither is a
#' like-for-like match to a MAgPIE land pool; treat the comparison as an order-of-magnitude and
#' pattern check, not a target.
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
