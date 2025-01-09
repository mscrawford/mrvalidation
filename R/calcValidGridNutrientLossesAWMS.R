#' @title calcValidGridNutrientLossesAWMS
#' @description reports Nutrient Losses in animal waste management on 0.5 degree grid
#'
#' @param nutrient can be c, nr, p, k. For p and k, no losses are assumed in confinements.
#' @return List of magpie objects with results on cellular level, weight on cellular level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' \dontrun{
#' calcOutput("ValidGridNitrogenBudgetCropland")
#' }
#'
#' @importFrom magpiesets reportingnames
#' @importFrom magclass getComment<-
#' @importFrom mstools toolIso2CellCountries

calcValidGridNutrientLossesAWMS <- function(nutrient = c("nr", "c")) {

  past               <- findset("past")
  excretion          <- collapseNames(calcOutput("Excretion", attributes = "npkc",
                                                 cellular = TRUE, cells = "lpjcell",
                                                 aggregate = FALSE)[, past, ])
  emissionFactorsN   <- dimSums(calcOutput("EF3confinement", aggregate = FALSE), dim = 3.3)
  lossRatesC         <- calcOutput("ClossConfinement", aggregate = FALSE)
  awmsShare          <- collapseNames(calcOutput("AWMSconfShr",
                                                 aggregate = FALSE)[, past, "constant"])

  emissionFactorsN   <- toolIso2CellCountries(emissionFactorsN)
  lossRatesC         <- toolIso2CellCountries(lossRatesC)
  awmsShare          <- toolIso2CellCountries(awmsShare)

  fuel <- excretion[, , "fuel"]

  manureNitrogen       <- dimSums(collapseNames(excretion[, , "confinement"])[, , "nr"] *
                                    awmsShare * emissionFactorsN,
                                  dim = c("Manure_Management_System_Usage"))
  vcat(verbosity = 2, "no P and K losses in manure management assumed")
  manurePhosphorKalium <- collapseNames(excretion[, , c("p", "k")][, , "confinement"])
  manureCarbon         <- dimSums(collapseNames(excretion[, , "confinement"])[, , "c"] *
                                    awmsShare * lossRatesC,
                                  dim = c("Manure_Management_System_Usage"))

  confinementLoss <- mbind(manureNitrogen, manurePhosphorKalium, manureCarbon)
  confinementLoss <- add_dimension(confinementLoss, dim = 3.1,
                                   add = "type",
                                   nm = "Losses in confinements")
  out <- mbind(fuel, confinementLoss)
  out <- out[, , nutrient]

  getNames(out, dim = 3) <- reportingnames(getNames(out, dim = 3))
  getNames(out, dim = 2) <- reportingnames(getNames(out, dim = 2))

  return(list(x = out,
              weight = NULL,
              unit = "Mt Nr/yr and Mt C/yr",
              description = "Losses in confinemnet and fuel use of manure",
              isocountries = FALSE))
}
