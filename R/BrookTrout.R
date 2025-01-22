#' Brook Trout eDNA and Environmental Data
#'
#' This dataset contains information on brook trout detections using 
#' environmental DNA (eDNA) and environmental parameters collected from various
#' sites in Ontario, Canada. The data was sourced from a scientific study 
#' comparing eDNA sampling methods with electrofishing to detect Brook trout 
#' populations.
#'
#' @format A dataframe with 10 variables and multiple rows (one row per sample):
#' \describe{
#'   \item{Backpack}{Character. The type of eDNA sampler: "OSMOS" or "ANDe".}
#'   \item{Site}{Integer. The site number where the sample was taken.}
#'   \item{eFishCatch}{Integer. The number of fish caught via electrofishing.}
#'   \item{AirTemp}{Numeric. Air temperature in degrees Celsius.}
#'   \item{WaterTemp}{Numeric. Water temperature in degrees Celsius.}
#'   \item{pH}{Numeric. pH level of the water sample.}
#'   \item{DissolvedOxygen}{Numeric. Dissolved oxygen concentration in mg/L.}
#'   \item{Conductivity}{Numeric. Conductivity in uS/cm.}
#'   \item{VolumeFiltered}{Numeric. Volume of water filtered in litres.}
#'   \item{eDNAConc}{Numeric. eDNA concentration in copies per microlitre.}
#' }
#'
#' @source Adapted from Nolan, K. P., Loeza-Quintana, T., Little, H. A., et al. 
#'   (2022). Detection of brook trout in spatiotemporally separate locations 
#'   using validated eDNA technology. *Journal of Environmental Studies and 
#'   Sciences*, 13, 66-82. <doi:10.1007/s13412-022-00800-x>
#'
#' @examples
#' data(BrookTrout)
#' summary(BrookTrout)
#' plot(eDNAConc ~ Site, data = BrookTrout)
"BrookTrout"

