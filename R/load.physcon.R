#' Load physical constants
#'
#' @return A list of physical constants
#' @export
#'
#' @examples
#' physcon<-load.physcon()
load.physcon<-function(){
  # We initiliaze the physcon list
  physcon<-list()

  physcon$rgas = 8.3144598        # Perfect gaz constant R in J mol-1 K-1
  physcon$tfrz = 273.15           # Converstion temperature from K to C, in K
  physcon$ratio_H2O_to_CO2 = 1.6  # Ratio of water vapor diffusivity to the CO2 diffusivity (unitless)

  return(physcon)
}
