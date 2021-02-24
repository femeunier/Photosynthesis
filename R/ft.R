#' Temperature response function based on Arrhenius
#'
#' @param tl Leaf temperature in K
#' @param ha Activation energy in J mol-1
#' @param physcon List of physical constants
#'
#' @return This function returns the temperature correction factor f(Tl)
#' @export
#'
#' @examples
#' physcon<-load.physcon()
#' leaf.param<-Load.LeafPhysiologyParams()
#' # We compute the temperature response of Kc at tl = 25°C
#' ft_Kc<-ft(tl=298.15,ha=leaf.param$kcha,physcon=physcon)
ft <- function(tl,ha,physcon){
  return(exp((ha/(physcon$rgas*(physcon$tfrz+25))) * (1-(physcon$tfrz+25)/tl)))
}
