#' Temperature response function with inhibition
#'
#' @param tl Leaf temeprature in K
#' @param hd Desactivation energy in J mol-1
#' @param se Entropy term in J K-1 mol-1
#' @param physcon List of physical constants
#'
#' @return This function returns the temperature response term fH(Tl)
#' @export
#'
#' @examples
#' #' physcon<-load.physcon()
#' leaf.param<-Load.LeafPhysiologyParams()
#' # We compute the temperature response of Vcmax at tl = 25°C
#' fth_Vcmax<-fth(tl=298.15,hd=leaf.param$hd,seleaf.param$se,physcon=physcon)
fth <- function(tl, hd, se, physcon){
  # we compute the upper part of the equation at 25°C
  fc25 <-1 + exp((-hd + se*(physcon$tfrz+25)) / (physcon$rgas*(physcon$tfrz+25)))
  # we compute the lower part of the equation at tl
  fc<-1 + exp((-hd+se*tl)/(physcon$rgas*tl))
  # inhibition factor fH(Tl)
  fh<- fc25/fc
  return(fh)
}
