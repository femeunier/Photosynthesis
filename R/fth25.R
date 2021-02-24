fth25 <- function(hd,se,physcon){
  return( 1 + exp((-hd + se*(physcon$tfrz+25)) / (physcon$rgas*(physcon$tfrz+25))))
}
