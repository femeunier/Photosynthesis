fth <- function(tl, hd, se, fc,physcon){
  return(fc / (1 + exp((-hd+se*tl)/(physcon$rgas*tl))))
}
