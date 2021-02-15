ft <- function(tl,ha,physcon){
  return(exp(ha/(physcon$rgas*(physcon$tfrz+25)) * (1-(physcon$tfrz+25)/tl)))
}
