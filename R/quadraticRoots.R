quadraticRoots <- function(aquad,bquad,cquad){

  rho <- bquad**2 - 4*aquad*cquad
  rhosq <- sqrt(rho)
  x1 <- (-bquad - rhosq)/(2*aquad)
  x2 <- (-bquad + rhosq)/(2*aquad)

  return(c(x1,x2))
}
