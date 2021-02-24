LeafPhysiologyParams <- function(params,physcon,leaf){

  leaf$vcmax25 = 60
  leaf$jmax25 = 1.67 * leaf$vcmax25
  leaf$rd25 = 0.015 * leaf$vcmax25

  leaf$kc25 = 404.9
  leaf$ko25 = 278.4
  leaf$cp25 = 42.75

  leaf$kcha = 79430
  leaf$koha = 36380
  leaf$cpha = 37830
  leaf$rdha = 46390
  leaf$vcmaxha = 65330
  leaf$jmaxha  = 43540

  leaf$rdhd = 150000
  leaf$vcmaxhd = 150000
  leaf$jmaxhd  = 150000

  leaf$rdse = 490
  leaf$vcmaxse = 490
  leaf$jmaxse  = 490

  leaf$phi_psii = 0.85

  leaf$theta_j = 0.90

  leaf$colim_c3 = 0.98

  leaf$rho[params$vis] = 0.10
  leaf$tau[params$vis] = 0.10

  return(leaf)
}

