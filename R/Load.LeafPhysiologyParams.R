#' Load leaf physiological parameters
#'
#' @return This function return a list of leaf physiological parameters needed for the FcVB model
#' @export
#'
#' @examples
#' leaf.param<-Load.LeafPhysiologyParams()
Load.LeafPhysiologyParams <- function(){
  # initialisation of the leaf list
  leaf<-list()

  # leaf parameters for carbon assimilation
  leaf$vcmax25 = 60                              # Maximum carboxylation rate at 25°C in µmol m-2 s-1
  leaf$jmax25 = 1.67 * leaf$vcmax25              # Maximum electron transport rate at 25°C in µmol m-2 s-1
  leaf$rd25 = 0.015 * leaf$vcmax25               # Dark respiration at 25°C in µmol m-2 s-1

  leaf$kc25 = 404.9                              # Michaelis-Menten constant  for CO2 at 25°C in µmol m-2 s-1
  leaf$ko25 = 278.4                              # Michaelis-Menten constant  for O2 at 25°C in mmol m-2 s-1
  leaf$cp25 = 42.75                              # Gamma* constant  at 25°C in µmol mol-1 µmol m-2 s-1

  # temperature response parameters
  leaf$kcha = 79430                              # Activation energy dHa for Kc in J mol-1
  leaf$koha = 36380                              # Activation energy dHa for Ko in J mol-1
  leaf$cpha = 37830                              # Activation energy dHa for Gamma* in J mol-1
  leaf$rdha = 46390                              # Activation energy dHa for Rd in J mol-1
  leaf$vcmaxha = 65330                           # Activation energy dHa for Vcmax in J mol-1
  leaf$jmaxha  = 43540                           # Activation energy dHa for Jmax in J mol-1

  leaf$hd = 150000                               # Desactivation energy dHd in J mol-1

  leaf$se = 490                                  # Entropy term dS in J mol-1 K-1

  # Light response parameters
  leaf$phi_psii = 0.85                           # Quantuum yield of PSII in µmol m-2 s-1
  leaf$theta_j = 0.90                            # Curvature of the light response curve (0-1)
  leaf$alpha_l = 0.88                            # Leaf absorptance (0-1)

  # stomatal conductance
  leaf$gmin=76.20                                # Minimum stomatal conductance to water vapour in mmol m-2 s-1
  leaf$gfac=13.70                                # Stomatal sensitivity coefficient (dimmensionless)
  leaf$gb=300                                    # Leaf boundary layer conductance to water vapour in mmol m-2 s-1
  leaf$Ifac=0.5                                  # Coefficient representing the extent to whichRdarkis inhibited in the light (dimmensionless)

  leaf$g0=0.01
  leaf$g1=9

  return(leaf)
}

