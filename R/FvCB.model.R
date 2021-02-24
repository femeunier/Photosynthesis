#' Computes the simplest form of the FvCB model without colimitation
#'
#' @param physcon List of physical constants
#' @param atmos List of atmospheric conditions containing the variables PAR, Ci, Oi and tleaf
#' @param leaf List of leaf physiological parameters
#'
#' @return This function returns a list of fluxes simulated by the photosynthesis model
#' @export
#'
#' @examples
#'physcon<-load.physcon()
#'leaf<-Load.LeafPhysiologyParams()
#'atmos<-list(PAR=2000,Ci=400,Oi=209,tleaf=300)
#'photosynthesis<-FvCB.model(physcon,atmos,leaf)
#'
FvCB.model <- function(physcon, atmos, leaf){
  # We initialize the flux output
  flux<-list()

  # We compute the adjusted parameter values for leaf temperature
  # for Kc
  flux$kc = leaf$kc25 * ft(atmos$tleaf, leaf$kcha, physcon)
  # for Ko
  flux$ko = leaf$ko25 * ft(atmos$tleaf, leaf$koha, physcon)
  # for Gamma*
  flux$cp = leaf$cp25 * ft(atmos$tleaf, leaf$cpha, physcon)

  # for Vcmax
  t1 = ft(atmos$tleaf, leaf$vcmaxha,physcon) # term 1 ft
  t2 = fth(atmos$tleaf, leaf$hd, leaf$se,physcon) #term 2 fHt
  flux$vcmax = leaf$vcmax25 * t1 * t2

  # for Jmax
  t1 = ft(atmos$tleaf, leaf$jmaxha,physcon)
  t2 = fth(atmos$tleaf, leaf$hd, leaf$se, physcon)
  flux$jmax = leaf$jmax25 * t1 * t2

  # for Rd

  t1 = ft(atmos$tleaf, leaf$rdha,physcon)
  t2 = fth(atmos$tleaf, leaf$hd, leaf$se, physcon)
  flux$rd = leaf$rd25 * t1 * t2

  # We compute the PSII light utilization Iabs in µmol m-2 s-1
  flux$Iabs = 0.5 * leaf$phi_psii * leaf$alpha_l * atmos$PAR

  # We compute the Electron transport rate J in µmol m-2 s-1
  flux$J = (flux$Iabs+ flux$jmax - ((flux$Iabs + flux$jmax)**2 - 4*leaf$theta_j*flux$Iabs*flux$jmax)**0.5)/(2*leaf$theta_j)

  # We compute the Light-limited assimiliation Wj in µmol m-2 s-1
  flux$Wj = flux$J/4*(atmos$Ci - flux$cp)/(atmos$Ci + 2*flux$cp)

  # We compute the CO2-limited assimiliation Wc in µmol m-2 s-1
  flux$Wc = flux$vcmax*(atmos$Ci - flux$cp)/(atmos$Ci + flux$kc*(1+atmos$Oi/flux$ko))

  # We compute net assimilation as the minimum of Wc and Wj in µmol m-2 s-1
  flux$An = min(flux$Wc,flux$Wj) -  flux$rd

  return(flux)
}
