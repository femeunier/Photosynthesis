#' Canopy fluxes
#' @description
#'
#' @param meteo List of meteorological variables as defined in FLUXNET data with TA in °C, PA in kPa, SW in W m-2 and RH in \%
#' @param leaf List of leaf physiological parameters
#' @param LAI_layer Number of LAI layers to compute photosynthesise, default = 10
#'
#' This parameter will define the extinction of light into the canopy.
#' If LAI_layer = 1, all leaves are considered top sun exposed leaves and will receive all the light.
#' @param LAI_tot Total LAI at site in m2/m2
#'
#' @return
#' @export
#'
#' @examples
#'
canopy_fluxes<-function(meteo, leaf, LAI_layer = 20, LAI_tot = 4){
  physcon<-load.physcon()

  # We initiate the list of arguments for the FvCB model PAR, Ci, Oi and tleaf
  atmos<-list()
  atmos$Oi<-209 # we fic Oi to atmospheric concentrations
  # We convert air temperature in K. Here we consider that leaf temperature = air temperature.
  # We could include an energy balance module to compute leaf temperature from radiations and wind.

  atmos$tleaf<-meteo$TA + physcon$tfrz

  # We convert atmospheric CO2 concentration in ppm to Ca in µmol mol-1
  atmos$Ca<-co2_to_ca(co2 = meteo$CO2, patm = meteo$PA)

  # We compute a first estimate of Ci
  # Several models make the assumption that Ci = 70% of Ca.In fact we know that Ci varies between biomes and climate conditions
  atmos$Ci <- 0.7 * atmos$Ca

  # We now add the relative humidity needed to compute stomatal conductance
  atmos$RH<-meteo$RH

  # We compute the extinction coefficient of the light into the canopy
  # we use an exponential extinction coefficient following the Beer-Lambert Law.
  # This approach is still commonly used in big leaf models and originates from Monsi & Saeki 1953 (slide 15 of Lecture 3)

  laitab<-NULL # tabulated LAI along the vertical profile
  ext_coef<-NULL # extinction coefficient
  lai_level_depth<-0.15 # factor of canopy depth from Monsi & Saeki 1953
  for (i in 1:(LAI_layer+1)){
    laitab[i]<-LAI_tot*(exp(lai_level_depth*(i-1)-1))/(exp(lai_level_depth*LAI_layer)-1)
    ext_coef[i]<-exp(-0.5*laitab[i]) # 0.5 corresponds to Kb, the direct beam extinction coefficient (see slide 16 of Lecture 3)
  }

  # If you want to look at the ext_coef profil, run the following commands
  # plot(ext_coef~c(1:20))

  # We have SW in FLUXNET data, we need to convert SW to PAR
  # 1 W/m2 ≈ 4.6 μmole.m2/s and only half of shortwave radiations correspond to PAR (see slide 5 of lecture 3)
  PAR<-meteo$SW*4.6*0.5


  # We then compute photosynthesis for each LAI layer
  canopy_integration<-NULL
  for (i in 1:LAI_layer){
    atmos$PAR<-PAR * ext_coef[i] * 0.5 # 0.5 extinction coefficient from Monsi & Saeki
    photo<-Photosynthesis(atmos = atmos,leaf = leaf,physcon = physcon)
    canopy_integration<-rbind(canopy_integration,as.data.frame(photo))
  }


  # We integrate fluxes at the canopy level
  assimtot<-0
  gstot<-0

  for (i in LAI_layer){
    assimtot = assimtot +   canopy_integration$assimi.An[i] * (laitab[i+1]-laitab[i])
    gstot = gstot +  canopy_integration$gs[i] * (laitab[i+1]-laitab[i])
  }

  # We export the variables
  out<-cbind(gpp_canopy=assimtot,
                  gs_canopy=gstot)
  return(out)

}
