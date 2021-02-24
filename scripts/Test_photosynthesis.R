rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)
library(Photosynthesis)

# Check the parameters for the functions canopy_fluxes
?canopy_fluxes

# we load needed parameters
# leaf physiolofical parameters
leaf <- Load.LeafPhysiologyParams()

# Adapt leaf photosynthetic parameters corresponding to average values for boreal conifers
# leaf$vcmax25 <- 30
# leaf$jmax25 <- 1.67 * leaf$vcmax25

# We load meteo data from the fluxnet site
fluxnet_data<-read.table(file = "scripts/FLUXNET_FI-HYY_2005-2014.csv",header = TRUE,sep = ",")

# Check and replace non-sensical values of met forcings here
# fluxnet_data$RH[fluxnet_data$RH<0] <- mean(fluxnet_data$RH[fluxnet_data$RH>0])
# fluxnet_data$NETRAD[fluxnet_data$NETRAD<0] <- mean(fluxnet_data$NETRAD[fluxnet_data$NETRAD>0])


#Fluxnet data: GPP in µmolCO2 m-2 leaf s-1
# we want to apply canopy_fluxes at each time step

siteLAI <- 6.7
apply.photosynthesis<-function(x,leaf,fluxnet_data){
  meteo<-fluxnet_data[x,]
  meteo$CO2<-410 # We don't forget atmospheric CO2 concentration, constant here
  gpp<-canopy_fluxes(meteo = meteo,leaf = leaf, LAI_layer = 20, LAI_tot = siteLAI)
  return(gpp)
}

N <- 5000 # number of timesteps to consider
results<-sapply(X = c(1:N),FUN = apply.photosynthesis,leaf=leaf,fluxnet_data=fluxnet_data)

# GPP is in µmolCO2 m-2 soil s-1, so we need to multiply by the site LAI
plot(results[1,1:N]*siteLAI,fluxnet_data$GPP[1:N])
