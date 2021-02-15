rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(Photosynthesis)


params <- list(vis = 1,nir = 2)
physcon <- list(tfrz = 273.15,rgas = 8.31446)

leaf <- list(colim = 1)
leaf <- LeafPhysiologyParams(params, physcon, leaf)
leaf$vcmax25 <- 30

co2conc = 380
par_sat = 2000
leaf_temp = physcon$tfrz + 25

atmos <- list(co2air = co2conc,
              o2air = 0.209 * 1000)

atmos$par = par_sat

flux <- list()
flux$apar = atmos$par * (1 - leaf$rho[params$vis] - leaf$tau[params$vis])
flux$tleaf = leaf_temp

plot_type = 1 # 1 = par, 2 = C02, 3 = Temp

par <- tleaf <- ca <- ci_ca <- ac <- aj <- an <- c()
if (plot_type == 1){

  for (i in seq(0,par_sat,length.out = 1000)){
    atmos$par = i
    flux$apar = atmos$par * (1 - leaf$rho[params$vis] - leaf$tau[params$vis])

    flux = LeafPhotosynthesis (physcon, atmos, leaf, flux)
    par <- c(par,atmos$par)
    tleaf <- c(tleaf,flux$tleaf - physcon$tfrz)
    ca <- c(ca,atmos$co2air)
    ci_ca <- c(ci_ca,flux$ci / atmos$co2air)
    ac <- c(ac,flux$ac - flux$rd)
    aj <- c(aj,flux$aj - flux$rd)
    an <- c(an,flux$an)
  }

  df <- data.frame(flux= flux,
                   par = par,
                   tleaf = tleaf,
                   ca = ca,
                   ci_ca = ci_ca,
                   Ac = ac,
                   Aj = aj,
                   An = an)

  df_new <- df %>% pivot_longer(cols = c(Ac,Aj,An),
                                names_to = "A",
                                values_to = "value")

  newplot <- ggplot(data = df_new) +
    geom_line(aes(x = par,y = value, col = A)) +
    theme_bw()

} else if (plot_type == 2){

  for (i in seq(40,1000,length.out = 1000)){

    atmos$co2air = i / 0.7
    flux = LeafPhotosynthesis (physcon, atmos, leaf, flux)

    par <- c(par,atmos$par)
    tleaf <- c(tleaf,flux$tleaf - physcon$tfrz)
    ca <- c(ca,atmos$co2air)
    ci_ca <- c(ci_ca,flux$ci / atmos$co2air)
    ac <- c(ac,flux$ac - flux$rd)
    aj <- c(aj,flux$aj - flux$rd)
    an <- c(an,flux$an)
  }

  df <- data.frame(flux= flux,
                   par = par,
                   tleaf = tleaf,
                   ca = ca,
                   ci_ca = ci_ca,
                   Ac = ac,
                   Aj = aj,
                   An = an)
  df_new <- df %>% pivot_longer(cols = c(Ac,Aj,An),
                                names_to = "A",
                                values_to = "value")


  newplot <- ggplot(data = df_new) +
    geom_line(aes(x = ca,y = value, col = A)) +
    theme_bw()

} else if (plot_type == 3){

  for (i in seq(10,35,length.out = 1000)){

    flux$tleaf = physcon$tfrz + i;
    flux = LeafPhotosynthesis (physcon, atmos, leaf, flux)

    par <- c(par,atmos$par)
    tleaf <- c(tleaf,flux$tleaf - physcon$tfrz)
    ca <- c(ca,atmos$co2air)
    ci_ca <- c(ci_ca,flux$ci / atmos$co2air)
    ac <- c(ac,flux$ac - flux$rd)
    aj <- c(aj,flux$aj - flux$rd)
    an <- c(an,flux$an)
  }

  df <- data.frame(flux= flux,
                   par = par,
                   tleaf = tleaf,
                   ca = ca,
                   ci_ca = ci_ca,
                   Ac = ac,
                   Aj = aj,
                   An = an)

  df_new <- df %>% pivot_longer(cols = c(Ac,Aj,An),
                                names_to = "A",
                                values_to = "value")



  newplot <- ggplot(data = df_new) +
    geom_line(aes(x = tleaf,y = value, col = A)) +
    theme_bw()
}

newplot
