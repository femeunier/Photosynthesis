rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)

# Parameters
Wmax = 800
W0 = 250
a = 1
b = 0.5

# System properties
Dz = 1000

# Timestep and number of timesteps
Delta_t = 1
N = 100

# Initialization
W <- rep(NA,N +1)
E <- Dr <- rep(NA,N)

# Forcings
Pr = c(runif(N/2,min = 0,max = 0.1),
       runif(N/2,min = 0,max = 10)) # mm dry/wet season
Ep = runif(N,min = 0,max = 6) # mm

# Initial conditions
W[1] = 500 # mm

# Resolution
for (i in seq(N)){
  Dr[i] <- a*(W[i]/Wmax)**b
  E[i] <- ifelse(W[i] < W0,Ep*(W[i]/W0),Ep[i])
  W[i+1] <- min(Dz,max(0,W[i] + Delta_t*(Pr[i] - E[i] - Dr[i])))
}

# Plotting
df <- data.frame(t = 1:N,Dr,E,W = W[2:(N+1)],Pr)
df.long <- df %>% pivot_longer(cols = c(Dr,E,W,Pr),
                               names_to = "variable",
                               values_to = "value")

A <- ggplot(data = df.long %>% dplyr::filter(variable != ("W"))) +
  geom_line(aes(x = t, y = value, col = as.factor(variable))) +
  theme_bw() +
  labs(x = "Time (d)", y = "flux (mm/d)", color = "Variable") +
  theme(legend.position = c(0.1,0.8),
        text = element_text(size = 24))

B <- ggplot(data = df.long %>% dplyr::filter(variable == ("W"))) +
  geom_line(aes(x = t, y = value)) +
  labs(x = "Time (d)", y = "W (mm)") +
  theme_bw()+ guides(color = FALSE) +
  theme(text = element_text(size = 24))

plot_grid(A,B,nrow = 2)
