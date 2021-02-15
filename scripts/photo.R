rm(list = ls())

# Params
Vcmax = 60
Jmax = 100.2
Rd = 0.01*Vcmax
Theta_j = 0.9
Phi_PSII = 0.85
alpha_l = 0.8
Theta_A = 0.98

# Not used
Tp = 7.1

# Similar among species
Kc = 404.9
Ko = 278.4
Gamma_star = 42.75

oi = 0.209 * 1000

#######################################################################################
# An-ci, no co-limitation
ci = seq(0,1000,length.out = 1001)
I_down = 2000

I_PSII = Phi_PSII/2*alpha_l*I_down
J = ((I_PSII + Jmax) - ((I_PSII + Jmax)**2 - 4*Theta_j*I_PSII*Jmax)**0.5)/(2*Theta_j)

Ac = Vcmax*(ci - Gamma_star)/(ci + Kc*(1+oi/Ko))
Aj = J/4*(ci - Gamma_star)/(ci + 2*Gamma_star)
An = pmin(Ac,Aj) -  Rd

plot(ci,An,type = 'l')
lines(ci,Ac-Rd,col = 'red')
lines(ci,Aj-Rd,col = 'green')
lines(ci,An,type = 'l',col = "black",lty = 2)
abline(h = 0,lt = 3)

#######################################################################################
# An-I, no co-limitation
ci = 266
I_down = seq(0,2000,length.out = 1000)

I_PSII = Phi_PSII/2*alpha_l*I_down
J = ((I_PSII + Jmax) - ((I_PSII + Jmax)**2 - 4*Theta_j*I_PSII*Jmax)**0.5)/(2*Theta_j)

Ac = Vcmax*(ci - Gamma_star)/(ci + Kc*(1+oi/Ko))
Aj = J/4*(ci - Gamma_star)/(ci + 2*Gamma_star)
An = pmin(Ac,Aj) -  Rd

plot(I_down,An,type = 'l')
lines(I_down,rep(Ac-Rd,length(I_down)),col = 'red')
lines(I_down,Aj-Rd,col = 'green')
lines(I_down,An,type = 'l',col = "black",lty = 2)
abline(h = 0,lt = 3)

#######################################################################################
# An-ci, co-limitation
ci = seq(0,1000,length.out = 1001)
I_down = 2000

I_PSII = Phi_PSII/2*alpha_l*I_down
J = ((I_PSII + Jmax) - ((I_PSII + Jmax)**2 - 4*Theta_j*I_PSII*Jmax)**0.5)/(2*Theta_j)

Ac = Vcmax*(ci - Gamma_star)/(ci + Kc*(1+oi/Ko))
Aj = J/4*(ci - Gamma_star)/(ci + 2*Gamma_star)
rho = (Ac + Aj)**2 - 4*Theta_A*Ac*Aj
x1 = ((Ac+Aj) + sqrt(rho))/(2*Theta_A)
x2 = ((Ac+Aj) - sqrt(rho))/(2*Theta_A)
A = pmin(x1,x2)
An = A -  Rd

plot(ci,An,type = 'l')
lines(ci,Ac-Rd,col = 'red')
lines(ci,Aj-Rd,col = 'green')
lines(ci,An,type = 'l')
abline(h = 0,lt = 3)


#######################################################################################
# An-I, co-limitation
ci = 266
I_down = seq(0,2000,length.out = 1000)

I_PSII = Phi_PSII/2*alpha_l*I_down
J = ((I_PSII + Jmax) - ((I_PSII + Jmax)**2 - 4*Theta_j*I_PSII*Jmax)**0.5)/(2*Theta_j)

Ac = Vcmax*(ci - Gamma_star)/(ci + Kc*(1+oi/Ko))
Aj = J/4*(ci - Gamma_star)/(ci + 2*Gamma_star)
rho = (Ac + Aj)**2 - 4*Theta_A*Ac*Aj
x1 = ((Ac+Aj) + sqrt(rho))/(2*Theta_A)
x2 = ((Ac+Aj) - sqrt(rho))/(2*Theta_A)
A = pmin(x1,x2)
An = A -  Rd

plot(I_down,An,type = 'l',ylim = c(-1,20))
lines(I_down,rep(Ac-Rd,length(I_down)),col = 'red')
lines(I_down,Aj-Rd,col = 'green')
abline(h = 0,lt = 3)