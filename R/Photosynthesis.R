#' Photosynthesis
#' @description This function compute photosynthesis based on the FvCB model for carbon assimilation and the
#' Ball and Berry model for stomatal conductance
#'
#' It uses an iterative approach. Analytical solutions exists, however using the iterative approach will help you
#' to make the link with the FvCB.model function that was created during Practical_A
#' @param atmos List of environemental variables to run the FvCB and gs model
#' @param leaf List of leaf physiological parameters
#' @param physcon List of physical constants
#'
#' @return Return a list with assimilation, Ci and gs
#' @export
#'
#' @examples
Photosynthesis<-function(atmos, leaf, physcon){

  ############ Compute An, gs and Ci in an iterative way until converge ##############
  # Using iterative approaches is not the best practice. Analytical solution exists.
  # However, you can see how the functioin FvCB.model that you created during Practical A is used here
  # This is also how it was done in the very first models (see slide 42 of lecture 2)


  # conditioins to end the iterations
  iter<-0 # number of iteration
  dCi<-0.1 # convergence criterion
  max_iter<-20 # max number of iteraction
  dCi_min<-0.01 # threshold for convergence

   while(dCi>dCi_min & iter<max_iter) {
    # we store the old ci
    old_ci<-atmos$Ci

    # We compute cO2 assimilation with the approximation of Ci
    Assimi = FvCB.model(physcon = physcon, atmos = atmos, leaf = leaf)

    # we compute stomatal conductance based on boundary layer conductance gb (see Slide 36, lecture 2)
    Cs=atmos$Ca/10-Assimi$An*1.4*100/leaf$gb # Ca in Pa
    gs=leaf$gmin+leaf$gfac*(Assimi$An+Assimi$rd*leaf$Ifac)*atmos$RH/Cs # in mmol m-2 s-1
   #gs=leaf$g0 + leaf$g1 * Assimi$An * atmos$RH / atmos$Ca


    # We estimate the corresponding Ci
    atmos$Ci<-10*(Cs-Assimi$An*1.6*100./gs) #Eq.16

    # We check for convergence
    dCi<-abs(old_ci-atmos$Ci)
    iter=iter+1
  }

  # list of outputs
  photo<-list()
  photo$assimi<-Assimi
  photo$Ci<-atmos$Ci
  photo$gs<-gs

  return(photo)
}
