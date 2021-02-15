LeafPhotosynthesis <- function(physcon, atmos, leaf, flux){

  leaf$vcmaxc = fth25(leaf$vcmaxhd, leaf$vcmaxse,physcon)
  leaf$jmaxc  = fth25(leaf$jmaxhd, leaf$jmaxse,physcon)
  leaf$rdc    = fth25(leaf$rdhd, leaf$rdse,physcon)

  flux$kc = leaf$kc25 * ft(flux$tleaf, leaf$kcha, physcon)
  flux$ko = leaf$ko25 * ft(flux$tleaf, leaf$koha, physcon)
  flux$cp = leaf$cp25 * ft(flux$tleaf, leaf$cpha, physcon)

  t1 = ft(flux$tleaf, leaf$vcmaxha,physcon)
  t2 = fth(flux$tleaf, leaf$vcmaxhd, leaf$vcmaxse, leaf$vcmaxc,physcon)
  flux$vcmax = leaf$vcmax25 * t1 * t2

  t1 = ft(flux$tleaf, leaf$jmaxha,physcon)
  t2 = fth(flux$tleaf, leaf$jmaxhd, leaf$jmaxse, leaf$jmaxc,physcon)
  flux$jmax = leaf$jmax25 * t1 * t2

  t1 = ft(flux$tleaf, leaf$rdha,physcon)
  t2 = fth(flux$tleaf, leaf$rdhd, leaf$rdse, leaf$rdc,physcon)
  flux$rd = leaf$rd25 * t1 * t2

  qabs = 0.5 * leaf$phi_psii * flux$apar
  aquad = leaf$theta_j
  bquad = -(qabs + flux$jmax)
  cquad = qabs * flux$jmax

  proots = quadraticRoots(aquad,bquad,cquad)
  flux$je = min(proots)

  flux$ci = 0.7 * atmos$co2air

  flux$ac = flux$vcmax * max(flux$ci - flux$cp, 0) / (flux$ci + flux$kc * (1 + atmos$o2air / flux$ko))
  flux$aj = flux$je * max(flux$ci - flux$cp, 0) / (4 * flux$ci + 8 * flux$cp)

  if (leaf$colim == 1) {

    aquad = leaf$colim_c3
    bquad = -(flux$ac + flux$aj)
    cquad = flux$ac * flux$aj
    proots = quadraticRoots(aquad,bquad,cquad)
    flux$ag = min(proots)
  } else{
    flux$ag = min(flux$ac, flux$aj)
  }

  flux$an = flux$ag - flux$rd

  return(flux)
}
