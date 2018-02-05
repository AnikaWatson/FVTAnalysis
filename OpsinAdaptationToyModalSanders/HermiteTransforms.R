#Hermite Transforms
HermiteTransfor <- function(n, m, spec, la, lb){
  integrate(psi2(n, x, m, s)*spec[x], la, lb)
}

invHermiteTransform <- function(l, m, s, hList)