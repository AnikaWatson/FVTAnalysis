library("orthopolynom2")

g <- function(x, m, s) {
  exp((-1/2)*((x-m)/s)^2)/(s*sqrt(2*pi))
}


psi <- function(n, x, m, s) {
  Herm <- (hermite.h.polynomials((n+1), normalized = FALSE)[n+1])
  Hermite.H <- function(x) {as.function(Herm)}
  (-1)^n*((Hermite.H(x)*g(x, m, s))/sqrt(factorial(n)*2^n))
}

psi(n, x, m, s)

