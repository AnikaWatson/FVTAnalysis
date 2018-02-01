#Hermite Weight Functions

psi <- function(n, x, m, s) {
  Herm <- (hermite.h.polynomials((n+1), normalized = FALSE)[n+1])
  Hermite.H <- function(x) {as.function(Herm)}
  (-1)^n*((Hermite.H(x)*g(x, m, s))/sqrt(factorial(n)*2^n))
}

str(Herm)
tt <- unlist(Herm)
toString((Herm))
polynomial.powers(Herm)
polies <- c(1, x, x^2, x^3, x^4, x^5)

phi*polies

psi(n, x, m, s)

Mite <- hermite(n, "h")

class(Mite)
as.function.m
as.function(Mite)

library(ggplot2); theme_set(theme_classic())
library(tidyr)
s <- seq(-3, 3, length.out = 201)
N <- 5 # number of hermite polynomials to plot
(hermPolys <- hermite(0:N, "h"))

df <- data.frame(s, as.function(hermPolys)(s))
names(df) <- c("x", paste0("T_", 0:N))
mdf <- gather(df, degree, value, -x)
qplot(x, value, data = mdf, geom = "line", color = degree, xlim = c(-10, 10))




#Trying to do four panel plot:

df1 <- data.frame(r, as.function(psi2)(1, r, m, s))
names(df1) <- c("x", paste0("T_", 1))
mdf1 <- gather(df1, degree, value, -x)

df2 <- data.frame(r, as.function(psi2)(2, r, m, s))
names(df2) <- c("x", paste0("T_", 2))
mdf2 <- gather(df2, degree, value, -x)

df3 <- data.frame(r, as.function(psi2)(3, r, m, s))
names(df3) <- c("x", 3)
mdf3 <- gather(df3, degree, value, -x)

df4 <- data.frame(r, as.function(psi2)(4, r, m, s))
names(df4) <- c("x", paste0("T_", 4))
mdf4 <- gather(df4, degree, value, -x)


par(mfrow=c(2,2))

layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
qplot(x, value, data = mdf1, geom = "line", color = degree, xlim = c(-5, 5))
qplot(x, value, data = mdf2, geom = "line", color = degree, xlim = c(-5, 5))
qplot(x, value, data = mdf3, geom = "line", color = degree, xlim = c(-5, 5))
qplot(x, value, data = mdf4, geom = "line", color = degree, xlim = c(-5, 5))

psi2 <- function(n, x, m, s) {
  Herm <- (hermite((n), "h"))
  Hermite.H <- function(x) {as.function(Herm)(x)}
  inFun <- function(x, m, s) {
    (x-m)/s
  }
  (-1)^n*((Hermite.H(inFun(x, m, s))*g(x, m, s))/sqrt(factorial(n)*2^n))
}
