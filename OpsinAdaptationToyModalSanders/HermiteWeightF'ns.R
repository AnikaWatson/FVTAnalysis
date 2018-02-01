library("orthopolynom")
library("mpoly")

#Here we define the weight function that will normalize the Hermite function...
#... at leas that's what I think it does...
g <- function(x, m, s) {
  exp((-1/2)*((x-m)/s)^2)/(s*sqrt(2*pi))
}

#Here we define the Hermite function
psi <- function(n, x, m, s) {
  Herm <- (hermite((0:n), "h"))
  Hermite.H <- function(x) {as.function(Herm)(x)}
  inFun <- function(x, m, s) {
    (x-m)/s
  }
  (-1)^n*((Hermite.H(inFun(x, m, s))*g(x, m, s))/sqrt(factorial(n)*2^n))
}

#Here I have defined some numbers so that I can check the functions against those in Sanders'
#Mathematica code
n <- 3
m <- 0
s <- 1

psi(n, 5, m, s)

#Here I checked that I get the same integral as Sanders
integrand <- function(x) {
  psi(n, x, m, s)
}

integrate(integrand, 0, 1)

#The following code was manipulated from "the man"'s GitHub account
#It's almost the same as Sanders except that I have pplotted all of the Hermite functions 
#on one graph
r <- seq(-10, 20, length.out = 2001)

df <- data.frame(r, as.function(psi)(n, r, m, s))
names(df) <- c("x", paste0("T_", 0:n))
mdf <- gather(df, degree, value, -x)
qplot(x, value, data = mdf, geom = "line", color = degree, xlim = c(-5, 5))


