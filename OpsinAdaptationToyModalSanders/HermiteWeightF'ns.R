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

psi(n, x, m, s)

#Here I checked that I get the same integral as Sanders
integrand <- function(x) {
  psi(n, x, m, s)
}

integrate(integrand, 0, 1)

#The following code was manipulated from "the man"'s GitHub account
#These are the polynomials in the n+1'th Hermite function

r <- seq(-10, 20, length.out = 2001)

df <- data.frame(r, as.function(psi)(n, r, m, s))
names(df) <- c("x", paste0("T_", 0:n))
mdf <- gather(df, degree, value, -x)
qplot(x, value, data = mdf, geom = "line", color = degree, xlim = c(-5, 5))



#Below I reproduce Sanders' plot of the basis Hermite functions
psi2 <- function(n, x, m, s) {
  Herm <- (hermite((n), "h"))
  Hermite.H <- function(x) {as.function(Herm)(x)}
  inFun <- function(x, m, s) {
    (x-m)/s
  }
  (-1)^n*((Hermite.H(inFun(x, m, s))*g(x, m, s))/sqrt(factorial(n)*2^n))
}

df1 <- data.frame(r, as.function(psi2)(0, r, m, s))
names(df1) <- c("x", paste0("T_", 0))
mdf1 <- gather(df1, degree, value, -x)

df2 <- data.frame(r, as.function(psi2)(1, r, m, s))
names(df2) <- c("x", paste0("T_", 1))
mdf2 <- gather(df2, degree, value, -x)

df3 <- data.frame(r, as.function(psi2)(2, r, m, s))
names(df3) <- c("x", paste0("T_", 2))
mdf3 <- gather(df3, degree, value, -x)

df4 <- data.frame(r, as.function(psi2)(3, r, m, s))
names(df4) <- c("x", paste0("T_", 3))
mdf4 <- gather(df4, degree, value, -x)


p1 <- qplot(x, value, data = mdf1, geom = "line", color = degree, xlim = c(-5, 5))
p2 <- qplot(x, value, data = mdf2, geom = "line", color = degree, xlim = c(-5, 5))
p3 <- qplot(x, value, data = mdf3, geom = "line", color = degree, xlim = c(-5, 5))
p4 <- qplot(x, value, data = mdf4, geom = "line", color = degree, xlim = c(-5, 5))

install.packages(gridExtra)
library("gridExtra")
grid.arrange(p1, p2, p3, p4, nrow = 2)




