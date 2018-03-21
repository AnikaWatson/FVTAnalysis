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

#bump hunting
fit <- lm(sWavelength[,2] ~ dnorm(x = xVals, mean = i))


-----## Fourier ## ------
plot(inp, fft(RcticG1ind1))

plot(inp, fft(fft(RcticG1ind1, 20)))
plot(inp, RcticG1ind1)

Fourier <- fft(RcticG1ind1, 20)

Mag <- Mod(Fourier)

x.axis <- 1:length(Mag)/0.5

plot(x=x.axis, y=Mag)

plot(inp, Fourier)

for (i in 1:20) {
  Add <- c(FALSE, rep(TRUE, 19))
  curve((Re(Fourier[i])*cos(x)-Im(Fourier[i])*sin(x))*(Re(Fourier[i])*cos(x)+Im(Fourier[i])*sin(x)), add = Add[i], xlim = c(0, 100), ylim = c(-5, 40), col = i)
}

for (i in 1:20) {
  Add <- c(FALSE, rep(TRUE, 19))
  curve((Re(Fourier[i])*cos((i-1)*((0.5)/20)*x)), add = Add[i], xlim = c(0, 10), ylim = c(-10, 10), col = i)
}

fourier <- function(z, x) {
  for (i in 1:length(z)){
    temp[i] <- z[i]*cos((i-1)*((0.5)/20)*x)
  }
  sum(temp)
}

curve(fourier(Fourier, x), from = 0, to = 20, ylab = "")

curve(Re(Fourier[1])+Re(Fourier[2])*cos((1)*((0.5)/20)*x)+Re(Fourier[3])*cos((2)*((0.5)/20)*x)+Re(Fourier[4])*cos((3)*((0.5)/20)*x)+Re(Fourier[5])*cos((4)*((0.5)/20)*x)+Re(Fourier[6])*cos((5)*((0.5)/20)*x)+Re(Fourier[7])*cos((6)*((0.5)/20)*x)+Re(Fourier[8])*cos((7)*((0.5)/20)*x)+Re(Fourier[9])*cos((8)*((0.5)/20)*x)+Re(Fourier[10])*cos((9)*((0.5)/20)*x)+Re(Fourier[11])*cos((10)*((0.5)/20)*x)+Re(Fourier[12])*cos((11)*((0.5)/20)*x)+Re(Fourier[13])*cos((12)*((0.5)/20)*x)+Re(Fourier[14])*cos((13)*((0.5)/20)*x)+Re(Fourier[15])*cos((14)*((0.5)/20)*x)+Re(Fourier[16])*cos((15)*((0.5)/20)*x)+Re(Fourier[17])*cos((16)*((0.5)/20)*x)+Re(Fourier[18])*cos((17)*((0.5)/20)*x)+Re(Fourier[19])*cos((18)*((0.5)/20)*x)+Re(Fourier[20])*cos((19)*((0.5)/20)*x),
      from = 0, to = 20, ylab = "")

curve(Re(Fourier[1])+Re(Fourier[2])*cos((1)*2*pi*(5/100)*x)+Re(Fourier[3])*cos((2)*(2*pi**(5/100))*x)+Re(Fourier[4])*cos((3)*(2*pi**(5/100))*x)+Re(Fourier[5])*cos((4)*(2*pi**(5/100))*x)+Re(Fourier[6])*cos((5)*(2*pi**(5/100))*x)+Re(Fourier[7])*cos((6)*(2*pi**(5/100))*x)+Re(Fourier[8])*cos((7)*(2*pi**(5/100))*x)+Re(Fourier[9])*cos((8)*(2*pi**(5/100))*x)+Re(Fourier[10])*cos((9)*(2*pi**(5/100))*x)+Im(Fourier[2])*sin((1)*(2*pi**(5/100))*x)+Im(Fourier[3])*sin((2)*(2*pi**(5/100))*x)+Im(Fourier[4])*sin((3)*(2*pi**(5/100))*x)+Im(Fourier[5])*sin((4)*(2*pi**(5/100))*x)+Im(Fourier[6])*sin((5)*(2*pi**(5/100))*x)+Im(Fourier[7])*sin((6)*(2*pi**(5/100))*x)+Im(Fourier[8])*sin((7)*(2*pi**(5/100))*x)+Im(Fourier[9])*sin((8)*(2*pi**(5/100))*x)+Im(Fourier[10])*sin((9)*(2*pi**(5/100))*x),
      from = 0, to = 20, ylab = "")

curve(Re(Fourier[2])*cos((1)*(2*pi**(5/100))*x))

curve(Im(Fourier[10])*sin((9)*(2*pi**(5/100))*x))

spectrum(RcticG1ind1)

convert.fft <- function(cs, sample.rate=1) {
  cs <- cs / length(cs) # normalize
  
  distance.center <- function(c)signif( Mod(c),        4)
  angle           <- function(c)signif( 180*Arg(c)/pi, 3)
  
  df <- data.frame(cycle    = 0:(length(cs)-1),
                   freq     = 0:(length(cs)-1) * sample.rate / length(cs),
                   strength = sapply(cs, distance.center),
                   delay    = sapply(cs, angle))
  df
}

convert.fft(fft(RcticG1ind1, 20))


for (i in 1:20) {
  Add <- c(FALSE, rep(TRUE, 19))
  curve((Im(Fourier[i])*sin(x)), add = Add[i], xlim = c(0, 10), ylim = c(-2, 2), col = i)
}

periodogram(RcticG1ind1)

curve(sum((Re(Fourier)))*sin(x), add = FALSE, xlim = c(0, 10), ylim = c(-200, 200))

curve(sum((Re(Fourier))*(rep(sin(x), 20))), add = FALSE, xlim = c(0, 20), ylim = c(-2, 2))

curve(sum((Im(Fourier)))*sin(x), add = FALSE, xlim = c(0, 20), ylim = c(-2, 2))

plot((Re(Fourier)+(i*Im(Fourier)))*Fourier, spectrum)
