#---- Polynomial Regression -----
#Note, run DataGeneration.R before running this code

#Let's begin by looking at one of the individuals from the MonoData1 population
#Here we coerce the data we want into class: numeric
Mono1ind1 <- rep(NA, 20)
for (i in 1:20) {
  Mono1ind1[i] <- MonoData1[1,i]
}

#make the model
PolyModelTest1 <- lm(Mono1ind1 ~ poly(x = inp, n = 3))

#calculate confidence intervals
confint(PolyModelTest1, level=0.95)

#Let's take a look at a residuals versus fits plot
plot(fitted(PolyModelTest1), residuals(PolyModelTest1))

#let's get the 99% confidence intervals of the model
predicted.intervals <- predict(PolyModelTest1, data.frame(x=inp),interval='confidence',
                               level=0.99)

#Now we can plot the original data and it's curve with the curves predicted by the model
plot(y = MonoData1[1,], x = inp, ylab = "Individual Phenotype", xlab = "x", las = 1)
curve(MonoGrowth(Mono1akg[1,1], Mono1akg[1,2], Mono1akg[1,3], x), n = 101, add = TRUE, col = "black")
lines(inp,predicted.intervals[,1],col='green', lwd=3)
lines(inp,predicted.intervals[,2],col=4, lwd=1, lty=2)
lines(inp,predicted.intervals[,3],col=4, lwd=1, lty=2)

#---- Now that we've modeled MonoData1 let's model MonoData2 and compare them----
Mono2ind1 <- rep(NA, 20)
for (i in 1:20) {
  Mono2ind1[i] <- MonoData2[1,i]
}

#make the model
model <- lm(Mono2ind1 ~ poly(x = inp, n = 3))

#calculate confidence intervals
confint(model, level=0.95)

#Let's take a look at a residuals versus fits plot
plot(fitted(model), residuals(model))

#let's get the 99% confidence intervals of the model
predicted.intervals <- predict(model, data.frame(x=inp),interval='confidence',
                               level=0.99)

#Now we can plot the original data and it's curve with the curves predicted by the model
plot(y = MonoData2[1,], x = inp, ylab = "Individual Phenotype", xlab = "x", las = 1)
curve(MonoGrowth(Mono2akg[1,1], Mono2akg[1,2], Mono2akg[1,3], x), n = 101, add = TRUE, col = "black")
lines(inp,predicted.intervals[,1],col='green', lwd=3)
lines(inp,predicted.intervals[,2],col=4, lwd=1, lty=2)
lines(inp,predicted.intervals[,3],col=4, lwd=1, lty=2)


#---- Now let's do this for a ten individual population ----

Mono1ind <- matrix(data=NA,nrow=10,ncol=20)

for (l in 1:10) {
  for (i in 1:20) {
    Mono1ind[l,i] <- MonoData1[l,i]
  }
}

PolyModel1 <- rep(NA, 10)

for (l in 1:10) {
  PolyModel1[l] <- lm(Mono1ind[l] ~ poly(x = inp, n = 3))
}

#make the model
PolyModel1 <- lm(Mono1ind1 ~ poly(x = inp, n = 3))

#---- Let's try another approach: Legendre ----
legendre.recurrences <- function( n, normalized=FALSE ) {
  ###
  ### This function returns a data frame with n+1 rows and four columns
  ### containing the coefficients c, d, e and f of the recurrence relations
  ### for the order k Legendre polynomial, Pk(x), and orders k=0,1,...,n
  ###
  ### Parameter
  ### n = integer highest polynomial order
  ### normalized = a boolean value.  if true, the recurrences are for normalized polynomials
  ###
  if ( n < 0 )
    stop( "negative highest polynomial order" )
  if ( n != round( n ) )
    stop( "highest polynomial order is not an integer" )
  np1 <- n + 1
  r <- data.frame( matrix( nrow=np1, ncol=4 ) )
  names( r ) <- c( "c", "d", "e", "f" )
  j <- 0
  k <- 1
  if ( normalized ) {
    while ( j <= n ) {
      r[k,"c"] <- j + 1
      r[k,"d"] <- 0
      r[k,"e"] <- sqrt( ( 2 * j + 1 ) * ( 2 * j + 3 ) )
      if ( j == 0 )
        r[k,"f"] <- 0
      else {
        r[k,"f"] <- j * sqrt( ( 2 * j + 3 ) / ( 2 * j - 1 ) )
      }
      j <- j + 1
      k <- k + 1
    }
    return( r )
  }
  else {
    while ( j <= n ) {
      r[k,"c"] <- j + 1
      r[k,"d"] <- 0
      r[k,"e"] <- 2 * j + 1
      r[k,"f"] <- j
      j <- j + 1
      k <- k + 1
    }
    return( r )
  }
  return( NULL )
}   

legendre.polynomials <- function( n, normalized=FALSE ){
  ###
  ###   This function returns a list with n+1 elements
  ###   containing the order k Legendre polynomials Pk(x),
  ###   for orders k=0,1,...,n
  ###
  ###   Parameters
  ###   n = integer highest polynomial order
  ###   normalized = boolean value.  if true, the polynomials are normalized
  ###
  recurrences <- legendre.recurrences( n, normalized )
  if ( normalized ) {
    h.0 <- 2
    p.0 <- polynomial( c( 1 / sqrt( h.0 ) ) )
    polynomials <- orthonormal.polynomials( recurrences, p.0 )
  }
  else
    polynomials <- orthogonal.polynomials( recurrences )
  return( polynomials )
}

Legendre <- function(x, n, normalized=TRUE, intercept=FALSE, rescale=TRUE)
{
  ## Create a design matrix for Legendre polynomials
  ## x - numeric
  ## n - see orthopolynom
  ## normalized - logical,
  ## intercept - logical, add intercept
  tmp <- legendre.polynomials(n=n, normalized=normalized)
  if(!intercept) tmp <- tmp[2:length(tmp)]
  polynomial.values(polynomials=tmp, x=x, matrix=TRUE)
}

LegendreModel <- lm(number ~ Legendre(x = inp, n=4))


#---- Sinusoidal stuff ----
#y <- c(11.622967, 12.006081, 11.760928, 12.246830, 12.052126, 12.346154, 12.039262, 12.362163, 12.009269, 11.260743, 10.950483, 10.522091,  9.346292,  7.014578,  6.981853,  7.197708,  7.035624,  6.785289, 7.134426,  8.338514,  8.723832, 10.276473, 10.602792, 11.031908, 11.364901, 11.687638, 11.947783, 12.228909, 11.918379, 12.343574, 12.046851, 12.316508, 12.147746, 12.136446, 11.744371,  8.317413, 8.790837, 10.139807,  7.019035,  7.541484,  7.199672,  9.090377,  7.532161,  8.156842,  9.329572, 9.991522, 10.036448, 10.797905)
#t <- 18:65
y <- number
t <- inp
ssp <- spectrum(Mono1ind1)  
per <- 1/ssp$freq[ssp$spec==max(ssp$spec)]
reslm <- lm(Mono1ind1 ~ sin(2*pi/per*inp)+cos(2*pi/per*inp))
reslm

rg <- diff(range(Mono1ind1))
plot(Mono1ind1~inp, ylim=c(min(Mono1ind1)-0.1*rg, max(Mono1ind1)+0.1*rg))
lines(fitted(reslm)~inp, col=2, lty=2)   # dashed blue line is sin fit
curve(MonoGrowth(Mono1akg[1,1], Mono1akg[1,2], Mono1akg[1,3], x), n = 101, add = TRUE, col = "black")


# including 2nd harmonic really improves the fit
reslm2 <- lm(Mono1ind1 ~ sin(2*pi/per*inp)+cos(2*pi/per*inp)+sin(4*pi/per*inp)+cos(4*pi/per*inp))
summary(reslm2)
lines(fitted(reslm2)~t, col = "green", lwd = 3)


predicted.intervals <- predict(reslm2, data.frame(x=inp),interval='confidence',
                               level=0.99)
lines(inp,predicted.intervals[,2],col=4, lwd=1, lty=2)
lines(inp,predicted.intervals[,3],col=4, lwd=1, lty=2)
