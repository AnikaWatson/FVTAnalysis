#---- Polynomial Regression -----
#Note, run DataGeneration.R before running this code

#Let's begin by looking at one of the individuals from the MonoData1 population
#Here we coerce the data we want into class: numeric
Mono1ind1 <- rep(NA, 20)
for (i in 1:20) {
  Mono1ind1[i] <- MonoData1[1,i]
}

as.numeric(MonoData1[1,])

#make the model
PolyModelTest1 <- lm(Mono1ind1 ~ poly(x = inp, n = 19))

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

#make the model

PolyModel2 <- data.frame(matrix(data=NA,nrow=4,ncol=10))

for (l in 1:10) {
  PolyModel2[l] <- coefficients(lm(Mono2ind[l,] ~ poly(x = inp, n = 3)))
}

class(coefficients(lm(Mono2ind[l,] ~ poly(x = inp, n = 3))))

#Now let's make the data frame of coefficients for the second MonoGrowth population
Mono2ind <- matrix(data=NA,nrow=10,ncol=20)

for (l in 1:10) {
  for (i in 1:20) {
    Mono2ind[l,i] <- MonoData2[l,i]
  }
}

#make the model

PolyModel2 <- data.frame(matrix(data=NA,nrow=4,ncol=10))

for (l in 1:10) {
  PolyModel2[l] <- coefficients(lm(Mono2ind[l,] ~ poly(x = inp, n = 3)))
}

class(coefficients(lm(Mono2ind[l,] ~ poly(x = inp, n = 3))))



library(orthopolynom)

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
ssp <- spectrum(Mono1ind1)  
per <- 1/ssp$freq[ssp$spec==max(ssp$spec)]
reslm <- lm(Mono1ind1 ~ sin(2*pi/per*inp)+cos(2*pi/per*inp))
reslm

rg <- diff(range(Mono1ind1))
plot(Mono1ind1~inp, ylim=c(min(Mono1ind1)-0.1*rg, max(Mono1ind1)+0.1*rg), las=1)
lines(fitted(reslm)~inp, col=2, lty=2, lwd = 2)   # dashed blue line is sin fit
curve(MonoGrowth(Mono1akg[1,1], Mono1akg[1,2], Mono1akg[1,3], x), n = 101, add = TRUE, col = "black")


# including 2nd harmonic really improves the fit
reslm2 <- lm(Mono1ind1 ~ sin(2*pi/per*inp)+cos(2*pi/per*inp)+sin(4*pi/per*inp)+cos(4*pi/per*inp))
summary(reslm2)
lines(fitted(reslm2)~t, col = "green", lwd = 3)


predicted.intervals <- predict(reslm2, data.frame(x=inp),interval='confidence',
                               level=0.99)
lines(inp,predicted.intervals[,2],col=4, lwd=1, lty=2)
lines(inp,predicted.intervals[,3],col=4, lwd=1, lty=2)
