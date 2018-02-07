#Bump Hunting

xVals <- seq(201, 800, 1)

sWavelength <- cbind(rep(NA, 600), rep(NA, 600))

for (i in 200:800) {
  sWavelength[(i-200),1] <- (i)
  sWavelength[(i-200),2] <- Aband1(i, lmax[1]) + Bband1(i, lmax[1])
}

sBHunt <- cbind(rep(NA, 600), rep(NA, 600))


for (i in 200:800) {
  sBHunt[(i-200),1] <- (i)
  fit <- nls(y ~ a*(psi2)(0, xVals, i, b), data = as.data.frame(sWavelength[,2]), start = list(a=1, b=1))
  sBHunt[(i-200),2] <- as.numeric(chisq.test(x = sWavelength, p = fit)[1])
}

plot(sBHunt)

lWavelength <- cbind(rep(NA, 600), rep(NA, 600))

for (i in 200:800) {
  lWavelength[i,1] <- (i)
  lWavelength[i,2] <- Aband1(i, lmax[2]) + Bband1(i, lmax[2])
}

lm(lWavelength ~ dnorm(x = xVals, mean = i))

chisq.test()