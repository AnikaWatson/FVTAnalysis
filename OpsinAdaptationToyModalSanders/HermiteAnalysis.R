Error <- rnorm(n=100, mean=0, sd=0.04)

LightData1 <- cbind(rep(NA, 100), rep(NA, 100))

for (i in 1:100) {
  LightData1[i,1] <- (200+6*i)
  LightData1[i,2] <- Aband1((200+6*i), lmax[1]) + Bband1((200+6*i), lmax[1]) + Error[i]
}

plot(LightData1, ylab = "Abundance", xlab = "Wavelength", las = 1)
curve(Aband1(x, lmax[1]) + Bband1(x, lmax[1]), la, lb, 101, ylab = "", xlab = "", col = "red", add = TRUE)

LightData <- rep(NA, 100)
for (i in 1:100) {
  LightData[i] <- LightData1[i,2]
}

LightDatax <- rep(NA, 100)
for (i in 1:100) {
  LightDatax[i] <- LightData1[i,1]
}

n <- 3
m <- 570
s <- 70

HermLm <- lm(LightData ~ as.function(psi2)(0, LightDatax, m, s)+as.function(psi2)(1, LightDatax, m, s)+as.function(psi2)(2, LightDatax, m, s))

plot(LightData1, ylab = "Abundance", xlab = "Wavelength", las = 1)
curve(Aband1(x, lmax[1]) + Bband1(x, lmax[1]), la, lb, 101, ylab = "", xlab = "", col = "red", add = TRUE)
lines(fitted(HermLm)~LightDatax, col = "green", lwd = 3)



