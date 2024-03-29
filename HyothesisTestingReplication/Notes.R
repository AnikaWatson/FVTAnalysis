# Example 1
p <- 0.5
q <- seq(0,100,1)
y <- p*q
plot(q,y,type='l',col='red',main='Linear relationship')

# Example 2
y <- 450 + p*(q-10)^3
plot(q,y,type='l',col='navy',main='Nonlinear relationship',lwd=3)


#a.all <- c(2,3,4)



#for( par.n in 1:length(a.all)){

#  functio.whatever(a.all[par.n], b.all[par.n], k.all[par.n])

#}

#a.all[par.n]


#-------------------------------------------------------------------------------
# Polynomial Regression

# Always remember use to set.seed(n) when generating pseudo random numbers.
# By doing this, the random number generator generates always the same numbers.
set.seed(20)

# Predictor (q). Use seq for generating equally spaced sequences fast
q <- seq(from=0, to=20, by=0.1)

# Value to predict (y)
y <- 500 + 0.4 * (q-10)^3

# Some noise is generated and added to the real signal (y)
noise <- rnorm(length(q), mean=10, sd=80)
noisy.y <- y + noise

# Plot of the noisy signal
plot(q,noisy.y,col='deepskyblue4',xlab='q',main='Observed data')
lines(q,y,col='firebrick1',lwd=3)

model <- lm(noisy.y ~ poly(q,3))
summary(model)

# Uncomment the next line to check residual plots and other model plots
plot(model,3)

# Confidence intervales for model parameters
confint(model, level=0.95)

# Plot of fitted vs residuals
# No clear pattern should show in the residual plot if the model is a good fit
plot(fitted(model),residuals(model))

# Predicted values and confidence intervals
predicted.intervals <- predict(model,data.frame(x=q),interval='confidence',
                               level=0.99)

# Add lines to the existing plot
lines(q,predicted.intervals[,1],col='green',lwd=3)
lines(q,predicted.intervals[,2],col='black',lwd=1)
lines(q,predicted.intervals[,3],col='black',lwd=1)

# Add a legend
legend("bottomright",c("Observ.","Signal","Predicted"), 
       col=c("deepskyblue4","red","green"), lwd=3)





citEntry(entry="article",
         title = "A simple regression-based method to map quantitative trait loci underlying function-valued phenotypes",
         author = personList(person("Il-Youp", "Kwak"),
                             person(c("Candace", "R."), "Moore"),
                             person(c("Edgar", "P."), "Spalding"),
                             person(c("Karl", "W."), "Broman")),
         journal = "Genetics",
         year = 2014,
         volume = 197,
         pages = "1409-1416",
         key = "kwak2014",

         textVersion =
         paste("Kwak et al. (2014) A simple regression-based method to map",
               "quantitative trait loci underlying function-valued phenotypes.",
               "Genetics 197: 1409-1416")
         )


Broomstick <- rm(list = lsc)

#utilities file <- folder structure path structure etc.
#source("rscript")

#Ok, here's a crazy thought
#Let's use a for loop to generate all five hundred data sets.
#Population 1
a1 <- 1
k1 <- 1
g1 <- 3

#Population 2
a2 <- 1.3
k2 <- 1
g2 <- 2.7

#for (h in 1:50) {
MonoData1 <- data.frame(matrix(NA, nrow=100, ncol=20))

#Time to fill the data frame
for (i in 1:100) {
  set.seed(1234*i)
  MonoError <- rnorm(n=20, mean=0, sd=0.04)
  IndiDev <- rnorm(n=3, mean=0, sd=0.2)
  for (l in 1:20){
    MonoData1[i,l] <- MonoGrowth((a1+IndiDev[1]), (k1+IndiDev[2]), (g1+IndiDev[3]), (l/2)) + MonoError[l]
  }
}
#Let's check this to make sure the data frame looks right
str(MonoData1)

#Now let's name the columns based on the x-value associated with each "observation"
colnames(MonoData1) <- seq(from = 0.5, to = 10, by = 0.5)

#and finally, we can export these data to a file
write.table(MonoData1, paste(outputlocation, "MonoData1", sep = ""))


MonoData2 <- data.frame(matrix(NA, nrow=100, ncol=20))

for (i in 1:100) {
  set.seed(1234*i)
  MonoError <- rnorm(n=20, mean=0, sd=0.04)
  IndiDev <- rnorm(n=3, mean=0, sd=0.2)
  for (l in 1:20){
    MonoData2[i,l] <- MonoGrowth(a2+IndiDev[1], k2+IndiDev[2], 
                                 g2+IndiDev[3], (l/2)) + MonoError[l]
  }
}
str(MonoData2)

colnames(MonoData2) <- seq(from = 0.5, to = 10, by = 0.5)

write.table(MonoData2, paste(outputlocation, "MonoData2", sep = ""))

plot(MonoTest, ylab = "Individual Phenotype", xlab = "x", las = 1)
curve(MonoGrowth(1, 1, 3, x), n = 101, 
      ylim=c(-0.5, 1.2), add = TRUE)


#install packages "devtools" and "roxygen2"

install.packages("roxygen2")

install.packages("devtools")

library("devtools")

install_github("devtools", "hadley")
-------# R Markdown Code -------
---
  title: "My First R Markdown Report"
author: "Anika Watson"
date: '2018-01-10'
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:
  
  ```{r cars}
summary(cars)
```

## Including Plots

You can also embed plots, for example:
  
  ```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

install.packages("scales")
library("scales")

install.packages("renderPlot")


library(shiny)
runExample("05_sliders")



TrigModel <- function (x, m, n){
  trigfun <- rep(NA, n)
  for (i in 1:n) {
    trigfun[i] <- sin((2*i)*pi/period*x)+cos((2*i)*pi/period*x)
  }
  lm(m ~ sum(trigfun))
}
install.packages("hermite")
library("hermite")
data.c <- c(rep(0,122), rep(1,40), rep(2,14), rep(3,16), rep(4,6), rep(5,2))
data.c
mle1 <- glm.hermite(data.c~1, link="log", start=NULL, m=3)
summary(mle1)
str(mle1)

install.packages("phylocurve")
library("phylocurve")



plot(data.c)
points(mle1$fitted.values)
mean(data.c)

#cool plot

```{r, echo=FALSE}
sliderInput("numb", label = "Polynomial Degree", min = 1, max = 19, value = 6, step = 1, animate = T)
```

```{r, echo=FALSE}

renderPlot({
  PolyModelPop1ind1 <- lm(RcticChR1ind1 ~ poly(x = inp, n = input$numb))
  predicted.intervals <- predict(PolyModelPop1ind1, data.frame(x=inp), interval='confidence', level=0.99)
  
  plot(y = RcticChR1ind1, x = xvalues, main = "Polynomial Regression", ylab = "Length (m)", xlab = "Age (months)", las = 1)
  curve(RcticChR(Mono1akg[1,1], Mono1akg[1,2], Mono1akg[1,3], x), n = 101, add = TRUE, col = "red")
  lines(inp,predicted.intervals[,1],col='green', lwd=3)
  lines(inp,predicted.intervals[,2],col=4, lwd=1, lty=2)
  lines(inp,predicted.intervals[,3],col=4, lwd=1, lty=2)
  legend("bottomright",c("Data", "True Fun.", "Predicted Fun.", "99% Conf. Int."), col = c("black", "red", "green", 4), pch = c(1, NA, NA, NA), lwd = c(NA, 1, 3, 1), lty = c(1, 1, 1, 2))
})

```


Curves <- c(0.67789, 0.67789 + 1.63443*x, 0.67789 + 1.63443*x - 0.58209*x^2, 0.67789 + 1.63443*x - 0.58209*x^2 - 0.07315*x^3, 0.67789 + 1.63443*x - 0.58209*x^2 - 0.07315*x^3 + 0.26519*x^4, 0.67789 + 1.63443*x - 0.58209*x^2 - 0.07315*x^3 + 0.26519*x^4 - 0.02839*x^5, 0.67789 + 1.63443*x - 0.58209*x^2 - 0.07315*x^3 + 0.26519*x^4 - 0.02839*x^5 + 0.03007*x^6)

Curve0 <- function(x) {
  0.67789 + 0*x
}

Curve1 <- function(x) {
  0.67789 + 1.63443*x
}

Curve2 <- function(x) {
  0.67789 + 1.63443*x - 0.58209*x^2
}

Curve3 <- function(x) {
  0.67789 + 1.63443*x - 0.58209*x^2 - 0.07315*x^3
}

Curve4 <- function(x) {
  0.67789 + 1.63443*x - 0.58209*x^2 - 0.07315*x^3 + 0.26519*x^4
}

Curve5 <- function(x) {
  0.67789 + 1.63443*x - 0.58209*x^2 - 0.07315*x^3 + 0.26519*x^4 - 0.02839*x^5
}

Curve6 <- function(x) {
  0.67789 + 1.63443*x - 0.58209*x^2 - 0.07315*x^3 + 0.26519*x^4 - 0.02839*x^5 + 0.03007*x^6
}

Curve6 <- function(x) {
  PolyModelPop1ind1$coefficients[1] + PolyModelPop1ind1$coefficients[2]*x + PolyModelPop1ind1$coefficients[3]*x^2 + PolyModelPop1ind1$coefficients[4]*x^3 + PolyModelPop1ind1$coefficients[5]*x^4 + PolyModelPop1ind1$coefficients[6]*x^5 + PolyModelPop1ind1$coefficients[7]*x^6
}

PolyModelPop1ind1$coefficients[1]

Curvez <- as.list(Curve0(x), Curve1(x), Curve2(x), Curve3(x), Curve4(x), Curve5(x), Curve6(x))

arch <- 6

plot(1, type="n", xlab="", ylab="", xlim = c(0, 10), ylim = c(-0.5, 1.5))
curve(get(paste("Curve", arch, sep = ""))(x), xlim = c(-1, 11), ylim = c(-1, 2), add = TRUE, col = 1)


```{r, echo=FALSE}
plot(1, type="n", xlab="", ylab="", xlim = c(0, 10), ylim = c(-50, 75))
curve(1.63443*x, xlim = c(-1, 11), ylim = c(-55, 55), add = TRUE, col = 1)
curve(-0.58209*x^2, to = 10, xlim = c(-1, 11), ylim = c(-55, 55), add = TRUE, col = 2)
curve(-0.07315*x^3, to = 10, xlim = c(-1, 11), ylim = c(-55, 55), add = TRUE, col = 3)
curve(0.26519*x^4, to = 10, xlim = c(-1, 11), ylim = c(-55, 55), add = TRUE, col = 4)
curve(-0.02839*x^5, to = 10, xlim = c(-1, 11), ylim = c(-55, 55), add = TRUE, col = 5)
curve(0.03007*x^6, to = 10, xlim = c(-1, 11), ylim = c(-55, 55), add = TRUE, col = 6)
abline(h=0.67789,col= "orange")
legend("topright", c("Constant", "x", as.expression(bquote( ~ x^2 ~ "")), as.expression(bquote( ~ x^3 ~ "")), as.expression(bquote( ~ x^4 ~ "")), as.expression(bquote( ~ x^5 ~ "")), as.expression(bquote( ~ x^6 ~ ""))), col = c("orange", 1, 2, 3, 4, 5, 6), lty = c(1, 1, 1, 1, 1))
```


Gauss <- function(x) {
  1.07*exp((-(x^2)) / (2*(40^2)))
}

Sine <- function(x) {
  (1.07/2)+(1.07/2)*cos(2*pi*(x)/180)
}

SolDataY <- c(1.07, 1, .92, .82, .67, .48, .35, .16, .06, .02)

SolDataX <- seq(0, 90, 10)

plot(SolDataX, SolDataY, xlim = c(0, 90), add = TRUE)
curve(Gauss(x), from = 0, to = 90, add = TRUE)

plot(SolDataX, SolDataY, xlim = c(0, 90), add = TRUE)
curve(Sine(x), from = 0, to = 90, add = TRUE)


curve(Sine(x), from = 0, to = 90)

t <- SolDataX
ssp <- spectrum(SolDataY)  
per <- 1/ssp$freq[ssp$spec==max(ssp$spec)]
reslm <- lm(SolDataY ~ sin(2*pi/per*SolDataX)+cos(2*pi/per*SolDataX))
reslm

rg <- diff(range(SolDataY))
plot(SolDataY~SolDataX, ylim=c(min(SolDataY)-0.1*rg, max(SolDataY)+0.1*rg), las=1)
lines(fitted(reslm)~SolDataX, col=2, lty=2, lwd = 2)   # dashed blue line is sin fit
curve(MonoGrowth(Mono1akg[1,1], Mono1akg[1,2], Mono1akg[1,3], x), n = 101, add = TRUE, col = "black")


# including 2nd harmonic really improves the fit
reslm2 <- lm(SolDataY ~ sin(2*pi/per*SolDataX)+cos(2*pi/per*SolDataX)+sin(4*pi/per*SolDataX)+cos(4*pi/per*SolDataX))
summary(reslm2)
lines(fitted(reslm2)~SolDataX, col = "green", lwd = 3)


predicted.intervals <- predict(reslm2, data.frame(x=SolDataX),interval='confidence',
                               level=0.99)
lines(SolDataX,predicted.intervals[,2],col=4, lwd=1, lty=2)
lines(SolDataX,predicted.intervals[,3],col=4, lwd=1, lty=2)

plot(fft(Mono1ind1))

###----- y -------

library("orthopolynom")
library("mpoly")
library("ggplot2")
library("gridExtra")

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

(hermite((0), "h"))

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


grid.arrange(p1+ theme(panel.background = element_rect(fill = 'white', colour = 'white')), p2+ theme(panel.background = element_rect(fill = 'white', colour = 'white')), p3+ theme(panel.background = element_rect(fill = 'white', colour = 'white')), p4+ theme(panel.background = element_rect(fill = 'white', colour = 'white')), nrow = 2)


HermLm <- lm(Mono1ind1 ~ as.function(psi2)(0, inp, m, s)+as.function(psi2)(1, inp, m, s)+as.function(psi2)(2, inp, m, s)+as.function(psi2)(3, inp, m, s))
plot(Mono1ind1~inp, ylim=c(min(Mono1ind1)-0.1*rg, max(Mono1ind1)+0.1*rg), las=1)
curve(MonoGrowth(Mono1akg[1,1], Mono1akg[1,2], Mono1akg[1,3], x), n = 101, add = TRUE, col = "black")
lines(fitted(HermLm)~inp, col = "green", lwd = 3)

