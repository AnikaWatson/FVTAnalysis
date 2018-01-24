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





