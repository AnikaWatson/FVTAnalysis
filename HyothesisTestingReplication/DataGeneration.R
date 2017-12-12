#Here I attempt to reproduce the data from HYPOTHESIS TESTING IN COMPARATIVE AND 
#EXPERIMENTAL STUDIES OF FUNCTION-VALUED TRAITS
#Referred to in this script as "HypothesisTesting"
#first let's set up an output folder for the data that we produce

wd <- getwd()

#check if output folder already exists otherwise create one
DataOut <- "data_output"
if (file.exists(DataOut) == FALSE) {
  dir.create(file.path(wd, folders), showWarnings = FALSE) 
} else print("Output Folder Already exists")

#map folders to R structure
outputlocation <- paste(wd, "/data_output/" , sep = "")
data.files <- list.files(outputlocation)


#------- Monotonic Growth Function ---------
#Here I define the monotonic growth function in terms of a (alpha in the paper), 
#k (kappa in the paper), and g (gamma in the paper)
MonoGrowth <- function(a, k, g, x) {
  a*exp(-exp(-k*(x-g)))-exp(-exp(-k*(-g)))
}

#Now let's plot this and fiddle around with the parameters a, k, and g...
#until we get some reasonable-looking function.
curve(MonoGrowth(1, 1, 3, x), 0, 10, 101, ylab = "Individual Phenotype", xlab = "x")

#Now let's account for our measurement error
set.seed(1234)
MonoError <- rnorm(n=20, mean=0, sd=0.04)

#Let's test this out by generating some data with the error and function defined above
MonoTest <- cbind(rep(NA, 20), rep(NA, 20))

for (i in 1:20) {
  MonoTest[i,1] <- (i/2)
  MonoTest[i,2] <- MonoGrowth(1, 1, 3, (i/2)) + MonoError[i]
}

#We can plot these data against the originl curve to see how we're doing
plot(MonoTest, ylab = "Individual Phenotype", xlab = "x")
curve(MonoGrowth(1, 1, 3, x), n = 101, 
      ylim=c(-0.5, 1.2), add = TRUE)

#Now that we've tested this out we're ready to generate some real data!

#Data Generation
#First let's define the underlying function of the first group of individuals
#population 1:
a1 <- 1
k1 <- 1
g1 <- 3

#Now let's build a data frame for our data
MonoData1 <- data.frame(matrix(NA, nrow=100, ncol=20))

#Time to fill the data frame
for (i in 1:100) {
  set.seed(1234*i)
  MonoError <- rnorm(n=20, mean=0, sd=0.04)
  IndiDev <- rnorm(n=3, mean=0, sd=0.2)
  for (l in 1:20){
    MonoData1[i,l] <- MonoGrowth((a1+IndiDev[1]), (k1+IndiDev[2]), (g1+IndiDev[3]), (l/2)) 
    + MonoError[l]
  }
}
#Let's check this to make sure the data frame looks right
str(MonoData1)

#Now let's name the columns based on the x-value associated with each "observation"
colnames(MonoData1) <- seq(from = 0.5, to = 10, by = 0.5)

#and finally, we can export these data to a file
write.table(MonoData1, paste(outputlocation, "MonoData1", sep = ""))

#Now let's repeat this for the second population that differs slightly (by 0.3)
#from the first in it's a and g values

#population 2:
a2 <- 1.3
k2 <- 1
g2 <- 2.7

#For reference:
#population 1:
#a1 <- 1
#k1 <- 1
#g1 <- 3

MonoData2 <- data.frame(matrix(NA, nrow=100, ncol=20))

for (i in 1:100) {
  set.seed(1234*i)
  MonoError <- rnorm(n=20, mean=0, sd=0.04)
  IndiDev <- rnorm(n=3, mean=0, sd=0.2)
  for (l in 1:20){
    MonoData2[i,l] <- MonoGrowth(a2+IndiDev[1], k2+IndiDev[2], g2+IndiDev[3], (l/2)) + MonoError[l]
  }
}
str(MonoData2)

colnames(MonoData2) <- seq(from = 0.5, to = 10, by = 0.5)

write.table(MonoData2, paste(outputlocation, "MonoData2", sep = ""))

#------- Cyclic Expression Function ---------
#Here I define the cyclic expression function in terms of a (alpha in the paper), 
#k (kappa in the paper), and g (gamma in the paper)
Cyclic <- function(a, k, g, x) {
  if(((2*pi)-g) > x){
    a*sin(x+g)
  } else if(x > (3*pi)-g) {
    a*sin(x+g)
  } else {
    k*sin(x+g)
  }
}

Cyclic <- function(a, k, g, x) {
  ifelse((((2*pi)-g) <= x & x <= ((3*pi)-g)), k*sin(x+g), a*sin(x+g))
}

curve(Cyclic(1.5, 5, 1, x), 0, 10, 101, ylab = "Individual Phenotype", xlab = "x")

#error
set.seed(1234)
CyclicError <- rnorm(n=20, mean=0, sd=0.2)

CyclicTest <- cbind(rep(NA, 20), rep(NA, 20))


for (i in 1:20) {
  CyclicTest[i,1] <- (i/2)
  CyclicTest[i,2] <- Cyclic(1.5, 5, 1, (i/2)) + CyclicError[i]
}


plot(CyclicTest,  ylab = "Individual Phenotype", xlab = "x")
curve(Cyclic(1.5, 5, 1, x), n = 101, 
      ylim=c(-0.5, 1.2), add = TRUE)

#Data Generation
#population 1:
a1 <- 1.5
k1 <- 5
g1 <- 1

CyclicData1 <- data.frame(matrix(NA, nrow=100, ncol=20))

for (i in 1:100) {
  set.seed(1234*i)
  CyclicError <- rnorm(n=20, mean=0, sd=0.04)
  IndiDev <- rnorm(n=3, mean=0, sd=0.2)
  for (l in 1:20){
    CyclicData1[i,l] <- Cyclic(a1+IndiDev[1], k1+IndiDev[2], g1+IndiDev[3], (l/2)) + CyclicError[l]
  }
}
str(CyclicData1)

colnames(CyclicData1) <- seq(from = 0.5, to = 10, by = 0.5)

write.table(CyclicData1, paste(outputlocation, "CyclicData1", sep = ""))

#population 2:
a1 <- 1.2
k1 <- 5
g1 <- 1.3


CyclicData2 <- data.frame(matrix(NA, nrow=100, ncol=20))

for (i in 1:100) {
  set.seed(1234*i)
  CyclicError <- rnorm(n=20, mean=0, sd=0.04)
  IndiDev <- rnorm(n=3, mean=0, sd=0.2)
  for (l in 1:20){
    CyclicData2[i,l] <- Cyclic(a2+IndiDev[1], k2+IndiDev[2], g2+IndiDev[3], (l/2)) + CyclicError[l]
  }
}

colnames(CyclicData2) <- seq(from = 0.5, to = 10, by = 0.5)

str(CyclicData2)

write.table(CyclicData2, paste(outputlocation, "CyclicData2", sep = ""))

#population 2:
a2 <- 1.2
k2 <- 1
g2 <- 1.3

#------- Unimodal Performance Function ---------
#Here I define the unimodal performance function in terms of a, k (kappa in the paper), 
#and g (gamma in the paper)

Unimodal <- function(a, k, g, x) {
  ifelse(x <= g, 
    exp(-a)*exp(-(a*x*(x-2*g))/(g^2)),
    exp(-k)*exp(-(k*x*(x-2*g))/(g^2)))
}

curve(Unimodal(1, 2, 4, x), 0, 10, 101, ylab = "Individual Phenotype", xlab = "x")

#error
set.seed(1234)
UnimodalError <- rnorm(n=20, mean=0, sd=0.2)

UnimodalTest <- cbind(rep(NA, 20), rep(NA, 20))


for (i in 1:20) {
  UnimodalTest[i,1] <- (i/2)
  UnimodalTest[i,2] <- Unimodal(1, 2, 4, (i/2)) + UnimodalError[i]
}


plot(UnimodalTest, ylab = "Individual Phenotype", xlab = "x")
curve(Unimodal(1, 2, 4, x), n = 101, 
      ylim=c(-0.5, 1.2), add = TRUE)

#Data Generation
#population 1:
a1 <- 1
k1 <- 2
g1 <- 4

UnimodalData1 <- data.frame(matrix(NA, nrow=100, ncol=20))

for (i in 1:100) {
  set.seed(1234*i)
  UnimodalError <- rnorm(n=20, mean=0, sd=0.04)
  IndiDev <- rnorm(n=3, mean=0, sd=0.2)
  for (l in 1:20){
    UnimodalData1[i,l] <- Unimodal(a1+IndiDev[1], k1+IndiDev[2], g1+IndiDev[3], (l/2)) + UnimodalError[l]
  }
}
str(UnimodalData1)

colnames(UnimodalData1) <- seq(from = 0.5, to = 10, by = 0.5)

write.table(UnimodalData1, paste(outputlocation, "UnimodalData1", sep = ""))

#population 2:
a1 <- 1.3
k1 <- 2
g1 <- 4.3

UnimodalData2 <- data.frame(matrix(NA, nrow=100, ncol=20))

for (i in 1:100) {
  set.seed(1234*i)
  UnimodalError <- rnorm(n=20, mean=0, sd=0.04)
  IndiDev <- rnorm(n=3, mean=0, sd=0.2)
  for (l in 1:20){
    UnimodalData2[i,l] <- Unimodal(a1+IndiDev[1], k1+IndiDev[2], g1+IndiDev[3], (l/2)) + UnimodalError[l]
  }
}
str(UnimodalData2)

colnames(UnimodalData2) <- seq(from = 0.5, to = 10, by = 0.5)

write.table(UnimodalData2, paste(outputlocation, "UnimodalData2", sep = ""))

