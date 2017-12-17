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
plot(MonoTest, ylab = "Individual Phenotype", xlab = "x", las = 1)
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
for (i in 1:10) {
  set.seed(1234*i)
  MonoError <- rnorm(n=20, mean=0, sd=0.04)
  IndiDev <- rnorm(n=3, mean=0, sd=0.2)
  Mono1akg[i,1] <- a1+IndiDev[1]
  Mono1akg[i,2] <- k1+IndiDev[2]
  Mono1akg[i,3] <- g1+IndiDev[3]
  for (l in 1:20){
    MonoData1[i,l] <- MonoGrowth((a1+IndiDev[1]), (k1+IndiDev[2]), (g1+IndiDev[3]), (l/2)) + MonoError[l]
  }
}

#Now we want to keep track of what a, k, and g were so let's make a table of these values.
Mono1akg <- data.frame(matrix(NA, nrow=100, ncol=3))



#Let's check this to make sure the data frame looks right
str(MonoData1)

# We'll likely be using these x-values a lot so let's make that an object
inp <- seq(from = 0.5, to = 10, by = 0.5)

#Now let's name the columns based on the x-value associated with each "observation"
colnames(MonoData1) <- inp

#let's plot this out to see if it worked
plot(inp, MonoData1[1,], ylab = "Individual Phenotype", xlab = "x", las = 1)
curve(MonoGrowth(Mono1akg[1,1], Mono1akg[1,2], Mono1akg[1,3], x), n = 101, 
      ylim=c(-0.5, 1.2), add = TRUE)

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
Mono2akg <- data.frame(matrix(NA, nrow=100, ncol=3))

a.all <- c(2,3,4)



for( par.n in 1:length(a.all)){
  
  functio.whatever(a.all[par.n], b.all[par.n], k.all[par.n])
  
}

a.all[par.n]


for (i in 1:10) {
  set.seed(1234*i)
  MonoError <- rnorm(n=20, mean=0, sd=0.04)
  IndiDev <- rnorm(n=3, mean=0, sd=0.2)
  for (l in 1:20){
    MonoData2[i,l] <- MonoGrowth(a2+IndiDev[1], k2+IndiDev[2], g2+IndiDev[3], (l/2)) + MonoError[l]
  }
  Mono2akg[i,1] <- a2+IndiDev[1]
  Mono2akg[i,2] <- k2+IndiDev[2]
  Mono2akg[i,3] <- g2+IndiDev[3]
}




str(MonoData2)

colnames(MonoData2) <- seq(from = 0.5, to = 10, by = 0.5)

#let's plot this out to see if it worked
plot(inp, MonoData2[1,], ylab = "Individual Phenotype", xlab = "x", las = 1)
curve(MonoGrowth(Mono2akg[1,1], Mono2akg[1,2], Mono2akg[1,3], x), n = 101, 
      ylim=c(-0.5, 1.2), add = TRUE)


write.table(MonoData2, paste(outputlocation, "MonoData2", sep = ""))


#Now let's compare these two datasets with a plot
plot(MonoTest, ylab = "Individual Phenotype", xlab = "x", las = 1)
curve(MonoGrowth(1, 1, 3, x), n = 101, 
      ylim=c(-0.5, 1.2), add = TRUE)



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


plot(CyclicTest,  ylab = "Individual Phenotype", xlab = "x", las = 1)
curve(Cyclic(1.5, 5, 1, x), n = 101, 
      ylim=c(-0.5, 1.2), add = TRUE)

#Data Generation
#population 1:
a1 <- 1.5
k1 <- 5
g1 <- 1

CyclicData1 <- data.frame(matrix(NA, nrow=100, ncol=20))
Cyclic1akg <- data.frame(matrix(NA, nrow=100, ncol=3))

for (i in 1:100) {
  set.seed(1234*i)
  CyclicError <- rnorm(n=20, mean=0, sd=0.04)
  IndiDev <- rnorm(n=3, mean=0, sd=0.2)
  for (l in 1:20){
    CyclicData1[i,l] <- Cyclic(a1+IndiDev[1], k1+IndiDev[2], 
                               g1+IndiDev[3], (l/2)) + CyclicError[l]
  }
  Cyclic1akg[i,1] <- a1+IndiDev[1]
  Cyclic1akg[i,2] <- k1+IndiDev[2]
  Cyclic1akg[i,3] <- g1+IndiDev[3]
}
str(CyclicData1)

colnames(CyclicData1) <- seq(from = 0.5, to = 10, by = 0.5)

write.table(CyclicData1, paste(outputlocation, "CyclicData1", sep = ""))

#population 2:
a1 <- 1.2
k1 <- 5
g1 <- 1.3


CyclicData2 <- data.frame(matrix(NA, nrow=100, ncol=20))
Cyclic2akg <- data.frame(matrix(NA, nrow=100, ncol=3))

for (i in 1:100) {
  set.seed(1234*i)
  CyclicError <- rnorm(n=20, mean=0, sd=0.04)
  IndiDev <- rnorm(n=3, mean=0, sd=0.2)
  for (l in 1:20){
    CyclicData2[i,l] <- Cyclic(a2+IndiDev[1], k2+IndiDev[2], 
                               g2+IndiDev[3], (l/2)) + CyclicError[l]
  }
  Cyclic2akg[i,1] <- a1+IndiDev[1]
  Cyclic2akg[i,2] <- k1+IndiDev[2]
  Cyclic2akg[i,3] <- g1+IndiDev[3]
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


plot(UnimodalTest, ylab = "Individual Phenotype", xlab = "x", las = 1)
curve(Unimodal(1, 2, 4, x), n = 101, 
      ylim=c(-0.5, 1.2), add = TRUE)

#Data Generation
#population 1:
a1 <- 1
k1 <- 2
g1 <- 4

UnimodalData1 <- data.frame(matrix(NA, nrow=100, ncol=20))
Unimodal1akg <- data.frame(matrix(NA, nrow=100, ncol=3))

for (i in 1:100) {
  set.seed(1234*i)
  UnimodalError <- rnorm(n=20, mean=0, sd=0.04)
  IndiDev <- rnorm(n=3, mean=0, sd=0.2)
  for (l in 1:20){
    UnimodalData1[i,l] <- Unimodal(a1+IndiDev[1], k1+IndiDev[2], 
                                   g1+IndiDev[3], (l/2)) + UnimodalError[l]
  }
  Unimodal1akg[i,1] <- a1+IndiDev[1]
  Unimodal1akg[i,2] <- k1+IndiDev[2]
  Unimodal1akg[i,3] <- g1+IndiDev[3]
}
str(UnimodalData1)

colnames(UnimodalData1) <- seq(from = 0.5, to = 10, by = 0.5)

write.table(UnimodalData1, paste(outputlocation, "UnimodalData1", sep = ""))

#population 2:
a1 <- 1.3
k1 <- 2
g1 <- 4.3

UnimodalData2 <- data.frame(matrix(NA, nrow=100, ncol=20))
Unimodal2akg <- data.frame(matrix(NA, nrow=100, ncol=3))

for (i in 1:100) {
  set.seed(1234*i)
  UnimodalError <- rnorm(n=20, mean=0, sd=0.04)
  IndiDev <- rnorm(n=3, mean=0, sd=0.2)
  for (l in 1:20){
    UnimodalData2[i,l] <- Unimodal(a1+IndiDev[1], k1+IndiDev[2], 
                                   g1+IndiDev[3], (l/2)) + UnimodalError[l]
  }
  Unimodal2akg[i,1] <- a1+IndiDev[1]
  Unimodal2akg[i,2] <- k1+IndiDev[2]
  Unimodal2akg[i,3] <- g1+IndiDev[3]
}
str(UnimodalData2)

colnames(UnimodalData2) <- seq(from = 0.5, to = 10, by = 0.5)

write.table(UnimodalData2, paste(outputlocation, "UnimodalData2", sep = ""))

