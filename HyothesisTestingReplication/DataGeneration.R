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
#We can make a vector of random measurement errors with a set standard deviation
set.seed(1234)
MonoError <- rnorm(n=20, mean=0, sd=0.04)

#Let's test this out by generating some data with the error and function defined above
MonoTest <- cbind(rep(NA, 20), rep(NA, 20))

for (i in 1:20) {
  MonoTest[i,1] <- (i/2)
  MonoTest[i,2] <- MonoGrowth(1, 1, 3, (i/2)) + MonoError[i]
}

#We can plot these data against the originl curve to see if we're on track
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
#Note that these are the average values for the population and not the values for each individual

#Now let's build a data frame for our data
MonoData1 <- data.frame(matrix(NA, nrow=100, ncol=20))


#Now we want to keep track of what a, k, and g are for each individual
#so let's make a table of these values.
Mono1akg <- data.frame(matrix(NA, nrow=100, ncol=3))

#Time to fill the data frame
for (i in 1:10) {
  set.seed(1234*i)
  MonoError <- rnorm(n=20, mean=0, sd=0.04) #measurement error
  IndiDev <- rnorm(n=3, mean=0, sd=0.2) #the individual's deviation from the population average
  Mono1akg[i,1] <- a1+IndiDev[1]
  Mono1akg[i,2] <- k1+IndiDev[2]
  Mono1akg[i,3] <- g1+IndiDev[3]
  for (l in 1:20){
    MonoData1[i,l] <- MonoGrowth((a1+IndiDev[1]), (k1+IndiDev[2]), (g1+IndiDev[3]), (l/2)) + MonoError[l]
  }
}

#Let's check that the data frame looks right
str(MonoData1)

#Before we plot this let's define our x-values
#We'll likely be using these x-values a lot so let's make them an object
inp <- seq(from = 0.5, to = 10, by = 0.5)

#Now let's name the columns based on the x-value associated with each "observation"
colnames(MonoData1) <- inp

#let's plot this out to see if it worked
plot(inp, MonoData1[1,], ylab = "Individual Phenotype", xlab = "x", las = 1)
curve(MonoGrowth(Mono1akg[1,1], Mono1akg[1,2], Mono1akg[1,3], x), n = 101, 
      ylim=c(-0.5, 1.2), add = TRUE)

#and finally, we can export these data to a file
#the advantage of doing this is that we can then input the data 
#and treat it as though it were collected from the wild
write.table(MonoData1, paste(outputlocation, "MonoData1", sep = ""))

#Now let's repeat this for the second population that differs slightly (by 0.3)
#from the first population in it's a and g values

#population 2:
a2 <- 1.3
k2 <- 1
g2 <- 2.7

#For reference:
#population 1:
#a1 <- 1
#k1 <- 1
#g1 <- 3

#Once again we need to build a data frame for our data
MonoData2 <- data.frame(matrix(NA, nrow=100, ncol=20))

#and one to keep track of what a, k, and g are for each individual in this population
Mono2akg <- data.frame(matrix(NA, nrow=100, ncol=3))


#Time to fill these frames
for (i in 1:10) {
  set.seed(1234*i)
  MonoError <- rnorm(n=20, mean=0, sd=0.04) #measurement error
  IndiDev <- rnorm(n=3, mean=0, sd=0.2) #individual deviation from population mean
  for (l in 1:20){
    MonoData2[i,l] <- MonoGrowth(a2+IndiDev[1], k2+IndiDev[2], g2+IndiDev[3], (l/2)) + MonoError[l]
  }
  Mono2akg[i,1] <- a2+IndiDev[1]
  Mono2akg[i,2] <- k2+IndiDev[2]
  Mono2akg[i,3] <- g2+IndiDev[3]
}



#Let's check that this is working
str(MonoData2)

#and label the columns with their x-values
#(in this case the x-values are the same for both populations but they need not be
#FVT analysis does not care what x-values we made observations at)
colnames(MonoData2) <- inp

#let's plot this out to see if it worked
plot(inp, MonoData2[1,], ylab = "Individual Phenotype", xlab = "x", las = 1)
curve(MonoGrowth(Mono2akg[1,1], Mono2akg[1,2], Mono2akg[1,3], x), n = 101, 
      ylim=c(-0.5, 1.2), add = TRUE)

#and once again we can export the data, and pretend it isn't artificial
write.table(MonoData2, paste(outputlocation, "MonoData2", sep = ""))


#Now let's compare these two populations with a plot
plot(1, type = "n", ylab = "Population Phenotype", xlab = "x", las = 1, xlim = c(0, 10), ylim = c(0, 1.4))
curve(MonoGrowth(a1, k1, g1, x), n = 101, 
      ylim=c(-0.5, 1.2), add = TRUE, col = "red")
curve(MonoGrowth(a2, k2, g2, x), n = 101, 
      ylim=c(-0.5, 1.2), add = TRUE, col = "blue")
legend(0, 1.4, legend = c("Polulation 1", "Population 2"), col=c("red", "blue"), lty=1, cex=0.8)


#------- Cyclic Expression Function ---------
#Here I define the cyclic expression function in terms of a (alpha in the paper), 
#k (kappa in the paper), and g (gamma in the paper)
Cyclic <- function(a, k, g, x) {
  ifelse((((2*pi)-g) <= x & x <= ((3*pi)-g)), k*sin(x+g), a*sin(x+g))
}

#Once again let's fiddle around with the values of a, k, and g until the function looks reasonable
curve(Cyclic(1.5, 5, 1, x), 0, 10, 101, ylab = "Individual Phenotype", xlab = "x")

#Now let's construct some random measurement error
#note that the measurement error for this cyclic function is greater that that of 
#the monotonous growth function with a standard deviation of 0.2 instead of 0.04
set.seed(1234)
CyclicError <- rnorm(n=20, mean=0, sd=0.2)

#Now lwt's construct a test dataset
#First we make an empty frame
CyclicTest <- cbind(rep(NA, 20), rep(NA, 20))

#Then we fill it with test data
for (i in 1:20) {
  CyclicTest[i,1] <- (i/2)
  CyclicTest[i,2] <- Cyclic(1.5, 5, 1, (i/2)) + CyclicError[i]
}

#Then we plot it
plot(CyclicTest,  ylab = "Individual Phenotype", xlab = "x", las = 1)
curve(Cyclic(1.5, 5, 1, x), n = 101, 
      ylim=c(-0.5, 1.2), add = TRUE)

#Time to generate data for an entire population
#First we will set the mean a, k, and g values for this population
#population 1:
a1 <- 1.5
k1 <- 5
g1 <- 1

#Next we will make an empty data frame for our data
CyclicData1 <- data.frame(matrix(NA, nrow=100, ncol=20))
#and an empty frae for the individual a, k, and g values
Cyclic1akg <- data.frame(matrix(NA, nrow=100, ncol=3))

#Now we fill them
for (i in 1:100) {
  set.seed(1234*i)
  CyclicError <- rnorm(n=20, mean=0, sd=0.04) #measurement error
  IndiDev <- rnorm(n=3, mean=0, sd=0.2) #Individual deviation from population mean
  for (l in 1:20){
    CyclicData1[i,l] <- Cyclic(a1+IndiDev[1], k1+IndiDev[2], 
                               g1+IndiDev[3], (l/2)) + CyclicError[l]
  }
  Cyclic1akg[i,1] <- a1+IndiDev[1]
  Cyclic1akg[i,2] <- k1+IndiDev[2]
  Cyclic1akg[i,3] <- g1+IndiDev[3]
}

#Let's make sure this data frame looks ok
str(CyclicData1)

#Let's label our columns with their corrosponding x-values
colnames(CyclicData1) <- inp

#and finally we can export this data to a file
write.table(CyclicData1, paste(outputlocation, "CyclicData1", sep = ""))

#Time to make cyclic data for a second population to compare with the first
#population 2:
a2 <- 1.2
k2 <- 5
g2 <- 1.3

#For Reference:
#population 1:
#a1 <- 1.5
#k1 <- 5
#g1 <- 1

#We start again with empty frames
#One for the data
CyclicData2 <- data.frame(matrix(NA, nrow=100, ncol=20))

#one for each individual's coefficients
Cyclic2akg <- data.frame(matrix(NA, nrow=100, ncol=3))

#Then we fill the data frames
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

#Label the columns
colnames(CyclicData2) <- inp

#Check structure
str(CyclicData2)

#Export data
write.table(CyclicData2, paste(outputlocation, "CyclicData2", sep = ""))

#Now let's compare these two populations with a plot
plot(1, type = "n", ylab = "Population Phenotype", xlab = "x", las = 1, xlim = c(0, 10), ylim = c(-2, 5))
curve(Cyclic(a1, k1, g1, x), n = 101, 
      ylim=c(-0.5, 1.2), add = TRUE, col = "red")
curve(Cyclic(a2, k2, g2, x), n = 101, 
      ylim=c(-0.5, 1.2), add = TRUE, col = "blue")
legend(0, 5, legend = c("Polulation 1", "Population 2"), col=c("red", "blue"), lty=1, cex=0.8)

#------- Unimodal Performance Function ---------
#Here I define the unimodal performance function in terms of a, k (kappa in the paper), 
#and g (gamma in the paper)
Unimodal <- function(a, k, g, x) {
  ifelse(x <= g, 
    exp(-a)*exp(-(a*x*(x-2*g))/(g^2)),
    exp(-k)*exp(-(k*x*(x-2*g))/(g^2)))
}
#Test plot to find appropriate coefficients
curve(Unimodal(1, 2, 4, x), 0, 10, 101, ylab = "Individual Phenotype", xlab = "x")

#manufacture measurement error
set.seed(1234)
UnimodalError <- rnorm(n=20, mean=0, sd=0.2)

#Construct test dataset
#Build empty frame
UnimodalTest <- cbind(rep(NA, 20), rep(NA, 20))

#fill frame
for (i in 1:20) {
  UnimodalTest[i,1] <- (i/2)
  UnimodalTest[i,2] <- Unimodal(1, 2, 4, (i/2)) + UnimodalError[i]
}

#Test plot
plot(UnimodalTest, ylab = "Individual Phenotype", xlab = "x", las = 1)
curve(Unimodal(1, 2, 4, x), n = 101, 
      ylim=c(-0.5, 1.2), add = TRUE)

#Data Generation
#population 1:
a1 <- 1
k1 <- 2
g1 <- 4

#Make empty frae for data
UnimodalData1 <- data.frame(matrix(NA, nrow=100, ncol=20))

#Make empty frame for individual a, k, and g values
Unimodal1akg <- data.frame(matrix(NA, nrow=100, ncol=3))

for (i in 1:100) {
  set.seed(1234*i)
  UnimodalError <- rnorm(n=20, mean=0, sd=0.2) #measurement error
  IndiDev <- rnorm(n=3, mean=0, sd=0.2) #Individual deviation from population mean
  for (l in 1:20){
    UnimodalData1[i,l] <- Unimodal(a1+IndiDev[1], k1+IndiDev[2], 
                                   g1+IndiDev[3], (l/2)) + UnimodalError[l]
  }
  Unimodal1akg[i,1] <- a1+IndiDev[1]
  Unimodal1akg[i,2] <- k1+IndiDev[2]
  Unimodal1akg[i,3] <- g1+IndiDev[3]
}

#Check structure for errors
str(UnimodalData1)

#Label columns
colnames(UnimodalData1) <- inp

#Export data
write.table(UnimodalData1, paste(outputlocation, "UnimodalData1", sep = ""))

#population 2:
a2 <- 1.3
k2 <- 2
g2 <- 4.3

#For reference:
#population 1:
#a1 <- 1
#k1 <- 2
#g1 <- 4

#Construct empty frame for the data
UnimodalData2 <- data.frame(matrix(NA, nrow=100, ncol=20))

#Construct empty frame for the coefficients
Unimodal2akg <- data.frame(matrix(NA, nrow=100, ncol=3))

#Fill frames
for (i in 1:100) {
  set.seed(1234*i)
  UnimodalError <- rnorm(n=20, mean=0, sd=0.2) #measurement error
  IndiDev <- rnorm(n=3, mean=0, sd=0.2) #
  for (l in 1:20){
    UnimodalData2[i,l] <- Unimodal(a1+IndiDev[1], k1+IndiDev[2], 
                                   g1+IndiDev[3], (l/2)) + UnimodalError[l]
  }
  Unimodal2akg[i,1] <- a1+IndiDev[1]
  Unimodal2akg[i,2] <- k1+IndiDev[2]
  Unimodal2akg[i,3] <- g1+IndiDev[3]
}

#Check structure for obvious issues
str(UnimodalData2)

#Label columns
colnames(UnimodalData2) <- seq(from = 0.5, to = 10, by = 0.5)

#export data
write.table(UnimodalData2, paste(outputlocation, "UnimodalData2", sep = ""))

#Now let's compare these two populations with a plot
plot(1, type = "n", ylab = "Population Phenotype", xlab = "x", las = 1, xlim = c(0, 10), ylim = c(0, 1))
curve(Unimodal(a1, k1, g1, x), n = 101, 
      ylim=c(-0.5, 1.2), add = TRUE, col = "red")
curve(Unimodal(a2, k2, g2, x), n = 101, 
      ylim=c(-0.5, 1.2), add = TRUE, col = "blue")
legend(5.6, 1.02, legend = c("Polulation 1", "Population 2"), col=c("red", "blue"), lty=1, cex=0.8)


