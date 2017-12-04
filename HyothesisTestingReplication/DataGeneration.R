#Here I attempt to reproduce the data from HYPOTHESIS TESTING IN COMPARATIVE AND EXPERIMENTAL STUDIES OF FUNCTION-VALUED TRAITS
#Referred to in this script as "HypothesisTesting"
#first let's set up an output folder for the data that we produce

wd <- getwd()

#check if output folder already exists otherwise create one
folders <- "data_output"
if (file.exists(folders) == FALSE) {
  dir.create(file.path(wd, folders), showWarnings = FALSE) 
} else print("Output Folder Already exists")

#map folders to R structure
outputlocation <- paste(wd, "/data_output/" , sep = "")
data.files <- list.files(outputlocation)


#------- Monotonic Growth Function ---------

MonoGrowth <- function(a, k, y, x) {
  a*exp(-exp(-k*(x-y)))-exp(-exp(-k*(-y)))
}

#------- Cyclic Expression Function ---------

Cyclic <- function(a, k, y, x) {
  if((2*pi)-y > x){
    a*sin(x+y)
  } else if(x > (3*pi)-y) {
    a*sin(x+y)
  } else {
    k*sin(x+y)
  }
}

#------- Unimodal Performance Function ---------

Unimodal <- function(a, k, y, x) {
  if(x <= y) {
    exp(-a)*exp(-(a*x*(x-2*y))/(y^2))
  } else {
    exp(-k)*exp(-(k*x*(x-2*y))/(y^2))
  }
}





