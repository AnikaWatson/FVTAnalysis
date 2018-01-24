#---- Preliminaries ----
#We'll start with some housekeeping
#get working directory
wd <- getwd()

#check if output folder already exists otherwise create one
folders <- "data_output"
if (file.exists(folders) == FALSE) {
  dir.create(file.path(wd, folders), showWarnings = FALSE) 
} else print("Output Folder Already exists")

#map folders to R structure
outputPath <- paste(wd, "/data_output/" , sep = "")
data.files <- list.files(outputPath)

#now we're ready to define some miscelaneous constants

#opsin labels and Lambda_max values for their sensitivity curves
opsins <- c("short wavelength", "long wavelenght")
lmax <- c(480., 620.)


nOpsin <- length(opsins) #number of opsins

nCoeff <- 2  #number of coefficients in Hermite series expansion

#make a vector of independent biological functions of the visual system that are 
#assumed to be the target of selection
functions <- c("foton capture efficiency", "peak sensitivity", "sensitivity range")
nFunc <- length(functions)

