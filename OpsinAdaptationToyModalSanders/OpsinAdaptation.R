#---- Preliminaries----
#opsin labels and Subscript[\[Lambda], max] values for their sensitivity curves*)

wd <- getwd()

#check if output folder already exists otherwise create one
folders <- "OutputData"
if (file.exists(folders) == FALSE) {
  dir.create(file.path(wd, folders), showWarnings = FALSE) 
} else print("Output Folder Already exists")

#map folders to R structure
outputPath <- paste(wd, "/data_output/" , sep = "")
data.files <- list.files(outputPath)

opsins <- c("short wavelength", "long wavelenght")

lmax <- c(480., 620.)

nOpsin <- length(opsins)

nCoeff <- 2  #number of coefficients in Hermite series expansion

functions <- c("foton capture efficiency", "peak sensitivity", "sensitivity range")
nFunc <- length(functions)

#lower and upper limits of wavelenght in opsin sensitivity spectra
la <- 200
lb <- 800

Aband1 <- function(l,L) {
  a <- (0.8795 + 0.0459*exp(-(L - 300.)^(2/11940.)))
  1.0/(exp(69.7 (a - (L/l))) + exp(28. (0.922 - (L/l))) + 
         exp(-14.9 (1.104 - (L/l))) + 0.674)
}

Bband1 <- function(l,L) {
  m <- 189. + 0.315*L
  b <- -40.5 + 0.195*L
  0.26*exp(-((l - m)/b)^2)
}

Aband2 <- function(l,L) {
  x <- lmax/l
  a <- 0.8795 + 0.0268*exp((L - 665.)/40.7)
  A <- 62.7 + 1.834*exp((L - 625.)/54.2)
  1.0/(Exp[A (a - x)] + Exp[20.85 (0.9101 - x)] + 
         Exp[-10.37 (1.1123 - x)] + 0.5343)
}

Bband2 <- function(l,L) {
  m <- 216.7 + 0.287*L
  b <- 317. - 1.149*L + 0.00124*L^2
  0.37*exp(-((l - m)/b)^2)
}

opsinSpectra <- table(Aband1(l, lmax((i))) + Bband1(l, lmax((i))),  {i, 1, nOpsin})

for(i in 1:nOpsin) {
  
}
