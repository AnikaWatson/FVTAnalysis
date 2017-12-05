#---- Preliminaries ----
#opsin labels and Subscript[\[Lambda], max] values for their sensitivity curves*)

wd <- getwd()

#check if output folder already exists otherwise create one
folders <- "data_output"
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

#---- Definition of Opsin Sensitivity Curves ----

Aband1 <- function(x,L) {
  a <- (0.8795 + 0.0459*exp(-(L - 300.)^(2/11940.)))
  1.0/(exp(69.7*(a - (L/x))) + exp(28.*(0.922 - (L/x))) + 
         exp(-14.9*(1.104 - (L/x))) + 0.674)
}

Bband1 <- function(x,L) {
  m <- 189. + 0.315*L
  b <- -40.5 + 0.195*L
  0.26*exp(-((x - m)/b)^2)
}

Aband2 <- function(x,L) {
  l <- lmax/x
  a <- 0.8795 + 0.0268*exp((L - 665.)/40.7)
  A <- 62.7 + 1.834*exp((L - 625.)/54.2)
  1.0/(exp[A*(a - l)] + exp[20.85*(0.9101 - l)] + 
         exp[-10.37*(1.1123 - l)] + 0.5343)
}

Bband2 <- function(x,L) {
  m <- 216.7 + 0.287*L
  b <- 317. - 1.149*L + 0.00124*L^2
  0.37*exp(-((x - m)/b)^2)
}


pdf(paste(outputPath, "OpsinSpectra.pdf", sep = ""))
curve(Aband1(x, lmax[1]) + Bband1(x, lmax[1]), la, lb, 101, ylab = "", xlab = "", col = "red")
par(new=TRUE)
curve(Aband1(x, lmax[2]) + Bband1(x, lmax[2]), la, lb, 101, ylab = "", xlab = "", col = "blue", axes = FALSE)
legend(199, 1, legend = opsins, col=c("red", "blue"), lty=1, cex=0.8)
dev.off()

