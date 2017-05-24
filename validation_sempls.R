#setwd("C:/Users/pspires/Documents/DA_PSM")
source("code.R")

library("semPLS")
library(readr)
library(readxl)

setwd("C:/Users/Asus/Documents/PLS_PATH_17/DA_PSM")
#setwd("C:/Users/pspires/Documents/DA_PSM")
innerm <- read_excel("sempls_models.xlsx", sheet = "INNERMODEL")
outerm <- read_excel("sempls_models.xlsx", sheet = "OUTERMODEL")

innerm <- as.matrix(innerm)
outerm <- as.matrix(outerm)

banksem <- read.csv("bank.csv")

banksem = banksem[, 2:length(banksem)]

banksem <- input.means(banksem)

banksem = normalize(banksem)

banksem <- as.data.frame(banksem)

result.pls <- plsm(banksem,innerm,outerm)

result.pls

<<<<<<< Updated upstream
ecsi <- sempls(model = result.pls, data = banksem, wscheme = "factorial", tol=0.0001)
=======
<<<<<<< Updated upstream
ecsi <- sempls(model = result.pls, data = banksem, wscheme = "factorial", tol=1e-4)
=======
<<<<<<< HEAD
ecsi <- sempls(model = result.pls, data = banksem, wscheme = "centroid", tol=1e-4)
=======
ecsi <- sempls(model = result.pls, data = banksem, wscheme = "factorial", tol=0.0001)
>>>>>>> master
>>>>>>> Stashed changes
>>>>>>> Stashed changes

