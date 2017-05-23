setwd("C:/Users/pspires/Documents/DA_PSM")
source("code.R")

library("semPLS")
library(readr)
library(readxl)

#setwd("C:/Users/Asus/Documents/PLS_PATH_17/DA_PSM")
setwd("C:/Users/pspires/Documents/DA_PSM")
inner.m <- read_excel("sempls_models.xlsx", sheet = "INNERMODEL")
outer.m <- read_excel("sempls_models.xlsx", sheet = "OUTERMODEL")

inner.m <- as.matrix(inner.m)
outer.m <- as.matrix(outer.m)

bank <- read.csv("bank.csv")

bank = bank[, 2:length(bank)]

bank <- input.means(bank)

bank = normalize(bank)

bank <- as.data.frame(bank)

res <- plsm(bank,inner.m,outer.m)

res

ecsi <- sempls(model = result.pls, data = bank, wscheme = "factorial", tol=1e-4)

