#setwd("C:/Users/pspires/Documents/DA_PSM")
source("code.R")
source("input_means.R")
source("normalize.R")

library("semPLS")
library(readr)
library(readxl)

#setwd("C:/Users/Asus/Documents/PLS_PATH_17/DA_PSM")
setwd("C:/Users/pspires/Documents/DA_PSM")
innerm <- read_excel("sempls_models.xlsx", sheet = "INNERMODEL")
outerm <- read_excel("sempls_models.xlsx", sheet = "OUTERMODEL")

#innerm <- as.matrix(innerm)
#outerm <- as.matrix(outerm)

banksem <- read.csv("bank.csv")

banksem = banksem[, 2:length(banksem)]

banksem <- input.means(banksem)

banksem = normalize(banksem)

banksem <- as.data.frame(banksem)

result.pls <- plsm(banksem,innerm,outerm)

#result.pls

ecsi <- sempls(model = result.pls, data = banksem, wscheme = "factorial", tol=0.0001)

set.seed(123)
ecsiBoot <- bootsempls(ecsi, nboot=200, start="ones", verbose=TRUE)
summary(ecsiBoot, type="perc", level=0.95)

j=1

m.coef = matrix[0,34,5]

### para testar a nossa função
aapls <- function(bank,i) {
  model=advance.analytics.pls(bank[i,],inner.m,outer.m,,0.1)
  ow <- model$outer_weights[model$outer_weights!=0]
  pc <- model$path[model$path_coefficients!=0]

  coef <- c(ow,pc)

  m.coef[j,] = coef

  return(coef)
}

boot(bank, aapls, R = 5)


model=advance.analytics.pls(profdata,inner.m,outer.m,"Factor",0.000001)


