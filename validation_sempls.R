#setwd("C:/Users/pspires/Documents/DA_PSM")
source("code.R")
source("input_means.R")
source("normalize.R")

library("semPLS")
library(readr)
library(readxl)
<<<<<<< HEAD
source("pathcoef.r")
source("LV_Conections.R")
source("weights_schemas.R")
source("input_means.r")
source("w_matrix.r")
source("update_weights.r")
source("normalize.r")
source("normalize_weights.r")
source("stop_criteria.r")
source("get_sd.r")
source("Cronbachs_alpha.r")
source("get_GoF.r")
source("get_rsquare.r")
source("get_redundancy_indexes.r")
source("get_Dillon_rho.r")
source("get_communality.r")
source("z_matrix.r")
source("y_matrix.r")
source("auxiliary_functions.r")
source("PLS.r")

setwd("C:/Users/Asus/Documents/DA_PSM_NEW")
#setwd("C:/Users/pspires/Documents/DA_PSM")
=======

#setwd("C:/Users/Asus/Documents/PLS_PATH_17/DA_PSM")
setwd("C:/Users/pspires/Documents/DA_PSM")
>>>>>>> 617b1fe1f54021fc0cb1be9dff4427b33751827c
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

ecsi <- sempls(model = result.pls, data = profdata, wscheme = "factorial", tol=0.0000001)


set.seed(123)
ecsiBoot <- bootsempls(ecsi, nboot=200, start="ones", verbose=TRUE)
summary(ecsiBoot, type="perc", level=0.95)

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


