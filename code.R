
## André Bruno Pedro
## The A-Team 

library(readr)
library(readxl)
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
#source("update.weigthsB.R")
setwd("C:/Users/Asus/Documents/DA_PSM_NEW")
#setwd("C:/Users/pspires/Documents/DA_PSM")
inner.m <- read_excel("models.xlsx", sheet = "INNERMODEL")
outer.m <- read_excel("models.xlsx", sheet = "OUTERMODEL")
profdata <- read_excel("C:/Users/Asus/Desktop/profdata.xlsx")

bank <- read.csv("bank.csv")

bank = bank[, 2:length(bank)]



aapls <- function(bank,i) {
  
  model=advance.analytics.pls(bank[i,],inner.m,outer.m,,0.1)
  return (model$outer_weights)
}

#boot(bank, aapls, R = 500, stype = "i")



model=advance.analytics.pls(profdata,inner.m,outer.m,"Factor",0.0000001,"A")



