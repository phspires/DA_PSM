##inner model - markov matrix (binary)
##outer model - weights matrix

#FIRST FUNCTION BUILD A MODEL
#ARGUMENTS (DATASET, INNER_MODEL,OUTER_MODEL)
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

bank <- read_csv("bank.csv")

bank = bank[, 2:length(bank)]


model=advance.analytics.pls(bank,inner.m,outer.m,,0.001)



