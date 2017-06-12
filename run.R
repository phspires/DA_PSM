##inner model - markov matrix (binary)
##outer model - weights matrix

#FIRST FUNCTION BUILD A MODEL
#ARGUMENTS (DATASET, INNER_MODEL,OUTER_MODEL)
library(readr)
library(readxl)
library(matpow)
source("path_coef.r")
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
source("Boot.R")
source("outer_loadings.R")
source("coefficients.R")
##setwd("C:/Users/Asus/Documents/DA_PSM_NEW")
setwd("C:/Users/pspires/Documents/DA_PSM")
inner.m <- read_excel("models.xlsx", sheet = "INNERMODEL")
outer.m <- read_excel("models.xlsx", sheet = "OUTERMODEL")

bank <- read.csv("bank.csv")
bank = bank[, 2:length(bank)]

### run model ####
model=advance.analytics.pls(bank,inner.m,outer.m,"Factor",tolerance=0.0000001, full=TRUE)

### do bootrapping ###
boot <- bootstrap.statistic(bank, n_samples = 500, inner.m, outer.m, tolerance=0.0000001)

### calculate statistics(t and p values) ###
tval <- bootstrap.tstat(boot,model)

### complete model$path ###
model$path <- cbind(model$path, boot$mean, boot$sd, tval$t, tval$p)
#######
