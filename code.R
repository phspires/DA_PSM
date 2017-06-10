##inner model - markov matrix (binary)
##outer model - weights matrix

#FIRST FUNCTION BUILD A MODEL
#ARGUMENTS (DATASET, INNER_MODEL,OUTER_MODEL)
library(readr)
library(readxl)
source("pathcoef.r")
source("update.weigthsB.R")
#setwd("C:/Users/Asus/Documents/DA_PSM")
setwd("C:/Users/pspires/Documents/DA_PSM")
inner.m <- read_excel("models.xlsx", sheet = "INNERMODEL")
outer.m <- read_excel("models.xlsx", sheet = "OUTERMODEL")

bank <- read_csv("bank.csv")

bank = bank[, 2:length(bank)]

input.means = function(data)
{
  #Missing values input with mean
  data.means = colMeans(data, na.rm = TRUE)
  
  for (i in 1:ncol(data)) {
    data[is.na(data[, i]), i] = data.means[i]
  }
  
  return(data)
}

get.total.mv = function (outermodel) {
  return(nrow(outermodel))
}

get.total.lv = function (outermodel) {
  return(nrow(unique(outermodel[, 2])))
}

get.number.mv = function (outermodel) {
  z = matrix(0, get.total.lv(outermodel), 2)
  z = as.data.frame(z)
  controlo = 0
  for (i in 1:get.total.lv(outermodel))
  {
    x <- unique(outermodel[, 2])[i, 1]
    z[i, 1] <- as.matrix(x)
    count = 1
    
    for (j in (1:(nrow(outermodel) - 1)))
    {
      if (j > controlo && outermodel[j, 2] == outermodel[j + 1, 2])
      {
        count = count + 1
      }
      
      else if (j <= controlo) {
        
      }
      
      else
        break
    }
    z[i, 2] = count
    controlo = controlo + count
  }
  return(z)
}

#outermodel
create.w.matrix = function(outermodel) {
  k = 1
  b = matrix(0, get.total.mv(outermodel), get.total.lv(outermodel))
  z = as.data.frame(b)
  
  for (i in 1:get.total.lv(outermodel)) {
    colnames(z)[i] = get.number.mv(outermodel)[i, 1]
    
    for (j in 1:get.total.mv(outermodel)) {
      rownames(z)[j] = outermodel[j, 1]
      if (j >= k && j <= k - 1 + get.number.mv(outermodel)[i, 2]) {
        z[j, i] = 1
      }
    }
    k = k + get.number.mv(outermodel)[i, 2]
    
  }
  return(z)
  
}
#bank and outermodel
create.y.matrix = function(data, outermodel, weights) {
  X = data
  Y = matrix(0, nrow(data), get.total.lv(outermodel))
  
  Y = as.matrix(X) %*% as.matrix(weights)
  return (Y)
  
}

#both bank and Y and Z
normalize = function (data) {
  return(scale(data))
  
}

#arguments Y standardize,innermodel
create.cov.matrix = function(data, innermodel) {
  x = innermodel[1:nrow(innermodel), 2:ncol(innermodel)]
  
  for (i in (1:nrow(x))) {
    rownames(x)[i] = innermodel[i, 1]
  }
  
  z = as.matrix(x)
  
  for (j in 1:nrow(x)) {
    for (i in (j:ncol(x))) {
      if (x[j, i] == 1)
        z[j, i] = cov(data[, j], data[, i])
    }
  }
  z = z + t(z)
  return (z)
  
}

#arguments Y standardize,innermodel
# Create the e matrix, covariance matrix, and if the cov<0 = -1 , cov>0=1
centroid.scheme = function(Y, innermodel) {
  x = innermodel[1:nrow(innermodel), 2:ncol(innermodel)]
  
  for (i in (1:nrow(x))) {
    rownames(x)[i] = innermodel[i, 1]
  }
  
  z = as.matrix(x)
  
  for (j in 1:nrow(x)) {
    for (i in (j:ncol(x))) {
      if (x[j, i] == 1)
        z[j, i] = sign(cov(Y[, j], Y[, i]))
      
    }
  }
  z = z + t(z)
  return (z)
  
}

Path.scheme = function(Y, innermodel) {
  x = innermodel[1:nrow(innermodel), 2:ncol(innermodel)]
  
  
  for (i in (1:nrow(x))) {
    rownames(x)[i] = innermodel[i, 1]
  }
  
  z = as.matrix(x)
  
  for (j in 1:nrow(x)) {
    for (i in (1:ncol(x))) {
      if (x[j, i] == 1) {
        z[j, i] = (cov(Y[, j], Y[, i]))
      }
    }
  }
  z = z + t(path.coef(Y, innermodel))
  return (z)
  
}
#arguments Y standardize,innermodel
create.z.matrix = function(LV, innermodel) {
  c = create.cov.matrix(LV, innermodel)
  
  #....pensar nisto
  Y = as.matrix(LV) %*% (t(c))
  
  return(Y)
  
}

update.weigths <- function(LV, data, outermodel, mode) {
  k = 1
  w = matrix(0, nrow(outermodel), ncol(LV))
  
  for (i in (1:get.total.lv(outermodel))) {
    index = get.number.mv(outermodel)[i, 2]
    
    if (mode == "A") {
      #message("mode A")
      w[k:(k + index - 1), i] = cov((LV[, i]), (data[, k:(k + index - 1)]))
      
    } else if (mode == "B") {
      #message("mode B")
      w[k:(k + index - 1), i] = solve(cor(data[, k:(k + index - 1)])) %*% cor((data[, k:(k + index - 1)]), (LV[, i]))
    }
    
    #  for (j in k:(k + index - 1)) {
    #  }
    k = k + get.number.mv(outermodel)[i, 2]
  }
  return (w)
}

stop.criteria = function (weights1, weights2) {
  z = matrix(NA, nrow(weights1), ncol(weights2))
  sum = 0
  
  for (i in (1:ncol(weights1))) {
    for (j in (1:nrow(weights1))) {
      sum = sum + abs(weights1[j, i] - weights2[j, i])
    }
  }
  return (sum)
  
}

get.sd = function(Y) {
  c = matrix(0, 1, ncol(Y))
  for (i in (1:ncol(Y))) {
    c[i] = sd(Y[, i])
    
  }
  return(c)
}

normalize.weights = function(weights, sd, outermodel) {
  k = 1
  
  for (i in (1:ncol(weights))) {
    index = get.number.mv(outermodel)[i, 2]
    
    for (j in k:(k + index - 1)) {
      weights[j, i] = weights[j, i] / sd[i]
    }
    k = k + get.number.mv(outermodel)[i, 2]
  }
  
  return(weights)
}

my.pls =  function (data,
                    innermodel,
                    outermodel,
                    wscheme,
                    tolerance,
                    mode="A") {
  result <- list(
    coefficients = NULL,
    path_coefficients = NULL,
    outer_loadings = NULL ,
    cross_loadings = NULL,
    total_effects = NULL,
    inner_weights = NULL,
    outer_weights = NULL,
    ##stopcriteria = NULL, replace by tolerance
    #blocks = NULL,
    #factor_scores = NULL,
    #data = NULL,
    #scaled = scaled,
    #model = model,
    weighting_scheme = NULL,
    #weights_evolution = NULL,
    #sum1 = sum1,
    #pairwise = pairwise,
    #method = method,
    iterations = NULL,
    #convCrit = convCrit,
    #verbose = verbose,
    tolerance = NULL,
    #N = NULL,
    #incomplete = NULL
    innermodel = inner.m,
    outermodel = outer.m,
    Y = NULL,
    z = NULL,
    aux = NULL
  )
  
  class(result) <- "my.pls"
  
  #pb <- winProgressBar("Starting PLS-PM...", "Please wait %",0, 100, 0)
  
  modes <- c("A","B")
  stopifnot(mode %in% modes)
  
  if (is.data.frame(innermodel) &
      (is.data.frame(outermodel) ||
       ncol(outermodel) != 2) & is.data.frame(data)) {
    message ("OBJECTS STRUCTURE IS FINE")
    
    stop = FALSE
    i = 0
    
    # data preparation
    ##treat missing values
    data = input.means(data)
    ## normalize X
    data = normalize(data)
    
    #step 1 -- initialize weights(w) to 1
    outer.w = create.w.matrix(outermodel)
    
    ### progression bar
    
    
    while (stop == FALSE) {
      i = i + 1
      u <- 0
      
      #step 2
      ##outer estimation of latente variables scores (Y)
      Y = create.y.matrix(data, outermodel, outer.w)
      
      #normalize Y
      Y = normalize(Y)
      
      #step 3
      ## inner weights estimation (e)
      if (wscheme == "Factor") {
        inner.w = create.cov.matrix(Y, innermodel)
      } else if (wscheme == "Centroid") {
        inner.w = centroid.scheme(Y, innermodel)
      } else if (wscheme == "Path") {
        inner.w = Path.scheme(Y, innermodel)
      }
      
      #step 4
      ## inner estimation of latent variables scores (Z)
      z = create.z.matrix(Y, innermodel)
      z = normalize(z)
      
      ##step 5
      ##outer weights update (w)
      ## get new weights

      new.outer.weights = update.weigths(z, data, outermodel, mode)
        
      ## auxiliar matrix needed for weight normalization
      m.aux = create.y.matrix(data, outermodel, new.outer.weights)
      
      #print(m.aux)
      #print(summary(m.aux))
      
      norm.weigh = get.sd(m.aux)
      ## for generalization add mean subtraction
      
      new.outer.weights = normalize.weights(new.outer.weights, norm.weigh, outermodel)
      
      stop.criteria <-
        stop.criteria(outer.w, new.outer.weights)
      
      message("e: ", stop.criteria)
      
      u <- round(100 * tolerance / stop.criteria, 2)
      
      if (stop.criteria < tolerance) {
        stop = TRUE
        u = 100
      }
      
      outer.w = new.outer.weights
      message("#", i)
      #info <- sprintf("%d%% completion - iteration %d", round(u),i)
      #setWinProgressBar(pb, u, sprintf("OurPLS (%s)", info), info)
      next
    }
    
    #stage 2
    p.Coef <- path.coef(scale(m.aux), innermodel)
    
    #staget 3
    ## o.load <- outer_loadings()
    
    #stage 4
    #Cross loadings
    c.load <- cor(data, scale(m.aux))
    
    #result$coefficients <- "atribuir coeficientes"
    result$path_coefficients <- p.Coef
    result$outer_loadings <- NULL #<- o.load
    result$cross_loadings <- c.load
    result$total_effects <- NULL
    result$inner_weights <- inner.w
    #result$outer_weights <- NULL
    result$tolerance <- stop.criteria
    result$iterations <- i
    result$outer_weights <- outer.w ## always updated
    result$z <- z
    result$Y <- Y
    result$aux <- scale(m.aux)
    result$weighting_scheme <- wscheme
    result$z <- z
    result$data <- data
    result$outm <- outermodel
    
    message("End!")
    #close(pb)
    return(result)
  }
  else
    warning('INNER MODEL STRUCTURE MUST BE A MATRIX')
}



alpha.metric = function(bank, outermodel) {
  number.lv = get.total.lv(outermodel)
  alpha_metrics = c()
  previous = 0
  
  for (i in (1:number.lv)) {
    k = get.number.mv(outermodel)[i, 2]
    
    items = bank[, (previous + 1):(previous + k)]
    previous = k
    
    ## testar isto melhor deposi
    
    total = 0
    c = cor(items)
    limite = k - 1
    for (i in 1:(k - 1)) {
      total = total + i
    }
    sum = 0
    for (i in 1:k) {
      s = i + 1
      if (s == k + 1)
        break
      for (j in s:k) {
        sum = sum + c[j, i]
        
      }
      
    }
    alpha_metrics = cbind(alpha_metrics, sum / total)
    
    
    
  }
  for (i in (1:length(alpha_metrics))) {
    alpha_metrics[i] = (alpha_metrics[i] * get.number.mv(outer.m)[i, 2]) / (1 +
                                                                              ((get.number.mv(outer.m)[i, 2]) - 1) * alpha_metrics[i])
    
  }
  
  alpha_metrics = as.data.frame(alpha_metrics)
  for (i in 1:length(alpha_metrics)) {
    colnames(alpha_metrics)[i] = get.number.mv(outermodel)[i, 1]
    
  }
  
  return(alpha_metrics)
}
