path.coef = function (LV, innermodel,outermodel) {
  x = create.conection.matrix(innermodel,outermodel)
  Y.prec.index = c()
  y = matrix(0, ncol(LV), ncol(LV))
  f = NULL
  
  for (i in (1:ncol(x))) {
    for (j in (1:ncol(x))) {
      if (x[j, i] == 1) {
        Y.prec.index = c(Y.prec.index, j)
      }
      
    }
    
    if (!is.null(Y.prec.index)) {
      f = solve(t(LV[, Y.prec.index]) %*% LV[, Y.prec.index]) %*% t(LV[, Y.prec.index]) %*%
        LV[, i]
    }
    y[Y.prec.index, i] <- f
    Y.prec.index <- NULL
    
  }
  
  for (i in unique(outermodel[,2])){
    colnames(y)=i 
    #rownames(y)=i
    }
  
  
  return(y)
}

total_effects = function (path_coef) {
  
  
  
  total_effect=matrix(0,nrow(path_coef),ncol(path_coef))
  
  for (i in (1:ncol(total_effect))){
    
    total_effect=total_effect+matpow(path_coef,i)$prod1
    
  } 
  #for (i in colnames(path_coef)){
  #  colnames(total_effect)=i 
    #rownames(total_effect)=i
#  }
  
  
  return(total_effect)
}