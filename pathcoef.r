path.coef = function (LV, innermodel) {
  x = innermodel[1:nrow(innermodel), 2:ncol(innermodel)]
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
  return(y)
}