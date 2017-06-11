
get.sd = function(Y) {
  c = matrix(0, 1, ncol(Y))
  for (i in (1:ncol(Y))) {
    c[i] = sd(Y[, i])
    
  }
  return(c)
}