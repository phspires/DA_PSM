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