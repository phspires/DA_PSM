#bank and outermodel
create.y.matrix = function(data, outermodel, weights) {
  X = data
  Y = matrix(0, nrow(data), get.total.lv(outermodel))
  
  Y = as.matrix(X) %*% as.matrix(weights)
  return (Y)
  
}

