#arguments Y standardize,innermodel
create.z.matrix = function(LV, innermodel,outermodel) {
  c = create.cov.matrix(LV, innermodel,outermodel)
  
  #....pensar nisto
  Y = as.matrix(LV) %*% (t(c))
  
  return(Y)
  
}