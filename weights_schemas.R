#arguments Y standardize,innermodel
create.cov.matrix = function(data, innermodel,outermodel) {
  x = create.conection.matrix(innermodel,outermodel)
  
  for (i in (1:nrow(x))) {
    rownames(x)[i] = x[i, 1]
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
centroid.scheme = function(Y, innermodel,outermodel) {
  x = create.conection.matrix(innermodel,outermodel)
  
  for (i in (1:nrow(x))) {
    rownames(x)[i] = x[i, 1]
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

Path.scheme = function(Y, innermodel,outermodel) {
  x = create.conection.matrix(innermodel,outermodel)
  
  
  for (i in (1:nrow(x))) {
    rownames(x)[i] = x[i, 1]
  }
  
  z = as.matrix(x)
  
  for (j in 1:nrow(x)) {
    for (i in (1:ncol(x))) {
      if (x[j, i] == 1) {
        z[j, i] = (cov(Y[, j], Y[, i]))
      }
    }
  }
  z = z + t(path.coef(Y, x))
  return (z)
  
}