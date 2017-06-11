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