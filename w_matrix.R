
# creates weights matrix, is a matrix (MV BY LV)

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