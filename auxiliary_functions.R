get.total.mv = function (outermodel) {
  return(nrow(outermodel))
}

get.total.lv = function (outermodel) {
  return(nrow(unique(outermodel[, 2])))
}

get.number.mv = function (outermodel) {
  
  z = matrix(0, get.total.lv(outermodel), 2)
  z = as.data.frame(z)
  
  for (i in 1:get.total.lv(outermodel))
  {
    x <- unique(outermodel[, 2])[i, 1]
    z[i, 1] <- as.matrix(x)
    count = 0
    
    for (j in (1:(nrow(outermodel))))
    {
      if (outermodel[j, 2] == z[i,1])
      {
        count = count + 1
      }
      
    

    }
    z[i, 2] = count

  }
  return(z)
}
