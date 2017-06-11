get.total.mv = function (outermodel) {
  return(nrow(outermodel))
}

get.total.lv = function (outermodel) {
  return(nrow(unique(outermodel[, 2])))
}

get.number.mv = function (outermodel) {
  z = matrix(0, get.total.lv(outermodel), 2)
  z = as.data.frame(z)
  controlo = 0
  for (i in 1:get.total.lv(outermodel))
  {
    x <- unique(outermodel[, 2])[i, 1]
    z[i, 1] <- as.matrix(x)
    count = 1
    
    for (j in (1:(nrow(outermodel) - 1)))
    {
      if (j > controlo && outermodel[j, 2] == outermodel[j + 1, 2])
      {
        count = count + 1
      }
      
      else if (j <= controlo) {
        
      }
      
      else
        break
    }
    z[i, 2] = count
    controlo = controlo + count
  }
  return(z)
}
