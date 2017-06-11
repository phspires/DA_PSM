#update weights

update.weigths <- function(LV, data, outermodel, mode) {
  k = 1
  w = matrix(0, nrow(outermodel), ncol(LV))
  
  for (i in (1:get.total.lv(outermodel))) {
    index = get.number.mv(outermodel)[i, 2]
    
    if (mode == "A") {
      #message("mode A")
      w[k:(k + index - 1), i] = cov((LV[, i]), (data[, k:(k + index - 1)]))
      
    } else if (mode == "B") {
      #message("mode B")
      w[k:(k + index - 1), i] = solve(cor(data[, k:(k + index - 1)])) %*% cor((data[, k:(k + index - 1)]), (LV[, i]))
    }
    
    #  for (j in k:(k + index - 1)) {
    #  }
    k = k + get.number.mv(outermodel)[i, 2]
  }
  return (w)
}