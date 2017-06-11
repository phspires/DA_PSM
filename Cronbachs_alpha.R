
# measure for unimdimensionality
# rho

alpha.metric = function(bank, outermodel) {
  number.lv = get.total.lv(outermodel)
  alpha_metrics = c()
  previous = 0
  
  for (i in (1:number.lv)) {
    k = get.number.mv(outermodel)[i, 2]
    
    items = bank[, (previous + 1):(previous + k)]
    previous = k
    
    ## testar isto melhor deposi
    
    total = 0
    c = cor(items)
    limite = k - 1
    for (i in 1:(k - 1)) {
      total = total + i
    }
    sum = 0
    for (i in 1:k) {
      s = i + 1
      if (s == k + 1)
        break
      for (j in s:k) {
        sum = sum + c[j, i]
        
      }
      
    }
    alpha_metrics = cbind(alpha_metrics, sum / total)
    
    
  }
  for (i in (1:length(alpha_metrics))) {
    alpha_metrics[i] = (alpha_metrics[i] * get.number.mv(outer.m)[i, 2]) / (1 +
                                                                              ((get.number.mv(outer.m)[i, 2]) - 1) * alpha_metrics[i])
    
  }
  
  alpha_metrics = as.data.frame(alpha_metrics)
  for (i in 1:length(alpha_metrics)) {
    colnames(alpha_metrics)[i] = get.number.mv(outermodel)[i, 1]
    
  }
  
  return(alpha_metrics)
}
    