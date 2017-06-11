get.communality= function (data,Z2,outermodel){
  
  res=c()
  
  previous=0
  for (i in (1:ncol(Z2))){
    k=get.number.mv(outermodel)[i,2]
    
    sum=0
    
    for (j in (previous+1):(previous+k)){
      sum=  sum+cor(data[,j],Z2[,i])*cor(data[,j],Z2[,i])
      if(is.null(sum)) sum=0
    }
    res[i]=sum/k
    res=as.data.frame(res)
    for (i in 1:length(res)) {
      colnames(res)[i] = get.number.mv(outermodel)[i, 1]
      
    }
    
    previous=previous+k
  }
  return(res)  
}

