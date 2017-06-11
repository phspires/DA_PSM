get.redundancy.indexes= function(data,commulative,Z2,outermodel,innermodel){
  x = create.conection.matrix(innermodel,outermodel)
  commulative=get.communality(data,Z2,outermodel)
  r.square=c()
  redundancy.index=c()
  indexes=c()
  
  for (i in 1:ncol(x)){
    
    for (j in 1:nrow(x)){
      
      if (x[j,i]==1){ indexes=cbind(indexes,j)}
      
      
    }
    if(length(indexes)>0){
      
      regression=lm(Z2[,i]~ Z2[,indexes])
      r.square[i]=summary(regression)$r.square
      redundancy.index[i]=as.data.frame(r.square[i]*commulative[i])
      indexes=NULL
    }
    
  }
  
  redundancy=as.data.frame(t(redundancy.index))
  for (i in 1:length(redundancy)) {
    colnames(redundancy)[i] = get.number.mv(outermodel)[i, 1]
    
  }
  
  return(redundancy)
}