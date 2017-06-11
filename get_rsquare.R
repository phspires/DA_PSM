
get.R.square = function (Z2,innermodel,outermodel){
  x = create.conection.matrix(innermodel,outermodel)
  indexes=c()
  r.square=c()
  for (i in 1:ncol(x)){
    
    for (j in 1:nrow(x)){
      
      if (x[j,i]==1){ indexes=cbind(indexes,j)}
      
      
    }
    if(length(indexes)>0){
      
      regression=lm(Z2[,i]~ Z2[,indexes])
      r.square[i]=summary(regression)$r.square
      indexes=NULL
    }
    
  }
  r.square = as.data.frame(t(r.square))
  for (i in 1:length(r.square)) {
    colnames(r.square)[i] = get.number.mv(outermodel)[i, 1]
    
  }
  return(r.square)
}