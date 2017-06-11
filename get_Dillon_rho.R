
get.Dillon.rho = function (crossload,outermodel){
  
  w=create.w.matrix(outermodel)
  pesos=w
  lamedas=c()
  rho=c()
  
 
      pesos=crossload*w
 
  
  previous=0
  for (i in 1:ncol(pesos)){
    k=get.number.mv(outermodel)[i,2]
    for (j in (previous+1):(previous+k)){
      
      lamedas[j-previous]=pesos[j,i]
      
    }
    
    
    lamedas.square=sapply(lamedas,function(x) x^2)
    error=sapply(lamedas.square, function(x) 1-x)
    sum.lamedas=sum(lamedas)
    sum.square.lamedas=sum.lamedas*sum.lamedas
    sum.error=sum(error)
    
    previous=previous+k
    lamedas=NULL
    rho[i]=(sum.square.lamedas/(sum.square.lamedas+sum.error))
    
  }
  return(rho)
}