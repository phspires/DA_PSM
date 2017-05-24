path.coef= function (LV,innermodel){
  
  x = innermodel[1:nrow(innermodel), 2:ncol(innermodel)]
  Y.prec.index = c()
  y=matrix(0,ncol(LV),ncol(LV))
  
  for ( i in (1:ncol(x))){
  
    for (j in (1:ncol(x))){
     
      if (x[j,i] ==1 ) {
        print(paste("i: ",i,"j: ",j))
        Y.prec.index=c(Y.prec.index,j)
      }
     
    }
    
    #print(Y.prec.index)
    f=solve(t(LV[,Y.prec.index])%*%LV[,Y.prec.index])%*%t(LV[,Y.prec.index])%*%LV[,i]
    Y.prec.index <- NULL
     #print(f)
   y[,i]=f
  
  }
 return(y)
}