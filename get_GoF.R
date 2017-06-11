
#quality index, GoF,AVERAGE_COMUL, AVERAGE_R^2
get.GoF=function(data,Z2,innermodel,outermodel){
  
  r.square=as.numeric(get.R.square(Z2,innermodel,outermodel))
  comulative=as.numeric(get.communality(data,Z2,outermodel))
  
  mv=c()
  index=c()
  
  for (i in 1:ncol(Z2)){
    
    mv[i]= get.number.mv(outermodel)[i,2]  
    
  }
  commulative.sum=0
  
  for (i in 1:length(comulative)){
    commulative.sum=commulative.sum+mv[i]*comulative[i]
  }
  
  average.r.square=mean(r.square,na.rm = TRUE)
  average.commulative=commulative.sum/sum(mv)
  GoF=(average.r.square*average.commulative)^0.5
  
  r1=c("Avg R.Square","Avg Commulative","GoF")
  r2=c(average.r.square,average.commulative,GoF)
  
  measure=as.data.frame(t(r2))
  
  for (i in 1:ncol(measure)){
    
    colnames(measure)[i]=r1[i]
    
  }
  
  return(measure)
}

