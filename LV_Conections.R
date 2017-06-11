##CREATE THE INNER MODEL
##CONECTION MATRIX BETWEEN LATENT VARIABLES


#the only reason why pass the outermodel as parameter is to construct the inner with tthe same order.
create.conection.matrix=function (innermodel,outermodel){
  
  if (ncol(innermodel)!=2){ stop("The inner model must be a file with to colums")}
  order=c()
  #attach(innermodel)
  
  conection = matrix(0,nrow(unique(innermodel[,2]))+1,nrow(unique(innermodel[,2]))+1)
  
  for (i in 1:ncol(conection)){
    order[i]=get.number.mv(outermodel)[i,1]
  }
  
  colnames(conection)=order
  rownames(conection)=order
  
  for (i in 1:ncol(conection)){
    new_data=innermodel[which(FROM==order[i]),]
    
    for (j in i:ncol(conection)) {
      d = colnames(conection)[j]
      
      for (k in 1:nrow(new_data)){
      if (colnames(conection)[j] %in% new_data[k, 2]) {
        conection[i, j] = 1
      }
      }
    }
  }
  
  return(conection)
}