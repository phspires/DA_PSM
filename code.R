##inner model - markov matrix (binary)
##outer model - weights matrix

#FIRST FUNCTION BUILD A MODEL
#ARGUMENTS (DATASET, INNER_MODEL,OUTER_MODEL)
library(readr)
library(readxl)

 
setwd("C:/Users/Asus/Desktop/DESCRIPTIVE I")
inner.m <- read_excel("models.xlsx",sheet = "INNERMODEL")
outer.m <- read_excel("models.xlsx",sheet = "OUTERMODEL")

bank <- read_csv("C:/Users/Asus/Desktop/DESCRIPTIVE I/bank.csv")

bank=bank[,2:length(bank)]

input.means= function(data) 
  {
    #Missing values input with mean
    data.means=colMeans(data,na.rm = TRUE)
  
    for(i in 1:ncol(data)){
      data[is.na(data[,i]), i] = data.means[i]
    }
    
  return(data)
  }
  
get.total.mv = function (outermodel){
  return(nrow(outermodel))
}

get.total.lv = function (outermodel){
  return(nrow(unique(outermodel[,2])))
}

get.number.mv= function (outermodel){
  z=matrix(0,get.total.lv(outermodel),2)
  z=as.data.frame(z)
  controlo=0
  for(i in 1:get.total.lv(outermodel))
  { 
    x<-unique(outermodel[,2])[i,1]
    z[i,1]<-as.matrix(x)
    count=1
    
      for (j in (1:(nrow(outermodel)-1)))
      {
        if(j>controlo && outermodel[j,2]==outermodel[j+1,2]) 
          {count=count+1}
           
          else if (j<=controlo) {}
                
              else break
      }
    z[i,2]=count
    controlo=controlo+count
  }
 return(z)
}

#outermodel
create.w.matrix=function(outermodel){
  k=1  
  b=matrix(0,get.total.mv(outermodel),get.total.lv(outermodel))
  z=as.data.frame(b)
 
  for (i in 1:get.total.lv(outermodel)){
    
  colnames(z)[i]=get.number.mv(outermodel)[i,1]
  
    for(j in 1:get.total.mv(outermodel)){
      rownames(z)[j]=outermodel[j,1]
      if(j>=k && j<=k-1+get.number.mv(outermodel)[i,2]){
        z[j,i]=1
      }
    }
    k=k+get.number.mv(outermodel)[i,2]
    
  }
return(z)  
}
#bank and outermodel
create.y.matrix=function(data,outermodel){
  X=data
  W=create.w.matrix(outermodel)
  Y=matrix(0,nrow(data),get.total.lv(outermodel))
  
  Y=  as.matrix(X) %*% as.matrix(W)
  return (Y)
  
}
#both bank and Y and Z
normalize= function (data){
  
  return(scale(data)*(nrow(data)-1)/nrow(data))
  
}

#arguments Y standardize,innermodel
create.cov.matrix= function(data,innermodel){
  
  x=innermodel[1:nrow(innermodel),2:ncol(innermodel)]
  
  for (i in (1:nrow(x))){
    rownames(x)[i]=innermodel[i,1]
  }
 
  z=as.matrix(x)
  
  for (j in 1:nrow(x)){
  
    for (i in (j:ncol(data))){
    
     if(x[j,i]==1) z[j,i]= cov(data[,j],data[,i])
    
     }
  }
return (z)
  
}
#arguments Y standardize,innermodel
create.z.matrix(data,innermodel){
  
  c=create.cov.matrix(data,innermodel)
  
  #....pensar nisto
  

}

  my.pls=  function (data, inner_model,outermodel){
    
    if (is.data.frame(inner_model) & (is.data.frame(outermodel) ||ncol(outermodel)!=2) & is.data.frame(data)) {
      
      print ("OBJECTS STRUCTURE ARE FINE")
      
      data=input.means(data)
      
      #Normalize
      return(normalize_data <- scale(data))
    }
    
    else
    
        print('A ESTRUTURA DO INNER MODEL MUST BE A MATRIX')
      
  }
  
   C<- my.pls(bank,inner.m,outer.m)


  