##inner model - markov matrix (binary)
##outer model - weights matrix

#FIRST FUNCTION BUILD A MODEL
#ARGUMENTS (DATASET, INNER_MODEL,OUTER_MODEL)
library(readr)
library(readxl)

setwd("C:/Users/Asus/Documents/PLS_PATH_17/DA_PSM")
inner.m <- read_excel("models.xlsx",sheet = "INNERMODEL")
outer.m <- read_excel("models.xlsx",sheet = "OUTERMODEL")

bank <- read_csv("bank.csv")

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
create.y.matrix=function(data,outermodel,weights){
  X=data
  Y=matrix(0,nrow(data),get.total.lv(outermodel))
  
  Y= as.matrix(X) %*% as.matrix(weights)
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
  
    for (i in (j:ncol(x))){
    
     if(x[j,i]==1) z[j,i]= cov(data[,j],data[,i])
    
     }
  }
z=z+t(z)
return (z)
  
}
#arguments Y standardize,innermodel
create.z.matrix=function(LV,innermodel){
  
  c=create.cov.matrix(LV,innermodel)
  
  #....pensar nisto
  Y= as.matrix(LV) %*% (t(c))
  
  return(Y)

}

update.weigths= function(LV,data,outermodel){
  k=1
  w=matrix(0,nrow(outermodel),ncol(LV))
  
for (i in (1:get.total.lv(outermodel))){
 
    index=get.number.mv(outermodel)[i,2]
  
    for (j in k:(k+index-1)){
      
      w[j,i] = cov((LV[,i]),(data[,j])) 
    
    }
    k=k+get.number.mv(outermodel)[i,2]
}

return (w)

}
  
stop.criteria=function (weights1,weights2){
  
  z=matrix(NA,nrow(weights1),ncol(weights2))
  sum=0

    for(i in (1:ncol(weights1))){
      for (j in (1:nrow(weights1))) {
        sum=sum + abs(weights1[j,i]-weights2[j,i])
      }
    }
  return (sum)
  
}

get.sd=function(Y){
  c=matrix(0,1,ncol(Y))
  for ( i in (1:ncol(Y))){
    c[i]= sd(Y[,i])
    
  }
  return(c)
  
}

normalize.weights=function(weights,sd,outermodel){
  k=1
  
  for (i in (1:ncol(weights))){
    
    index=get.number.mv(outermodel)[i,2]
    
    for (j in k:(k+index-1)){
      
      weights[j,i]=weights[j,i]/sd[i]
    }
    k=k+get.number.mv(outermodel)[i,2]
  }
  
  return(weights)
}

my.pls=  function (data, innermodel,outermodel,schema,tolerance){
  
  if (is.data.frame(innermodel) & (is.data.frame(outermodel) ||ncol(outermodel)!=2) & is.data.frame(data)) {
    
    print ("OBJECTS STRUCTURE ARE FINE")

        stop=FALSE
        i=1
        initialweights=create.w.matrix(outermodel)
        #treat missing values
        data=input.means(data)
        #normalize X
        data=normalize(data)
        #score Y, latent variables
        while(stop==FALSE) {
         
          Y=create.y.matrix(data,outermodel,initialweights)
          #normalize Y
          Y=normalize(Y)
          cov=create.cov.matrix(Y, innermodel)
          z=create.z.matrix(Y,innermodel)
          z=normalize(z)
          newweights=update.weigths(z,data,outermodel)
          e= create.y.matrix(data,outermodel,newweights)
          #print(e)
          #print(summary(e))
          
          norm.weigh=get.sd(e)
          newweights=normalize.weights(newweights,norm.weigh,outermodel)
          print(i)
          
          if(stop.criteria(initialweights,newweights)<tolerance){
            print("stop")
            print(stop.criteria(initialweights,newweights))
            stop=TRUE
          }
          
          initialweights=newweights
          
          i=i+1
          next
        
          }
    return(stop.criteria(initialweights,newweights))
  }
  else
    print('A ESTRUTURA DO INNER MODEL MUST BE A MATRIX')
}

  