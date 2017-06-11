source("code.R")
bootstrap.matrix.sample<-function (data){

  n_rows = nrow(data)
  ind<-  ceiling(runif(n_rows, min=1, max=n_rows))

  return (ind)
}

bootstrap.matrix.statistic<-function (data,n_samples = 10,innerm,outerm){
  
  sample_matrix <- matrix(NA,n_samples,nrow(innerm)+nrow(outerm))
  
  # Compute statistics and mean it to get statistic's value
  for (i in 1:n_samples) {
    model<-advance.analytics.pls(data[bootstrap.matrix.sample(data),],innerm,outerm,tolerance = 0.00001)
    val<-c(model$outer_weights[model$outer_weights!=0],model$path_coefficients[model$path_coefficients!=0])
    print(length(val))
    #sample_matrix[i,]<- c(model$outer_weights[model$outer_weights!=0],model$path_coefficients[model$path_coefficients!=0])
  }
   
  mean<- colMeans(sample_matrix)
  #compute standard deviation
  sd<- apply(sample_matrix, 1:ncol(sample_matrix), sd)
  
  #compute tvalue
  return (sd)
  
}
