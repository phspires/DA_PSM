#Get bootstrap samples
bootstrap.matrix.sample<-function (data){

  n_rows = nrow(data)
  ind<-  ceiling(runif(n_rows, min=1, max=n_rows))

  return (ind)
}
#Get boostrap samples mean and standard deviation
bootsrap.statistic<-function (data,n_samples = 10,innerm,outerm){
  
  sample_matrix <- matrix(NA,n_samples,nrow(innerm)+nrow(outerm))
  results <- list(mean = NULL, sd= NULL)
  class(results) <-"bootstrap.matrix.statistic" 
  # Compute statistics and mean it to get statistic's value
  for (i in 1:n_samples) {
    model<-advance.analytics.pls(data[bootstrap.matrix.sample(data),],innerm,outerm,tolerance = 1e-7)
    outer_loading<- create.w.matrix(outerm)*model$cross_loadings
    val<-c(outer_loading[outer_loading!=0],model$path_coefficients[model$path_coefficients!=0])
    print(length(val))
    sample_matrix[i,]<-val
  }
   
  mean<- colMeans(sample_matrix)
  #compute standard deviation
  sd<- apply(sample_matrix, 2, sd)
  results$mean<-mean
  results$sd<-sd
  
 
  return (results)
  
}
#t statistitc
bootstrap.tstat <- function(result,model,outerm,n_samples){
  outer_loading<- create.w.matrix(outerm)*model$cross_loadings
  val<-c(outer_loading[outer_loading!=0],model$path_coefficients[model$path_coefficients!=0])
  t<- ((result$mean-val)*sqrt(n_samples))/(result$sd)
  return(t)
}
