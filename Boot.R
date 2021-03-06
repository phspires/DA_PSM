#source("code.R")
#source("w_matrix.R")
bootstrap.matrix.sample<-function (data){

  n_rows = nrow(data)
  ind<-  ceiling(runif(n_rows, min=1, max=n_rows))

  return (ind)
}

bootstrap.statistic<-function (data, n_samples = 50, innerm, outerm, mode="A",tolerance){

  sample_matrix <- matrix(NA,n_samples,nrow(innerm)+nrow(outerm))
  results <- list(mean = NULL, sd= NULL,nsamples=NULL)
  #class(results) <-"bootstrap.matrix.statistic"
  # Compute statistics and mean it to get statistic's value
  for (i in 1:n_samples) {
    model<-advance.analytics.pls(data[bootstrap.matrix.sample(data),],innerm,outerm,mode=mode,tolerance=tolerance, full=FALSE)
    outer_loading<- create.w.matrix(outerm)*model$cross_loadings
    val<-c(outer_loading[outer_loading!=0],model$path_coefficients[model$path_coefficients!=0])
    sample_matrix[i,]<-val
  }

  mean<- colMeans(sample_matrix)
  #compute standard deviation
  sd<- apply(sample_matrix, 2, sd)
  results$mean<-mean
  results$sd<-sd
  results$nsamples<-n_samples
  #compute tvalue
  return (results)
}

#t statistitc
bootstrap.tstat <- function(result,model){
  tstats <- list(t = NULL, p= NULL)

  outer_loading<- create.w.matrix(model$outermodel)*model$cross_loadings

  val<-c(outer_loading[outer_loading!=0],model$path_coefficients[model$path_coefficients!=0])

  t<- ((result$mean-val)*sqrt(result$nsamples))/(result$sd)

  p<- 2*pt(t, df=result$nsamples-1)

  tstats$t <- t
  tstats$p <- p

  return(tstats)
}
