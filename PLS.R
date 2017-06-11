advance.analytics.pls =  function (data,
                    innermodel,
                    outermodel,
                    wscheme="Factor",
                    tolerance,
                    mode="A",max_iter=1) {
  result <- list(
    coefficients = NULL,
    path_coefficients = NULL,
    outer_loadings = NULL ,
    cross_loadings = NULL,
    total_effects = NULL,
    inner_weights = NULL,
    outer_weights = NULL,
    ##stopcriteria = NULL, replace by tolerance
    #blocks = NULL,
    #factor_scores = NULL,
    #data = NULL,
    #scaled = scaled,
    #model = model,
    weighting_scheme = NULL,
    #weights_evolution = NULL,
    #sum1 = sum1,
    #pairwise = pairwise,
    #method = method,
    iterations = NULL,
    #convCrit = convCrit,
    #verbose = verbose,
    tolerance = NULL,
    #N = NULL,
    #incomplete = NULL
    innermodel = create.conection.matrix(innermodel,outermodel),
    outermodel = outer.m,
    Y = NULL,
    z = NULL,
    r.square=NULL,
    GoF=NULL,
    Cronbach.alpha=NULL,
    Dillon.Goldstein=NULL,
    Redundancy.indexes=NULL,
    Communality=NULL
  )
  
  class(result) <- "my.pls"
  
  #pb <- winProgressBar("Starting PLS-PM...", "Please wait %",0, 100, 0)
  
  modes <- c("A","B")
  stopifnot(mode %in% modes)
  
  if (is.data.frame(innermodel) &
      (is.data.frame(outermodel) ||
       ncol(outermodel) != 2) & is.data.frame(data)) {
    message ("OBJECTS STRUCTURE IS FINE")
    
    stop = FALSE
    i = 0
    
    # data preparation
    ##treat missing values
    data = input.means(data)
    ## normalize X
    data = normalize(data)
    
    #step 1 -- initialize weights(w) to 1
    outer.w = create.w.matrix(outermodel)
    
    ### progression bar
    
    
    while (stop == FALSE) {
      i = i + 1
      u <- 0
      
      #step 2
      ##outer estimation of latente variables scores (Y)
      Y = create.y.matrix(data, outermodel, outer.w)
      
      #normalize Y
      Y = normalize(Y)
      
      #step 3
      ## inner weights estimation (e)
      if (wscheme == "Factor") {
        inner.w = create.cov.matrix(Y, innermodel,outermodel)
      } else if (wscheme == "Centroid") {
        inner.w = centroid.scheme(Y, innermodel,outermodel)
      } else if (wscheme == "Path") {
        inner.w = Path.scheme(Y, innermodel,outermodel)
      }
      
      #step 4
      ## inner estimation of latent variables scores (Z)
      z = create.z.matrix(Y, innermodel,outermodel)
      z = normalize(z)
      
      ##step 5
      ##outer weights update (w)
      ## get new weights
      
      new.outer.weights = update.weigths(z, data, outermodel, mode)
      
      ## auxiliar matrix needed for weight normalization
      m.aux = create.y.matrix(data, outermodel, new.outer.weights)
      
      #print(m.aux)
      #print(summary(m.aux))
      
      norm.weigh = get.sd(m.aux)
      ## for generalization add mean subtraction
      
      new.outer.weights = normalize.weights(new.outer.weights, norm.weigh, outermodel)
      
      stop.criteria <-
        stop.criteria(outer.w, new.outer.weights)
      
      message("e: ", stop.criteria)
      
      u <- round(100 * tolerance / stop.criteria, 2)
      
      if (stop.criteria < tolerance) {
        stop = TRUE
        u = 100
      }
      
      outer.w = new.outer.weights
      message("#", i)
      #info <- sprintf("%d%% completion - iteration %d", round(u),i)
      #setWinProgressBar(pb, u, sprintf("OurPLS (%s)", info), info)
      if(i==max_iter){ paste("max_interations reach, algorithm will stop without reach the stop criteria")
      break
      }
      next
     
    }
    
    #stage 2
    p.Coef <- path.coef(scale(m.aux), innermodel,outermodel)
    
    #staget 3
    ## o.load <- outer_loadings()
    
    #stage 4
    #Cross loadings
    c.load <- cor(data, scale(m.aux))
    
    #r.square
    r.square<- get.R.square(scale(m.aux),innermodel,outermodel)
    
    #GoF
    
    Gof<- get.GoF(data,scale(m.aux),innermodel,outermodel)
    
    Cronbach.alpha<- alpha.metric(data,outermodel)
    
    Dillon.Goldstein <- get.Dillon.rho(c.load,outermodel)
    
    Communality <- get.communality(data,scale(m.aux),outermodel)
    
    
    Redundancy.indexes <- get.redundancy.indexes(data,Communality,scale(m.aux),outermodel,innermodel)
    
    #result$coefficients <- "atribuir coeficientes"
    result$path_coefficients <- p.Coef
    result$outer_loadings <- NULL #<- o.load
    result$cross_loadings <- c.load
    result$total_effects <- NULL
    result$inner_weights <- inner.w
    #result$outer_weights <- NULL
    result$tolerance <- stop.criteria
    result$iterations <- i
    result$outer_weights <- outer.w ## always updated
    result$z <- z
    result$Y <- Y
    result$z <- scale(m.aux)
    result$weighting_scheme <- wscheme
    result$data <- data
    result$outm <- outermodel
    result$r.square<-r.square
    result$.Gof<- Gof
    result$Cronbach.alpha<-Cronbach.alpha
    result$Dillon.Goldstein<-Dillon.Goldstein
    result$Redundancy.indexes<-Redundancy.indexes
    result$Communality<-Communality
    
    message("End!")
    #close(pb)
    return(result)
  }
  else
    warning('INNER MODEL STRUCTURE MUST BE A MATRIX')
}
