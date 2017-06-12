# result$coefficients  returns structure of data with (loadings + path coefficients) x 1
# results$values returns the effect in text ex: IMAGE->IMAGE1
coefficients <-
  function(path_coefficients,
           inm,
           outerloadings,
           outm,
           mod = "A") {
    inm <- model$innermodel
    outm <- as.matrix(outm)
    outerloadings <- as.matrix(outerloadings)

    ##path coefficients

    ind <- which(inm == 1, arr.ind = TRUE)
    col2 <-
      paste(rownames(inm)[ind[, 1]], "->", colnames(inm)[ind[, 2]], sep = "")

    indval <- which(inm == 1)
    val <- path_coefficients[indval]

    ## loadings
    ind2 <- which(outm == 1, arr.ind = TRUE)

    if (mod == "A"){
      col1 <-
      paste(colnames(outm)[ind2[, 2]], sep = "", "->", rownames(outm)[ind2[, 1]])
    }
    else {
      col1 <-
      paste(rownames(outm)[ind2[, 1]], "->", colnames(outm)[ind2[, 2]], sep =
              "")
    }
    indval2 <- which(outm == 1)
    val2 <- outerloadings[indval2]

    path <- cbind.data.frame(c(n1,n2), c(c1,c2))
    names(path) <- c("Links","Coefficients")

    return(path)
  }

