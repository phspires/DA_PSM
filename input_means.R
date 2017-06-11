
# replaces NA values for column mean 
input.means = function(data)
{
  #Missing values input with mean
  data.means = colMeans(data, na.rm = TRUE)
  
  for (i in 1:ncol(data)) {
    data[is.na(data[, i]), i] = data.means[i]
  }
  
  return(data)
}