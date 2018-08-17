movmean <- function(x = x, w = w){
  i = 1
  temp.mean <- numeric(length(x))
  w <- w+1
  for(k in 1:w){
    if(i < w){
      temp.mean[k] <- mean(x[1:i])
    }
    i <- i + 1
  }
  temp.mean <- c(temp.mean, rollapply(x, w, mean))
  return(temp.mean)
}
