movstd <- function(x = x, w = w){
  temp.sd <- numeric(length(x))
  w <- w+1
  for(k in 2:(w-1)){
    temp.sd[k] <- std(x[1:k])
  }
  temp.sd <- na.omit(c(temp.sd, (runSD(x, w) *sqrt((w-1)/(w)))))

  return(c(0, temp.sd))
}



