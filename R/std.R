std <- function(x){
  result <- sd(x)*sqrt((length(x)-1)/length(x))
  return(result)
}
