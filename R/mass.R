mass <- function(q, t){
  n <- length(t) ; w <- length(q)

  meany = mean(q)
  sigmay = std(q)
  meanx = movmean(x = t, w = (w-1))
  sigmax = movstd(x = t, w = (w-1))

  q <- rev(q)
  q <- append(q, rep(0, (n-w)))
  options(digits = 20)
  z <- ifft(fft(t) * (fft(q)))
  z <- Re(z)
  options(warn = -1)
  dist <- 2*(w - (z[w:n] - w*meanx[w:n]*meany)/(sigmax[w:n]*sigmay))
  dist <- (sqrt(dist))
  dist <- replace(dist, is.na(dist), 0)

  return(dist)
}
