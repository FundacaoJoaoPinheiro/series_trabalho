teste_H <-  function(vt){
  n <- length(vt)
  h <- round((n)/3)
  H <- sum(vt[(n-h+1):n]^2)/sum(vt[1:h]^2)
  return(c(round(1-pf(H,h,h),4)))
}
