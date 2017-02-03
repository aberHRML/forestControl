#'
#'
#'
#'
#'
#'
#' @keywords internals

nCm_ratio <- function(n1,m1,n2,m2){
  if (m1 > n1){
    return(0)
  }else{
    if (m2 > n2) {
      return(0)
    }
  }

  RN1 <- sum(log(seq(1,n1)))
  RM1 <- sum(log(seq(1,m1)))
  RNM1 <- sum(log(seq(1,n1 - m1)))
  RN2 <- sum(log(seq(1,n2)))
  RM2 <- sum(log(seq(1,m2)))
  RNM2 <- sum(log(seq(1,n2 - m2)))

  return(exp(RN1 - RM1 - RNM1 - RN2 + RM2 + RNM2))
}



prob_Ckt <- function(F, N, Fn, K, k){
  p_Ct <- nCm_ratio(Ft - 1, Fn - 1, Ft, Fn)
  p <- p_Ct * dbinom(1,K,1/Fn)
  return(p)
}


prob_Cft <- function(Ft,N,Fn){
  return(prob_Ckt(F, N = 0, Fn, 1,1))
}
