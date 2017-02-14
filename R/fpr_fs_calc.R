#'
#'
#'
#'
#'
#'
#'
#'
#'
#'


fpr_fs_calc <- function(k,Ft,Fn,Tr,K)
  {

  SF_FPR <- function(k,Ft,Fn,Tr,K){
  p_Ct <- nCm_ratio(Ft - 1, Fn - 1, Ft, Fn)
  p <- p_Ct * dbinom(1,1,1/Fn)
  p <- dbinom(k,Tr*K,p)
  return(p)
  }

  if (k < 20) {
    val <- 20
  }else{
    if (k < round(Tr * K * 2/Ft)) {
      val <- round(Tr * K * 2/Ft)
  }else{
      val <- k
  }
}

  p <- sapply(0:val,SF_FPR,Ft = Ft,Fn = Fn,Tr = Tr,K = K)

  p <- cumsum(p[length(p):1])
  p <- p[length(p):1]
  p <- round(p,7)
  return(p[k + 1])

  }





