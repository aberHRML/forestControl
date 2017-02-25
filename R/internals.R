#' @keywords internals



is.rf <- function(x)inherits(x, "randomForest")


is.ranger <- function(x)inherits(x, "ranger")


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



prob_Ckt <- function(Ft, N, Fn, K, k){
  p_Ct <- nCm_ratio(Ft - 1, Fn - 1, Ft, Fn)
  p <- p_Ct * dbinom(1,K,1/Fn)
  return(p)
}


prob_Cft <- function(Ft,N,Fn){
  return(prob_Ckt(Ft, N = 0, Fn,1,1))
}


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



#' Selection Frequency Threshold
#'
#' Calculate the selection frequency threshold for a give approximate false postive rate
#'
#' @param Ft the total number of features
#' @param Fn the number of features considered at each internal node (mtry)
#' @param K the average number of binary tests/internal nodes across the enitre forest
#' @param Tr the total number of trees in the forest
#' @param alpha a false positive rate (ie, 0.01)
#' @return a list of two elements
#' \describe{
#'     \item{sft}{the selection frequency threshold}
#'     \item{probs_atsft}{the esimated false positive rate}
#' }
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @author Jasen Finch \email{jsf9@aber.ac.uk}
#'
#' @keywords internal
#' @seealso \code{\link{extract_params}}

sft_calc <- function(Ft, Fn, K, Tr, alpha)
{

  max_val <- pmax(20, (Tr * K * 2/Ft))

  log_range <- log(1: (Tr*K))

  probs <- NULL
  for(i in seq_along(1:max_val)){
    probs[[i]] <- dbinom(i, K*Tr, prob_Cft(Ft, N = 0, Fn))
  }

  cprobs_null <- cumsum(probs)
  ind <- which(1 - cprobs_null <= alpha)[1]
  sft = ind + 1

  probs_atsft <- 1 - cprobs_null[ind]

  return(list(sft = sft, prob = probs_atsft))
}




