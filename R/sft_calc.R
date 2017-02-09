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
#' @export
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









