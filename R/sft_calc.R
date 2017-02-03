#' Selection Frequency Threshold
#'
#'
#'
#'
#'




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









