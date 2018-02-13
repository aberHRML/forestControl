#' Selection Frequency Threshold
#'
#'
#' @param x a `randomForest` or `ranger` object
#' @param alpha a false positive rate (ie, 0.01)
#' @return a list of two elements
#' \describe{
#'     \item{sft}{the selection frequency threshold}
#'     \item{probs_atsft}{the esimated false positive rate}
#' }
#'
#' @author Tom Wilson \email{tpw2@aber.ac.uk}
#' @export

sft <- function(x, alpha)
  {
  params <- extract_params(x)
  sft_res <- sft_calc(Ft = params$Ft,Fn = params$Fn, K = params$K, Tr = params$Tr, alpha = alpha)
  sft_res <- list(sft = round(sft_res[1]), prob = sft_res[2])
  return(sft_res)
  }
