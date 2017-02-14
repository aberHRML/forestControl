#' Selection Frequency Threshold
#'
#' @rdname sft
#' @param x a \code{randomForest} or \code{ranger} object
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
  return(sft_res)
  }
