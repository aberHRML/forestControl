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



sft <- function(x, alpha){
  UseMethod("sft")
}


sft.default <- function(x,...){
  warning(paste("sft can only be used with classes randomForest and ranger"))
}
