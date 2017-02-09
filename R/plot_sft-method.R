#' Plot Selection Frequency Threshold
#'
#' @rdname plot_sft
#' @param x a randomForest of ranger forest object
#' @return a plot
#'
#' @author Tom Wilson \email{tpw2@aber.ac.uk}
#' @export



plot_sft <- function(x){
  UseMethod("plot_sft")
}


plot_sft.default <- function(x){
  warning(paste("sft can only be used with classes randomForest and ranger"))
}
