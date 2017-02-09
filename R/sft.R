#' @rdname sft
#' @export

sft.randomForest <- function(x, alpha)
  {
  params <- extract_params(x)
  sft_res <- sft_calc(Ft = params$Ft,Fn = params$Fn, K = params$K, Tr = params$Tr, alpha = alpha)
  return(sft_res)
  }


#' @rdname sft
#' @export
sft.ranger <- sft.randomForest
