#' False Postivie Rate Feature Selection
#'
#'
#'
#'
#'
#'
#' @export

fpr_fs <- function(x)
  {

  if(is.rf(x) == FALSE & is.ranger(x) == FALSE){
    stop(deparse(substitute(x)), " is not a valid randomForest or ranger object", call. = FALSE)
  }

  if(is.null(x$forest)){
    stop(deparse(substitute(x)), " has no forest", call. = FALSE)
  }

  params <- extract_params(x)
  freq <- selecFreqs(x)
  fpr <- lapply(freq$freq, function(x)(fpr_fs_calc(k = x,Ft = params$Ft, Fn = params$Fn, Tr = params$Tr, K = params$K)))

  feat_sel <- data.frame(freq, fpr = unlist(fpr))

  return(feat_sel)
  }
