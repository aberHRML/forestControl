#' False Postivie Rate Feature Selection
#'
#' Calculate the False Positive Rate (FPR) for each feature using it's selection frequency
#'
#' @param x a \code{randomForest} or \code{ranger} object
#' @return a \code{data.frame} of selection frequencies and their false positive rate
#'
#' @author Jasen Finch \email{jsf9@@aber.ac.uk}
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
  fpr <- lapply(freq$freq, function(x){fpr_fs_calc(k = x,Ft = params$Ft, Fn = params$Fn, Tr = params$Tr, K = params$K)})

  feat_sel <- data.frame(freq, fpr = unlist(fpr))

  return(feat_sel)
  }
