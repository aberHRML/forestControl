#' False Postivie Rate Feature Selection
#'
#' Calculate the False Positive Rate (FPR) for each feature using it's selection frequency
#'
#' @param x a `randomForest` or `ranger` object
#' @return a `data.frame` of selection frequencies and their false positive rate
#'
#' @author Jasen Finch \email{jsf9@@aber.ac.uk}
#' @importFrom purrr map_dbl
#' @export
#' @examples
#' library(randomForest)
#' data(iris)
#' iris.rf <- randomForest(iris[,-5], iris[,5], forest = TRUE)
#'
#' iris.features <- fpr_fs(iris.rf)
#' print(iris.features)

fpr_fs <- function(x)
  {

  if(is.rf(x) == FALSE & is.ranger(x) == FALSE){
    stop(deparse(substitute(x)), " is not a valid randomForest or ranger object", call. = FALSE)
  }

  if(is.null(x$forest)){
    stop(deparse(substitute(x)), " has no forest", call. = FALSE)
  }

  params <- extract_params(x)
  freq <- selection_freqs(x)
  fpr <- map_dbl(freq$freq,fpr_fs_calc,Ft = params$Ft, Fn = params$Fn, Tr = params$Tr, K = params$K)

  feat_sel <- data.frame(freq, fpr = fpr)

  return(feat_sel)
  }
