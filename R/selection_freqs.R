#' Variable Selection Frequencies
#'
#' Extract variable selection frequencies from `randomForest` and `ranger` model objects
#'
#' @param x a `randomForest` or `ranger` object
#' @return `data.frame` of variable selection frequencies
#'
#' @export
#' @examples
#' library(randomForest)
#' data(iris)
#' iris.rf <- randomForest(iris[,-5], iris[,5], forest = TRUE)
#'
#' iris.freqs <- selection_freqs(iris.rf)
#' print(iris.freqs)


selection_freqs <- function(x){

if(is.rf(x) == FALSE & is.ranger(x) == FALSE){
    stop(deparse(substitute(x)), " is not a valid randomForest or ranger object", call. = FALSE)
}

if(is.null(x$forest)){
  stop(deparse(substitute(x)), " has no forest", call. = FALSE)
}


if(class(x) == "randomForest"){
  var <- numeric(length(x$forest$ncat))
  var_freqs <- table(x$forest$bestvar[x$forest$bestvar > 0])
  var[as.numeric(names(var_freqs))] <- var_freqs
  names(var) <- names(x$forest$xlevels)
  var_df <- data.frame(freq = var)
}


if(class(x) == "ranger"){
  var <- numeric(length(x$forest$independent.variable.names))
  var_freqs <- table(unlist(x$forest$split.varIDs)[unlist(x$forest$split.varIDs) > 0])
  var[as.numeric(names(var_freqs))] <- var_freqs
  names(var) <- x$forest$independent.variable.names
  var_df <- data.frame(freq = var)
}


return(var_df)
}



