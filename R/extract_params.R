#' Extract forest parameters
#'
#' For a forest model (randomForest or ranger) extract the parameters needed to calculate an approximate selection frequency threshold
#'
#' @param x a \code{randomForest} or \code{ranger} object
#' @return a list of four elements
#' \describe{
#'     \item{\strong{Fn}}{The number of features considered at each internal node (mtry)}
#'     \item{\strong{Ft}}{The total number of features in the data set}
#'     \item{\strong{K}}{The average number of binary tests/internal nodes across the enitre forest}
#'     \item{\strong{Tr}}{The total number of trees in the forest}
#'  }
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export


extract_params <- function(x)
  {

  if(is.rf(x) == FALSE & is.ranger(x) == FALSE){
    stop(deparse(substitute(x)), " is not a valid randomForest or ranger object", call. = FALSE)
  }

  if(is.null(x$forest)){
    stop(deparse(substitute(x)), " has no forest", call. = FALSE)
  }


  if(class(x) == "randomForest"){
          Fn <- x$mtry
          Fe <- nrow(x$importance)
          K <- round(mean(apply(x$forest$nodestatus,2,function(x)(length(which(x == 1))))), digits = 0)
          Tr <- x$forest$ntree

  }

  if(class(x) == "ranger"){
          Fn <- x$mtry
          Fe <- x$num.independent.variables
          K <- round(mean(sapply(x$forest$split.varIDs, function(x)(length(which(x != 0))))), digits = 0)
          Tr <- x$num.trees
  }

  return(list(Fn = Fn,Ft = Fe, K = K, Tr = Tr))
  }
