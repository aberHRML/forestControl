#' extract
#'
#' extact
#'
#'
#' @param x
#'
#' @export


extract_params <- function(x)
  {

  if(inherits(x, "randomForest") | inherits(x, "ranger")){
      type <- class(x)
  }else{
      stop("Input object must be either ranomForest or ranger")
  }

  if(type == "randomForest"){
      if(!is.null(x$forest)){
          Fn <- x$mtry
          Fe <- nrow(x$importance)
          K <- round(mean(apply(x$forest$nodestatus,2,function(x)(length(which(x == 1))))), digits = 0)
          Tr <- x$forest$ntree
          }else{
        stop("randomForest object must have forest data run model using;

                  keep.forest = TRUE", call. = FALSE)
      }
  }

  if(type == "ranger"){
      if(!is.null(x$forest)){
          Fn <- x$mtry
          Fe <- x$num.independent.variables
          K <- round(mean(sapply(x$forest$split.varIDs, function(x)(length(which(x != 0))))), digits = 0)
          Tr <- x$num.trees
      }else{
        stop("ranger object must have forest data run model using;

                  write.forest = TRUE", call. = FALSE)
      }
  }

  return(list(Fn = Fn,F = Fe, K = K, Tr = Tr))
  }












