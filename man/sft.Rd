% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sft.R
\name{sft}
\alias{sft}
\title{Selection Frequency Threshold}
\usage{
sft(x, alpha)
}
\arguments{
\item{x}{a \code{randomForest} or \code{ranger} object}

\item{alpha}{a false positive rate (ie, 0.01)}
}
\value{
a list of two elements
\itemize{
\item \strong{sft} Tthe selection frequency threshold
\item \strong{probs_atsft} The esimated false positive rate
}
}
\description{
Determine the selecton frequency threshold of a model at a specified false positive rate
}
\examples{
library(randomForest)
data(iris)
iris.rf <- randomForest(iris[,-5], iris[,5], forest = TRUE)

# For a false positive rate of 1\%
iris.sft <- sft(iris.rf, 0.01)
print(iris.sft)

# To iterate through a range of alpha values

alpha <- c(0.01,0.05, 0.1,0.15,0.2, 0.25)
threshold <- NULL
for(i in seq_along(alpha)){
    threshold[i] <- sft(iris.rf, alpha[i])$sft
}

plot(alpha, threshold, type = 'b')

}
\author{
Tom Wilson \email{tpw2@aber.ac.uk}
}
