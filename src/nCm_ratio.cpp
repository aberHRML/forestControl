#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double nCm_ratio(double n1,double m1,double n2,double m2) {
  if (m1 > n1){
    return(0);
  } else {
    if (m2 > n2) {
      return(0);
    }

    IntegerVector Sn1 = IntegerVector::create( 0, n1, NA_INTEGER, 3 );
    IntegerVector Sm1 = IntegerVector::create( 0, m1, NA_INTEGER, 3 );
    IntegerVector Snm1 = IntegerVector::create( 0, n1 - m1, NA_INTEGER, 3 );
    IntegerVector Sn2 = IntegerVector::create( 0, n2, NA_INTEGER, 3 );
    IntegerVector Sm2 = IntegerVector::create( 0, m2, NA_INTEGER, 3 );
    IntegerVector Snm2 = IntegerVector::create( 0, n2 - m2, NA_INTEGER, 3 );

    IntegerVector sRN1 = seq_along(Sn1);
    NumericVector lRN1 = log(sRN1);
    double RN1 = sum(lRN1);

    IntegerVector sRM1 = seq_along(Sm1);
    NumericVector lRM1 = log(sRM1);
    double RM1 = sum(lRM1);

    IntegerVector sRNM1 = seq_along(Snm1);
    NumericVector lRNM1 = log(sRNM1);
    double RNM1 = sum(lRNM1);

    IntegerVector sRN2 = seq_along(Sn2);
    NumericVector lRN2 = log(sRN2);
    double RN2 = sum(lRN2);

    IntegerVector sRM2 = seq_along(Sm2);
    NumericVector lRM2 = log(sRM2);
    double RM2 = sum(lRM2);

    IntegerVector sRNM2 = seq_along(Snm2);
    NumericVector lRNM2 = log(sRNM2);
    double RNM2 = sum(lRNM2);

    double R = exp(RN1 - RM1 - RNM1 - RN2 + RM2 + RNM2);

    return(R);
  }
}
