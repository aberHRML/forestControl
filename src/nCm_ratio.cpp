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

    IntegerVector RN1 = seq_along(Sn1);
    RN1 = sum(log(RN1));

    IntegerVector RM1 = seq_along(Sm1);
    RM1 = sum(log(RM1));

    IntegerVector RNM1 = seq_along(Snm1);
    RNM1 = sum(log(RNM1));

    IntegerVector RN2 = seq_along(Sn2);
    RN2 = sum(log(RN2));

    IntegerVector RM2 = seq_along(Sm2);
    RM2 = sum(log(RM2));

    IntegerVector RNM2 = seq_along(Snm2);
    RNM2 = sum(log(RNM2));

    double R = exp(RN1 - RM1 - RNM1 - RN2 + RM2 + RNM2);

    return(R);
  }
}
