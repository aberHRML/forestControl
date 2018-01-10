#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double nCm_ratio2(double n1,double m1,double n2,double m2) {
  if (m1 > n1){
    return(0);
  } else {
    if (m2 > n2) {
      return(0);
    }

    IntegerVector sRN1 = seq_len(n1);
    IntegerVector sRM1 = seq_len(m1);
    IntegerVector sRNM1 = seq_len(n1 - m1);
    IntegerVector sRN2 = seq_len(n2);
    IntegerVector sRM2 = seq_len(m2);
    IntegerVector sRNM2 = seq_len(n2-m2);

    NumericVector lRN1 = log(sRN1);
    double RN1 = sum(lRN1);

    NumericVector lRM1 = log(sRM1);
    double RM1 = sum(lRM1);

    NumericVector lRNM1 = log(sRNM1);
    double RNM1 = sum(lRNM1);

    NumericVector lRN2 = log(sRN2);
    double RN2 = sum(lRN2);

    NumericVector lRM2 = log(sRM2);
    double RM2 = sum(lRM2);

    NumericVector lRNM2 = log(sRNM2);
    double RNM2 = sum(lRNM2);

    return exp(RN1 - RM1 - RNM1 - RN2 + RM2 + RNM2);
  }
}

// [[Rcpp::export]]
double prob_Ckt2(double Ft, double N, double Fn, double K, double k){
  double p_Ct = nCm_ratio2(Ft - 1, Fn - 1, Ft, Fn);
  double p = p_Ct * R::dbinom(1,K,1/Fn,0);
  return p;
}

// [[Rcpp::export]]
double prob_Cft2(double Ft,double Fn){
  return prob_Ckt2(Ft, 0, Fn,1,1);
}

// [[Rcpp::export]]
double SF_FPR(double k,double Ft,double Fn,double Tr,double K){
  double p_Ct = nCm_ratio2(Ft - 1, Fn - 1, Ft, Fn);
  double p = p_Ct * R::dbinom(1,1,1/Fn,0);
  return R::dbinom(k,Tr*K,p,0);
}


// [[Rcpp::export]]
double fpr_fs_calc2(double k,double Ft,double Fn,double Tr,double K){
  int val;

  if (k < 20) {
    val = 20;
  }else{
    if (k < round(Tr * K * 2/Ft)) {
      val = round(Tr * K * 2/Ft);
    }else{
      val = k;
    }
  }

  NumericVector p(val+1);

  for(int i = 0;i < val +1; ++i){
    p[i] = SF_FPR(i,Ft,Fn,Tr,K);
  }

  NumericVector p1 = cumsum(rev(p));
  NumericVector p2 = rev(p1);
  p2 = round(p2,7);
  return p2[k + 1];
}
