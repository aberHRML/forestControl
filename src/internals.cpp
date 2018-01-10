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
double SF_FPR2(double k,double Ft,double Fn,double Tr,double K){
  double p_Ct = nCm_ratio2(Ft - 1, Fn - 1, Ft, Fn);
  double p = p_Ct * R::dbinom(1,1,1/Fn,0);
  return R::dbinom(k,Tr*K,p,0);
}


// [[Rcpp::export]]
NumericVector fpr_fs_calc2(double k,double Ft,double Fn,double Tr,double K){
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

  for(int i = 0;i < val + 1; ++i){
    p(i) = SF_FPR2(i,Ft,Fn,Tr,K);
  }

  NumericVector p1 = rev(p);
  // NumericVector p2 = cumsum(p1);
  // NumericVector p3 = rev(p2);
  // p3 = round(p3,7);
  // return p3[k + 1];
  return p;
}

// [[Rcpp::export]]
NumericVector sft_calc2(double Ft, double Fn, double K, double Tr, double alpha){

  NumericVector max_val = pmax(NumericVector::create(20), Tr * K * 2/Ft);
  NumericVector log_range =  log(seq_len(Tr*K));

  NumericVector probs(max_val[0]);

  int mv = max_val[0];

  for(int i = 0; i < mv; ++i){
    double j = i + 1;
    probs(i) = R::dbinom(j, K*Tr, prob_Cft2(Ft,Fn),0);
  }

  NumericVector cprobs_null = cumsum(probs);
  LogicalVector sub(cprobs_null.size());

  for(int i = 0; i < sub.size(); ++i){
    sub(i) = 1- cprobs_null(i) <= alpha;
  }
  NumericVector ind = cprobs_null[sub];
  int ind1 = ind.size();

  double sft = round(ind1 + 1);

  double probs_atsft = cprobs_null[ind1];

  NumericVector r = NumericVector::create(sft,probs_atsft);

  return r;
}
