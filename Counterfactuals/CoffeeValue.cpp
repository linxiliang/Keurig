#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

// [[Rcpp::export]]
double cvfun(double e1, double m0, double rho, NumericVector alpha, NumericVector u0, NumericVector price){
  int size = alpha.size();
  NumericVector psi = exp(u0 + log(price));
  NumericVector e(size);
  e[0] = e1;
  double v1 = u0[0]+(alpha[0]-1)*log(e[0]/price[0]+1);
  if (size>=2){
    for (int i=1; i<size; i++){
      e[i] = price[i]*(exp((v1-u0[i])/(alpha[i]-1))-1);
    }
  }
  double xiv = 0;
  for (int i=0; i<size; i++){
    xiv = xiv + psi[i]/alpha[i] * (pow(e[i]/price[i]+1, alpha[i])-1);
  }
  double fv =log(rho) + (rho-1)*log(xiv) + v1 - m0;
  return fv;
}

// [[Rcpp::export]]
double cufun(double e1, double m0, double rho, NumericVector alpha, NumericVector u0, NumericVector price){
  int size = alpha.size();
  NumericVector psi = exp(u0 + log(price));
  NumericVector e(size);
  e[0] = e1;
  double v1 = u0[0]+(alpha[0]-1)*log(e[0]/price[0]+1);
  if (size>=2){
    for (int i=1; i<size; i++){
      e[i] = price[i]*(exp((v1-u0[i])/(alpha[i]-1))-1);
    }
  }
  double xiv = 0;
  for (int i=0; i<size; i++){
    xiv = xiv + psi[i]/alpha[i] * (pow(e[i]/price[i]+1, alpha[i])-1);
  }
  double fv = pow(xiv, rho) - exp(m0)*sum(e);
  return fv;
}

// [[Rcpp::export]]
double ccvfun(double e1, double m0, double rho, NumericVector alpha, NumericVector u0, NumericVector price){
  int size = alpha.size();
  NumericVector psi = exp(u0 + log(price));
  NumericVector e(size);
  e[0] = e1;
  double v1 = u0[0]+(alpha[0]-1)*log(e[0]/price[0]+1);
  if (size>=2){
    for (int i=1; i<size; i++){
      e[i] = price[i]*(exp((v1-u0[i])/(alpha[i]-1))-1);
    }
  }
  double xiv = 0;
  for (int i=0; i<size; i++){
    xiv = xiv + psi[i]/alpha[i] * (pow(e[i]/price[i]+1, alpha[i])-1);
  }
  double fv = 1/exp(m0) * pow(xiv, rho) - sum(e);
  return fv;
}

