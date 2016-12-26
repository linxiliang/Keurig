#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double sumf(NumericVector x) {
  int n = x.size();
  double sum = 0; 
  for (int i = 0; i<n; i++){
    sum+=x[i];
  }
  return sum;
}
