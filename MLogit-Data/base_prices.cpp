#define ARMA_64BIT_WORD
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector cmax(NumericVector x) {
  int size = x.size();
  LogicalVector l = is_na(x);
  NumericVector y(size);
  for (int i = 0; i<size; i++){
    if (l[i]!=TRUE){
      y[i]=x[i];
    }
  }
  NumericVector z(size, max(y));
  return z;
}

// [[Rcpp::export]]
NumericVector smax(NumericVector x, double k) {
  int size = x.size();
  LogicalVector l = is_na(x);
  if (all(is_na(x))){
    NumericVector y(size, k);
    return y;
  } else{
    NumericVector y(size);
    for (int i = 0; i<size; i++){
      if (l[i]!=TRUE){
        y[i]=x[i];
      }
    }
    if (max(y)<k){
      int j = 0;
      for (int i = 0; i<size; i++){
        if ((l[i]==TRUE) & (j==0)){
          y[i]=k;
        } else{
          y[i]=max(y);
          j = 1;
        }
      }
      return y;
    } else{
      if (max(y)==k){
        NumericVector z(size, k); 
        return z;
      } else{
        int j = 0;
        for (int i = 0; i<size; i++){
          if ((y[i]<max(y)) & (y[i]!=0) & (j==0)){
            y[i]=k;
          } else{
            y[i]=max(y);
            j = 1;
          }
        }
        return y;
      }
    }
  } 
}

// [[Rcpp::export]]
NumericVector csort(NumericVector x) {
  NumericVector y = clone(x);
  std::sort(y.begin(), y.end());
  return y;
}

// [[Rcpp::export]]
double cquantile(NumericVector x, double k){
  if (x.size()==0){
    return NA_REAL;
  } else{
    double qval = (x.size() - 1)* k;
    int pos1 = floor(qval);
    int pos2 = ceil(qval);
    NumericVector y = csort(x);
    if (pos1 == pos2){
      return y[pos2];
    } else{
      return (y[pos2]-y[pos1])*(qval-pos1)+y[pos1];
    }
  }
}

// [[Rcpp::export]]
NumericVector subnv(NumericVector x, int i, int j){
  NumericVector y( x.begin()+i-1, x.begin()+j );
  return y;
}

// [[Rcpp::export]]
NumericVector basevector(NumericVector x, int w){
  NumericVector y(x.size());
  if (x.size()<=w){
    return cmax(x);
  }
  else {
    y[0] = cmax(subnv(x, 1, w))[0];
    NumericVector z(w);
    for (int i = 1; i<x.size(); i++){
      if (i <= (x.size()-w)){
        z = smax(subnv(x, (i), (w+i)), y[i-1]);
        y[i] = z[0];
      } else{
        y[i] = z[w-(x.size()-i)];
      }
    }
    return y;
  }
}

// [[Rcpp::export]]
NumericVector baseprice(DataFrame a, DataFrame b, int w){
  NumericVector start = a("start");
  NumericVector end = a("end");
  NumericVector vars = b("price_mid");
  int n = start.size();
  //Create a data matrix to store values
  NumericVector DM(vars.size());
  NumericVector y(end[1]);
  int k;
  
  for (int i = 0; i<n; i++){
    //NumericVector k = subnv(vars, start[i], end[i]);
    y = basevector(subnv(vars, start[i], end[i]), w);
    for (int j = (start[i]-1); j<(end[i]); j++){
      k = j - (start[i]-1);
      DM[j] = y[k];
    }
  }
  return(DM);
}

// [[Rcpp::export]]
NumericVector mavg(NumericVector x, double k) {
  int size = x.size();
  NumericVector y(size);
  
  for (int i = 0; i<size; i++){
    if (i<1){
      y[i] = x[i];
    } else{
      y[i] = (1-k) * y[i-1] + k * x[i-1];
    }
  }
  
  return(y);
}


// [[Rcpp::export]]
NumericVector mavgn(NumericVector x, double k) {
  int size = x.size();
  NumericVector y(size);
  
  for (int i = 0; i<size; i++){
    if (i<1){
      y[i] = x[i];
    } else{
      y[i] = (1-k) * y[i-1] + k * x[i];
    }
  }
  
  return(y);
}

