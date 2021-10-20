// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>

using namespace Rcpp;

// [[Rcpp::export]]
arma::vec colMediansArma(arma::sp_mat& x, bool ignore_missing) {

  bool is_even;
  bool is_na;
  int half;
  int numel;
  arma::vec vals;
  arma::sp_mat colvals;
  
  arma::vec med = arma::zeros<arma::vec>(x.n_cols);

  if (!ignore_missing) {
    is_even=((x.n_rows) % 2 == 0);
    half=(int)((x.n_rows)/2);
  }

  for (size_t i = 0; i != x.n_cols; ++i) {

    colvals = x.col(i);
    is_na = FALSE;
    
    if (ignore_missing) {
      
      vals = arma::zeros<arma::vec>(x.n_rows);
      
      numel = 0;
      for (int j = 0; j != x.n_rows; ++j) {
        if (colvals[j] != 0) {
          vals[numel] = colvals[j];
          numel++;
        }
      }
      
      is_even = ((numel) % 2 == 0);
      half = (int)((numel) / 2);
      
    } else {
      
      vals = arma::zeros<arma::vec>(x.n_rows);
      for (int j = 0; j != x.n_rows; ++j) {
        if (colvals[j] == 0) {
          med[i] = NA_REAL;
          is_na = TRUE;
          break;
        }
        vals[j] = colvals[j];
      }
      
      if (is_na) {
        continue;
      }
      
      numel = x.n_rows;
    }
    
    if (numel == 0) {
      med[i] = 0;
    } else {
      
      if (numel < x.n_rows) {
        vals = vals.head(numel);
      }
      vals = arma::sort(vals);
      
      if (is_even) {
        med[i] = (vals[half] + vals[half - 1]) / 2;
      } else {
        med[i] = vals[half];
      }
      
    }

  }
  
  return(med);
}


// [[Rcpp::export]]
arma::vec colMeansArma(arma::sp_mat& x, bool ignore_missing) {
  int numel;
  double sigma;
  bool is_na;
  arma::sp_mat colvals;
  
  arma::vec mu = arma::zeros<arma::vec>(x.n_cols);
  
  for (size_t i = 0; i != x.n_cols; ++i) {
    
    colvals = x.col(i);
    is_na = FALSE;
    
    if (ignore_missing) {
      numel = 0;
      for (int j = 0; j != x.n_rows; ++j) {
        if (colvals[j] != 0) {
          sigma += colvals[j];
          numel++;
        }
      }
      
    } else {
      
      // If any element is zero = missing, return NA
      for (int j = 0; j != x.n_rows; ++j) {
        if (colvals[j] == 0) {
          mu[i] = NA_REAL;
          is_na = TRUE;
          break;
        }
      }
      
      if (is_na) {
        continue;
      }
      
      numel = x.n_rows;  // In this case use all elements
      
    }
    
    // If no element is different from zero, return NA
    if (numel == 0) {
      
      mu[i] = NA_REAL;
      
    } else {
      
      sigma = 0;
      for (int j = 0; j != x.n_rows; ++j) {
        sigma += colvals[j];
      }
      
      mu[i] = sigma / numel;
      
    }
    
  }
  
  return(mu);
}