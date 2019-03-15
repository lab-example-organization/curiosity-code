#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double cpp_med(NumericVector x) {
  std::size_t size;
  size = x.size();
  if (size % 2 == 0) {
    return (x[size / 2 - 1] + x[size / 2]) / 2.0;
  }
  else {
    return x[size / 2];
  }
}
