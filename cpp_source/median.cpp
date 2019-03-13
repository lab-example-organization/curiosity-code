#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double cpp_med(Rcpp::NumericVector x) {
  std::size_t size = x.size();
  if (size % 2 == 0) return (x[size / 2 - 1] + x[size / 2]) / 2.0;
  return x[size / 2];
}
