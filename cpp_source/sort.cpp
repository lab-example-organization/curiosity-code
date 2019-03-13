#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector cpp_sort_indices(const NumericVector &v) {
  NumericVector idx(v.size());
  for (size_t i = 1; i != idx.size() + 1; i++) idx(i-1) = i;

  // sort indexes based on comparing values in v
  std::sort(idx.begin(), idx.end(),
            [&v](size_t i1, size_t i2) {return v[i1 - 1] < v[i2 - 1];});

  return idx;
}
