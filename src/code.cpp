#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector pinp_char(CharacterVector x, CharacterVector y) {
  LogicalVector out = in(x,y);
  return out;
}
