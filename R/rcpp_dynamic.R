library(Rcpp)

cppFunction('
List knapsack_dynamic_cpp(NumericMatrix x, int W) {
  int n = x.nrow();
  NumericMatrix m(n + 1, W + 1);
  
  for (int i = 1; i <= (n); i++) {
    for (int j = 1; j <= (W); j++) {
      if (x(i - 1,0) > j) {
        m(i, j) = m(i - 1, j);
      } else {
        m(i, j) = std::max(m(i - 1, j), m(i - 1, j - x(i - 1, 0)) + x(i - 1, 1));
      }
    }
  }

  int i = n;
  int w = W;
  double max_value = m(i, w);
  
  IntegerVector list_idx(n);
  double value_to_check = m(i, w);
  
  while (i > 0) {
    NumericVector upper_row = m(i - 1, _);
    bool value_exists = std::any_of(upper_row.begin(), upper_row.end(), [&](double val) { return val == value_to_check; });
    if (value_exists) {
      i--;
    } else {
      list_idx(i - 1) = 1;
      value_to_check -= x(i - 1, 1);
      i--;
    }
  }

  IntegerVector selected;
  for (int i = 0; i < n; i++) {
    if (list_idx[i] == 1) {
      selected.push_back(i + 1);
    }
  }
  List result = List::create(_["value"] = max_value, _["elements"] = selected);
  return result;
}
')