#' Dynammic Programming for Knapsack Problem
#'
#' @description The algorithm solves the knapsack problem using Dynammic Programming approach. 
#' Its computational complexity is O(Wn).
#' @param x 
#' @param W 
#'
#' @return It returns the maximum value obtained and selected items.
#' @seealso [Knapsack Problem](https://en.wikipedia.org/wiki/Knapsack_problem#0.2F1_knapsack_problem)
#' @export
#'
#' @examples
#' x <- data.frame(v = c(10, 5, 15, 7, 6, 18), w = c(2, 3, 5, 7, 1, 4))
#' W <- 15
#' knapsack_dynamic(x,W)

knapsack_dynamic <- function(x, W){
  #Initialize the matrix
  n <- nrow(x)
  m <- matrix(0, n+1, W+1)
  
  #Filling the matrix by calculating the values
  for (i in 2:(n + 1)) { 
    for (j in 2:(W + 1)) { 
      if (x[i - 1, 'w'] > j) {
        m[i, j] <- m[i - 1, j] 
      } else {
        m[i, j] <- max(m[i - 1, j], m[i - 1, j - x[i - 1, 'w']] + x[i - 1, 'v'])
      }
    }
  }
  
  i <- n+1
  w <- W+1
  max_value <- m[i,w]
  list_idx <- rep(0, n)
  value_to_check <- m[i,w]
  
  #Checking which elements are selected
  while (i>0){
    upper_row <- m[i-1, ]
    value_exists <- any(upper_row == value_to_check)
    if (value_exists){
      i <- i-1 
    } else {
      list_idx[i-1] <- 1
      value_to_check <- value_to_check - x[i - 1, 'v']
      i <- i-1
    }
  }
  
  selected <- which(list_idx == 1)
  return(list(value = max_value, elements = selected))
}