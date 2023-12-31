#' Greedy Heuristic for Knapsack Problem
#'
#' @description The algorithm solves the knapsack problem using Greedy approach. Its computational 
#' complexity is O(n*log(n))
#' 
#' @param x a data.frame with two variables v (Value) and w (Weight) and returns the maximum knapsack value and which elements (rows in the data.frame).
#' @param W is the knapsack size.
#'
#' @return It returns the maximum value obtained and selected items.
#' @seealso [Knapsack Problem](https://en.wikipedia.org/wiki/Knapsack_problem#Greedy_approximation_algorithm)
#' @export
#'
#' @examples
#' x <- data.frame(v = c(10, 5, 15, 7, 6, 18), w = c(2, 3, 5, 7, 1, 4))
#' W <- 15
#' greedy_knapsack(x,W)

greedy_knapsack <- function(x, W) {
  if (W < 0){
    stop("Capacity cannot be negative!")
  }
  
  # Calculate the ratio
  x$value_per_weight <- x$v / x$w
  
  # Sort items according to ratio
  x <- x[order(-x$value_per_weight), ]
  
  # Initialize variables
  n <- nrow(x)
  knapsack <- rep(0, n)
  total_value <- 0
  remaining_capacity <- W
  
  # Fill the knapsack
  for (i in 1:n) {
    if (remaining_capacity == 0) {
      break
    }
    
    if (x$w[i] <= remaining_capacity) {
      knapsack[i] <- 1
      total_value <- total_value + x$v[i]
      remaining_capacity <- remaining_capacity - x$w[i]
    } else {
      # Take a fraction of the item
      fraction <- remaining_capacity / x$w[i]
      knapsack[i] <- fraction
      remaining_capacity <- 0
    }
  }
  
  selected <- max(which(knapsack == 1))
  selected_idx <- (rownames(x))[1:selected]
  selected_idx <- as.numeric(selected_idx)
  return(list(value = total_value, elements = selected_idx))
}
