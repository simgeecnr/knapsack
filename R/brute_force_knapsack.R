#' Knapsack Brute Force
#'
#' @param x a data.frame with two variables v and w and returns the maximum knapsack value and which elements (rows in the data.frame).
#' @param W is the knapsack size.
#' @param parallel Default: False. Use paralleling to solve problem
#'
#' @return returns the maximum knapsack value and which elements (rows in the data.frame).
#' @export
#'
#' @examples

brute_force_knapsack <- function(x, W, parallel=FALSE){
  #The function should check that the inputs are correct (i.e. a data.frame with two variables v and w)
  #with only positive values.
  stopifnot(W >= 0)
  stopifnot(is.data.frame(x))
  stopifnot("v" %in% colnames(x) && "w" %in% colnames(x))
  
  #N being the number of options to put in the bag 
  #Create a list with all the binaries of the numbers between 1 and N, 
  # intToBits(n) returns a list of 32 0/1 for the binary rep of n, from left to right. the list is indexable
  #2^32 = 4294967296 , the max number of options that this algo can handle
  
  #Initialise all the possible combinations
  combinations <- c()
  for (i in 1:length(x$v)){
    bin_rep <- as.number(intToBits(i))
    print(bin_rep)
    combinations <- append(combinations, bin_rep)
  }
  combinations
  #print(combinations)
  #Create a max_value variable to store the best found option, and a max_value_elements variable to store how to reach that best option (store the binary rep)
  max_value <- 0
  max_value_elements <- intToBits(0)
  
  #Helper functions
  get_element_idx <- function(bin_rep){
    #given a binary representation, return the indexes of the 1s in the bin_rep
    indexes <- c()
    j <- 1
    #print(bin_rep)
    for (i in 1:32){
      if (bin_rep[i] == 1){
        indexes[j] <- i
        j <- j+1
      }
    }
    return(indexes)
  }
  get_sum_weight <- function(bin_rep, x){
    #given a bin_rep and a dataframe with v and w , returns the weight of the selection
    indexes <- get_element_idx(bin_rep)
    print("Indexes :")
    print(indexes)
    sum <- 0
    for (i in 1:length(indexes)){
      weight <- x$w[indexes[i]]
      sum <- sum + weight
    }
    return(sum)
  }
  
  #loop through the list and test if each combination yield a better answer using the binary rep as indexing reference for the possible items
  #print(length(combinations))
  for (comb in combinations){
    #Get the weight of the combinations of items
    new_sum <- get_sum_weight(comb, x)
    print("weight")
    print(new_sum)
    #Check if it is a better option
    if(new_sum > max_value && new_sum <= W){
      max_value <- new_sum
      max_value_elements <- get_element_idx(comb)
    }
  }
  answer <- list(value = max_value, elements = max_value_elements)
  return(answer)
}