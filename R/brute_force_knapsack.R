#' Knapsack Brute Force
#'
#' @param x a data.frame with two variables v (Value) and w (Weight) and returns the maximum knapsack value and which elements (rows in the data.frame).
#' @param W is the knapsack size.
#'
#' @return returns the maximum knapsack value and which elements (rows in the data.frame).
#' @export
#'
#' @examples
#' x <- data.frame(v = c(10, 5, 15, 7, 6, 18), w = c(2, 3, 5, 7, 1, 4))
#' W <- 15
#' brute_force_knapsack(x,W)

brute_force_knapsack <- function(x, W){
  
  #=== Input Check =========
  #The function should check that the inputs are correct (i.e. a data.frame with two variables v and w)
  #with only positive values.
  stopifnot(W >= 0)
  stopifnot(is.data.frame(x))
  stopifnot("v" %in% colnames(x) && "w" %in% colnames(x))

#=== Helper functions================================
  get_element_idx <- function(bin_rep){
    #given a binary representation, return the indexes of the 1s in the bin_rep
    bin_rep_split <- strsplit(bin_rep, "")[[1]] # https://stackoverflow.com/questions/26721340/iterating-over-characters-of-string-r
    indexes <- c()
    j <- 1
    for (i in 1:32){
      if (bin_rep_split[i] == "1"){
        indexes[j] <- i
        j <- j+1
      }
    }
    return(indexes)
  }
  get_comb_info <- function(bin_rep, x){
    #given a bin_rep and a dataframe with v and w , returns the value and weight of the specified combination of items
    indexes <- get_element_idx(bin_rep)
    tot_value <- 0
    tot_weight <- 0
    for (i in 1:length(indexes)){
      weight <- x$w[indexes[i]]
      value <- x$v[indexes[i]]
      tot_weight <- tot_weight + weight
      tot_value <- tot_value + value
    }
    return(c(weight = tot_weight, value = tot_value, bin_rep = bin_rep))
  }
  find_bin_rep <- function(df, max_weight) {
    result <- df[, "result"]
    result["value"] <- as.numeric(result["value"])
    subset_df <- result[result["weight"] <= max_weight, ]
    max_value_row <- subset_df[which.max(subset_df["value"]), ]
    bin_rep_for_max_value <- max_value_row["bin_rep"]
    return(bin_rep_for_max_value)
  }
  recursive_knap <- function(i, left_idx, right_idx, x, W){
    #i is my vector from 1:2^N
    if(left_idx == right_idx){
      #Convert i to binary
      binary <- as.integer(intToBits(i[left_idx]))
      bin_rep <- paste(binary, collapse = "")
      #Fetch the infos
      info <- get_comb_info(bin_rep, x)
      if(as.numeric(info[1]) <= W){ #Check if solution is under weight limit
        return(info)
      }
      else{
        return(c(weight = 0,value = 0,bin_rep = 0))
      }
    }
    else{
      #Split the set in 2 
      middle <- (left_idx+right_idx)%/%2
      left_info <- recursive_knap(i, left_idx, middle, x, W)
      right_info <- recursive_knap(i, middle+1, right_idx, x, W)
      
      #Calculate the max of left and right
      left_max_value <- as.numeric(left_info[2])
      right_max_value <- as.numeric(right_info[2])
      #return the max value
      if(left_max_value > right_max_value){
        return(left_info)
      }else{
        return(right_info)
      }
    }
  }

#=== Brute Force Section ============================
    
  #--- Set the best option to taking No item ----------
  max_value <- 0
  max_value_elements <- intToBits(0)
    
  n <- (2^(length(x$v))-1) #The number of combinations to try
  info <- recursive_knap(c(1:n),left_idx=1, right_idx=n, x=x, W=W)
  max_value <- as.numeric(info[2])
  max_value_elements <- get_element_idx(info[3])
  answer <- list(value = round(max_value, 0), elements = max_value_elements)

  return(answer)
}