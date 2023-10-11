#' Knapsack Brute Force
#'
#' @param x a data.frame with two variables v (Value) and w (Weight) and returns the maximum knapsack value and which elements (rows in the data.frame).
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
  
#=== Initialize all the possible combinations========
  combinations <- c()
  for (i in 1:2^(length(x$v))){ #Generate all 2^N combinations
    binary <- intToBits(i)
    binary <- as.integer(binary)
    bin_rep <- paste(binary, collapse = "")
    combinations <- append(combinations, bin_rep)
  }
  combinations <- head(combinations, -1) #Get rid of the last combination (overflow !)

#=== Helper functions================================
  get_element_idx <- function(bin_rep){
    #given a binary representation, return the indexes of the 1s in the bin_rep
    bin_rep_split <- strsplit(bin_rep, "")[[1]] # https://stackoverflow.com/questions/26721340/iterating-over-characters-of-string-r
    indexes <- c()
    j <- 1
    #print(bin_rep)
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
    return(c(tot_weight, tot_value))
  }

#=== Parallel Section ===============================
  if(parallel){
    #Source Detect OS: https://stackoverflow.com/questions/4463087/detecting-operating-system-in-r-e-g-for-adaptive-rprofile-files
    # switch(Sys.info()[['sysname']],
    #        Windows= {
    #          source(parallel)
    #          print("--- Starting to run Parallel for Windows code: ---")
    #          nCores <- parallel::detectCores()
    #          cat("Number of cores detected:", nCores, "\n")
    #          
    #          cl <- makeClusters(nCores)
    #          #combinations is my list of all the bin_rep that needs to be calculated
    #          
    #          
    #          
    #          stopCluster(cl)
    #          
    #        },
    #        Linux  = {print("Parallel is function is not implemented for Linux yet")},
    #        Darwin = {print("Parallel is function is not implemented for Mac yet")})
    answer <- 0
  }
#=== Brute Force Section ============================
  #loop through the list and test if each combination yield a better answer using the binary rep as indexing reference for the possible items
  #print(length(combinations))
  #Create a max_value variable to store the best found option, and a max_value_elements variable to store how to reach that best option (store the binary rep)
  else{
    max_value <- 0
    max_value_elements <- intToBits(0)
    
    for (comb in combinations){
      #Get the weight of the combinations of items
      new_res <- get_comb_info(comb, x)
      #Check if it is a better option
      if(new_res[2] > max_value && new_res[1] <= W){
        max_value <- new_res[2]
        max_value_elements <- get_element_idx(comb)
      }
    }
    #I dont know why but values have decimals, so I round the number to get rid of the issue
    answer <- list(value = round(max_value, 0), elements = max_value_elements)
  }
  
#=== Return the answer found in either Parallel or not =========
  return(answer)
}