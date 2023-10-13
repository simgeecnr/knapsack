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
    return(c(weight = round(tot_weight,0),value = round(tot_value,0), bin_rep = bin_rep))
  }
  
  find_max_value_subset <- function(subset, W) {
    max_value <- -Inf
    max_id <- NULL
    for (vec in subset) {
      if (vec[1] <= W) {
        if (vec[2] > max_value) {
          max_value <- vec[2]
          max_id <- vec[3]
        }
      }
    }
    return(list(max_value, max_id))
  }
  
  if(parallel){
    #=== Parallel Section ===============================
    print("Parallel selected")
    #Source Detect OS: https://stackoverflow.com/questions/4463087/detecting-operating-system-in-r-e-g-for-adaptive-rprofile-files
    switch(Sys.info()[['sysname']],
           Windows= {
             print("--- Starting to run Parallel for Windows code: ---")
             #Parallel workflow source : https://www.blasbenito.com/post/02_parallelizing_loops_with_r/
             
             library(parallel)
             library(doParallel)
             
             nCores <- parallel::detectCores() #Leave out one core for safety
             cat("Number of cores detected:", nCores, "\n")
             cat("Running parallelism on", nCores-1,"cores. \n")
             
             #Create the cluster, necessary for the %dopar%, keep 1 core for safety
             cl <- parallel::makeCluster(nCores-1)
             
             #register the cluster
             doParallel::registerDoParallel(cl = cl)
             
             # -- Parallel For each --
             para_work <- foreach(
               i = 1:(2^(length(x$v))-1),
               .combine = 'rbind' #options : c, cbind, rbind, list? , ...?
             ) %dopar% {
                  # Calculate the binary representation
                  binary <- intToBits(i)
                  binary <- as.integer(binary)
                  bin_rep <- paste(binary, collapse = "")
                  
                  #Get de combination information (weight, value, bin_rep)
                  g <- get_comb_info(bin_rep, x)
             }
             #print(as.data.frame(para_work))
             para_work_df <- as.data.frame(para_work)
             print(which(max(para_work_df$weight)))
             print(para_work_df[1,])
             print(typeof(para_work[1,]))
             para_work_split <- split(para_work, rep(1:(nCores - 1), length.out = length(para_work)))
             
             #Find max using "divide and conquer" parallel
             results <- parLapply(para_work_split, find_max_value_subset, W = W, cl = cl)
             
             parallel:stopCluster(cl)
             
             max_value <- -Inf
             max_id <- NULL
             for (result in results) {
               if (result[[1]] > max_value) {
                 max_value <- result[[1]]
                 max_id <- result[[2]]
               }
             }
             
             cat("The ID of the vector with the maximum value under weight", W, "is", max_id, "\n")
             
           },
           Linux  = {print("Parallel is function is not implemented for Linux yet")},
           Darwin = {print("Parallel is function is not implemented for Mac yet")})
    answer <- 0
  }

  else{
    #=== Brute Force Section ============================
    
    #--- Initialize all the possible combinations ------
    combinations <- c()
    for (i in 1:2^(length(x$v))){ #Generate all 2^N -1 combinations
      binary <- intToBits(i)
      binary <- as.integer(binary)
      bin_rep <- paste(binary, collapse = "")
      combinations <- append(combinations, bin_rep)
    }
    combinations <- head(combinations, -1) #Get rid of the last combination (overflow !)
    #--- Set the best option to taking No item ----------
    max_value <- 0
    max_value_elements <- intToBits(0)
    
    #--- Calculate every possible combination --------
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