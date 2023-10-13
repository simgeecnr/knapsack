RNGversion(min(as.character(getRversion()),"3.5.3"))

##old sampler used for backward compatibility
## suppressWarnings() can be used so that the above warning is not displayed
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500, TRUE)

#Parallel stuf
detectCores()
#Initialize the parallel processing environment: After loading the parallel package, you will need to initialize the parallel processing environment by using the ‘parLapply()’ function. This function takes a vector of inputs, divides it into sub-vectors, and applies a function to each sub-vector in parallel.
parLapply()
