library(doParallel)

# we specify the number of cores/workers we want to use
n_cores <- detectCores() - 2
n_cores
## [1] 7
# generate a toy function that
# simply generate the summary of a bunch of random numbers
source("Source_Multiple_Runs.R")

# the time difference between using n_cores and not using it
shifting_curstart <- 1:2
mclapply(shifting_curstart, multi_runs, mc.cores = n_cores)
