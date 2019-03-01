rm(list=objects())
source("Source_Reference_Section.R")

# we specify the number of cores/workers we want to use
n_cores <- detectCores() - 4
n_cores
# 

source("Source_Multiple_Runs.R")

# the time difference between using n_cores and not using it
shifting_curstart <- 1:4
paramsFile <- c("params.yaml")
mclapply(shifting_curstart, multi_runs, mc.cores = n_cores)
mclapply(shifting_curstart, sum, mc.cores = n_cores)