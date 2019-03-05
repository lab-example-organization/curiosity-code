#setwd("/home/parker/Documents/projects/Code/curiosity-code/scripts")
rm(list=objects())
setwd(file.path(strsplit(getwd(), "curiosity-code")[[1]][1], "curiosity-code"))
source(file.path("scripts", "Source_Reference_Section.R"))

# we specify the number of cores/workers we want to use
n_cores <- detectCores() - 4
n_cores
# 

source(file.path("scripts", "Source_Multiple_Runs.R"))

# the time difference between using n_cores and not using it
shifting_curstart <- 1:250
paramsFile <- c("params.yaml")
mclapply(shifting_curstart, multi_runs, paramsSource = paramsFile, mc.cores = n_cores)
