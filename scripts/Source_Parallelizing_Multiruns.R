#setwd("/home/parker/Documents/projects/Code/curiosity-code/scripts")
rm(list=objects())
setwd(file.path(strsplit(getwd(), "curiosity-code")[[1]][1], "curiosity-code"))


# Take care of first-time local repos that don't have the "extra folders" that are still vital - results/ and source/temp/
if(!(dir.exists(file.path(strsplit(getwd(), 
        "curiosity-code", )[[1]][1], "curiosity-code", "results")))) {
          
          dir.create(file.path(strsplit(getwd(), 
        "curiosity-code", )[[1]][1], "curiosity-code", "results"))}

if(!(dir.exists(file.path(strsplit(getwd(), 
        "curiosity-code", )[[1]][1], "curiosity-code", "source", "temp")))) {
          
          dir.create(file.path(strsplit(getwd(), 
        "curiosity-code", )[[1]][1], "curiosity-code", "source", "temp"))}

# 
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
