# You Should be Here To: Repeat A Lot of Simulations in Parallel
#
# __________  _____ __________    _____   ____     ____     ___________ ____
# \______   \/  _  \\______   \  /  _  \ |    |   |    |    \_   _____/|    |    
#  |     ___/  /_\  \|       _/ /  /_\  \|    |   |    |     |    __)_ |    |    
#  |    |  /    |    \    |   \/    |    \    |___|    |___  |        \|    |___ 
#  |____|  \____|__  /____|_  /\____|__  /_______ \_______ \/_______  /|_______ \
#                  \/       \/         \/        \/       \/        \/         \/




# This is an Example of what you should NEVER have in your code, presented here,

# "So that I can use it when I'm being a bad person :P"

    ##### -> setwd(file.path(strsplit(getwd(), "curiosity-code")[[1]][1], "curiosity-code"))

#____________________________________________________________________________________

rm(list=objects()) # Did you want to run me while you were doing other stuff? Sorry! 
#


# This bout takes care of first-time remote repos (from OP's perspective)
# that don't have the "extra folders" that are still vital for a Github 
# clone of master branch.-> Code/curiosity-code/
#                                     - results/
#                                     - source/temp/

if(!(dir.exists(file.path(strsplit(getwd(), 
        "curiosity-code", )[[1]][1], "curiosity-code", "results")))) {
          
          dir.create(file.path(strsplit(getwd(), 
        "curiosity-code", )[[1]][1], "curiosity-code", "results"))}

if(!(dir.exists(file.path(strsplit(getwd(), 
        "curiosity-code", )[[1]][1], "curiosity-code", "source", "temp")))) {
          
          dir.create(file.path(strsplit(getwd(), 
        "curiosity-code", )[[1]][1], "curiosity-code", "source", "temp"))}

if(!(dir.exists(file.path(strsplit(getwd(), 
        "curiosity-code", )[[1]][1], "curiosity-code", "source", "RtempFiles")))) {
          
          dir.create(file.path(strsplit(getwd(), 
        "curiosity-code", )[[1]][1], "curiosity-code", "source", "RtempFiles"))
          dir.create(file.path(strsplit(getwd(), 
        "curiosity-code", )[[1]][1], "curiosity-code", "source", "archive", "RtempFiles"))
        }


# This line will source packagaes, either:
    # by loading them from the computer, or 
    # by downloading them from the internet.
source(file.path("scripts", "Source_Reference_Section.R"))


# we specify the number of cores/workers we want to use
# n_cores <- detectCores() - 4
n_cores <- 16


source(file.path("scripts", "Source_Multiple_Runs.R"))


# the time difference between using n_cores and not using it


shifting_curstart <- 1:16
paramsFile <- c("diffFemParams.yaml")
mclapply(shifting_curstart, multi_runs, paramsSource = paramsFile, mc.cores = n_cores)


# paramsFile <- c("params.yaml")
# profvis({
#   shifting_curstart <- 1
#   multi_runs(shifting_curstart = shifting_curstart, paramsSource = paramsFile)
# })

