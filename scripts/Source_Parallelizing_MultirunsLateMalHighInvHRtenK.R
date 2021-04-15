# You Should be Here To: Repeat A Lot of Simulations in Parallel
#
# __________  _____ __________    _____   ____     ____     ___________ ____
# \______   \/  _  \\______   \  /  _  \ |    |   |    |    \_   _____/|    |
#  |     ___/  /_\  \|       _/ /  /_\  \|    |   |    |     |    __)_ |    |
#  |    |  /    |    \    |   \/    |    \    |___|    |___  |        \|    |___
#  |____|  \____|__  /____|_  /\____|__  /_______ \_______ \/_______  /|_______ \
#                  \/       \/         \/        \/       \/        \/         \/




# This is an Example of what you should NEVER have in your code,

# presented here, "So that I can use it when I'm being a bad person :P"

    ##### -> setwd(file.path(strsplit(getwd(), "curiosity-code")[[1]][1], "curiosity-code"))

#____________________________________________________________________________________

rm(list=objects()) # Clear environment
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
referencesection("multirun")
# referenceSection("profiler")

n_cores <- 20
# Specify the number of cores/workers we want to use
    # n_cores <- detectCores() - 3 # built around a maximum allowance
# n_cores <- 2
# n_cores <- 1

  sourceCpp(file.path('cpp_source', 'median.cpp'))
  sourceCpp(file.path('cpp_source', 'rowSums.cpp'))
  sourceCpp(file.path('cpp_source', 'sort.cpp'))

source(file.path("scripts", "Source_Multiple_Runs.R"))


shifting_curstart <- 1:200
paramsfile <- c("paramsLateInvMalHighHrTenK.yaml")
# paramsFile <- c("diffZwischensTnN.yaml")
simdate <- gsub('-', '', substring(Sys.Date(), 3))
secretcode <- 58418
mclapply(shifting_curstart,
         multi_runs,
         paramssource = paramsfile,
         dirdate = simdate,
         seednumber = secretcode,
         mc.cores = n_cores)


# paramsFile <- c("params.yaml")
# profvis({
#   shifting_curstart <- 1
#   multi_runs(shifting_curstart = shifting_curstart, paramsSource = paramsFile)
# })

# print("stuff")




