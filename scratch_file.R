data1s <- paste0(names[data_subset], "_", 1:num_timechunks, " <- readRDS(file = ", '"', run_number_directory, "/", strsplit(run_number_directory, "-GMT-")[[1]][2], "-", 1:num_timechunks, "-", names[data_subset], ".RData", '"', ")")
cat(data1s, file = "data_subset.R", sep = "\n")
source("data_subset.R")

#thing_1 <- paste0("stuff1 <- getwd()")
#thing_2 <- paste0("stuff2 <- getwd()")
thing_3 <- paste0("stuff3 <- readRDS(file = \"", run_number_directory, "/variable-store-", 1:num_timechunks, "-", names[data_subset], ".RData)")
#eval(parse(text=c(thing_1, thing_2)))
#rm(stuff1,stuff2,thing_1,thing_2)


thing <- paste0()

for(data_subset in 1:4) {
  for(i in 1:num_timechunks) {
    variable_killer <- paste0("rm(", names[1:4], "_", 1:num_timechunks, ")")
  }
}

for(i in 1:4) {
  thing <- paste0("variable_killer", i, " <- rm(", old_names[i], "_", 1:num_timechunks, ")")
  blah <- past
  stuff <- paste0("rm(variable_killer", i)
  eval(parse(text=c(thing, stuff)))
}
### FINAL SUCCESSFUL REMOVE OPTION
for(i in 1:4) {
  thing <- paste0("rm(", old_names[i], "_", 1:num_timechunks, ")")
  eval(parse(text=thing))
}



