
number_of_runs <- source("number_of_runs.txt")$value

make_vstore_folder_list <- function() {
  connection <- file(description = "sim_data.txt", open = "rt")
  folderNames <- as.vector(read.table(connection, -1L)[[2]])
  close(connection)
  return(folderNames)
}

multiRun_folderList <- make_vstore_folder_list()


#multirunParentDirectory <- "/home/labuser/Documents/Parker Scratch Folder/Code/Curiosity Code/StartingCuriosityValues/15-18"
##### setwd("/home/labuser/Documents/Parker Scratch Folder/Code/Curiosity Code")
parent_directory <- getwd()
#parent_directory <- ("/home/labuser/Documents/Parker Scratch Folder/Code/Curiosity Code")
#number_of_runs <- 10

for(run_visual in 1:number_of_runs) {
  #run_visual=1
  if(run_visual == 1) {
    multiRunTime <- format(Sys.time(), "%F-%H%M%S")
    setwd(paste0(strsplit(parent_directory, "curiosity-code")[[1]][1], "Results/", tail(list.files(path = paste0(strsplit(parent_directory, "curiosity-code")[[1]][1], "Results/"), pattern = "[0-9][0-9][0-9][0-9][0-9][0-9]_[0-9][0-9]"),1)))
    if(!(dir.exists("multirun_output") {dir.create("multirun_output")}
    dir.create(file.path("multirun_output", paste0(multiRunTime, "-GMT-multirun-output")))
    #multiFolderName <- paste0(parent_directory, "/", multiRunTime, "-GMT-multirun-output")
    #saveRDS(object = multiRun_folderList, file = paste0(multiFolderName, "/folderList.RData"))
    if(!(file.exists("folderList.RData"))) {saveRDS(object = multiRun_folderList, file = "folderList.RData")}    
    # metadata <- readRDS(file = paste0(multiRun_folderList[run_visual], "/metadata.RData"))
  } # makes the folder for multirun results, saves multiRun_folderList there.
  
# tail(list.files(pattern = "[0-9][0-9][0-9][0-9][0-9][0-9]_[0-9][0-9]"),1)

  setwd(multiRun_folderList[run_visual])
      
  source("../../../../curiosity-code/Source_Visualizing_Data.R") ####### rm(list=objects())!!!!!!
  run_number_directory <- getwd()
  parent_directory <- paste0(strsplit(run_number_directory, "Code")[[1]][1], "Code/curiosity-code/")
  multiRun_folderList <- readRDS(file = paste0(strsplit(getwd(), "variable")[[1]][1], "/", "folderList.RData"))
  run_visual <- which(multiRun_folderList == run_number_directory)
  
  # parent_directory <- getwd()
  #multirun_directory <- paste0(parent_directory, "/", tail(list.files(pattern = "multirun"),1))
  multirun_directory <- paste0(strsplit(getwd(), "variable")[[1]][1], "multirun_output/", list.files(path = paste0(strsplit(getwd(), "variable")[[1]][1], "multirun_output/"), pattern = "multirun"))
  #results_directory <- paste0(str_split(parent_directory, "Curiosity")[[1]][1], "Results/")
  setwd(multirun_directory)
  info <- readRDS(file = paste0(run_number_directory, "/metadata.RData"))
  #converted_data <- convert_stored_data(P = P, num_timechunks = thousand_timesteps)
  data_convert <- paste0("converted_data", run_visual, " <- convert_stored_data(P = P, num_timechunks = thousand_timesteps, data_dir = \"", 
                         run_number_directory, "\", simplification_factor = P$num_timesteps/(P$num_timesteps/100))")
  cat(data_convert, file = "data_convert.R", sep = "\n")
  source("data_convert.R")
  old_names = c("sylrep_rowcol","sylrep_dstbxn","curity_mean_t","curity_repert")
  rm(list=ls(pattern=old_names[1]))
  rm(list=ls(pattern=old_names[2]))
  rm(list=ls(pattern=old_names[3]))
  rm(list=ls(pattern=old_names[4]))
  rm(old_names)
  #R <- create_plot_info(info[[2]], info[[1]])
  #fig_text_make <- paste0("R <- create_plot_info(info[[2]], info[[1]])")
  #eval(parse(text=fig_text_make))
  
  
  #info_make <- paste(paste0("sink(file = \"", run_visual, " - Parameters and Info\")"), 
  #              "cat(paste0(\"Number of Timesteps: \", info[[3]][1], \"\nNumber of Populations: \", info[[3]][2], \"\nPopulation Size: \", info[[3]][3], \"\nNumber of Syllables: \", info[[3]][4], \"\nNumber of Syllable Positions Assigned to Specific Probability Levels: \", info[[3]][5], \"\nNumber of Singers Sampled from One Population for Mating: \", info[[3]][7], \"\nNumber of Singers Sampled from One Population for Tutoring: \", info[[3]][6], \"\nProbability of Inheriting Curiosity Accurately: \", info[[3]][8], \"\nProbability of Learning Syllables Accurately from Parent: \", info[[3]][10], \"\nProbability of Learning Syllables Accurately from Tutor: \", info[[3]][9], \"\nProbability of Picking up Random Extra Syllables from Parent: \", info[[3]][12], \"\nProbability of Picking up Random Extra Syllables from Tutor: \", info[[3]][11], \"\nStandard Deviation of Randomly-picked-up Sylls from Established Mean: \", info[[3]][13], \"\nNumber of Rows in Population Calls Matrix: \", info[[3]][14], \"\nNumber of Columns in Pop Calls Matrix: \", info[[3]][15], \"\nPairing Pool Rows: \", info[[3]][16], \"\nPairing Pool Columns: \", info[[3]][17], \"\nPairing Pool Slices: \", info[[3]][18], \"\nCuriosity Counter Rows: \", info[[3]][19], \"\nCuriosity Counter Columns: \", info[[3]][20], \"\nPopulation Syllable Probability Rows: \", info[[3]][21], \"\nPopulation Probability Columns: \", info[[3]][22], \"\nLength of Curiosity Breaks Vector: \", info[[3]][23], \"\nLength of Zero to One Template: \", info[[3]][24], \"\nLearning Pool Rows: \", info[[3]][25], \"\nLearning Pool Columns: \", info[[3]][26], \"\nLearning Pool Slices: \", info[[3]][27]))", 
  #             "sink()", sep = "\n")
  #saveRDS(object = converted_data)
  #dataConveRtDS <- paste0("saveRDS(object = converted_data", run_visual, ", file = \"dataConvert", run_visual, ".RData\")")
  #eval(parse(text=c(fig_text_make, info_make, dataConveRDSt)))
  #eval(parse(text=dataConveRtDS))
  
  sylrepblahz <- paste0("sylrepz", run_visual, " <- split_data(converted_data", run_visual, ", 1)")
  sdstbxblahn <- paste0("sdstbxn", run_visual, " <- split_data(converted_data", run_visual, ", 2)")
  cursitblahy <- paste0("cursity", run_visual, " <- split_data(converted_data", run_visual, ", 3)")
  curhisblaht <- paste0("curhist", run_visual, " <- split_data(converted_data", run_visual, ", 4)")
  eval(parse(text=c(sylrepblahz, sdstbxblahn, cursitblahy, curhisblaht)))
  
  sylrepzConveRtDS <- paste0("saveRDS(object = sylrepz", run_visual, ", file = \"SylReps", run_visual, ".RData\")")
  sdstbxnConveRtDS <- paste0("saveRDS(object = sdstbxn", run_visual, ", file = \"SylDist", run_visual, ".RData\")")
  cursityConveRtDS <- paste0("saveRDS(object = cursity", run_visual, ", file = \"Cursity", run_visual, ".RData\")")
  curhistConveRtDS <- paste0("saveRDS(object = curhist", run_visual, ", file = \"CurHist", run_visual, ".RData\")")
  eval(parse(text=c(sylrepzConveRtDS, sdstbxnConveRtDS, cursityConveRtDS, curhistConveRtDS)))
  
  
  setwd(parent_directory)
  
} # outputs pieces of different runs 


datanames <- c("CurHist","Cursity","SylDist","SylReps")
objectnames <- c("curhist","cursity","sdstbxn","sylrepz")
listnames <- c("hist","sity","sdst","repz")
number_of_runs <- source("number_of_runs.txt")$value

for(i in 1:4) {
  listlister <- paste0(listnames[i], "list <- vector(mode = \"character\", length = number_of_runs)")
  listmaker <- paste0(listnames[i], "list[", 1:number_of_runs, "] <- \"", datanames[i], 1:number_of_runs, ".RData\"")
  eval(parse(text=c(listlister, listmaker)))
}

sylrepzlist <- list()
sdstbxnlist <- list()
cursitylist <- list()
curhistlist <- list()

setwd(multirun_directory)

for(i in 1:number_of_runs) {
  histthing <- paste0("curhistlist[[i]] <- readRDS(\"", histlist[i], "\")")
  sitything <- paste0("cursitylist[[i]] <- readRDS(\"", sitylist[i], "\")")
  sdstthing <- paste0("sdstbxnlist[[i]] <- readRDS(\"", sdstlist[i], "\")")
  repzthing <- paste0("sylrepzlist[[i]] <- readRDS(\"", repzlist[i], "\")")
  eval(parse(text=c(histthing, sitything, sdstthing, repzthing)))
}


sylrepzlist[[number_of_runs + 1]] <- sylrepzlist[[number_of_runs]]
sdstbxnlist[[number_of_runs + 1]] <- sdstbxnlist[[number_of_runs]]
cursitylist[[number_of_runs + 1]] <- cursitylist[[number_of_runs]]
curhistlist[[number_of_runs + 1]] <- curhistlist[[number_of_runs]]

for(i in 1:length(curhistlist[[1]])) {
  #curhistlist[[number_of_runs + 1]][i] <- mean(c(curhistlist[[1]][i],curhistlist[[2]][i],curhistlist[[3]][i],curhistlist[[4]][i],curhistlist[[5]][i],curhistlist[[6]][i],curhistlist[[7]][i],curhistlist[[8]][i],curhistlist[[9]][i],curhistlist[[10]][i]))
  eval(parse(text=paste0("curhistlist[[number_of_runs + 1]][i] <- mean(c(curhistlist[[", paste0(1:(number_of_runs-1),"]][i],curhistlist[[", collapse=''), number_of_runs, "]][i]))")))
}
#mean(cat(paste0("curhistlist[[", 1:number_of_runs, "]][i]"), sep = ", "))
#thing <- paste0("curhistlist[[", 1:number_of_runs, "]][i]")
for(i in 1:length(cursitylist[[1]])) {
  #cursitylist[[number_of_runs + 1]][i] <- mean(c(cursitylist[[1]][i],cursitylist[[2]][i],cursitylist[[3]][i],cursitylist[[4]][i],cursitylist[[5]][i],cursitylist[[6]][i],cursitylist[[7]][i],cursitylist[[8]][i],cursitylist[[9]][i],cursitylist[[10]][i]))
  eval(parse(text=paste0("cursitylist[[number_of_runs + 1]][i] <- mean(c(cursitylist[[", paste0(1:(number_of_runs-1),"]][i],cursitylist[[", collapse=''), number_of_runs, "]][i]))")))
}
for(i in 1:length(sdstbxnlist[[1]])) {
  #sdstbxnlist[[number_of_runs + 1]][i] <- mean(c(sdstbxnlist[[1]][i],sdstbxnlist[[2]][i],sdstbxnlist[[3]][i],sdstbxnlist[[4]][i],sdstbxnlist[[5]][i],sdstbxnlist[[6]][i],sdstbxnlist[[7]][i],sdstbxnlist[[8]][i],sdstbxnlist[[9]][i],sdstbxnlist[[10]][i]))
  eval(parse(text=paste0("sdstbxnlist[[number_of_runs + 1]][i] <- mean(c(sdstbxnlist[[", paste0(1:(number_of_runs-1),"]][i],sdstbxnlist[[", collapse=''), number_of_runs, "]][i]))")))
}
for(i in 1:length(sylrepzlist[[1]])) {
  #sylrepzlist[[number_of_runs + 1]][i] <- mean(c(sylrepzlist[[1]][i],sylrepzlist[[2]][i],sylrepzlist[[3]][i],sylrepzlist[[4]][i],sylrepzlist[[5]][i],sylrepzlist[[6]][i],sylrepzlist[[7]][i],sylrepzlist[[8]][i],sylrepzlist[[9]][i],sylrepzlist[[10]][i]))
  eval(parse(text=paste0("sylrepzlist[[number_of_runs + 1]][i] <- mean(c(sylrepzlist[[", paste0(1:(number_of_runs-1),"]][i],sylrepzlist[[", collapse=''), number_of_runs, "]][i]))")))
}

last_stats <- paste0("rm(sylrepz", number_of_runs, ", sdstbxn", number_of_runs,
                     ", cursity", number_of_runs, ", curhist", number_of_runs,
                     ", sylrepblahz, sdstbxblahn, cursitblahy, curhisblaht",
                     ", sylrepzConveRtDS, sdstbxnConveRtDS, cursityConveRtDS, curhistConveRtDS",
                     ", last_stats, data_convert, histthing, sitything, sdstthing, repzthing",
                     ", histlist, sitylist, sdstlist, repzlist, listlister, listmaker, listnames",
                     ", objectnames, datanames)")
eval(parse(text=last_stats))


info <- readRDS("metadata.RData")
R <- create_plot_info(info[[2]], info[[1]])
info_make <- paste(paste0("sink(file = \"Multirun - Parameters and Info\")"), 
              "cat(paste0(\"Number of Timesteps: \", info[[3]][1], \"\nNumber of Populations: \", info[[3]][2], \"\nPopulation Size: \", info[[3]][3], \"\nNumber of Syllables: \", info[[3]][4], \"\nNumber of Syllable Positions Assigned to Specific Probability Levels: \", info[[3]][5], \"\nNumber of Singers Sampled from One Population for Mating: \", info[[3]][7], \"\nNumber of Singers Sampled from One Population for Tutoring: \", info[[3]][6], \"Probability of Inheriting Curiosity Accurately: \", info[[3]][8], \"\nProbability of Learning Syllables Accurately from Parent: \", info[[3]][10], \"\nProbability of Learning Syllables Accurately from Tutor: \", info[[3]][9], \"\nProbability of Picking up Random Extra Syllables from Parent: \", info[[3]][12], \"\nProbability of Picking up Random Extra Syllables from Tutor: \", info[[3]][11], \"\nStandard Deviation of Randomly-picked-up Sylls from Established Mean: \", info[[3]][13], \"\nNumber of Rows in Population Calls Matrix: \", info[[3]][14], \"\nNumber of Columns in Pop Calls Matrix: \", info[[3]][15], \"\nPairing Pool Rows: \", info[[3]][16], \"\nPairing Pool Columns: \", info[[3]][17], \"\nPairing Pool Slices: \", info[[3]][18], \"\nCuriosity Counter Rows: \", info[[3]][19], \"\nCuriosity Counter Columns: \", info[[3]][20], \"\nPopulation Syllable Probability Rows: \", info[[3]][21], \"\nPopulation Probability Columns: \", info[[3]][22], \"\nLength of Curiosity Breaks Vector: \", info[[3]][23], \"\nLength of Zero to One Template: \", info[[3]][24], \"\nLearning Pool Rows: \", info[[3]][25], \"\nLearning Pool Columns: \", info[[3]][26], \"\nLearning Pool Slices: \", info[[3]][27]))", 
             "sink()", sep = "\n")
eval(parse(text=info_make))

mins_n_maxes <- min_n_max(number_of_runs = number_of_runs)
simple_plots(Q = "converted_data", extra_lines = TRUE, number_of_runs)
#paste_split_data_runs(data_subset, num_runs = 10, also_mean = TRUE)
  
  
#simple_multiplots(R = R1, Q = converted_data, simplification_factor = 100, extra_lines = TRUE)



#FolderName <- list.files(pattern = reg_spresh)[offset + 1]
#setwd(results_directory)

#simple_multiplots(R = R1, Q = converted_data, simplification_factor = 100, extra_lines = TRUE)
#full_plots(R = R, Q = converted_data, extra_lines = TRUE)

#library(rstudioapi)
#documentSave(getActiveDocumentContext()$id)
