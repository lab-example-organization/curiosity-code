
figProdMultRun <- function(shifting_curstart, number_of_runs) {
  connection <- file(description = file.path("..", "source","temp", paste0(shifting_curstart, "_sim_data.txt")), open = "rt")
  multiRun_folderList <- as.vector(read.table(connection, -1L)[[2]])
  close(connection)
  
  parent_directory <- getwd()
  
  for(run_visual in 1:number_of_runs) {
    #run_visual=1
    if(run_visual == 1) {
      multiRunTime <- format(Sys.time(), "%F-%H%M%S")
      setwd(strsplit(multiRun_folderList[run_visual], split = "variable_store", )[[1]][1])
      if(!(dir.exists("multirun_output"))) {dir.create("multirun_output")}
      dir.create(file.path("multirun_output", paste0(multiRunTime, "-GMT-multirun-output")))
      if(!(file.exists("folderList.RData"))) {saveRDS(object = multiRun_folderList, file = "folderList.RData")}    
    } # makes the folder for multirun results, saves multiRun_folderList there.
    
    
    setwd(multiRun_folderList[run_visual])
    parameters = readRDS(paste0(getwd(), "/parameters.RData"))
    source("../../../../scripts/Source_Visualizing_Data.R") ####### rm(list=objects())!!!!!!
    visualizing_data(number_of_runs = number_of_runs)
    multirun_directory <- paste0(strsplit(getwd(), "variable")[[1]][1], "multirun_output/", list.files(path = paste0(strsplit(getwd(), "variable")[[1]][1], "multirun_output/"), pattern = "multirun"))
    setwd(multirun_directory)
    info <- readRDS(file = paste0(multiRun_folderList[run_visual], "/metadata.RData"))
    data_convert <- paste0("converted_data", run_visual, " <- convert_stored_data(parameters = parameters, data_dir = \"", 
                           multiRun_folderList[run_visual], "\", simplification_factor = parameters$num_timesteps/(parameters$num_timesteps/100))")
    cat(data_convert, file = "data_convert.R", sep = "\n")
    source("data_convert.R", local=TRUE)
    old_names = c("sylrep_rowcol","sylrep_dstbxn","curity_mean_t","curity_repert")
    rm(list=ls(pattern=old_names[1]))
    rm(list=ls(pattern=old_names[2]))
    rm(list=ls(pattern=old_names[3]))
    rm(list=ls(pattern=old_names[4]))
    rm(old_names)
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
    eval(parse(text=paste0("curhistlist[[number_of_runs + 1]][i] <- mean(c(curhistlist[[", 
                           paste0(1:(number_of_runs),"]][i],curhistlist[[", collapse=''), 
                           number_of_runs, "]][i]))")))
  }
  for(i in 1:length(cursitylist[[1]])) {
    eval(parse(text=paste0("cursitylist[[number_of_runs + 1]][i] <- mean(c(cursitylist[[", 
                           paste0(1:(number_of_runs),"]][i],cursitylist[[", collapse=''), 
                           number_of_runs, "]][i]))")))
  }
  for(i in 1:length(sdstbxnlist[[1]])) {
    eval(parse(text=paste0("sdstbxnlist[[number_of_runs + 1]][i] <- mean(c(sdstbxnlist[[", 
                           paste0(1:(number_of_runs),"]][i],sdstbxnlist[[", collapse=''), 
                           number_of_runs, "]][i]))")))
  }
  for(i in 1:length(sylrepzlist[[1]])) {
    eval(parse(text=paste0("sylrepzlist[[number_of_runs + 1]][i] <- mean(c(sylrepzlist[[", 
                           paste0(1:(number_of_runs),"]][i],sylrepzlist[[", collapse=''), 
                           number_of_runs, "]][i]))")))
  }
  last_stats <- paste0("rm(sylrepz", number_of_runs, ", sdstbxn", number_of_runs,
                       ", cursity", number_of_runs, ", curhist", number_of_runs,
                       ", sylrepblahz, sdstbxblahn, cursitblahy, curhisblaht",
                       ", sylrepzConveRtDS, sdstbxnConveRtDS, cursityConveRtDS, curhistConveRtDS",
                       ", last_stats, data_convert, histthing, sitything, sdstthing, repzthing",
                       ", histlist, sitylist, sdstlist, repzlist, listlister, listmaker, listnames",
                       ", objectnames, datanames)")
  eval(parse(text=last_stats))
  
  R <- create_plot_info(info[[2]], info[[1]])
  info_make <- paste(paste0("sink(file = \"Multirun - Parameters and Info\")"), 
                     "cat(paste0(\"Number of Timesteps: \", info[[3]][1], \"\nNumber of Populations: \", info[[3]][2], \"\nPopulation Size: \", info[[3]][3], \"\nNumber of Syllables: \", info[[3]][4], \"\nNumber of Syllable Positions Assigned to Specific Probability Levels: \", info[[3]][5], \"\nNumber of Singers Sampled from One Population for Mating: \", info[[3]][7], \"\nNumber of Singers Sampled from One Population for Tutoring: \", info[[3]][6], \"\nProbability of Inheriting Curiosity Accurately: \", info[[3]][8], \"\nProbability of Learning Syllables Accurately from Parent: \", info[[3]][10], \"\nProbability of Learning Syllables Accurately from Tutor: \", info[[3]][9], \"\nProbability of Picking up Random Extra Syllables from Parent: \", info[[3]][12], \"\nProbability of Picking up Random Extra Syllables from Tutor: \", info[[3]][11], \"\nStandard Deviation of Randomly-picked-up Sylls from Established Mean: \", info[[3]][13], \"\nNumber of Rows in Population Calls Matrix: \", info[[3]][14], \"\nNumber of Columns in Pop Calls Matrix: \", info[[3]][15], \"\nPairing Pool Rows: \", info[[3]][16], \"\nPairing Pool Columns: \", info[[3]][17], \"\nPairing Pool Slices: \", info[[3]][18], \"\nCuriosity Counter Rows: \", info[[3]][19], \"\nCuriosity Counter Columns: \", info[[3]][20], \"\nPopulation Syllable Probability Rows: \", info[[3]][21], \"\nPopulation Probability Columns: \", info[[3]][22], \"\nLength of Curiosity Breaks Vector: \", info[[3]][23], \"\nLength of Zero to One Template: \", info[[3]][24], \"\nLearning Pool Rows: \", info[[3]][25], \"\nLearning Pool Columns: \", info[[3]][26], \"\nLearning Pool Slices: \", info[[3]][27]))", 
                     "sink()", sep = "\n")
  eval(parse(text=info_make))
  mins_n_maxes <- min_n_max(parameters = parameters, number_of_runs = number_of_runs, 
                            cursitylist = cursitylist, sdstbxnlist = sdstbxnlist, 
                            curhistlist = curhistlist, sylrepzlist = sylrepzlist)
  simple_plots(parameters = parameters, R = R, Q = "converted_data", extra_lines = TRUE, 
               number_of_runs = number_of_runs, cursitylist = cursitylist, 
               sdstbxnlist = sdstbxnlist, curhistlist = curhistlist, sylrepzlist = sylrepzlist, 
               mins_n_maxes = mins_n_maxes)
  src.dir = "../../../../scripts/"
  file.names = dir(src.dir)[grep("Source", dir(src.dir))]
  sapply(file.names, function(x) { 
    file.copy(from=paste0(src.dir, x), 
              to=paste0(getwd(), x), 
              overwrite = FALSE) })
  setwd(parent_directory)
  return(print("It's Done, Yo!"))              
}
