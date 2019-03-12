
figProdMultRun <- function(specificSimNumber, number_of_runs, paramsSource = paramsSource) {
  
  connection <- file(description = file.path("source","temp", paste0(specificSimNumber, "_sim_data.txt")), open = "rt")
  multiRun_folderList <- as.vector(read.table(connection, -1L)[[2]])
  close(connection)

  params = yaml.load_file(file.path("parameters", paramsSource))

  for(run_visual in 1:number_of_runs) {
    #run_visual=1
    if(run_visual == 1) {
      multiRunTime <- format(Sys.time(), "%F-%H%M%S")
      
      if(!(dir.exists(file.path(strsplit(multiRun_folderList[run_visual], 
        "/variable_store", )[[1]][1], "multirun_output")))) {
          
          dir.create(file.path(strsplit(multiRun_folderList[run_visual], 
          "/variable_store", )[[1]][1], "multirun_output"))
          
          dir.create(file.path(strsplit(multiRun_folderList[run_visual], 
          "/variable_store", )[[1]][1], "multirun_output", 
          paste0(multiRunTime, "-GMT-multirun-output")))
          }
      if(!(file.exists(file.path(strsplit(multiRun_folderList[run_visual], 
        "variable_store", )[[1]][1], paste0("Group_", specificSimNumber, "_folderList.RData"))))) {
          saveRDS(object = multiRun_folderList, file = 
                       file.path(strsplit(multiRun_folderList[run_visual], 
          "variable_store", )[[1]][1], paste0("Group_", specificSimNumber, "_folderList.RData")))}
    } # makes the folder for multirun results, saves multiRun_folderList there.
    
    
    
    source(file.path('scripts', 'Source_Visualizing_Data.R'))
    multirun_directory <- paste0(strsplit(multiRun_folderList[run_visual], "variable")[[1]][1], "multirun_output/", 
                                  list.files(path = paste0(strsplit(multiRun_folderList[run_visual], 
                                                  "variable")[[1]][1], "multirun_output/"), pattern = "multirun"))
    info <- readRDS(file = file.path(multiRun_folderList[run_visual], "metadata.RData"))
    data_convert <- paste0("converted_data", run_visual, " <- convert_stored_data(parameters = params, data_dir = \"", 
                           multiRun_folderList[run_visual], "\", simplification_factor = 100)")
    cat(data_convert, file = file.path("source","data_convert.R"), sep = "\n")
    source(file.path("source", "data_convert.R"), local=TRUE)
    
    sylrepblahz <- paste0("sylrepz", run_visual, " <- split_data(converted_data", run_visual, ", 1)")
    sdstbxblahn <- paste0("sdstbxn", run_visual, " <- split_data(converted_data", run_visual, ", 2)")
    cursitblahy <- paste0("cursity", run_visual, " <- split_data(converted_data", run_visual, ", 3)")
    curhisblaht <- paste0("curhist", run_visual, " <- split_data(converted_data", run_visual, ", 4)")
    eval(parse(text=c(sylrepblahz, sdstbxblahn, cursitblahy, curhisblaht)))
    
    sylrepzConveRtDS <- paste0("saveRDS(object = sylrepz", run_visual, ", file = \"", multirun_directory, "/SylReps", run_visual, ".RData\")")
    sdstbxnConveRtDS <- paste0("saveRDS(object = sdstbxn", run_visual, ", file = \"", multirun_directory, "/SylDist", run_visual, ".RData\")")
    cursityConveRtDS <- paste0("saveRDS(object = cursity", run_visual, ", file = \"", multirun_directory, "/Cursity", run_visual, ".RData\")")
    curhistConveRtDS <- paste0("saveRDS(object = curhist", run_visual, ", file = \"", multirun_directory, "/CurHist", run_visual, ".RData\")")
    eval(parse(text=c(sylrepzConveRtDS, sdstbxnConveRtDS, cursityConveRtDS, curhistConveRtDS)))
    
    
  } 
  
  datanames <- c("CurHist","Cursity","SylDist","SylReps")
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
  
  multirun_directory <- paste0(strsplit(multiRun_folderList[1], "variable")[[1]][1], 
                                "multirun_output/", 
                                 list.files(path = paste0(strsplit(multiRun_folderList[1], 
                                   "variable")[[1]][1], "multirun_output/"), pattern = "multirun"))

  for(i in 1:number_of_runs) {

    

    histthing <- paste0("curhistlist[[i]] <- readRDS(\"", multirun_directory, "/", histlist[i], "\")")
    sitything <- paste0("cursitylist[[i]] <- readRDS(\"", multirun_directory, "/", sitylist[i], "\")")
    sdstthing <- paste0("sdstbxnlist[[i]] <- readRDS(\"", multirun_directory, "/", sdstlist[i], "\")")
    repzthing <- paste0("sylrepzlist[[i]] <- readRDS(\"", multirun_directory, "/", repzlist[i], "\")")
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
                       ", datanames)")
  eval(parse(text=last_stats))
  
  R <- create_plot_info(info[[2]], info[[1]])
  info_make <- paste(paste0("sink(file = paste0(multirun_directory, \"Multirun - Parameters and Info\"))"), 
                     "cat(paste0(\"Number of Timesteps: \", info[[3]][1], \"\nNumber of Populations: \", info[[3]][2], \"\nPopulation Size: \", info[[3]][3], \"\nNumber of Syllables: \", info[[3]][4], \"\nNumber of Syllable Positions Assigned to Specific Probability Levels: \", info[[3]][5], \"\nNumber of Singers Sampled from One Population for Mating: \", info[[3]][7], \"\nNumber of Singers Sampled from One Population for Tutoring: \", info[[3]][6], \"\nProbability of Inheriting Curiosity Accurately: \", info[[3]][8], \"\nProbability of Learning Syllables Accurately from Parent: \", info[[3]][10], \"\nProbability of Learning Syllables Accurately from Tutor: \", info[[3]][9], \"\nProbability of Picking up Random Extra Syllables from Parent: \", info[[3]][12], \"\nProbability of Picking up Random Extra Syllables from Tutor: \", info[[3]][11], \"\nStandard Deviation of Randomly-picked-up Sylls from Established Mean: \", info[[3]][13], \"\nNumber of Rows in Population Calls Matrix: \", info[[3]][14], \"\nNumber of Columns in Pop Calls Matrix: \", info[[3]][15], \"\nPairing Pool Rows: \", info[[3]][16], \"\nPairing Pool Columns: \", info[[3]][17], \"\nPairing Pool Slices: \", info[[3]][18], \"\nCuriosity Counter Rows: \", info[[3]][19], \"\nCuriosity Counter Columns: \", info[[3]][20], \"\nPopulation Syllable Probability Rows: \", info[[3]][21], \"\nPopulation Probability Columns: \", info[[3]][22], \"\nLength of Curiosity Breaks Vector: \", info[[3]][23], \"\nLength of Zero to One Template: \", info[[3]][24], \"\nLearning Pool Rows: \", info[[3]][25], \"\nLearning Pool Columns: \", info[[3]][26], \"\nLearning Pool Slices: \", info[[3]][27]))", 
                     "sink()", sep = "\n")
  eval(parse(text=info_make))
  
  mins_n_maxes <- min_n_max(parameters = params, number_of_runs = number_of_runs, 
                            cursitylist = cursitylist, sdstbxnlist = sdstbxnlist, 
                            curhistlist = curhistlist, sylrepzlist = sylrepzlist)
  simple_plots(parameters = params, R = R, Q = "converted_data", extra_lines = TRUE, 
               number_of_runs = number_of_runs, cursitylist = cursitylist, 
               sdstbxnlist = sdstbxnlist, curhistlist = curhistlist, sylrepzlist = sylrepzlist, 
               mins_n_maxes = mins_n_maxes, saving_dir = multirun_directory)
  
  src.dir = file.path("scripts")
  file.names = dir(src.dir)[grep("Source", dir(src.dir))]
  
  dir.create(file.path(strsplit(multiRun_folderList[1], "/variable_store", )[[1]][1], "copy_of_scripts"))

  sapply(file.names, function(x) { 
    
    file.copy(from=file.path(src.dir, x), 
              to=file.path(strsplit(multiRun_folderList[1], "/variable_store", )[[1]][1], "copy_of_scripts", x), 
              overwrite = FALSE) })
  
  return(print("Exit Status: 0"))
}
