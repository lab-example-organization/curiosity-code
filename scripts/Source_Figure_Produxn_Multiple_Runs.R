
figProdMultRun <- function(specificSimNumber = 1, number_of_repeats, paramsSource = paramsSource) {
  
#     print("figpromultrunStart")

  connection <- file(description = file.path(
    "source","temp", paste0(
      specificSimNumber, "_sim_data.txt")), open = "rt")
  multiRun_folderList <- as.vector(read.table(connection, -1L)[[2]])
  close(connection)
#     print("multiRunFolderList")
  params = yaml.load_file (file.path ("parameters", paramsSource))
#     print ("params load")
  converted_data <- vector ("list", number_of_repeats)
  for (run_visual in 1:number_of_repeats) {
    #run_visual=1
    if (run_visual == 1) {
      multiRunTime <- format (Sys.time (), "%F-%H%M%S")
#     print ("run_visual == 1")
      if(!(dir.exists(file.path(strsplit(multiRun_folderList[run_visual], "/variable_store", )[[1]][1], "multirun_output")))) {
#     print ("makin multirun_output")
        dir.create(file.path(strsplit(multiRun_folderList[run_visual], "/variable_store", )[[1]][1], "multirun_output"))
#     print ("makin specific multirun_output folder")
        dir.create(file.path(strsplit(multiRun_folderList[run_visual], "/variable_store", )[[1]][1], "multirun_output", paste0(multiRunTime, "-GMT-multirun-output")))
      }
#     print("Do we need to record the list of folders?")
      if(!(file.exists(file.path(strsplit(
        multiRun_folderList[run_visual], "variable_store", )[[1]][1], 
        paste0("Group_", specificSimNumber, "_folderList.RData"))))) {
          # saveRDS(object = multiRun_folderList, file = 
          #              file.path(strsplit(multiRun_folderList[run_visual], 
          # "variable_store", )[[1]][1], paste0("Group_", specificSimNumber, "_folderList.RData")))}
#     print ("yes we do.")
          saveRDS(multiRun_folderList, file.path(strsplit(
              multiRun_folderList[run_visual], "variable_store", 
            )[[1]][1], paste0(
              "Group_", specificSimNumber, "_folderList.RData")))
      }
    } # makes the folder for multirun results, saves multiRun_folderList there.
    
    
#     print(paste0("source SVD - ", run_visual, " round"))
    source(file.path("scripts", "Source_Visualizing_Data.R"))
#     print("multirun_directory")
    multirun_directory <- paste0(strsplit(multiRun_folderList[run_visual],
                                  "variable")[[1]][1], "multirun_output/", 
                                  list.files(
                                    path = paste0(
                                      strsplit(multiRun_folderList[run_visual],
                                      "variable")[[1]][1], "multirun_output/"), 
                                    pattern = "multirun_output$"))
#     print("sourcing info")
    info <- readRDS(file = file.path(multiRun_folderList[run_visual], "metadata.RData"))

    # info1 <- fread(file.path(multiRun_folderList[run_visual], "docnamez.csv"))
    # info2 <- fread(file.path(multiRun_folderList[run_visual], "datez.csv"))
    # info3 <- fread(file.path(multiRun_folderList[run_visual], "deetz.csv"))

    # data_convert <- paste0("converted_data", run_visual, " <- convert_stored_data(parms = params, data_dir = \"", 
    #                        multiRun_folderList[run_visual], "\", simpleObjectSize = simplification_factor)")
    # cat(data_convert, file = file.path("source", "RtempFiles", paste0(specificSimNumber, "-", run_visual, "_data_convert.R")), sep = "\n")
    # source(file.path("source", "RtempFiles", paste0(specificSimNumber, "-", run_visual, "_data_convert.R")), local=TRUE)
#     print("converted_data time")
    converted_data[[run_visual]] <- convert_stored_data(parms = params, data_dir = multiRun_folderList[run_visual])
#     print("process_data time")
    process_data(converted_data, specificRepeat = run_visual, path = multirun_directory)

    # movingOutput <- paste0("process_data(converted_data", run_visual, ", specificRepeat = run_visual, path = multirun_directory)")
    # eval(parse(text=movingOutput))
  } 
  
#     print("makin' lists")
  datanames <- c("CurHist","Cursity","SylDist","SylReps")
  listnames <- c("hist","sity","sdst","repz")
  for(i in 1:4) {
    listlister <- paste0(listnames[i], "list <- vector(mode = \"character\", length = number_of_repeats)")
    listmaker <- paste0(listnames[i], "list[", 1:number_of_repeats, "] <- \"", datanames[i], 1:number_of_repeats, ".RData\"")
    eval(parse(text=c(listlister, listmaker)))
  }
  
  
  sylrepzlist <- list()
  sdstbxnlist <- list()
  cursitylist <- list()
  curhistlist <- list()

  multirun_directory <- paste0(strsplit(multiRun_folderList[1], "variable")[[1]][1], 
                                "multirun_output/", 
                                 list.files(path = paste0(strsplit(multiRun_folderList[1], 
                                   "variable")[[1]][1], "multirun_output/"), pattern = "multirun_output$"))
#     print("sourcing from lists")
  for(i in 1:number_of_repeats) {
    curhistlist[[i]] <- readRDS(paste0(multirun_directory, "/", histlist[i]))
    cursitylist[[i]] <- readRDS(paste0(multirun_directory, "/", sitylist[i]))
    sdstbxnlist[[i]] <- readRDS(paste0(multirun_directory, "/", sdstlist[i]))
    sylrepzlist[[i]] <- readRDS(paste0(multirun_directory, "/", repzlist[i]))

    # curhistlist[[i]] <- fread(file.path(multirun_directory, histlist[i]))
    # cursitylist[[i]] <- fread(file.path(multirun_directory, sitylist[i]))
    # sdstbxnlist[[i]] <- fread(file.path(multirun_directory, sdstlist[i]))
    # sylrepzlist[[i]] <- fread(file.path(multirun_directory, repzlist[i]))
  }
  


  sylrepzlist[[number_of_repeats + 1]] <- sylrepzlist[[number_of_repeats]]
  sdstbxnlist[[number_of_repeats + 1]] <- sdstbxnlist[[number_of_repeats]]
  cursitylist[[number_of_repeats + 1]] <- cursitylist[[number_of_repeats]]
  curhistlist[[number_of_repeats + 1]] <- curhistlist[[number_of_repeats]]
#     print("histlist")
  for(i in 1:length(curhistlist[[1]])) {
    eval(parse(text=paste0("curhistlist[[number_of_repeats + 1]][i] <- mean(c(curhistlist[[", 
                           paste0(1:(number_of_repeats - 1),"]][i],curhistlist[[", collapse=''), 
                           number_of_repeats, "]][i]))")))
  }
#     print("sitylist")
  for(i in 1:length(cursitylist[[1]])) {
    eval(parse(text=paste0("cursitylist[[number_of_repeats + 1]][i] <- mean(c(cursitylist[[", 
                           paste0(1:(number_of_repeats - 1),"]][i],cursitylist[[", collapse=''), 
                           number_of_repeats, "]][i]))")))
  }
#     print("sdstlist")
  for(i in 1:length(sdstbxnlist[[1]])) {
    eval(parse(text=paste0("sdstbxnlist[[number_of_repeats + 1]][i] <- mean(c(sdstbxnlist[[", 
                           paste0(1:(number_of_repeats - 1),"]][i],sdstbxnlist[[", collapse=''), 
                           number_of_repeats, "]][i]))")))
  }
#     print("repzlist")
  for(i in 1:length(sylrepzlist[[1]])) {
    eval(parse(text=paste0("sylrepzlist[[number_of_repeats + 1]][i] <- mean(c(sylrepzlist[[", 
                           paste0(1:(number_of_repeats - 1),"]][i],sylrepzlist[[", collapse=''), 
                           number_of_repeats, "]][i]))")))
  }
#     print("last_stats")
  last_stats <- paste0("rm(sylrepz", number_of_repeats, ", sdstbxn", number_of_repeats,
                       ", cursity", number_of_repeats, ", curhist", number_of_repeats,
                       ", last_stats, histlist, sitylist, sdstlist, repzlist",
                       ", listlister, listmaker, listnames, datanames)")
                       
  eval(parse(text=last_stats))
  
  R <- create_plot_info(info[[2]], info[[1]])
  info_make <- paste(paste0("sink(file = paste0(multirun_directory, \"Multirun - Parameters and Info\"))"), 
                     "cat(paste0(\"Number of Timesteps: \", info[[3]][1], \"\nNumber of Populations: \", info[[3]][2], \"\nPopulation Size: \", info[[3]][3], \"\nNumber of Syllables: \", info[[3]][4], \"\nNumber of Syllable Positions Assigned to Specific Probability Levels: \", info[[3]][5], \"\nNumber of Singers Sampled from One Population for Mating: \", info[[3]][7], \"\nNumber of Singers Sampled from One Population for Tutoring: \", info[[3]][6], \"\nProbability of Inheriting Curiosity Accurately: \", info[[3]][8], \"\nProbability of Learning Syllables Accurately from Parent: \", info[[3]][10], \"\nProbability of Learning Syllables Accurately from Tutor: \", info[[3]][9], \"\nProbability of Picking up Random Extra Syllables from Parent: \", info[[3]][12], \"\nProbability of Picking up Random Extra Syllables from Tutor: \", info[[3]][11], \"\nStandard Deviation of Randomly-picked-up Sylls from Established Mean: \", info[[3]][13], \"\nNumber of Rows in Population Calls Matrix: \", info[[3]][14], \"\nNumber of Columns in Pop Calls Matrix: \", info[[3]][15], \"\nPairing Pool Rows: \", info[[3]][16], \"\nPairing Pool Columns: \", info[[3]][17], \"\nPairing Pool Slices: \", info[[3]][18], \"\nCuriosity Counter Rows: \", info[[3]][19], \"\nCuriosity Counter Columns: \", info[[3]][20], \"\nPopulation Syllable Probability Rows: \", info[[3]][21], \"\nPopulation Probability Columns: \", info[[3]][22], \"\nLength of Curiosity Breaks Vector: \", info[[3]][23], \"\nLength of Zero to One Template: \", info[[3]][24], \"\nLearning Pool Rows: \", info[[3]][25], \"\nLearning Pool Columns: \", info[[3]][26], \"\nLearning Pool Slices: \", info[[3]][27]))", 
                     "sink()", sep = "\n")
  eval(parse(text=info_make))
#     print("info made")
  mins_n_maxes <- min_n_max(parameters = params, number_of_runs = number_of_repeats, 
                            cursitylist = cursitylist, sdstbxnlist = sdstbxnlist, 
                            curhistlist = curhistlist, sylrepzlist = sylrepzlist)
#     print("mins_n_maxes")
  simple_plots(parameters = params, R = R, Q = "converted_data", extra_lines = TRUE, 
               number_of_runs = number_of_repeats, cursitylist = cursitylist, 
               sdstbxnlist = sdstbxnlist, curhistlist = curhistlist, sylrepzlist = sylrepzlist, 
               mins_n_maxes = mins_n_maxes, saving_dir = multirun_directory)
#     print("simple_plots done")

  srcDir = file.path("scripts")
  file.names = dir(srcDir)[grep("Source", dir(srcDir))]
  dir.create(file.path(strsplit(multiRun_folderList[1], "/variable_store", )[[1]][1], "copy_of_scripts"))
  sapply(file.names, function(x) { 
    file.copy(from=file.path(srcDir, x), 
              to=file.path(strsplit(multiRun_folderList[1], "/variable_store", )[[1]][1], "copy_of_scripts", x), 
              overwrite = FALSE) })

  parmDir = file.path("parameters")
  file.names = dir(parmDir)[grep("*.yaml", dir(parmDir))]
  dir.create(file.path(strsplit(multiRun_folderList[1], "/variable_store", )[[1]][1], "copy_of_params"))
  sapply(file.names, function(x) { 
    file.copy(from=file.path(parmDir, x), 
              to=file.path(strsplit(multiRun_folderList[1], "/variable_store", )[[1]][1], "copy_of_params", x), 
              overwrite = FALSE) })
  
  return(print("Exit Status: 0"))
}

