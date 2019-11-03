
figprodmultrun <- function(specificsimnumber = 1, number_of_repeats, 
                           paramssource = paramssource) {
  
#     print("figprodmultrunStart")

  connection <- file(description = file.path(
    "source","temp", paste0(
      specificsimnumber, "_sim_data.txt")), open = "rt")
  multirun_folderlist <- as.vector(read.table(connection, -1L)[[2]])
  close(connection)
#     print("multiRunFolderList")
  params = yaml.load_file (file.path ("parameters", paramssource))
#     print ("params load")
  converted_data <- vector ("list", number_of_repeats)

#  print(paste0("source SVD"))
  source(file.path("scripts", "Source_Visualizing_Data.R"))
  
  for (run_visual in 1:number_of_repeats) {
    # run_visual=1
    if (run_visual == 1) {
      multiRunTime <- format (Sys.time (), "%F-%H%M%S")
#     print ("run_visual == 1")
      if(!(dir.exists(file.path(strsplit(
        multirun_folderlist[run_visual], "/variable_store", )[[1]][1], 
        "multirun_output")))) {
#     print ("makin multirun_output")
        dir.create(file.path(strsplit(
          multirun_folderlist[run_visual], "/variable_store", )[[1]][1],
          "multirun_output"))
#     print ("makin specific multirun_output folder")
        dir.create(file.path(strsplit(
          multirun_folderlist[run_visual], "/variable_store", )[[1]][1],
          "multirun_output", paste0(multiRunTime, 
          "-GMT-multirun-output")))
      }
#     print("Do we need to record the list of folders?")
      if(!(file.exists(file.path(strsplit(
        multirun_folderlist[run_visual], "variable_store", )[[1]][1], 
        paste0("Group_", specificsimnumber, "_folderList.RData"))))) {
          # saveRDS(object = multirun_folderlist, file = 
          #         file.path(strsplit(multirun_folderlist[run_visual], 
          #         "variable_store", )[[1]][1], paste0("Group_", 
          #         specificsimnumber, "_folderList.RData")))}
          # print ("yes we do.")

          saveRDS(multirun_folderlist, file.path(strsplit(
              multirun_folderlist[run_visual], "variable_store", 
            )[[1]][1], paste0(
              "Group_", specificsimnumber, "_folderList.RData")))
      }
    } 
# makes the folder for multirun 
# results, saves multirun_folderlist there.
    
    
    
#     print("multirun_directory")
    multirun_directory <- paste0(
      strsplit(multirun_folderlist[run_visual],
      "variable")[[1]][1], "multirun_output/", 
      list.files(
      path = paste0(
      strsplit(multirun_folderlist[run_visual],
      "variable")[[1]][1], "multirun_output/"), 
      pattern = "multirun_output$"))
#     print("sourcing info")
    info <- readRDS(file = file.path(
      multirun_folderlist[run_visual], "metadata.RData"))

    # data_convert <- paste0("converted_data", run_visual, " <- concatenate_data(parms = params, data_dir = \"", 
    #                        multirun_folderlist[run_visual], "\", simpleObjectSize = simplification_factor)")
    # cat(data_convert, file = file.path("source", "RtempFiles", paste0(specificsimnumber, "-", run_visual, "_data_convert.R")), sep = "\n")
    # source(file.path("source", "RtempFiles", paste0(specificsimnumber, "-", run_visual, "_data_convert.R")), local=TRUE)
#     print("converted_data time")
    converted_data <- concatenate_data(specific_run = run_visual, converteddata = converted_data, parms = params, data_dir = multirun_folderlist)
    # print("process_data time")
    # process_data(converted_data[[run_visual]], specificrepeat = run_visual, path = multirun_directory)

    # movingOutput <- paste0("process_data(converted_data", run_visual, ", specificrepeat = run_visual, path = multirun_directory)")
    # eval(parse(text=movingOutput))
  } 
  
  process_data(converted_data, specificrepeat = number_of_repeats, path = multirun_directory)

#  print("makin' lists")
  datanames <- c("CurHist","Cursity","SylDist","SylReps")
  listnames <- c("hist","sity","sdst","repz")
  for(i in 1:4) {
    listlister <- paste0(listnames[i], "list <- vector(mode = \"character\", length = number_of_repeats)")
    listmaker <- paste0(listnames[i], "list[", 1:number_of_repeats, "] <- \"", datanames[i], 1:number_of_repeats, ".RData\"")
    eval(parse(text=c(listlister, listmaker)))
  }
  
  
  curhistlist <- vector (mode = "list", length = number_of_repeats + 1)
  sylrepzlist <- vector (mode = "list", length = number_of_repeats + 1)
  sdstbxnlist <- vector (mode = "list", length = number_of_repeats + 1)
  cursitylist <- vector (mode = "list", length = number_of_repeats + 1)

  # multirun_directory <- paste0(strsplit(multirun_folderlist[1], "variable")[[1]][1], 
  #                               "multirun_output/", 
  #                                list.files(path = paste0(strsplit(multirun_folderlist[1], 
  #                                  "variable")[[1]][1], "multirun_output/"), pattern = "multirun_output$"))

  multirun_directory <- paste0(strsplit(multirun_folderlist[1], "variable")[[1]][1], 
                                "multirun_output/")

#     print("sourcing from lists")
  for(i in 1:number_of_repeats) {
    # print(paste0(multirun_directory, "/", histlist[i]))
    curhistlist[[i]] <- readRDS(paste0(multirun_directory, histlist[i]))
    # print(paste0(multirun_directory, "/", sitylist[i]))
    cursitylist[[i]] <- readRDS(paste0(multirun_directory, sitylist[i]))
    # print(paste0(multirun_directory, "/", sdstlist[i]))
    sdstbxnlist[[i]] <- readRDS(paste0(multirun_directory, sdstlist[i]))
    # print(paste0(multirun_directory, "/", repzlist[i]))
    sylrepzlist[[i]] <- readRDS(paste0(multirun_directory, repzlist[i]))

    # curhistlist[[i]] <- fread(file.path(multirun_directory, histlist[i]))
    # cursitylist[[i]] <- fread(file.path(multirun_directory, sitylist[i]))
    # sdstbxnlist[[i]] <- fread(file.path(multirun_directory, sdstlist[i]))
    # sylrepzlist[[i]] <- fread(file.path(multirun_directory, repzlist[i]))
  }
#  print(paste0("curhistlist length = ", length(curhistlist)))
#  print(paste0("cursitylist length = ", length(cursitylist)))
#  print(paste0("sdstbxnlist length = ", length(sdstbxnlist)))
#  print(paste0("sylrepzlist length = ", length(sylrepzlist)))
  
  

  # sylrepzlist[[number_of_repeats + 1]] <- sylrepzlist[[number_of_repeats]]
  # sdstbxnlist[[number_of_repeats + 1]] <- sdstbxnlist[[number_of_repeats]]
  # cursitylist[[number_of_repeats + 1]] <- cursitylist[[number_of_repeats]]
  # curhistlist[[number_of_repeats + 1]] <- curhistlist[[number_of_repeats]]
#     print("histlist")
  for(i in 1:length(curhistlist[[1]])) {
    eval(parse(text=paste0("curhistlist[[number_of_repeats + 1]][i] <- mean(c(curhistlist[[", 
                           paste0(1:(number_of_repeats - 1),"]][i],curhistlist[[", collapse=''), 
                           number_of_repeats, "]][i]))")))
  }

#  print ("dimensions of curhistlist[[1]] - ")
#  print (dim (curhistlist[[1]]))
  thing <- dim (curhistlist[[1]])
  dim (curhistlist[[number_of_repeats + 1]]) <- thing

#  print("sitylist")
  for(i in 1:length(cursitylist[[1]])) {
    eval(parse(text=paste0("cursitylist[[number_of_repeats + 1]][i] <- mean(c(cursitylist[[", 
                           paste0(1:(number_of_repeats - 1),"]][i],cursitylist[[", collapse=''), 
                           number_of_repeats, "]][i]))")))
  }

#  print ("dimensions of cursitylist[[1]] - ")
#  print (dim (cursitylist[[1]]))
  thing <- dim (cursitylist[[1]])
  dim (cursitylist[[number_of_repeats + 1]]) <- thing

#  print("sdstlist")
  
  for(i in 1:length(sdstbxnlist[[1]])) {
    eval(parse(text=paste0("sdstbxnlist[[number_of_repeats + 1]][i] <- mean(c(sdstbxnlist[[", 
                           paste0(1:(number_of_repeats - 1),"]][i],sdstbxnlist[[", collapse=''), 
                           number_of_repeats, "]][i]))")))
  }

#  print ("dimensions of sdstbxnlist[[1]] - ")
#  print (dim (sdstbxnlist[[1]]))
  thing <- dim (sdstbxnlist[[1]])
  dim (sdstbxnlist[[number_of_repeats + 1]]) <- thing

#  print("repzlist")

  for(i in 1:length(sylrepzlist[[1]])) {
    eval(parse(text=paste0("sylrepzlist[[number_of_repeats + 1]][i] <- mean(c(sylrepzlist[[", 
                           paste0(1:(number_of_repeats - 1),"]][i],sylrepzlist[[", collapse=''), 
                           number_of_repeats, "]][i]))")))
  }

#  print ("dimensions of sylrepzlist[[1]] - ")
#  print (dim (sylrepzlist[[1]]))
  thing <- dim (sylrepzlist[[1]])
  dim (sylrepzlist[[number_of_repeats + 1]]) <- thing

#  print("last_stats")
  last_stats <- paste0("rm(sylrepz", number_of_repeats, ", sdstbxn", number_of_repeats,
                       ", cursity", number_of_repeats, ", curhist", number_of_repeats,
                       ", last_stats, histlist, sitylist, sdstlist, repzlist",
                       ", listlister, listmaker, listnames, datanames)")
                       
  eval(parse(text=last_stats))
  
  plot_info <- create_plot_info(info[[2]], info[[1]])
  info_make <- paste(paste0("sink(file = paste0(multirun_directory, \"Multirun - Parameters and Info\"))"), 
                     "cat(paste0(\"Number of Timesteps: \", info[[3]][1], \"\nNumber of Populations: \", info[[3]][2], \"\nPopulation Size: \", info[[3]][3], \"\nNumber of Syllables: \", info[[3]][4], \"\nNumber of Syllable Positions Assigned to Specific Probability Levels: \", info[[3]][5], \"\nNumber of Singers Sampled from One Population for Mating: \", info[[3]][7], \"\nNumber of Singers Sampled from One Population for Tutoring: \", info[[3]][6], \"\nProbability of Inheriting Curiosity Accurately: \", info[[3]][8], \"\nProbability of Learning Syllables Accurately from Parent: \", info[[3]][10], \"\nProbability of Learning Syllables Accurately from Tutor: \", info[[3]][9], \"\nProbability of Picking up Random Extra Syllables from Parent: \", info[[3]][12], \"\nProbability of Picking up Random Extra Syllables from Tutor: \", info[[3]][11], \"\nStandard Deviation of Randomly-picked-up Sylls from Established Mean: \", info[[3]][13], \"\nNumber of Rows in Population Calls Matrix: \", info[[3]][14], \"\nNumber of Columns in Pop Calls Matrix: \", info[[3]][15], \"\nPairing Pool Rows: \", info[[3]][16], \"\nPairing Pool Columns: \", info[[3]][17], \"\nPairing Pool Slices: \", info[[3]][18], \"\nCuriosity Counter Rows: \", info[[3]][19], \"\nCuriosity Counter Columns: \", info[[3]][20], \"\nPopulation Syllable Probability Rows: \", info[[3]][21], \"\nPopulation Probability Columns: \", info[[3]][22], \"\nLength of Curiosity Breaks Vector: \", info[[3]][23], \"\nLength of Zero to One Template: \", info[[3]][24], \"\nLearning Pool Rows: \", info[[3]][25], \"\nLearning Pool Columns: \", info[[3]][26], \"\nLearning Pool Slices: \", info[[3]][27]))", 
                     "sink()", sep = "\n")
  eval(parse(text=info_make))
#     print("info made")
  mins_n_maxes <- min_n_max(parameters = params, number_of_runs = number_of_repeats, 
                            cursitylist = cursitylist, sdstbxnlist = sdstbxnlist, 
                            curhistlist = curhistlist, sylrepzlist = sylrepzlist)
#     print("mins_n_maxes: ")
#     print (mins_n_maxes)
#     print ("length of cursitylist: ")
#     print (dim (cursitylist[[number_of_repeats + 1]]))
  simple_plots(parameters = params, plot_info = plot_info, converted_data = "converted_data", extra_lines = TRUE, 
               number_of_runs = number_of_repeats, cursitylist = cursitylist, 
               sdstbxnlist = sdstbxnlist, curhistlist = curhistlist, sylrepzlist = sylrepzlist, 
               mins_n_maxes = mins_n_maxes, saving_dir = multirun_directory)
#     print("simple_plots done")

  # srcdir = file.path("scripts")
  # file.names = dir(srcdir)[grep("Source", dir(srcdir))]
  # dir.create(file.path(strsplit(multirun_folderlist[1], "/variable_store", )[[1]][1], "copy_of_scripts"))
  # sapply(file.names, function(x) { 
  #   file.copy(from=file.path(srcdir, x), 
  #             to=file.path(strsplit(multirun_folderlist[1], "/variable_store", )[[1]][1], "copy_of_scripts", x), 
  #             overwrite = FALSE) })

  # parmdir = file.path("parameters")
  # file.names = dir(parmdir)[grep("*.yaml", dir(parmdir))]
  # dir.create(file.path(strsplit(multirun_folderlist[1], "/variable_store", )[[1]][1], "copy_of_params"))
  # sapply(file.names, function(x) { 
  #   file.copy(from=file.path(parmdir, x), 
  #             to=file.path(strsplit(multirun_folderlist[1], "/variable_store", )[[1]][1], "copy_of_params", x), 
  #             overwrite = FALSE) })



  srcdir = file.path("scripts")
  file.names = dir(srcdir)[grep("Source", dir(srcdir))]
  dir.create(file.path(strsplit(multirun_folderlist[1], "/variable_store", )[[1]][1], "copy_of_scripts"))
  # sapply(file.names, function(x) { 
  #   file.copy(from=file.path(srcdir, x), 
  #             to=file.path(strsplit(multirun_folderlist[1], "/variable_store", )[[1]][1], "copy_of_scripts", x), 
  #             overwrite = FALSE) })
  zipr(file.path(strsplit(multirun_folderlist[1], "/variable_store", )[[1]][1], "copy_of_scripts", "scriptsFiles.zip"), file.path(srcdir), T, 9, T)
  

  parmdir = file.path("parameters")
  file.names = dir(parmdir)[grep("*.yaml", dir(parmdir))]
  dir.create(file.path(strsplit(multirun_folderlist[1], "/variable_store", )[[1]][1], "copy_of_params"))
  # sapply(file.names, function(x) { 
  #   file.copy(from=file.path(parmdir, x), 
  #             to=file.path(strsplit(multirun_folderlist[1], "/variable_store", )[[1]][1], "copy_of_params", x), 
  #             overwrite = FALSE) })
  zipr(file.path(strsplit(multirun_folderlist[1], "/variable_store", )[[1]][1], "copy_of_params", "paramsFiles.zip"), file.path(parmdir), T, 9, T)


  return(print("Exit Status: 0"))
}

