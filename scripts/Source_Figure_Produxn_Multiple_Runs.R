# results_pool <- c ("parentNoInv",        "childF1NoInv",       "childMalHihInv",  "childMalLowInv",     "childFemLowInv",
#   "childBothLowInv",    "childFemHihInv",     "childBothHihInv", "childSmolMalHihInv", "childSmolMalLowInv",
#   "childSmolFemHihInv", "childSmolFemLowInv", "childNoInvF2",    "childNoInvF3",       "childNoInvF4",
#   "childNoInvF5",       "childNoInvF6",       "childNoInvF7",    "childNoInvF8",       "childNoInvF9",
#   "childNoInvF10", "childLateInvMalHih", "childLateInvMalLow", "childLateInvFemHih", "childLateInvFemLow",
#   "childLateInvBothHih", "childLateInvBothLow", "childLateSmolInvMalHih", "childLateSmolInvMalLow",  "childLateSmolInvFemHih",
#    "childLateSmolInvFemLow")

figprodmultrun <- function (
  specificsimnumber = 1,
  number_of_repeats,
  paramssource = paramssource,
  redo = FALSE,
  recolorize = FALSE,
  results_dir = FALSE,
  lineplots = FALSE,
  curMeans_only = FALSE,
  recolorize_style = "variance",
  compare_subsets = FALSE) { 
    # example of text for results_dir is "childLateSmolInvFemLow"
    #     print ("figprodmultrunStart")
    source (file.path ("scripts", "Source_Visualizing_Data.R"))

    params = yaml.load_file (file.path ("parameters", paramssource))
    #     print ("params load")
    converted_data <- vector ("list", number_of_repeats)


    ### Control flow giving us back the object pointing us to simulation folders:
    ###   "redo" allows us to skip doing some stuff that would be redundant - make Group_###_folderList.RData, saving a backup copy of all scripts and params files at the time that the code runs
    ###   "results_dir" has been used once, for the "redo function" - 'results_dir = "parentNoInv"' - 
    if (redo != FALSE) {
      if (results_dir != FALSE) {
        if (! (file.exists (file.path ("results", paste0 ("tenKfiveByFive_", results_dir), (
            list.files (file.path ("results", paste0 ("tenKfiveByFive_", results_dir)), pattern = paste0 ("*_", specificsimnumber, "*_")) [
              length (list.files (file.path ("results", paste0 ("tenKfiveByFive_", results_dir)), pattern = paste0 ("*_", specificsimnumber, "*_")))
            ] # This selects the latest iteration of this sim number, so this is the line to change if that is no longer true
          ), (paste0 ("Group_", specificsimnumber, "_folderList.RData")))))) {
            # stop ("results_dir doesn't know where to find Group_#_folderList.RData")
            connection <- file (description = file.path ("source","temp", paste0 (specificsimnumber, "_sim_data.txt")), open = "rt")
            multirun_folderlist <- as.vector (read.table (connection, -1L) [[2]])
            close (connection)
        } else {
          multirun_folderlist <- readRDS (file.path ("results", paste0 ("tenKfiveByFive_", results_dir), (
            list.files (file.path ("results", paste0 ("tenKfiveByFive_", results_dir)), pattern = paste0 ("*_", specificsimnumber, "*_")) [
              length (list.files (file.path ("results", paste0 ("tenKfiveByFive_", results_dir)), pattern = paste0 ("*_", specificsimnumber, "*_")))
            ] # This selects the latest iteration of this sim number, so this is the line to change if that is no longer true
          ), (paste0 ("Group_", specificsimnumber, "_folderList.RData"))))
  
          for (run_visual in 1 : number_of_repeats) {
            multirun_folderlist [run_visual] <- paste0 (
              file.path ("results", paste0 ("tenKfiveByFive_", results_dir)),
              strsplit (multirun_folderlist, "results") [[1]][2]
            )
          }
        }
      } else {
         if (! (file.exists (file.path ("results", (
          list.files (file.path ("results"), pattern = paste0 ("*_", specificsimnumber, "*_")) [
            length (list.files (file.path ("results"), pattern = paste0 ("*_", specificsimnumber, "*_")))
          ] # this selects... (see above)
        ), (paste0 ("Group_", specificsimnumber, "_folderList.RData")))))) {
            stop ("non results_dir doesn't know where to find Group_#_folderList.RData")
        } else {
          multirun_folderlist <- readRDS (file.path ("results", (
            list.files (file.path ("results"), pattern = paste0 ("*_", specificsimnumber, "*_")) [
              length (list.files (file.path ("results"), pattern = paste0 ("*_", specificsimnumber, "*_")))
            ] # this selects... (see above)
          ), (paste0 ("Group_", specificsimnumber, "_folderList.RData"))))
        }
      }
    } else {
      connection <- file (description = file.path ("source","temp", paste0 (specificsimnumber, "_sim_data.txt")), open = "rt")
      multirun_folderlist <- as.vector (read.table (connection, -1L) [[2]])
      close (connection)
  
      for (run_visual in 1 : number_of_repeats) {
        if (results_dir != FALSE) {
          multirun_folderlist [run_visual] <- paste0 (
            file.path ("results", paste0 ("tenKfiveByFive_", results_dir)),
            strsplit (multirun_folderlist, "results") [[1]][2]
          )
        }
        
        if (run_visual == 1) {
          multiRunTime <- format (Sys.time (), "%F-%H%M%S")
      #     print ("run_visual == 1")
          if (! (dir.exists (file.path (strsplit (
            multirun_folderlist [run_visual], "/variable_store", ) [[1]][1],
            "multirun_output")))) {
      #     print ("makin multirun_output")
            dir.create (file.path (strsplit (
              multirun_folderlist [run_visual], "/variable_store", ) [[1]][1],
              "multirun_output"))
      #     print ("makin specific multirun_output folder")
            dir.create (file.path (strsplit (
              multirun_folderlist [run_visual], "/variable_store", ) [[1]][1],
              "multirun_output", paste0 (multiRunTime,
              "-GMT-multirun-output")))
          }
      #     print ("Do we need to record the list of folders?")
          if (! (file.exists (file.path (strsplit (
            multirun_folderlist [run_visual], "variable_store", ) [[1]][1],
            paste0 ("Group_", specificsimnumber, "_folderList.RData"))))) {
              # print ("yes we do.")
              saveRDS (multirun_folderlist, file.path (strsplit (
                  multirun_folderlist [run_visual], "variable_store",
                ) [[1]][1], paste0 (
                  "Group_", specificsimnumber, "_folderList.RData")))
          }
        }
      #     print ("multirun_directory")
        multirun_directory <- paste0 (
          strsplit (multirun_folderlist [run_visual],
          "variable") [[1]][1], "multirun_output/",
          list.files (
          path = paste0 (
          strsplit (multirun_folderlist [run_visual],
          "variable") [[1]][1], "multirun_output/"),
          pattern = "multirun-output$"))
      #     print ("converted_data time")
        converted_data <- concatenate_data (
          specific_run = run_visual,
          converteddata = converted_data,
          parms = params,
          data_dir = multirun_folderlist
        )
      #     print ("process_data time")
      }
      process_data (
        data_conglomerate = converted_data,
        specificrepeat = number_of_repeats,
        path = multirun_directory
      )
    }
    #     print ("multiRunFolderList")
  
  
  
    # params = yaml.load_file (file.path ("parameters", paramssource))
    # #     print ("params load")
    # converted_data <- vector ("list", number_of_repeats)
  
    #  print (paste0 ("source SVD"))
    # source (file.path ("scripts", "Source_Visualizing_Data.R"))
  
    # if (recolorize == FALSE) {
    #   for (run_visual in 1 : number_of_repeats) {
    #     if (results_dir != FALSE) {
    #       multirun_folderlist [run_visual] <- paste0 (
    #         file.path ("results", paste0 ("tenKfiveByFive_", results_dir)),
    #         strsplit (multirun_folderlist, "results") [[1]][2]
    #       )
    #     }
    #     # str_split (multirun_folderlist [1], "/") [[1]][1]
    #     # tenKfiveByFive_
    #     # paste0 (str_split (multirun_folderlist [run_visual], "/") [[1]][1], "tenKfiveByFive_", results_dir[run_visual])
    #     # run_visual=1
    #     if (run_visual == 1) {
    #       multiRunTime <- format (Sys.time (), "%F-%H%M%S")
    #   #     print ("run_visual == 1")
    #       if (! (dir.exists (file.path (strsplit (
    #         multirun_folderlist [run_visual], "/variable_store", ) [[1]][1],
    #         "multirun_output")))) {
    #   #     print ("makin multirun_output")
    #         dir.create (file.path (strsplit (
    #           multirun_folderlist [run_visual], "/variable_store", ) [[1]][1],
    #           "multirun_output"))
    #   #     print ("makin specific multirun_output folder")
    #         dir.create (file.path (strsplit (
    #           multirun_folderlist [run_visual], "/variable_store", ) [[1]][1],
    #           "multirun_output", paste0 (multiRunTime,
    #           "-GMT-multirun-output")))
    #       }
    #   #     print ("Do we need to record the list of folders?")
    #       if (! (file.exists (file.path (strsplit (
    #         multirun_folderlist [run_visual], "variable_store", ) [[1]][1],
    #         paste0 ("Group_", specificsimnumber, "_folderList.RData"))))) {
    #           # print ("yes we do.")
    #           saveRDS (multirun_folderlist, file.path (strsplit (
    #               multirun_folderlist [run_visual], "variable_store",
    #             ) [[1]][1], paste0 (
    #               "Group_", specificsimnumber, "_folderList.RData")))
    #       }
    #     }
    #   #     print ("multirun_directory")
    #     multirun_directory <- paste0 (
    #       strsplit (multirun_folderlist [run_visual],
    #       "variable") [[1]][1], "multirun_output/",
    #       list.files (
    #       path = paste0 (
    #       strsplit (multirun_folderlist [run_visual],
    #       "variable") [[1]][1], "multirun_output/"),
    #       pattern = "multirun-output$"))
    #   #     print ("converted_data time")
    #     converted_data <- concatenate_data (
    #       specific_run = run_visual,
    #       converteddata = converted_data,
    #       parms = params,
    #       data_dir = multirun_folderlist
    #     )
    #   #     print ("process_data time")
    #   }
    #   process_data (
    #     data_conglomerate = converted_data,
    #     specificrepeat = number_of_repeats,
    #     path = multirun_directory
    #   )
    # }
    #     print ("sourcing info")
    info <- readRDS (file = file.path (
      multirun_folderlist [1], "metadata.RData"))
    #     print ("makin' lists")
    datanames <- c ("CurHist","Cursity","SylDist","SylReps")
    listnames <- c ("hist","sity","sdst","repz")
    for (i in 1 : 4) {
      listlister <- paste0 (listnames [i], "list <- vector (mode = \"character\", length = number_of_repeats)")
      listmaker <- paste0 (listnames [i], "list [", 1 : number_of_repeats, "] <- \"", datanames[i], 1 : number_of_repeats, ".RData\"")
      eval (parse (text = c (listlister, listmaker)))
    }
  
    curhistlist <- vector (mode = "list", length = number_of_repeats + 1)
    sylrepzlist <- vector (mode = "list", length = number_of_repeats + 1)
    sdstbxnlist <- vector (mode = "list", length = number_of_repeats + 2)
    cursitylist <- vector (mode = "list", length = number_of_repeats + 1)
  
    multirun_directory <- paste0 (strsplit (multirun_folderlist [1], "variable") [[1]][1],
                                  "multirun_output/")
  
    if (length (list.files (multirun_directory, pattern = "multirun-output")) != 0) {
      if (length (list.files (file.path (multirun_directory, list.files (multirun_directory, pattern = "multirun-output")))) != 0) {
        multirun_directory <- paste0 (multirun_directory, list.files (multirun_directory, pattern = "multirun-output"), "/")
      }
    }
    # else if (length (list.files (multirun_directory, pattern = "multirun-output")) == 0) {
    #   multirun_directory <- paste0 (strsplit (multirun_directory[1], "multirun_output") [[1]][1],
    #                               "multirun_output/")
    # }
  
    #     print ("sourcing from lists")
    for (i in 1 : number_of_repeats) {
      # print (paste0 (multirun_directory, "/", histlist [i]))
      curhistlist [[i]] <- readRDS (paste0 (multirun_directory, histlist [i]))
      # print (paste0 (multirun_directory, "/", sitylist [i]))
      cursitylist [[i]] <- readRDS (paste0 (multirun_directory, sitylist [i]))
      # print (paste0 (multirun_directory, "/", sdstlist [i]))
      sdstbxnlist [[i]] <- readRDS (paste0 (multirun_directory, sdstlist [i]))
      # print (paste0 (multirun_directory, "/", repzlist [i]))
      sylrepzlist [[i]] <- readRDS (paste0 (multirun_directory, repzlist [i]))
    }
    #     print ("histlist")
    for (i in 1 : length (curhistlist [[1]])) {
      eval (parse (text = paste0 ("curhistlist [[number_of_repeats + 1]][i] <- mean (c (curhistlist [[",
                             paste0 (1 : (number_of_repeats - 1),"]][i],curhistlist [[", collapse=''),
                             number_of_repeats, "]][i]))")))
    }
  
    #     print (paste0 ("dimensions of curhistlist [[1]] - ", dim (curhistlist [[1]])))
    dim (curhistlist [[number_of_repeats + 1]]) <- dim (curhistlist [[1]])
  
    #     print ("sitylist")
    for (i in 1 : length (cursitylist [[1]])) {
      eval (parse (text = paste0 ("cursitylist [[number_of_repeats + 1]][i] <- mean (c (cursitylist [[",
                             paste0 (1 : (number_of_repeats - 1),"]][i],cursitylist [[", collapse=''),
                             number_of_repeats, "]][i]))")))
    }
  
    #     print (paste0 ("dimensions of cursitylist [[1]] - ", dim (cursitylist [[1]])))
    dim (cursitylist [[number_of_repeats + 1]]) <- dim (cursitylist [[1]])
  
    #     print ("sdstlist")
  
    for (i in 1 : length (sdstbxnlist [[1]])) {
      eval (parse (text = paste0 ("sdstbxnlist [[number_of_repeats + 1]][i] <- mean (c (sdstbxnlist [[",
                             paste0 (1 : (number_of_repeats - 1),"]][i],sdstbxnlist [[", collapse=''),
                             number_of_repeats, "]][i]))")))
    }
  
    #     print (paste0 ("dimensions of sdstbxnlist [[1]] - ", dim (sdstbxnlist [[1]])))
    dim (sdstbxnlist [[number_of_repeats + 1]]) <- dim (sdstbxnlist [[1]])
  
    #     print ("repzlist")
  
    for (i in 1 : length (sylrepzlist [[1]])) {
      eval (parse (text = paste0 ("sylrepzlist [[number_of_repeats + 1]][i] <- mean (c (sylrepzlist [[",
                             paste0 (1 : (number_of_repeats - 1),"]][i],sylrepzlist [[", collapse=''),
                             number_of_repeats, "]][i]))")))
    }
  
    #     print (paste0 ("dimensions of sylrepzlist [[1]] - ", dim (sylrepzlist [[1]])))
    dim (sylrepzlist [[number_of_repeats + 1]]) <- dim (sylrepzlist [[1]])
  
    #     print ("last_stats")
    last_stats <- paste0 ("rm (sylrepz", number_of_repeats, ", sdstbxn", number_of_repeats,
                         ", cursity", number_of_repeats, ", curhist", number_of_repeats,
                         ", last_stats, histlist, sitylist, sdstlist, repzlist",
                         ", listlister, listmaker, listnames, datanames)")
  
    eval (parse (text = last_stats))
  
    plot_info <- create_plot_info (info[[2]], info[[1]])
    info_make <- paste(paste0 ("sink (file = paste0 (multirun_directory, \"Multirun - Parameters and Info\"))"),
                       "cat (paste0 (\"Number of Timesteps: \", info[[3]][1], \"\nNumber of Populations: \", info[[3]][2], \"\nPopulation Size: \", info[[3]][3], \"\nNumber of Syllables: \", info[[3]][4], \"\nNumber of Syllable Positions Assigned to Specific Probability Levels: \", info[[3]][5], \"\nNumber of Singers Sampled from One Population for Mating: \", info[[3]][7], \"\nNumber of Singers Sampled from One Population for Tutoring: \", info[[3]][6], \"\nProbability of Inheriting Curiosity Accurately: \", info[[3]][8], \"\nProbability of Learning Syllables Accurately from Parent: \", info[[3]][10], \"\nProbability of Learning Syllables Accurately from Tutor: \", info[[3]][9], \"\nProbability of Picking up Random Extra Syllables from Parent: \", info[[3]][12], \"\nProbability of Picking up Random Extra Syllables from Tutor: \", info[[3]][11], \"\nStandard Deviation of Randomly-picked-up Sylls from Established Mean: \", info[[3]][13], \"\nNumber of Rows in Population Calls Matrix: \", info[[3]][14], \"\nNumber of Columns in Pop Calls Matrix: \", info[[3]][15], \"\nPairing Pool Rows: \", info[[3]][16], \"\nPairing Pool Columns: \", info[[3]][17], \"\nPairing Pool Slices: \", info[[3]][18], \"\nCuriosity Counter Rows: \", info[[3]][19], \"\nCuriosity Counter Columns: \", info[[3]][20], \"\nPopulation Syllable Probability Rows: \", info[[3]][21], \"\nPopulation Probability Columns: \", info[[3]][22], \"\nLength of Curiosity Breaks Vector: \", info[[3]][23], \"\nLength of Zero to One Template: \", info[[3]][24], \"\nLearning Pool Rows: \", info[[3]][25], \"\nLearning Pool Columns: \", info[[3]][26], \"\nLearning Pool Slices: \", info[[3]][27]))",
                       "sink ()", sep = "\n")
    eval (parse (text = info_make))
    #     print ("info made")
    mins_n_maxes <- min_n_max (parameters = params, number_of_runs_mnx = number_of_repeats,
                              cursitylist = cursitylist, sdstbxnlist = sdstbxnlist,
                              curhistlist = curhistlist, sylrepzlist = sylrepzlist)
    #     print (paste0 ("mins_n_maxes: ", mins_n_maxes))
    #     print (paste0 ("length of cursitylist: ", dim (cursitylist [[number_of_repeats + 1]])))
    if (recolorize == FALSE) {
      simple_plots (parameters = params, plot_info = plot_info,
                   number_of_runs = number_of_repeats, cursitylist = cursitylist,
                   sdstbxnlist = sdstbxnlist, curhistlist = curhistlist, sylrepzlist = sylrepzlist,
                   mins_n_maxes = mins_n_maxes, saving_dir = multirun_directory, lineplots = lineplots, curMeans_only = curMeans_only, absolute_y = params$absolute_yAxis)
    } else {
      if (compare_subsets == TRUE) {
        output_variable <- recolorized_simple_plots (recolorize_style = recolorize_style, # "clustering"
                                                     parameters = params, plot_info = plot_info,
                                                     number_of_runs = number_of_repeats, cursitylist = cursitylist,
                                                     sdstbxnlist = sdstbxnlist, curhistlist = curhistlist, sylrepzlist = sylrepzlist,
                                                     mins_n_maxes = mins_n_maxes, saving_dir = multirun_directory, lineplots = lineplots, curMeans_only = curMeans_only, absolute_y = params$absolute_yAxis, compare_subsets = TRUE)
  
        return (output_variable)
      } else {
        recolorized_simple_plots (recolorize_style = recolorize_style, # "clustering"
                                  parameters = params, plot_info = plot_info,
                                  number_of_runs = number_of_repeats, cursitylist = cursitylist,
                                  sdstbxnlist = sdstbxnlist, curhistlist = curhistlist, sylrepzlist = sylrepzlist,
                                  mins_n_maxes = mins_n_maxes, saving_dir = multirun_directory, lineplots = lineplots, curMeans_only = curMeans_only, absolute_y = params$absolute_yAxis)
      }
  
    }
    #     print ("simple_plots done")
  
    # if (redo != FALSE) {
    
    # if (redo == FALSE) {
      srcdir = file.path ("scripts")
      # file.names = dir (srcdir) [grep ("Source", dir (srcdir))]
      if (! (dir.exists (file.path (strsplit (multirun_folderlist [1], "/variable_store", ) [[1]][1], "copy_of_scripts")))) {
        dir.create (file.path (strsplit (multirun_folderlist [1], "/variable_store", ) [[1]][1], "copy_of_scripts"))
      }
      if (! (file.exists (file.path (strsplit (multirun_folderlist [1], "/variable_store", ) [[1]][1], "copy_of_scripts", "scriptsFiles.zip")))) {
        zipr (file.path (strsplit (multirun_folderlist [1], "/variable_store", ) [[1]][1], "copy_of_scripts", "scriptsFiles.zip"), file.path (srcdir), TRUE, 9, TRUE)
      }
      
  
      parmdir = file.path ("parameters")
      # file.names = dir (parmdir) [grep ("*.yaml", dir (parmdir))]
      if (! (dir.exists (file.path (strsplit (multirun_folderlist [1], "/variable_store", ) [[1]][1], "copy_of_params")))) {
        dir.create (file.path (strsplit (multirun_folderlist [1], "/variable_store", ) [[1]][1], "copy_of_params"))
      }
      if (! (file.exists (file.path (strsplit (multirun_folderlist [1], "/variable_store", ) [[1]][1], "copy_of_params", "paramsFiles.zip")))) {
        zipr (file.path (strsplit (multirun_folderlist [1], "/variable_store", ) [[1]][1], "copy_of_params", "paramsFiles.zip"), file.path (parmdir), TRUE, 9, TRUE)
      }
      
  
      if (! (file.exists (file.path (strsplit (multirun_folderlist [1], "/variable_store", ) [[1]][1], "copy_of_params", "paramsSource.RData")))) {
        saveRDS (params, file.path (strsplit (multirun_folderlist [1], "/variable_store", ) [[1]][1], "copy_of_params", "paramsSource.RData"))
      }
    # }
    return (print (paste0 (specificsimnumber," - Exit Status: 0")))
}




# recolorize_style = "variance"
# parameters = params
# plot_info = plot_info
# number_of_runs = number_of_repeats
# cursitylist = cursitylist
# sdstbxnlist = sdstbxnlist
# curhistlist = curhistlist
# sylrepzlist = sylrepzlist
# mins_n_maxes = mins_n_maxes
# saving_dir = multirun_directory