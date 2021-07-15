# results_pool <- c ("parentNoInv",        "childF1NoInv",       "childMalHihInv",  "childMalLowInv",     "childFemLowInv",
#   "childBothLowInv",    "childFemHihInv",     "childBothHihInv", "childSmolMalHihInv", "childSmolMalLowInv",
#   "childSmolFemHihInv", "childSmolFemLowInv", "childNoInvF2",    "childNoInvF3",       "childNoInvF4",
#   "childNoInvF5",       "childNoInvF6",       "childNoInvF7",    "childNoInvF8",       "childNoInvF9",
#   "childNoInvF10", "childLateInvMalHih", "childLateInvMalLow", "childLateInvFemHih", "childLateInvFemLow",
#   "childLateInvBothHih", "childLateInvBothLow", "childLateSmolInvMalHih", "childLateSmolInvMalLow",  "childLateSmolInvFemHih",
#    "childLateSmolInvFemLow")

figprodmultrun <- function (
  specificsimnumber = 1,
  # number_of_repeats,
  paramssource = paramssource,
  # recolorize = FALSE,
  # results_tenK_dir = FALSE,
  lineplots = FALSE,
  curMeans_only = FALSE#,
  # recolorize = "variance",
  # compare_subsets = FALSE
) {

  params = yaml.load_file (file.path ("parameters", paramssource))
  # params <- paramssource
  number_of_repeats <- as.numeric (params$number_of_reps)

  # example of text for results_tenK_dir is "childLateSmolInvFemLow"
  # print ("figprodmultrunStart")
  source (file.path ("scripts", "Source_Visualizing_Data.R"))

  # print ("params load")
  converted_data <- vector ("list", number_of_repeats)

  if (params$redodir != FALSE) {
    #
    if (dir.exists(eval(parse(text=params$redodir))) == TRUE) { # eval(parse(text=params$redodir)), when params$redodir == file.path("/", "mnt", "f", "results", "tenKfiveByFive_parentNoInv"); paste(params$redodir, collapse = "/") when it's "/", "mnt", "f", "results", "tenKfiveByFive_parentNoInv"
      simnumber_host_folder <- eval(parse(text=params$redodir)) # paste(params$redodir, collapse = "/")
    } else {
      stop("params$redodir isn't working")
    }
  } else {
    simnumber_host_folder <- "results"
  }

  individual_simnumber_folder <- list.files(file.path (simnumber_host_folder), pattern = paste0 ("*_", specificsimnumber, "*_"))
      
  variable_dir <- file.path(simnumber_host_folder, individual_simnumber_folder, "variable_store")[length(file.path(simnumber_host_folder, individual_simnumber_folder, "variable_store"))]
  if (! (dir.exists (variable_dir))) {stop("variable_dir does not exist")}
  
  multirun_dir <- file.path(simnumber_host_folder, individual_simnumber_folder, "multirun_output")[length(file.path(simnumber_host_folder, individual_simnumber_folder, "multirun_output"))]
  
  if (! (dir.exists (multirun_dir))) {dir.create (file.path (multirun_dir))}

  ### Control flow directing script to simulation files:
  ###   "redo" skips making Group_###_folderList.RData, and finds it instead
  ###   "redo" also skips concatenating and copying simulation data into the multirun_output folder.
  ###   "results_tenK_dir" has been used once, for the "redo function" - 'results_tenK_dir = "parentNoInv"' - 
  if (params$indrunredo != FALSE) {
    # individual_simnumber_folder <- list.files(file.path (simnumber_host_folder), pattern = paste0 ("*_", specificsimnumber, "*_"))
      
    # variable_dir <- file.path(simnumber_host_folder, individual_simnumber_folder, "variable_store")
    # if (! (dir.exists (variable_dir))) {stop("variable_dir does not exist")}
    
    # multirun_folderlist <- array(rep(0, length(list.files(variable_dir))), c(length(list.files(variable_dir))))
    # for (m in 1:length(list.files(variable_dir))) {
    #   multirun_folderlist[m] <- as.character(list.files(variable_dir)[m]) #; stop(print(paste0("", "", individual_simnumber_folder))) ### list.files(variable_dir, pattern = "variable-store")
    # }

    multirun_folderlist <- file.path(variable_dir, list.files(variable_dir))
  } else {
    if(file.exists(file.path ("source","temp", paste0 (specificsimnumber, "_sim_data.txt")))) {
      connection <- file (description = file.path ("source","temp", paste0 (specificsimnumber, "_sim_data.txt")), open = "rt")
      multirun_folderlist <- as.vector (read.table (connection, -1L) [[2]])
      close (connection)
      if(length(multirun_folderlist) < 1) {
        multirun_folderlist <- file.path(variable_dir, list.files(variable_dir))
      }
    } else {
      multirun_folderlist <- file.path(variable_dir, list.files(variable_dir))
    }

    # multirun_folderlist <- 

    for (runs_visual in 1 : number_of_repeats) {
      # if (results_tenK_dir != FALSE) {
      #   multirun_folderlist [runs_visual] <- paste0 (
      #     file.path (simnumber_host_folder, paste0 ("tenKfiveByFive_", results_tenK_dir)),
      #     strsplit (multirun_folderlist[runs_visual], simnumber_host_folder) [[1]][2]
      #   )
      # }
      
      if (runs_visual == 1) {
        multiRunTime <- format (Sys.time (), "%F-%H%M%S")
    # print ("runs_visual == 1")
        if (length(list.files (file.path (multirun_dir))) == 0) {
          dir.create (file.path (multirun_dir, paste0 (multiRunTime,
            "-GMT-multirun-output")))
        } else {
          
          if (! (dir.exists (file.path (multirun_dir, list.files (file.path (multirun_dir), pattern = "-GMT-multirun-output"))[length(file.path (multirun_dir, list.files (file.path (multirun_dir), pattern = "-GMT-multirun-output")))]))) {
    # print ("makin specific multirun_output folder")
            dir.create (file.path (multirun_dir, paste0 (multiRunTime,
              "-GMT-multirun-output")))
          }

        }
    # print ("Do we need to record the list of folders?")
        if (! (file.exists (file.path (simnumber_host_folder, individual_simnumber_folder,
          paste0 ("Group_", specificsimnumber, "_folderList.RData"))))) {
            # print ("yes we do.")
            saveRDS (multirun_folderlist, file.path (simnumber_host_folder, individual_simnumber_folder, 
            paste0 ("Group_", specificsimnumber, "_folderList.RData")))
        }
      }
    
    # print ("converted_data time")
      converted_data <- concatenate_data (
        specific_run = runs_visual,
        converteddata = converted_data,
        parms = params,
        data_dir = multirun_folderlist)
    # print ("process_data time")
    }

    # print ("multirun_directory")
    multirun_directory <- paste0 (
      strsplit (multirun_folderlist [length(multirun_folderlist)],
      "variable") [[1]][1], "multirun_output/",
      list.files (
      path = paste0 (
      strsplit (multirun_folderlist [length(multirun_folderlist)],
      "variable") [[1]][1], "multirun_output/"),
      pattern = "multirun-output$"))

    process_data (
      data_conglomerate = converted_data,
      specificrepeat = number_of_repeats,
      path = multirun_directory)
  }
  
  # if (params$redodir != FALSE) {
  #   #
  #   tryCatch({
        # info <- readRDS (file = file.path (variable_dir, multirun_folderlist [1], list.files(file.path(variable_dir, multirun_folderlist [1]), pattern = "metadata.RData")))
        info <- readRDS (file = file.path (multirun_folderlist [1], "metadata.RData"))
  #     }, error = function(e) {
  #       # stop("info <- readRDS (file = file.path (multirun_folderlist [1], 'metadata.RData'))")
  #       stop(paste0(
  #         multirun_folderlist [1], " -- ", 
  #         variable_dir, " -- ", 
  #         dir.exists(variable_dir), " -- ", 
  #         dir.exists(file.path(variable_dir, multirun_folderlist[1])), " -- ", 
  #         file.path(variable_dir, multirun_folderlist [1]), " -- ", 
  #         file.exists(file.path(variable_dir, multirun_folderlist [1], "metadata.RData")),
  #         readRDS (file = file.path (variable_dir, multirun_folderlist [1], "metadata.RData"))
  #       ))
  #       # stop(saveRDS(multirun_folderlist, "210531_multirun_object.RData"))
  #     }
  #   )
  # } else {
  #   #
  #     individual_simnumber_folder <- list.files(file.path (simnumber_host_folder), pattern = paste0 ("*_", specificsimnumber, "*_"))
      
  #     variable_dir <- file.path(simnumber_host_folder, individual_simnumber_folder, "variable_store")
  #     if (! (dir.exists (variable_dir))) {stop("variable_dir does not exist")}

  #   info <- readRDS (file = file.path (multirun_folderlist [1], "metadata.RData"))
  #   #stop(file.path (multirun_folderlist [1], 'metadata.RData'))
  #   #stop("readRDS (file = file.path (multirun_folderlist [1], 'metadata.RData'))")
  # }
  
  # print ("makin' lists")
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
                                "multirun_output")

  if (length (list.files (multirun_directory, pattern = "multirun-output")) != 0) {
    if (length (list.files (file.path (multirun_directory, list.files (multirun_directory, pattern = "multirun-output")))) != 0) {
      multirun_directory <- file.path (multirun_directory, list.files (multirun_directory, pattern = "multirun-output"))[
                     length(file.path (multirun_directory, list.files (multirun_directory, pattern = "multirun-output")))]
    }
  }
  # else if (length (list.files (multirun_directory, pattern = "multirun-output")) == 0) {
  #   multirun_directory <- paste0 (strsplit (multirun_directory[1], "multirun_output") [[1]][1],
  #                               "multirun_output/")
  # }

  # print ("sourcing from lists")
  for (i in 1 : number_of_repeats) {
    # print (paste0 (multirun_directory, "/", histlist [i]))
    tryCatch({curhistlist [[i]] <- readRDS (file.path (multirun_directory, histlist [i]))}, error = function(e) {stop(file.path (multirun_directory, histlist [i]))})
    # print (paste0 (multirun_directory, "/", sitylist [i]))
    tryCatch({cursitylist [[i]] <- readRDS (file.path (multirun_directory, sitylist [i]))}, error = function(e) {stop(file.path (multirun_directory, sitylist [i]))})
    # print (paste0 (multirun_directory, "/", sdstlist [i]))
    tryCatch({sdstbxnlist [[i]] <- readRDS (file.path (multirun_directory, sdstlist [i]))}, error = function(e) {stop(file.path (multirun_directory, sdstlist [i]))})
    # print (paste0 (multirun_directory, "/", repzlist [i]))
    tryCatch({sylrepzlist [[i]] <- readRDS (file.path (multirun_directory, repzlist [i]))}, error = function(e) {stop(file.path (multirun_directory, repzlist [i]))})
  }
  # print ("histlist")
  for (i in 1 : length (curhistlist [[1]])) {
    eval (parse (text = paste0 ("curhistlist [[number_of_repeats + 1]][i] <- mean (c (curhistlist [[",
                            paste0 (1 : (number_of_repeats - 1),"]][i],curhistlist [[", collapse=''),
                            number_of_repeats, "]][i]))")))
  }

  # print (paste0 ("dimensions of curhistlist [[1]] - ", dim (curhistlist [[1]])))
  dim (curhistlist [[number_of_repeats + 1]]) <- dim (curhistlist [[1]])

  # print ("sitylist")
  for (i in 1 : length (cursitylist [[1]])) {
    eval (parse (text = paste0 ("cursitylist [[number_of_repeats + 1]][i] <- mean (c (cursitylist [[",
                            paste0 (1 : (number_of_repeats - 1),"]][i],cursitylist [[", collapse=''),
                            number_of_repeats, "]][i]))")))
  }

  # print (paste0 ("dimensions of cursitylist [[1]] - ", dim (cursitylist [[1]])))
  dim (cursitylist [[number_of_repeats + 1]]) <- dim (cursitylist [[1]])

  # print ("sdstlist")

  for (i in 1 : length (sdstbxnlist [[1]])) {
    eval (parse (text = paste0 ("sdstbxnlist [[number_of_repeats + 1]][i] <- mean (c (sdstbxnlist [[",
                            paste0 (1 : (number_of_repeats - 1),"]][i],sdstbxnlist [[", collapse=''),
                            number_of_repeats, "]][i]))")))
  }

  # print (paste0 ("dimensions of sdstbxnlist [[1]] - ", dim (sdstbxnlist [[1]])))
  dim (sdstbxnlist [[number_of_repeats + 1]]) <- dim (sdstbxnlist [[1]])

  # print ("repzlist")

  for (i in 1 : length (sylrepzlist [[1]])) {
    eval (parse (text = paste0 ("sylrepzlist [[number_of_repeats + 1]][i] <- mean (c (sylrepzlist [[",
                            paste0 (1 : (number_of_repeats - 1),"]][i],sylrepzlist [[", collapse=''),
                            number_of_repeats, "]][i]))")))
  }

  # print (paste0 ("dimensions of sylrepzlist [[1]] - ", dim (sylrepzlist [[1]])))
  dim (sylrepzlist [[number_of_repeats + 1]]) <- dim (sylrepzlist [[1]])

  # print ("last_stats")
  last_stats <- paste0 ("rm (sylrepz", number_of_repeats, ", sdstbxn", number_of_repeats,
                        ", cursity", number_of_repeats, ", curhist", number_of_repeats,
                        ", last_stats, histlist, sitylist, sdstlist, repzlist",
                        ", listlister, listmaker, listnames, datanames)")

  eval (parse (text = last_stats))

  tryCatch({plot_info <- create_plot_info (info[[2]], info[[1]])
    }, error = function(e) {plot_info <- create_plot_info (
      paste(strsplit(strsplit(multirun_folderlist[1], "/")[[1]][4], "-")[[1]][1:3], collapse="-"),
      strsplit(multirun_folderlist[1], "/")[[1]][2]
  )})
  # info now has all params info in list item [[4]] - change these out for item [[3]] for info_make
  info_make <- paste(paste0 ("sink (file = file.path (multirun_directory, \"Multirun - Parameters and Info\"))"),
                      "cat (paste0 (\"Number of Timesteps: \", info[[3]][1], \"\nNumber of Populations: \", info[[3]][2], \"\nPopulation Size: \", info[[3]][3], \"\nNumber of Syllables: \", info[[3]][4], \"\nNumber of Syllable Positions Assigned to Specific Probability Levels: \", info[[3]][5], \"\nNumber of Singers Sampled from One Population for Mating: \", info[[3]][7], \"\nNumber of Singers Sampled from One Population for Tutoring: \", info[[3]][6], \"\nProbability of Inheriting Curiosity Accurately: \", info[[3]][8], \"\nProbability of Learning Syllables Accurately from Parent: \", info[[3]][10], \"\nProbability of Learning Syllables Accurately from Tutor: \", info[[3]][9], \"\nProbability of Picking up Random Extra Syllables from Parent: \", info[[3]][12], \"\nProbability of Picking up Random Extra Syllables from Tutor: \", info[[3]][11], \"\nStandard Deviation of Randomly-picked-up Sylls from Established Mean: \", info[[3]][13], \"\nNumber of Rows in Population Calls Matrix: \", info[[3]][14], \"\nNumber of Columns in Pop Calls Matrix: \", info[[3]][15], \"\nPairing Pool Rows: \", info[[3]][16], \"\nPairing Pool Columns: \", info[[3]][17], \"\nPairing Pool Slices: \", info[[3]][18], \"\nCuriosity Counter Rows: \", info[[3]][19], \"\nCuriosity Counter Columns: \", info[[3]][20], \"\nPopulation Syllable Probability Rows: \", info[[3]][21], \"\nPopulation Probability Columns: \", info[[3]][22], \"\nLength of Curiosity Breaks Vector: \", info[[3]][23], \"\nLength of Zero to One Template: \", info[[3]][24], \"\nLearning Pool Rows: \", info[[3]][25], \"\nLearning Pool Columns: \", info[[3]][26], \"\nLearning Pool Slices: \", info[[3]][27]))",
                      "sink ()", sep = "\n")
  # info_make <- paste(paste0 ("sink (file = paste0 (multirun_directory, \"Multirun - Parameters and Info\"))"),
  #                    "cat (paste0 (\"Number of Timesteps: \", info[[4]][1], \"\nNumber of Populations: \", info[[4]][2], \"\nPopulation Size: \", info[[4]][3], \"\nNumber of Syllables: \", info[[4]][4], \"\nNumber of Syllable Positions Assigned to Specific Probability Levels: \", info[[4]][5], \"\nNumber of Singers Sampled from One Population for Mating: \", info[[4]][7], \"\nNumber of Singers Sampled from One Population for Tutoring: \", info[[4]][6], \"\nProbability of Inheriting Curiosity Accurately: \", info[[4]][8], \"\nProbability of Learning Syllables Accurately from Parent: \", info[[4]][10], \"\nProbability of Learning Syllables Accurately from Tutor: \", info[[4]][9], \"\nProbability of Picking up Random Extra Syllables from Parent: \", info[[4]][12], \"\nProbability of Picking up Random Extra Syllables from Tutor: \", info[[4]][11], \"\nStandard Deviation of Randomly-picked-up Sylls from Established Mean: \", info[[4]][13], \"\nNumber of Rows in Population Calls Matrix: \", info[[4]][14], \"\nNumber of Columns in Pop Calls Matrix: \", info[[4]][15], \"\nPairing Pool Rows: \", info[[4]][16], \"\nPairing Pool Columns: \", info[[4]][17], \"\nPairing Pool Slices: \", info[[4]][18], \"\nCuriosity Counter Rows: \", info[[4]][19], \"\nCuriosity Counter Columns: \", info[[4]][20], \"\nPopulation Syllable Probability Rows: \", info[[4]][21], \"\nPopulation Probability Columns: \", info[[4]][22], \"\nLength of Curiosity Breaks Vector: \", info[[4]][23], \"\nLength of Zero to One Template: \", info[[4]][24], \"\nLearning Pool Rows: \", info[[4]][25], \"\nLearning Pool Columns: \", info[[4]][26], \"\nLearning Pool Slices: \", info[[4]][27]))",
  #                    "sink ()", sep = "\n")
  eval (parse (text = info_make))
  # print ("info made")
  mins_n_maxes <- min_n_max (parameters = params, number_of_runs_mnx = number_of_repeats,
                            cursitylist = cursitylist, sdstbxnlist = sdstbxnlist,
                            curhistlist = curhistlist, sylrepzlist = sylrepzlist)
  # print (paste0 ("mins_n_maxes: ", mins_n_maxes))
  # print (paste0 ("length of cursitylist: ", dim (cursitylist [[number_of_repeats + 1]])))
  recolorize <- params$recolorize
  compare_subsets <- params$compare_subsets
  if (recolorize == FALSE) {
    simple_plots (parameters = params, plot_info = plot_info,
                  number_of_runs = number_of_repeats, cursitylist = cursitylist,
                  sdstbxnlist = sdstbxnlist, curhistlist = curhistlist, sylrepzlist = sylrepzlist,
                  mins_n_maxes = mins_n_maxes, saving_dir = multirun_directory, lineplots = lineplots, 
                  curMeans_only = curMeans_only, absolute_y = params$absolute_yAxis)
  } else {

    subset_pool <- array (0, c (4,2,number_of_repeats))
    # subset_pool <-
    for (lengthCursityList_minusMean in 1 : number_of_repeats) {
      subset_pool[,,lengthCursityList_minusMean] <- cursitylist [[lengthCursityList_minusMean]][c (1,2,3,4),,(params$runlength/params$recordsimplifyfactor)]
    }
# print ("subpop_measures")
    subpop_measures <- matrix (nrow = 2, ncol = 2, byrow = TRUE)
    if (compare_subsets == TRUE) {subset_output_pool <- vector("list", 4)} # change size of list as number of recolorize options changes
    if (recolorize == "variance" || compare_subsets == TRUE) {
      #
      #     highest_variance = [which (max (variance_among_subpopulations))],
      #     # whichever subpopulation has the highest variance, the groups that cluster together are colored similarly
      for (pop in 1 : params$num_pop) {
        for (sex in 1 : 2) {
          subpop_measures[sex,pop] <- var (subset_pool[sex,pop,1 : params$number_of_reps])
        }
      }

      thing <- which (subpop_measures == max (subpop_measures))
      if (thing == 1) {thing <- c (1,1)} else if (thing == 2) {thing <- c (1,2)} else if (thing == 3) {thing <- c (2,1)} else if (thing == 4) {thing <- c (2,2)} else {stop ("whoops")}
      tryCatch ({subset_output <- which (subset_pool[thing[1], thing[2], 1 : params$number_of_reps] > mean(subset_pool[thing[1], thing[2], params$number_of_reps]))}, error = function(e) {stop(print(paste0(paste0(dim(subset_pool), collapse = ", "), "; ", params$number_of_reps)))})
      if (compare_subsets == TRUE) {tryCatch({subset_output_pool[[1]] <- subset_output}, error = function (e) {stop(print(subset_output))})}
    } 
    
    if (recolorize == "variance-median" || compare_subsets == TRUE) {

      # subpop_measures <- matrix (nrow = 2, ncol = 2, byrow = TRUE)
      for (pop in 1 : params$num_pop) {
        for (sex in 1 : 2) {
          subpop_measures[sex,pop] <- var (subset_pool[sex,pop,1 : params$number_of_reps])
        }
      }

      thing <- which (subpop_measures == max (subpop_measures))
      if (thing == 1) {thing <- c (1,1)} else if (thing == 2) {thing <- c (1,2)} else if (thing == 3) {thing <- c (2,1)} else if (thing == 4) {thing <- c (2,2)} else {stop ("whoops")}
      whatever <- median (subset_pool[thing[1], thing[2], 1 : params$number_of_reps])
      subset_output <- which (subset_pool[thing[1], thing[2], 1 : params$number_of_reps] > whatever)
      if (compare_subsets == TRUE) {subset_output_pool[[2]] <- subset_output}
    }
    
    if (recolorize == "range-median" || compare_subsets == TRUE) {
# print ("range-median")
      # subpop_measures <- matrix (nrow = 2, ncol = 2, byrow = TRUE)
      for (pop in 1 : params$num_pop) {
        for (sex in 1 : 2) {
          subpop_measures[sex,pop] <- max (subset_pool[sex,pop,1 : params$number_of_reps]) - min (subset_pool[sex,pop,1 : params$number_of_reps])
        }
      }

      thing <- which (subpop_measures == max (subpop_measures))
      # thing <- 1

      if (thing == 1) {thing <- c (1,1)} else if (thing == 2) {thing <- c (1,2)} else if (thing == 3) {thing <- c (2,1)} else if (thing == 4) {thing <- c (2,2)} else {stop ("whoops")}
      whatever <- median (subset_pool[thing[1], thing[2], 1 : params$number_of_reps])
      subset_output <- which (subset_pool[thing[1], thing[2], 1 : params$number_of_reps] > whatever)
      if (compare_subsets == TRUE) {subset_output_pool[[3]] <- subset_output}
      # just var, but with a subcluster far below "varX#" so we see how the very edge cases line up with other subpopulations
      # subset_output <- 1 : 50 # whatever... fix it only if it breaks
    }
    
    if (recolorize == "clustering" || compare_subsets == TRUE) {
      # #
      # #     highest_clustering_score
      # #     # two metrics - first metric is: best_clusternumber that is highest when "many" reps are clustered, "very close" to each other; the other metric is: having at least one value that is distinct (having a certain minimal distance (fraction of the total possible space/spectrum, say, 10%) ->) from the cluster
      # clustering_measures <- matrix (nrow = 4, ncol = 2, byrow = TRUE)
      # for (pop in 1 : parameters$num_pop) {
      #   for (sex in 1 : 2) {
      #     clustering_measures[sex,pop] <- max (hist (subset_pool[sex,pop,1 : parameters$number_of_reps], breaks = seq (0, 1, 0.1), plot = FALSE)$counts)
      #     second_measure <- sort (hist (subset_pool[sex,pop,1 : parameters$number_of_reps], breaks = seq (0, 1, 0.1), plot = FALSE)$counts, index.return = TRUE)$ix
      #     clustering_measures[sex + 2, pop] <-
      #   }
      # }

      # thing <- which (clustering_measures == max (clustering_measures))
      # if (thing == 1) {thing <- c (1,1)} else if (thing == 2) {thing <- c (1,2)} else if (thing == 3) {thing <- c (2,1)} else if (thing == 4) {thing <- c (2,2)} else {stop ("whoops")}
      # subset_output <- which (subset_pool[thing[1], thing[2], 1 : parameters$number_of_reps] > subset_pool[thing[1], thing[2], parameters$number_of_reps + 1])
      
      subset_output <- 1 : params$number_of_reps # whatever... fix it only if it breaks
      
      # subset_output <- c(1,2,3,4)

      if (compare_subsets == TRUE) {
        # subset_output_pool[[4]] <- vector ("numeric", length(subset_output))
        subset_output_pool[[4]] <- subset_output
      }
    } 
    
    # if (compare_subsets == TRUE) {
    #   saveRDS (subset_output_pool, file.path(saving_dir, "list_-_subset_comparison_output.RData"))
    # }
    # # if (compare_subsets == TRUE) {
    # recolorized_simple_plots (
    #   recolorize = recolorize, # "clustering"
    #   parameters = params, plot_info = plot_info, 
    #   number_of_runs = number_of_repeats, cursitylist = cursitylist,
    #   sdstbxnlist = sdstbxnlist, curhistlist = curhistlist, sylrepzlist = sylrepzlist,
    #   mins_n_maxes = mins_n_maxes, saving_dir = multirun_directory, lineplots = lineplots, 
    #   curMeans_only = curMeans_only, absolute_y = params$absolute_yAxis, compare_subsets = compare_subsets, subset_output = subset_output)

    # # if (compare_subsets == TRUE) {return (output_variable)}
    # # } else {
    # #   recolorized_simple_plots (recolorize = recolorize, # "clustering"
    # #                             parameters = params, plot_info = plot_info,
    # #                             number_of_runs = number_of_repeats, cursitylist = cursitylist,
    # #                             sdstbxnlist = sdstbxnlist, curhistlist = curhistlist, sylrepzlist = sylrepzlist,
    # #                             mins_n_maxes = mins_n_maxes, saving_dir = multirun_directory, lineplots = lineplots, 
    # #                             curMeans_only = curMeans_only, absolute_y = params$absolute_yAxis)
    # # }

    if (compare_subsets == TRUE) {
      saveRDS (subset_output_pool, file.path(multirun_directory, "list_-_subset_comparison_output.RData"))
    }
# print ("simple_plots start")

    simple_plots (parameters = params, plot_info = plot_info,
                number_of_runs = number_of_repeats, cursitylist = cursitylist,
                sdstbxnlist = sdstbxnlist, curhistlist = curhistlist, sylrepzlist = sylrepzlist,
                mins_n_maxes = mins_n_maxes, saving_dir = multirun_directory, recolorize = subset_output,
                lineplots = lineplots, curMeans_only = curMeans_only, absolute_y = params$absolute_yAxis,
                compare_subsets = compare_subsets)

  }
  # print ("simple_plots done")

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