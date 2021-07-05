par_plot <- function (
  # plot_settings (meanz, xlab = "Timestep", ylab = paste0 ("Pop ", population, plot_title_retainer[individual_figures]),cex=0.2, ylim=c (miny, maxy), xaxt="n")
    data_pP,
    xlab_pP,
    ylab_pP,
    cex_pP = 0.2,
    ylim_pP = c (0,100),
    xaxt_pP = "n",

  #

  # par_settings (cex.lab = 1.5, cex.main = 2)
    cex.lab_Pp = 2,
    cex.main_Pp = 1,
    mar_Pp = FALSE,
  # layout_settings
    layout = FALSE,
    plot_num = FALSE
  ) {
  par (
    cex.lab = cex.lab_Pp,
    cex.main = cex.main_Pp,
    if(mar_Pp) {mfrow = mar_Pp} else {no.readonly = FALSE}
  ) # par_settings

  plot (
    x = data_pP,
    xlab = xlab_pP,
    ylab = ylab_pP,
    cex = cex_pP,
    ylim = ylim_pP,
    xaxt = xaxt_pP
  ) # plot_settings

  # dev.off ()
}

# stitch together output files from simulations ("variable-store" files), 
concatenate_data <- function (specific_run,
                                converteddata = converted_data,
                                parms = params,
                                data_dir = multirun_folderlist) {
  # This function takes
  data_dir <- data_dir[specific_run]
  nts = parms$runlength/1000 # number of 1k timesteps
  # knts = nts*1000
  # cnts = nts*100
  # dnts = nts*10

  numslice <- 1000/parms$recordsimplifyfactor # number of slices to take from 1000 timesteps

  npp <- parms$num_pop
  ops <- parms$one_pop_singers
  snm <- parms$sylnum

  converted_names = c ("sylrepz", "sdstbxn", "cursity", "curhist")


  sylrepz = array (0, c (2, npp,  nts * numslice))
  sdstbxn = array (0, c ((2 * npp), snm,  nts * numslice))
  cursity = array (0, c (16, npp,  nts * numslice))
  # let's make indices 13 and 14 on dimension 1... these are the measures of variance in curiosity level in both male and female subpopulations
  curhist = array (0, c ((2 * npp), (npp * ops[1]),  nts * numslice))

  # converteddata <- vector (mode = "list", length = length (specific_run))

  converteddata[[specific_run]] <- vector (mode = "list", length (converted_names))

  converteddata[[specific_run]][[1]] <- sylrepz
  converteddata[[specific_run]][[2]] <- sdstbxn
  converteddata[[specific_run]][[3]] <- cursity
  converteddata[[specific_run]][[4]] <- curhist

  for (specificchunk in 1 : nts) {
      # specificchunk <- 1
      sc <- (1 + ((specificchunk - 1) * numslice)) # output_chunk_start
      ec <- specificchunk * numslice # output_chunk_end
      converteddata[[specific_run]][[1]][,,sc:ec] <- readRDS (file.path (data_dir, paste0 ("variable-store-", specificchunk, "-sylrep_rowcol.RData")))
      converteddata[[specific_run]][[2]][,,sc:ec] <- readRDS (file.path (data_dir, paste0 ("variable-store-", specificchunk, "-sylrep_dstbxn.RData")))
      converteddata[[specific_run]][[3]][,,sc:ec] <- readRDS (file.path (data_dir, paste0 ("variable-store-", specificchunk, "-curity_mean_t.RData")))
      converteddata[[specific_run]][[4]][,,sc:ec] <- readRDS (file.path (data_dir, paste0 ("variable-store-", specificchunk, "-curity_repert.RData")))
  }
  return (converteddata)
}

process_data <- function (data_conglomerate = converted_data, specificrepeat = run_visual, path = getwd ()) {
  objectnames <- c ("sylrepz", "sdstbxn", "cursity", "curhist")
  datanames <- c ("SylReps", "SylDist", "Cursity", "CurHist")
  modified_data <- c ()
  if (typeof (data_conglomerate[[1]]) == "list") {
    for (iteration in 1 : specificrepeat) {
      for (data_subset in 1 : 4) {
        modified_data <- data_conglomerate[[iteration]][[data_subset]]
        # saveRDS (object = modified_data, file = file.path (path, paste0 (datanames[data_subset], specificrepeat, ".RData")))
        if (! (file.exists (file.path (path, paste0 (datanames[data_subset], iteration, ".RData"))))) {
          saveRDS (modified_data, file.path (path, paste0 (datanames[data_subset], iteration, ".RData")))
        }
      }
    }
  } else {
    for (data_subset in 1 : 4) {
        modified_data <- data_conglomerate[[data_subset]]
        # saveRDS (object = modified_data, file = file.path (path, paste0 (datanames[data_subset], specificrepeat, ".RData")))
        if (! (file.exists (file.path (path, paste0 (datanames[data_subset], iteration, ".RData"))))) {
          saveRDS (modified_data, file.path (path, paste0 (datanames[data_subset], iteration, ".RData")))
        }
    }
  }
}

concat_and_move_simData <- function (
  # args that concatenate_data and process_data need
) {
  # concatenate_data ()
  # process_data ()
  # if (!(obsessively_save_all_files)) {delete files from variable_store}
}

paste_split_data_runs <- function (data_subset, num_runs = 10, also_mean = TRUE) {
  if (also_mean == TRUE) {
    num_runs <- num_runs + 1
    pasted_runs <- array (0, c (dim (data_subset), num_runs))
    thing <- paste0 ("pasted_runs[", 1 : (num_runs - 1), "] <- ", quote(data_subset), 1 : (num_runs - 1))
    eval (parse (text = thing))
    pasted_runs[num_runs] <- rowMeans (pasted_runs[, , , 1 : (num_runs - 1)], dims = 3)
  }
  return (pasted_runs)
}

create_plot_info <- function (datez = "180803", run_name = "initial_test_1") {
  datez = datez
  run_name = run_name
  #sylnum_palette <- colorRampPalette(c ("darkblue","royalblue","skyblue","turquoise1","springgreen","gold","orangered","firebrick4"))
  sylnum_palette <- colorRampPalette(c ("#641e16", "#943126", "#cb4335", "#f5b7b1", "#aed6f1", "#3498db"))
  #sylsub_palette <- colorRampPalette(c ("darkgreen","lawngreen","grey90","orchid1","orchid4"))
  sylsub_palette <- colorRampPalette(c ("#5b2c6f00", "#abebc6"))
  best_colorbrewpal <- colorRampPalette(c ("#fef0d9", "#fdcc8a", "#fc8d59", "#d7301f")) # 4-class orrd
  diverge_colbruplt <- colorRampPalette(c ("#f1a340", "#F7F7F7", "#998ec3"))

  source (file.path ("scripts", "Source_colorseqmultpalette.R"))
  colorseqmultpalette <- make_colorpalettes ()

  ### SEQUENTIAL, SINGLE-HUE, COLORBLIND FRIENDLY, PRINT FRIENDLY, PHOTOCOPY FRIENDLY

  # source (file.path ("scripts", "Source_colorseqmultpalette.R"))
  colorseqsingpalette <- make_singpalette ()


  sexes_lc <- c ("male", "female")
  sexes_uc <- c ("Male", "Female")
  popgroup <- c ("Pop_1_male","Pop_1_female","Pop_2_male","Pop_2_female") #, "Pop_3_male", "Pop_3_female", "Pop_4_male", "Pop_4_female"
  plot_info <- list (sylnum_palette = sylnum_palette, sylsub_palette = sylsub_palette,
                    datez = datez, run_name = run_name, sexes_lc = sexes_lc, sexes_uc = sexes_uc,
                    popgroup = popgroup, colorseqmultpalette = colorseqmultpalette,
                    colorseqsingpalette = colorseqsingpalette, best_colorbrewpal = best_colorbrewpal)

  return (plot_info)
}

min_n_max <- function (parameters, number_of_runs_mnx = number_of_runs, cursitylist = cursitylist,
                         sdstbxnlist = sdstbxnlist, curhistlist = curhistlist,
                         sylrepzlist = sylrepzlist) {
  nrowsminsmaxes <- 16
  mins_n_maxes <- array (0,c (nrowsminsmaxes,parameters$num_pop,2)) # rows = different things being measured, columns = populations (1&2) for 1 : 9 and populations & sex ((1) pop1male (2) pop1female (3) pop2male (4) pop2female); depth = min (1) and max (2)
  mn_mx_container <- c ("min", "max") # 3rd-dim-dependent ---
  objectnames <- c ("curhist","cursity","sdstbxn","sylrepz") # row-dependent --- k -> (objectnames[objectsubset[k]])
  figuresubset <- c (3,10,4,5,6,7,8,9,11,12, 1, 2, 1, 2,13,14) # row-dependent --- k
  #                 1,2, 3,4,5,6,7,8, 9,10,11,12,13,14,15,16
  objectsubset <- c (2, 2,2,2,2,2,2,2, 2, 2, 4, 4, 2, 2, 2, 2) # row-dependent --- k

  for (j in 1 : parameters$num_pop) {
    for (k in 1 : nrowsminsmaxes ) {
      for (L in 1 : 2) {
        # This is for min (1) and max (2)
        container <- vector ("numeric", number_of_runs_mnx)

        for (i in 1 : number_of_runs_mnx) {
          #container[i] <- min (eval (parse (text = paste0 (objectnames, "list [[", i, "]][", subset, ", ", j, ",]"))))
          eval (parse (text = paste0 ("container[i] <- ", mn_mx_container[L], "(", paste0 (objectnames[objectsubset[k]], "list [[", i, "]][", figuresubset[k], ", ", j, ",])"))))
        }

        eval (parse (text = paste0 ("mins_n_maxes[k,j,L] <- ", mn_mx_container[L], "(container)")))
      }
    }
    #thing_to_evalparse <- paste0 ("mins_n_maxes[," ,j, ",] <- min (container)")
  }

  return (mins_n_maxes)
}

curiosity_figures <- function (parameters, number_of_runs, population, cursitylist, plot_info, mins_n_maxes, saving_dir = multirun_directory, recolorize = FALSE, lineplots = FALSE) {
  figure_retainer <- c (3,10,4,5,6,7,8,9,11,13,14)
  just_curiosity <- c (1, 2,3,4,5,6,7,8,9,15,16)
  ### 1,2 - mate/tutor select chances; 3,4 - curlevel parents; 5,6 - curlevel offspring; 7,8 - curlevel replaced individuals; 9 - curinh attempts; 10,11 -

  filename_retainer <- c ("_mate_selections_pop", "_tutor_selections_pop", "_AC_parent_m_pop",
                          "_AC_parent_f_pop", "_AC_offspring_m_pop", "_AC_offspring_f_pop", "_AC_replaced_m_pop",
                          "_AC_replaced_f_pop", "_cur_inh_attempts", "_AC_var_m_pop", "_AC_var_f_pop")
  plot_title_retainer <- c (" Mate Selection Chances", " Tutor Selection Chances", " Father AC", " Mother AC",
                            " Son AC", " Daughter AC", " Dead Man AC", " Dead Woman AC", " Cur Inh Attempts", " AC Variance Mal", " AC Variance Fem")
  num_timesteps = parameters$runlength
  for (individual_figures in 1 : length (figure_retainer)) {

    meanz <- cursitylist [[number_of_runs + 1]][(figure_retainer[individual_figures]),population,]
    direct_object <- paste0 ("points (cursitylist [[", 1 : number_of_runs, "]][", (figure_retainer[individual_figures]), ",population,],col=\"grey\", cex=0.2)")
    if (recolorize != FALSE) {
      if (length (which (! (1 : number_of_runs %in% recolorize))) > 0) {
        if (lineplots != FALSE) {
          # lines
          direct_object <- paste0 ("lines (cursitylist [[", which (1 : number_of_runs %in% recolorize), "]][", (figure_retainer[individual_figures]), ",population,],col=\"red\", cex=0.5)")
          indirect_object <- paste0 ("lines (cursitylist [[", which (! (1 : number_of_runs %in% recolorize)), "]][", (figure_retainer[individual_figures]), ",population,],col=\"blue\", cex=0.5)")
        } else {
          # points
          direct_object <- paste0 ("points (cursitylist [[", which (1 : number_of_runs %in% recolorize), "]][", (figure_retainer[individual_figures]), ",population,],col=\"red\", cex=0.5)")
          indirect_object <- paste0 ("points (cursitylist [[", which (! (1 : number_of_runs %in% recolorize)), "]][", (figure_retainer[individual_figures]), ",population,],col=\"blue\", cex=0.5)")
        }
      } else {
        direct_object <- paste0 ("points (cursitylist [[", 1 : number_of_runs, "]][", (figure_retainer[individual_figures]), ",population,],col=\"grey\", cex=0.2)")
        # recolorize <- FALSE
      }
    } else {
      direct_object <- paste0 ("points (cursitylist [[", 1 : number_of_runs, "]][", (figure_retainer[individual_figures]), ",population,],col=\"grey\", cex=0.2)")
    }
    file_name <- paste0 (plot_info$datez, "_", plot_info$run_name, filename_retainer[individual_figures], population, ".png")
    miny <- mins_n_maxes[just_curiosity[individual_figures],population,1]
    maxy <- mins_n_maxes[just_curiosity[individual_figures],population,2]

    # par (cex.lab = 5, cex.main = 2)
    png (filename = paste0 (saving_dir, "/", file_name), width = 554, height = 467, units = "px", pointsize = 12, bg = "white")
    par_plot (data_pP = meanz, xlab_pP = "Timestep", ylab_pP = paste0 ("Pop ", population, plot_title_retainer[individual_figures]),cex_pP=0.2, ylim_pP=c (miny, maxy), xaxt_pP="n")

    axis (side = 1,
          at = c (seq.int (0,length (cursitylist [[number_of_runs + 1]][figure_retainer[individual_figures],population,]),
                                  ((length (cursitylist [[number_of_runs + 1]][figure_retainer[individual_figures],population,]))/10))),
          labels = c (seq.int (0,num_timesteps,(num_timesteps/10))))

    eval (parse (text = direct_object))
    if (recolorize != FALSE) {
      if (length (which (! (1 : number_of_runs %in% recolorize))) > 0) {
        tryCatch({eval (parse (text = indirect_object))}, error = function(e) {stop(recolorize)})
      }
    }
    lines (cursitylist [[number_of_runs + 1]][figure_retainer[individual_figures],population,],col="black", cex=0.2)
    dev.off ()
    # dev.off ()

  }
}

recolorized_simple_plots <- function (
  recolorize_lineplots = "variance", # "clustering"
  parameters = params, plot_info = plot_info,
  number_of_runs = number_of_repeats, cursitylist = cursitylist,
  sdstbxnlist = sdstbxnlist, curhistlist = curhistlist, sylrepzlist = sylrepzlist,
  mins_n_maxes = mins_n_maxes, saving_dir = multirun_directory, lineplots = FALSE,
  curMeans_only = FALSE, absolute_y = TRUE, compare_subsets = FALSE, subset_output = subset_output) {

  # subset_pool <- cursitylist [[]]
  # if (typeof (cursitylist) == "list") {
  #   thing <- length (cursitylist)
  #   for (i in 1 : thing) {
  #     if (! (typeof (cursitylist [[i]]) == "list")) {
  #       direct_object <- dim (cursitylist [[i]])
  #     } else {
  #       direct_object <- length (cursitylist [[i]])
  #       recordings <- list ()
  #       for (j in 1 : whatever) {
  #         if (! (typeof (cursitylist [[i]][[j]]) == "list")) {
  #           bemgcasrba <- dim (cursitylist [[i]][[j]])
  #         }
  #       }
  #     }
  #   }
  #   output_from_this_monstrosity <- c (thing,direct_object,)
  # }

  # subset_pool <- array (0, c (4,2,length (cursitylist)))
  # # subset_pool <-
  # for (lengthCursityList in 1 : length (cursitylist)) {
  #   subset_pool[,,lengthCursityList] <- cursitylist [[lengthCursityList]][c (1,2,3,4),,(parameters$runlength/parameters$recordsimplifyfactor)]
  # }

  # subpop_measures <- matrix (nrow = 2, ncol = 2, byrow = TRUE)
  # if (compare_subsets == TRUE) {subset_output_pool <- vector("list", 4)} # change size of list as number of recolorize options changes
  # if (recolorize_lineplots == "variance" || compare_subsets == TRUE) {
  #   #
  #   #     highest_variance = [which (max (variance_among_subpopulations))],
  #   #     # whichever subpopulation has the highest variance, the groups that cluster together are colored similarly
  #   for (pop in 1 : parameters$num_pop) {
  #     for (sex in 1 : 2) {
  #       subpop_measures[sex,pop] <- var (subset_pool[sex,pop,1 : parameters$number_of_reps])
  #     }
  #   }

  #   thing <- which (subpop_measures == max (subpop_measures))
  #   if (thing == 1) {thing <- c (1,1)} else if (thing == 2) {thing <- c (1,2)} else if (thing == 3) {thing <- c (2,1)} else if (thing == 4) {thing <- c (2,2)} else {stop ("whoops")}
  #   subset_output <- which (subset_pool[thing[1], thing[2], 1 : parameters$number_of_reps] > subset_pool[thing[1], thing[2], parameters$number_of_reps + 1])
  #   if (compare_subsets == TRUE) {subset_output_pool[1] <- subset_output}
  # } 
  
  # if (recolorize_lineplots == "variance-median" || compare_subsets == TRUE) {

  #   # subpop_measures <- matrix (nrow = 2, ncol = 2, byrow = TRUE)
  #   for (pop in 1 : parameters$num_pop) {
  #     for (sex in 1 : 2) {
  #       subpop_measures[sex,pop] <- var (subset_pool[sex,pop,1 : parameters$number_of_reps])
  #     }
  #   }

  #   thing <- which (subpop_measures == max (subpop_measures))
  #   if (thing == 1) {thing <- c (1,1)} else if (thing == 2) {thing <- c (1,2)} else if (thing == 3) {thing <- c (2,1)} else if (thing == 4) {thing <- c (2,2)} else {stop ("whoops")}
  #   whatever <- median (subset_pool[thing[1], thing[2], 1 : parameters$number_of_reps])
  #   subset_output <- which (subset_pool[thing[1], thing[2], 1 : parameters$number_of_reps] > whatever)
  #   if (compare_subsets == TRUE) {subset_output_pool[2] <- subset_output}
  # }
  
  # if (recolorize_lineplots == "range-median" || compare_subsets == TRUE) {

  #   # subpop_measures <- matrix (nrow = 2, ncol = 2, byrow = TRUE)
  #   for (pop in 1 : parameters$num_pop) {
  #     for (sex in 1 : 2) {
  #       subpop_measures[sex,pop] <- max (subset_pool[sex,pop,1 : parameters$number_of_reps]) - min (subset_pool[sex,pop,1 : parameters$number_of_reps])
  #     }
  #   }

  #   thing <- which (subpop_measures == max (subpop_measures))
  #   # thing <- 1

  #   if (thing == 1) {thing <- c (1,1)} else if (thing == 2) {thing <- c (1,2)} else if (thing == 3) {thing <- c (2,1)} else if (thing == 4) {thing <- c (2,2)} else {stop ("whoops")}
  #   whatever <- median (subset_pool[thing[1], thing[2], 1 : parameters$number_of_reps])
  #   subset_output <- which (subset_pool[thing[1], thing[2], 1 : parameters$number_of_reps] > whatever)
  #   if (compare_subsets == TRUE) {subset_output_pool[3] <- subset_output}
  #   # just var, but with a subcluster far below "varX#" so we see how the very edge cases line up with other subpopulations
  #   # subset_output <- 1 : 50 # whatever... fix it only if it breaks
  # }
  
  # if (recolorize_lineplots == "clustering" || compare_subsets == TRUE) {
  #   # #
  #   # #     highest_clustering_score
  #   # #     # two metrics - first metric is: best_clusternumber that is highest when "many" reps are clustered, "very close" to each other; the other metric is: having at least one value that is distinct (having a certain minimal distance (fraction of the total possible space/spectrum, say, 10%) ->) from the cluster
  #   # clustering_measures <- matrix (nrow = 4, ncol = 2, byrow = TRUE)
  #   # for (pop in 1 : parameters$num_pop) {
  #   #   for (sex in 1 : 2) {
  #   #     clustering_measures[sex,pop] <- max (hist (subset_pool[sex,pop,1 : parameters$number_of_reps], breaks = seq (0, 1, 0.1), plot = FALSE)$counts)
  #   #     second_measure <- sort (hist (subset_pool[sex,pop,1 : parameters$number_of_reps], breaks = seq (0, 1, 0.1), plot = FALSE)$counts, index.return = TRUE)$ix
  #   #     clustering_measures[sex + 2, pop] <-
  #   #   }
  #   # }

  #   # thing <- which (clustering_measures == max (clustering_measures))
  #   # if (thing == 1) {thing <- c (1,1)} else if (thing == 2) {thing <- c (1,2)} else if (thing == 3) {thing <- c (2,1)} else if (thing == 4) {thing <- c (2,2)} else {stop ("whoops")}
  #   # subset_output <- which (subset_pool[thing[1], thing[2], 1 : parameters$number_of_reps] > subset_pool[thing[1], thing[2], parameters$number_of_reps + 1])
  #   subset_output <- 1 : 50 # whatever... fix it only if it breaks
  #   if (compare_subsets == TRUE) {subset_output_pool[4] <- subset_output}
  # } 
  
  if (compare_subsets == TRUE) {
    saveRDS (subset_output_pool, file.path(saving_dir, "list_-_subset_comparison_output.RData"))
  }

  simple_plots (parameters = parameters, plot_info = plot_info,
               number_of_runs = number_of_runs, cursitylist = cursitylist,
               sdstbxnlist = sdstbxnlist, curhistlist = curhistlist, sylrepzlist = sylrepzlist,
               mins_n_maxes = mins_n_maxes, saving_dir = saving_dir, recolorize = subset_output,
               lineplots = lineplots, curMeans_only = curMeans_only, absolute_y = absolute_y,
               compare_subsets = compare_subsets)
}

simple_plots <- function (parameters, plot_info = plot_info,
                         number_of_runs = number_of_runs, cursitylist = cursitylist,
                         sdstbxnlist = sdstbxnlist, curhistlist = curhistlist,
                         sylrepzlist = sylrepzlist, mins_n_maxes = mins_n_maxes,
                         saving_dir = multirun_directory, recolorize = TRUE,
                         lineplots = TRUE, curMeans_only = FALSE,
                         absolute_y = TRUE, compare_subsets = FALSE) {
  # params = yaml.load_file (file.path ("parameters", parameters))
  num_timesteps = parameters$runlength
  
  if (recolorize != FALSE) {
    saving_dir <- file.path (saving_dir, "recolorizedLineplots")
    if (! (dir.exists (saving_dir))) {dir.create (saving_dir)}
  }
  for (population in 1 : parameters$num_pop) {
    if (curMeans_only == FALSE) {
      # print("curiosity_figures start")
      curiosity_figures (parameters = parameters, number_of_runs = number_of_runs,
                         population = population, cursitylist = cursitylist, plot_info = plot_info,
                         mins_n_maxes = mins_n_maxes, saving_dir = saving_dir, recolorize = recolorize, lineplots = lineplots)
      # print("curiosity_figures done")
    }

    for (sex in 1 : 2) {
      if (curMeans_only == FALSE) {
        meanz <- sylrepzlist [[number_of_runs + 1]][sex,population,]
        if (recolorize != FALSE) {
          if (length (which (! (1 : number_of_runs %in% recolorize))) > 0) {
            if (lineplots != FALSE) {
              # print("lineplots")
              direct_object <- paste0 ("lines (sylrepzlist [[", which (1 : number_of_runs %in% recolorize), "]][sex,population,],col=\"red\", cex=0.5)")
              indirect_object <- paste0 ("lines (sylrepzlist [[", which (! (1 : number_of_runs %in% recolorize)), "]][sex,population,],col=\"blue\", cex=0.5)")
            } else {
              # print("points on plots")
              direct_object <- paste0 ("points (sylrepzlist [[", which (1 : number_of_runs %in% recolorize), "]][sex,population,],col=\"red\", cex=0.5)")
              indirect_object <- paste0 ("points (sylrepzlist [[", which (! (1 : number_of_runs %in% recolorize)), "]][sex,population,],col=\"blue\", cex=0.5)")
            }
          } else {
            direct_object <- paste0 ("points (sylrepzlist [[", 1 : number_of_runs, "]][sex,population,],col=\"grey\", cex=0.2)")
            # recolorize <- FALSE
          }

        } else {
          # print("point on plot")
          direct_object <- paste0 ("points (sylrepzlist [[", 1 : number_of_runs, "]][sex,population,],col=\"grey\", cex=0.2)")
        }

        file_name <- paste0 (plot_info$datez, "_", plot_info$run_name, "_mean_repertoire_size_-_pop_", population, "_", plot_info$sexes_lc[sex], "s.png")
        if (absolute_y == TRUE) {
          # print("min and max if absolute is true")
          miny <- min (sapply(sylrepzlist, min))
          maxy <- max (sapply(sylrepzlist, max))
        } else {
          miny <- mins_n_maxes [(sex + 10), population, 1]
          maxy <- mins_n_maxes [(sex + 10), population, 2]
        }

        # miny <- 0
        # maxy <- 1

        png (filename = paste0 (saving_dir, "/", file_name), width = 554, height = 467, units = "px", pointsize = 12, bg = "white")

        par_plot (data_pP = meanz, xlab_pP = "Timestep", ylab_pP = paste0 ("Pop ", population, " ", plot_info$sexes_uc [sex], "s - Mean Repertoire Size"), cex_pP = 0.2, ylim_pP = c (miny, maxy), xaxt_pP = "n")
        axis (side = 1, at = c (seq.int (0,length (cursitylist [[number_of_runs + 1]][sex,population,]),
                                      ((length (cursitylist [[number_of_runs + 1]][sex,population,]))/10))),
              labels = c (seq.int (0,num_timesteps,(num_timesteps/10))))
        eval (parse (text = direct_object))
        if (recolorize != FALSE) {
          if (length (which (! (1 : number_of_runs %in% recolorize))) > 0) {
            tryCatch({eval (parse (text = indirect_object))}, error = function(e) {stop(recolorize)})
          }
        }
        lines (sylrepzlist [[number_of_runs + 1]][sex,population,],col="black", cex=0.2)
        dev.off ()
      }


      meanz <- cursitylist [[number_of_runs + 1]][sex,population,]
      if (recolorize != FALSE) {
        if (length (which (! (1 : number_of_runs %in% recolorize))) > 0) {
          if (lineplots != FALSE) {
            # print("lines on plot")
            direct_object <- paste0 ("lines (cursitylist [[", which (1 : number_of_runs %in% recolorize), "]][sex,population,],col=\"red\", cex=0.5)")
            indirect_object <- paste0 ("lines (cursitylist [[", which (! (1 : number_of_runs %in% recolorize)), "]][sex,population,],col=\"blue\", cex=0.5)")
          } else {
            # print("pointss on plots")
            direct_object <- paste0 ("points (cursitylist [[", which (1 : number_of_runs %in% recolorize), "]][sex,population,],col=\"red\", cex=0.5)")
            indirect_object <- paste0 ("points (cursitylist [[", which (! (1 : number_of_runs %in% recolorize)), "]][sex,population,],col=\"blue\", cex=0.5)")
          }
        } else {
          direct_object <- paste0 ("points (cursitylist [[", 1 : number_of_runs, "]][sex,population,],col=\"grey\", cex=0.2)")
        }

      } else {
        # print("pointon plot")
        direct_object <- paste0 ("points (cursitylist [[", 1 : number_of_runs, "]][sex,population,],col=\"grey\", cex=0.2)")
      }
      # direct_object <- paste0 ("points (cursitylist [[", 1 : number_of_runs, "]][sex,population,],col=\"grey\", cex=0.2)")
      file_name <- paste0 (plot_info$datez, "_", plot_info$run_name, "_mean_curiosity_-_pop_", population, "_", plot_info$sexes_lc[sex], "s.png")
      if (absolute_y == TRUE) {
        miny <- min (sapply(cursitylist, min))
        # maxy <- max (sapply(cursitylist, max))
        maxy <- 1
      # stop(paste0(mins_n_maxes, ", "))
      } else {
        miny <- mins_n_maxes [(sex + 10), population, 1]
        maxy <- mins_n_maxes [(sex + 10), population, 2]
      }

      # miny <- 0
      # maxy <- 1

      png (filename = paste0 (saving_dir, "/", file_name), width = 554, height = 467, units = "px", pointsize = 12, bg = "white")
      par_plot (data_pP = meanz, xlab_pP = "Timestep", ylab_pP = paste0 ("Pop ", population, " ", plot_info$sexes_uc [sex], "s - Mean Curiosity"),cex_pP = 0.2, ylim_pP = c (miny, maxy), xaxt_pP = "n")
      axis (side = 1, at = c (seq.int (0,length (cursitylist [[number_of_runs + 1]][sex,population,]),
                                    ((length (cursitylist [[number_of_runs + 1]][sex,population,]))/10))),
            labels = c (seq.int (0,num_timesteps,(num_timesteps/10))))
      eval (parse (text = direct_object))
      if (recolorize != FALSE) {
        if (length (which (! (1 : number_of_runs %in% recolorize))) > 0) {
          tryCatch({eval (parse (text = indirect_object))}, error = function(e) {stop(recolorize)})
        }
      }
      lines (cursitylist [[number_of_runs + 1]][sex,population,],col="black", cex=0.2)
      dev.off ()

      # if (recolorize == FALSE) {
      if (curMeans_only == FALSE) {

        if (compare_subsets == TRUE) {

# print("compare_subsets stuff")
          for (i in 1 : length (sdstbxnlist [[1]])) {
            sdstbxnlist [[i]] <- readRDS (paste0 (saving_dir, sdstlist [i]))
            
            eval (parse (text = paste0 ("sdstbxnlist [[number_of_runs + 2]][i] <- mean (c (sdstbxnlist [[",
                                  paste0 (1 : (number_of_runs - 1),"]][i],sdstbxnlist [[", collapse=''),
                                  number_of_runs, "]][i]))")))
          } # made object size flexible

          #     print (paste0 ("dimensions of sdstbxnlist [[1]] - ", dim (sdstbxnlist [[1]])))
          dim (sdstbxnlist [[number_of_runs + 1]]) <- dim (sdstbxnlist [[1]])

          meanz <- sdstbxnlist [[number_of_runs + 1]][(sex + ((population - 1) * 2)), ,]
          file_name <- paste0 (plot_info$datez, "_", plot_info$run_name, "_sylnum_pop_", population, "_", plot_info$sexes_lc[sex], "s.png")
          png (filename = paste0 (saving_dir, "/", file_name), width = 554, height = 467, units = "px", pointsize = 12, bg = "white")
          image(t (meanz), col = plot_info$sylnum_palette(100), xlab = "Timestep", ylab = paste0 ("Pop ", population, " ", plot_info$sexes_uc[sex], "s Sylnum"), axes=FALSE)
          axis (1, tck=-0.05, at=c (seq.int (0,1,0.1)),labels=c (seq.int (0,1,0.1)*num_timesteps), col.axis="black", las=2)
          axis (2, tck=-0.05, at=c (seq.int (0,1,(1/12))),labels=c (seq.int (0,1,(1/12))*156), col.axis="black", las=2)
          minor.tick (nx=4, ny=4.8, tick.ratio=1, x.args = list (), y.args = list ())
          dev.off ()
        }

        if (length(recolorize) > 0) {
          # print("DOsdstbxnlist")
          DOsdstbxnlist <- vector (mode = "list", length = length(recolorize) + 1)
          # print("IOsdstbxnlist")
          if (length (which (! (1 : number_of_runs %in% recolorize))) > 0) {
            IOsdstbxnlist <- vector (mode = "list", length = number_of_runs - length(recolorize) + 1)
          }
          # save.image("210519_workspace.RData")
          # print("fill DO chunks")
          for (DO_chunks in 1: length(recolorize)) {
            DOsdstbxnlist [[DO_chunks]] <- array(sdstbxnlist[[recolorize[DO_chunks]]], dim(sdstbxnlist[[1]]))
          }
          
          DOsdstbxnlist[[length(DOsdstbxnlist)]] <- array(rep(0, length(sdstbxnlist[[1]])), dim(sdstbxnlist[[1]]))
          
          for (j in 1 : length (sdstbxnlist [[1]])) {
            eval (parse (text = paste0 ("DOsdstbxnlist [[length(DOsdstbxnlist)]][j] <- mean (c (DOsdstbxnlist [[",
                                  paste0 (1 : (length(DOsdstbxnlist) - 2),"]][j],DOsdstbxnlist [[", collapse=''),
                                  length(DOsdstbxnlist) - 1, "]][j]))")))
          }

          # direct_object - which (1 : number_of_runs %in% recolorize)
          DO_meanz <- DOsdstbxnlist [[length(DOsdstbxnlist)]][(sex + ((population - 1) * 2)), ,]
          file_name <- paste0 (plot_info$datez, "_", plot_info$run_name, "_sylnum_DO_pop_", population, "_", plot_info$sexes_lc[sex], "s.png")
          png (filename = paste0 (saving_dir, "/", file_name), width = 554, height = 467, units = "px", pointsize = 12, bg = "white")
          image(t (DO_meanz), col = plot_info$sylnum_palette(100), xlab = "Timestep", ylab = paste0 ("Pop ", population, " ", plot_info$sexes_uc[sex], "s DO Sylnum"), axes=FALSE)
          axis (1, tck=-0.05, at=c (seq.int (0,1,0.1)),labels=c (seq.int (0,1,0.1)*num_timesteps), col.axis="black", las=2)
          axis (2, tck=-0.05, at=c (seq.int (0,1,(1/12))),labels=c (seq.int (0,1,(1/12))*156), col.axis="black", las=2)
          minor.tick (nx=4, ny=4.8, tick.ratio=1, x.args = list (), y.args = list ())
          dev.off ()

# print("fill IO chunks")
          if (length (which (! (1 : number_of_runs %in% recolorize))) > 0) {
            for (IO_chunks in 1: (number_of_runs - length(recolorize))) {
              IOsdstbxnlist [[IO_chunks]] <- array(sdstbxnlist[[which (! (1:number_of_runs %in% recolorize))[IO_chunks]]], dim(sdstbxnlist[[1]]))
            }
            
            IOsdstbxnlist[[length(IOsdstbxnlist)]] <- array(rep(0, length(sdstbxnlist[[1]])), dim(sdstbxnlist[[1]]))
            
            for (k in 1 : length (sdstbxnlist [[1]])) {
              eval (parse (text = paste0 ("IOsdstbxnlist [[length(IOsdstbxnlist)]][k] <- mean (c (IOsdstbxnlist [[",
                                    paste0 (1 : (length(IOsdstbxnlist) - 2),"]][k],IOsdstbxnlist [[", collapse=''),
                                    length(IOsdstbxnlist) - 1, "]][k]))")))
            }

            # indirect_object - which (! (1 : number_of_runs %in% recolorize))
            IO_meanz <- IOsdstbxnlist [[length(IOsdstbxnlist)]][(sex + ((population - 1) * 2)), ,]
            file_name <- paste0 (plot_info$datez, "_", plot_info$run_name, "_sylnum_IO_pop_", population, "_", plot_info$sexes_lc[sex], "s.png")
            png (filename = paste0 (saving_dir, "/", file_name), width = 554, height = 467, units = "px", pointsize = 12, bg = "white")
            image(t (IO_meanz), col = plot_info$sylnum_palette(100), xlab = "Timestep", ylab = paste0 ("Pop ", population, " ", plot_info$sexes_uc[sex], "s IO Sylnum"), axes=FALSE)
            axis (1, tck=-0.05, at=c (seq.int (0,1,0.1)),labels=c (seq.int (0,1,0.1)*num_timesteps), col.axis="black", las=2)
            axis (2, tck=-0.05, at=c (seq.int (0,1,(1/12))),labels=c (seq.int (0,1,(1/12))*156), col.axis="black", las=2)
            minor.tick (nx=4, ny=4.8, tick.ratio=1, x.args = list (), y.args = list ())
            dev.off ()
          }

        
          #     print (paste0 ("dimensions of sdstbxnlist [[1]] - ", dim (sdstbxnlist [[1]])))
# print("is this where the problem is? 555")
          #______________________________________________________
          #__________________________________________________
          # code for: DO and IO both populations on one plot 
          #---------------------------------------------------
          #-----------------------------------------------------
          #
          #
          #
          #
          #
          if (length (which (! (1 : number_of_runs %in% recolorize))) > 0) {
            doANDio <- 2
            DO_and_IO <- list(DOsdstbxnlist[[length(DOsdstbxnlist)]], IOsdstbxnlist[[length(IOsdstbxnlist)]])
            doANDio_names <- c("DO", "IO")
          } else {
            doANDio <- 1
            DO_and_IO <- list(DOsdstbxnlist[[length(DOsdstbxnlist)]])
            doANDio_names <- c("DO")
          }

          for (doORio in 1:doANDio) {
            red_to_yellow_to_blue <- colorRampPalette(c("#DE2D26", "#C7D34D", "#3182BD"))
            range_1 <- colorRampPalette(c(colorRampPalette(c("#F7F7F7", red_to_yellow_to_blue(7)[1]))(10)[3], colorRampPalette(c("#F7F7F7", red_to_yellow_to_blue(7)[1]))(10)[10]))
            range_2 <- colorRampPalette(c(colorRampPalette(c("#F7F7F7", red_to_yellow_to_blue(7)[2]))(10)[3], colorRampPalette(c("#F7F7F7", red_to_yellow_to_blue(7)[2]))(10)[10]))
            range_3 <- colorRampPalette(c(colorRampPalette(c("#F7F7F7", red_to_yellow_to_blue(7)[3]))(10)[3], colorRampPalette(c("#F7F7F7", red_to_yellow_to_blue(7)[3]))(10)[10]))
            range_4 <- colorRampPalette(c(colorRampPalette(c("#F7F7F7", red_to_yellow_to_blue(7)[4]))(10)[3], colorRampPalette(c("#F7F7F7", red_to_yellow_to_blue(7)[4]))(10)[10]))
            range_5 <- colorRampPalette(c(colorRampPalette(c("#F7F7F7", red_to_yellow_to_blue(7)[5]))(10)[3], colorRampPalette(c("#F7F7F7", red_to_yellow_to_blue(7)[5]))(10)[10]))
            range_6 <- colorRampPalette(c(colorRampPalette(c("#F7F7F7", red_to_yellow_to_blue(7)[6]))(10)[3], colorRampPalette(c("#F7F7F7", red_to_yellow_to_blue(7)[6]))(10)[10]))
            range_7 <- colorRampPalette(c(colorRampPalette(c("#F7F7F7", red_to_yellow_to_blue(7)[7]))(10)[3], colorRampPalette(c("#F7F7F7", red_to_yellow_to_blue(7)[7]))(10)[10]))

            full_colors <- colorRampPalette(c(
              colorRampPalette(c(range_1(50)[50 - (7:1)^2]))(7),
              colorRampPalette(c(range_2(50)[50 - (7:1)^2]))(7),
              colorRampPalette(c(range_3(50)[50 - (7:1)^2]))(7),
              colorRampPalette(c(range_4(50)[50 - (7:1)^2]))(7),
              colorRampPalette(c(range_5(50)[50 - (7:1)^2]))(7),
              colorRampPalette(c(range_6(50)[50 - (7:1)^2]))(7),
              colorRampPalette(c(range_7(50)[50 - (7:1)^2]))(7)
            ))


            one_more_bxnlist <- array(0, c(156,length(sdstbxnlist[[1]][1,1,])))

            for (i in 1:length(sdstbxnlist[[1]][1,1,])) {
              # sort((sdstbxnlist[[7]][3,,i]+0.5)/(sdstbxnlist[[7]][1,,i]+0.5), index.return = TRUE)
              # the ratio
              # print("is this where the problem is? 594")
              
              # sort((sdstbxnlist[[7]][3,,i]+0.5)+(sdstbxnlist[[7]][1,,i]+0.5), index.return = TRUE)
              # the total
              color_chunk <- (DO_and_IO[[doORio]][3,,i])/(DO_and_IO[[doORio]][1,,i])
              nanana_s <- c()
              pop_1_only <- c()
              pop_2_only <- c()
              for (k in 1:length(color_chunk)) {
                tryCatch({if(is.na(color_chunk[k]) == TRUE) {
                  nanana_s <- append(nanana_s, k)
                } else if((1/color_chunk[k]) < 0.00001) {
                  pop_2_only <- append(pop_2_only, k)
                } else if(color_chunk[k] < 0.00001) {
                  pop_1_only <- append(pop_1_only, k)
                }}, error = function(e) {stop(length(sdstbxnlist[[1]][1,1,]))})
              }
              
              # color_categories <- ((1:13)*(1/13))/((13:1)*(1/13))
              col_cat <- ((1:4)*(1/4))/((4:1)*(1/4)) + 0.00001
              # print("is this where the problem is? 616")
              ind_rcc <- which(!((1:156) %in% c(nanana_s, pop_1_only, pop_2_only)))
              # color_chunk_values_ircc <- color_chunk[ind_rcc]
              ccv <- color_chunk[ind_rcc]
              #color_chunk <- color_chunk[!((1:156) %in% c(nanana_s, pop_1_only, pop_2_only))]
              #num_colors <- 13
              #mixed_sylls <- num_colors - 2
              #if (length(pop_1_only) > 1) {mixed_sylls <- mixed_sylls - 1}; if (length(pop_2_only) > 1) {mixed_sylls <- mixed_sylls - 1}
              #if((length(color_chunk) %% mixed_sylls) == 0) {} else {}
              #if (length(pop_1_only) > 1) {
              color_1 <- pop_1_only
              color_2 <- ind_rcc[which(ccv < col_cat[1])]
              color_3 <- ind_rcc[which(ccv > col_cat[1])[which(which(ccv > col_cat[1]) %in% which(ccv < col_cat[2]))]]
              color_4 <- ind_rcc[which(ccv > col_cat[2])[which(which(ccv > col_cat[2]) %in% which(ccv < col_cat[3]))]]
              color_5 <- ind_rcc[which(ccv > col_cat[3])[which(which(ccv > col_cat[3]) %in% which(ccv < col_cat[4]))]]
              color_6 <- ind_rcc[which(ccv > col_cat[4])]
              color_7 <- pop_2_only
# print("is this where the problem is? 634")
              category_1 <- ((DO_and_IO[[doORio]][1,,i]+DO_and_IO[[doORio]][3,,i])/max(DO_and_IO[[doORio]][1,,i]+DO_and_IO[[doORio]][3,,i]))[color_1]*(1/7)
              category_2 <- ((DO_and_IO[[doORio]][1,,i]+DO_and_IO[[doORio]][3,,i])/max(DO_and_IO[[doORio]][1,,i]+DO_and_IO[[doORio]][3,,i]))[color_2]*(1/7)+(1/7)
              category_3 <- ((DO_and_IO[[doORio]][1,,i]+DO_and_IO[[doORio]][3,,i])/max(DO_and_IO[[doORio]][1,,i]+DO_and_IO[[doORio]][3,,i]))[color_3]*(1/7)+(2/7)
              category_4 <- ((DO_and_IO[[doORio]][1,,i]+DO_and_IO[[doORio]][3,,i])/max(DO_and_IO[[doORio]][1,,i]+DO_and_IO[[doORio]][3,,i]))[color_4]*(1/7)+(3/7)
              category_5 <- ((DO_and_IO[[doORio]][1,,i]+DO_and_IO[[doORio]][3,,i])/max(DO_and_IO[[doORio]][1,,i]+DO_and_IO[[doORio]][3,,i]))[color_5]*(1/7)+(4/7)
              category_6 <- ((DO_and_IO[[doORio]][1,,i]+DO_and_IO[[doORio]][3,,i])/max(DO_and_IO[[doORio]][1,,i]+DO_and_IO[[doORio]][3,,i]))[color_6]*(1/7)+(5/7)
              category_7 <- ((DO_and_IO[[doORio]][1,,i]+DO_and_IO[[doORio]][3,,i])/max(DO_and_IO[[doORio]][1,,i]+DO_and_IO[[doORio]][3,,i]))[color_7]*(1/7)+(6/7)
              # print("is this where the problem is? 643")
              temp_object <- array(c(c(
                category_1, category_2, category_3, category_4, category_5, category_6, category_7, rep(0, length(nanana_s))
              ), c(
                color_1, color_2, color_3, color_4, color_5, color_6, color_7, nanana_s
              )), c(156,2))
              # print("is this where the problem is? 650")
              tryCatch({one_more_bxnlist[,i] <- temp_object[sort(temp_object[,2], index.return = TRUE)[[2]],][,1]}, error = function(e) {paste0(dim(one_more_bxnlist), ", ", i)})
            }




            # sort((sdstbxnlist[[7]][2,,1]+0.1)+(sdstbxnlist[[7]][1,,1]+0.1), index.return = TRUE)[[1]]/max((sdstbxnlist[[7]][2,,1]+0.1)+(sdstbxnlist[[7]][1,,1]+0.1))

# print("is this where the problem is? 654")

            meanz <- one_more_bxnlist
            file_name <- paste0 (plot_info$datez, "_", plot_info$run_name, doANDio_names[doORio], "_sylnum_both_pops.png")
            png (filename = paste0 (saving_dir, "/", file_name), width = 554, height = 467, units = "px", pointsize = 12, bg = "white")
            image(t (meanz), col = full_colors(49), xlab = "Timestep", ylab = paste0 ("Both Pop Sylnums"), axes=FALSE)
            axis (1, tck=-0.05, at=c (seq.int (0,1,0.1)),labels=c (seq.int (0,1,0.1)*num_timesteps), col.axis="black", las=2)
            axis (2, tck=-0.05, at=c (seq.int (0,1,(1/12))),labels=c (seq.int (0,1,(1/12))*156), col.axis="black", las=2)
            minor.tick (nx=4, ny=4.8, tick.ratio=1, x.args = list (), y.args = list ())
            dev.off ()
          }
          #
          #
          #
          #
          #
          #
          #
          #
          #
          #
          #
          #
          #
          #
          #
          #
          #
          #
          #
        }

        meanz <- sdstbxnlist [[number_of_runs + 1]][(sex + ((population - 1) * 2)), ,]
        file_name <- paste0 (plot_info$datez, "_", plot_info$run_name, "_sylnum_pop_", population, "_", plot_info$sexes_lc[sex], "s.png")
        png (filename = paste0 (saving_dir, "/", file_name), width = 554, height = 467, units = "px", pointsize = 12, bg = "white")
        image(t (meanz), col = plot_info$sylnum_palette(100), xlab = "Timestep", ylab = paste0 ("Pop ", population, " ", plot_info$sexes_uc[sex], "s Sylnum"), axes=FALSE)
        axis (1, tck=-0.05, at=c (seq.int (0,1,0.1)),labels=c (seq.int (0,1,0.1)*num_timesteps), col.axis="black", las=2)
        axis (2, tck=-0.05, at=c (seq.int (0,1,(1/12))),labels=c (seq.int (0,1,(1/12))*156), col.axis="black", las=2)
        minor.tick (nx=4, ny=4.8, tick.ratio=1, x.args = list (), y.args = list ())
        dev.off ()

        red_to_yellow_to_blue <- colorRampPalette(c("#DE2D26", "#C7D34D", "#3182BD"))
        range_1 <- colorRampPalette(c(colorRampPalette(c("#F7F7F7", red_to_yellow_to_blue(7)[1]))(10)[2], colorRampPalette(c("#F7F7F7", red_to_yellow_to_blue(7)[1]))(10)[10]))
        range_2 <- colorRampPalette(c(colorRampPalette(c("#F7F7F7", red_to_yellow_to_blue(7)[2]))(10)[2], colorRampPalette(c("#F7F7F7", red_to_yellow_to_blue(7)[2]))(10)[10]))
        range_3 <- colorRampPalette(c(colorRampPalette(c("#F7F7F7", red_to_yellow_to_blue(7)[3]))(10)[2], colorRampPalette(c("#F7F7F7", red_to_yellow_to_blue(7)[3]))(10)[10]))
        range_4 <- colorRampPalette(c(colorRampPalette(c("#F7F7F7", red_to_yellow_to_blue(7)[4]))(10)[2], colorRampPalette(c("#F7F7F7", red_to_yellow_to_blue(7)[4]))(10)[10]))
        range_5 <- colorRampPalette(c(colorRampPalette(c("#F7F7F7", red_to_yellow_to_blue(7)[5]))(10)[2], colorRampPalette(c("#F7F7F7", red_to_yellow_to_blue(7)[5]))(10)[10]))
        range_6 <- colorRampPalette(c(colorRampPalette(c("#F7F7F7", red_to_yellow_to_blue(7)[6]))(10)[2], colorRampPalette(c("#F7F7F7", red_to_yellow_to_blue(7)[6]))(10)[10]))
        range_7 <- colorRampPalette(c(colorRampPalette(c("#F7F7F7", red_to_yellow_to_blue(7)[7]))(10)[2], colorRampPalette(c("#F7F7F7", red_to_yellow_to_blue(7)[7]))(10)[10]))

        full_colors <- colorRampPalette(c(
          colorRampPalette(c(range_1(50)[50 - (7:1)^2]))(7),
          colorRampPalette(c(range_2(50)[50 - (7:1)^2]))(7),
          colorRampPalette(c(range_3(50)[50 - (7:1)^2]))(7),
          colorRampPalette(c(range_4(50)[50 - (7:1)^2]))(7),
          colorRampPalette(c(range_5(50)[50 - (7:1)^2]))(7),
          colorRampPalette(c(range_6(50)[50 - (7:1)^2]))(7),
          colorRampPalette(c(range_7(50)[50 - (7:1)^2]))(7)
        ))

        one_more_bxnlist <- array(0, c(156,length(sdstbxnlist[[1]][1,1,])))
# print("is this where the problem is? 715")
        for (i in 1:length(sdstbxnlist[[1]][1,1,])) {
            
          color_chunk <- (sdstbxnlist[[number_of_runs + 1]][3,,i])/(sdstbxnlist[[number_of_runs + 1]][1,,i])
          nanana_s <- c()
          pop_1_only <- c()
          pop_2_only <- c()
          for (k in 1:length(color_chunk)) {
            tryCatch({if(is.na(color_chunk[k]) == TRUE) {
              nanana_s <- append(nanana_s, k)
            } else if((1/color_chunk[k]) < 0.00001) {
              pop_2_only <- append(pop_2_only, k)
            } else if(color_chunk[k] < 0.00001) {
              pop_1_only <- append(pop_1_only, k)
            }}, error = function(e) {stop(length(sdstbxnlist[[1]][1,1,]))})
          }
          
            

          col_cat <- ((1:4)*(1/4))/((4:1)*(1/4)) + 0.00001
          # col_cat_b <- ((1:10)*(1/10))/((10:1)*(1/10)) + 0.00001
          
          ind_rcc <- which(!((1:156) %in% c(nanana_s, pop_1_only, pop_2_only)))
          # color_chunk_values_ircc <- color_chunk[ind_rcc]
          ccv <- color_chunk[ind_rcc]

          color_1 <- pop_1_only
          color_2 <- ind_rcc[which(ccv < col_cat[1])]
          color_3 <- ind_rcc[which(ccv > col_cat[1])[which(which(ccv > col_cat[1]) %in% which(ccv < col_cat[2]))]]
          color_4 <- ind_rcc[which(ccv > col_cat[2])[which(which(ccv > col_cat[2]) %in% which(ccv < col_cat[3]))]]
          color_5 <- ind_rcc[which(ccv > col_cat[3])[which(which(ccv > col_cat[3]) %in% which(ccv < col_cat[4]))]]
          color_6 <- ind_rcc[which(ccv > col_cat[4])]
          color_7 <- pop_2_only
          
          #(sdstbxnlist[[7]][1,,i]+sdstbxnlist[[7]][3,,i])/max(sdstbxnlist[[7]][1,,i]+sdstbxnlist[[7]][3,,i])
          
          category_1 <- ((sdstbxnlist[[number_of_runs + 1]][1,,i]+sdstbxnlist[[number_of_runs + 1]][3,,i])/max(sdstbxnlist[[number_of_runs + 1]][1,,i]+sdstbxnlist[[number_of_runs + 1]][3,,i]))[color_1]*(1/7)
          category_2 <- ((sdstbxnlist[[number_of_runs + 1]][1,,i]+sdstbxnlist[[number_of_runs + 1]][3,,i])/max(sdstbxnlist[[number_of_runs + 1]][1,,i]+sdstbxnlist[[number_of_runs + 1]][3,,i]))[color_2]*(1/7)+(1/7)
          category_3 <- ((sdstbxnlist[[number_of_runs + 1]][1,,i]+sdstbxnlist[[number_of_runs + 1]][3,,i])/max(sdstbxnlist[[number_of_runs + 1]][1,,i]+sdstbxnlist[[number_of_runs + 1]][3,,i]))[color_3]*(1/7)+(2/7)
          category_4 <- ((sdstbxnlist[[number_of_runs + 1]][1,,i]+sdstbxnlist[[number_of_runs + 1]][3,,i])/max(sdstbxnlist[[number_of_runs + 1]][1,,i]+sdstbxnlist[[number_of_runs + 1]][3,,i]))[color_4]*(1/7)+(3/7)
          category_5 <- ((sdstbxnlist[[number_of_runs + 1]][1,,i]+sdstbxnlist[[number_of_runs + 1]][3,,i])/max(sdstbxnlist[[number_of_runs + 1]][1,,i]+sdstbxnlist[[number_of_runs + 1]][3,,i]))[color_5]*(1/7)+(4/7)
          category_6 <- ((sdstbxnlist[[number_of_runs + 1]][1,,i]+sdstbxnlist[[number_of_runs + 1]][3,,i])/max(sdstbxnlist[[number_of_runs + 1]][1,,i]+sdstbxnlist[[number_of_runs + 1]][3,,i]))[color_6]*(1/7)+(5/7)
          category_7 <- ((sdstbxnlist[[number_of_runs + 1]][1,,i]+sdstbxnlist[[number_of_runs + 1]][3,,i])/max(sdstbxnlist[[number_of_runs + 1]][1,,i]+sdstbxnlist[[number_of_runs + 1]][3,,i]))[color_7]*(1/7)+(6/7)
          
          temp_object <- array(c(c(
            category_1, category_2, category_3, category_4, category_5, category_6, category_7, rep(0, length(nanana_s))
          ), c(
            color_1, color_2, color_3, color_4, color_5, color_6, color_7, nanana_s
          )), c(156,2))

          one_more_bxnlist[,i] <- temp_object[sort(temp_object[,2], index.return = TRUE)[[2]],][,1]
        }




        # sort((sdstbxnlist[[7]][2,,1]+0.1)+(sdstbxnlist[[7]][1,,1]+0.1), index.return = TRUE)[[1]]/max((sdstbxnlist[[7]][2,,1]+0.1)+(sdstbxnlist[[7]][1,,1]+0.1))
# print("is this where the problem is? 770")


        meanz <- one_more_bxnlist
        file_name <- paste0 (plot_info$datez, "_", plot_info$run_name, "_sylnum_both_pops.png")
        png (filename = paste0 (saving_dir, "/", file_name), width = 554, height = 467, units = "px", pointsize = 12, bg = "white")
        image(t (meanz), col = full_colors(49), xlab = "Timestep", ylab = paste0 ("Both Pop Sylnums"), axes=FALSE)
        axis (1, tck=-0.05, at=c (seq.int (0,1,0.1)),labels=c (seq.int (0,1,0.1)*num_timesteps), col.axis="black", las=2)
        axis (2, tck=-0.05, at=c (seq.int (0,1,(1/12))),labels=c (seq.int (0,1,(1/12))*156), col.axis="black", las=2)
        minor.tick (nx=4, ny=4.8, tick.ratio=1, x.args = list (), y.args = list ())
        dev.off ()

        meanz <- curhistlist [[number_of_runs + 1]][(sex + ((population - 1) * 2)), ,]
        file_name <- paste0 (plot_info$datez, "_", plot_info$run_name, "_curiosity_bins_pop_", population, "_", plot_info$sexes_lc[sex], "s.png")
        png (filename = paste0 (saving_dir, "/", file_name), width = 554, height = 467, units = "px", pointsize = 12, bg = "white")
        image(t (meanz), col = plot_info$sylsub_palette(100), xlab = "Timestep", ylab = paste0 ("Pop ", population, " ", plot_info$sexes_uc[sex], "s Curiosity Bin"), axes=FALSE)
        axis (1, tck=-0.05, at=c (seq.int (0,1,0.1)),labels=c (seq.int (0,1,0.1)*num_timesteps), col.axis="black", las=0)
        axis (2, tck=-0.05, at=c (seq.int (0,1,0.1)),labels=c (seq.int (0,1,0.1)*20), col.axis="black", las=2)
        minor.tick (nx=4, ny=4, tick.ratio=1, x.args = list (), y.args = list ())
        dev.off ()

        meanz <- curhistlist [[number_of_runs + 1]][(sex + ((population - 1) * 2)), ,]
        file_name <- paste0 (plot_info$datez, "_", plot_info$run_name, "_curiosity_bins_pop_", population, "_", plot_info$sexes_lc[sex], "s.png")
        png (filename = paste0 (saving_dir, "/", file_name), width = 554, height = 467, units = "px", pointsize = 12, bg = "white")
        image(t (meanz), col = plot_info$sylsub_palette(100), xlab = "Timestep", ylab = paste0 ("Pop ", population, " ", plot_info$sexes_uc[sex], "s Curiosity Bin"), axes=FALSE)
        axis (1, tck=-0.05, at=c (seq.int (0,1,0.1)),labels=c (seq.int (0,1,0.1)*num_timesteps), col.axis="black", las=0)
        axis (2, tck=-0.05, at=c (seq.int (0,1,0.1)),labels=c (seq.int (0,1,0.1)*20), col.axis="black", las=2)
        minor.tick (nx=4, ny=4, tick.ratio=1, x.args = list (), y.args = list ())
        dev.off ()
# print("is this where the problem is? 799")
        sink (file = paste0 (saving_dir, plot_info$datez, plot_info$run_name, "_Summary_Statistics"), append = TRUE)
        print (paste0 ("pop ", population, " ", plot_info$sexes_uc[sex], " rep size - avg over last 1% of timesteps"))
        print (mean (sylrepzlist [[number_of_runs + 1]][sex, population,
          ((num_timesteps / parameters$recordsimplifyfactor-1):(num_timesteps / parameters$recordsimplifyfactor))]))
        print (paste0 ("pop ", population, " ", plot_info$sexes_uc[sex], " rep size - avg over last 5% of timesteps"))
        print (mean (sylrepzlist [[number_of_runs + 1]][sex, population,
          (num_timesteps / parameters$recordsimplifyfactor-5):num_timesteps / parameters$recordsimplifyfactor]))
        print (paste0 ("pop ", population, " ", plot_info$sexes_uc[sex], " curiosity - avg over last 1% of timesteps"))
        print (mean (cursitylist [[number_of_runs + 1]][sex, population,
          (num_timesteps / parameters$recordsimplifyfactor-1):num_timesteps / parameters$recordsimplifyfactor]))
        print (paste0 ("pop ", population, " ", plot_info$sexes_uc[sex], " curiosity - avg over last 5% of timesteps"))
        print (mean (cursitylist [[number_of_runs + 1]][sex, population,
          (num_timesteps / parameters$recordsimplifyfactor-5):num_timesteps / parameters$recordsimplifyfactor]))
        sink ()
      }
      # }
    }
  }
}


# params$runlength/parameters$recordsimplifyfactor
