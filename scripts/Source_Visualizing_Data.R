concatenate_data <- function(specific_run,
                                converteddata = converted_data,
                                parms = params,
                                data_dir = multirun_folderlist) {
  # This function takes
  data_dir <- data_dir[specific_run]
  nts = as.numeric(strsplit(parms$runlength, "k")[[1]][1]) # number of 1k timesteps
  # knts = nts*1000
  # cnts = nts*100
  # dnts = nts*10

  numslice <- 1000/parms$recordsimplifyfactor # number of slices to take from 1000 timesteps

  npp <- parms$num_pop
  ops <- parms$one_pop_singers
  snm <- parms$sylnum

  converted_names = c("sylrepz", "sdstbxn", "cursity", "curhist")


  sylrepz = array (0, c (2, npp,  nts * numslice))
  sdstbxn = array (0, c ((2 * npp), snm,  nts * numslice))
  cursity = array (0, c (14, npp,  nts * numslice))
  # let's make indices 13 and 14 on dimension 1... these are the measures of variance in curiosity level in both male and female subpopulations
  curhist = array (0, c ((2 * npp), (npp * ops[1]),  nts * numslice))

  # converteddata <- vector (mode = "list", length = length(specific_run))

  converteddata[[specific_run]] <- vector (mode = "list", length (converted_names))

  converteddata[[specific_run]][[1]] <- sylrepz
  converteddata[[specific_run]][[2]] <- sdstbxn
  converteddata[[specific_run]][[3]] <- cursity
  converteddata[[specific_run]][[4]] <- curhist

  for(specificchunk in 1 : nts) {
      # specificchunk <- 1
      sc <- (1 + ((specificchunk - 1) * numslice)) # output_chunk_start
      ec <- specificchunk * numslice # output_chunk_end
      converteddata[[specific_run]][[1]][,,sc:ec] <- readRDS(file.path(data_dir, paste0("variable-store-", specificchunk, "-sylrep_rowcol.RData")))
      converteddata[[specific_run]][[2]][,,sc:ec] <- readRDS(file.path(data_dir, paste0("variable-store-", specificchunk, "-sylrep_dstbxn.RData")))
      converteddata[[specific_run]][[3]][,,sc:ec] <- readRDS(file.path(data_dir, paste0("variable-store-", specificchunk, "-curity_mean_t.RData")))
      converteddata[[specific_run]][[4]][,,sc:ec] <- readRDS(file.path(data_dir, paste0("variable-store-", specificchunk, "-curity_repert.RData")))
  }
  return(converteddata)
}

process_data <- function (data_conglomerate = converted_data, specificrepeat = run_visual, path = getwd()) {
  objectnames <- c("sylrepz", "sdstbxn", "cursity", "curhist")
  datanames <- c("SylReps", "SylDist", "Cursity", "CurHist")
  modified_data <- c()
  if (typeof (data_conglomerate[[1]]) == "list") {
    for (iteration in 1:specificrepeat) {
      for (data_subset in 1:4) {
        modified_data <- data_conglomerate[[iteration]][[data_subset]]
        # saveRDS(object = modified_data, file = file.path(path, paste0(datanames[data_subset], specificrepeat, ".RData")))
        if (! (file.exists (file.path(path, paste0(datanames[data_subset], iteration, ".RData"))))) {
          saveRDS (modified_data, file.path(path, paste0(datanames[data_subset], iteration, ".RData")))
        }
      }
    }
  } else {
    for (data_subset in 1:4) {
        modified_data <- data_conglomerate[[data_subset]]
        # saveRDS(object = modified_data, file = file.path(path, paste0(datanames[data_subset], specificrepeat, ".RData")))
        if (! (file.exists (file.path(path, paste0(datanames[data_subset], iteration, ".RData"))))) {
          saveRDS (modified_data, file.path(path, paste0(datanames[data_subset], iteration, ".RData")))
        }
    }
  }
}

paste_split_data_runs <- function(data_subset, num_runs = 10, also_mean = TRUE) {
  if(also_mean == TRUE) {
    num_runs <- num_runs + 1
    pasted_runs <- array(0, c(dim(data_subset), num_runs))
    thing <- paste0("pasted_runs[", 1 : (num_runs - 1), "] <- ", quote(data_subset), 1 : (num_runs - 1))
    eval(parse(text=thing))
    pasted_runs[num_runs] <- rowMeans(pasted_runs[, , , 1 : (num_runs - 1)], dims = 3)
  }
  return(pasted_runs)
}

create_plot_info <- function(datez = "180803", run_name = "initial_test_1") {
  datez = datez
  run_name = run_name
  #sylnum_palette <- colorRampPalette(c("darkblue","royalblue","skyblue","turquoise1","springgreen","gold","orangered","firebrick4"))
  sylnum_palette <- colorRampPalette(c("#641e16", "#943126", "#cb4335", "#f5b7b1", "#aed6f1", "#3498db"))
  #sylsub_palette <- colorRampPalette(c("darkgreen","lawngreen","grey90","orchid1","orchid4"))
  sylsub_palette <- colorRampPalette(c("#5b2c6f00", "#abebc6"))
  best_colorbrewpal <- colorRampPalette(c("#fef0d9", "#fdcc8a", "#fc8d59", "#d7301f")) # 4-class orrd
  diverge_colbruplt <- colorRampPalette(c("#f1a340", "#f7f7f7", "#998ec3"))

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

min_n_max <- function(parameters, number_of_runs = number_of_runs, cursitylist = cursitylist,
                         sdstbxnlist = sdstbxnlist, curhistlist = curhistlist,
                         sylrepzlist = sylrepzlist) {
  nrowsminsmaxes <- 16
  mins_n_maxes <- array(0,c(nrowsminsmaxes,parameters$num_pop,2)) # rows = different things being measured, columns = populations (1&2) for 1:9 and populations & sex ((1) pop1male (2) pop1female (3) pop2male (4) pop2female); depth = min (1) and max (2)
  mn_mx_container <- c("min", "max") # 3rd-dim-dependent ---
  objectnames <- c("curhist","cursity","sdstbxn","sylrepz") # row-dependent --- k -> (objectnames[objectsubset[k]])
  figuresubset <- c(3,10,4,5,6,7,8,9,11,12, 1, 2, 1, 2,13,14) # row-dependent --- k
  #                 1,2, 3,4,5,6,7,8, 9,10,11,12,13,14,15,16
  objectsubset <- c(2, 2,2,2,2,2,2,2, 2, 2, 4, 4, 2, 2, 2, 2) # row-dependent --- k

  for(j in 1:parameters$num_pop) {
    for(k in 1:nrowsminsmaxes ) {
      for(L in 1:2) {
        # This is for min (1) and max (2)
        container <- vector("numeric", number_of_runs)

        for(i in 1:number_of_runs) {
          #container[i] <- min(eval(parse(text=paste0(objectnames, "list[[", i, "]][", subset, ", ", j, ",]"))))
          eval(parse(text=paste0("container[i] <- ", mn_mx_container[L], "(", paste0(objectnames[objectsubset[k]], "list[[", i, "]][", figuresubset[k], ", ", j, ",])"))))
        }

        eval(parse(text=paste0("mins_n_maxes[k,j,L] <- ", mn_mx_container[L], "(container)")))
      }
    }
    #thing_to_evalparse <- paste0("mins_n_maxes[," ,j, ",] <- min(container)")
  }

  return(mins_n_maxes)
}

curiosity_figures <- function(parameters, number_of_runs, population, cursitylist, plot_info, mins_n_maxes, saving_dir = multirun_directory, recolorize = FALSE, lineplots = FALSE) {
  figure_retainer <- c(3,10,4,5,6,7,8,9,11,13,14)
  just_curiosity <- c (1, 2,3,4,5,6,7,8,9,15,16)
  ### 1,2 - mate/tutor select chances; 3,4 - curlevel parents; 5,6 - curlevel offspring; 7,8 - curlevel replaced individuals; 9 - curinh attempts; 10,11 -

  filename_retainer <- c("_mate_selections_pop", "_tutor_selections_pop", "_AC_parent_m_pop",
                          "_AC_parent_f_pop", "_AC_offspring_m_pop", "_AC_offspring_f_pop", "_AC_replaced_m_pop",
                          "_AC_replaced_f_pop", "_cur_inh_attempts", "_AC_var_m_pop", "_AC_var_f_pop")
  plot_title_retainer <- c(" Mate Selection Chances", " Tutor Selection Chances", " Father AC", " Mother AC",
                            " Son AC", " Daughter AC", " Dead Man AC", " Dead Woman AC", " Cur Inh Attempts", " AC Variance Mal", " AC Variance Fem")
  num_timesteps = as.numeric(strsplit(parameters$runlength, "k")[[1]][1])*1000
  for(individual_figures in 1:length(figure_retainer)) {

    meanz <- cursitylist[[number_of_runs + 1]][(figure_retainer[individual_figures]),population,]
    stuff <- paste0("points(cursitylist[[", 1:number_of_runs, "]][", (figure_retainer[individual_figures]), ",population,],col=\"grey\", cex=0.2)")
    if (recolorize != FALSE) {
      if (lineplots != FALSE) {
        stuff <- paste0("lines(cursitylist[[", which(1:number_of_runs %in% recolorize), "]][", (figure_retainer[individual_figures]), ",population,],col=\"red\", cex=0.5)")
        stuff2 <- paste0("lines(cursitylist[[", which(!(1:number_of_runs %in% recolorize)), "]][", (figure_retainer[individual_figures]), ",population,],col=\"blue\", cex=0.5)")
      } else {
        stuff <- paste0("points(cursitylist[[", which(1:number_of_runs %in% recolorize), "]][", (figure_retainer[individual_figures]), ",population,],col=\"red\", cex=0.5)")
        stuff2 <- paste0("points(cursitylist[[", which(!(1:number_of_runs %in% recolorize)), "]][", (figure_retainer[individual_figures]), ",population,],col=\"blue\", cex=0.5)")
      }


      } else {
        stuff <- paste0("points(cursitylist[[", 1:number_of_runs, "]][", (figure_retainer[individual_figures]), ",population,],col=\"grey\", cex=0.2)")
      }
    file_name <- paste0(plot_info$datez, "_", plot_info$run_name, filename_retainer[individual_figures], population, ".png")
    miny <- mins_n_maxes[just_curiosity[individual_figures],population,1]
    maxy <- mins_n_maxes[just_curiosity[individual_figures],population,2]

    png(filename = paste0(saving_dir, "/", file_name), width = 554, height = 467, units = "px", pointsize = 12, bg = "white")
    plot(meanz, xlab = "Timestep", ylab = paste0("Pop ", population, plot_title_retainer[individual_figures]),cex=0.2, ylim=c(miny, maxy), xaxt="n")
    axis(side = 1,
          at = c(seq.int(0,length(cursitylist[[number_of_runs + 1]][figure_retainer[individual_figures],population,]),
                                  ((length(cursitylist[[number_of_runs + 1]][figure_retainer[individual_figures],population,]))/10))),
          labels = c(seq.int(0,num_timesteps,(num_timesteps/10))))

    eval(parse(text=stuff))
    if (recolorize != FALSE) {
      eval(parse(text=stuff2))
    }
    lines(cursitylist[[number_of_runs + 1]][figure_retainer[individual_figures],population,],col="black", cex=0.2)
    dev.off()

  }
}

recolorized_simple_plots <- function (
  recolorize_style = "variance", # "clustering"
  parameters = params, plot_info = plot_info,
  number_of_runs = number_of_repeats, cursitylist = cursitylist,
  sdstbxnlist = sdstbxnlist, curhistlist = curhistlist, sylrepzlist = sylrepzlist,
  mins_n_maxes = mins_n_maxes, saving_dir = multirun_directory, lineplots = FALSE, curMeans_only = FALSE
) {

  # subset_pool <- cursitylist[[]]
  # if (typeof(cursitylist) == "list") {
  #   thing <- length(cursitylist)
  #   for (i in 1:thing) {
  #     if (! (typeof(cursitylist[[i]]) == "list")) {
  #       stuff <- dim(cursitylist[[i]])
  #     } else {
  #       stuff <- length(cursitylist[[i]])
  #       recordings <- list()
  #       for (j in 1:whatever) {
  #         if (! (typeof (cursitylist[[i]][[j]]) == "list")) {
  #           bemgcasrba <- dim(cursitylist[[i]][[j]])
  #         }
  #       }
  #     }
  #   }
  #   output_from_this_monstrosity <- c(thing,stuff,)
  # }

  subset_pool <- array (0, c(4,2,length(cursitylist)))
  # subset_pool <-
  for (stuff in 1:length(cursitylist)) {
    subset_pool[,,stuff] <- cursitylist[[stuff]][c(1,2,3,4),,(as.numeric(strsplit(parameters$runlength, "k")[[1]][1]) * (1000/parameters$recordsimplifyfactor))]
  }

  subpop_measures <- matrix(nrow = 2, ncol = 2, byrow = TRUE)

  if (recolorize_style == "variance") {
    #
    #     highest_variance = [which(max(variance_among_subpopulations))],
    #     # whichever subpopulation has the highest variance, the groups that cluster together are colored similarly
    for (pop in 1:parameters$num_pop) {
      for (sex in 1:2) {
        subpop_measures[sex,pop] <- var(subset_pool[sex,pop,1:parameters$number_of_reps])
      }
    }

    thing <- which (subpop_measures == max(subpop_measures))
    if (thing == 1) {thing <- c(1,1)} else if (thing == 2) {thing <- c(1,2)} else if (thing == 3) {thing <- c(2,1)} else if (thing == 4) {thing <- c(2,2)} else {stop("whoops")}
    subset_output <- which(subset_pool[thing[1], thing[2], 1:parameters$number_of_reps] > subset_pool[thing[1], thing[2], parameters$number_of_reps + 1])

  } else if (recolorize_style == "variance-median") {

    # subpop_measures <- matrix(nrow = 2, ncol = 2, byrow = TRUE)
    for (pop in 1:parameters$num_pop) {
      for (sex in 1:2) {
        subpop_measures[sex,pop] <- var(subset_pool[sex,pop,1:parameters$number_of_reps])
      }
    }

    thing <- which (subpop_measures == max(subpop_measures))
    if (thing == 1) {thing <- c(1,1)} else if (thing == 2) {thing <- c(1,2)} else if (thing == 3) {thing <- c(2,1)} else if (thing == 4) {thing <- c(2,2)} else {stop("whoops")}
    whatever <- median (subset_pool[thing[1], thing[2], 1:parameters$number_of_reps])
    subset_output <- which(subset_pool[thing[1], thing[2], 1:parameters$number_of_reps] > whatever)

  } else if (recolorize_style == "range-median") {

    # subpop_measures <- matrix(nrow = 2, ncol = 2, byrow = TRUE)
    for (pop in 1:parameters$num_pop) {
      for (sex in 1:2) {
        subpop_measures[sex,pop] <- max(subset_pool[sex,pop,1:parameters$number_of_reps]) - min(subset_pool[sex,pop,1:parameters$number_of_reps])
      }
    }

    thing <- which (subpop_measures == max(subpop_measures))
    # thing <- 1

    if (thing == 1) {thing <- c(1,1)} else if (thing == 2) {thing <- c(1,2)} else if (thing == 3) {thing <- c(2,1)} else if (thing == 4) {thing <- c(2,2)} else {stop("whoops")}
    whatever <- median (subset_pool[thing[1], thing[2], 1:parameters$number_of_reps])
    subset_output <- which(subset_pool[thing[1], thing[2], 1:parameters$number_of_reps] > whatever)

    # just var, but with a subcluster far below "varX#" so we see how the very edge cases line up with other subpopulations
    # subset_output <- 1:50 # whatever... fix it only if it breaks
  } else if (recolorize_style == "clustering") {
    # #
    # #     highest_clustering_score
    # #     # two metrics - first metric is: best_clusternumber that is highest when "many" reps are clustered, "very close" to each other; the other metric is: having at least one value that is distinct (having a certain minimal distance (fraction of the total possible space/spectrum, say, 10%) ->) from the cluster
    # clustering_measures <- matrix(nrow = 4, ncol = 2, byrow = TRUE)
    # for (pop in 1:parameters$num_pop) {
    #   for (sex in 1:2) {
    #     clustering_measures[sex,pop] <- max(hist(subset_pool[sex,pop,1:parameters$number_of_reps], breaks = seq(0, 1, 0.1), plot = F)$counts)
    #     second_measure <- sort(hist(subset_pool[sex,pop,1:parameters$number_of_reps], breaks = seq(0, 1, 0.1), plot = F)$counts, index.return = TRUE)$ix
    #     clustering_measures[sex + 2, pop] <-
    #   }
    # }

    # thing <- which (clustering_measures == max(clustering_measures))
    # if (thing == 1) {thing <- c(1,1)} else if (thing == 2) {thing <- c(1,2)} else if (thing == 3) {thing <- c(2,1)} else if (thing == 4) {thing <- c(2,2)} else {stop("whoops")}
    # subset_output <- which(subset_pool[thing[1], thing[2], 1:parameters$number_of_reps] > subset_pool[thing[1], thing[2], parameters$number_of_reps + 1])
    subset_output <- 1:50 # whatever... fix it only if it breaks
  } else {
    subset_output <- 1:50
  }

  simple_plots(parameters = parameters, plot_info = plot_info,
               number_of_runs = number_of_repeats, cursitylist = cursitylist,
               sdstbxnlist = sdstbxnlist, curhistlist = curhistlist, sylrepzlist = sylrepzlist,
               mins_n_maxes = mins_n_maxes, saving_dir = saving_dir, recolorize = subset_output, lineplots = lineplots, curMeans_only = curMeans_only)
}

simple_plots <- function(parameters, plot_info = plot_info,
                         number_of_runs = number_of_runs, cursitylist = cursitylist,
                         sdstbxnlist = sdstbxnlist, curhistlist = curhistlist,
                         sylrepzlist = sylrepzlist, mins_n_maxes = mins_n_maxes, saving_dir = multirun_directory, recolorize = FALSE, lineplots = FALSE, curMeans_only = FALSE
                         ) {
  num_timesteps = as.numeric(strsplit(parameters$runlength, "k")[[1]][1])*1000

  if (recolorize != FALSE) {
    if (lineplots != FALSE) {
      saving_dir <- file.path(saving_dir, lineplots)
    } else {
      saving_dir <- file.path(saving_dir, "recolorizedLineplots")
    }
    if (! (dir.exists (saving_dir))) {dir.create (saving_dir)}
  }
  for(population in 1:parameters$num_pop) {
    if (curMeans_only == FALSE) {
      if (recolorize != FALSE) {
      curiosity_figures(parameters = parameters, number_of_runs = number_of_runs,
                      population = population, cursitylist = cursitylist, plot_info = plot_info,
                      mins_n_maxes = mins_n_maxes, saving_dir = saving_dir, recolorize = recolorize, lineplots = lineplots)
    } else {
      curiosity_figures(parameters = parameters, number_of_runs = number_of_runs,
                      population = population, cursitylist = cursitylist, plot_info = plot_info,
                      mins_n_maxes = mins_n_maxes, saving_dir = saving_dir, recolorize = FALSE, lineplots = lineplots)
    }
    }



    for(sex in 1:2) {
      if (curMeans_only == FALSE) {
        meanz <- sylrepzlist[[number_of_runs + 1]][sex,population,]
        if (recolorize != FALSE) {
          if (lineplots != FALSE) {
            stuff <- paste0("lines(sylrepzlist[[", which(1:number_of_runs %in% recolorize), "]][sex,population,],col=\"red\", cex=0.5)")
            stuff2 <- paste0("lines(sylrepzlist[[", which(!(1:number_of_runs %in% recolorize)), "]][sex,population,],col=\"blue\", cex=0.5)")
          } else {
            stuff <- paste0("points(sylrepzlist[[", which(1:number_of_runs %in% recolorize), "]][sex,population,],col=\"red\", cex=0.5)")
            stuff2 <- paste0("points(sylrepzlist[[", which(!(1:number_of_runs %in% recolorize)), "]][sex,population,],col=\"blue\", cex=0.5)")
          }

        } else {
          stuff <- paste0("points(sylrepzlist[[", 1:number_of_runs, "]][sex,population,],col=\"grey\", cex=0.2)")
        }

        file_name <- paste0(plot_info$datez, "_", plot_info$run_name, "_mean_repertoire_size_-_pop_", population, "_", plot_info$sexes_lc[sex], "s.png")
        miny <- mins_n_maxes[(sex + 10),population,1]
        maxy <- mins_n_maxes[(sex + 10),population,2]
        png(filename = paste0(saving_dir, "/", file_name), width = 554, height = 467, units = "px", pointsize = 12, bg = "white")
        plot(meanz, xlab = "Timestep", ylab = paste0("Pop ", population, " ", plot_info$sexes_uc[sex], "s - Mean Repertoire Size"),cex=0.2, ylim=c(miny, maxy), xaxt="n")
        axis(side = 1, at = c(seq.int(0,length(cursitylist[[number_of_runs + 1]][sex,population,]),
                                      ((length(cursitylist[[number_of_runs + 1]][sex,population,]))/10))),
              labels = c(seq.int(0,num_timesteps,(num_timesteps/10))))
        eval(parse(text=stuff))
        if (recolorize != FALSE) {
          eval(parse(text=stuff2))
        }
        lines(sylrepzlist[[number_of_runs + 1]][sex,population,],col="black", cex=0.2)
        dev.off()
      }


      meanz <- cursitylist[[number_of_runs + 1]][sex,population,]
      if (recolorize != FALSE) {
        if (lineplots != FALSE) {
          stuff <- paste0("lines(cursitylist[[", which(1:number_of_runs %in% recolorize), "]][sex,population,],col=\"red\", cex=0.5)")
          stuff2 <- paste0("lines(cursitylist[[", which(!(1:number_of_runs %in% recolorize)), "]][sex,population,],col=\"blue\", cex=0.5)")
        } else {
          stuff <- paste0("points(cursitylist[[", which(1:number_of_runs %in% recolorize), "]][sex,population,],col=\"red\", cex=0.5)")
          stuff2 <- paste0("points(cursitylist[[", which(!(1:number_of_runs %in% recolorize)), "]][sex,population,],col=\"blue\", cex=0.5)")
        }

      } else {
        stuff <- paste0("points(cursitylist[[", 1:number_of_runs, "]][sex,population,],col=\"grey\", cex=0.2)")
      }
      # stuff <- paste0("points(cursitylist[[", 1:number_of_runs, "]][sex,population,],col=\"grey\", cex=0.2)")
      file_name <- paste0(plot_info$datez, "_", plot_info$run_name, "_mean_curiosity_-_pop_", population, "_", plot_info$sexes_lc[sex], "s.png")
      miny <- mins_n_maxes[(sex + 12),population,1]
      maxy <- mins_n_maxes[(sex + 12),population,2]
      png(filename = paste0(saving_dir, "/", file_name), width = 554, height = 467, units = "px", pointsize = 12, bg = "white")
      plot(meanz, xlab = "Timestep", ylab = paste0("Pop ", population, " ", plot_info$sexes_uc[sex], "s - Mean Curiosity"),cex=0.2, ylim=c(miny, maxy), xaxt="n")
      axis(side = 1, at = c(seq.int(0,length(cursitylist[[number_of_runs + 1]][sex,population,]),
                                    ((length(cursitylist[[number_of_runs + 1]][sex,population,]))/10))),
            labels = c(seq.int(0,num_timesteps,(num_timesteps/10))))
      eval(parse(text=stuff))
      if (recolorize != FALSE) {
        eval(parse(text=stuff2))
      }
      lines(cursitylist[[number_of_runs + 1]][sex,population,],col="black", cex=0.2)
      dev.off()

      if (recolorize == FALSE) {
        if (curMeans_only == FALSE) {
          meanz <- sdstbxnlist[[number_of_runs + 1]][(sex + ((population - 1) * 2)), ,]
          file_name <- paste0(plot_info$datez, "_", plot_info$run_name, "_sylnum_pop_", population, "_", plot_info$sexes_lc[sex], "s.png")
          png(filename = paste0(saving_dir, "/", file_name), width = 554, height = 467, units = "px", pointsize = 12, bg = "white")
          image(t(meanz), col = plot_info$sylnum_palette(100), xlab = "Timestep", ylab = paste0("Pop ", population, " ", plot_info$sexes_uc[sex], "s Sylnum"), axes=F)
          axis(1, tck=-0.05, at=c(seq.int(0,1,0.1)),labels=c(seq.int(0,1,0.1)*num_timesteps), col.axis="black", las=2)
          axis(2, tck=-0.05, at=c(seq.int(0,1,(1/12))),labels=c(seq.int(0,1,(1/12))*156), col.axis="black", las=2)
          minor.tick(nx=4, ny=4.8, tick.ratio=1, x.args = list(), y.args = list())
          dev.off()


          meanz <- curhistlist[[number_of_runs + 1]][(sex + ((population - 1) * 2)), ,]
          file_name <- paste0(plot_info$datez, "_", plot_info$run_name, "_curiosity_bins_pop_", population, "_", plot_info$sexes_lc[sex], "s.png")
          png(filename = paste0(saving_dir, "/", file_name), width = 554, height = 467, units = "px", pointsize = 12, bg = "white")
          image(t(meanz), col = plot_info$sylsub_palette(100), xlab = "Timestep", ylab = paste0("Pop ", population, " ", plot_info$sexes_uc[sex], "s Curiosity Bin"), axes=F)
          axis(1, tck=-0.05, at=c(seq.int(0,1,0.1)),labels=c(seq.int(0,1,0.1)*num_timesteps), col.axis="black", las=0)
          axis(2, tck=-0.05, at=c(seq.int(0,1,0.1)),labels=c(seq.int(0,1,0.1)*20), col.axis="black", las=2)
          minor.tick(nx=4, ny=4, tick.ratio=1, x.args = list(), y.args = list())
          dev.off()

          sink(file = paste0(saving_dir, plot_info$datez, plot_info$run_name, "_Summary_Statistics"), append = TRUE)
          print(paste0("pop ", population, " ", plot_info$sexes_uc[sex], " rep size - avg over last 1% of timesteps"))
          print(mean(sylrepzlist[[number_of_runs + 1]][sex, population,
            ((num_timesteps / parameters$recordsimplifyfactor-1):(num_timesteps / parameters$recordsimplifyfactor))]))
          print(paste0("pop ", population, " ", plot_info$sexes_uc[sex], " rep size - avg over last 5% of timesteps"))
          print(mean(sylrepzlist[[number_of_runs + 1]][sex, population,
            (num_timesteps / parameters$recordsimplifyfactor-5):num_timesteps / parameters$recordsimplifyfactor]))
          print(paste0("pop ", population, " ", plot_info$sexes_uc[sex], " curiosity - avg over last 1% of timesteps"))
          print(mean(cursitylist[[number_of_runs + 1]][sex, population,
            (num_timesteps / parameters$recordsimplifyfactor-1):num_timesteps / parameters$recordsimplifyfactor]))
          print(paste0("pop ", population, " ", plot_info$sexes_uc[sex], " curiosity - avg over last 5% of timesteps"))
          print(mean(cursitylist[[number_of_runs + 1]][sex, population,
            (num_timesteps / parameters$recordsimplifyfactor-5):num_timesteps / parameters$recordsimplifyfactor]))
          sink()
        }
      }
    }
  }
}


# as.numeric (strsplit(parameters$runlength, "k")[[1]][1])  * 1000/(parameters$recordsimplifyfactor)
