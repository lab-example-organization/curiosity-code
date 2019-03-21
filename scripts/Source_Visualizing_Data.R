convert_stored_data <- function(parms = params, data_dir = getwd(), simpleObjectSize=100) {
  nTstps = as.numeric(strsplit(parms$runLength, "k")[[1]][1])*1000
  old_names = c("sylrep_rowcol","sylrep_dstbxn","curity_mean_t","curity_repert")
  converted_names = c("sylrepz","sdstbxn","cursity","curhist")
  sylrepz <- array(0, c(
    2, parms$num_pop, nTstps))
  sdstbxn <- array(0, c(
    (2 * parms$num_pop), parms$sylnum, nTstps))
  cursity <- array(0, c(
    12, parms$num_pop, nTstps))
  curhist <- array(0, c(
    (2*parms$num_pop), (parms$num_pop * parms$one_pop_singers[1]), nTstps))
  numDataChunks <- nTstps/1000
  for(data_subset in 1:4) {
    
    thing <- paste0(converted_names[data_subset], 
          "[, , ((1 + ((", 1:numDataChunks, " - 1) * 1000)) : (", 
          1:numDataChunks, " * 1000))] <- readRDS(file = ", '"', 
          data_dir, "/variable-store-", 1:numDataChunks, "-", 
          old_names[data_subset], ".RData", '"', ")")
    eval(parse(text=thing))
    # data1s <- paste0(
    #   old_names[data_subset], "_", 1:numDataChunks, 
    #   " <- readRDS(file = ", '"', data_dir, "/variable-store-", 
    #   1:numDataChunks, "-", old_names[data_subset], ".RData", '"', ")")
    # cat(data1s, file = file.path("source", "data_subset.R"), sep = "\n")
    # source(file.path("source", "data_subset.R"))
    
    # for(i in 1:numDataChunks) {
    #   data2s <- paste0(
    #     converted_names[data_subset], 
    #     "[, , ((1 + ((", i, " - 1) * 1000)) : (", i, " * 1000))] <- ", 
    #     old_names[data_subset], "_", i)
    #   eval(parse(text=data2s))
    # }
    
    if(data_subset==4) {
      sapply(1:4, function(x) {rm(list=ls(pattern=old_names[x]), envir = .GlobalEnv)})
    }
  }

  # within converted_data, converted files are simplified by the factor put in the simpleObjectSize argument
  converted_data <- list(sylrepz = sylrepz[,,tail(seq.int(1,nTstps,
                          (nTstps)/(simpleObjectSize)), simpleObjectSize)],
                         sdstbxn = sdstbxn[,,tail(seq.int(1,nTstps,
                          (nTstps)/(simpleObjectSize)), simpleObjectSize)], 
                         cursity = cursity[,,tail(seq.int(1,nTstps,
                          (nTstps)/(simpleObjectSize)), simpleObjectSize)], 
                         curhist = curhist[,,tail(seq.int(1,nTstps,
                          (nTstps)/(simpleObjectSize)), simpleObjectSize)])
  
  return(converted_data)
}

record_converted_data <- function(converted_data = converted_data) {
  converted_data[[sylrepz]] = saveRDS(file = "sylreps.RData")
  converted_data[[sdstbxn]] = saveRDS(file = "sylnums.RData")
  converted_data[[cursity]] = saveRDS(file = "cursity.RData")
  converted_data[[curhist]] = saveRDS(file = "curhist.RData")
}

harvest_converted_data <- function() {
  sylrepz = readRDS(file = "sylreps.RData")
  sdstbxn = readRDS(file = "sylnums.RData")
  cursity = readRDS(file = "cursity.RData")
  curhist = readRDS(file = "curhist.RData")
  converted_data <- list(c(sylrepz = sylrepz, sdstbxn = sdstbxn, cursity = cursity, curhist = curhist))
  return(converted_data)
}

#converted_data <- harvest_converted_data()
process_data <- function(data_conglomerate = converted_data, specificRepeat = run_visual, path = getwd()) {
  objectnames <- c("sylrepz", "sdstbxn", "cursity", "curhist")
  datanames <- c("SylReps", "SylDist", "Cursity", "CurHist")
  modified_data <- c()
  for (data_subset in 1:4) {
    modified_data <- data_conglomerate[[data_subset]]
    saveRDS(object = modified_data, file = file.path(path, paste0(datanames[data_subset], specificRepeat, ".RData")))
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
  best_colorBrewPal <- colorRampPalette(c("#fef0d9", "#fdcc8a", "#fc8d59", "#d7301f")) # 4-class OrRd
  diverge_colBruPlt <- colorRampPalette(c("#f1a340", "#f7f7f7", "#998ec3"))
  
  
  ### SEQUENTIAL, MULTI-HUE, COLORBLIND FRIENDLY, PRINT FRIENDLY, PHOTOCOPY FRIENDLY
  
  colorSeqMultPalette <- matrix(data = c(c("#e5f5f9", "#99d8c9", "#2ca25f"), # 3-class BuGn
                                          c("#e0ecf4", "#9ebcda", "#8856a7"), # 3-class BuPu
                                          c("#e0f3db", "#a8ddb5", "#43a2ca"), # 3-class GnBu
                                          c("#fee8c8", "#fdbb84", "#e34a33"), # 3-class OrRd
                                          c("#ece7f2", "#a6bddb", "#2b8cbe"), # 3-class PuBu
                                          c("#ece2f0", "#a6bddb", "#1c9099"), # 3-class PuBuGn
                                          c("#e7e1ef", "#c994c7", "#dd1c77"), # 3-class PuRd
                                          c("#fde0dd", "#fa9fb5", "#c51b8a"), # 3-class RdPu
                                          c("#f7fcb9", "#addd8e", "#31a354"), # 3-class YlGn
                                          c("#edf8b1", "#7fcdbb", "#2c7fb8"), # 3-class YlGnBu
                                          c("#fff7bc", "#fec44f", "#d95f0e"), # 3-class YlOrBr
                                          c("#ffeda0", "#feb24c", "#f03b20")), # 3-class YlOrRd
                                nrow = 12, ncol = 3,byrow = T,dimnames = list(
                                  c("BuGn", "BuPu", "GnBu", "OrRd", "PuBu", "PuBuGn", "PuRd",
                                    "RdPu", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"), 
                                  c("Light first color", "Middle color", "Dark final color")))
  
  
  colorSeqMultPalette <- list(BuGn = colorRampPalette(c("#e5f5f9", "#99d8c9", "#2ca25f")), # 3-class BuGn
                              BuPu = colorRampPalette(c("#e0ecf4", "#9ebcda", "#8856a7")), # 3-class BuPu
                              GnBu = colorRampPalette(c("#e0f3db", "#a8ddb5", "#43a2ca")), # 3-class GnBu
                              OrRd = colorRampPalette(c("#fee8c8", "#fdbb84", "#e34a33")), # 3-class OrRd
                              PuBu = colorRampPalette(c("#ece7f2", "#a6bddb", "#2b8cbe")), # 3-class PuBu
                              PuBuGn = colorRampPalette(c("#ece2f0", "#a6bddb", "#1c9099")), # 3-class PuBuGn
                              PuRd = colorRampPalette(c("#e7e1ef", "#c994c7", "#dd1c77")), # 3-class PuRd
                              RdPu = colorRampPalette(c("#fde0dd", "#fa9fb5", "#c51b8a")), # 3-class RdPu
                              YlGn = colorRampPalette(c("#f7fcb9", "#addd8e", "#31a354")), # 3-class YlGn
                              YlGnBu = colorRampPalette(c("#edf8b1", "#7fcdbb", "#2c7fb8")), # 3-class YlGnBu
                              YlOrBr = colorRampPalette(c("#fff7bc", "#fec44f", "#d95f0e")), # 3-class YlOrBr
                              YlOrRd = colorRampPalette(c("#ffeda0", "#feb24c", "#f03b20")))
  
  
  ### SEQUENTIAL, SINGLE-HUE, COLORBLIND FRIENDLY, PRINT FRIENDLY, PHOTOCOPY FRIENDLY
  
  colorSeqSingPalette <- matrix(data = c(c("#deebf7", "#9ecae1", "#3182bd"), # 3-class blues
                                          c("#e5f5e0", "#a1d99b", "#31a354"), # 3-class greens
                                          c("#f0f0f0", "#bdbdbd", "#636363"), # 3-class greys
                                          c("#fee6ce", "#fdae6b", "#e6550d"), # 3-class oranges
                                          c("#efedf5", "#bcbddc", "#756bb1"), # 3-class purples
                                          c("#fee0d2", "#fc9272", "#de2d26")), # 3-class reds
                                nrow = 6, ncol = 3,byrow = T,dimnames = list(
                                  c("blue", "green", "grey", "orange", "purple", "red"), 
                                  c("Light first color", "Middle color", "Dark final color")))
  
  
  sexes <- c("male", "female")
  Sexes <- c("Male", "Female")
  popgroup <- c("Pop_1_male","Pop_1_female","Pop_2_male","Pop_2_female") #, "Pop_3_male", "Pop_3_female", "Pop_4_male", "Pop_4_female"
  plot_info <- list(sylnum_palette = sylnum_palette, sylsub_palette = sylsub_palette, 
                    datez = datez, run_name = run_name, sexes = sexes, Sexes = Sexes, 
                    popgroup = popgroup, colorSeqMultPalette = colorSeqMultPalette, 
                    colorSeqSingPalette = colorSeqSingPalette, best_colorBrewPal = best_colorBrewPal)
  
  return(plot_info)
}

figure_maker <- function(parameters, Q, R, population, q_subset, subset_number, filename, sex_dependent, simple, ylab1, ylab2, number_of_runs, saving_dir = multirun_directory) { # needs number_of_runs passed to it if simple ever == FALSE
  num_timesteps = as.numeric(strsplit(parameters$runLength, "k")[[1]][1])*1000
  if(simple == T) {
    if(sex_dependent == T) {
      for(sex in 1:2) {
        file_name <- paste0(R$datez, "_", R$run_name, filename, population, "_", R$sexes[sex], ".png")
        png(filename = paste0(saving_dir, "/", file_name), width = 554, height = 467, units = "px", pointsize = 12, bg = "white")
        if((q_subset == "sdstbxn") | (q_subset == "curhist")) {
          thing <- paste0("objectz <- Q$", q_subset, "[", sex + ((population - 1) * 2), ",,]")
          eval(parse(text=thing))
          if(q_subset == "sdstbxn") {
            image(t(objectz), col = R$sylnum_palette(100), xlab = "Timestep", ylab = paste0(ylab1, population, " ", R$Sexes[sex], ylab2))
          } else{
            image(t(objectz), col = R$sylsub_palette(100), xlab = "Timestep", ylab = paste0(ylab1, population, " ", R$Sexes[sex], ylab2))
          }
        } else {
          thing <- paste0("objectz <- Q$", q_subset, "[", sex, ",population,]")
          eval(parse(text=thing))
          plot(objectz, xlab = paste0("Timestep"), ylab = paste0(ylab1, population, " ", R$Sexes[sex], ylab2))
        }
        dev.off()
      }
    } else {
      thing <- paste0("objectz <- Q$", q_subset, "[", subset_number, ",population,]")
      eval(parse(text=thing))
      file_name <- paste0(R$datez, "_", R$run_name, filename, population, ".png")
      png(filename = paste0(saving_dir, "/", file_name), width = 554, height = 467, units = "px", pointsize = 12, bg = "white")
      plot(objectz, xlab = "Timestep", ylab = paste0(ylab1, population, ylab2))
      dev.off()
    }
  } else {
    if(sex_dependent == T) {
      for(sex in 1:2) {
        #
      }
    } else {
      #
    }
    meanz <- cursitylist[[number_of_runs + 1]][10,population,]
    stuff <- paste0("points(cursitylist[[", 1:number_of_runs, "]][10,population,],col=\"grey\", cex=0.2)")
    file_name <- paste0(R$datez, "_", R$run_name, "_tutor_selections_pop", population, ".png")
    minY <- mins_n_maxes[2,population,1]
    maxY <- mins_n_maxes[2,population,2]
    png(filename = paste0(saving_dir, "/", file_name), width = 554, height = 467, units = "px", pointsize = 12, bg = "white")
    plot(meanz, xlab = "Timestep", ylab = paste0("Pop ", population, " Tutor Selection Chances"),cex=0.2, ylim=c(minY, maxY), xaxt="n")
    #axis(side = 1, at = c(which((1:P$num_timesteps)%%(P$num_timesteps/10)==0)), labels = which((1:P$num_timesteps)%%(P$num_timesteps/10)==0))
    axis(side = 1, at = c(seq.int(0,length(cursitylist[[number_of_runs + 1]][10,population,]),
                                  ((length(cursitylist[[number_of_runs + 1]][10,population,]))/10))), 
        labels = c(seq.int(0,num_timesteps,(num_timesteps/10))))
    eval(parse(text=stuff))
    lines(cursitylist[[number_of_runs + 1]][10,population,],col="black", cex=0.2)
    dev.off()
    
    meanz <- sylrepzlist[[number_of_runs + 1]][sex,population,]
    stuff <- paste0("points(sylrepzlist[[", 1:number_of_runs, "]][sex,population,],col=\"grey\", cex=0.2)")
    file_name <- paste0(R$datez, "_", R$run_name, "_mean_repertoire_size_-_pop_", population, "_", R$sexes[sex], "s.png")
    minY <- mins_n_maxes[(sex + 10),population,1]
    maxY <- mins_n_maxes[(sex + 10),population,2]
    png(filename = paste0(saving_dir, "/", file_name), width = 554, height = 467, units = "px", pointsize = 12, bg = "white")
    plot(meanz, xlab = "Timestep", ylab = paste0("Pop ", population, " ", R$Sexes[sex], "s - Mean Repertoire Size"),cex=0.2, ylim=c(minY, maxY), xaxt="n")
    #axis(side = 1, at = c(which((1:P$num_timesteps)%%(P$num_timesteps/10)==0)), labels = which((1:P$num_timesteps)%%(P$num_timesteps/10)==0))
    axis(side = 1, at = c(seq.int(0,length(cursitylist[[number_of_runs + 1]][sex,population,]),
                                  ((length(cursitylist[[number_of_runs + 1]][sex,population,]))/10))), 
        labels = c(seq.int(0,num_timesteps,(num_timesteps/10))))
    eval(parse(text=stuff))
    lines(cursitylist[[number_of_runs + 1]][sex,population,],col="black", cex=0.2)
    dev.off()
    
  }
}

summary_statistics <- function(parameters, Q, R, population, simplification_factor) {
  num_timesteps = as.numeric(strsplit(parameters$runLength, "k")[[1]][1])*1000
  for(sex in 1:2) {
    sink(file = paste0(R$datez, R$run_name, "_Summary_Statistics"), append = TRUE)
    print(paste0("pop ", population, " ", R$Sexes[sex], " rep size - avg over last 1% of timesteps"))
    print(mean(Q$sylrepz[sex, population, 
      ((num_timesteps/(num_timesteps/100) - 1):
        num_timesteps/(num_timesteps/100))]))
    print(paste0("pop ", population, " ", R$Sexes[sex], " rep size - avg over last 5% of timesteps"))
    print(mean(Q$sylrepz[sex, population, 
      ((num_timesteps/(num_timesteps/100) - 5):
        num_timesteps/(num_timesteps/100))]))
    print(paste0("pop ", population, " ", R$Sexes[sex], " curiosity - avg over last 1% of timesteps"))
    print(mean(Q$cursity[sex, population, 
      ((num_timesteps/(num_timesteps/100) - 1):
        num_timesteps/(num_timesteps/100))]))
    print(paste0("pop ", population, " ", R$Sexes[sex], " curiosity - avg over last 5% of timesteps"))
    print(mean(Q$cursity[sex, population, 
      ((num_timesteps/(num_timesteps/100) - 5):
        num_timesteps/(num_timesteps/100))]))
    sink() 
  }
}


min_n_max <- function(parameters, number_of_runs = number_of_runs, cursitylist = cursitylist, 
                         sdstbxnlist = sdstbxnlist, curhistlist = curhistlist, 
                         sylrepzlist = sylrepzlist) {
  nrowMinsMaxes <- 14
  mins_n_maxes <- array(0,c(nrowMinsMaxes,parameters$num_pop,2)) # rows = different things being measured, columns = populations (1&2) for 1:9 and populations & sex ((1) pop1male (2) pop1female (3) pop2male (4) pop2female); depth = min (1) and max (2)
  mn_mx_container <- c("min", "max") # 3rd-dim-dependent --- 
  objectnames <- c("curhist","cursity","sdstbxn","sylrepz") # row-dependent --- k -> (objectnames[objectSubset[k]])
  figureSubset <- c(3,10,4,5,6,7,8,9,11,12,1,2,1,2) # row-dependent --- k
  objectSubset <- c(2,2,2,2,2,2,2,2,2,2,4,4,2,2) # row-dependent --- k
  
  for(j in 1:parameters$num_pop) {
    for(k in 1:nrowMinsMaxes ) {
      for(L in 1:2) {
        # This is for min (1) and max (2)
        container <- vector("numeric", number_of_runs)
        
        for(i in 1:number_of_runs) {
          #container[i] <- min(eval(parse(text=paste0(objectnames, "list[[", i, "]][", subset, ", ", j, ",]"))))
          eval(parse(text=paste0("container[i] <- ", mn_mx_container[L], "(", paste0(objectnames[objectSubset[k]], "list[[", i, "]][", figureSubset[k], ", ", j, ",])"))))
        }
        
        eval(parse(text=paste0("mins_n_maxes[k,j,L] <- ", mn_mx_container[L], "(container)")))
      }
    }
    #thing_to_evalparse <- paste0("mins_n_maxes[," ,j, ",] <- min(container)")
  }
  
  return(mins_n_maxes)
}


curiosity_figures <- function(parameters, number_of_runs, population, cursitylist, R, mins_n_maxes, saving_dir = multirun_directory) {
  figure_retainer <- c(3,10,4,5,6,7,8,9,11)
  
  filename_retainer <- c("_mate_selections_pop", "_tutor_selections_pop", "_AC_parent_m_pop", 
                          "_AC_parent_f_pop", "_AC_offspring_m_pop", "_AC_offspring_f_pop", "_AC_replaced_m_pop",
                          "_AC_replaced_f_pop", "_cur_inh_attempts")
  plot_title_retainer <- c(" Mate Selection Chances", " Tutor Selection Chances", " Father AC", " Mother AC",
                            " Son AC", " Daughter AC", " Dead Man AC", " Dead Woman AC", " Cur Inh Attempts")
  num_timesteps = as.numeric(strsplit(parameters$runLength, "k")[[1]][1])*1000
  for(individual_figures in 1:9) {

    meanz <- cursitylist[[number_of_runs + 1]][(figure_retainer[individual_figures]),population,]
    stuff <- paste0("points(cursitylist[[", 1:number_of_runs, "]][", (figure_retainer[individual_figures]), ",population,],col=\"grey\", cex=0.2)")
    file_name <- paste0(R$datez, "_", R$run_name, filename_retainer[individual_figures], population, ".png")
    minY <- mins_n_maxes[individual_figures,population,1]
    maxY <- mins_n_maxes[individual_figures,population,2]
    
    png(filename = paste0(saving_dir, "/", file_name), width = 554, height = 467, units = "px", pointsize = 12, bg = "white")
    plot(meanz, xlab = "Timestep", ylab = paste0("Pop ", population, plot_title_retainer[individual_figures]),cex=0.2, ylim=c(minY, maxY), xaxt="n")
    axis(side = 1, 
          at = c(seq.int(0,length(cursitylist[[number_of_runs + 1]][figure_retainer[individual_figures],population,]),
                                  ((length(cursitylist[[number_of_runs + 1]][figure_retainer[individual_figures],population,]))/10))),
          labels = c(seq.int(0,num_timesteps,(num_timesteps/10))))
    
    eval(parse(text=stuff))
    lines(cursitylist[[number_of_runs + 1]][figure_retainer[individual_figures],population,],col="black", cex=0.2)
    dev.off()

  }  
}


simple_plots <- function(parameters, R = R, Q = converted_data, extra_lines = FALSE,
                         number_of_runs=number_of_runs, cursitylist = cursitylist, 
                         sdstbxnlist = sdstbxnlist, curhistlist = curhistlist, 
                         sylrepzlist = sylrepzlist, mins_n_maxes = mins_n_maxes, saving_dir = multirun_directory) {
  num_timesteps = as.numeric(strsplit(parameters$runLength, "k")[[1]][1])*1000
  if(extra_lines == FALSE) {
    for(population in 1:parameters$num_pop) {
      figure_maker(parameters = parameters, Q, R, population, "cursity", "3", "_mate_selections_pop", F, T, "Pop", " Selection Chances", number_of_runs = number_of_runs)
      figure_maker(parameters = parameters, Q, R, population, "cursity", "10", "_tutor_selections_pop", F, T, "Pop", " Selection Chances", number_of_runs = number_of_runs)
      figure_maker(parameters = parameters, Q, R, population, "cursity", "4", "_AC_parent_m_pop", F, T, "Pop", " Father AC", number_of_runs = number_of_runs)
      figure_maker(parameters = parameters, Q, R, population, "cursity", "5", "_AC_parent_f_pop", F, T, "Pop", " Mother AC", number_of_runs = number_of_runs)
      figure_maker(parameters = parameters, Q, R, population, "cursity", "6", "_AC_offspring_m_pop", F, T, "Pop", " Son AC", number_of_runs = number_of_runs)
      figure_maker(parameters = parameters, Q, R, population, "cursity", "7", "_AC_offspring_f_pop", F, T, "Pop", " Daughter AC", number_of_runs = number_of_runs)
      figure_maker(parameters = parameters, Q, R, population, "cursity", "8", "_AC_replaced_m_pop", F, T, "Pop", " Dead Man AC", number_of_runs = number_of_runs)
      figure_maker(parameters = parameters, Q, R, population, "cursity", "9", "_AC_replaced_f_pop", F, T, "Pop", " Dead Woman AC", number_of_runs = number_of_runs)
      figure_maker(parameters = parameters, Q, R, population, "cursity", "11", "_cur_inh_attempts", F, T, "Pop", " Cur Inh Attempts", number_of_runs = number_of_runs)
      figure_maker(parameters = parameters, Q, R, population, "cursity", "12", "_newsyll_attempts", F, T, "Pop", " New Syll Attempts", number_of_runs = number_of_runs)
      figure_maker(parameters = parameters, Q, R, population, "sylrepz", "sex_dependent == TRUE", "_mean_repertoire_size_-_pop_", T, T, "Pop", "s - Mean Repertoire Size", number_of_runs = number_of_runs)
      figure_maker(parameters = parameters, Q, R, population, "cursity", "sex_dependent == TRUE", "_mean_curiosity_-_pop_", T, T, "Pop", "s - Mean Curiosity", number_of_runs = number_of_runs)
      figure_maker(parameters = parameters, Q, R, population, "sdstbxn", "sex_dependent == TRUE", "_sylnum_pop_", T, T, "Pop", "s Sylnum", number_of_runs = number_of_runs)
      figure_maker(parameters = parameters, Q, R, population, "curhist", "sex_dependent == TRUE", "_curiosity_bins_pop_", T, T, "Pop", "s Curiosity Bin", number_of_runs = number_of_runs)
      
      summary_statistics(parameters = parameters, Q, R, population)
    }
  } else {
    for(population in 1:parameters$num_pop) {
      
      curiosity_figures(parameters = parameters, number_of_runs = number_of_runs, 
                        population = population, cursitylist = cursitylist, R = R, 
                        mins_n_maxes = mins_n_maxes, saving_dir = saving_dir)
      
      for(sex in 1:2) {
        
        meanz <- sylrepzlist[[number_of_runs + 1]][sex,population,]
        stuff <- paste0("points(sylrepzlist[[", 1:number_of_runs, "]][sex,population,],col=\"grey\", cex=0.2)")
        file_name <- paste0(R$datez, "_", R$run_name, "_mean_repertoire_size_-_pop_", population, "_", R$sexes[sex], "s.png")
        minY <- mins_n_maxes[(sex + 10),population,1]
        maxY <- mins_n_maxes[(sex + 10),population,2]
        png(filename = paste0(saving_dir, "/", file_name), width = 554, height = 467, units = "px", pointsize = 12, bg = "white")
        plot(meanz, xlab = "Timestep", ylab = paste0("Pop ", population, " ", R$Sexes[sex], "s - Mean Repertoire Size"),cex=0.2, ylim=c(minY, maxY), xaxt="n")
        axis(side = 1, at = c(seq.int(0,length(cursitylist[[number_of_runs + 1]][sex,population,]),
                                      ((length(cursitylist[[number_of_runs + 1]][sex,population,]))/10))), 
              labels = c(seq.int(0,num_timesteps,(num_timesteps/10))))
        eval(parse(text=stuff))
        lines(cursitylist[[number_of_runs + 1]][sex,population,],col="black", cex=0.2)
        dev.off()
        
        meanz <- cursitylist[[number_of_runs + 1]][sex,population,]
        stuff <- paste0("points(cursitylist[[", 1:number_of_runs, "]][sex,population,],col=\"grey\", cex=0.2)")
        file_name <- paste0(R$datez, "_", R$run_name, "_mean_curiosity_-_pop_", population, "_", R$sexes[sex], "s.png")
        minY <- mins_n_maxes[(sex + 12),population,1]
        maxY <- mins_n_maxes[(sex + 12),population,2]
        png(filename = paste0(saving_dir, "/", file_name), width = 554, height = 467, units = "px", pointsize = 12, bg = "white")
        plot(meanz, xlab = "Timestep", ylab = paste0("Pop ", population, " ", R$Sexes[sex], "s - Mean Curiosity"),cex=0.2, ylim=c(minY, maxY), xaxt="n")
        axis(side = 1, at = c(seq.int(0,length(cursitylist[[number_of_runs + 1]][sex,population,]),
                                      ((length(cursitylist[[number_of_runs + 1]][sex,population,]))/10))), 
              labels = c(seq.int(0,num_timesteps,(num_timesteps/10))))
        eval(parse(text=stuff))
        lines(cursitylist[[number_of_runs + 1]][sex,population,],col="black", cex=0.2)
        dev.off()
        
        
        meanz <- sdstbxnlist[[number_of_runs + 1]][(sex + ((population - 1) * 2)), ,]
        file_name <- paste0(R$datez, "_", R$run_name, "_sylnum_pop_", population, "_", R$sexes[sex], "s.png")
        png(filename = paste0(saving_dir, "/", file_name), width = 554, height = 467, units = "px", pointsize = 12, bg = "white")
        image(t(meanz), col = R$sylnum_palette(100), xlab = "Timestep", ylab = paste0("Pop ", population, " ", R$Sexes[sex], "s Sylnum"), axes=F)
        axis(1, tck=-0.05, at=c(seq.int(0,1,0.1)),labels=c(seq.int(0,1,0.1)*num_timesteps), col.axis="black", las=2)
        axis(2, tck=-0.05, at=c(seq.int(0,1,(1/12))),labels=c(seq.int(0,1,(1/12))*156), col.axis="black", las=2)
        minor.tick(nx=4, ny=4.8, tick.ratio=1, x.args = list(), y.args = list())
        dev.off()
        
        
        meanz <- curhistlist[[number_of_runs + 1]][(sex + ((population - 1) * 2)), ,]
        file_name <- paste0(R$datez, "_", R$run_name, "_curiosity_bins_pop_", population, "_", R$sexes[sex], "s.png")
        png(filename = paste0(saving_dir, "/", file_name), width = 554, height = 467, units = "px", pointsize = 12, bg = "white")
        image(t(meanz), col = R$sylsub_palette(100), xlab = "Timestep", ylab = paste0("Pop ", population, " ", R$Sexes[sex], "s Curiosity Bin"), axes=F)
        axis(1, tck=-0.05, at=c(seq.int(0,1,0.1)),labels=c(seq.int(0,1,0.1)*num_timesteps), col.axis="black", las=0)
        axis(2, tck=-0.05, at=c(seq.int(0,1,0.1)),labels=c(seq.int(0,1,0.1)*20), col.axis="black", las=2)
        minor.tick(nx=4, ny=4, tick.ratio=1, x.args = list(), y.args = list())
        dev.off()
        
        sink(file = paste0(saving_dir, R$datez, R$run_name, "_Summary_Statistics"), append = TRUE)
        print(paste0("pop ", population, " ", R$Sexes[sex], " rep size - avg over last 1% of timesteps"))
        print(mean(sylrepzlist[[number_of_runs + 1]][sex, population, 
          ((num_timesteps/(num_timesteps/100) - 1):
            num_timesteps/(num_timesteps/100))]))
        print(paste0("pop ", population, " ", R$Sexes[sex], " rep size - avg over last 5% of timesteps"))
        print(mean(sylrepzlist[[number_of_runs + 1]][sex, population, 
          ((num_timesteps/(num_timesteps/100) - 5):
            num_timesteps/(num_timesteps/100))]))
        print(paste0("pop ", population, " ", R$Sexes[sex], " curiosity - avg over last 1% of timesteps"))
        print(mean(cursitylist[[number_of_runs + 1]][sex, population, 
          ((num_timesteps/(num_timesteps/100) - 1):
            num_timesteps/(num_timesteps/100))]))
        print(paste0("pop ", population, " ", R$Sexes[sex], " curiosity - avg over last 5% of timesteps"))
        print(mean(cursitylist[[number_of_runs + 1]][sex, population, 
          ((num_timesteps/(num_timesteps/100) - 5):
            num_timesteps/(num_timesteps/100))]))
        sink()
        
      }
    }
  }
}