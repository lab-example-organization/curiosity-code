#Data Recording objects
library(abind)
library(stringr)
library(Hmisc)
rm(list = objects())
number_of_runs <- source(paste0(strsplit(getwd(),"20")[[1]][1],"number_of_runs.txt"))$value
FolderName = readRDS(file = "harvest_info.RData")
P = readRDS(file = "parameters.RData")
thousand_timesteps = readRDS(file = "timestep_grps.RData")
#thousand_timesteps <- 5
convert_stored_data <- function(P = P, num_timechunks=thousand_timesteps, data_dir = getwd(), simplification_factor=100) {
  #dir <- getwd()
  old_names = c("sylrep_rowcol","sylrep_dstbxn","curity_mean_t","curity_repert")
  converted_names = c("sylrepz","sdstbxn","cursity","curhist")
  sylrepz <- array(0, c(2, P$num_pop, P$num_timesteps))
  sdstbxn <- array(0, c((2 * P$num_pop), P$sylnum, P$num_timesteps))
  cursity <- array(0, c(12, P$num_pop, P$num_timesteps))
  curhist <- array(data = 0, dim = c((2*P$num_pop), (P$num_pop * P$num_one.pop_singers_sampled[1]), P$num_timesteps))
  
  for(data_subset in 1:4) {
    data1s <- paste0(
      old_names[data_subset], "_", 1:num_timechunks, " <- readRDS(file = ", '"',
      run_number_directory, "/", strsplit(run_number_directory, "-GMT-")[[1]][2], "-",
      1:num_timechunks, "-", old_names[data_subset], ".RData", '"', ")")
    cat(data1s, file = "data_subset.R", sep = "\n")
    source("data_subset.R")
    
    for(i in 1:num_timechunks) {
      data2s <- paste0(converted_names[data_subset], "[, , ((1 + ((", i, " - 1) * 1000)) : (", i, " * 1000))] <- ", old_names[data_subset], "_", i)
      eval(parse(text=data2s))
      #data3s <- paste0("rm(", names[data_subset], "_", i, ")")
      #eval(parse(text=data3s))
      #rm(paste0(names[data_subset], "_", i))
    }
    #data3s <- paste0("rm(", old_names[data_subset], "_", 1:num_timechunks, ")")
    #eval(parse(text=data3s))
    if(data_subset==4) {
      data3s <- paste0(old_names[data_subset], "_", 1:num_timechunks)
      #sapply(1:num_timechunks, function(x) {rm(paste0(data3s[x]))})
      sapply(1:4, function(x) {rm(list=ls(pattern=old_names[x]), envir = .GlobalEnv)})
      
      #rm(list=ls(pattern=c("sylrep_rowcol","sylrep_dstbxn","curity_mean_t","curity_repert")))
      
    }
  }
  converted_data <- list(sylrepz = sylrepz[,,seq.int(1,P$num_timesteps,simplification_factor)], sdstbxn = sdstbxn[,,seq.int(1,P$num_timesteps,simplification_factor)], cursity = cursity[,,seq.int(1,P$num_timesteps,simplification_factor)], curhist = curhist[,,seq.int(1,P$num_timesteps,simplification_factor)])
  #shortened_convert <- list()
  #for(i in 1:4) {
  #  
  #  converted_data[[i]] <- converted_data[[i]][,,seq.int(1,length(P$num_timesteps),simplification_factor)]
  #}
  return(converted_data)
  #rm(list=ls(pattern=old_names[1]),envir = .GlobalEnv)
  #rm(list=ls(pattern=old_names[2]),envir = .GlobalEnv)
  #rm(list=ls(pattern=old_names[3]),envir = .GlobalEnv)
  #rm(list=ls(pattern=old_names[4]),envir = .GlobalEnv)
}



#converted_data <- convert_stored_data(P = P, num_timechunks = thousand_timesteps)
record_converted_data <- function(converted_data = converted_data) {
  converted_data[[sylrepz]] = saveRDS(file = "sylreps.RData")
  converted_data[[sdstbxn]] = saveRDS(file = "sylnums.RData")
  converted_data[[cursity]] = saveRDS(file = "cursity.RData")
  converted_data[[curhist]] = saveRDS(file = "curhist.RData")
}

#rm(converted_data)
harvest_converted_data <- function() {
  sylrepz = readRDS(file = "sylreps.RData")
  sdstbxn = readRDS(file = "sylnums.RData")
  cursity = readRDS(file = "cursity.RData")
  curhist = readRDS(file = "curhist.RData")
  converted_data <- list(c(sylrepz = sylrepz, sdstbxn = sdstbxn, cursity = cursity, curhist = curhist))
  return(converted_data)
}

#converted_data <- harvest_converted_data()
split_data <- function(data_conglomerate = converted_data, data_subset = 1) {
  subset_of_the_data <- data_conglomerate[[data_subset]]
  return(subset_of_the_data)
}

#sylrepz <- split_data(data_subset = 1)
#sdstbxn <- split_data(data_subset = 2)
#cursity <- split_data(data_subset = 3)
#curhist <- split_data(data_subset = 4)

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
  
  sexes <- c("male", "female")
  Sexes <- c("Male", "Female")
  popgroup <- c("Pop_1_male","Pop_1_female","Pop_2_male","Pop_2_female") #, "Pop_3_male", "Pop_3_female", "Pop_4_male", "Pop_4_female"
  plot_info <- list(sylnum_palette = sylnum_palette, sylsub_palette = sylsub_palette, datez = datez, run_name = run_name, sexes = sexes, Sexes = Sexes, popgroup = popgroup)
  
  return(plot_info)
}

#col_sylsub <- colorRampPalette(c("dark red","red","white","blue","dark blue"))
#col_extremes <- colorRampPalette(c("dark red","goldenrod","red","red","azure","azure","azure","azure","azure","azure","azure","azure","azure","azure","azure","blue","blue","salmon","dark blue"))
#col_syladd <- colorRampPalette(c("black","dark blue","plum","pink","orange","yellow","light yellow"))

#aa <- paste("sylnum_palette.tiff",sep = "_")
#tiff(filename = aa, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none",)
#plot(rep(1,100),col=sylnum_palette(100),pch=19,cex=3)
#dev.off()

#aa <- paste("sylsub_palette.tiff",sep = "_")
#tiff(filename = aa, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none",)
#plot(rep(1,100),col=sylsub_palette(100),pch=19,cex=3)
#dev.off()

#aa <- paste(w,x,y,z,"col_col_extremes.tiff",sep = "_")
#tiff(filename = aa, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none",)
#plot(rep(1,100),col=col_extremes(100),pch=19,cex=3)
#dev.off()

#abebc6 light green
#85c1e9 light blue
#a93226 dark red
#5b2c6f dark purple

### figure_maker(P=P, Q=Q, R=R, population=population, q_subset="cursity", subset_number="3", filename="_mate_selections_pop", 
### sex_dependent=F, simple=T, ylab1="Pop", ylab2=" Selection Chances")

figure_maker <- function(P, Q, R, population, q_subset, subset_number, filename, sex_dependent, simple, ylab1, ylab2) {
  if(simple == T) {
    if(sex_dependent == T) {
      for(sex in 1:2) {
        file_name <- paste0(R$datez, "_", R$run_name, filename, population, "_", R$sexes[sex], ".tiff")
        tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
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
      file_name <- paste0(R$datez, "_", R$run_name, filename, population, ".tiff")
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(objectz, xlab = "Timestep", ylab = paste0(ylab1, population, ylab2))
      dev.off()
    }
  } else {
    if(sex_dependent == T) {
      for(sex in 1:2) {
        file_name <- paste0(R$datez, "_", R$run_name, filename, population, "_", R$sexes[sex], "s_full.tiff")
        tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
        if((q_subset == "sdstbxn") | (q_subset == "curhist")) {
          thing <- paste0("objectz <- Q$", q_subset, "[", sex + ((population - 1) * 2), ",,]")
          eval(parse(text=thing))
          if(q_subset == "sdstbxn") {
            image(t(objectz), col = R$sylnum_palette(100), xlab = paste0("Timestep"), ylab = paste0(ylab1, population, " ", R$Sexes[sex], ylab2))
          } else{
            image(t(objectz), col = R$sylsub_palette(100), xlab = paste0("Timestep"), ylab = paste0(ylab1, population, " ", R$Sexes[sex], ylab2))
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
      file_name <- paste0(R$datez, "_", R$run_name, filename, population, "_full.tiff")
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(objectz, xlab = "Timestep", ylab = paste0(ylab1, population, ylab2))
      dev.off()
    }
  }
}

summary_statistics <- function(P, Q, R, population) {
  for(sex in 1:2) {
    sink(file = paste0(R$datez, R$run_name, "_Summary_Statistics"), append = TRUE)
    print(paste0("pop ", population, " ", R$Sexes[sex], " rep size - avg over last 100 timesteps"))
    print(mean(Q$sylrepz[sex, population, ((P$num_timesteps - 100):P$num_timesteps)]))
    print(paste0("pop ", population, " ", R$Sexes[sex], " rep size - avg over last 500 timesteps"))
    print(mean(Q$sylrepz[sex, population, ((P$num_timesteps - 500):P$num_timesteps)]))
    print(paste0("pop ", population, " ", R$Sexes[sex], " curiosity - avg over last 100 timesteps"))
    print(mean(Q$cursity[sex, population, ((P$num_timesteps - 100):P$num_timesteps)]))
    print(paste0("pop ", population, " ", R$Sexes[sex], " curiosity - avg over last 500 timesteps"))
    print(mean(Q$cursity[sex, population, ((P$num_timesteps - 500):P$num_timesteps)]))
    sink() 
  }
}


#Simple Stuff


##########Template for extra_lines
#meanz <- cursitylist[[11]][3,population,]
#stuff <- paste0("points(cursitylist[[", 1:number_of_runs, "]][3,1,],col=\"grey\", cex=0.1)")
#file_name <- paste0(R1$datez, "_", R1$run_name, "_mate_selections_pop", population, ".tiff")
#tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
#plot(meanz, xlab = "Timestep", ylab = paste0("Pop ", population, " Selection Chances"),cex=0.1)
#eval(parse(text=stuff))
#dev.off()

##########Template for plot function:
#plot(x, y, main="title", sub="subtitle",
# xlab="X-axis label", ylab="y-axix label",
# xlim=c(xmin, xmax), ylim=c(ymin, ymax))

#title(main="main title", sub="sub-title", 
# xlab="x-axis label", ylab="y-axis label")

## EXAMPLE: Add a red title and a blue subtitle. Make x and y 
## labels 25% smaller than the default and green. 
#title(main="My Title", col.main="red", 
#      sub="My Sub-title", col.sub="blue", 
#      xlab="My X label", ylab="My Y label",
#      col.lab="green", cex.lab=0.75)

#text(location, "text to place", pos, ...)
#mtext("text to place", side, line=n, ...)

#Common options are described below.

#location:	location can be an x,y coordinate. 
#   Alternatively, the text can be placed interactively 
#   via mouse by specifying location as locator(1).
#pos:	position relative to location. 1=below, 2=left, 
#   3=above, 4=right. If you specify pos, you can specify 
#   offset= in percent of character width.
#side:	which margin to place text. 1=bottom, 2=left, 3=top, 
#   4=right. you can specify line= to indicate the line in the 
#   margin starting with 0 and moving out. you can also specify 
#   adj=0 for left/bottom alignment or adj=1 for top/right alignment.
#Other common options are cex, col, and font 
#   (for size, color, and font style respectively).



#Full Plot Stuff

min_n_max <- function(number_of_runs = number_of_runs) {
  mins_n_maxes <- array(0,c(14,2,2)) # rows = different things being measured, columns = populations (1&2) for 1:9 and populations & sex ((1) pop1male (2) pop1female (3) pop2male (4) pop2female); depth = min (1) and max (2)
  mn_mx_container <- c("min", "max") # 3rd-dim-dependent --- 
  objectnames <- c("curhist","cursity","sdstbxn","sylrepz") # row-dependent --- k -> (objectnames[objectSubset[k]])
  figureSubset <- c(3,10,4,5,6,7,8,9,11,12,1,2,1,2) # row-dependent --- k
  objectSubset <- c(2,2,2,2,2,2,2,2,2,2,4,4,2,2) # row-dependent --- k
  for(j in 1:P$num_pop) {
    for(k in 1:nrow(mins_n_maxes)) {
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



simple_plots <- function(Q = converted_data, extra_lines = FALSE) {
  if(extra_lines == FALSE) {
    for(population in 1:P$num_pop) {
      figure_maker(P, Q, R, population, "cursity", "3", "_mate_selections_pop", F, T, "Pop", " Selection Chances")
      figure_maker(P, Q, R, population, "cursity", "10", "_tutor_selections_pop", F, T, "Pop", " Selection Chances")
      figure_maker(P, Q, R, population, "cursity", "4", "_AC_parent_m_pop", F, T, "Pop", " Father AC")
      figure_maker(P, Q, R, population, "cursity", "5", "_AC_parent_f_pop", F, T, "Pop", " Mother AC")
      figure_maker(P, Q, R, population, "cursity", "6", "_AC_offspring_m_pop", F, T, "Pop", " Son AC")
      figure_maker(P, Q, R, population, "cursity", "7", "_AC_offspring_f_pop", F, T, "Pop", " Daughter AC")
      figure_maker(P, Q, R, population, "cursity", "8", "_AC_replaced_m_pop", F, T, "Pop", " Dead Man AC")
      figure_maker(P, Q, R, population, "cursity", "9", "_AC_replaced_f_pop", F, T, "Pop", " Dead Woman AC")
      figure_maker(P, Q, R, population, "cursity", "11", "_cur_inh_attempts", F, T, "Pop", " Cur Inh Attempts")
      figure_maker(P, Q, R, population, "cursity", "12", "_newsyll_attempts", F, T, "Pop", " New Syll Attempts")
      figure_maker(P, Q, R, population, "sylrepz", "sex_dependent == TRUE", "_mean_repertoire_size_-_pop_", T, T, "Pop", "s - Mean Repertoire Size")
      figure_maker(P, Q, R, population, "cursity", "sex_dependent == TRUE", "_mean_curiosity_-_pop_", T, T, "Pop", "s - Mean Curiosity")
      figure_maker(P, Q, R, population, "sdstbxn", "sex_dependent == TRUE", "_sylnum_pop_", T, T, "Pop", "s Sylnum")
      figure_maker(P, Q, R, population, "curhist", "sex_dependent == TRUE", "_curiosity_bins_pop_", T, T, "Pop", "s Curiosity Bin")
      
      summary_statistics(P, Q, R, population)
    }
  } else {
    for(population in 1:P$num_pop) {
      # make an object for the mean value
      meanz <- cursitylist[[11]][3,population,]
      #thing <- paste0("objectz", 1:number_of_runs, " <- mean(converted_data", 1:mult_file_length, "$cursity[3, ,])")
      #thing <- paste0("objectz", 1:number_of_runs, " <- mean(converted_data", 1:mult_file_length, "$cursity[3, ,])")
      stuff <- paste0("points(cursitylist[[", 1:number_of_runs, "]][3,population,],col=\"grey\", cex=0.2)")
      #eval(parse(text=thing))
      file_name <- paste0(R$datez, "_", R$run_name, "_mate_selections_pop", population, ".tiff")
      
      
      ## minY <- min(cursitylist[[", 1:number_of_runs, "]][3,1,])
      #sink(file = "min_retainer.R", append = F, split = TRUE)
      #thing <- paste(cat("minY = min("), cat(paste0("cursitylist[[", 1:number_of_runs, "]][3,population,]"), sep=", "), cat(")"))
      #sink()
      #source("min_retainer.R")
      
      ## maxY <- max(cursitylist[[", 1:number_of_runs, "]][3,1,])
      #sink(file = "max_retainer.R", append = F, split = TRUE)
      #thing <- paste(cat("maxY = max("), cat(paste0("cursitylist[[", 1:number_of_runs, "]][3,population,]"), sep=", "), cat(")"))
      #sink()
      #source("max_retainer.R")
      minY <- mins_n_maxes[1,population,1]
      maxY <- mins_n_maxes[1,population,2]
      
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(meanz, xlab = "Timestep", ylab = paste0("Pop ", population, " Mate Selection Chances"),cex=0.2, ylim=c(minY, maxY), xaxt="n")
      axis(side = 1, at = c(which((1:P$num_timesteps)%%100==0)), labels = which((1:P$num_timesteps)%%100==0))
      eval(parse(text=stuff))
      dev.off()
      
      meanz <- cursitylist[[11]][10,population,]
      stuff <- paste0("points(cursitylist[[", 1:number_of_runs, "]][10,population,],col=\"grey\", cex=0.2)")
      file_name <- paste0(R$datez, "_", R$run_name, "_tutor_selections_pop", population, ".tiff")
      minY <- mins_n_maxes[2,population,1]
      maxY <- mins_n_maxes[2,population,2]
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(meanz, xlab = "Timestep", ylab = paste0("Pop ", population, " Tutor Selection Chances"),cex=0.2, ylim=c(minY, maxY))
      axis(side = 1, at = c(which((1:P$num_timesteps)%%100==0)), labels = which((1:P$num_timesteps)%%100==0))
      eval(parse(text=stuff))
      dev.off()
      
      meanz <- cursitylist[[11]][4,population,]
      stuff <- paste0("points(cursitylist[[", 1:number_of_runs, "]][4,population,],col=\"grey\", cex=0.2)")
      file_name <- paste0(R$datez, "_", R$run_name, "_AC_parent_m_pop", population, ".tiff")
      minY <- mins_n_maxes[3,population,1]
      maxY <- mins_n_maxes[3,population,2]
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(meanz, xlab = "Timestep", ylab = paste0("Pop ", population, " Father AC"),cex=0.2, ylim=c(minY, maxY))
      axis(side = 1, at = c(which((1:P$num_timesteps)%%100==0)), labels = which((1:P$num_timesteps)%%100==0))
      eval(parse(text=stuff))
      dev.off()
      
      meanz <- cursitylist[[11]][5,population,]
      stuff <- paste0("points(cursitylist[[", 1:number_of_runs, "]][5,population,],col=\"grey\", cex=0.2)")
      file_name <- paste0(R$datez, "_", R$run_name, "_AC_parent_f_pop", population, ".tiff")
      minY <- mins_n_maxes[4,population,1]
      maxY <- mins_n_maxes[4,population,2]
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(meanz, xlab = "Timestep", ylab = paste0("Pop ", population, " Mother AC"),cex=0.2, ylim=c(minY, maxY))
      axis(side = 1, at = c(which((1:P$num_timesteps)%%100==0)), labels = which((1:P$num_timesteps)%%100==0))
      eval(parse(text=stuff))
      dev.off()
      
      meanz <- cursitylist[[11]][6,population,]
      stuff <- paste0("points(cursitylist[[", 1:number_of_runs, "]][6,population,],col=\"grey\", cex=0.2)")
      file_name <- paste0(R$datez, "_", R$run_name, "_AC_offspring_m_pop", population, ".tiff")
      minY <- mins_n_maxes[5,population,1]
      maxY <- mins_n_maxes[5,population,2]
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(meanz, xlab = "Timestep", ylab = paste0("Pop ", population, " Son AC"),cex=0.2, ylim=c(minY, maxY))
      axis(side = 1, at = c(which((1:P$num_timesteps)%%100==0)), labels = which((1:P$num_timesteps)%%100==0))
      eval(parse(text=stuff))
      dev.off()
      
      meanz <- cursitylist[[11]][7,population,]
      stuff <- paste0("points(cursitylist[[", 1:number_of_runs, "]][7,population,],col=\"grey\", cex=0.2)")
      file_name <- paste0(R$datez, "_", R$run_name, "_AC_offspring_f_pop", population, ".tiff")
      minY <- mins_n_maxes[6,population,1]
      maxY <- mins_n_maxes[6,population,2]
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(meanz, xlab = "Timestep", ylab = paste0("Pop ", population, " Daughter AC"),cex=0.2, ylim=c(minY, maxY))
      axis(side = 1, at = c(which((1:P$num_timesteps)%%100==0)), labels = which((1:P$num_timesteps)%%100==0))
      eval(parse(text=stuff))
      dev.off()
      
      meanz <- cursitylist[[11]][8,population,]
      stuff <- paste0("points(cursitylist[[", 1:number_of_runs, "]][8,population,],col=\"grey\", cex=0.2)")
      file_name <- paste0(R$datez, "_", R$run_name, "_AC_replaced_m_pop", population, ".tiff")
      minY <- mins_n_maxes[7,population,1]
      maxY <- mins_n_maxes[7,population,2]
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(meanz, xlab = "Timestep", ylab = paste0("Pop ", population, " Dead Man AC"),cex=0.2, ylim=c(minY, maxY))
      axis(side = 1, at = c(which((1:P$num_timesteps)%%100==0)), labels = which((1:P$num_timesteps)%%100==0))
      eval(parse(text=stuff))
      dev.off()
      
      meanz <- cursitylist[[11]][9,population,]
      stuff <- paste0("points(cursitylist[[", 1:number_of_runs, "]][9,population,],col=\"grey\", cex=0.2)")
      file_name <- paste0(R$datez, "_", R$run_name, "_AC_replaced_f_pop", population, ".tiff")
      minY <- mins_n_maxes[8,population,1]
      maxY <- mins_n_maxes[8,population,2]
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(meanz, xlab = "Timestep", ylab = paste0("Pop ", population, " Dead Woman AC"),cex=0.2, ylim=c(minY, maxY))
      axis(side = 1, at = c(which((1:P$num_timesteps)%%100==0)), labels = which((1:P$num_timesteps)%%100==0))
      eval(parse(text=stuff))
      dev.off()
      
      meanz <- cursitylist[[11]][11,population,]
      stuff <- paste0("points(cursitylist[[", 1:number_of_runs, "]][11,population,],col=\"grey\", cex=0.2)")
      file_name <- paste0(R$datez, "_", R$run_name, "_cur_inh_attempts", population, ".tiff")
      minY <- mins_n_maxes[9,population,1]
      maxY <- mins_n_maxes[9,population,2]
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(meanz, xlab = "Timestep", ylab = paste0("Pop ", population, " Cur Inh Attempts"),cex=0.2, ylim=c(minY, maxY))
      axis(side = 1, at = c(which((1:P$num_timesteps)%%100==0)), labels = which((1:P$num_timesteps)%%100==0))
      eval(parse(text=stuff))
      dev.off()
      
      
      
      #selection_tiff <- paste0("tiff(filename = ", file_name, ", width = 554, height = 467, units = \"px\", pointsize = 12, bg = \"white\", compression = \"none\")")
      #selection_plot <- paste0("plot(objectz[], xlab = \"Timesteps\", ylab = paste0(\"Pop \",", population, ", \"Select Chances\"))")
      #close_out_port <- paste0("dev.off()")
      #eval(parse(text = c(selection_tiff, selection_plot, close_out_port)))
      for(sex in 1:2) {
        
        meanz <- sylrepzlist[[11]][sex,population,]
        stuff <- paste0("points(sylrepzlist[[", 1:number_of_runs, "]][sex,population,],col=\"grey\", cex=0.2)")
        file_name <- paste0(R$datez, "_", R$run_name, "_mean_repertoire_size_-_pop_", population, "_", R$sexes[sex], "s.tiff")
        minY <- mins_n_maxes[(sex + 10),population,1]
        maxY <- mins_n_maxes[(sex + 10),population,2]
        tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
        plot(meanz, xlab = "Timestep", ylab = paste0("Pop ", population, " ", R$Sexes[sex], "s - Mean Repertoire Size"),cex=0.2, ylim=c(minY, maxY))
        axis(side = 1, at = c(which((1:P$num_timesteps)%%100==0)), labels = which((1:P$num_timesteps)%%100==0))
        eval(parse(text=stuff))
        dev.off()
        
        meanz <- cursitylist[[11]][sex,population,]
        stuff <- paste0("points(cursitylist[[", 1:number_of_runs, "]][sex,population,],col=\"grey\", cex=0.2)")
        file_name <- paste0(R$datez, "_", R$run_name, "_mean_curiosity_-_pop_", population, "_", R$sexes[sex], "s.tiff")
        minY <- mins_n_maxes[(sex + 12),population,1]
        maxY <- mins_n_maxes[(sex + 12),population,2]
        tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
        plot(meanz, xlab = "Timestep", ylab = paste0("Pop ", population, " ", R$Sexes[sex], "s - Mean Curiosity"),cex=0.2, ylim=c(minY, maxY), axes=F)
        axis(side = 1, at = c(which((1:P$num_timesteps)%%100==0)), labels = which((1:P$num_timesteps)%%100==0))
        axis(2, tck=-0.05, at=c(seq.int(0,1,0.1)),labels=c(seq.int(0,1,0.1)*20), col.axis="black", las=1.5)
        minor.tick(nx=4, ny=3, tick.ratio=1, x.args = list(), y.args = list())
        eval(parse(text=stuff))
        dev.off()
        
        #image(t(objectz), col = R$sylnum_palette(100), xlab = "Timestep", ylab = paste0(ylab1, population, " ", R$Sexes[sex], ylab2))
        
        #meansyldist <- (sdstbxnlist[[11]] * 1000) + sdstbxnlist[[10]]
        #cat(rep("\"#3498db\"",10),sep=", ")
        # #sylsub_palette <- colorRampPalette(c("darkgreen","lawngreen","grey90","orchid1","orchid4"))
        #sylsub_palette <- colorRampPalette(c("#5b2c6f", "#abebc6"))
        #plot(rep(1,100),col=sylnum_palette(100),pch=19,cex=3)
        #cat(rep("\"#3498db\"",10),sep=", ")
        
        #### Define each vector before interlacing them; 200 positions between red and yellow, 200 positions between black and white. Latter based around the local dominant color? 
        
        #mean_spectrum <- colorRampPalette(c("red","yellow"))
        #mean_spectrum <- mean_spectrum(200)
        #individual_spectrum <- array(0,c(200,200))
        #individual_spectrum <- vector()
        #for(i in 1:200) {
          #individual_spectrum[i,] <- colorRampPalette(c("dark grey", mean_spectrum[i]))(200)
        #}
        #combo_palette <- as.vector(individual_spectrum)
        
        
        #> whatever <- colorRamp(c("grey","red"),bias = 10000)
        #> whatever(seq.int(0,1,0.5))
        #[,1] [,2] [,3]
        #[1,] 190.0  190  190
        #[2,] 222.5   95   95
        #[3,] 255.0    0    0
        
        #> whatever(seq.int(0,1,0.5))[2,]
        #[1] 222.5  95.0  95.0
        #> as.hexmode(round(whatever(seq.int(0,1,0.5))[2,]))
        #[1] "de" "5f" "5f"
        
        #hexholder <- c(0,0,0)
        #hexholder[1] <- as.hexmode(round(whatever(seq.int(0,1,0.5))[2,]))[1]
        #hexholder[2] <- as.hexmode(round(whatever(seq.int(0,1,0.5))[2,]))[2]
        #hexholder[3] <- as.hexmode(round(whatever(seq.int(0,1,0.5))[2,]))[3]
        
        #> paste0("#",as.hexmode(hexholder)[1],as.hexmode(hexholder)[2],as.hexmode(hexholder)[3])
        #[1] "#de5f5f"
        ### Proof that it worked:
        #> whatever <- colorRamp(c("#de5f5f","red"),bias = 10000)
        #> whatever(seq.int(0,1,0.5))
        #[,1] [,2] [,3]
        #[1,] 222.0 95.0 95.0
        #[2,] 238.5 47.5 47.5
        #[3,] 255.0  0.0  0.0
        
        meanz <- sdstbxnlist[[11]][(sex + ((population - 1) * 2)), ,]
        #stuff <- paste0("points(sdstbxnlist[[", 1:number_of_runs, "]][(sex + ((population - 1) * 2)), ,],col=\"grey\", cex=0.2)")
        file_name <- paste0(R$datez, "_", R$run_name, "_sylnum_pop_", population, "_", R$sexes[sex], "s.tiff")
        tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
        #plot(meanz, xlab = "Timestep", ylab = paste0("Pop ", population, " ", R$Sexes[sex], "s Sylnum"),cex=0.1)
        image(t(meanz), col = R$sylnum_palette(100), xlab = "Timestep", ylab = paste0("Pop ", population, " ", R$Sexes[sex], "s Sylnum"), axes=F)
        axis(1, tck=-0.05, at=c(seq.int(0,1,0.1)),labels=c(seq.int(0,1,0.1)*P$num_timesteps), col.axis="black", las=2)
        axis(2, tck=-0.05, at=c(seq.int(0,1,(1/12))),labels=c(seq.int(0,1,(1/12))*156), col.axis="black", las=2)
        minor.tick(nx=4, ny=4.8, tick.ratio=1, x.args = list(), y.args = list())
        #eval(parse(text=stuff))
        dev.off()
        
        #image(t(meanz), col = R$sylsub_palette(100), xlab = "Timestep", ylab = paste0(ylab1, population, " ", R$Sexes[sex], ylab2))
        
        meanz <- curhistlist[[11]][(sex + ((population - 1) * 2)), ,]
        #stuff <- paste0("points(curhistlist[[", 1:number_of_runs, "]][(sex + ((population - 1) * 2)), ,],col=\"grey\", cex=0.2)")
        file_name <- paste0(R$datez, "_", R$run_name, "_curiosity_bins_pop_", population, "_", R$sexes[sex], "s.tiff")
        tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
        #plot(meanz, xlab = "Timestep", ylab = paste0("Pop ", population, " ", R$Sexes[sex], "s Curiosity Bin"),cex=0.1)
        image(t(meanz), col = R$sylsub_palette(100), xlab = "Timestep", ylab = paste0("Pop ", population, " ", R$Sexes[sex], "s Curiosity Bin"), axes=F)
        axis(1, tck=-0.05, at=c(seq.int(0,1,0.1)),labels=c(seq.int(0,1,0.1)*P$num_timesteps), col.axis="black", las=2)
        axis(2, tck=-0.05, at=c(seq.int(0,1,0.1)),labels=c(seq.int(0,1,0.1)*20), col.axis="black", las=1.5)
        minor.tick(nx=4, ny=4, tick.ratio=1, x.args = list(), y.args = list())
        #eval(parse(text=stuff))
        dev.off()
        
        
        sink(file = paste0(R$datez, R$run_name, "_Summary_Statistics"), append = TRUE)
        print(paste0("pop ", population, " ", R$Sexes[sex], " rep size - avg over last 100 timesteps"))
        print(mean(sylrepzlist[[11]][sex, population, ((P$num_timesteps - 100):P$num_timesteps)]))
        print(paste0("pop ", population, " ", R$Sexes[sex], " rep size - avg over last 500 timesteps"))
        print(mean(sylrepzlist[[11]][sex, population, ((P$num_timesteps - 500):P$num_timesteps)]))
        print(paste0("pop ", population, " ", R$Sexes[sex], " curiosity - avg over last 100 timesteps"))
        print(mean(cursitylist[[11]][sex, population, ((P$num_timesteps - 100):P$num_timesteps)]))
        print(paste0("pop ", population, " ", R$Sexes[sex], " curiosity - avg over last 500 timesteps"))
        print(mean(cursitylist[[11]][sex, population, ((P$num_timesteps - 500):P$num_timesteps)]))
        sink() 
      }
    }
  }
}

full_plots <- function(R = R, Q = converted_data, extra_lines = FALSE) {
  if(extra_lines == FALSE) {
    for(population in 1:P$num_pop) {
    
      figure_maker(P, Q, R, population, "cursity", "3", "_mate_selections_pop", F, F, "Pop", " Selection Chances")
      figure_maker(P, Q, R, population, "cursity", "10", "_tutor_selections_pop", F, F, "Pop", " Selection Chances")
      figure_maker(P, Q, R, population, "cursity", "4", "_AC_parent_m_pop", F, F, "Pop", " Father AC")
      figure_maker(P, Q, R, population, "cursity", "5", "_AC_parent_f_pop", F, F, "Pop", " Mother AC")
      figure_maker(P, Q, R, population, "cursity", "6", "_AC_offspring_m_pop", F, F, "Pop", " Son AC")
      figure_maker(P, Q, R, population, "cursity", "7", "_AC_offspring_f_pop", F, F, "Pop", " Daughter AC")
      figure_maker(P, Q, R, population, "cursity", "8", "_AC_replaced_m_pop", F, F, "Pop", " Dead Man AC")
      figure_maker(P, Q, R, population, "cursity", "9", "_AC_replaced_f_pop", F, F, "Pop", " Dead Woman AC")
      figure_maker(P, Q, R, population, "cursity", "11", "_cur_inh_attempts", F, F, "Pop", " Cur Inh Attempts")
      
      figure_maker(P, Q, R, population, "sylrepz", "11", "_mean_repertoire_size_-_pop_", T, F, "Pop", "s - Mean Repertoire Size")
      figure_maker(P, Q, R, population, "cursity", "11", "_mean_curiosity_-_pop_", T, F, "Pop", "s - Mean Curiosity")
      figure_maker(P, Q, R, population, "sdstbxn", "11", "_sylnum_pop_", T, F, "Pop", "s Sylnum")
      figure_maker(P, Q, R, population, "curhist", "11", "_curiosity_bins_pop_", T, F, "Pop", "s Curiosity Bin")
    }
  } else {
    for(population in 1:P$num_pop) {
      
      objectz <- Q$cursity[3,population,]
      file_name <- paste0(R$datez, "_", R$run_name, "_mate_selections_pop", population, "_full.tiff")
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(objectz, xlab = "Timestep", ylab = paste0("Pop ", population, " Selection Chances"))
      dev.off()
      
      objectz <- Q$cursity[10,population,]
      file_name <- paste0(R$datez, "_", R$run_name, "_tutor_selections_pop", population, ".tiff")
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(objectz, xlab = "Timestep", ylab = paste0("Pop ", population, " Selection Chances"))
      dev.off()
      
      objectz <- Q$cursity[4,population,]
      file_name <- paste0(R$datez, "_", R$run_name, "_AC_parent_m_pop", population, ".tiff")
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(objectz, xlab = "Timestep", ylab = paste0("Pop ", population, " Father AC"))
      dev.off()
      
      objectz <- Q$cursity[5,population,]
      file_name <- paste0(R$datez, "_", R$run_name, "_AC_parent_f_pop", population, ".tiff")
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(objectz, xlab = "Timestep", ylab = paste0("Pop ", population, " Mother AC"))
      dev.off()
      
      objectz <- Q$cursity[6,population,]
      file_name <- paste0(R$datez, "_", R$run_name, "_AC_offspring_m_pop", population, ".tiff")
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(objectz, xlab = "Timestep", ylab = paste0("Pop ", population, " Son AC"))
      dev.off()
      
      objectz <- Q$cursity[7,population,]
      file_name <- paste0(R$datez, "_", R$run_name, "_AC_offspring_f_pop", population, ".tiff")
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(objectz, xlab = "Timestep", ylab = paste0("Pop ", population, " Daughter AC"))
      dev.off()
      
      objectz <- Q$cursity[8,population,]
      file_name <- paste0(R$datez, "_", R$run_name, "_AC_replaced_m_pop", population, ".tiff")
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(objectz, xlab = "Timestep", ylab = paste0("Pop ", population, " Dead Man AC"))
      dev.off()
      
      objectz <- Q$cursity[9,population,]
      file_name <- paste0(R$datez, "_", R$run_name, "_AC_replaced_f_pop", population, ".tiff")
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(objectz, xlab = "Timestep", ylab = paste0("Pop ", population, " Dead Woman AC"))
      dev.off()
      
      objectz <- Q$cursity[11,population,]
      file_name <- paste0(R$datez, "_", R$run_name, "_cur_inh_attempts", population, ".tiff")
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(objectz, xlab = "Timestep", ylab = paste0("Pop ", population, " Cur Inh Attempts"))
      dev.off()
      
      
      #selection_tiff <- paste0("tiff(filename = ", file_name, ", width = 554, height = 467, units = \"px\", pointsize = 12, bg = \"white\", compression = \"none\")")
      #selection_plot <- paste0("plot(objectz[], xlab = \"Timesteps\", ylab = paste0(\"Pop \",", population, ", \"Select Chances\"))")
      #close_out_port <- paste0("dev.off()")
      #eval(parse(text = c(selection_tiff, selection_plot, close_out_port)))
      for(sex in 1:2) {
        objectz <- Q$sylrepz[sex, population, ]
        file_name <- paste0(R$datez, "_", R$run_name, "_mean_repertoire_size_-_pop_", population, "_", R$sexes[sex], "s_full.tiff")
        tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
        plot(objectz, xlab = paste0("Timestep"), ylab = paste0("Pop ", population, " ", R$Sexes[sex], "s - Mean Repertoire Size"))
        dev.off()
        
        objectz <- Q$cursity[sex, population, ]
        file_name <- paste0(R$datez, "_", R$run_name, "_mean_curiosity_-_pop_", population, "_", R$sexes[sex], "s_full.tiff")
        tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
        plot(objectz, xlab = paste0("Timestep"), ylab = paste0("Pop ", population, " ", R$Sexes[sex], "s - Mean Curiosity"))
        dev.off()
        
        objectz <- Q$sdstbxn[(sex + ((population - 1) * 2)), , ]
        file_name <- paste0(R$datez, "_", R$run_name, "_sylnum_pop_", population, "_", R$sexes[sex], "s_full.tiff")
        tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
        image(t(objectz), col = R$sylnum_palette(100), xlab = paste0("Timestep"), ylab = paste0("Pop ", population, " ", R$Sexes[sex], "s Sylnum"))
        dev.off()
        
        objectz <- Q$curhist[(sex + ((population - 1) * 2)), , ]
        file_name <- paste0(R$datez, "_", R$run_name, "_curiosity_bins_pop_", population, "_", R$sexes[sex], "s_full.tiff")
        tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
        image(t(objectz), col = R$sylsub_palette(100), xlab = paste0("Timestep"), ylab = paste0("Pop ", population, " ", R$Sexes[sex], "s Curiosity Bin"))
        dev.off()
      }
    }
  }
}
