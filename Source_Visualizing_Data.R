#Data Recording objects
library(abind)
library(stringr)
setwd(FolderName)

rm(list = objects())

FolderName = readRDS(file = "harvest_info.RData")
P = readRDS(file = "parameters.RData")
thousand_timesteps = readRDS(file = "timestep_grps.RData")
#thousand_timesteps <- 5

convert_stored_data <- function(P = P, num_timechunks=thousand_timesteps) {
  dir <- getwd()
  names = c("sylrep_rowcol","sylrep_dstbxn","curity_mean_t","curity_repert")
  converted_names = c("sylrepz","sdstbxn","cursity","curhist")
  sylrepz <- array(0, c(2, P$num_pop, P$num_timesteps))
  sdstbxn <- array(0, c((2 * P$num_pop), P$sylnum, P$num_timesteps))
  cursity <- array(0, c(12, P$num_pop, P$num_timesteps))
  curhist <- array(data = 0, dim = c((2*P$num_pop), (P$num_pop * P$num_one.pop_singers_sampled[1]), P$num_timesteps))
  for(data_subset in 1:4) {
    data1s <- paste0(names[data_subset], "_", 1:num_timechunks, " <- readRDS(file = paste0(dir, \"/variable-store-\", ", 1:num_timechunks, ", \"-", names[data_subset], ".RData\"))")
    eval(parse(text=data1s))
    
    for(i in 1:num_timechunks) {
      data2s <- paste0(converted_names[data_subset], "[, , ((1 + ((", i, " - 1) * 1000)) : (", i, " * 1000))] <- ", names[data_subset], "_", i)
      eval(parse(text=data2s))
      
      data3s <- paste0("rm(", names[data_subset], "_", i, ")")
      eval(parse(text=data3s))
    }
  }
  converted_data <- list(sylrepz = sylrepz, sdstbxn = sdstbxn, cursity = cursity, curhist = curhist)
  return(converted_data)
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
  subset_of_the_data <- converted_data[[data_subset]]
  return(subset_of_the_data)
}

#sylrepz <- split_data(data_subset = 1)
#sdstbxn <- split_data(data_subset = 2)
#cursity <- split_data(data_subset = 3)
#curhist <- split_data(data_subset = 4)

create_plot_info <- function(datez = "180803", run_name = "initial_test_1") {
  datez = datez
  run_name = run_name
  #sylnum_palette <- colorRampPalette(c("darkblue","royalblue","skyblue","turquoise1","springgreen","gold","orangered","firebrick4"))
  sylnum_palette <- colorRampPalette(c("#641e16", "#943126", "#cb4335", "#f5b7b1", "#aed6f1", "#3498db"))
  #sylsub_palette <- colorRampPalette(c("darkgreen","lawngreen","grey90","orchid1","orchid4"))
  sylsub_palette <- colorRampPalette(c("#5b2c6f", "#abebc6"))
  
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



simple_plots <- function(R = R, Q = converted_data, simplification_factor = 10, extra_lines = FALSE) {
  if(extra_lines == FALSE) {
    for(population in 1:P$num_pop) {
    
      objectz <- Q$cursity[3,population,seq.int(1, P$num_timesteps, simplification_factor)]
      file_name <- paste0(R$datez, "_", R$run_name, "_mate_selections_pop", population, ".tiff")
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(objectz, xlab = "Timestep", ylab = paste0("Pop ", population, " Selection Chances"))
      points
      dev.off()
      
      objectz <- Q$cursity[10,population,seq.int(1, P$num_timesteps, simplification_factor)]
      file_name <- paste0(R$datez, "_", R$run_name, "_tutor_selections_pop", population, ".tiff")
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(objectz, xlab = "Timestep", ylab = paste0("Pop ", population, " Selection Chances"))
      dev.off()
      
      objectz <- Q$cursity[4,population,seq.int(1, P$num_timesteps, simplification_factor)]
      file_name <- paste0(R$datez, "_", R$run_name, "_AC_parent_m_pop", population, ".tiff")
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(objectz, xlab = "Timestep", ylab = paste0("Pop ", population, " Father AC"))
      dev.off()
      
      objectz <- Q$cursity[5,population,seq.int(1, P$num_timesteps, simplification_factor)]
      file_name <- paste0(R$datez, "_", R$run_name, "_AC_parent_f_pop", population, ".tiff")
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(objectz, xlab = "Timestep", ylab = paste0("Pop ", population, " Mother AC"))
      dev.off()
      
      objectz <- Q$cursity[6,population,seq.int(1, P$num_timesteps, simplification_factor)]
      file_name <- paste0(R$datez, "_", R$run_name, "_AC_offspring_m_pop", population, ".tiff")
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(objectz, xlab = "Timestep", ylab = paste0("Pop ", population, " Son AC"))
      dev.off()
      
      objectz <- Q$cursity[7,population,seq.int(1, P$num_timesteps, simplification_factor)]
      file_name <- paste0(R$datez, "_", R$run_name, "_AC_offspring_f_pop", population, ".tiff")
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(objectz, xlab = "Timestep", ylab = paste0("Pop ", population, " Daughter AC"))
      dev.off()
      
      objectz <- Q$cursity[8,population,seq.int(1, P$num_timesteps, simplification_factor)]
      file_name <- paste0(R$datez, "_", R$run_name, "_AC_replaced_m_pop", population, ".tiff")
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(objectz, xlab = "Timestep", ylab = paste0("Pop ", population, " Dead Man AC"))
      dev.off()
      
      objectz <- Q$cursity[9,population,seq.int(1, P$num_timesteps, simplification_factor)]
      file_name <- paste0(R$datez, "_", R$run_name, "_AC_replaced_f_pop", population, ".tiff")
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(objectz, xlab = "Timestep", ylab = paste0("Pop ", population, " Dead Woman AC"))
      dev.off()
      
      objectz <- Q$cursity[11,population,seq.int(1, P$num_timesteps, simplification_factor)]
      file_name <- paste0(R$datez, "_", R$run_name, "_cur_inh_attempts", population, ".tiff")
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(objectz, xlab = "Timestep", ylab = paste0("Pop ", population, " Cur Inh Attempts"))
      dev.off()
      
      
      #selection_tiff <- paste0("tiff(filename = ", file_name, ", width = 554, height = 467, units = \"px\", pointsize = 12, bg = \"white\", compression = \"none\")")
      #selection_plot <- paste0("plot(objectz[seq.int(1,", P$num_timesteps, " , ", simplification_factor, ")], xlab = \"Timesteps\", ylab = paste0(\"Pop \",", population, ", \"Select Chances\"))")
      #close_out_port <- paste0("dev.off()")
      #eval(parse(text = c(selection_tiff, selection_plot, close_out_port)))
      for(sex in 1:2) {
        
        objectz <- Q$sylrepz[sex,population,seq.int(1, P$num_timesteps, simplification_factor)]
        file_name <- paste0(R$datez, "_", R$run_name, "_mean_repertoire_size_-_pop_", population, "_", R$sexes[sex], "s.tiff")
        tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
        plot(objectz, xlab = paste0("Timestep x ", simplification_factor), ylab = paste0("Pop ", population, " ", R$Sexes[sex], "s - Mean Repertoire Size"))
        dev.off()
        
        objectz <- Q$cursity[sex,population,seq.int(1, P$num_timesteps, simplification_factor)]
        file_name <- paste0(R$datez, "_", R$run_name, "_mean_curiosity_-_pop_", population, "_", R$sexes[sex], "s.tiff")
        tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
        plot(objectz, xlab = paste0("Timestep x ", simplification_factor), ylab = paste0("Pop ", population, " ", R$Sexes[sex], "s - Mean Curiosity"))
        dev.off()
        
        objectz <- Q$sdstbxn[(sex + ((population - 1) * 2)), , seq.int(1, P$num_timesteps, simplification_factor)]
        file_name <- paste0(R$datez, "_", R$run_name, "_sylnum_pop_", population, "_", R$sexes[sex], ".tiff")
        tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
        image(t(objectz), col = R$sylnum_palette(100), xlab = paste0("Timestep x ", simplification_factor), ylab = paste0("Pop ", population, " ", R$Sexes[sex], "s Sylnum"))
        dev.off()
        
        objectz <- Q$curhist[(sex + ((population - 1) * 2)), , seq.int(1, P$num_timesteps, simplification_factor)]
        file_name <- paste0(R$datez, "_", R$run_name, "_curiosity_bins_pop_", population, "_", R$sexes[sex], ".tiff")
        tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
        image(t(objectz), col = R$sylsub_palette(100), xlab = paste0("Timestep x ", simplification_factor), ylab = paste0("Pop ", population, " ", R$Sexes[sex], "s Curiosity Bin"))
        dev.off()
        
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
  } else {
    for(population in 1:P$num_pop) {
      
      for()
      objectz <- Q$cursity[3,population,seq.int(1, P$num_timesteps, simplification_factor)]
      
      file_name <- paste0(R$datez, "_", R$run_name, "_mate_selections_pop", population, ".tiff")
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(objectz, xlab = "Timestep", ylab = paste0("Pop ", population, " Selection Chances"))
      dev.off()
      
      objectz <- Q$cursity[10,population,seq.int(1, P$num_timesteps, simplification_factor)]
      file_name <- paste0(R$datez, "_", R$run_name, "_tutor_selections_pop", population, ".tiff")
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(objectz, xlab = "Timestep", ylab = paste0("Pop ", population, " Selection Chances"))
      dev.off()
      
      objectz <- Q$cursity[4,population,seq.int(1, P$num_timesteps, simplification_factor)]
      file_name <- paste0(R$datez, "_", R$run_name, "_AC_parent_m_pop", population, ".tiff")
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(objectz, xlab = "Timestep", ylab = paste0("Pop ", population, " Father AC"))
      dev.off()
      
      objectz <- Q$cursity[5,population,seq.int(1, P$num_timesteps, simplification_factor)]
      file_name <- paste0(R$datez, "_", R$run_name, "_AC_parent_f_pop", population, ".tiff")
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(objectz, xlab = "Timestep", ylab = paste0("Pop ", population, " Mother AC"))
      dev.off()
      
      objectz <- Q$cursity[6,population,seq.int(1, P$num_timesteps, simplification_factor)]
      file_name <- paste0(R$datez, "_", R$run_name, "_AC_offspring_m_pop", population, ".tiff")
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(objectz, xlab = "Timestep", ylab = paste0("Pop ", population, " Son AC"))
      dev.off()
      
      objectz <- Q$cursity[7,population,seq.int(1, P$num_timesteps, simplification_factor)]
      file_name <- paste0(R$datez, "_", R$run_name, "_AC_offspring_f_pop", population, ".tiff")
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(objectz, xlab = "Timestep", ylab = paste0("Pop ", population, " Daughter AC"))
      dev.off()
      
      objectz <- Q$cursity[8,population,seq.int(1, P$num_timesteps, simplification_factor)]
      file_name <- paste0(R$datez, "_", R$run_name, "_AC_replaced_m_pop", population, ".tiff")
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(objectz, xlab = "Timestep", ylab = paste0("Pop ", population, " Dead Man AC"))
      dev.off()
      
      objectz <- Q$cursity[9,population,seq.int(1, P$num_timesteps, simplification_factor)]
      file_name <- paste0(R$datez, "_", R$run_name, "_AC_replaced_f_pop", population, ".tiff")
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(objectz, xlab = "Timestep", ylab = paste0("Pop ", population, " Dead Woman AC"))
      dev.off()
      
      objectz <- Q$cursity[11,population,seq.int(1, P$num_timesteps, simplification_factor)]
      file_name <- paste0(R$datez, "_", R$run_name, "_cur_inh_attempts", population, ".tiff")
      tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
      plot(objectz, xlab = "Timestep", ylab = paste0("Pop ", population, " Cur Inh Attempts"))
      dev.off()
      
      
      #selection_tiff <- paste0("tiff(filename = ", file_name, ", width = 554, height = 467, units = \"px\", pointsize = 12, bg = \"white\", compression = \"none\")")
      #selection_plot <- paste0("plot(objectz[seq.int(1,", P$num_timesteps, " , ", simplification_factor, ")], xlab = \"Timesteps\", ylab = paste0(\"Pop \",", population, ", \"Select Chances\"))")
      #close_out_port <- paste0("dev.off()")
      #eval(parse(text = c(selection_tiff, selection_plot, close_out_port)))
      for(sex in 1:2) {
        
        objectz <- Q$sylrepz[sex,population,seq.int(1, P$num_timesteps, simplification_factor)]
        file_name <- paste0(R$datez, "_", R$run_name, "_mean_repertoire_size_-_pop_", population, "_", R$sexes[sex], "s.tiff")
        tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
        plot(objectz, xlab = paste0("Timestep x ", simplification_factor), ylab = paste0("Pop ", population, " ", R$Sexes[sex], "s - Mean Repertoire Size"))
        dev.off()
        
        objectz <- Q$cursity[sex,population,seq.int(1, P$num_timesteps, simplification_factor)]
        file_name <- paste0(R$datez, "_", R$run_name, "_mean_curiosity_-_pop_", population, "_", R$sexes[sex], "s.tiff")
        tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
        plot(objectz, xlab = paste0("Timestep x ", simplification_factor), ylab = paste0("Pop ", population, " ", R$Sexes[sex], "s - Mean Curiosity"))
        dev.off()
        
        objectz <- Q$sdstbxn[(sex + ((population - 1) * 2)), , seq.int(1, P$num_timesteps, simplification_factor)]
        file_name <- paste0(R$datez, "_", R$run_name, "_sylnum_pop_", population, "_", R$sexes[sex], ".tiff")
        tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
        image(t(objectz), col = R$sylnum_palette(100), xlab = paste0("Timestep x ", simplification_factor), ylab = paste0("Pop ", population, " ", R$Sexes[sex], "s Sylnum"))
        dev.off()
        
        objectz <- Q$curhist[(sex + ((population - 1) * 2)), , seq.int(1, P$num_timesteps, simplification_factor)]
        file_name <- paste0(R$datez, "_", R$run_name, "_curiosity_bins_pop_", population, "_", R$sexes[sex], ".tiff")
        tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
        image(t(objectz), col = R$sylsub_palette(100), xlab = paste0("Timestep x ", simplification_factor), ylab = paste0("Pop ", population, " ", R$Sexes[sex], "s Curiosity Bin"))
        dev.off()
        
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
  }
}

full_plots <- function(R = R, Q = converted_data, extra_lines = FALSE) {
  if(extra_lines == FALSE) {
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
      #selection_plot <- paste0("plot(objectz[seq.int(1,", P$num_timesteps, " , ", simplification_factor, ")], xlab = \"Timesteps\", ylab = paste0(\"Pop \",", population, ", \"Select Chances\"))")
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
      #selection_plot <- paste0("plot(objectz[seq.int(1,", P$num_timesteps, " , ", simplification_factor, ")], xlab = \"Timesteps\", ylab = paste0(\"Pop \",", population, ", \"Select Chances\"))")
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
