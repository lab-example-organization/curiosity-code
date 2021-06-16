# for use with the figure stitching script; stores the functions that do more of the back-end work

if (getwd() == "C:/Users/Parker/Documents/Lab_Notebook/projects/Code/figures-code") {
  source(file.path("..", "curiosity-code", "scripts", "Source_Reference_Section.R"))
} else {stop(print("Need to source referencesection function"))}

referencesection ("parallelHeatmaps")

mult_ImgAppend <- function (
        ...
    ) {
        arguments <- list (...)
        argsLength <- length (arguments)
        while(argsLength>2) {
            extraArgs <- arguments[-c (1,2)]
            appendedImg <- image_append (c (arguments[[1]], arguments[[2]]))
            arguments <- list (appendedImg, extraArgs)
            argsLength <- length (arguments)
        }
        appendedImg <- image_append (c (arguments[[1]], arguments[[2]]))
        return (appendedImg)
}


panel_labels <- function (
  row = 2,
  column = 1,
  label_style = "letters",
  charsize = 1
 ) {
  # run this after plot() to get text to show up around the panels! Currently only helps label the top-left with letters
  coords <- rep(0,row+column) # coordinates
  if (row == 1) {
    if (column == 3) {
      if (label_style == "letters") {
        row <- c(350,-120); column <- c(90,630,1200); coords <- c("A", "B", "C", "D", "E", "F") 
      }
    } else if (column == 2) {
      if (label_style == "letters") {
        row <- c(520,50); column <- c(75,630); coords <- c("A", "B", "C", "D")
      }
    } else if (column == 1) {
      if (label_style == "letters") {
        row <- c(54); column <- c(75,630); coords <- c("A", "B")
      }
    }
  } else if (row == 2) {
    if (column == 3) {
      if (label_style == "letters") {
        row <- c(350,-120); column <- c(90,630,1200); coords <- c("A", "B", "C", "D", "E", "F")
        #text(x = coords[3], y = coords[1], labels = "A", pos = c(3), offset = c(10), adj = c(0.5,0.5))
        #text(x = coords[4], y = coords[1], labels = "B", pos = c(3), offset = c(10), adj = c(0.5,0.5))
        #text(x = coords[5], y = coords[1], labels = "C", pos = c(3), offset = c(10), adj = c(0.5,0.5))
        #text(x = coords[3], y = coords[2], labels = "D", pos = c(3), offset = c(10), adj = c(0.5,0.5))
        #text(x = coords[4], y = coords[2], labels = "E", pos = c(3), offset = c(10), adj = c(0.5,0.5))
        #text(x = coords[5], y = coords[2], labels = "F", pos = c(3), offset = c(10), adj = c(0.5,0.5))  
      }
    } else if (column == 2) {
      if (label_style == "letters") {
        row <- c(520,50); column <- c(75,630); coords <- c("A", "B", "C", "D")
      }
    } else if (column == 1) {
      if (label_style == "letters") {
        row <- c(54); column <- c(75,630); coords <- c("A", "B")
      }
    }
  }
  for (i in 1:length(row)) {
    for (j in 1:length(column)) {
      text(x = column[j], y = row[i], labels = coords[j +2*(i-1)], pos = c(3), offset = c(10), adj = c(0.5,0.5), cex = charsize)
    }
  }
}




# "C:/Users/Parker/Documents/Lab_Notebook/projects/Code/figures-code"
# "..", "curiosity-code", "scripts", "Source_Reference_Section.R"
# "F:/results/tenKfiveByFive_parentNoInv/200110_3905_-_10k_nsL_normVO_oppsyl_1-8mp1_25-28fp1_8-11p2_c/multirun_output/recolorizedLineplots_lines/2020-01-10_200110_3905_-_10k_nsL_normVO_oppsyl_1-8mp1_25-28fp1_8-11p2_c_mean_curiosity_-_pop_1_females.png"
# "tenKfiveByFive_childBothHihInv"
picturefinder <- function (
  measurement, # mean_curiosity, mean_repertoire_size, sylnum_both_pops, sylnum_DO, sylnum_IO, sylnum (_sylnum_pop_), DO_sylnum_both_pops, IO_sylnum_both_pops ###curiosity_bins, AC_offspring, AC_parent, AC_replaced, cur_inh_attempts (1, 2), mate_selections (_pop1, _pop2), tutor_selections (_pop1, _pop2)
  population,
  sex,
  simnumber,
  tenk = "parentNoInv",
  drive = "c",# "f"
  workspace = "windows" # ubuntu?
 ) {
  population <- paste0("_pop_", population, "_")
  measurement <- paste0("_", measurement, "_")
  sex <- paste0("_", sex)
  
  #source correct drive; get into results folder
  if (workspace == "windows") {
    if (drive == "c") {
        drive_path <- file.path ("C:", "Users", "Parker", "Documents", "Lab_Notebook", "projects", "Code", "curiosity-code", "results")
    } else if (drive == "f") {drive_path <- file.path ("F:", "results")}

  } else {
    if (drive == "c") {
            drive_path <- file.path ("/", "mnt", "c", "Users", "Parker", "Documents", "Lab_Notebook", "projects", "Code", "curiosity-code", "results")
        } else if (drive == "f") {drive_path <- file.path ("/", "mnt", "f", "results")}
  }
  
  #get into sim folder
  if (tenk != FALSE) {
    sim_path <- file.path (drive_path, paste0 ("tenKfiveByFive_", tenk), list.files (file.path (drive_path, paste0 ("tenKfiveByFive_", tenk)), pattern = paste0 ("_", simnumber, "_")))
  } else {
    sim_path <- file.path (drive_path, list.files (file.path (drive_path), pattern = paste0 ("_", simnumber, "_")))
  }
  
  #length(list.files(file.path("results", "210511_10909_-_10k_nsL_normVO_oppsyl_8-11_c", "multirun_output", "recolorizedLineplots")))==0
  #if (length(list.files(file.path (sim_path, "multirun_output"), pattern = "nsL_normVO_oppsyl")) == 0) {
    if (length(grep(list.dirs (file.path (sim_path, "multirun_output")), pattern = "recolorized")) != 0) {
      recolorized_folders <- list.dirs (file.path (sim_path, "multirun_output"))[grep(list.dirs (file.path (sim_path, "multirun_output")), pattern = "recolorized")]
      
      if (length(grep(list.dirs (file.path (sim_path, "multirun_output")), pattern = "recolorized")) > 1) {
        pics_folders <- sapply (recolorized_folders, function(i) list.files(i))
        pics_folders <- sapply(pics_folders, function(i) length(i))
        pics_folder <- recolorized_folders[which(as.numeric(pics_folders) == max (as.numeric(pics_folders)))]
      } else {
        pics_folder <- recolorized_folders
      }
    } else if (length(list.dirs (file.path (sim_path, "multirun_output"), pattern = "multirun-output")) != 0) {
      # m_output_folders
      stop ("recolorized didn't work; write the rest of multirun-output")
      if (length(list.files (file.path (sim_path, "multirun_output"), pattern = "multirun-output")) != 0) {
      }
      pics_folder <- stuff
        
    } #else  #else if (length(list.files (file.path (sim_path, "multirun_output"), pattern = "multirun-output")) != 0) {
      #
      #pics_folder <-
    #}
  #} else {
  #  nsL_normVO_oppsyl_files <- list.files (file.path (sim_path, "multirun_output"), pattern = "multirun-output")
    
  #}
  nsL_normVO_oppsyl_files <- list.files (file.path (pics_folder))
  nsL_normVO_oppsyl_files <- nsL_normVO_oppsyl_files[grep(measurement, nsL_normVO_oppsyl_files)]
  nsL_normVO_oppsyl_files <- nsL_normVO_oppsyl_files[grep(population, nsL_normVO_oppsyl_files)]
  nsL_normVO_oppsyl_files <- nsL_normVO_oppsyl_files[grep(sex, nsL_normVO_oppsyl_files)]
  
  
  #multirun_path <- file.path (sim_path, "multirun_output", )
  
  picture_path <- file.path (pics_folder, nsL_normVO_oppsyl_files)
    
  
  return (picture_path)
}

# nsL_normVO_oppsyl
# list.dirs(file.path("..", "curiosity-code", "results", "", "multirun_output"))

thing <- c(1:4)
measurement_container <- "mean_curiosity"
sex_container <- c("females", "males", "females", "males")
population_container <- c(1, 1, 2, 2)
simnumber_pattern <- c (7, 10, 22, 25, 32, 35, 47, 50)
simnumber_chunks <- c(3900, 3950, 4000, 4050)
simnumber_container <- c(
  c(simnumber_chunks[1] + simnumber_pattern), 
  c(simnumber_chunks[2] + simnumber_pattern), 
  c(simnumber_chunks[3] + simnumber_pattern), 
  c(simnumber_chunks[4] + simnumber_pattern))
inheritance_pattern_names <- c("paternal", "maternal", "same-sex", "shared")
wsl_workaround <- file.path("/", "mnt", "c", "Users", "Parker", "Documents", "Lab_Notebook", "projects", "Code", "figures-code")
tenk_label <- "parentNoInv"
dir.create(file.path(wsl_workaround, tenk_label))
for (x in 1:length(simnumber_container)) {
  picture_paths <- c()
  for (
    i in 1: length(thing)
  ) {
    picture_paths[i] <- image_read(picturefinder(
      measurement = measurement_container,
      population = population_container[i],
      sex = sex_container[i],
      simnumber = simnumber_container[x],
      tenk = tenk_label,
      drive = "c",
      workspace = "ubuntu"
    ))
  }
  
  row1 <- mult_ImgAppend(picture_paths[1], picture_paths[2])
  row2 <-mult_ImgAppend(picture_paths[3], picture_paths[4])
  
  last_step <- image_append(c(row1, row2), stack = TRUE)
  simnumber_detail <- list.files (file.path ("C:", "Users", "Parker", "Documents", "Lab_Notebook", "projects", "Code", "curiosity-code", "results", paste0 ("tenKfiveByFive_", tenk_label)), pattern = paste0 ("_", simnumber, "_"))
  simnumber_detail <- strsplit(simnumber_detail, "oppsyl_")[[1]][2]
  image_write(last_step, file.path(wsl_workaround, tenk_label, paste0(inheritance_pattern_names[x], "_inh_", simnumber_detail, ".png")))
  
}