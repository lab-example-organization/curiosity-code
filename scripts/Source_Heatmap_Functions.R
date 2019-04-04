# Heatmap Directory Creation and Referencing

remakeString <- function(target, comp, out) {
  # tR stands for temporary retainer
  tR <- strsplit(target, comp)
  
  remadeStrings <- target
  for(x in 1:125) {
    if(is.na(tR[[x]][10])) {
        remadeStrings[x] <- paste("sim", tR[[x]][8], tR[[x]][9], sep = out)
    } else {
      if(is.na(tR[[x]][11])) {
        remadeStrings[x] <- paste("sim", tR[[x]][8], tR[[x]][9], tR[[x]][10], sep = out)
      } else {
        remadeStrings[x] <- paste("sim", tR[[x]][8], tR[[x]][9], tR[[x]][10], tR[[x]][11], sep = out)
      }
    }
  }
  
  remadeStrings <- str_replace_all(remadeStrings, "[-]", ".")

  return(remadeStrings)
}

HtMpDir <- function() {

  heatmaps_dir <- c("results", "Heatmaps")

  if(!(dir.exists(file.path(heatmaps_dir[1], heatmaps_dir[2])))) {
      dir.create(file.path(heatmaps_dir[1], heatmaps_dir[2]))
  }

  return(file.path(heatmaps_dir[1], heatmaps_dir[2]))
}

extractVarDirs <- function(home_path, fileNamePattern) {
  variableStore_folderList <- list.files(file.path(home_path), pattern = fileNamePattern)
  # list.files(file.path(home_path), pattern = fileNamePattern)
  
  return(variableStore_folderList)
}

extractMeans <- function(allRunDirs, dirHeatMap, source_of_params) {
  number_of_runs <- length(allRunDirs)
  number_of_reps <- length(list.files(file.path(dirHeatMap, allRunDirs[1], "variable_store")))
  dim_source = yaml.load_file(file.path("parameters", source_of_params))
  
  RunMeans <- list()

  for(individual_run in 1:number_of_runs) {
    
    #individual_run <- 1

    multirun_directory <-
      file.path(dirHeatMap, allRunDirs[individual_run], "multirun_output", 
      list.files(path = file.path(dirHeatMap, 
      allRunDirs[individual_run], "multirun_output"), pattern = "output$")
      )
    datanames <- c("CurHist","Cursity","SylDist","SylReps")
    objectnames <- c("curhist","cursity","sdstbxn","sylrepz")
    listnames <- c("hist","sity","sdst","repz")
    for(i in 1:4) {
      listlister <- paste0(listnames[i], "list <- vector(mode = \"character\", length = number_of_reps)")
      listmaker <- paste0(listnames[i], "list[", 1:number_of_reps, "] <- \"", datanames[i], 1:number_of_reps, ".RData\"")
      eval(parse(text=c(listlister, listmaker))) # fill up '[listnames]list' objects with calls to multirun csv files
    }

    timeSpanChunks <- 1000

    sylrepzlist <- array(0, c(2, dim_source$num_pop, timeSpanChunks, number_of_reps))
    sdstbxnlist <- array(0, c((2 * dim_source$num_pop), dim_source$sylnum, timeSpanChunks, number_of_reps))
    cursitylist <- array(0, c(12, dim_source$num_pop, timeSpanChunks, number_of_reps))
    curhistlist <- array(0, c((2*dim_source$num_pop), (dim_source$num_pop * dim_source$one_pop_singers[1]), timeSpanChunks, number_of_reps))

    for(i in 1:number_of_reps) {
      curhistlist[,,,i] <- readRDS(paste0(multirun_directory, "/", histlist[i]))
      cursitylist[,,,i] <- readRDS(paste0(multirun_directory, "/", sitylist[i]))
      sdstbxnlist[,,,i] <- readRDS(paste0(multirun_directory, "/", sdstlist[i]))
      sylrepzlist[,,,i] <- readRDS(paste0(multirun_directory, "/", repzlist[i]))

      # curhistlist[,,,i] <- fread(file.path(multirun_directory, histlist[i]))
      # cursitylist[,,,i] <- fread(file.path(multirun_directory, sitylist[i]))
      # sdstbxnlist[,,,i] <- fread(file.path(multirun_directory, sdstlist[i]))
      # sylrepzlist[,,,i] <- fread(file.path(multirun_directory, repzlist[i]))
    }
    
    # These four lines calculate the mean value across all the replicates

    curHstMeans <- colMeans(aperm(curhistlist, c(4, 1, 2, 3)), na.rm = TRUE)
    curLvlMeans <- colMeans(aperm(cursitylist, c(4, 1, 2, 3)), na.rm = TRUE)
    sylDbnMeans <- colMeans(aperm(sdstbxnlist, c(4, 1, 2, 3)), na.rm = TRUE)
    sylRepMeans <- colMeans(aperm(sylrepzlist, c(4, 1, 2, 3)), na.rm = TRUE)
    
    RunMeans[[individual_run]] <- list(
      sylRepMeans = sylRepMeans,
      sylDbnMeans = sylDbnMeans,
      curLvlMeans = curLvlMeans,
      curHstMeans = curHstMeans
    )
  }
  return(RunMeans)
}


print("HtMpDir, extractVarDirs, remakeString and extractMeans loaded")
