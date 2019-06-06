# Heatmap Directory Creation and Referencing

remakeString <- function(target, comp, out) {
  # tR stands for temporary retainer
  tR <- strsplit(target, comp)
  size <- length(target)
  remadeStrings <- target
  for(x in 1:size) {
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

HtMpDir <- function(extraDir = "extraDirectory") {
  
  heatmapDirectory <- file.path("results", "Heatmaps")
  if(exists("extraDir")) {
    for (sepDirs in 1:length(extraDir)) {
      heatmapDirectory <- file.path(heatmapDirectory, extraDir[sepDirs])
      
      if(!(dir.exists(file.path(heatmapDirectory)))) {
        dir.create(file.path(heatmapDirectory))
      }
    }
  }

  return(heatmapDirectory)
  
}

# heatmapLand <- HtMpDir("extraDirectory")


extractVarDirs <- function(home_path, fileNamePattern) {
  variableStore_folderList <- list.files(file.path(home_path), pattern = fileNamePattern)
  # list.files(file.path(home_path), pattern = fileNamePattern)
  
  return(variableStore_folderList)
}

extractMeans <- function(allRunDirs, dirHeatMap, source_of_params, deeper = FALSE) {
  number_of_runs <- length(allRunDirs)
  number_of_reps <- length(list.files(file.path(dirHeatMap, allRunDirs[1], "variable_store")))
  dim_source = yaml.load_file(file.path("parameters", source_of_params))
  
  RunMeans <- list()

  for(individual_run in 1:number_of_runs) {
    
    #individual_run <- 1
    if (deeper) {
      multirun_directory <-
      file.path(
        dirHeatMap, allRunDirs[individual_run], "multirun_output", 
      list.files(path = file.path(
        dirHeatMap, allRunDirs[individual_run], "multirun_output"), 
        pattern = "output$")
      )
    } else {
      multirun_directory <-
      file.path(dirHeatMap, allRunDirs[individual_run], "multirun_output"#, 
      # list.files(path = file.path(dirHeatMap, 
      # allRunDirs[individual_run], "multirun_output"), pattern = "output$")
      )
    }
    
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

makeHeatmapFile <- function (
  inheritance = 3,
  diffcurstartBias = 1,
  absolute = TRUE,
  reDo = FALSE,
  hoominReadable = FALSE,
  specialFigs = FALSE,
  lmhVnw = TRUE,
  highRes = FALSE,
  extractedMeans = extractedMeans
) {

  whichInh <- c("male","moth","same","opps","sNTn",
                "sSTf","sSFr","sFrS","sTfS","sTnN", "FfFf")

  whichBias <- c("male","female", "pop1", "pop2", "both")

  whichLmhVnw <- c("lowMedHigh", "narrowWide")
  if (lmhVnw) {SpecFigTypeLmhNw = whichLmhVnw[1]} else {SpecFigTypeLmhNw = whichLmhVnw[2]}

  lenF = 5

  if (reDo) {

    folderName <- list.files(path = file.path("results", "Heatmaps",
    "output_objects"), pattern = paste0("_slices_-_", whichInh[inheritance],
          "inh_", whichBias[diffcurstartBias], "Bias"))
          # "inh_", SpecFigTypeLmhNw, "Bias"))

    heatmap_array <- readRDS(file.path("results", "Heatmaps", "output_objects", 
      folderName, list.files(path = file.path("results", "Heatmaps", "output_objects",
        folderName), pattern = 
          "heatmap_output_-_")
      )
    )
    
  } else {
    if (!highRes) {
      folderName <- paste0(
      str_sub(paste(str_extract_all(
        Sys.time(), "[0123456789]"
      )[[1]], collapse = ""), 3, 8),
      "_slices_-_",
      whichInh[inheritance],
      "inh_",
      whichBias[diffcurstartBias],
      "Bias")

      if (diffcurstartBias == 1) {
        heatmap_array <- array(
        0, dim = c(5,5,5,8), list(
          c("1-7mp1", "7-13mp1", "11-26mp1", "1-26mp1", "11-15mp1"), 
          c("1-7mp2", "7-13mp2", "11-26mp2", "1-26mp2", "11-15mp2"), 
          c("1-7f", "7-13f", "11-26f", "1-26f", "11-15f"), 
          c("endcurm1","endcurm2","endcurf1","endcurf2","endrepm1","endrepm2","endrepf1","endrepf2")
        ))
        longLenF <- 5
      } else if (diffcurstartBias == 2) {
        heatmap_array <- array(
        0, dim = c(5,5,5,8), list(
          c("1-7fp1", "7-13fp1", "11-26fp1", "1-26fp1", "11-15fp1"), 
          c("1-7fp2", "7-13fp2", "11-26fp2", "1-26fp2", "11-15fp2"), 
          c("1-7m", "7-13m", "11-26m", "1-26m", "11-15m"), 
          c("endcurm1","endcurm2","endcurf1","endcurf2","endrepm1","endrepm2","endrepf1","endrepf2")
        ))
        longLenF <- 5
      } else if (diffcurstartBias == 3) {
        longLenF <- 2
        if (specialFigs) {
          if (lmhVnw) {
            lenF = 3
            heatmap_array <- array(
              0, dim = c(3,3,2,8), list(
                c("1-7mp1", "7-13mp1", "11-26mp1"), 
                c("1-7fp1", "7-13fp1", "11-26fp1"), 
                c("1-7p2", "11-26p2"), 
                c("endcurm1","endcurm2","endcurf1","endcurf2","endrepm1","endrepm2","endrepf1","endrepf2")
              )
            )
          } else {
            lenF = 2
            heatmap_array <- array(
            0, dim = c(2,2,2,8), list(
              c("1-26mp1", "11-15mp1"), 
              c("1-26fp1", "11-15fp1"), 
              c("1-7p2", "11-26p2"), 
              c("endcurm1","endcurm2","endcurf1","endcurf2","endrepm1","endrepm2","endrepf1","endrepf2")
            ))
          }
        }
      } else if (diffcurstartBias == 4) {
        longLenF <- 2
        if (specialFigs) {
          if (lmhVnw) {
            lenF = 3
            heatmap_array <- array(
              0, dim = c(2,3,3,8), list(
                c("1-7p1", "11-26p1"), 
                c("1-7mp2", "7-13mp2", "11-26mp2"), 
                c("1-7fp2", "7-13fp2", "11-26fp2"), 
                c("endcurm1","endcurm2","endcurf1","endcurf2","endrepm1","endrepm2","endrepf1","endrepf2")
              )
            )
          } else {
            lenF = 2
            heatmap_array <- array(
            0, dim = c(2,2,2,8), list(
              c("1-7p1", "11-26p1"), 
              c("1-26mp2", "11-15mp2"), 
              c("1-26fp2", "11-15fp2"), 
              c("endcurm1","endcurm2","endcurf1","endcurf2","endrepm1","endrepm2","endrepf1","endrepf2")
            ))
          }
        }
      }
    } else {
      lenF = 10
      longLenF = 1
      heatmap_array <- array(
            0, dim = c(10,10,1,8), list(
              c("1-7mp1", "4-10mp1", "7-13mp1", "10-15mp1", "13-19mp1", "15-23mp1", "19-26mp1", "23-29mp1", "26-31mp1", "29-34mp1"), 
              c("1-7fp1", "4-10fp1", "7-13fp1", "10-15fp1", "13-19fp1", "15-23fp1", "19-26fp1", "23-29fp1", "26-31fp1", "29-34fp1"), 
              # c("7-10p2", "26-29fp2"), 
              c("7-10p2"), 
              c("endcurm1","endcurm2","endcurf1","endcurf2","endrepm1","endrepm2","endrepf1","endrepf2")
            ))
    }
  }
    

  for(long in 1:longLenF) { # femalez
    for(medium in 1:lenF) { # malez1
      for(short in 1:lenF) { # malez2

        tally <- short + lenF*(medium - 1) + lenF*lenF*(long - 1)
        thing <- length(extractedMeans[[1]][[1]][1,1,])
        sumStats <- c(
          extractedMeans[[tally]]$curLvlMeans[1,1,thing],
          extractedMeans[[tally]]$curLvlMeans[1,2,thing],
          extractedMeans[[tally]]$curLvlMeans[2,1,thing],
          extractedMeans[[tally]]$curLvlMeans[2,2,thing],
          extractedMeans[[tally]]$sylRepMeans[1,1,thing],
          extractedMeans[[tally]]$sylRepMeans[1,2,thing],
          extractedMeans[[tally]]$sylRepMeans[2,1,thing],
          extractedMeans[[tally]]$sylRepMeans[2,2,thing]
        )

        heatmap_array[medium,short,long,] <- sumStats
        
      }
    }
  }

    # if(!(dir.exists(file.path("results", "Heatmaps", "output_objects", folderName)))) {
    #   dir.create(file.path("results", "Heatmaps", "output_objects", folderName))
    #   if(!(file.exists(file.path(
    #     "results", "Heatmaps", "output_objects", folderName, paste0("heatmap_output_-_", folderName, ".RData")
    #           )))) {saveRDS(heatmap_array, file.path(
    #     "results", "Heatmaps", "output_objects", folderName, paste0("heatmap_output_-_", folderName, ".RData")
    #   ))}
    #   for (subset in 1:5) {
    #     dir.create(file.path("results", "Heatmaps", "output_objects", folderName, paste0("slice_", subset)))
    #   }
    # }

    if(!(dir.exists(file.path("results", folderName)))) {

      dir.create(file.path("results", folderName))}
      
    if (specialFigs) {
      if(!(file.exists(file.path(
        "results", folderName, paste0("heatmap_output_-_", whichInh[inheritance], 
        "inh_", whichBias[diffcurstartBias], "Bias_", SpecFigTypeLmhNw, ".RData")
      )))) {
      
      saveRDS(heatmap_array, file.path(
        "results",folderName, paste0("heatmap_output_-_", whichInh[inheritance], 
        "inh_", whichBias[diffcurstartBias], "Bias_", SpecFigTypeLmhNw, ".RData")

      ))}
    } else {
      if(!(file.exists(file.path(
        "results", folderName, paste0("heatmap_output_-_", whichInh[inheritance], 
        "inh_", whichBias[diffcurstartBias], "Bias.RData")
      )))) {
      
      saveRDS(heatmap_array, file.path(
        "results",folderName, paste0("heatmap_output_-_", whichInh[inheritance], 
        "inh_", whichBias[diffcurstartBias], "Bias.RData")

      ))}
    }
    
    # for (subset in 1:5) {
    #   dir.create(file.path("results", folderName, paste0("slice_", subset)))
    # }
  
    
}

# }



#   #image(x = matrix(as.numeric(heatmap_array[,,1,1]),5,5),col =colorSeqMultPalette$PuBuGn(100), xlab = "")

 
#   # heatmap_array <- readRDS("../../../../../../media/parker/A443-E926/simulation runs/heatmap_output.RData")
#   colorSeqMultPalette <- list(
#     BuGn = colorRampPalette(c("#e5f5f9", "#99d8c9", "#2ca25f")), # 3-class BuGn
#     BuPu = colorRampPalette(c("#e0ecf4", "#9ebcda", "#8856a7")), # 3-class BuPu
#     GnBu = colorRampPalette(c("#e0f3db", "#a8ddb5", "#43a2ca")), # 3-class GnBu
#     OrRd = colorRampPalette(c("#fee8c8", "#fdbb84", "#e34a33")), # 3-class OrRd
#     PuBu = colorRampPalette(c("#ece7f2", "#a6bddb", "#2b8cbe")), # 3-class PuBu
#     PuBuGn = colorRampPalette(c("#ece2f0", "#a6bddb", "#1c9099")), # 3-class PuBuGn
#     PuRd = colorRampPalette(c("#e7e1ef", "#c994c7", "#dd1c77")), # 3-class PuRd
#     RdPu = colorRampPalette(c("#fde0dd", "#fa9fb5", "#c51b8a")), # 3-class RdPu
#     YlGn = colorRampPalette(c("#f7fcb9", "#addd8e", "#31a354")), # 3-class YlGn
#     YlGnBu = colorRampPalette(c("#edf8b1", "#7fcdbb", "#2c7fb8")), # 3-class YlGnBu
#     YlOrBr = colorRampPalette(c("#fff7bc", "#fec44f", "#d95f0e")), # 3-class YlOrBr
#     YlOrRd = colorRampPalette(c("#ffeda0", "#feb24c", "#f03b20")))

#   #image(x = matrix(as.numeric(heatmap_array[,,1,1]),5,5),col =colorSeqMultPalette$PuBuGn(100), xlab = "Pop 1 Male Curstart", ylab = "Pop 2 Male Curstart")

#   title_names <- c("Ending Curiosity Values - Pop 1 Males","Ending Curiosity Values - Pop 2 Males",
#                   "Ending Curiosity Values - Pop 1 Females","Ending Curiosity Values - Pop 2 Females",
#                   "Ending Syll Rept Values - Pop 1 Males","Ending Syll Rept Values - Pop 2 Males",
#                   "Ending Syll Rept Values - Pop 1 Females","Ending Syll Rept Values - Pop 2 Females")
#   # heatmap_categories <- c("cat(\"[,,1,1]\")","cat(\"[,1,,1]\")","cat(\"[1,,,1]\")")

#   if (
#     diffcurstartBias == 1
#   ) {
#     heatmap_axes <- list(
#       plotOne = c("Pop 2 Male Starting Curiosity", "Female Starting Curiosity"),    # mp2Vfem
#       plotTwo = c("Pop 1 Male Starting Curiosity", "Female Starting Curiosity"),    # mp1Vfem
#       plotTre = c("Pop 1 Male Starting Curiosity", "Pop 2 Male Starting Curiosity") # mp1Vmp2
#     )
#   } else {
#     heatmap_axes <- list(
#       mp2Vfem = c("Pop 2 Female Starting Curiosity", "Male Starting Curiosity"),
#       mp1Vfem = c("Pop 1 Female Starting Curiosity", "Male Starting Curiosity"),
#       mp1Vmp2 = c("Pop 1 Female Starting Curiosity", "Pop 2 Female Starting Curiosity")
#     )
#   }

#   # MALE PATTERN INHERITANCE (BIAS) (MORE DIFFERING FEMALE CURSTARTS)

#   # heatmap_axes <- list(
#   #   plotOne = c("Pop 2 Male Starting Curiosity", "Female Starting Curiosity"),    # mp2Vfem
#   #   plotTwo = c("Pop 1 Male Starting Curiosity", "Female Starting Curiosity"),    # mp1Vfem
#   #   plotTre = c("Pop 1 Male Starting Curiosity", "Pop 2 Male Starting Curiosity") # mp1Vmp2
#   # )


#   # FEMALE PATTERN INHERITANCE (BIAS) (MORE DIFFERING FEMALE CURSTARTS)

#   # heatmap_axes <- list(
#   #   mp2Vfem = c("Pop 2 Female Starting Curiosity", "Male Starting Curiosity"),
#   #   mp1Vfem = c("Pop 1 Female Starting Curiosity", "Male Starting Curiosity"),
#   #   mp1Vmp2 = c("Pop 1 Female Starting Curiosity", "Pop 2 Female Starting Curiosity")
#   # )

#   range_list <- array(data = c("Less Curiosity", "More Curiosity", "Seeks Similar Songs", "Seeks Novel Songs",
#                               "Low SylRep", "High Sylrep", "Limited Song Variety", "Highly Varied Song"), c(2,2,2))

#   # making the layout matrix that will be populated by the figures. Named because they're arranged by column; one could conceivably arrange them by row as well.
#   layoutDistribution <- c(0,0,1,3,1,3,1,3,1,
#                           3,1,3,1,3,1,3,1,3,
#                           2,0,2,4,2,4,2,4,2,
#                           5,2,5,2,5,2,5,0,0)
#   layoutSize <- length(layoutDistribution)
#   byTheCol <- vector("numeric", length = layoutSize*8)
#   for(i in 1:layoutSize) {
#     byTheCol[(1 + (i - 1)*8):(i*8)] <- rep(layoutDistribution[i], 8)
#   }



#   byTheCol <- c(rep(c(11,11,rep(1,4),rep( 2,4)),4),
#                 rep(c(11,11,rep(3,4),rep( 4,4)),4),
#                 rep(c(11,11,rep(5,4),rep( 6,4)),4),
#                 rep(c(11,11,rep(7,4),rep( 8,4)),4),
#                 rep(c(11,11,rep(9,4),rep(10,4)),4))
#   # layoutSize <- length(layoutDistribution)
#   # byTheCol <- vector("numeric", length = layoutSize)
#   # for(i in 1:layoutSize) {
#   #   byTheCol[(1 + (i - 1)*8):(i*8)] <- rep(layoutDistribution[i], 8)
#   # }

#   for (specificPlot in 1:3) {
#     file_name <- paste0(title_names[SxMtPop], "_slice_", slice, ".png")
#       # dimensions? dunno; not too worried though
      
#     png(filename = file.path("results", "Heatmaps", "output_objects", folderName, paste0("slice_", slice), file_name), width = 554, height = 554, units = "px", pointsize = 12, bg = "white")

#     layout(matrix(byTheCol,10,20,F))

#     for (slice in 1:5) {

#     }
#   }



#   legend_title <- c("Auditory Curiosity", "Syllable Repertoire")

#   # whichInh <- c(
#   #     "male",
#   #     "moth",
#   #     "same",
#   #     "opps",
#   #     "sNTn",
#   #     "sSTf",
#   #     "sSFr",
#   #     "sFrS",
#   #     "sTfS",
#   #     "sTnN"
#   # )

#   # whichBias <- c(
#   #   "male",
#   #   "female"
#   # )
#   # folderName <- paste0(
#   #   str_sub(paste(str_extract_all(
#   #     Sys.time(), "[0123456789]"
#   #   )[[1]], collapse = ""), 3, 8),
#   #   "_slices_-_",
#   #   whichInh[inheritance],
#   #   "inh_",
#   #   whichBias[diffcurstartBias],
#   #   "Bias"
#   # )

  
  
  
  

  

#   for(SxMtPop in 1:8) {
#     for (slice in 1:5) {
#         # Start to make the file ########### still need to fix the name so they don't overwrite one another ############
#       file_name <- paste0(title_names[SxMtPop], "_slice_", slice, ".png")
#         # dimensions? dunno; not too worried though
      
#       png(filename = file.path("results", "Heatmaps", "output_objects", folderName, paste0("slice_", slice), file_name), width = 554, height = 554, units = "px", pointsize = 12, bg = "white")
      
#       layout(matrix(byTheCol,16,18,F))
      
#       # The Fake one!

#       # plotNames <- array(c("heatmap_axes$plotOne[1]", "heatmap_axes$plotTwo[1]", "heatmap_axes$plotTre[1]", "heatmap_axes$plotOne[2]", "heatmap_axes$plotTwo[2]", "heatmap_axes$plotTre[2]")
      
#       for (htmpCycle in 1:3) {
        
#         dat_array_doh <- array(c(
#             rep(c(1, 1, 1, 1), 2), 1, 1, rep(c(5, 5, 5, 1), 2),
#             rep(c(2, 1, 1, 1), 2), 2, 2, rep(c(5, 5, 5, 2), 2),
#             rep(c(3, 1, 1, 1), 2), 3, 3, rep(c(5, 5, 5, 3), 2),
#             rep(c(4, 1, 1, 1), 2), 4, 4, rep(c(5, 5, 5, 4), 2),
#             rep(c(5, 1, 1, 1), 2), 5, 5, rep(c(5, 5, 5, 5), 2)
#           ), c(3,3,2,5))
        
#         if(absolute) {
#           if ("Curiosity" %in% str_split(title_names[SxMtPop], " ")[[1]]
#           ) {heatmapRange <- c(0,1)} else {heatmapRange <- c(1,100)}
#         } else {
          
#           heatmapRangeDatasetOne <- heatmap_array[
#             dat_array_doh[1,1,1,slice]:dat_array_doh[1,1,2,slice],
#             dat_array_doh[1,2,1,slice]:dat_array_doh[1,2,2,slice],
#             dat_array_doh[1,3,1,slice]:dat_array_doh[1,3,2,slice],
#             SxMtPop]
#           heatmapRangeDatasetTwo <- heatmap_array[
#             dat_array_doh[2,1,1,slice]:dat_array_doh[2,1,2,slice],
#             dat_array_doh[2,2,1,slice]:dat_array_doh[2,2,2,slice],
#             dat_array_doh[2,3,1,slice]:dat_array_doh[2,3,2,slice],
#             SxMtPop]
#           heatmapRangeDatasetTre <- heatmap_array[
#             dat_array_doh[3,1,1,slice]:dat_array_doh[3,1,2,slice],
#             dat_array_doh[3,2,1,slice]:dat_array_doh[3,2,2,slice],
#             dat_array_doh[3,3,1,slice]:dat_array_doh[3,3,2,slice],
#             SxMtPop]
#           heatmap_min <- c(
#             round(min(heatmapRangeDatasetOne), 2),
#             round(min(heatmapRangeDatasetTwo), 2),
#             round(min(heatmapRangeDatasetTre), 2)
#           )
#           heatmap_max <- c(
#             round(max(heatmapRangeDatasetOne), 2),
#             round(max(heatmapRangeDatasetTwo), 2),
#             round(max(heatmapRangeDatasetTre), 2)
#           )
          
#           heatmapRange <- c(heatmap_min[htmpCycle]-0.01,heatmap_max[htmpCycle]+0.01)
#         }
#         findXLab <- heatmap_axes[[htmpCycle]][1]
#         findYLab <- heatmap_axes[[htmpCycle]][2]
#         image(x = matrix(as.numeric(
#           heatmap_array[
#             dat_array_doh[htmpCycle,1,1,slice]:dat_array_doh[htmpCycle,1,2,slice],
#             dat_array_doh[htmpCycle,2,1,slice]:dat_array_doh[htmpCycle,2,2,slice],
#             dat_array_doh[htmpCycle,3,1,slice]:dat_array_doh[htmpCycle,3,2,slice],
#             SxMtPop
#           ]),5,5),
#         col = colorSeqMultPalette$YlOrBr(100),
#         axes = F, 
#         xlab = findXLab, 
#         ylab = findYLab,cex.lab=1.4, zlim = heatmapRange)
      
#         axis(1,c(-0.125,0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1,1.12),
#             c("","0-.25","", ".25-.5","", ".45-1","", "0-1","", ".45-.55",""),
#             T,0,NA,F,cex.axis=0.8, tck = 0)
#         axis(1,c(-0.125,0.125,0.375,0.625,0.875,1.125),
#             c("","","","","",""),
#             T,-0.03,NA,F,cex.axis=1, tck = -0.03)
        
#         axis(2,c(-0.125,0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1,1.12),
#             c("","0-.25","", ".25-.5","", ".45-1","", "0-1","", ".45-.55",""),
#             T,0,NA,F,cex.axis=0.6, tck = 0)
#         axis(2,c(-0.125,0.125,0.375,0.625,0.875,1.125),
#             c("","","","","",""),
#             T,-0.03,NA,F,cex.axis=1, tck = -0.03)
#       }
      
#       plot(matrix(c(rep(1,20),1:20),20,2),col=colorSeqMultPalette$YlOrBr(20),pch=15,cex=15, xlab = NA, ylab = NA, axes = F)
#       a <- 0.35; b <- 20.5; c <- (b-a)/10
#       axis(2, seq(a,b,c),c("","","","","","","","","","",""), line=0)
#       axis(2, c(4,17),c(range_list[1,1,ceiling(SxMtPop/4)],range_list[2,1,ceiling(SxMtPop/4)]), las=0,tck = 0, line = 0)
#       axis(4, c(1,10,19),c("min_val","mid_val","max_val"), las=1,tck = 0, lwd=0, line=0)
#       axis(4, c(17,18,19),c("min:","mid:","max:"), las=1,tck = 0, lwd=0, line=4)
#       if (absolute) {
#         if ("Curiosity" %in% str_split(title_names[SxMtPop], " ")[[1]]
#           ) {
#             axis(4, c(17,18,19,20),c("0","0.5","1", "All:"), las=1,tck = 0, lwd=0, line=6)
#           } else {
#             axis(4, c(17,18,19,20),c("1","50.5","100", "All:"), las=1,tck = 0, lwd=0, line=6)
#           }
        
#       } else {
#         axis(4, c(17,18,19,20),c(heatmap_min[1],round((heatmap_min[1]+heatmap_max[1])/2,2),heatmap_max[1], "d2s"), las=1,tck = 0, lwd=0, line=6)
#         axis(4, c(17,18,19,20),c(heatmap_min[2],round((heatmap_min[2]+heatmap_max[2])/2,2),heatmap_max[2], "d1s"), las=1,tck = 0, lwd=0, line=9)
#         axis(4, c(17,18,19,20),c(heatmap_min[3],round((heatmap_min[3]+heatmap_max[3])/2,2),heatmap_max[3], "d12"), las=1,tck = 0, lwd=0, line=12)
#       }
      
#       mtext(c(paste0(legend_title[ceiling(SxMtPop/4)],"    ")),3,2.2,cex=1) # the fecking spaces are for keeping text center-aligned
#       mtext("Seeks Novel Songs",3,1,cex = 0.8)
#       mtext(range_list[1,2,ceiling(SxMtPop/4)],1,0.7,cex = 0.8)
#       box("outer", "solid")
#       #mtext(paste0(title_names[SxMtPop], "                                  "),3,cex = 1.5,line=30)
#       par(mfrow=c(1,1))
#       dev.off()
#     }

#     # here's where the pasting-together of slices into a single line of figures would be very helpful

#   }
#   return(print("done, in the specified folder"))
# # }
  
# }
print("HtMpDir, extractVarDirs, remakeString, extractMeans and makeHeatmaps loaded")
