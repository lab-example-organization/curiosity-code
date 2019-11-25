# Heatmap Directory Creation and Referencing

remakestring <- function(target, comp, out) {
  # tR stands for temporary retainer
  tR <- strsplit(target, comp)
  size <- length(target)
  remadestrings <- target
  for(x in 1:size) {
    if(is.na(tR[[x]][10])) {
        remadestrings[x] <- paste("sim", tR[[x]][8], tR[[x]][9], sep = out)
    } else {
      if(is.na(tR[[x]][11])) {
        remadestrings[x] <- paste("sim", tR[[x]][8], tR[[x]][9], tR[[x]][10], sep = out)
      } else {
        remadestrings[x] <- paste("sim", tR[[x]][8], tR[[x]][9], tR[[x]][10], tR[[x]][11], sep = out)
      }
    }
  }

  remadestrings <- str_replace_all(remadestrings, "[-]", ".")

  return(remadestrings)
}

htmpdir <- function(extradir = "extraDirectory") {

  heatmapdirectory <- file.path("results", "Heatmaps")
  if(exists("extradir")) {
    for (sepdirs in 1:length(extradir)) {
      heatmapdirectory <- file.path(heatmapdirectory, extradir[sepdirs])

      if(!(dir.exists(file.path(heatmapdirectory)))) {
        dir.create(file.path(heatmapdirectory))
      }
    }
  }

  return(heatmapdirectory)

}

# heatmapLand <- htmpdir("extraDirectory")


extractvardirs <- function(home_path, filenamepattern) {
  variablestore_folderlist <- list.files(file.path(home_path), pattern = filenamepattern)
  # list.files(file.path(home_path), pattern = filenamepattern)

  return(variablestore_folderlist)
}

outputfilenames <- function (totalreplicates) {
  datanames <- c("CurHist","Cursity","SylDist","SylReps")
  objectnames <- c("curhist","cursity","sdstbxn","sylrepz")
  listnames <- c("hist","sity","sdst","repz")
  # filenamelist <- list()
  filenamelist <- list(
    histlist = vector(length = totalreplicates),
    sitylist = vector(length = totalreplicates),
    sdstlist = vector(length = totalreplicates),
    repzlist = vector(length = totalreplicates)
  )
  for (individualreplicates in 1:totalreplicates) {
      filenamelist$histlist[individualreplicates] <- paste0("CurHist", individualreplicates, ".RData")
      filenamelist$sitylist[individualreplicates] <- paste0("Cursity", individualreplicates, ".RData")
      filenamelist$sdstlist[individualreplicates] <- paste0("SylDist", individualreplicates, ".RData")
      filenamelist$repzlist[individualreplicates] <- paste0("SylReps", individualreplicates, ".RData")
  }
  return(filenamelist)
}

extractmeans <- function(allrundirs,
                         dirheatmap,
                         source_of_params,
                         ordering = FALSE,
                         deeper = FALSE) {
  if (ordering != FALSE && length(ordering) < 2) {
    stop("either the runs are ordered correctly for later heatmap building, or they have to be rearranged.
        So, if the order needs to be set up differently then you need a vector of numbers to tell it how to rearrange.")
  }
  number_of_runs <- length(allrundirs)
  number_of_reps <- length(list.files(
    file.path(dirheatmap, allrundirs[1], "variable_store")))
  dim_source = yaml.load_file(file.path("parameters", source_of_params))

  # reordering the elements of the directory to line up right
  if (!(ordering)) {
    thing <- sapply(1:number_of_runs, function(x)
    str_split(allrundirs[x], "_")[[1]][2])

    allrundirs <- allrundirs[str_order(thing)]
  } else {
    allrundirs <- allrundirs[ordering]
  }
  runmeans <- list()

  timespanchunks <- as.numeric(str_split(str_split(allrundirs[1], "_")[[1]][4], "k")[[1]][1]) * (1000 / dim_source$recordsimplifyfactor)

  for(individual_run in 1:number_of_runs) {

    # individual_run <- 1
    if (deeper) {
      multirun_directory <-
      file.path(
        dirheatmap, allrundirs[individual_run], "multirun_output",
      list.files(path = file.path(
        dirheatmap, allrundirs[individual_run], "multirun_output"),
        pattern = "output$")
      )
    } else {
      multirun_directory <-
      file.path(dirheatmap, allrundirs[individual_run], "multirun_output"#,
      # list.files(path = file.path(dirheatmap,
      # allrundirs[individual_run], "multirun_output"), pattern = "output$")
      )
    }

    namedrdatas <- outputfilenames(number_of_reps)



    datanrepzlist <- array(0, c(2, dim_source$num_pop, timespanchunks, number_of_reps))
    datantbxnlist <- array(0, c((2 * dim_source$num_pop), dim_source$sylnum, timespanchunks, number_of_reps))
    datansitylist <- array(0, c(12, dim_source$num_pop, timespanchunks, number_of_reps))
    datanhistlist <- array(0, c((2*dim_source$num_pop), (dim_source$num_pop * dim_source$one_pop_singers[1]), timespanchunks, number_of_reps))

    for(i in 1:number_of_reps) {
      datanhistlist[,,,i] <- readRDS(paste0(multirun_directory, "/", namedrdatas$histlist[i]))
      datansitylist[,,,i] <- readRDS(paste0(multirun_directory, "/", namedrdatas$sitylist[i]))
      datantbxnlist[,,,i] <- readRDS(paste0(multirun_directory, "/", namedrdatas$sdstlist[i]))
      datanrepzlist[,,,i] <- readRDS(paste0(multirun_directory, "/", namedrdatas$repzlist[i]))

      # curhistlist[,,,i] <- fread(file.path(multirun_directory, histlist[i]))
      # cursitylist[,,,i] <- fread(file.path(multirun_directory, sitylist[i]))
      # sdstbxnlist[,,,i] <- fread(file.path(multirun_directory, sdstlist[i]))
      # sylrepzlist[,,,i] <- fread(file.path(multirun_directory, repzlist[i]))
    }

    # These four lines calculate the mean value across all the replicates

    curhstmeans <- colMeans(aperm(datanhistlist, c(4, 1, 2, 3)), na.rm = TRUE)
    curlvlmeans <- colMeans(aperm(datansitylist, c(4, 1, 2, 3)), na.rm = TRUE)
    syldbnmeans <- colMeans(aperm(datantbxnlist, c(4, 1, 2, 3)), na.rm = TRUE)
    sylrepmeans <- colMeans(aperm(datanrepzlist, c(4, 1, 2, 3)), na.rm = TRUE)

    runmeans[[individual_run]] <- list(
      sylrepmeans = sylrepmeans,
      syldbnmeans = syldbnmeans,
      curlvlmeans = curlvlmeans,
      curhstmeans = curhstmeans
    )
  }
  return(runmeans)
}

makeheatmapfile <- function (
  inheritance,# = 3,
  diffcurstartbias,# = 1,
  biassize,# = 3,
  othersize,# = 2,
  reversedruns = FALSE,
  redo = FALSE,
  runstyle = 1,
  highres = FALSE,
  extractedmeans = extractedmeans
) {

  zero_to_one_template <- c ( 0.00,0.01,0.05,0.09, 0.1,0.15,0.18, 0.2,0.25,0.27,
  #                             #1,  #2,  #3,  #4,  #5,  #6,  #7,  #8,  #9, #10,
                               0.3,0.35,0.36, 0.4,0.45,0.49, 0.5,0.51,0.54,0.55,
  #                            #11, #12, #13, #14, #15, #16, #17, #18, #19, #20,
                              0.59, 0.6,0.63,0.65, 0.7,0.72,0.75, 0.8,0.81,0.85,
  #                            #21, #22, #23, #24, #25, #26, #27, #28, #29, #30,
                               0.9,0.95,0.99,1.0)
  #                            #31, #32, #33,#34

  whichinh <- c("male","moth","same","opps","sNTn",
                "sSTf","sSFr","sFrS","sTfS","sTnN", "FfFf")
  inheritance <- whichinh[inheritance]
  # whichbias <- c("male","female", "pop1", "pop2", "both")
  # diffcurstartbias <- whichbias[diffcurstartbias]

  whichrunstyle <- c("lowMedHigh", "narrowWide", "lowHigh")
  if (runstyle %in% 1 : length(whichrunstyle)) {
    runstyle = whichrunstyle[runstyle]
  }

  # biassize = 5

#  print("and a one")

  if (redo) {

    foldername <- list.files(path = file.path("results"#, "Heatmaps", "output_objects"
    ), pattern = paste0(inheritance, "inh_", diffcurstartbias, "Bias"))
          # "inh_", runstyle, "Bias"))

    heatmap_array <- readRDS(file.path("results", "Heatmaps", "output_objects",
      foldername, list.files(path = file.path("results", "Heatmaps", "output_objects",
        foldername), pattern =
          "heatmap_output_-_")
      )
    )

    #     print("and a two")

    placeholder <- array(rep(0,biassize * biassize * 8), dim(heatmap_array))

    for (third_dimension in 1:othersize) { # femalez
      for (second_dimension in 1:biassize) { # malez1
        for(first_dimension in 1:biassize) { # malez2
          # placeholder <- array(rep(0,biassize * biassize * 8))
          # tally <- first_dimension + biassize*(second_dimension - 1) + biassize*biassize*(third_dimension - 1)
          # thing <- length (extractedmeans [[1]][[1]][1,1,])
          # sumstats <- c (
            # extractedmeans [[tally]]$curlvlmeans [1,1,thing],
            # extractedmeans [[tally]]$curlvlmeans [1,2,thing],
            # extractedmeans [[tally]]$curlvlmeans [2,1,thing],
            # extractedmeans [[tally]]$curlvlmeans [2,2,thing],
            # extractedmeans [[tally]]$sylrepmeans [1,1,thing],
            # extractedmeans [[tally]]$sylrepmeans [1,2,thing],
            # extractedmeans [[tally]]$sylrepmeans [2,1,thing],
            # extractedmeans [[tally]]$sylrepmeans [2,2,thing]
          # )
          # this part might not work if the population of
          # disinterest continues to be the final addition
          # if (reversedruns) {
            # if (othersize == 1) {
              placeholder [second_dimension, first_dimension, third_dimension, ] <- heatmap_array [biassize - (second_dimension - 1), biassize - (first_dimension - 1), third_dimension,]
            # } else {
              # heatmap_array [biassize - (second_dimension - 1), biassize - (first_dimension - 1), otherBias - (third_dimension - 1),] <- sumstats
            # }
          # } else {
            # heatmap_array [second_dimension, first_dimension, third_dimension, ] <- sumstats
          # }
        }
      }
    }

    heatmap_array <- placeholder

  } else {
    if (highres) {

      # print("and a three")

      foldername <- paste0 (
      str_sub (paste (str_extract_all (
        Sys.time (), "[0123456789]"
      )[[1]], collapse = ""), 3, 8),
      "_slices_-_",
      inheritance,
      "inh_",
      diffcurstartbias,
      "Bias_", runstyle)
      biassize = 10
      othersize = 1
      heatmap_array <- array (
            0, dim = c(biassize, biassize, othersize, 8), list (
              c ("0-0.18mp1", "0.09-0.27mp1", "0.18-0.36mp1", "0.27-0.45mp1", "0.36-0.54mp1", "0.45-0.63mp1", "0.54-0.72mp1", "0.63-0.81mp1", "0.72-0.9mp1", "0.81-1mp1"),
              c ("0-0.18fp1", "0.09-0.27fp1", "0.18-0.36fp1", "0.27-0.45fp1", "0.36-0.54fp1", "0.45-0.63fp1", "0.54-0.72fp1", "0.63-0.81fp1", "0.72-0.9fp1", "0.81-1fp1"),
              # c("0.18-0.27p2", "0.72-0.81fp2"),
              c ("0.18-0.27p2"),
              c ("endcurm1","endcurm2","endcurf1","endcurf2","endrepm1","endrepm2","endrepf1","endrepf2")
            ))
    } else {
      # print("and no highres three")
      foldername <- paste0 (
      str_sub (paste (str_extract_all (
        Sys.time (), "[0123456789]"
      )[[1]], collapse = ""), 3, 8),
      "_slices_-_",
      inheritance,
      "inh_",
      diffcurstartbias,
      "Bias")

      if (diffcurstartbias == "male") {
        if (othersize == 5) {
          heatmap_array <- array (
          0, dim = c (biassize, biassize, othersize, 8), list (
            c ("0-0.25mp1", "0.25-0.5mp1", "0.5-1.0mp1", "0-1.0mp1", "0.45-0.55mp1"),
            c ("0-0.25mp2", "0.25-0.5mp2", "0.5-1.0mp2", "0-1.0mp2", "0.45-0.55mp2"),
            c ("0-0.25f", "0.25-0.5f", "0.5-1.0f", "0-1.0f", "0.45-0.55f"),
            c ("endcurm1","endcurm2","endcurf1","endcurf2","endrepm1","endrepm2","endrepf1","endrepf2")
          ))
        }
        othersize <- 5
      } else if (diffcurstartbias == "female") {
        if (othersize == 5) {
          heatmap_array <- array (
            0, dim = c(biassize, biassize, othersize, 8), list (
              c ("0-0.25fp1", "0.25-0.5fp1", "0.5-1.0fp1", "0-1.0fp1", "0.45-0.55fp1"),
              c ("0-0.25fp2", "0.25-0.5fp2", "0.5-1.0fp2", "0-1.0fp2", "0.45-0.55fp2"),
              c ("0-0.25m", "0.25-0.5m", "0.5-1.0m", "0-1.0m", "0.45-0.55m"),
              c ("endcurm1","endcurm2","endcurf1","endcurf2","endrepm1","endrepm2","endrepf1","endrepf2")
            )
          )
        }

      } else if (diffcurstartbias == "pop1") {
        # if (othersize == 2) {
          # if (specialfigs) {
        if (runstyle == "lowMedHigh") {
          # print("dcb pop1, runstyle lowMedHigh")
          heatmap_array <- array (
            0, dim = c(biassize, biassize, othersize, 8), list (
              c ("0-0.25mp1", "0.25-0.5mp1", "0.5-1.0mp1"),
              c ("0-0.25fp1", "0.25-0.5fp1", "0.5-1.0fp1"),
              c ("0-0.25p2", "0.5-1.0p2"),
              c ("endcurm1","endcurm2","endcurf1","endcurf2","endrepm1","endrepm2","endrepf1","endrepf2")
            )
          )
        } else if (runstyle == "narrowWide") {
          # print("dcb pop1, runstyle narrowWide")
          heatmap_array <- array (
          0, dim = c(biassize, biassize, othersize, 8), list (
            c ("0-1.0mp1", "0.45-0.55mp1"),
            c ("0-1.0fp1", "0.45-0.55fp1"),
            c ("0-0.25p2", "0.5-1.0p2"),
            c ("endcurm1","endcurm2","endcurf1","endcurf2","endrepm1","endrepm2","endrepf1","endrepf2")
          ))
        } else if (runstyle == "lowHigh") {
          # print("dcb pop1, runstyle lowHigh")
          heatmap_array <- array (
          0, dim = c (biassize, biassize, othersize, 8), list (
            c ("0-0.2mp1", "0.2-0.3mp1", "0.4-0.6mp1", "0.55-0.75mp1", "0.7-0.8mp1"),
            c ("0-0.2fp1", "0.2-0.3fp1", "0.4-0.6fp1", "0.55-0.75fp1", "0.7-0.8fp1"),
            c ("0.2-0.3p2", "0.7-0.8p2"),
            c ("endcurm1","endcurm2","endcurf1","endcurf2","endrepm1","endrepm2","endrepf1","endrepf2")
          ))
        } else if (runstyle == "binary") {
          # print("dcb pop1, runstyle binary")
          heatmap_array <- array(
            0, dim = c (biassize, biassize, othersize, 8), list (
              c ("0-0.18mp1", "0.81-1mp1"),
              c ("0-0.18fp1", "0.81-1fp1"),
              c ("0-0.18p2", "0.81-1p2"),
              c ("endcurm1","endcurm2","endcurf1","endcurf2","endrepm1","endrepm2","endrepf1","endrepf2")
            )
          )
        } else if (runstyle == "binaryHB") {
          # print("dcb pop1, runstyle binaryHB")
          heatmap_array <- array(
            0, dim = c (biassize, biassize, othersize, 8), list (
              c ("0-0.18mp1", "0.81-1mp1"),
              c ("0-0.18fp1", "0.81-1fp1"),
              c ("0.81-1p2"),
              c ("endcurm1","endcurm2","endcurf1","endcurf2","endrepm1","endrepm2","endrepf1","endrepf2")
            )
          )
        } else if (runstyle == "binaryLB") {
          # print("dcb pop1, runstyle binaryLB")
          heatmap_array <- array(
            0, dim = c (biassize, biassize, othersize, 8), list (
              c ("0-0.18mp1", "0.81-1mp1"),
              c ("0-0.18fp1", "0.81-1fp1"),
              c ("0-0.18p2"),
              c ("endcurm1","endcurm2","endcurf1","endcurf2","endrepm1","endrepm2","endrepf1","endrepf2")
            )
          )
        }
          # }
        # }

      } else if (diffcurstartbias == "pop2") {
        # othersize <- 2
        # if (specialfigs) {
          if (runstyle == "lowMedHigh") {
            # print("dcb pop2, runstyle lowMedHigh")
            heatmap_array <- array(
              0, dim = c (othersize, biassize, biassize, 8), list (
                c ("0-0.25p1", "0.5-1.0p1"),
                c ("0-0.25mp2", "0.25-0.5mp2", "0.5-1.0mp2"),
                c ("0-0.25fp2", "0.25-0.5fp2", "0.5-1.0fp2"),
                c ("endcurm1","endcurm2","endcurf1","endcurf2","endrepm1","endrepm2","endrepf1","endrepf2")
              )
            )
          } else if (runstyle == "narrowWide") {
            # print("dcb pop2, runstyle narrowWide")
            heatmap_array <- array(
            0, dim = c (othersize, biassize, biassize, 8), list (
              c ("0-0.25p1", "0.5-1.0p1"),
              c ("0-1.0mp2", "0.45-0.55mp2"),
              c ("0-1.0fp2", "0.45-0.55fp2"),
              c ("endcurm1","endcurm2","endcurf1","endcurf2","endrepm1","endrepm2","endrepf1","endrepf2")
            ))
          }
        # }
      }
    }

#     print("and a five")
    # print(paste0("heatmap_array dimensions: ", dim(heatmap_array)))
    for (third_dimension in 1:othersize) { # femalez
      for (second_dimension in 1:biassize) { # malez1
        for(first_dimension in 1:biassize) { # malez2

          tally <- first_dimension + biassize*(second_dimension - 1) + biassize*biassize*(third_dimension - 1)
          thing <- length (extractedmeans [[1]][[1]][1,1,])
          sumstats <- c (
            extractedmeans[[tally]][[3]][1,1,thing],
            extractedmeans[[tally]][[3]][1,2,thing],
            extractedmeans[[tally]][[3]][2,1,thing],
            extractedmeans[[tally]][[3]][2,2,thing],
            extractedmeans[[tally]][[1]][1,1,thing],
            extractedmeans[[tally]][[1]][1,2,thing],
            extractedmeans[[tally]][[1]][2,1,thing],
            extractedmeans[[tally]][[1]][2,2,thing]
          )
          # print(paste(length(sumstats)))
          # this part might not work if the population of
          # disinterest continues to be the final addition
          if (reversedruns) {
            if (othersize == 1) {
              heatmap_array [biassize - (second_dimension - 1), biassize - (first_dimension - 1), third_dimension,] <- sumstats
            } else {
              heatmap_array [biassize - (second_dimension - 1), biassize - (first_dimension - 1), othersize - (third_dimension - 1),] <- sumstats
            }
          } else {
            heatmap_array [second_dimension, first_dimension, third_dimension, ] <- sumstats
          }
        }
      }
    }

    # if(!(dir.exists(file.path("results", "Heatmaps", "output_objects", foldername)))) {
    #   dir.create(file.path("results", "Heatmaps", "output_objects", foldername))
    #   if(!(file.exists(file.path(
    #     "results", "Heatmaps", "output_objects", foldername, paste0("heatmap_output_-_", foldername, ".RData")
    #           )))) {saveRDS(heatmap_array, file.path(
    #     "results", "Heatmaps", "output_objects", foldername, paste0("heatmap_output_-_", foldername, ".RData")
    #   ))}
    #   for (subset in 1:5) {
    #     dir.create(file.path("results", "Heatmaps", "output_objects", foldername, paste0("slice_", subset)))
    #   }
    # }

    # print("and a six")

    if(!(dir.exists(file.path("results", foldername)))) {

      dir.create(file.path("results", foldername))}

    # if (specialfigs) {
    if(!(file.exists(file.path(
      "results", foldername, paste0("heatmap_output_-_", inheritance,
      "inh_", diffcurstartbias, "Bias_", runstyle, ".RData")
    )))) {

    saveRDS(heatmap_array, file.path(
      "results",foldername, paste0("heatmap_output_-_", inheritance,
      "inh_", diffcurstartbias, "Bias_", runstyle, ".RData")

    ))}
    # }# else {
    #   if(!(file.exists(file.path(
    #     "results", foldername, paste0("heatmap_output_-_", inheritance,
    #     "inh_", diffcurstartbias, "Bias.RData")
    #   )))) {

    #   saveRDS(heatmap_array, file.path(
    #     "results",foldername, paste0("heatmap_output_-_", inheritance,
    #     "inh_", diffcurstartbias, "Bias.RData")

    #   ))}
    # }
  }
  # for (subset in 1:5) {
  #   dir.create(file.path("results", foldername, paste0("slice_", subset)))
  # }
#   print("and a seven")
# print("and an eight")
# print("and a nine")
# print("and a ten")
# print("and an eleven")
# print("and a twelve")
# print("and a thirteen")
  outputBall <- list(
    foldername = foldername,
    inheritance = inheritance,
    diffcurstartbias = diffcurstartbias,
    biassize = biassize,
    othersize = othersize
  )
  return(outputBall)
}

individualfigures <- function (

  colorrange = 2, # c("relative", "absolute", "differences")
  colorpalette = 5, # Numbers correspond to specific color palettes
  foldername = heatmapoutput,
  midpoint_size = 1 # ranges from 1-7; smallest size midpoint color range (# 1's size: 2) to largest (# 7's size: 86)
) {

  # reds, rdpu, oranges, orrd, ylorrd, ylorbr, ylgn, ylgnbu, greens, gnbu, blues, bugn, bupu, purples, purd, pubu, pubugn, greys, midpoint
  #    1,    2,       3,    4,      5,      6,    7,      8,      9,   10,    11,   12,   13,      14,   15,   16,     17,    18,      19

  # heatmap_sourcefolder <- file.path("results", "Heatmaps", "output_objects")
  heatmap_sourcefolder <- file.path("results")
  # heatmap_sourcefolder <- file.path("sameSexFigResults", "results")


  # Character vectors for args - indices  Not sure what else to call them, but they'll be used to reassign the args to non-numeric objects

  clrrngcontainer <- c("relative", "absolute", "differences")

  colorrange <- clrrngcontainer[colorrange]

  inheritancecontainer <- c("maleinh", "mothinh", "sameinh", "oppsinh",
                            "sNTninh", "sSTfinh", "sSFrinh", "sFrSinh",
                            "sTfSinh", "sTnNinh", "FfFfinh")

  whichbias <- c("malebias", "femaleBias", "pop1Bias", "pop2Bias", "bothBias")

  if (foldername$diffcurstartbias == "male" || foldername$diffcurstartbias == 1) {
    heatmap_axes <- list(
      mp2vfem = c("Pop 2 Male Starting Curiosity", "Female Starting Curiosity"),    # mp2vfem
      mp1vfem = c("Pop 1 Male Starting Curiosity", "Female Starting Curiosity"),    # mp1vfem
      mp1vmp2 = c("Pop 1 Male Starting Curiosity", "Pop 2 Male Starting Curiosity") # mp1vmp2
    )
    slicedpop <- list(
      "MalPop1",
      "MalPop2",
      "FemalePop"
    )
  } else if (foldername$diffcurstartbias == "female" || foldername$diffcurstartbias == 2) {
    heatmap_axes <- list(
      mf2vmal = c("Pop 2 Female Starting Curiosity", "Male Starting Curiosity"),
      mf1vmal = c("Pop 1 Female Starting Curiosity", "Male Starting Curiosity"),
      mf1vmf2 = c("Pop 1 Female Starting Curiosity", "Pop 2 Female Starting Curiosity")
    )
    slicedpop <- list(
      "FemPop1",
      "FemPop2",
      "MalePop"
    )
  } else if (foldername$diffcurstartbias == "pop1" || foldername$diffcurstartbias == 3) {
    heatmap_axes <- list(
      fp1Vpp2 = c("Pop 1 Female Starting Curiosity", "Pop 2 Starting Curiosity"),
      mp1Vpp2 = c("Pop 1 Male Starting Curiosity", "Pop 2 Starting Curiosity"),
      mp1Vfp1 = c("Pop 1 Male Starting Curiosity", "Pop 1 Female Starting Curiosity")
    )
    slicedpop <- list(
      "MalPop1",
      "FemPop1",
      "Popula2"
    )
  } else if (foldername$diffcurstartbias == "pop2" || foldername$diffcurstartbias == 4) {
    heatmap_axes <- list(
      fp1Vpp2 = c("Pop 2 Female Starting Curiosity", "Pop 1 Starting Curiosity"),
      mp1Vpp2 = c("Pop 2 Male Starting Curiosity", "Pop 1 Starting Curiosity"),
      mp1Vfp1 = c("Pop 2 Male Starting Curiosity", "Pop 2 Female Starting Curiosity")
    )
    slicedpop <- list(
      "MalPop2",
      "FemPop2",
      "Popula1"
    )
  }


  # inheritance <- inheritancecontainer[inheritance]

  # thisBias <- whichbias[thisBias]

  # foldername <- list.files(heatmap_sourcefolder)[which(sapply(list.files(heatmap_sourcefolder), function(x) (inheritance %in% str_split(x, "_")[[1]][4] && thisBias %in% str_split(x, "_")[[1]][5])))]
  # foldername <-

  # temphtmparray <- readRDS(file.path(heatmap_sourcefolder, foldername, list.files(file.path(heatmap_sourcefolder, foldername), pattern = ".RData")))
  htmparrays <- list.files (file.path (heatmap_sourcefolder, foldername$foldername), pattern = ".RData")

  if (length (htmparrays) == 1) {
    temphtmparray <- readRDS (file.path (heatmap_sourcefolder, foldername$foldername, htmparrays))
  } else {stop ("there's either more or less than one .RData file in that directory!")}

  if (colorpalette == 19) {

    whatever <- array(c(2, 16, 30, 44, 58, 72, 86, 7, 6, 5, 4, 3, 2, 1), c(7,2))
    stuff <- whatever[midpoint_size,]
  } else {
    stuff <- c(2, 7)
  }


  colorseqmultpalette <- list (
    reds = colorRampPalette (c ("#fee0d2", "#fc9272", "#de2d26")), # 3-class reds                                        ### 1
    rdpu = colorRampPalette (c ("#fde0dd", "#fa9fb5", "#c51b8a")), # 3-class rdpu                                        ### 2
    oranges = colorRampPalette (c ("#fee6ce", "#fdae6b", "#e6550d")), # 3-class oranges                                  ### 3
    orrd = colorRampPalette (c ("#fee8c8", "#fdbb84", "#e34a33")), # 3-class orrd                                        ### 4
    ylorrd = colorRampPalette (c ("#ffeda0", "#feb24c", "#f03b20")), # 3-class ylorrd                                    ### 5
    ylorbr = colorRampPalette (c ("#fff7bc", "#fec44f", "#d95f0e")), # 3-class ylorbr                                    ### 6
    ylgn = colorRampPalette (c ("#f7fcb9", "#addd8e", "#31a354")), # 3-class ylgn                                        ### 7
    ylgnbu = colorRampPalette (c ("#edf8b1", "#7fcdbb", "#2c7fb8")), # 3-class ylgnbu                                    ### 8
    greens = colorRampPalette (c ("#e5f5e0", "#a1d99b", "#31a354")), # 3-class greens                                    ### 9
    gnbu = colorRampPalette (c ("#e0f3db", "#a8ddb5", "#43a2ca")), # 3-class gnbu                                        ### 10
    blues = colorRampPalette (c ("#deebf7", "#9ecae1", "#3182bd")), # 3-class blues                                      ### 11
    bugn = colorRampPalette (c ("#e5f5f9", "#99d8c9", "#2ca25f")), # 3-class bugn                                        ### 12
    bupu = colorRampPalette (c ("#e0ecf4", "#9ebcda", "#8856a7")), # 3-class bupu                                        ### 13
    purples = colorRampPalette (c ("#efedf5", "#bcbddc", "#756bb1")), # 3-class purples                                  ### 14
    purd = colorRampPalette (c ("#e7e1ef", "#c994c7", "#dd1c77")), # 3-class purd                                        ### 15
    pubu = colorRampPalette (c ("#ece7f2", "#a6bddb", "#2b8cbe")), # 3-class pubu                                        ### 16
    pubugn = colorRampPalette (c ("#ece2f0", "#a6bddb", "#1c9099")), # 3-class pubugn                                    ### 17
    greys = colorRampPalette (c ("#f0f0f0", "#bdbdbd", "#636363")), # 3-class greys                                      ### 18
    # midpoint = colorRampPalette (c ("#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac"))

    eval (parse (text = paste0 ("midpoint = colorRampPalette (c (rep(\"#67001f\", ",
                                                   stuff[2], "), rep(\"#b2182b\", ",
                                                   stuff[2], "), rep(\"#ca0020\", ",
                                                   stuff[2], "), rep(\"#d6604d\", ",
                                                   stuff[2], "), rep(\"#ef8a62\", ",
                                                   stuff[2], "), rep(\"#f4a582\", ",
                                                   stuff[2], "), rep(\"#fddbc7\", ",
                                                   stuff[2], "), rep(\"#f7f7f7\", ",
                                                   stuff[1], "), rep(\"#d1e5f0\", ",
                                                   stuff[2], "), rep(\"#92c5de\", ",
                                                   stuff[2], "), rep(\"#67a9cf\", ",
                                                   stuff[2], "), rep(\"#4393c3\", ",
                                                   stuff[2], "), rep(\"#0571b0\", ",
                                                   stuff[2], "), rep(\"#2166ac\", ",
                                                   stuff[2], "), rep(\"#053061\", ",
                                                   stuff[2], ")))")))
  )
    # ("#ef8a62", "#f7f7f7", "#67a9cf"))
    # ("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061"))

# eval(parse(text=paste0("curhistlist[[number_of_repeats + 1]][i] <- mean(c(curhistlist[[",
#                            paste0(1:(number_of_repeats - 1),"]][i],curhistlist[[", collapse=''),
#                            number_of_repeats, "]][i]))")))

    # ("#ca0020", "#ca0020", "#f4a582", "#f4a582", "#f7f7f7", "#92c5de", "#92c5de", "#0571b0", "#0571b0"))
                                  #  ("#67001f",
                                  #   "#b2182b", "#b2182b", "#b2182b",
                                  #   "#d6604d", "#d6604d", "#d6604d", "#d6604d",
                                  #   "#fddbc7", "#fddbc7",
                                  #   "#f7f7f7",
                                  #   "#d1e5f0", "#d1e5f0",
                                  #   "#4393c3", "#4393c3", "#4393c3", "#4393c3",
                                  #   "#2166ac", "#2166ac", "#2166ac",
                                  #   "#053061")) # 2-class divergent Red -> Blue Spectrum ### 19
  # )

  regularnames <- c (
    "EndCurValP1M",
    "EndCurValP2M",
    "EndCurValP1F",
    "EndCurValP2F",
    "EndSRpValP1M",
    "EndSRpValP2M",
    "EndSRpValP1F",
    "EndSRpValP2F"
  )

  # source("/home/parker/Documents/projects/curmodel_pcomp1/Code/curiosity-code/scripts/Source_Magick_Functions.R")

  # for (htmpView in 1:3) { # looking at the cubes from different angles (aka which population are we seeing one slice at a time, while the other populations are plotted on the axes?)

  if (! (dir.exists (file.path (
    heatmap_sourcefolder, foldername$foldername, slicedpop[3] # paste0("slice_", slice)
  )))) {
    dir.create (file.path (
      heatmap_sourcefolder, foldername$foldername, slicedpop[3] # paste0("slice_", slice)
    ))
  }

  otherpopsize <- foldername$othersize
  dat_array_doh <- array (c (
    1,1,1, 1,1,1, 1,1,1, 1,3,3, 3,1,3, otherpopsize,otherpopsize,1,
    2,1,1, 1,2,1, 1,1,2, 2,3,3, 3,2,3, otherpopsize,otherpopsize,2,
    3,1,1, 1,3,1, 1,1,otherpopsize, 3,3,3, 3,3,3, otherpopsize,otherpopsize,otherpopsize
    # rep(c(1, 1, 1, 1), 2), 1, 1, rep(c(3, 3, 3, 1), 2),
    # rep(c(2, 1, 1, 1), 2), 2, 2, rep(c(3, 3, 3, 2), 2),
    # rep(c(3, 1, 1, 1), 2), 3, 3, rep(c(3, 3, 3, 3), 2)
  ), c (3, 3, otherpopsize, 3))

  # saveRDS(foldername, file.path (heatmap_sourcefolder, foldername$foldername, "foldername.RData"))

  for (sxmtpop in 1:8) {
    for (slice in 1:otherpopsize) {

      file_name <- paste0 (regularnames[sxmtpop], "_slice_", slice, "_", slicedpop[3], ".png")
      # rule of thumb: if we're splitting up htmpView _within_ slice and sxmtpop, then we need to save the output files according to the schema that will help pull back together the slices.
      png (filename = file.path (
          heatmap_sourcefolder, foldername$foldername, #inhoptions[inhstyle + 2],
          # paste0("slice_", slice), file_name),
          slicedpop[3], file_name),
        width = 554, height = 554, units = "px", pointsize = 12, bg = "white")

      if (colorrange == "absolute") {
        if (sxmtpop <= 4) {
          heatmaprange <- c (0,1)
        } else {
          heatmaprange <- c (1,156)
        }
      } else if (colorrange == "relative") {

        heatmaprange <- inhoptions[[inhstyle]][
          dat_array_doh[1,1,1,slice]:dat_array_doh[1,1,2,slice],
          dat_array_doh[1,2,1,slice]:dat_array_doh[1,2,2,slice],
          dat_array_doh[1,3,1,slice]:dat_array_doh[1,3,2,slice],
          sxmtpop]
        heatmap_min <- c (
          round(min(heatmaprangedatasetone), 2),
          round(min(heatmaprangedatasettwo), 2),
          round(min(heatmaprangedatasettre), 2)
        )
        heatmap_max <- c (
          round(max(heatmaprangedatasetone), 2),
          round(max(heatmaprangedatasettwo), 2),
          round(max(heatmaprangedatasettre), 2)
        )

        heatmaprange <- c (heatmap_min[3] - 0.01, heatmap_max[3] + 0.01)
        rm(heatmaprangedatasetone, heatmaprangedatasettwo, heatmaprangedatasettre,
          heatmap_min, heatmap_max)
      }# else if (colorrange == "differences") {
      #   heatmaprange <- c (0,1)
      # }

       # UNFINISHED - depreciated?
      # findXLab <- heatmap_axes[[3]][1]
      # findYLab <- heatmap_axes[[3]][2]

      # if(inhstyle == 1) {
        # dim_1 = 3
        # dim_2 = 3
        # dim_3 = 2


        image(x = temphtmparray[,,slice,sxmtpop],
          col = colorseqmultpalette[[colorpalette]](100),
          axes = F,
          xlab = heatmap_axes[[3]][1],
          ylab = heatmap_axes[[3]][2],cex.lab=1.4, zlim = heatmaprange
        )

        if (!(is.null(dimnames(temphtmparray)))) {
          temphtmpdimensions <- dimnames(temphtmparray)
          temptemp <- vector(mode = "character", length = length(temphtmpdimensions))
          for (thething in 1:length(temphtmpdimensions[[1]])) {
            temptemp[thething] <- str_extract_all(temphtmpdimensions[[1]][thething], "[0123456789|0123456789.0123456789]*-[0123456789|0123456789.0123456789]*")
          }


          # sets up the axes regardless of size, based on what they were labeled when they were originally run.
          if (foldername$biassize == 2) {
            axis(1,c(-0.495,  0  ,0.5,    1    ,1.495),
              c(  ""   ,temptemp[[1]][1],"" ,temptemp[[2]][1],"" ),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(1,c(-0.495,0.5,1.495),
              c("","",""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)

            axis(2,c(-0.495,  0  ,0.5,    1    ,1.495),
              c(  ""   ,temptemp[[1]][1],"" ,temptemp[[2]][1],"" ),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(2,c(-0.495,0.5,1.495),
              c("","",""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)
          } else if (foldername$biassize == 3) {
            axis(1,c(-0.25, 0, 0.25, 0.5, 0.75, 0.97, 1.25),
              c("", temptemp[[1]][1], "", temptemp[[2]][1], "", temptemp[[3]][1], ""),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(1,c(-0.25, 0.25, 0.75, 1.25),
              c("", "", "", ""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)

            axis(2,c(-0.25, 0, 0.25, 0.5, 0.75, 0.97, 1.25),
              c("", temptemp[[1]][1], "", temptemp[[2]][1], "", temptemp[[3]][1], ""),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(2,c(-0.25, 0.25, 0.75, 1.25),
              c("", "", "", ""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)
          } else if (foldername$biassize ==  4) {
            axis(1,c(-0.165, 0, 0.167, 0.334, 0.5, 0.667, 0.834, 1, 1.1649),
              c("", temptemp[[1]][1], "", temptemp[[2]][1], "", temptemp[[3]][1], "", temptemp[[4]][1], ""),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(1,c(-0.165, 0.168, 0.5, 0.835, 1.1649),
              c("", "", "", "", ""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)

            axis(2,c(-0.165, 0, 0.167, 0.334, 0.5, 0.667, 0.834, 1, 1.1649),
              c("", temptemp[[1]][1], "", temptemp[[2]][1], "", temptemp[[3]][1], "", temptemp[[4]][1], ""),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(2,c(-0.165, 0.168, 0.5, 0.835, 1.1649),
              c("", "", "", "", ""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)
          } else if (foldername$biassize == 5) {
            axis(1,c(-0.124, 0, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 0.97, 1.124),
              c("", temptemp[[1]][1], "", temptemp[[2]][1], "", temptemp[[3]][1], "", temptemp[[4]][1], "", temptemp[[5]][1], ""),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(1,c(-0.124, 0.125, 0.375, 0.625, 0.875, 1.124),
              c("", "", "", "", "", ""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)

            axis(2,c(-0.124, 0, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 0.97, 1.124),
              c("", temptemp[[1]][1], "", temptemp[[2]][1], "", temptemp[[3]][1], "", temptemp[[4]][1], "", temptemp[[5]][1], ""),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(2,c(-0.124, 0.125, 0.375, 0.625, 0.875, 1.124),
              c("", "", "", "", "", ""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)
          } else if (foldername$biassize == 6) {
            axis(1,c(-0.1, 0, 0.1, 0.2, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1),
              c("", temptemp[[1]][1], "", temptemp[[2]][1], temptemp[[3]][1], "", temptemp[[4]][1], "", temptemp[[5]][1], "", temptemp[[6]][1], ""),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(1,c(-0.1, 0.1, 0.3, 0.5, 0.7, 0.9, 1.1),
              c("", "", "", "", "", "", ""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)

            axis(2,c(-0.1, 0, 0.1, 0.2, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1),
              c("", temptemp[[1]][1], "", temptemp[[2]][1], temptemp[[3]][1], "", temptemp[[4]][1], "", temptemp[[5]][1], "", temptemp[[6]][1], ""),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(2,c(-0.1, 0.1, 0.3, 0.5, 0.7, 0.9, 1.1),
              c("", "", "", "", "", "", ""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)
          } else if (foldername$biassize == 7) {
            axis(1,c(-0.083,   0, 0.083, 0.167, 0.25, 0.334, 0.416, 0.5, 0.583, 0.667, 0.75, 0.833, 0.916, 1.0, 1.083),
              c("", temptemp[[1]][1], "", temptemp[[2]][1], temptemp[[3]][1], "", temptemp[[4]][1], "", temptemp[[5]][1], "", temptemp[[6]][1], "", temptemp[[7]][1],    ""),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(1,c(-0.083, 0.083, 0.25, 0.416, 0.583, 0.75, 0.916, 1.083),
              c("", "", "", "", "", "", "", ""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)

            axis(2,c(-0.083,   0, 0.083, 0.167, 0.25, 0.334, 0.416, 0.5, 0.583, 0.667, 0.75, 0.833, 0.916, 1.0, 1.083),
                  c("", temptemp[[1]][1], "", temptemp[[2]][1], temptemp[[3]][1], "", temptemp[[4]][1], "", temptemp[[5]][1], "", temptemp[[6]][1], "", temptemp[[7]][1],    ""),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(2,c(-0.083, 0.083, 0.25, 0.416, 0.583, 0.75, 0.916, 1.083),
              c("", "", "", "", "", "", "", ""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)
          } else if (foldername$biassize == 8) {
            axis(1,c(-0.071,   0, 0.071, 0.145, 0.216, 0.287, 0.358, 0.429, 0.5, 0.571, 0.645, 0.716, 0.787, 0.858, 0.929, 1.0, 1.071),
              c("", temptemp[[1]][1], "", temptemp[[2]][1], temptemp[[3]][1], "", temptemp[[4]][1], "", temptemp[[5]][1], "", temptemp[[6]][1], "",   temptemp[[7]][1],    "",    temptemp[[8]][1],    ""),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(1,c(-0.0714, 0.071, 0.216, 0.358, 0.5, 0.645, 0.787, 0.929, 1.071),
              c("", "", "", "", "", "", "", "", ""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)

            axis(2,c(-0.071,   0, 0.071, 0.142, 0.213, 0.284, 0.356, 0.427, 0.5, 0.571, 0.642, 0.713, 0.784, 0.855, 0.93, 1.0, 1.071),
                  c("", temptemp[[1]][1], "", temptemp[[2]][1], temptemp[[3]][1], "", temptemp[[4]][1], "", temptemp[[5]][1], "", temptemp[[6]][1], "", temptemp[[7]][1],    "",    temptemp[[8]][1],    ""),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(2,c(-0.071, 0.071, 0.213, 0.356, 0.498, 0.64, 0.782, 0.93, 1.071),
              c("", "", "", "", "", "", "", "", ""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)
          } else if (foldername$biassize == 9) {
            axis(1,c(-0.0625,   0, 0.0625, 0.125, 0.1875, 0.25, 0.3125, 0.375, 0.4375, 0.5, 0.5625, 0.625, 0.6875, 0.75, 0.8125, 0.875, 0.9375, 1.0, 1.0625),
              c("", temptemp[[1]][1], "", temptemp[[2]][1], temptemp[[3]][1], "", temptemp[[4]][1], "", temptemp[[5]][1], "", temptemp[[6]][1], "",  temptemp[[7]][1],    "",    temptemp[[8]][1],    "",    temptemp[[9]][1],    ""),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(1,c(-0.0625, 0.0625, 0.1875, 0.3125, 0.4375, 0.5625, 0.6875, 0.8125, 0.9375, 1.0625),
              c("", "", "", "", "", "", "", "", "", ""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)

            axis(2,c(-0.0625,   0, 0.0625, 0.125, 0.1875, 0.25, 0.3125, 0.375, 0.4375, 0.5, 0.5625, 0.625, 0.6875, 0.75, 0.8125, 0.875, 0.9375, 1.0, 1.0625),
                  c("", temptemp[[1]][1], "", temptemp[[2]][1], temptemp[[3]][1], "", temptemp[[4]][1], "", temptemp[[5]][1], "", temptemp[[6]][1], "",  temptemp[[7]][1],    "",    temptemp[[8]][1],    "",    temptemp[[9]][1],    ""),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(2,c(-0.0625, 0.0625, 0.1875, 0.3125, 0.4375, 0.5625, 0.6875, 0.8125, 0.9375, 1.0625),
              c("", "", "", "", "", "", "", "", "", ""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)
          } else if (foldername$biassize == 10) {
            axis(1,c(-0.0555,                0, 0.0555, 0.111, 0.1665, 0.222, 0.2775, 0.333, 0.3885, 0.444, 0.4995, 0.555, 0.611, 0.6665, 0.722, 0.7775, 0.833, 0.8885, 0.944, 0.9995, 1.055),
                  c(     "", temptemp[[1]][1], "", temptemp[[2]][1], "", temptemp[[3]][1], "", temptemp[[4]][1], "", temptemp[[5]][1], "", temptemp[[6]][1], "",    temptemp[[7]][1],    "",    temptemp[[8]][1],    "",    temptemp[[9]][1],    "",   temptemp[[10]][1],    ""),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(1,c(-0.0555, 0.0555, 0.1665, 0.2775, 0.3885, 0.5, 0.611, 0.722, 0.833, 0.944, 1.055),
              c("", "", "", "", "", "", "", "", "", "", ""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)

            axis(2,c(-0.0555,   0, 0.0555, 0.111, 0.1665, 0.222, 0.2775, 0.333, 0.3885, 0.444, 0.4995, 0.555, 0.611, 0.6665, 0.722, 0.7775, 0.833, 0.8885, 0.944, 0.9995, 1.055),
                  c("", temptemp[[1]][1], "", temptemp[[2]][1], "", temptemp[[3]][1], "", temptemp[[4]][1], "", temptemp[[5]][1], "", temptemp[[6]][1], "",    temptemp[[7]][1],    "",    temptemp[[8]][1],    "",    temptemp[[9]][1],    "",   temptemp[[10]][1],    ""),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(2,c(-0.0555, 0.0555, 0.1665, 0.2775, 0.3885, 0.5, 0.611, 0.722, 0.833, 0.944, 1.055),
              c("", "", "", "", "", "", "", "", "", "", ""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)
          }

        } else {


          if (foldername$biassize == 2) {
            axis(1,c(-0.495,  0  ,0.5,    1    ,1.495),
              c(  ""   ,temptemp[[1]][1],"" ,temptemp[[2]][1],"" ),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(1,c(-0.495,0.5,1.495),
              c("","",""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)

            axis(2,c(-0.495,  0  ,0.5,    1    ,1.495),
              c(  ""   ,temptemp[[1]][1],"" ,temptemp[[2]][1],"" ),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(2,c(-0.495,0.5,1.495),
              c("","",""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)
          } else if (foldername$biassize == 3) {
            axis(1,c(-0.25, 0, 0.25, 0.5, 0.75, 0.97, 1.25),
              c("", temptemp[[1]][1], "", temptemp[[2]][1], "", temptemp[[3]][1], ""),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(1,c(-0.25, 0.25, 0.75, 1.25),
              c("", "", "", ""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)

            axis(2,c(-0.25, 0, 0.25, 0.5, 0.75, 0.97, 1.25),
              c("", temptemp[[1]][1], "", temptemp[[2]][1], "", temptemp[[3]][1], ""),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(2,c(-0.25, 0.25, 0.75, 1.25),
              c("", "", "", ""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)
          } else if (foldername$biassize ==  4) {
            axis(1,c(-0.165, 0, 0.167, 0.334, 0.5, 0.667, 0.834, 1, 1.1649),
              c("", temptemp[[1]][1], "", temptemp[[2]][1], "", temptemp[[3]][1], "", temptemp[[4]][1], ""),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(1,c(-0.165, 0.168, 0.5, 0.835, 1.1649),
              c("", "", "", "", ""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)

            axis(2,c(-0.165, 0, 0.167, 0.334, 0.5, 0.667, 0.834, 1, 1.1649),
              c("", temptemp[[1]][1], "", temptemp[[2]][1], "", temptemp[[3]][1], "", temptemp[[4]][1], ""),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(2,c(-0.165, 0.168, 0.5, 0.835, 1.1649),
              c("", "", "", "", ""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)
          } else if (foldername$biassize == 5) {
            axis(1,c(-0.124, 0, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 0.97, 1.124),
              c("", temptemp[[1]][1], "", temptemp[[2]][1], "", temptemp[[3]][1], "", temptemp[[4]][1], "", temptemp[[5]][1], ""),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(1,c(-0.124, 0.125, 0.375, 0.625, 0.875, 1.124),
              c("", "", "", "", "", ""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)

            axis(2,c(-0.124, 0, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 0.97, 1.124),
              c("", temptemp[[1]][1], "", temptemp[[2]][1], "", temptemp[[3]][1], "", temptemp[[4]][1], "", temptemp[[5]][1], ""),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(2,c(-0.124, 0.125, 0.375, 0.625, 0.875, 1.124),
              c("", "", "", "", "", ""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)
          } else if (foldername$biassize == 6) {
            axis(1,c(-0.1, 0, 0.1, 0.2, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1),
              c("", temptemp[[1]][1], "", temptemp[[2]][1], temptemp[[3]][1], "", temptemp[[4]][1], "", temptemp[[5]][1], "", temptemp[[6]][1], ""),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(1,c(-0.1, 0.1, 0.3, 0.5, 0.7, 0.9, 1.1),
              c("", "", "", "", "", "", ""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)

            axis(2,c(-0.1, 0, 0.1, 0.2, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1),
              c("", temptemp[[1]][1], "", temptemp[[2]][1], temptemp[[3]][1], "", temptemp[[4]][1], "", temptemp[[5]][1], "", temptemp[[6]][1], ""),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(2,c(-0.1, 0.1, 0.3, 0.5, 0.7, 0.9, 1.1),
              c("", "", "", "", "", "", ""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)
          } else if (foldername$biassize == 7) {
            axis(1,c(-0.083,   0, 0.083, 0.167, 0.25, 0.334, 0.416, 0.5, 0.583, 0.667, 0.75, 0.833, 0.916, 1.0, 1.083),
              c("", temptemp[[1]][1], "", temptemp[[2]][1], temptemp[[3]][1], "", temptemp[[4]][1], "", temptemp[[5]][1], "", temptemp[[6]][1], "", temptemp[[7]][1],    ""),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(1,c(-0.083, 0.083, 0.25, 0.416, 0.583, 0.75, 0.916, 1.083),
              c("", "", "", "", "", "", "", ""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)

            axis(2,c(-0.083,   0, 0.083, 0.167, 0.25, 0.334, 0.416, 0.5, 0.583, 0.667, 0.75, 0.833, 0.916, 1.0, 1.083),
                  c("", temptemp[[1]][1], "", temptemp[[2]][1], temptemp[[3]][1], "", temptemp[[4]][1], "", temptemp[[5]][1], "", temptemp[[6]][1], "", temptemp[[7]][1],    ""),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(2,c(-0.083, 0.083, 0.25, 0.416, 0.583, 0.75, 0.916, 1.083),
              c("", "", "", "", "", "", "", ""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)
          } else if (foldername$biassize == 8) {
            axis(1,c(-0.071,   0, 0.071, 0.145, 0.216, 0.287, 0.358, 0.429, 0.5, 0.571, 0.645, 0.716, 0.787, 0.858, 0.929, 1.0, 1.071),
              c("", temptemp[[1]][1], "", temptemp[[2]][1], temptemp[[3]][1], "", temptemp[[4]][1], "", temptemp[[5]][1], "", temptemp[[6]][1], "",   temptemp[[7]][1],    "",    temptemp[[8]][1],    ""),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(1,c(-0.0714, 0.071, 0.216, 0.358, 0.5, 0.645, 0.787, 0.929, 1.071),
              c("", "", "", "", "", "", "", "", ""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)

            axis(2,c(-0.071,   0, 0.071, 0.142, 0.213, 0.284, 0.356, 0.427, 0.5, 0.571, 0.642, 0.713, 0.784, 0.855, 0.93, 1.0, 1.071),
                  c("", temptemp[[1]][1], "", temptemp[[2]][1], temptemp[[3]][1], "", temptemp[[4]][1], "", temptemp[[5]][1], "", temptemp[[6]][1], "", temptemp[[7]][1],    "",    temptemp[[8]][1],    ""),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(2,c(-0.071, 0.071, 0.213, 0.356, 0.498, 0.64, 0.782, 0.93, 1.071),
              c("", "", "", "", "", "", "", "", ""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)
          } else if (foldername$biassize == 9) {
            axis(1,c(-0.0625,   0, 0.0625, 0.125, 0.1875, 0.25, 0.3125, 0.375, 0.4375, 0.5, 0.5625, 0.625, 0.6875, 0.75, 0.8125, 0.875, 0.9375, 1.0, 1.0625),
              c("", temptemp[[1]][1], "", temptemp[[2]][1], temptemp[[3]][1], "", temptemp[[4]][1], "", temptemp[[5]][1], "", temptemp[[6]][1], "",  temptemp[[7]][1],    "",    temptemp[[8]][1],    "",    temptemp[[9]][1],    ""),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(1,c(-0.0625, 0.0625, 0.1875, 0.3125, 0.4375, 0.5625, 0.6875, 0.8125, 0.9375, 1.0625),
              c("", "", "", "", "", "", "", "", "", ""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)

            axis(2,c(-0.0625,   0, 0.0625, 0.125, 0.1875, 0.25, 0.3125, 0.375, 0.4375, 0.5, 0.5625, 0.625, 0.6875, 0.75, 0.8125, 0.875, 0.9375, 1.0, 1.0625),
                  c("", temptemp[[1]][1], "", temptemp[[2]][1], temptemp[[3]][1], "", temptemp[[4]][1], "", temptemp[[5]][1], "", temptemp[[6]][1], "",  temptemp[[7]][1],    "",    temptemp[[8]][1],    "",    temptemp[[9]][1],    ""),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(2,c(-0.0625, 0.0625, 0.1875, 0.3125, 0.4375, 0.5625, 0.6875, 0.8125, 0.9375, 1.0625),
              c("", "", "", "", "", "", "", "", "", ""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)
          } else if (foldername$biassize == 10) {
            axis(1,c(-0.0555,                0, 0.0555, 0.111, 0.1665, 0.222, 0.2775, 0.333, 0.3885, 0.444, 0.4995, 0.555, 0.611, 0.6665, 0.722, 0.7775, 0.833, 0.8885, 0.944, 0.9995, 1.055),
                  c("", "0-0.18", "", "0.09-0.27", "", "0.18-0.36", "", "0.27-0.45", "", "0.36-0.54", "", "0.45-0.63", "", "0.54-0.72", "", "0.63-0.81", "", "0.72-0.9", "", "0.81-1", ""),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(1,c(-0.0555, 0.0555, 0.1665, 0.2775, 0.3885, 0.5, 0.611, 0.722, 0.833, 0.944, 1.055),
              c("", "", "", "", "", "", "", "", "", "", ""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)

            axis(2,c(-0.0555,   0, 0.0555, 0.111, 0.1665, 0.222, 0.2775, 0.333, 0.3885, 0.444, 0.4995, 0.555, 0.611, 0.6665, 0.722, 0.7775, 0.833, 0.8885, 0.944, 0.9995, 1.055),
                  c("", "0-0.18", "", "0.09-0.27", "", "0.18-0.36", "", "0.27-0.45", "", "0.36-0.54", "", "0.45-0.63", "", "0.54-0.72", "", "0.63-0.81", "", "0.72-0.9", "", "0.81-1", ""),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(2,c(-0.0555, 0.0555, 0.1665, 0.2775, 0.3885, 0.5, 0.611, 0.722, 0.833, 0.944, 1.055),
              c("", "", "", "", "", "", "", "", "", "", ""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)
          }
        }

      dev.off()
    }
  }
  return(print("Done, in the specified folder"))
  # return(foldername)
}

combineeditsingles <- function (
  # inheritancestyle = 1,
  # bias = 1,
  metricssexpop = 1, # only allowed 1-4 (p1m, p2m, p1f, p2f)
  otherpopstyle = 3, # 1 = low, 2 = high (for lmh & nw stuff round May 2019) # 3 = third group has same number of tested conditions (slices) as the first two groups.
  edit = FALSE,
  lmhvnw = FALSE,
  heatmapfile = heatmapoutput
) {

inheritancestyle <- paste0(heatmapfile$inheritance, "inh")
bias <- paste0(heatmapfile$diffcurstartbias, "Bias")
whichbias <- c("maleBias", "femaleBias", "pop1Bias")
popbias <- c("FemalePop", "MalePop", "Popula2")[which(whichbias == bias)]
#   source("/home/parker/Documents/projects/curmodel_pcomp1/Code/curiosity-code/scripts/Source_Magick_Functions.R")
# Access the same subdirectory where the individual images are stored
#  heatmap_sourcefolder <- file.path("results", "Heatmaps", "output_objects")
  heatmap_sourcefolder <- file.path("results")

  sxmtpopcontainer <- c("EndCurValP1M",
                        "EndCurValP2M",
                        "EndCurValP1F",
                        "EndCurValP2F",
                        "EndSRpValP1M",
                        "EndSRpValP2M",
                        "EndSRpValP1F",
                        "EndSRpValP2F")



  curstartpatterncontainer <- c("lowMedHigh", "narrowWide", "lowHigh")

  # inheritancecontainer <- c("maleinh", "mothinh", "sameinh", "oppsinh", "sNTninh",
  #                           "sSTfinh", "sSFrinh", "sFrSinh", "sTfSinh", "sTnNinh",
  #                           "FfFfinh")
  # inheritancecontainer <- inheritancecontainer[inheritancestyle]

  titlesxmtpop <- c("Pop 1 Mal", "Pop 2 Mal",
                    "Pop 1 Fem", "Pop 2 Fem",
                    "Pop 1 Mal", "Pop 2 Mal",
                    "Pop 1 Fem", "Pop 2 Fem")

  titleinhstyle <- c("Male", "Female", "Same-Sex", "Opposite", "90M10F",
                     "75M25F", "60M40F", "40M60F", "25M75F", "10M90F",
                     "50-50")

  namesforotherpop <- c("slice_1", "slice_2", "slice_3")
  stylesforotherpop <- c("low", "high", "lmh")

  if (otherpopstyle == 3) { # LMH and NW are still split up, but pop 2 also has lmh and nw, so there's a new arrangement needed?
    if (lmhvnw == TRUE) {
      for (
        comparisonpattern in 1:2
      ) {
        titlebackgroundpop <- c("Low", "Med", "High")

        subpopulation <- c("p1mAC", "p2mAC", "p1fAC", "p2fAC", "p1mSR", "p2mSR", "p1fSR", "p2fSR")

        folderbias <- list.files(heatmap_sourcefolder)[
          which(sapply(list.files(heatmap_sourcefolder), function(x) (
          inheritancestyle %in% str_split(x, "_")[[1]][4])))
        ]


        #   for (i in 1:2) {

        namesforotherpop <- c("slice_1", "slice_2", "slice_3")
        stylesforotherpop <- c("low", "high", "lmh")

        firstbiasfolder <- file.path(heatmap_sourcefolder, folderbias[2], curstartpatterncontainer[comparisonpattern], whichpopbias[1])
        firstbiaslist <- list.files(file.path(heatmap_sourcefolder, folderbias[2], curstartpatterncontainer[comparisonpattern], whichpopbias[1]), pattern = sxmtpopcontainer[metricssexpop])
        secndbiasfolder <- file.path(heatmap_sourcefolder, folderbias[1], curstartpatterncontainer[comparisonpattern], whichpopbias[2])
        secndbiaslist <- list.files(file.path(heatmap_sourcefolder, folderbias[1], curstartpatterncontainer[comparisonpattern], whichpopbias[2]), pattern = sxmtpopcontainer[metricssexpop])
        if(length(firstbiaslist) == 3) {
          image_1 <- image_read(file.path(firstbiasfolder, firstbiaslist[1]))
          image_2 <- image_read(file.path(firstbiasfolder, firstbiaslist[2]))
          image_3 <- image_read(file.path(firstbiasfolder, firstbiaslist[3]))
          image_4 <- image_read(file.path(secndbiasfolder, secndbiaslist[1]))
          image_5 <- image_read(file.path(secndbiasfolder, secndbiaslist[2]))
          image_6 <- image_read(file.path(secndbiasfolder, secndbiaslist[3]))


          top_row <- image_append(c(image_1, image_2, image_3))
          bottom_row <- image_append(c(image_4, image_5, image_6))
          final_set <- image_append(c(top_row, bottom_row), stack = TRUE)

          final_set <- image_border(final_set, "white", "75x75")

          final_set <- image_annotate(
              final_set, paste0(titlesxmtpop[metricssexpop],
                                " Ending Traits - ",
                                titleinhstyle[inheritancestyle],
                                " AC Inheritance"),
              size="50",
              location = "+40+25")

          final_set <- image_annotate(
              final_set, paste0("Low-Med-High Background Population Starting Curiosity"),
              size="30",
              location = "+350+80")

          final_set <- image_annotate(
              final_set, paste0("Female Split        |        Male Split"),
              size="40",
              degrees=270,
              location = "+20+1055")

          final_set <- image_border(final_set, "white", "30x30")

          final_set <- image_annotate(
              final_set, paste0("Background Starting AC: Low        Medium        High"),
              size="35",
              location = "+275+1235")
        } else if (length(firstbiaslist) == 2) {
          image_1 <- image_read(file.path(firstbiasfolder, firstbiaslist[1]))
          image_2 <- image_read(file.path(firstbiasfolder, firstbiaslist[2]))
          image_3 <- image_read(file.path(secndbiasfolder, secndbiaslist[1]))
          image_4 <- image_read(file.path(secndbiasfolder, secndbiaslist[2]))



          top_row <- image_append(c(image_1, image_2))
          bottom_row <- image_append(c(image_3, image_4))
          final_set <- image_append(c(top_row, bottom_row), stack = TRUE)

          final_set <- image_border(final_set, "white", "75x75")

          final_set <- image_annotate(
              final_set, paste0(titlesxmtpop[metricssexpop],
                                " Ending Traits - ",
                                titleinhstyle[inheritancestyle],
                                " AC Inheritance"),
              size="50",
              location = "+40+25")

          final_set <- image_annotate(
              final_set, paste0("Narrow-Wide Background Population Starting Curiosity"),
              size="30",
              location = "+350+80")

          final_set <- image_annotate(
              final_set, paste0("Female Split        |        Male Split"),
              size="40",
              degrees=270,
              location = "+20+1055")

          final_set <- image_border(final_set, "white", "30x30")

          final_set <- image_annotate(
              final_set, paste0("Background Starting AC: Narrow             Wide"),
              size="35",
              location = "+275+1235")
        }



        # image_write(final_set, path = file.path(
        #     heatmap_sourcefolder, folderbias, paste0(
        #                 "Popula2", "_", "p1f",
        #                 "_measure_", "low",
        #                 "_background.png")))
          # This is where we edit the stuff we worked on!
          # There need to be ways to access the files made in the first half, and it should also contain everything within another control structure: "if(files exist in folder) {} else {stop("Great Job, oh wait this is an error message. Um, you should make sure the function is pointed at the right files. Are the right ones perhaps absent?")}"

        image_write(final_set, path = file.path(heatmap_sourcefolder, folderbias[1], paste0("BothSexes_", subpopulation[metricssexpop], "_measure_", inheritancestyle, "_", stylesforotherpop[otherpopstyle], "_background.png")))
      }
    } else {

      titlebackgroundpop <- c("Low", "Med", "High")
      # whichbias <- c("maleBias", "femaleBias", "pop1Bias")
      # whichpopbias <- c("FemalePop", "MalePop", "Popula2")

      subpopulation <- c("p1mAC", "p2mAC", "p1fAC", "p2fAC", "p1mSR", "p2mSR", "p1fSR", "p2fSR")

      folderbias <- list.files(heatmap_sourcefolder)[which(sapply(list.files(heatmap_sourcefolder), function(x) (inheritancestyle %in% str_split(x, "_")[[1]][4])))]

      # popbias <- whichpopbias[bias]

      #   for (i in 1:2) {


      lmhvnw <- 1
      if (
        bias == 1
      ) {
        firstbiasfolder <- file.path(heatmap_sourcefolder, folderbias[2], curstartpatterncontainer[lmhvnw], whichpopbias[1])
        firstbiaslist <- list.files(file.path(heatmap_sourcefolder, folderbias[2], curstartpatterncontainer[lmhvnw], whichpopbias[1]), pattern = sxmtpopcontainer[metricssexpop])
        secndbiasfolder <- file.path(heatmap_sourcefolder, folderbias[1], curstartpatterncontainer[lmhvnw], whichpopbias[2])
        secndbiaslist <- list.files(file.path(heatmap_sourcefolder, folderbias[1], curstartpatterncontainer[lmhvnw], whichpopbias[2]), pattern = sxmtpopcontainer[metricssexpop])
      } else {
        firstbiasfolder <- file.path(heatmap_sourcefolder, folderbias[1], curstartpatterncontainer[lmhvnw], whichpopbias[2])
        firstbiaslist <- list.files(file.path(heatmap_sourcefolder, folderbias[1], curstartpatterncontainer[lmhvnw], whichpopbias[2]), pattern = sxmtpopcontainer[metricssexpop])
        secndbiasfolder <- file.path(heatmap_sourcefolder, folderbias[2], curstartpatterncontainer[lmhvnw], whichpopbias[1])
        secndbiaslist <- list.files(file.path(heatmap_sourcefolder, folderbias[2], curstartpatterncontainer[lmhvnw], whichpopbias[1]), pattern = sxmtpopcontainer[metricssexpop])
      }


      image_1 <- image_read(file.path(firstbiasfolder, firstbiaslist[1]))
      image_2 <- image_read(file.path(firstbiasfolder, firstbiaslist[2]))
      image_3 <- image_read(file.path(firstbiasfolder, firstbiaslist[3]))
      image_4 <- image_read(file.path(secndbiasfolder, secndbiaslist[1]))
      image_5 <- image_read(file.path(secndbiasfolder, secndbiaslist[2]))
      image_6 <- image_read(file.path(secndbiasfolder, secndbiaslist[3]))

      if(edit == TRUE) {

        top_row <- image_append(c(image_1, image_2, image_3))
        bottom_row <- image_append(c(image_4, image_5, image_6))
        final_set <- image_append(c(top_row, bottom_row), stack = TRUE)

        final_set <- image_border(final_set, "white", "75x75")

        final_set <- image_annotate(
            final_set, paste0(titlesxmtpop[metricssexpop],
                              " Ending Traits - ",
                              titleinhstyle[inheritancestyle],
                              " AC Inheritance"),
            size="50",
            location = "+300+25")

        final_set <- image_annotate(
            final_set, paste0("Low-Med-High Background Population Starting Curiosity"),
            size="30",
            location = "+530+80")
        if (
          bias == 1
        ) {
          final_set <- image_annotate(
            final_set, paste0("Female Split                     |                    Male Split"),
            size="40",
            degrees=270,
            location = "+20+1055")
        } else {
          final_set <- image_annotate(
            final_set, paste0("Male Split                |                Female Split"),
            size="40",
            degrees=270,
            location = "+20+1055")
        }


        final_set <- image_border(final_set, "white", "30x30")

        final_set <- image_annotate(
            final_set, paste0("Background Starting AC:     Low                                Medium                                             High"),
            size="35",
            location = "+65+1235")


        # image_write(final_set, path = file.path(
        #     heatmap_sourcefolder, folderbias, paste0(
        #                 "Popula2", "_", "p1f",
        #                 "_measure_", "low",
        #                 "_background.png")))
          # This is where we edit the stuff we worked on!
          # There need to be ways to access the files made in the first half, and it should also contain everything within another control structure: "if(files exist in folder) {} else {print("Great Job, oh wait this is an error message. Um, you should make sure the function is pointed at the right files. Are the right ones perhaps absent?")}"

      } else {

        top_row <- image_append(c(image_1, image_2))
        bottom_row <- image_append(c(image_3, image_4))
        final_set <- image_append(c(top_row, bottom_row), stack = TRUE)
        # image_write(final_set, path = file.path(heatmap_sourcefolder, folderbias, paste0(popbias, "_", subpopulation[metricssexpop], "_measure_", stylesforotherpop[otherpopstyle], "_background.png")))

      }

      if(!(dir.exists(file.path(heatmap_sourcefolder, paste0("SexBiasedCurInhRange"))))) {dir.create(file.path(heatmap_sourcefolder, paste0("SexBiasedCurInhRange")))}

      image_write(final_set, path = file.path(heatmap_sourcefolder, paste0("SexBiasedCurInhRange"), paste0(inheritancestyle, "_", subpopulation[metricssexpop], "_measure_", stylesforotherpop[otherpopstyle], "_background.png")))
    }


  } else {

    titlebackgroundpop <- c("Low", "High")

    # whichbias <- c("maleBias", "femaleBias", "pop1Bias")
    # whichpopbias <- c("FemalePop", "MalePop", "Popula2")

    subpopulation <- c("p1m", "p2m", "p1f", "p2f")

    folderbias <- list.files(heatmap_sourcefolder)[
      which(sapply(list.files(heatmap_sourcefolder), function(x) (
      inheritancestyle %in% str_split(x, "_")[[1]][4] &&
      which (whichbias == bias) %in% str_split(x, "_")[[1]][5])))]

    # popbias <- whichpopbias[bias]

    #   for (i in 1:2) {

    # namesforotherpop <- c("slice_1", "slice_2", "slice_3")
    # stylesforotherpop <- c("low", "high")

    lowmedhighfolder <- file.path(heatmap_sourcefolder, folderbias, curstartpatterncontainer[1], popbias)
    narrowwidefolder <- file.path(heatmap_sourcefolder, folderbias, curstartpatterncontainer[2], popbias)

    image_1 <- image_read(file.path(lowmedhighfolder, paste0(sxmtpopcontainer[metricssexpop], "_", namesforotherpop[otherpopstyle], "_", popbias, ".png")))
    image_2 <- image_read(file.path(narrowwidefolder, paste0(sxmtpopcontainer[metricssexpop], "_", namesforotherpop[otherpopstyle], "_", popbias, ".png")))
    image_3 <- image_read(file.path(lowmedhighfolder, paste0(sxmtpopcontainer[metricssexpop+4], "_", namesforotherpop[otherpopstyle], "_", popbias, ".png")))
    image_4 <- image_read(file.path(narrowwidefolder, paste0(sxmtpopcontainer[metricssexpop+4], "_", namesforotherpop[otherpopstyle], "_", popbias, ".png")))

    if(edit == TRUE) {

      top_row <- image_append(c(image_1, image_2))
      bottom_row <- image_append(c(image_3, image_4))
      final_set <- image_append(c(top_row, bottom_row), stack = TRUE)

      final_set <- image_border(final_set, "white", "75x75")

      final_set <- image_annotate(
          final_set, paste0(titlesxmtpop[metricssexpop],
                            " Ending Traits - ",
                            titleinhstyle[inheritancestyle],
                            " AC Inheritance"),
          size="50",
          location = "+40+25")

      final_set <- image_annotate(
          final_set, paste0(titlebackgroundpop[otherpopstyle], " Background Population Starting Curiosity"),
          size="30",
          location = "+350+80")

      final_set <- image_annotate(
          final_set, paste0("Syllable Repertoire        |        Auditory Curiosity"),
          size="40",
          degrees=270,
          location = "+20+1055")

      final_set <- image_border(final_set, "white", "30x30")

      final_set <- image_annotate(
          final_set, paste0("Low/Medium/High        |        Narrow/Wide"),
          size="35",
          location = "+315+1235")


      # image_write(final_set, path = file.path(
      #     heatmap_sourcefolder, folderbias, paste0(
      #                 "Popula2", "_", "p1f",
      #                 "_measure_", "low",
      #                 "_background.png")))
        # This is where we edit the stuff we worked on!
        # There need to be ways to access the files made in the first half, and it should also contain everything within another control structure: "if(files exist in folder) {} else {print("Great Job, oh wait this is an error message. Um, you should make sure the function is pointed at the right files. Are the right ones perhaps absent?")}"

    } else {

      top_row <- image_append(c(image_1, image_2))
      bottom_row <- image_append(c(image_3, image_4))
      final_set <- image_append(c(top_row, bottom_row), stack = TRUE)
      # image_write(final_set, path = file.path(heatmap_sourcefolder, folderbias, paste0(popbias, "_", subpopulation[metricssexpop], "_measure_", stylesforotherpop[otherpopstyle], "_background.png")))

    }

    image_write(final_set, path = file.path(heatmap_sourcefolder, folderbias, paste0(popbias, "_", subpopulation[metricssexpop], "_measure_", inheritancestyle, "_", stylesforotherpop[otherpopstyle], "_background.png")))

  }



  return(print("done"))
}


stackmultiples <- function (
  inheritance = 1, # c("sameinh", "oppsinh", "maleinh", "mothinh")
  pattern = 1 # 1 = narrowWide, 2 = lowMedHigh
) {


  # maleInhMaleVFemaleBias

  sxmtpopcontainer <- c("EndCurValP1M",
                        "EndCurValP2M",
                        "EndCurValP1F",
                        "EndCurValP2F",
                        "EndSRpValP1M",
                        "EndSRpValP2M",
                        "EndSRpValP1F",
                        "EndSRpValP2F")

  heatmap_sourcefolder <- file.path("results", "Heatmaps", "output_objects")
  whichbias <- c("maleBias", "femaleBias")
  whichpopbias <- c("FemalePop", "MalePop")
  # folderbias <- list.files(heatmap_sourcefolder)[which(sapply(list.files(heatmap_sourcefolder), function(x) (inheritance %in% str_split(x, "_")[[1]][4] && whichbias[bias] %in% str_split(x, "_")[[1]][5])))]
  curstartpatterncontainer <- c("narrowWide", "lowMedHigh")
  # relevantFolder <- file.path(heatmap_sourcefolder, folderbias, curstartpatterncontainer[pattern])

  inheritancecontainer <- c("sameinh", "oppsinh", "maleinh", "mothinh")
  inheritance <- inheritancecontainer[inheritance]

  heatmap_sourcefolder <- file.path("results", "Heatmaps", "output_objects")

  output_folder <- file.path(heatmap_sourcefolder, paste0("Combined_", inheritance))# "_pattern_", curstartpatterncontainer[pattern]))
  if(!(dir.exists(output_folder))) {dir.create(output_folder)}
  if(!(dir.exists(file.path(output_folder, curstartpatterncontainer[pattern])))) {dir.create(file.path(output_folder, curstartpatterncontainer[pattern]))}

  malebias <- list.files(heatmap_sourcefolder)[which(sapply(list.files(heatmap_sourcefolder), function(x) (inheritance %in% str_split(x, "_")[[1]][4] && whichbias[1] %in% str_split(x, "_")[[1]][5])))]
  femsbias <- list.files(heatmap_sourcefolder)[which(sapply(list.files(heatmap_sourcefolder), function(x) (inheritance %in% str_split(x, "_")[[1]][4] && whichbias[2] %in% str_split(x, "_")[[1]][5])))]

  for (metsxpop in 1:8) {
    stackone <- CombineSingles(inheritance, 1, metsxpop, pattern)
    stacktwo <- CombineSingles(inheritance, 2, metsxpop, pattern)
    # stackone <- image_read(file.path(heatmap_sourcefolder, malebias, curstartpatterncontainer[pattern]), paste0(sxmtpopcontainer[metsxpop], "_", whichpopbias[1], ".png"))
    # stacktwo <- image_read(file.path(heatmap_sourcefolder, femsbias, curstartpatterncontainer[pattern]), paste0(sxmtpopcontainer[metsxpop], "_", whichpopbias[2], ".png"))
    thing <- image_append(c(stackone, stacktwo), stack = TRUE)
    image_write(thing, path = file.path(output_folder, curstartpatterncontainer[pattern]))
  }

  # stackone <- image_read(file.path(heatmap_sourcefolder, malebias, curstartpatterncontainer[pattern]),)
  # stacktwo <- image_read(file.path(heatmap_sourcefolder, femsbias, curstartpatterncontainer[pattern]),)


}

print("htmpdir, extractvardirs, remakestring, extractmeans, makeheatmapfile, individualfigures, combineeditsingles and stackmultiples loaded")
