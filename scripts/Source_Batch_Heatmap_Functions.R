find_the_dir <- function (
  pattern, #male
  source_dir # c("results", "five-by-five-vanilla_lowHigh_Background")
) {

  dirs_within_sourcedir <- list.dirs(file.path ("results", source_dir), recursive = FALSE)

  thing <- FALSE

  for (i in 1 : length (dirs_within_sourcedir)) {
    if (! (thing)) {
      stuff <- strsplit(strsplit(dirs_within_sourcedir, "inh")[[i]], "_")
      for (j in 1:length (stuff)) {
        whatever <- which (stuff[[j]] == pattern)
        if (length(whatever > 0)) {
          # print ("you found it!")
          # i <- append(i, j)
          # whatever <- append(whatever, i)
          found_it <- i
          thing <- TRUE
          break
        }
        # print ("thing")
      }
      if (thing) {break}
      # print ("stuff")
    } else {
      break
    }
    if (thing) {break}
    # print ("whatever")
  }
  # pattern_interrogation <- dirs_within_sourcedir[[1:length(dirs_within_sourcedir)]]

  final_dir <- file.path (dirs_within_sourcedir[[found_it]])

  return (final_dir) # "results/five-by-five-vanilla_lowHigh_Background/191111_slices_-_maleinh_pop1Bias"
}

plot_that_spectrum <- function (file_name, colorPalette) {

  x <- array(c(1:100, rep(9.5,100), rep(9.7,100), rep(9.9,100), rep(10.1,100), rep(10.3,100), rep(10.5,100), rep(10.7,100), rep(10.9,100), rep(11.1,100), rep(11.3,100)), c(100, 11))

  # sink(file = file.path("source", "temp", paste0(
  #       simnumber, "_console_copy.txt")), append = TRUE, split = TRUE)
  #     print(paste0("Sim Number ", strsplit(docnamez,
  #       "_")[[1]][2], " - storing data packet ",
  #       thousand_timesteps, " at ", Sys.time()))
  #     sink()
  # sink (file = file.path (file_name, "difference_heatmap_spectrum."))

  png (filename = file.path (
            # heatmap_sourcefolder, foldername$foldername, #inhoptions[inhstyle + 2],
            # paste0("slice_", slice), file_name),
            # slicedpop[3], file_name
            file_name),
          width = 554, height = 554, units = "px", pointsize = 12, bg = "white")



  # plot(x[,1], x[,2], col = colorseqmultpalette[[19]](100), pch = 1, cex = 1, axes = F, xlab = "", ylab = "")
  # plot(x[,1], x[,2], col = colorseqmultpalette[[19]](100), pch = 2, cex = 1, axes = F, xlab = "", ylab = "")
  # plot(x[,1], x[,2], col = colorseqmultpalette[[19]](100), pch = 3, cex = 1, axes = F, xlab = "", ylab = "")
  # plot(x[,1], x[,2], col = colorseqmultpalette[[19]](100), pch = 4, cex = 1, axes = F, xlab = "", ylab = "")
  # plot(x[,1], x[,2], col = colorseqmultpalette[[19]](100), pch = 5, cex = 1, axes = F, xlab = "", ylab = "")
  # plot(x[,1], x[,2], col = colorseqmultpalette[[19]](100), pch = 6, cex = 1, axes = F, xlab = "", ylab = "")
  # plot(x[,1], x[,2], col = colorseqmultpalette[[19]](100), pch = 7, cex = 1, axes = F, xlab = "", ylab = "")
  # plot(x[,1], x[,2], col = colorseqmultpalette[[19]](100), pch = 8, cex = 1, axes = F, xlab = "", ylab = "")
  # plot(x[,1], x[,2], col = colorseqmultpalette[[19]](100), pch = 9, cex = 1, axes = F, xlab = "", ylab = "")
  # plot(x[,1], x[,2], col = colorseqmultpalette[[19]](100), pch = 10, cex = 1, axes = F, xlab = "", ylab = "")
  # plot(x[,1], x[,2], col = colorseqmultpalette[[19]](100), pch = 11, cex = 1, axes = F, xlab = "", ylab = "")
  # plot(x[,1], x[,2], col = colorseqmultpalette[[19]](100), pch = 12, cex = 1, axes = F, xlab = "", ylab = "")
  # plot(x[,1], x[,2], col = colorseqmultpalette[[19]](100), pch = 13, cex = 1, axes = F, xlab = "", ylab = "")
  # plot(x[,1], x[,2], col = colorseqmultpalette[[19]](100), pch = 14, cex = 1, axes = F, xlab = "", ylab = "")
  plot(rep(x[,1],10), x[,2:11], col = colorseqmultpalette[[colorPalette]](100), pch = 15, cex = 1, axes = F, xlab = "", ylab = "", ylim = c(5,16), main = "Legend
  (heatmap 1 minus heatmap 2)")

  # legend(x = "bottomleft", legend = c("legend", "great", "this", "aw", "fuck", "now", "what", "goddammit", "cmon", "wat", "four", "score", "and", "seven", "years", "ago", "our", "forefathers", "arrived", "and", "threw", "a", "banger", "party", "sucks"), fill = "#efedf5", col = colorseqmultpalette[[19]](23), border = "#bcbddc", lty = 5, lwd = 1, pch = 15)
  # legend(x = "bottomleft", legend = array(c("-1", "-0.86", "-0.72", "-0.58", "-0.44", "-0.3", "-0.16", "-.02", ".02", "0.16", "0.3", "0.44", "0.58", "0.72", "0.86", "1"), c(4,4)), fill = "#efedf5", col = colorseqmultpalette[[19]](16), border = "#bcbddc", lty = 5, lwd = 1, pch = 15)

  # plot(x[,1], x[,2], col = colorseqmultpalette[[19]](100), pch = 16, cex = 1, axes = F, xlab = "", ylab = "")
  # plot(x[,1], x[,2], col = colorseqmultpalette[[19]](100), pch = 17, cex = 1, axes = F, xlab = "", ylab = "")
  # plot(x[,1], x[,2], col = colorseqmultpalette[[19]](100), pch = 18, cex = 1, axes = F, xlab = "", ylab = "")
  # plot(x[,1], x[,2], col = colorseqmultpalette[[19]](100), pch = 19, cex = 1, axes = F, xlab = "", ylab = "")
  # plot(x[,1], x[,2], col = colorseqmultpalette[[19]](100), pch = 20, cex = 1, axes = F, xlab = "", ylab = "")
  # plot(x[,1], x[,2], col = colorseqmultpalette[[19]](100), pch = 21, cex = 1, axes = F, xlab = "", ylab = "")
  # plot(x[,1], x[,2], col = colorseqmultpalette[[19]](100), pch = 22, cex = 1, axes = F, xlab = "", ylab = "")
  # plot(x[,1], x[,2], col = colorseqmultpalette[[19]](100), pch = 23, cex = 1, axes = F, xlab = "", ylab = "")
  # plot(x[,1], x[,2], col = colorseqmultpalette[[19]](100), pch = 24, cex = 1, axes = F, xlab = "", ylab = "")
  # plot(x[,1], x[,2], col = colorseqmultpalette[[19]](100), pch = 25, cex = 1, axes = F, xlab = "", ylab = "")

  # axis(1, c(-2.5, 4.5, 11.5, 18.5, 25.5, 32.5, 39.5, 46.5, 48.5, 55.5, 62.5, 69.5, 76.5, 83.5, 90.5, 97.5, 103.5), c("-1", "-0.86", "-0.72", "-0.58", "-0.44", "-0.3", "-0.16", "-0.08", "-0.02", "0.02", "0.16", "0.3", "0.44", "0.58", "0.72", "0.86", "1"),T,-11.5,NA,F,cex.axis = 0.7, tck = 0.015)
  axis(1, c(0.5, 7.5, 14.5, 21.5, 28.5, 35.5, 42.5, 49.5, 51.5, 58.5, 65.5, 72.5, 79.5, 86.5, 93.5, 100.5), c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""),T,-8.5,NA,F,cex.axis = 0.7, tck = -0.015)
  # axis(1, c(0.5, 7.5, 14.5, 21.5, 28.5, 35.5, 42.5, 47.5, 53.5, 58.5, 65.5, 72.5, 79.5, 86.5, 93.5, 100.5), c("-1", "-0.86", "-0.72", "-0.58", "-0.44", "-0.3", "-0.16", "-.02", ".02", "0.16", "0.3", "0.44", "0.58", "0.72", "0.86", "1"),T,-12.5,NA,F,cex.axis = 0.7, tck = -0.015)
  title(xlab = c("-1     -0.86    -0.72   -0.58   -0.44     -0.3     -0.16  -0.02  0.02   0.16     0.3     0.44      0.58      0.72    0.86         1 "), line = -8, cex.lab = 0.7)

  # axis(1, c(0.5, 7.5, 14.5, 21.5, 28.5, 35.5, 42.5, 49.5, 51.5, 58.5, 65.5, 72.5, 79.5, 86.5, 93.5, 100.5), c("-155", "-133", "-111", "-89", "-67", "-45", "-23", "-3", "3", "23", "45", "67", "89", "111", "133", "155"),T,-14.5,NA,F,cex.axis = 0.7, tck = 0.015)
  axis(1, c(0.5, 7.5, 14.5, 21.5, 28.5, 35.5, 42.5, 49.5, 51.5, 58.5, 65.5, 72.5, 79.5, 86.5, 93.5, 100.5), c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""),T,-12.8,NA,F,cex.axis = 0.7, tck = 0.015)
  title(xlab = c("-155     -133     -111      -89       -67      -45      -23        -3  3         23       45        67        89       111       133      155 "), line = -14.3, cex.lab = 0.7)

  # c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "")

  title(xlab = c("Syllable Repertoire Differences"), line = -15.3, cex.lab = 0.7)
  title(xlab = c("Curiosity Level Differences"), line = -7, cex.lab = 0.7)

  # axis(1, c(50), c(""), )
  # axis(1, c(1, 8, 15, 22, 29, 36, 43, 47.5, 52, 59, 66, 73, 80, 87, 94, 100.5), c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""),T,-10,NA,F,cex.axis = 0.8, tck = 0) # Curiosity Range
  # axis(1, c(4.5, 11.5, 18.5, 25.5, 32.5, 39.5, 46.5, 48.5, 55.5, 62.5, 69.5, 76.5, 83.5, 90.5, 97.5), c("", "", "", "", "", "", "-1", "1", "", "", "", "", "", "", ""),T,-10,NA,F,cex.axis = 0.3, tck = 0) # Syllable Range
  # saveRDS(output_heatmap, file.path ("results", paste0("Difference_Heatmaps_", first_source_directory, "_versus_", secnd_source_directory), paste0(source_pattern, "_inh"), paste0("Difference_Heatmap_", source_pattern, "_inheritance.RData")))
  # sink()
  dev.off()
}

heatmap_difference <- function (
  source_pattern = "male", # "moth", "same", "FfFf"
  first_source_names = c("five-by-five-vanilla_lowHigh_Background"),
  secnd_source_names = c("five-by-five-Inv3k_lowHigh_Background"),
  visualization = "absolute", # "absolute" - takes absolute value of differences between different conditions. "midpoint" - grades differences according to which of the two heatmaps has a greater value!
  replace = FALSE,
  special_source = FALSE,
#   foldername = list(
    #   diffcurstartbias = ,
    #   biassize = ,
    #   othersize =
#   ),
  UL = 10
) {
  if (length (source_pattern) > 1) {
    stop ("Trying to compare things along a categorical axis? Then pick one!")
  }
  if (first_source_names[1] != secnd_source_names[1]) {
    if (length (first_source_names) == 1) {
      first_source_directory <- file.path(first_source_names[1])
    } else if (length (first_source_names) == 2) {
      first_source_directory <- file.path(first_source_names[1], first_source_names[2])
    } else if (length (first_source_names) == 3) {
      first_source_directory <- file.path(first_source_names[1], first_source_names[2], first_source_names[3])
    } else if (length (first_source_names) == 4) {
      first_source_directory <- file.path(first_source_names[1], first_source_names[2], first_source_names[3], first_source_names[4])
    } else if (length (first_source_names) == 5) {
      first_source_directory <- file.path(first_source_names[1], first_source_names[2], first_source_names[3], first_source_names[4], first_source_names[5])
    } else if (length (first_source_names) == 6) {
      first_source_directory <- file.path(first_source_names[1], first_source_names[2], first_source_names[3], first_source_names[4], first_source_names[5], first_source_names[6])
    } else if (length (first_source_names) == 7) {
      first_source_directory <- file.path(first_source_names[1], first_source_names[2], first_source_names[3], first_source_names[4], first_source_names[5], first_source_names[6], first_source_names[7])
    } else if (length (first_source_names) == 8) {
      first_source_directory <- file.path(first_source_names[1], first_source_names[2], first_source_names[3], first_source_names[4], first_source_names[5], first_source_names[6], first_source_names[7], first_source_names[8])
    } else if (length (first_source_names) == 9) {
      first_source_directory <- file.path(first_source_names[1], first_source_names[2], first_source_names[3], first_source_names[4], first_source_names[5], first_source_names[6], first_source_names[7], first_source_names[8], first_source_names[9])
    } else if (length (first_source_names) == 10) {
      first_source_directory <- file.path(first_source_names[1], first_source_names[2], first_source_names[3], first_source_names[4], first_source_names[5], first_source_names[6], first_source_names[7], first_source_names[8], first_source_names[9], first_source_names[10])
    } else if (length (first_source_names) > UL) {
      stop ("make more categories for first directory! Too much directory for this lil function!")
    }

    if (length (secnd_source_names) == 1) {
      secnd_source_directory <- file.path(secnd_source_names[1])
    } else if (length (secnd_source_names) == 2) {
      secnd_source_directory <- file.path(secnd_source_names[1], secnd_source_names[2])
    } else if (length (secnd_source_names) == 3) {
      secnd_source_directory <- file.path(secnd_source_names[1], secnd_source_names[2], secnd_source_names[3])
    } else if (length (secnd_source_names) == 4) {
      secnd_source_directory <- file.path(secnd_source_names[1], secnd_source_names[2], secnd_source_names[3], secnd_source_names[4])
    } else if (length (secnd_source_names) == 5) {
      secnd_source_directory <- file.path(secnd_source_names[1], secnd_source_names[2], secnd_source_names[3], secnd_source_names[4], secnd_source_names[5])
    } else if (length (secnd_source_names) == 6) {
      secnd_source_directory <- file.path(secnd_source_names[1], secnd_source_names[2], secnd_source_names[3], secnd_source_names[4], secnd_source_names[5], secnd_source_names[6])
    } else if (length (secnd_source_names) == 7) {
      secnd_source_directory <- file.path(secnd_source_names[1], secnd_source_names[2], secnd_source_names[3], secnd_source_names[4], secnd_source_names[5], secnd_source_names[6], secnd_source_names[7])
    } else if (length (secnd_source_names) == 8) {
      secnd_source_directory <- file.path(secnd_source_names[1], secnd_source_names[2], secnd_source_names[3], secnd_source_names[4], secnd_source_names[5], secnd_source_names[6], secnd_source_names[7], secnd_source_names[8])
    } else if (length (secnd_source_names) == 9) {
      secnd_source_directory <- file.path(secnd_source_names[1], secnd_source_names[2], secnd_source_names[3], secnd_source_names[4], secnd_source_names[5], secnd_source_names[6], secnd_source_names[7], secnd_source_names[8], secnd_source_names[9])
    } else if (length (secnd_source_names) == 10) {
      secnd_source_directory <- file.path(secnd_source_names[1], secnd_source_names[2], secnd_source_names[3], secnd_source_names[4], secnd_source_names[5], secnd_source_names[6], secnd_source_names[7], secnd_source_names[8], secnd_source_names[9], secnd_source_names[10])
    } else if (length (secnd_source_names) > UL) {
      stop ("make more categories for second directory! Too much directory for this lil function!")
    }
  }

  # heatmap_array <- readRDS(file.path("results", "Heatmaps", "output_objects",
  #   foldername, list.files(path = file.path("results", "Heatmaps", "output_objects",
  #     foldername), pattern =
  #       "heatmap_output_-_")
  #   )
  # )



  first_heatmap <- readRDS (file.path(find_the_dir (pattern = source_pattern, source_dir = first_source_directory), list.files (find_the_dir (pattern = source_pattern, source_dir = first_source_directory), pattern = ".RData")))
  second_heatmap <- readRDS (file.path(find_the_dir (pattern = source_pattern, source_dir = secnd_source_directory), list.files (find_the_dir (pattern = source_pattern, source_dir = secnd_source_directory), pattern = ".RData")))
  if (visualization == "absolute") {
    output_curiosity <- abs(first_heatmap[,,,1:4] - second_heatmap[,,,1:4])
    output_sylreps <- (abs(first_heatmap[,,,5:8] - second_heatmap[,,,5:8]) / (156 - 1))
    output_heatmap <- abind(output_curiosity, output_sylreps, along = 4)
  } else if (visualization == "midpoint") {
    output_curiosity <- ((first_heatmap[,,,1:4] - second_heatmap[,,,1:4]) / 2) + 0.5
    output_sylreps <- ((first_heatmap[,,,5:8] - second_heatmap[,,,5:8]) / 2) + (155/2)
    output_heatmap <- abind(output_curiosity, output_sylreps, along = 4)
  }


  # if(!(dir.exists(file.path("results", foldername)))) {

  #     dir.create(file.path("results", foldername))}

  # if(!(file.exists(file.path(
  #   "results", foldername, paste0("heatmap_output_-_", inheritance,
  #   "inh_", diffcurstartbias, "Bias_", runstyle, ".RData")
  # )))) {

  # saveRDS(heatmap_array, file.path(
  #   "results",foldername, paste0("heatmap_output_-_", inheritance,
  #   "inh_", diffcurstartbias, "Bias_", runstyle, ".RData")

  # ))}

  fsd <- setdiff(strsplit(first_source_directory, "_")[[1]], strsplit(secnd_source_directory, "_")[[1]])
  ssd <- setdiff(strsplit(secnd_source_directory, "_")[[1]], strsplit(first_source_directory, "_")[[1]])

  first_source_directory <- setdiff(strsplit(fsd, "-")[[1]], strsplit(ssd, "-")[[1]])
  secnd_source_directory <- setdiff(strsplit(ssd, "-")[[1]], strsplit(fsd, "-")[[1]])

  # differences <-
  if (! (dir.exists (file.path ("results", paste0("Difference_Heatmaps_", first_source_directory, "_versus_", secnd_source_directory))))) {
      dir.create(file.path ("results", paste0("Difference_Heatmaps_", first_source_directory, "_versus_", secnd_source_directory)))
    }

    if (! (dir.exists (file.path ("results", paste0("Difference_Heatmaps_", first_source_directory, "_versus_", secnd_source_directory), paste0(source_pattern, "_inh"))))) {
      dir.create(file.path("results", paste0("Difference_Heatmaps_", first_source_directory, "_versus_", secnd_source_directory), paste0(source_pattern, "_inh")))
    }

  if (replace) {
    saveRDS(output_heatmap, file.path ("results", paste0("Difference_Heatmaps_", first_source_directory, "_versus_", secnd_source_directory), paste0(source_pattern, "_inh"), paste0("Difference_Heatmap_", source_pattern, "_inheritance.RData")))
  } else {
    if (! (file.exists (file.path ("results", paste0("Difference_Heatmaps_", first_source_directory, "_versus_", secnd_source_directory), paste0(source_pattern, "_inh"), paste0("Difference_Heatmap_", source_pattern, "_inheritance.RData"))))) {
      saveRDS(output_heatmap, file.path ("results", paste0("Difference_Heatmaps_", first_source_directory, "_versus_", secnd_source_directory), paste0(source_pattern, "_inh"), paste0("Difference_Heatmap_", source_pattern, "_inheritance.RData")))
    }
  }

  # plot_that_spectrum(file_name = file.path ("results", paste0("Difference_Heatmaps_", first_source_directory, "_versus_", secnd_source_directory)))

  output_heatmap <- list(
    # biassize = foldername$biassize,
    # othersize = foldername$othersize,
    # diffcurstartbias = foldername$diffcurstartbias,
    # output_heatmap = output_heatmap,
    foldername = file.path (paste0("Difference_Heatmaps_", first_source_directory, "_versus_", secnd_source_directory), paste0(source_pattern, "_inh"))
  )

  return (output_heatmap)
}

print("find_the_dir, plot_that_spectrum and heatmap_difference loaded")
