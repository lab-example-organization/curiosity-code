find_the_dir <- function (
  patternz, #male
  source_dir # c("results", "five-by-five-vanilla_lowHigh_Background")
) {

  dirs_within_sourcedir <- list.dirs(file.path ("results", source_dir), recursive = FALSE)
#   dirs_within_sourcedir <- list.dirs(file.path ("results", source_dir), pattern = patternz, recursive = FALSE)

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

#   final_dir <- file.path ("results", source_dir, dirs_within_sourcedir)

  return (final_dir) # "results/five-by-five-vanilla_lowHigh_Background/191111_slices_-_maleinh_pop1Bias"
}

plot_that_spectrum <- function (file_name, colorPalette, legend_scale, theme) {

  # x <- array(c(1:100, rep(9.5,100), rep(9.7,100), rep(9.9,100), rep(10.1,100), rep(10.3,100), rep(10.5,100), rep(10.7,100), rep(10.9,100), rep(11.1,100), rep(11.3,100)), c(100, 11))
  x <- array(c(1:100, rep(9.7,100), rep(10.1,100), rep(10.5,100), rep(10.9,100), rep(11.3,100), rep(11.7,100), rep(12.1,100), rep(12.5,100), rep(12.9,100), rep(13.3,100)), c(100, 11))

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
          width = 554, height = 277, units = "px", pointsize = 12, bg = "white")



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
  if (theme == "difference") {
    title_and_stuff <- c ("Legend
(heatmap 1 minus heatmap 2)")
  } else if (theme == "variance") {
    title_and_stuff <- c ("Legend
(variance scale)")
  }

  plot (rep (x[,1],10), x[,2:11], col = colorseqmultpalette[[colorPalette]](100), pch = 15, cex = 1, axes = F, xlab = "", ylab = "", ylim = c (5,16), main = title_and_stuff)

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
  if (legend_scale == "midpoint") {
    # axis(1, c(-2.5, 4.5, 11.5, 18.5, 25.5, 32.5, 39.5, 46.5, 48.5, 55.5, 62.5, 69.5, 76.5, 83.5, 90.5, 97.5, 103.5), c("-1", "-0.86", "-0.72", "-0.58", "-0.44", "-0.3", "-0.16", "-0.08", "-0.02", "0.02", "0.16", "0.3", "0.44", "0.58", "0.72", "0.86", "1"),T,-11.5,NA,F,cex.axis = 0.7, tck = 0.015)
    axis (1, c(0.5, 7.5, 14.5, 21.5, 28.5, 35.5, 42.5, 49.5, 51.5, 58.5, 65.5, 72.5, 79.5, 86.5, 93.5, 100.5), c ("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""),T,-4.1,NA,F,cex.axis = 0.7, tck = -0.02)
    # axis(1, c(0.5, 7.5, 14.5, 21.5, 28.5, 35.5, 42.5, 47.5, 53.5, 58.5, 65.5, 72.5, 79.5, 86.5, 93.5, 100.5), c("-1", "-0.86", "-0.72", "-0.58", "-0.44", "-0.3", "-0.16", "-.02", ".02", "0.16", "0.3", "0.44", "0.58", "0.72", "0.86", "1"),T,-12.5,NA,F,cex.axis = 0.7, tck = -0.015)
    title (xlab = c("-1       -0.86     -0.72    -0.58     -0.44       -0.3      -0.16  -0.02  0.02    0.16       0.3        0.44        0.58       0.72     0.86           1 "), line = -3.7, cex.lab = 0.7)

    # axis(1, c(0.5, 7.5, 14.5, 21.5, 28.5, 35.5, 42.5, 49.5, 51.5, 58.5, 65.5, 72.5, 79.5, 86.5, 93.5, 100.5), c("-155", "-133", "-111", "-89", "-67", "-45", "-23", "-3", "3", "23", "45", "67", "89", "111", "133", "155"),T,-14.5,NA,F,cex.axis = 0.7, tck = 0.015)
    axis (1, c(0.5, 7.5, 14.5, 21.5, 28.5, 35.5, 42.5, 49.5, 51.5, 58.5, 65.5, 72.5, 79.5, 86.5, 93.5, 100.5), c ("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""),T,-7.6,NA,F,cex.axis = 0.7, tck = 0.02)
    title (xlab = c("-155       -133       -111        -89        -67         -45        -23          -3  3            23          45          67           89         111          133       155 "), line = -8.9, cex.lab = 0.7)

    title (xlab = c("Syllable Repertoire Differences"), line = -10, cex.lab = 0.7)
    title (xlab = c("Curiosity Level Differences"), line = -2.5, cex.lab = 0.7)


  } else if (legend_scale == "variance") {
    # axis(1, c(0.5, 10.5, 20.5, 30.5, 40.5, 50.5, 60.5, 70.5, 80.5, 90.5, 100.5), c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"),T,-7.6,NA,F,cex.axis = 0.7, tck = 0.015)
    axis (1, c (0.5, 10.5, 20.5, 30.5, 40.5, 50.5, 60.5, 70.5, 80.5, 90.5, 100.5), c ("", "", "", "", "", "", "", "", "", "", ""),T,-7.6,NA,F,cex.axis = 0.7, tck = 0.015)
    title (xlab = c("0                0.1                0.2                0.3                0.4                0.5                0.6                0.7                0.8                0.9                1"), line = -9.7, cex.lab = 0.7)
    axis (1, c (5.5, 15.5, 25.5, 35.5, 45.5, 55.5, 65.5, 75.5, 85.5, 95.5), c ("0.05", "0.15", "0.25", "0.35", "0.45", "0.55", "0.65", "0.75", "0.85", "0.95"),T,-4.1,NA,F,cex.axis = 0.7, tck = -0.015)
    # title(xlab = c("-155       -133       -111        -89        -67         -45         -23          -3  3           23           45          67          89          111          133        155 "), line = -10.3, cex.lab = 0.7)
  }


  # c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "")


  # axis(1, c(50), c(""), )
  # axis(1, c(1, 8, 15, 22, 29, 36, 43, 47.5, 52, 59, 66, 73, 80, 87, 94, 100.5), c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""),T,-10,NA,F,cex.axis = 0.8, tck = 0) # Curiosity Range
  # axis(1, c(4.5, 11.5, 18.5, 25.5, 32.5, 39.5, 46.5, 48.5, 55.5, 62.5, 69.5, 76.5, 83.5, 90.5, 97.5), c("", "", "", "", "", "", "-1", "1", "", "", "", "", "", "", ""),T,-10,NA,F,cex.axis = 0.3, tck = 0) # Syllable Range
  # saveRDS(output_heatmap, file.path ("results", paste0("Difference_Heatmaps_", first_source_directory, "_versus_", secnd_source_directory), paste0(source_pattern, "_inh"), paste0("Difference_Heatmap_", source_pattern, "_inheritance.RData")))
  # sink()
  dev.off()
}

# plot_that_spectrum (file.path ("spectra", "variance.png"), 21, "variance", "variance") ###

# plot_that_spectrum (file.path ("spectra", "midpoint.png"), 19, "midpoint", "difference") ###

heatmap_difference <- function (
  source_pattern = "male", # "moth", "same", "FfFf"
  first_source_names = c ("five-by-five-vanilla_lowHigh_Background"),
  secnd_source_names = c ("five-by-five-Inv3k_lowHigh_Background"),
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
    stop ("Trying to compare things using a category? Then pick a category!")
  }
  first_source_directory <- file.path (first_source_names[1])
  secnd_source_directory <- file.path (secnd_source_names[1])
  if (length (first_source_names) > 1) {
    for (i in 2:length (first_source_names)) {
      first_source_directory <- file.path (first_source_directory, first_source_names[i])
    }
  }
  if (length (secnd_source_names) > 1) {
    for (j in 2:length (secnd_source_names)) {
      secnd_source_directory <- file.path (secnd_source_directory, secnd_source_names[j])
    }
  }


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

  if (! (dir.exists (file.path ("results", "DifferenceHeatmaps")))) {dir.create (file.path ("results", "DifferenceHeatmaps"))}

  if (! (dir.exists (file.path ("results", "DifferenceHeatmaps", paste0("Difference_Heatmaps_", first_source_directory, "_versus_", secnd_source_directory))))) {
      dir.create(file.path ("results", "DifferenceHeatmaps", paste0("Difference_Heatmaps_", first_source_directory, "_versus_", secnd_source_directory)))
    }

    if (! (dir.exists (file.path ("results", "DifferenceHeatmaps", paste0("Difference_Heatmaps_", first_source_directory, "_versus_", secnd_source_directory), paste0(source_pattern, "_inh"))))) {
      dir.create(file.path("results", "DifferenceHeatmaps", paste0("Difference_Heatmaps_", first_source_directory, "_versus_", secnd_source_directory), paste0(source_pattern, "_inh")))
    }

  if (replace) {
    saveRDS(output_heatmap, file.path ("results", "DifferenceHeatmaps", paste0("Difference_Heatmaps_", first_source_directory, "_versus_", secnd_source_directory), paste0(source_pattern, "_inh"), paste0("Difference_Heatmap_", source_pattern, "_inheritance.RData")))
  } else {
    if (! (file.exists (file.path ("results", "DifferenceHeatmaps", paste0("Difference_Heatmaps_", first_source_directory, "_versus_", secnd_source_directory), paste0(source_pattern, "_inh"), paste0("Difference_Heatmap_", source_pattern, "_inheritance.RData"))))) {
      saveRDS(output_heatmap, file.path ("results", "DifferenceHeatmaps", paste0("Difference_Heatmaps_", first_source_directory, "_versus_", secnd_source_directory), paste0(source_pattern, "_inh"), paste0("Difference_Heatmap_", source_pattern, "_inheritance.RData")))
    }
  }

  # plot_that_spectrum(file_name = file.path ("results", paste0("Difference_Heatmaps_", first_source_directory, "_versus_", secnd_source_directory)))

  output_heatmap <- list(
    # biassize = foldername$biassize,
    # othersize = foldername$othersize,
    # diffcurstartbias = foldername$diffcurstartbias,
    # output_heatmap = output_heatmap,
    foldername = file.path ("DifferenceHeatmaps", paste0("Difference_Heatmaps_", first_source_directory, "_versus_", secnd_source_directory), paste0(source_pattern, "_inh"))
  )

  return (output_heatmap)
}

print_regex_num_range <- function (
    num_range = "0000-0001"
) {

    # Beginning vars to sort out
    first_term <- str_split(str_split(num_range, "-")[[1]][1], "")
    secnd_term <- str_split(str_split(num_range, "-")[[1]][2], "")

    # the first number is smaller than the second number.
    if (as.numeric(paste(first_term[[1]], collapse = "")) >= as.numeric(paste(secnd_term[[1]], collapse = ""))) {
        stop ("first number needs to be smaller than the second number")
    }

    # Fill in potentially smaller first number with sufficient leading zeroes
    if (length (first_term[[1]]) < length (secnd_term[[1]])) {
        difference_of_length <- length (secnd_term[[1]]) - length (first_term[[1]])
        first_term[[1]] <- c(rep("0", difference_of_length), first_term[[1]])
    }

    # Working number data structure
    # zv <- array (c(as.numeric(first_term[[1]]), as.numeric(secnd_term[[1]])), c(2, max (c(length(first_term[[1]]), length (secnd_term[[1]])))))
    zv <- matrix (c(as.numeric(first_term[[1]]), as.numeric(secnd_term[[1]])), 2, max (c(length(first_term[[1]]), length (secnd_term[[1]]))), byrow = T)

    # substitute list calls for matrix calls


    ### This function lives and breathes on "append" as the builder function;
    ### each step in the control flow below is slowly building the ending
    ### character string depending on the relationship of the two terms.

    # Opening up the variable that will returned
    output_object <- vector (mode = "character", length = 1)

    # start with "*_"
    output_object <- paste0 ("*_")

        # number of digits stops at 1
        if (length (zv[2,]) == 1) {
            # append "[1-2]_", where 1 is the first term and 2 is the second term ### "*_[1-2]_"
            append (output_object, paste0 ("[", zv[1,1], "-", zv[2,1], "]_"))
        # number of digits stops at 2
        } else if (length (zv[2,]) == 2) {
            # append "1" ### "*_1"
            append (output_object, paste0(zv[1,1]))
            # 1X-4X
            if (zv[1,1] + 1 < zv[2,1]) {
                # 1X where X < 9
                if (as.numeric (zv[1,2]) < 9) {
                    # append "[X-9]_|*_[(1+1)-(4-1)][0-9]_|*_4" ### "*_1[X-9]_|*_[(1+1)-(4-1)][0-9]_|*_4"
                    append (output_object, paste0 ("[", zv[1,2], "-9]_|*_[", zv[1,1] + 1, "-", zv[2,1] - 1, "][0-9]_|*_", zv[2,1]))
                    # 2X where X > 0
                    if (as.numeric (zv[2,2]) > 0) {
                        # append "[0-X]_" ### "*_1[X-9]_|*_[(1+1)-(4-1)][0-9]_|*_4[0-X]_"
                        append (output_object, paste0 ("[0-", zv[2,2], "]_"))
                    # otherwise 2X where X = 0
                    } else {
                        # append "0_" ###  ### "*_1[X-9]_|*_[(1+1)-(4-1)][0-9]_|*_40_"
                        append (output_object, "0_")
                    }
                # 1X where X = 9
                } else {
                    # append "X_|*_[(1+1)-(4-1)][0-9]_|*_4" ### "*_1X_|*_[(1+1)-(4-1)][0-9]_|*_4"
                    append (output_object, paste0 (zv[1,2], "_|*_[", zv[1,1] + 1, "-", zv[2,1] - 1, "][0-9]_|*_", zv[2,1]))
                    # 2X where X > 0
                    if (as.numeric (zv[2,2]) > 0) {
                        # append "[0-X]_" ### "*_1X_|*_[(1+1)-(4-1)][0-9]_|*_4[0-X]_"
                        append (output_object, paste0 ("[0-", zv[2,2], "]_"))
                    # 2X where X = 0
                    } else {
                        # append "0_" ### "*_1X_|*_[(1+1)-(4-1)][0-9]_|*_40_"
                        append (output_object, "0_")
                    }
                }
            # 1X-2X
            } else if (zv[1,1] + 1 == zv[2,1]) {
                # 1X where X < 9
                if (as.numeric (zv[1,2]) < 9) {
                    # append "[X-9]_|*_2" ### "*_1[X-9]_|*_2"
                    append (output_object, paste0 ("[", zv[1,2], "-9]_|*_", zv[2,1]))
                    # 2X where X > 0
                    if (as.numeric (zv[2,2]) > 0) {
                        # append "[0-X]_" ### "*_1[X-9]_|*_2[0-X]_"
                        append (output_object, paste0 ("[0-", zv[2,2], "]_"))
                    # otherwise 2X where X = 0
                    } else {
                        # append "0_" ###  ### "*_1[X-9]_|*_20_"
                        append (output_object, "0_")
                    }
                # 1X where X = 9
                } else {
                    # append "9_|*_2" ### "*_19_|*_2"
                    append (output_object, paste0 (zv[1,2], "_|*_", zv[2,1]))
                    # 2X where X > 0
                    if (as.numeric (zv[2,2]) > 0) {
                        # append "[0-X]_" ### "*_19_|*_2[0-X]_"
                        append (output_object, paste0 ("[0-", zv[2,2], "]_"))
                    # 2X where X = 0
                    } else {
                        # append "0_" ### "*_19_|*_20_"
                        append (output_object, paste0("0_"))
                    }
                }
            # 1X -1Y
            } else if (as.numeric (zv[1,1]) == as.numeric(zv[2,1])) {
                # append "[X-Y]_" ### "*_1[X-Y]_"
                append (output_object, paste0 ("[", zv[1,2], "-", zv[2,2], "]_"))
            }
        # number of digits stops at 3 ### at this point, we have output_object = "*_"
        } else if (length (zv[2,]) == 3) {
            # append "1" ### "*_1"
            output_object <- append (output_object, paste0 (zv[1,1]))
            # 1XY-4ZA
            if (zv[1,1] + 1 < as.numeric(zv[2,1])) {
                # 1X where X < 9
                if (as.numeric (zv[1,2]) < 9) {
                    # append "X" ### "*_1X"
                    output_object <- append (output_object, paste0 (zv[1,2]))
                    # 1XY where Y < 9
                    if (as.numeric (zv[1,3]) < 9) {
                        # append "[Y-9]_|*_1[(X+1)-9][0-9]_|*_[(1+1)-(4-1)][0-9][0-9]_|*_4" ### "*_1X[Y-9]_|*_1[(X+1)-9][0-9]_|*_[(1+1)-(4-1)][0-9][0-9]_|*_4"
                        output_object <- append(output_object, paste0 ("[", zv[1,3], "-9]_|*_", zv[1,1], "[", as.numeric(zv[1,2]) + 1, "-9][0-9]_|*_[", zv[1,1] + 1, "-", zv[2,1] - 1, "][0-9][0-9]_|*_", zv[2,1]))
                        # 4Z where Z > 0
                        if (as.numeric (zv[2,2]) > 0) {
                            # append "[0-(Z-1)][0-9]_|*_4Z" ### "*_1X[Y-9]_|*_1[(X+1)-9][0-9]_|*_[(1+1)-(4-1)][0-9][0-9]_|*_4[0-(Z-1)][0-9]_|*_4Z"
                            output_object <- append (output_object, paste0 ("[0-", zv[2,2] - 1, "][0-9]_|*_", zv[2,1], zv[2,2]))
                            # 4ZA where A > 0
                            if (as.numeric (zv[2,3]) > 0) {
                                # append "[0-A]_" ### "*_1X[Y-9]_|*_1[(X+1)-9][0-9]_|*_[(1+1)-(4-1)][0-9][0-9]_|*_4[0-(Z-1)][0-9]_|*_4Z[0-A]_"
                                output_object <- append (output_object, paste0 ("[0-", zv[2,3], "]_"))
                            # 4ZA where A == 0
                            } else {
                                # append "0_" ### "*_1X[Y-9]_|*_1[(X+1)-9][0-9]_|*_[(1+1)-(4-1)][0-9][0-9]_|*_4[0-(Z-1)][0-9]_|*_4Z0_"
                                output_object <- append (output_object, paste0 ("0_"))
                            }
                        # 4Z where Z == 0
                        } else {
                            # append "0" ### "*_1X[Y-9]_|*_1[(X+1)-9][0-9]_|*_[(1+1)-(4-1)][0-9][0-9]_|*_40"
                            output_object <- append (output_object, paste0 ("0"))
                            # 4ZA where A > 0
                            if (as.numeric (zv[2,3]) > 0) {
                                # append "[0-A]_" ### "*_1X[Y-9]_|*_1[(X+1)-9][0-9]_|*_[(1+1)-(4-1)][0-9][0-9]_|*_40[0-A]_"
                                output_object <- append (output_object, paste0 ("[0-", zv[2,3], "]_"))
                            # 4ZA where A == 0
                            } else {
                                # append "0_" ### "*_1X[Y-9]_|*_1[(X+1)-9][0-9]_|*_[(1+1)-(4-1)][0-9][0-9]_|*_400_"
                                output_object <- append (output_object, paste0 ("0_"))
                            }
                        }
                    # 1XY where Y == 9
                    }

                }
            # 1XY-2ZA
            } else if (zv[1,1] + 1 == zv[2,1]) {
                # 1XY where X is less than 9
                if (as.numeric (zv[1,2]) < 9) {
                    # append "X" ### "*_1X"
                    output_object <- append (output_object, paste0 (zv[1,2]))
                    #1XY where Y is less than 9
                    if (as.numeric (zv[1,3]) < 9) {
                        # append "[Y-9]_|*_1[(X+1)-9][0-9]_|*_2" ### "*_1X[Y-9]_|*_1[(X+1)-9][0-9]_|*_2"
                        output_object <- append (output_object, paste0 ("[", zv[1,3], "-9]_|*_", zv[1,1], "[", zv[1,2] + 1, "-9][0-9]_|*_", zv[2,1]))
                        # 2ZA where Z > 0
                        if (as.numeric (zv[2,2]) > 0) {
                            # append "0[0-9]_|*_2[1-(Z-1)][0-9]_|*_2Z" ### "*_1X[Y-9]_|*_1[(X+1)-9][0-9]_|*_20[0-9]_|*_2[1-(Z-1)][0-9]_|*_2Z"
                            output_object <- append (output_object, paste0 ("0[0-9]_|*_", zv[2,1], "[1-", zv[2,2] - 1, "][0-9]_|*_", zv[2,1], zv[2,2]))
                            # 2ZA where A > 0
                            if (as.numeric (zv[2,3]) > 0) {
                                # append "[0-A]_" ### "*_1X[Y-9]_|*_1[(X+1)-9][0-9]_|*_20[0-9]_|*_2[1-(Z-1)][0-9]_|*_2Z[0-A]_"
                                output_object <- append (output_object, paste0 ("[0-", zv[2,3], "]_"))
                            # 2ZA where A == 0
                            } else {
                                # append "0_" ### "*_1X[Y-9]_|*_1[(X+1)-9][0-9]_|*_20[0-9]_|*_2[1-(Z-1)][0-9]_|*_2Z0_"
                                output_object <- append (output_object, paste0 ("0_"))
                            }
                        # 2ZA where Z == 0
                        } else {
                            # append "0" ### "*_1X[Y-9]_|*_1[(X+1)-9][0-9]_|*_20"
                            output_object <- append (output_object, paste0 ("0"))
                            # 2ZA where A > 0
                            if (as.numeric (zv[2,3]) > 0) {
                                # append "[0-A]_" ### *_1X[Y-9]_|*_1[(X+1)-9][0-9]_|*_20[0-A]_"
                                output_object <- append (output_object, paste0 ("[0-", zv[2,3], "]_"))
                            # 2ZA where A == 0
                            } else {
                                # append "0_" ### "*_1X[Y-9]_|*_1[(X+1)-9][0-9]_|*_200_"
                                output_object <- append (output_object, paste0 ("0_"))
                            }
                        }

                    # 1XY where Y == 9
                    }
                }
            # 1XY-1ZA
            } else if (zv[1,1] == zv[2,1]) {
                # 11X-14X
                if (zv[1,2] + 1 < zv[2,2]) {
                    # 11X where X < 9
                    # append "1[X-9]_|*_1[(1+1)-(4-1)][0-9]_|*_14" ### "*_11[X-9]_|*_1[(1+1)-(4-1)][0-9]_|*_14"
                    output_object <- append (output_object, paste0 (zv[1,2], "[", zv[1,3], "-9]_|*_", zv[1,1], "[", zv[1,2] + 1, "-", zv[2,2] - 1, "][0-9]_|*_", zv[2,1], zv[2,2]))
                    # 14X where X > 0
                    if (zv[2,3] > 0) {
                        # append "[0-X]_" ### "*_11[X-9]_|*_1[(1+1)-(4-1)][0-9]_|*_14[0-X]_"
                        output_object <- append (output_object, paste0 ("[0-", zv[2,3], "]_"))
                    # otherwise 14X where X = 0
                    } else {
                        # append "0_" ###  ### "*_11[X-9]_|*_1[(1+1)-(4-1)][0-9]_|*_140_"
                        output_object <- append (output_object, "0_")
                    }
                    # 11X where X = 9
                # 11X-12X
                } else if (zv[1,2] + 1 == zv[2,2]) {
                    # 11X where X < 9
                    # append "1[X-9]_|*_12" ### "*_11[X-9]_|*_12"
                    output_object <- append (output_object, paste0 (zv[1,2], "[", zv[1,3], "-9]_|*_", zv[2,1], zv[2,2]))
                    # 12X where X > 0
                    if (zv[2,3] > 0) {
                        # append "[0-X]_" ### "*_11[X-9]_|*_12[0-X]_"
                        output_object <- append (output_object, paste0 ("[0-", zv[2,3], "]_"))
                    # otherwise 12X where X = 0
                    } else {
                        # append "0_" ###  ### "*_11[X-9]_|*_120_"
                        output_object <- append (output_object, "0_")
                    }
                    #
                # 11X -11Y
                } else if (zv[1,2] == zv[2,2]) {
                    # append "1[X-Y]_" ### "*_11[X-Y]_"
                    output_object <- append (output_object, paste0 (zv[1,2], "[", zv[1,3], "-", zv[2,3], "]_"))
                }
            }
        } else if (length (zv[2,]) == 4) {
            # append "1" ### "*_1"
            output_object <- append (output_object, paste0 (zv[1,1]))
            # 1XXX-4XXX
            if (zv[1,1] + 1 < zv[2,1]) {
                # append "AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_[(1+1)-(4-1)][0-9][0-9][0-9]_|*_4" ### "*_1AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_[(1+1)-(4-1)][0-9][0-9][0-9]_|*_4"
                output_object <- append (output_object, paste0 (zv[1,2], zv[1,3], "[", zv[1,4], "-9]_|*_", zv[1,1], zv[1,2], "[", zv[1,3], "-9][0-9]_|*_", zv[1,1], "[", zv[1,2], "-9][0-9][0-9]_|*_[", zv[1,1] + 1, "-", zv[2,1] - 1, "][0-9][0-9][0-9]_|*_", zv[2,1]))
                # 4DEF where D > 0
                if (zv[2,2] > 0) {
                    # append "[0-(D-1)][0-9][0-9]_|*_4D" ### "*_1AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_[(1+1)-(4-1)][0-9][0-9][0-9]_|*_4[0-(D-1)][0-9][0-9]_|*_4D"
                    output_object <- append (output_object, paste0 ("[0-", zv[2,2] - 1, "][0-9][0-9]_|*_", zv[2,1], zv[2,2]))
                    # 4DEF where E > 0
                    if (as.numeric (zv[2,3]) > 0) {
                        # append "[0-(E-1)][0-9]_|*_4DE" ### "*_1AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_[(1+1)-(4-1)][0-9][0-9][0-9]_|*_4[0-(D-1)][0-9][0-9]_|*_4D[0-(E-1)][0-9]_|*_4DE"
                        output_object <- append (output_object, paste0 ("[0-", zv[2,3] - 1, "][0-9]_|*_", zv[2,1], zv[2,2], zv[2,3]))
                        # 4DEF where F > 0
                        if (as.numeric (zv[2,4]) > 0) {
                            # append "[0-F]_" ### "*_1AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_[(1+1)-(4-1)][0-9][0-9][0-9]_|*_4[0-(D-1)][0-9][0-9]_|*_4D[0-(E-1)][0-9]_|*_4DE[0-F]_"
                            output_object <- append (output_object, paste0 ("[0-", zv[2,4], "]_"))
                        # 4DEF where F == 0
                        } else {
                            # append "0_" ### "*_1AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_[(1+1)-(4-1)][0-9][0-9][0-9]_|*_4[0-(D-1)][0-9][0-9]_|*_4D[0-(E-1)][0-9]_|*_4DE0_"
                            output_object <- append (output_object, paste0 ("0_"))
                        }
                    # 4DEF where E == 0
                    } else if (zv[2,3] == 0) {
                        # append "0" ### "*_1AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_[(1+1)-(4-1)][0-9][0-9][0-9]_|*_4[0-(D-1)][0-9][0-9]_|*_4D0"
                        output_object <- append (output_object, paste0 ("0"))
                        # 4DEF where F > 0
                        if (zv[2,4] > 0) {
                            output_object <- append (output_object, paste0 ("[0-", zv[2,4], "]_"))
                        # 4DEF where F == 0
                        } else {
                            output_object <- append (output_object, paste0 ("0_"))
                        }
                    }
                # 4DEF where D == 0
                } else if (zv[2,2] == 0) {
                    # append "0" ### "*_1AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_[(1+1)-(4-1)][0-9][0-9][0-9]_|*_40"
                    output_object <- append (output_object, paste0 ("0"))
                    # 4DEF where E > 0
                    if (zv[2,3] > 0) {
                        # append "[0-(E-1)][0-9]_|*_40E" ### "*_1AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_[(1+1)-(4-1)][0-9][0-9][0-9]_|*_40[0-(E-1)][0-9]_|*_40E"
                        output_object <- append (output_object, paste0 ("[0-", zv[2,3] - 1, "][0-9]_|*_", zv[2,1], zv[2,2], zv[2,3]))
                        # 4DEF where F > 0
                        if (zv[2,4] > 0) {
                            output_object <- append (output_object, paste0 ("[0-", zv[2,4], "]_"))
                        # 4DEF where F == 0
                        } else {
                            output_object <- append (output_object, paste0 ("0_"))
                        }
                    # 4DEF where E == 0
                    } else if (zv[2,3] == 0) {
                        # append "0" ### "*_1AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_[(1+1)-(4-1)][0-9][0-9][0-9]_|*_400"
                        output_object <- append (output_object, paste0 ("0"))
                        # 4DEF where F > 0
                        if (zv[2,4] > 0) {
                            output_object <- append (output_object, paste0 ("[0-", zv[2,4], "]_"))
                        # 4DEF where F == 0
                        } else {
                            output_object <- append (output_object, paste0 ("0_"))
                        }
                    }
                }
            # 1XXX-2XXX
            } else if (zv[1,1] + 1 == zv[2,1]) {
                # append "AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_2" ### "*_1AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_2"
                output_object <- append (output_object, paste0 (zv[1,2], zv[1,3], "[", zv[1,4], "-9]_|*_", zv[1,1], zv[1,2], "[", zv[1,3], "-9][0-9]_|*_", zv[1,1], "[", zv[1,2], "-9][0-9][0-9]_|*_", zv[2,1]))
                # 2DEF where D > 0
                if (zv[2,2] > 0) {
                    # append "[0-(D-1)][0-9][0-9]_|*_2D" ### "*_1AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_2[0-(D-1)][0-9][0-9]_|*_2D"
                    output_object <- append (output_object, paste0 ("[0-", zv[2,2] - 1, "][0-9][0-9]_|*_", zv[2,1], zv[2,2]))
                    # 2DEF where E > 0
                    if (zv[2,3] > 0) {
                        # append "[0-(E-1)][0-9]_|*_2DE" ### "*_1AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_2[0-(D-1)][0-9][0-9]_|*_2D[0-(E-1)][0-9]_|*_2DE"
                        output_object <- append (output_object, paste0 ("[0-", zv[2,3] - 1, "][0-9]_|*_", zv[2,1], zv[2,2], zv[2,3]))
                        # 2DEF where F > 0
                        if (zv[2,4] > 0) {
                            # append "[0-F]_"
                            output_object <- append (output_object, paste0 ("[0-", zv[2,4], "]_"))
                        # 2DEF where F == 0
                        } else {
                            # append "0_"
                            output_object <- append (output_object, paste0 ("0_"))
                        }
                    # 2DEF where E == 0
                    } else if (zv[2,3] == 0) {
                        # append "0" ### "*_1AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_2[0-(D-1)][0-9][0-9]_|*_2D0"
                        output_object <- append (output_object, paste0 ("0"))
                        # 2DEF where F > 0
                        if (zv[2,4] > 0) {
                            # append "[0-F]_"
                            output_object <- append (output_object, paste0 ("[0-", zv[2,4], "]_"))
                        # 2DEF where F == 0
                        } else {
                            # append "0_"
                            output_object <- append (output_object, paste0 ("0_"))
                        }
                    }
                # 2DEF where D == 0
                } else if (zv[2,2] == 0) {
                    # append "0" ### "*_1AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_20"
                    # 2DEF where E > 0
                    if (zv[2,3] > 0) {
                        # append "[0-(E-1)][0-9]_|*_20E" ### "*_1AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_20[0-(E-1)][0-9]_|*_20E"
                        output_object <- append (output_object, paste0 ("[0-", zv[2,3] - 1, "][0-9]_|*_", zv[2,1], zv[2,2], zv[2,3]))
                        # 2DEF where F > 0
                        if (zv[2,4] > 0) {
                            # append "[0-F]_" ### "*_1AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_20[0-(E-1)][0-9]_|*_20E"
                            output_object <- append (output_object, paste0 ("[0-", zv[2,4], "]_"))
                        # 2DEF where F == 0
                        } else {
                            # append "0_" ### "*_1AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_20[0-(E-1)][0-9]_|*_20E"
                            output_object <- append (output_object, paste0 ("0_"))
                        }
                    # 2DEF where E == 0
                    } else if (zv[2,3] == 0) {
                        # append "0" ### "*_1AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_200"
                        output_object <- append (output_object, paste0 ("0"))
                        # 2DEF where F > 0
                        if (zv[2,4] > 0) {
                            # append "[0-F]_" ### "*_1AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_200[0-F]_"
                            output_object <- append (output_object, paste0 ("[0-", zv[2,4], "]_"))
                        # 2DEF where F == 0
                        } else {
                            # append "0_" ### "*_1AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_2000_"
                            output_object <- append (output_object, paste0 ("0_"))
                        }
                    }
                }
            # 1XXX-1YYY
            } else if (zv[1,1] == zv[2,1]) {
                # 12BC-15EF
                if (zv[1,2] + 1 < zv[2,2]) {
                    # 12BC where B < 9
                    # 12BC where C < 9
                    # append "2B[C-9]_|*_12[B-9][0-9]_|*_1[(2+1)-(5-1)][0-9][0-9]_|*_15" ### "*_1"
                    output_object <- append (output_object, paste0 (zv[1,2], zv[1,3], "[", zv[1,4], "-9]_|*_", zv[1,1], zv[1,2], "[", zv[1,3], "-9][0-9]_|*_", zv[1,1], "[", zv[1,2] + 1, "-", zv[2,2] - 1, "][0-9][0-9]_|*_", zv[2,1], zv[2,2]))
                    # 15EF where E > 0
                    if (zv[2,3] > 0) {
                    # append "[0-(E-1)][0-9]_|*_15E"
                    output_object <- append (output_object, paste0 ("[0-", zv[2,3] - 1, "][0-9]_|*_", zv[2,1], zv[2,2], zv[2,3]))
                    # 15EF where F > 0
                    if (zv[2,4] > 0) {
                        # append "[0-F]_" ### "*_1AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_20[0-(E-1)][0-9]_|*_20E"
                        output_object <- append (output_object, paste0 ("[0-", zv[2,4], "]_"))
                    # 2DEF where F == 0
                    } else {
                        # append "0_" ### "*_1AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_20[0-(E-1)][0-9]_|*_20E"
                        output_object <- append (output_object, paste0 ("0_"))
                    }
                # 15EF where E == 0
                } else if (zv[2,3] == 0) {
                    # append "0" ###
                    output_object <- append (output_object, paste0 ("0"))
                    # 15EF where F > 0
                    if (zv[2,4] > 0) {
                        # append "[0-F]_" ### "*_1AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_20[0-(E-1)][0-9]_|*_20E[0-F]_"
                        output_object <- append (output_object, paste0 ("[0-", zv[2,4], "]_"))
                    # 2DEF where F == 0
                    } else {
                        # append "0_" ### "*_1AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_20[0-(E-1)][0-9]_|*_20E0_"
                        output_object <- append (output_object, paste0 ("0_"))
                    }
                }
                # 12BC-13EF
                } else if (zv[1,2] + 1 == zv[2,2]) {
                    # append "2B[C-9]_|*_12[B-9][0-9]_|*_13"
                    output_object <- append (output_object, paste0 (zv[1,2], zv[1,3], "[", zv[1,4], "-9]_|*_", zv[1,1], zv[1,2], "[", zv[1,3], "-9][0-9]_|*_", zv[2,1], zv[2,2]))
                    # 13EF where E > 0
                    if (zv[2,3] > 0) {
                        # append "[0-(E-1)][0-9]_|*_15E"
                        output_object <- append (output_object, paste0 ("[0-", zv[2,3] - 1, "][0-9]_|*_", zv[2,1], zv[2,2], zv[2,3]))
                        # 15EF where F > 0
                        if (zv[2,4] > 0) {
                            # append "[0-F]_" ### "*_1AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_20[0-(E-1)][0-9]_|*_20E"
                            output_object <- append (output_object, paste0 ("[0-", zv[2,4], "]_"))
                        # 2DEF where F == 0
                        } else {
                            # append "0_" ### "*_1AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_20[0-(E-1)][0-9]_|*_20E"
                            output_object <- append (output_object, paste0 ("0_"))
                        }
                    # 15EF where E == 0
                    } else if (zv[2,3] == 0) {
                        # append "0" ###
                        output_object <- append (output_object, paste0 ("0"))
                        # 15EF where F > 0
                        if (zv[2,4] > 0) {
                            # append "[0-F]_" ### ""
                            output_object <- append (output_object, paste0 ("[0-", zv[2,4], "]_"))
                        # 2DEF where F == 0
                        } else {
                            # append "0_" ### ""
                            output_object <- append (output_object, paste0 ("0_"))
                        }
                    }
                # 12BC-12EF
                } else if (zv[1,2] == zv[2,2]) {
                    # 123C-126F
                    if (zv[1,3] + 1 < zv[2,3]) {
                        # append "2B[C-9]_|*_12[(3+1)-(6-1)][0-9]_|*_126" ### "*_12B[C-9]_|*_12[(3+1)-(6-1)][0-9]_|*_126"
                        output_object <- append (output_object, paste0 (zv[1,2], zv[1,3], "[", zv[1,4], "-9]_|*_", zv[1,1], zv[1,2], "[", zv[1,3] + 1, "-", zv[2,3] - 1, "][0-9]_|*_", zv[2,1], zv[2,2], zv[2,3]))
                        # 15EF where F > 0
                        if (zv[2,4] > 0) {
                            # append "[0-F]_" ### ""
                            output_object <- append (output_object, paste0 ("[0-", zv[2,4], "]_"))
                        # 2DEF where F == 0
                        } else {
                            # append "0_" ### ""
                            output_object <- append (output_object, paste0 ("0_"))
                        }
                    # 123C-124F
                    } else if (zv[1,3] + 1 == zv[2,3]) {
                        # append "23[C-9]_|*_124" ### "*_123[C-9]_|*_124"
                        output_object <- append (output_object, paste0 (zv[1,2], zv[1,3], "[", zv[1,4], "-9]_|*_", zv[2,1], zv[2,2], zv[2,3]))
                        # 124F where F > 0
                        if (zv[2,4] > 0) {
                            # append "[0-F]_" ### ""
                            output_object <- append (output_object, paste0 ("[0-", zv[2,4], "]_"))
                        # 124F where F == 0
                        } else {
                            # append "0_" ### ""
                            output_object <- append (output_object, paste0 ("0_"))
                        }
                    # 123C-123F
                    } else if (zv[1,3] == zv[2,3]) {
                        # append "23[C-F]_" ### "*_1"
                        output_object <- append (output_object, paste0 (zv[1,2], zv[1,3], "[", zv[1,4], "-", zv[2,4], "]_"))
                    }
                }
            }
        } # else if (bigger than 4 digits?) {craaaap.}
    return (paste(output_object, collapse = ""))
}

print("find_the_dir, plot_that_spectrum, print_regex_num_range and heatmap_difference loaded")
