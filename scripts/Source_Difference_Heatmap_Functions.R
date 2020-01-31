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
        whatever <- which (stuff[[j]] == patternz)
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

plot_that_spectrum <- function (file_name, colorPalette, midpoint_size, legend_scale, theme) {

  # colorPalette <- 19
  # midpoint_size <- 1

  if (colorPalette == 19) {
      midpoint_director <- array(c(2, 16, 30, 44, 58, 72, 86, 7, 6, 5, 4, 3, 2, 1), c(7,2))
      stuff <- midpoint_director[midpoint_size,]
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
                                                  stuff[2], ")))"))),
    midpoint_but_smooth = colorRampPalette (c ("#67001f", "#b2182b", "#ca0020", "#d6604d", "#ef8a62", "#f4a582", "#fddbc7", "#f7f7f7", "#d1e5f0", "#92c5de", "#67a9cf", "#4393c3", "#0571b0", "#2166ac", "#053061")),
    midpoint_but_smooshed = colorRampPalette (c ("#67001f", "#b2182b", "#ca0020", "#d6604d", "#ef8a62", "#f4a582", "#fddbc7", "#f7f7f7", "#d1e5f0", "#92c5de", "#67a9cf", "#4393c3", "#0571b0", "#2166ac", "#053061",
    "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7",
    "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7")),
    midpoint_detail = colorRampPalette (c ("#67001f", "#67001f", "#67001f", "#8c0b24", "#8c0b24", "#8c0b24", "#b1172a", "#b1172a", "#b1172a", "#bc0c25",
                                           "#bc0c25", "#bc0c25", "#c90020", "#c90020", "#c90020", "#cf2d35", "#cf2d35", "#cf2d35", "#d55d4b", "#d55d4b",
                                           "#d55d4b", "#e17356", "#e17356", "#e17356", "#ed8861", "#ed8861", "#ed8861", "#f19670", "#f19670", "#f19670",
                                           "#f2a380", "#f2a380", "#f2a380", "#f8bda0", "#f8bda0", "#f8bda0", "#fbd7c2", "#fbd7c2", "#fbd7c2", "#fae7db",
                                           "#fae7db", "#fae7db", "#f7f6f6", "#f7f6f6", "#f7f6f6", "#e6eff4", "#e6eff4", "#e6eff4", "#d4e6f0", "#d4e6f0",
                                           "#d4e6f0", "#b6d7e8", "#b6d7e8", "#b6d7e8", "#97c7df", "#97c7df", "#97c7df", "#80b9d7", "#80b9d7", "#80b9d7",
                                           "#6babd0", "#6babd0", "#6babd0", "#58a0ca", "#58a0ca", "#58a0ca", "#4695c4", "#4695c4", "#4695c4", "#2b85bb",
                                           "#2b85bb", "#2b85bb", "#0c75b2", "#0c75b2", "#0c75b2", "#0f6cad", "#0f6cad", "#0f6cad", "#1d67ac", "#1d67ac",
                                           "#1d67ac", "#165290", "#165290", "#165290", "#08376b", "#08376b", "#08376b", "#5a7696", "#5a7696", "#5a7696",
                                           "#d2d8e0", "#d2d8e0", "#d2d8e0", "#f7f6f6", "#f7f6f6", "#f7f6f6", "#f7f6f6", "#f7f6f6", "#f7f6f6", "#f7f6f6"))
  )



  x <- array(c(1:100, rep(9.5,100), rep(9.65,100), rep(9.8,100), rep(9.95,100), rep(10.1,100), rep(10.25,100), rep(10.4,100), rep(10.55,100), rep(10.7,100), rep(10.85,100)), c(100, 11))

  png (filename = file.path (
            file_name),
          width = 554, height = 554, units = "px", pointsize = 12, bg = "white")

  if (theme == "difference") {
    title_and_stuff <- c ("Legend
(heatmap 1 minus heatmap 2)")
  } else if (theme == "variance") {
    title_and_stuff <- c ("Legend
(variance scale)")
  }

  plot(rep(x[,1],10), x[,2:11], col = colorseqmultpalette[[colorPalette]](100), pch = 15, cex = 1, axes = F, xlab = "", ylab = "", ylim = c(5,16), main = title_and_stuff)

  if (legend_scale == "midpoint") {
    axis(1, c(0.5, 7.5, 14.5, 21.5, 28.5, 35.5, 42.5, 49.5, 51.5, 58.5, 65.5, 72.5, 79.5, 86.5, 93.5, 100.5), c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""),T,-11.85,NA,F,cex.axis = 0.7, tck = -0.015)
    title(xlab = c("-1     -0.86    -0.72   -0.58   -0.44     -0.3     -0.16  -0.02  0.02   0.16     0.3     0.44      0.58      0.72    0.86         1 "), line = -11.2, cex.lab = 0.7)

    axis(1, c(0.5, 7.5, 14.5, 21.5, 28.5, 35.5, 42.5, 49.5, 51.5, 58.5, 65.5, 72.5, 79.5, 86.5, 93.5, 100.5), c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""),T,-15.8,NA,F,cex.axis = 0.7, tck = 0.015)
    title(xlab = c("-155     -133     -111      -89       -67      -45      -23        -3  3         23       45        67        89       111       133      155 "), line = -17.3, cex.lab = 0.7)

    title(xlab = c("Syllable Repertoire Differences"), line = -18.3, cex.lab = 0.7)
    title(xlab = c("Curiosity Level Differences"), line = -10.3, cex.lab = 0.7)

  } else if (legend_scale == "variance") {
    #   c ("", "", "", "", "", "", "", "")
    # c("0", "0.05", "0.1", "0.15", "0.2", "0.25", "0.3", "0.35")
    # axis (1, c(0.5, 7.5, 14.5, 21.5, 28.5, 35.5, 42.5, 49.5, 51.5, 58.5, 65.5, 72.5, 79.5, 86.5, 93.5, 100.5), c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""),T,-11.85,NA,F,cex.axis = 0.7, tck = 0.015)
    # title (xlab = c("0.02           0.04         0.06          0.08        0.1         0.125       0.145     0.165  0.185     0.205     0.225       0.245       0.265      0.285   0.315"), line = -17.8, cex.lab = 0.7)
    # axis (1, c (5.5, 15.5, 25.5, 35.5), c ("0.05", "0.15", "0.25", "0.35"),T,-11.85,NA,F,cex.axis = 0.7, tck = -0.015)
    axis (1, c(0.5, 3.5, 6.5, 9.5, 12.5, 15.5, 18.5, 21.5, 24.5, 27.5, 30.5, 33.5, 36.5, 39.5, 42.5, 45.5, 48.5, 51.5, 54.5, 57.5, 60.5, 63.5, 66.5, 69.5, 72.5, 75.5, 78.5, 81.5, 84.5, 87.5, 90.5, 93.5, 96.5), c("0", "0.01", "0.02", "0.03", "0.04", "0.05", "0.06", "0.07", "0.08", "0.09", "0.1", "0.11", "0.12", "0.13", "0.14", "0.15", "0.16", "0.17", "0.18", "0.19", "0.2", "0.21", "0.22", "0.23", "0.24", "0.25", "0.26", "0.27", "0.28", "0.29", "0.3", "0.31", "... 1"),T,-11.85,NA,F,cex.axis = 0.7, tck = -0.015)
  }

  dev.off()
}

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
  } else if (visualization == "midpoint_but_high_res") {
    output_curiosity <- (first_heatmap[,,,1:4] - second_heatmap[,,,1:4]) + 0.5
    output_sylreps <- (first_heatmap[,,,5:8] - second_heatmap[,,,5:8]) + (155/2)
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

  if (! (dir.exists (file.path ("results", "DifferenceHeatmaps", paste0("dh_", first_source_directory, "_versus_", secnd_source_directory))))) {
      dir.create(file.path ("results", "DifferenceHeatmaps", paste0("dh_", first_source_directory, "_versus_", secnd_source_directory)))
    }

    if (! (dir.exists (file.path ("results", "DifferenceHeatmaps", paste0("dh_", first_source_directory, "_versus_", secnd_source_directory), paste0(source_pattern, "_inh"))))) {
      dir.create(file.path("results", "DifferenceHeatmaps", paste0("dh_", first_source_directory, "_versus_", secnd_source_directory), paste0(source_pattern, "_inh")))
    }

  if (replace) {
    saveRDS(output_heatmap, file.path ("results", "DifferenceHeatmaps", paste0("dh_", first_source_directory, "_versus_", secnd_source_directory), paste0(source_pattern, "_inh"), paste0("Difference_Heatmap_", source_pattern, "_inheritance.RData")))
  } else {
    if (! (file.exists (file.path ("results", "DifferenceHeatmaps", paste0("dh_", first_source_directory, "_versus_", secnd_source_directory), paste0(source_pattern, "_inh"), paste0("Difference_Heatmap_", source_pattern, "_inheritance.RData"))))) {
      saveRDS(output_heatmap, file.path ("results", "DifferenceHeatmaps", paste0("dh_", first_source_directory, "_versus_", secnd_source_directory), paste0(source_pattern, "_inh"), paste0("Difference_Heatmap_", source_pattern, "_inheritance.RData")))
    }
  }

  # plot_that_spectrum(file_name = file.path ("results", paste0("Difference_Heatmaps_", first_source_directory, "_versus_", secnd_source_directory)))

  output_heatmap <- list(
    # biassize = foldername$biassize,
    # othersize = foldername$othersize,
    # diffcurstartbias = foldername$diffcurstartbias,
    # output_heatmap = output_heatmap,
    foldername = file.path (paste0("dh_", first_source_directory, "_versus_", secnd_source_directory), paste0(source_pattern, "_inh"))
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
