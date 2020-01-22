source(file.path("scripts", "Source_Reference_Section.R"))
referencesection("heatmaps")
source(file.path("scripts", "Source_Heatmap_Functions.R"))
heatmapland <- file.path("results")

all_the_runs <- extractvardirs(heatmapland,
  "*_360[8-9]_|*_36[1-4][0-9]_|*_365[0-7]_") ### oct3n8results ### Father Inheritance 5x5x2 lowHigh Background

extractedmeans <- extractmeans(
  allrundirs = all_the_runs,
  dirheatmap = heatmapland,
  # ordering = c(1, 3, 4, 2),
  source_of_params = "params.yaml")
all_the_names <- remakestring(all_the_runs, "_", ".")

names(extractedmeans) <- all_the_names

heatmapoutput <- list()

heatmapoutput <- makeheatmapfile(
                inheritance = 1, diffcurstartbias = "pop1",
                biassize = 5, othersize = 2,
                reversedruns = FALSE,
                runstyle = "lowHigh", highres = FALSE,
                extractedmeans = extractedmeans)

individualfigures(2,5,heatmapoutput)

all_the_runs <- extractvardirs(heatmapland,
  "*_379[4-9]_|*_38[0-3][0-9]_|*_384[0-3]_") ### octXresults ### Mother Inheritance 5x5x2 lowHigh Background

extractedmeans <- extractmeans(
  allrundirs = all_the_runs,
  dirheatmap = heatmapland,
  # ordering = c(1, 3, 4, 2),
  source_of_params = "params.yaml")
all_the_names <- remakestring(all_the_runs, "_", ".")

names(extractedmeans) <- all_the_names

heatmapoutput <- list()

heatmapoutput <- makeheatmapfile(
                inheritance = 2, diffcurstartbias = "pop1",
                biassize = 5, othersize = 2,
                reversedruns = FALSE,
                runstyle = "lowHigh", highres = FALSE,
                extractedmeans = extractedmeans)

individualfigures(2,5,heatmapoutput)

all_the_runs <- extractvardirs(heatmapland,
  "*_365[8-9]_|*_36[6-9][0-9]_|*_370[0-7]_") ### oct3n8results ### SameSex Inheritance 5x5x2 lowHigh Background

extractedmeans <- extractmeans(
  allrundirs = all_the_runs,
  dirheatmap = heatmapland,
  # ordering = c(1, 3, 4, 2),
  source_of_params = "params.yaml")
all_the_names <- remakestring(all_the_runs, "_", ".")

names(extractedmeans) <- all_the_names

heatmapoutput <- list()



# extractedmeans <- extractmeans(
#   allrundirs = all_the_runs,
#   dirheatmap = heatmapland,
#   # ordering = c(1, 3, 4, 2),
#   source_of_params = "params.yaml")
# all_the_names <- remakestring(all_the_runs, "_", ".")

# names(extractedmeans) <- all_the_names

# heatmapoutput <- list()

# heatmapoutput <- makeheatmapfile(
#                 inheritance = 3, diffcurstartbias = "pop1",
#                 biassize = 2, othersize = 1,
#                 reversedruns = FALSE,
#                 runstyle = "binaryLB", highres = FALSE,
#                 extractedmeans = extractedmeans)

# individualfigures(2,5,heatmapoutput)

# all_the_runs <- extractvardirs(heatmapland,

source(file.path("scripts", "Source_Reference_Section.R"))
referencesection("heatmaps")

source(file.path("scripts", "Source_Heatmap_Functions.R"))
source(file.path("scripts", "Source_Batch_Heatmap_Functions.R"))

thing <- c("male", "moth", "same", "FfFf")
# das_dinge <- c(1,2,3,11)
# stuff <- c("five-by-five-Inv3k_lowHigh_Background",
#            "five-by-five-vanilla_lowHigh_Background",
#            "five-by-five-followUpVanilla_lowHigh_Background",
#            "five-by-five-followUpInvLow1k_lowHigh_Background",
#            "five-by-five-followUpInvHigh1k_lowHigh_Background",
#            "five-by-five-followUpFemInvLow1k_lowHigh_Background",
#            "five-by-five-followUpBothInvLow1k_lowHigh_Background",
#            "",
#           #  "tenKfiveByFive_child-lowFemInvtK/five-by-five-followUpFemHigh1k_lowHigh_Background",
#          #  "",)
# )

stuff <- c("parentNoInv", "childF1NoInv", "childMalHihInv", "childMalLowInv",
           "childFemLowInv", "childBothLowInv", "childFemHihInv", "childBothHihInv",
           "childSmolMalHihInv", "childSmolMalLowInv", "childSmolFemHihInv", "childSmolFemLowInv",
           "childF2NoInv", "childF3NoInv", "childF4NoInv", "childF5NoInv",
           "childF6NoInv", "childF7NoInv", "childF8NoInv", "childF9NoInv",
           "childF10NoInv")

# stuff_n_things <- array (c (1, 1, 1, 2, 3, 3, 4, 2, 3, 5, 3, 4, 5, 5), c (7,2))
stuff_n_things <- array (c (1, 1, 1, 1, 1, 1, 2,
                            2, 2, 2, 2, 3, 3, 3,
                            3, 4, 4, 4, 5, 5, 6,

                            2, 3, 4, 5, 6, 7, 3,
                            4, 5, 6, 7, 4, 5, 6,
                            7, 5, 6, 7, 6, 7, 7), c (21,2))

for (bs in 1:dim(stuff_n_things)[1]) {
  for(whaaat in 1:4) {

    output_heatmap <- heatmap_difference (
                        source_pattern = thing[whaaat],
                        first_source_names = paste0 ("five-by-five-", stuff[stuff_n_things[bs, 1]]),
                        secnd_source_names = paste0 ("five-by-five-", stuff[stuff_n_things[bs, 2]]),
                        visualization = "midpoint",
                        replace = TRUE
                        )

    #### Blue values = High Number,
    #### Red values = Low Number

    #### So, for example, Inv(high) - Vanilla = Blue,
    #### while, in contrast, Van - Inv(high) = Red

    # five-by-five-followUpInvLow1k_lowHigh_Background
    individualfigures(
      output_foldername = "DifferenceHeatmaps",
      colorrange = 2,
      colorpalette = 19,
      foldername = list(
        foldername = output_heatmap$foldername,
        biassize = 5,
        othersize = 2,
        diffcurstartbias = "pop1"
      )
    )

    # _source_names = "five-by-five-followUpVanilla_lowHigh_Background", ### 4101-4300
    # _source_names = "five-by-five-followUpInv1k_lowHigh_Background", ### 4101-4300

    # source_pattern = "male" # "moth", "same", "FfFf"
    # first_source_names = c("five-by-five-followUpInv1k_lowHigh_Background")
    # secnd_source_names = c("five-by-five-followUpVanilla_lowHigh_Background")
    # visualization = "absolute"
    # replace = TRUE



  }
}


# image(x = array(c(39:63)/100, c(5,5)),
thing <- as.matrix((first_heatmap[,,1,1] - second_heatmap[,,1,1])/2 + 0.5)
thing2 <- as.matrix((second_heatmap[,,1,1] - first_heatmap[,,1,1])/2 + 0.5)
thing3 <- output_heatmap[,,1,1]
# image(x = thing,
image(x = thing3,
# image(x = array(c(39:63)/100, c(5,5)),
          col = colorseqmultpalette[[19]](100),
          axes = F, zlim = c(0, 1))








# 777

# x <- array(c(1:100, rep(1,100)), c(100, 2))
# x <- array(c(1:100, rep(9.5,100), rep(9.7,100), rep(9.9,100), rep(10.1,100), rep(10.3,100), rep(10.5,100), rep(10.7,100), rep(10.9,100), rep(11.1,100), rep(11.3,100)), c(100, 11))

# plot(x[,1], x[,2], col = colorseqmultpalette[[19]](100), pch = 15, cex = 5, axes = F, xlab = "", ylab = "")
# plot(x[,1], x[,2], col = colorseqmultpalette[[19]](100), pch = 15, cex = 5, axes = F, xlab = "", ylab = "")
# axis(1, c(0, 0.1, 1, 10, 100), c("0", "0.1", "1", "10", "100"),T,0,NA,F,cex.axis = 0.8, tck = 0)

colorpalette = 19
midpoint_size <- 1

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

plot_that_spectrum <- function (file_name) {

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
  plot(rep(x[,1],10), x[,2:11], col = colorseqmultpalette[[19]](100), pch = 15, cex = 1, axes = F, xlab = "", ylab = "", ylim = c(5,16), main = "Legend
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

# c(-2.5, 4.5, 11.5, 18.5, 25.5, 32.5, 39.5, 46.5, 48.5, 55.5, 62.5, 69.5, 76.5, 83.5, 90.5, 97.5, 103.5)
# length(c(1, 8, 15, 22, 29, 36, 43, 47.5, 52, 59, 66, 73, 80, 87, 94, 100.5))


