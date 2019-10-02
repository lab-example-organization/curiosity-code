source(file.path("scripts", "Source_Reference_Section.R"))
referencesection("heatmaps")

# You Should be Here To: Run some Heatmaps to compar a wide range of inherited traits!
#
#   ___ ______________   _____________________      _____ __________  _________
#  /   |   \_   _____/  /  _  \__    ___/     \    /  _  \\______   \/   _____/
# /    ~    \    __)_  /  /_\  \|    | /  \ /  \  /  /_\  \|     ___/\_____  \ 
# \    Y    /        \/    |    \    |/    Y    \/    |    \    |    /        \
#  \___|_  /_______  /\____|__  /____|\____|__  /\____|__  /____|   /_______  /
#        \/        \/         \/              \/         \/                 \/ 




# This is an Example of what you should NEVER have in your code, presented here,

# "So that I can use it when I'm being a bad person :P"

######  setwd(file.path(strsplit(getwd(), "curiosity-code")[[1]][1], "curiosity-code"))


# Source the Functions

source(file.path("scripts", "Source_Heatmap_Functions.R"))

############## # # ARRANGEMENT OF FUNCTIONS  # # ##############

# heatmapland <- HtMpDir(extraDir = "sameInh")
# heatmapland <- file.path("results", "Heatmaps", "sameInh")
# heatmapland <- file.path("results", "Heatmaps", "maleInh_maleBias")
# heatmapland <- file.path("results", "Heatmaps", "femInh_maleBias")
# heatmapland <- file.path("results", "Heatmaps", "femInh_femBias")
# heatmapland <- file.path("results")
# heatmapland <- file.path("..", "..", "old_stuff", "curiosity-code", 
#                          "results", "mixCI_10m-90f")
# heatmapland <- file.path("..", "..", "old_stuff", "curiosity-code", 
#                          "results", "mixCI_25m-75f")
# heatmapland <- file.path("..", "..", "old_stuff", "curiosity-code", 
#                          "results", "mixCI_40m-60f")
# heatmapland <- file.path("..", "..", "old_stuff", "curiosity-code", 
#                          "results", "mixCI_60m-40f")
# heatmapland <- file.path("..", "..", "old_stuff", "curiosity-code", 
#                          "results", "mixCI_75m-25f")
# heatmapland <- file.path("..", "..", "old_stuff", "curiosity-code", 
#                          "results", "mixCI_90m-10f")
# heatmapland <- file.path("..", "..", "190601_old_stuff", "HRSmSxLB")
# heatmapland <- file.path("..", "..", "190601_old_stuff", "HRFfFfLB")

heatmapland <- file.path("results", "190704_SmSxHB")
heatmapland <- file.path("results", "190705_FfFfHB")

#190601_old_stuff/HRSmSxLB/
# heatmapland <- file.path("results", "Heatmaps")

# all_the_runs <- list.files(heatmapland, 
all_the_runs <- extractvardirs(heatmapland, 
  #"_1[7-9][0-9]|2[0-9][0-9]|3[0-9][0-9]|4[0-1][0-9]_") # <- This was for the very first run - non-automated... more code to follow.
  #"190304_1[7-9][0-9]_|190304_2[0-8][0-9]_|190304_29[0-5]_")
  # "*_1[7-9][0-9]_|*_2[0-8][0-9]_|*_29[0-5]_")                # maleinh malebias
  # "*_2[9][6-9]_|*_3[0-9][0-9]_|*_4[0-1][0-9]_|*_420_")       # mothinh malebias
  # "*_42[1-9]_|*_4[3-9][0-9]_|*_5[0-3][0-9]_|*_54[0-5]_")      # mothinh femBias
  # "*_54[6-9]_|*_5[5-9][0-9]_|*_6[0-6][0-9]_|*_670_")     # sameinh femaleBias
  # "*_67[1-9]_|*_6[8-9][0-9]_|*_7[0-8][0-9]_|*_79[0-4]_")  # sameinh_maleBias
  # "*_79[4-9]_|*_8[0-9][0-9]_|*_90[0-9]_|*_91[0-7]_|*_1041_")   # oppinh malebias
  # "*_794_|*_91[8-9]_|*_9[2-9][0-9]_|*_10[0-3][0-9]_|*_104[0-1]_")   # oppinh femBias
  ##### "*_104[2-9]_|*_10[5-9][0-9]_|*_11[0-5][0-9]_|*_116[0-5]_|*_1289_") # maleinh femBias
  ##### "*_116[6-9]_|*_11[7-9][0-9]_|*_12[0-8][0-9]_") # 
  # "*_129[0-9]_|*_13[0-9][0-9]_|*_140[0-9]_|*_141[0-4]_") # mixedCurInh - sNTn (males 90%, females 10%)
  # "*_141[5-9]_|*_14[2-9][0-9]_|*_15[0-9][0-9]_|*_16[0-5][0-9]_|*_166[0-2]_") # mixedCurInh_-_sSTf (males 75%, females 25%) 
  # "*_166[3-9]_|*_16[7-9][0-9]_|*_1[7-8][0-9][0-9]_|*_190[0-9]_|*_1910_") # mixedCurInh_-_sSFr (males 60%, females 40%) ### running on LeonServer
  # "*_191[1-9]_|*_19[2-9][0-9]_|*_20[1-2][0-9]_|*_203[0-5]_") # mothInh_femaleBias_SD=5 ### running on pComp
  # "*_203[6-9]_|*_20[4-9][0-9]_|*_21[0-9][0-9]_|*_22[0-7][0-9]_|*_228[0-3]_") # mixedCurInh_-_sTnN (sub curinh males - 10%, curinh females - 90%)
  
  # "*_253[2-9]_|*_25[4-9][0-9]_|*_26[0-9][0-9]_|*_27[0-6][0-9]_|*_277[0-9]_")      # mixCI_10m-90f
  # "*_228[4-9]_|*_229[0-9]_|*_23[0-9][0-9]_|*_24[0-9][0-9]_|*_25[0-2][0-9]_|*_253[0-1]_")      # mixCI_25m-75f
  # "*_203[6-9]_|*_20[4-9][0-9]_|*_21[0-9][0-9]_|*_22[0-7][0-9]_|*_228[0-3]_")      # mixCI_40m-60f
  # "*_166[3-9]_|*_16[7-9][0-9]_|*_1[7-8][0-9][0-9]_|*_190[0-9]_|*_1910_")      # mixCI_60m-40f
  # "*_141[5-9]_|*_14[2-9][0-9]_|*_15[0-9][0-9]_|*_16[0-5][0-9]_|*_166[0-2]_")      # mixCI_75m-25f
  #"*_27[8-9][0-9]_|*_28[0-9][0-9]_|*_29[0-9][0-9]_|*_30[0-1][0-9]_|*_302[0-7]_")      # mixCI_90m-10f
  
  "10k") # Carefully curated directory, contains all necessary runs and nothing else

  # "*_302[8-9]_|*_303[0-9]_|*_304[0-5]_")      # sameinh popsplit lmh
  # "*_304[6-9]_|*_305[0-3]_")      # sameinh popsplit nw
  # "*_305[4-9]_|*_30[6][0-9]_|*_307[0-1]")      # mixinh popsplit lmh
  # "*_307[2-9]_")      # mixinh popsplit nw
#   connection <- file(description = file.path("source","temp", paste0(specificSimNumber, "_sim_data.txt")), open = "rt")
#   multiRun_folderList <- as.vector(read.table(connection, -1L)[[2]])
#   close(connection)


# profvis({
# #   for(iteration in 1:10) {
#     extractedmeans <- extractmeans(allrundirs = all_the_runs, 
#         dirheatmap = heatmapland, source_of_params = "params.yaml")
# #   }
# })




extractedmeans <- extractmeans(allrundirs = all_the_runs, 
                               dirheatmap = heatmapland, 
                               source_of_params = "params.yaml", 
                               deeper = FALSE)
all_the_names <- remakestring(all_the_runs, "_", ".")

names(extractedmeans) <- all_the_names

### SEPARATE SEXES (extractmeans)

# all_the_MaleRuns <- c(all_the_runs[1:124], all_the_runs[248])
# all_the_FemaleRuns <- c(all_the_runs[1], all_the_runs[125:248])

# extractedFemaleMeans <- extractmeans(allrundirs = all_the_FemaleRuns, dirheatmap = heatmapland, source_of_params = "params.yaml", deeper = FALSE)
# # all_the_names <- remakestring(all_the_FemaleRuns, "_", ".")
# # names(extractedFemaleMeans) <- all_the_names
# makeheatmapfile(inheritance = 5, diffcurstartbias = 2, absolute = TRUE, specialfigs = FALSE, lmhvnw = FALSE, extractedmeans = extractedFemaleMeans)
# ## inheritance went from 5 to 9

# extractedMaleMeans <- extractmeans(allrundirs = all_the_MaleRuns, dirheatmap = heatmapland, source_of_params = "params.yaml")
# # all_the_names <- remakestring(all_the_MaleRuns, "_", ".")
# # names(extractedMaleMeans) <- all_the_names
# makeheatmapfile(inheritance = 5, diffcurstartbias = 1, absolute = TRUE, specialfigs = FALSE, lmhvnw = FALSE, extractedmeans = extractedMaleMeans)
# ## inheritance went from 5 to 9

### END OF SEPARATE SEXES

# names(extractedMaleMeans) <- all_the_names


# heatmapland

# makeHeatmaps <- function (
#   inheritance = 1,
#   diffcurstartbias = 1
# )

# whichInh <- c("male","moth","same","opps","sNTn","sSTf","sSFr","sFrS","sTfS","sTnN", "FfFf")

# whichbias <- c("male","female", "pop1", "pop2", "both")

# source(file.path("scripts", "Source_Heatmap_Functions.R"))

heatmapoutput <- list()
heatmapoutput <- makeheatmapfile(inheritance = 11, diffcurstartbias = 3, 
                biassize = 10, othersize = 1, 
                reversedruns = TRUE, specialfigs = TRUE, 
                runstyle = 1, highres = TRUE, 
                extractedmeans = extractedmeans)

# # USING SOME OLD RDS FILES? USE THIS VERSION OF THE FUNCTION

# heatmapoutput <- makeheatmapfile(inheritance = 3, diffcurstartbias = 3, 
#                 biassize = 10, othersize = 1, 
#                 reversedruns = TRUE, reDo = TRUE, specialfigs = TRUE, 
#                 runstyle = 1, highres = TRUE, 
#                 extractedmeans = NULL)

# makeheatmapfile(inheritance = 10, diffcurstartbias = 1, absolute = TRUE, specialfigs = FALSE, lmhvnw = FALSE, extractedmeans = extractedmeans)
# makeheatmapfile(inheritance = 10, diffcurstartbias = 2, absolute = TRUE, specialfigs = FALSE, lmhvnw = FALSE, extractedmeans = extractedmeans)

# makeheatmapfile(inheritance = 11, diffcurstartbias = 3, absolute = TRUE, specialfigs = TRUE, lmhvnw = TRUE, extractedmeans = extractedmeans)
# makeheatmapfile(inheritance = 11, diffcurstartbias = 3, absolute = TRUE, specialfigs = TRUE, lmhvnw = FALSE, extractedmeans = extractedmeans)
# makeHeatmaps(inheritance = 1, diffcurstartbias = 1, absolute = TRUE, reDo = TRUE)
# makeHeatmaps(inheritance = 2, diffcurstartbias = 1, absolute = TRUE, reDo = TRUE)
# makeHeatmaps(inheritance = 1, diffcurstartbias = 2, absolute = TRUE, reDo = TRUE)
# makeHeatmaps(inheritance = 2, diffcurstartbias = 2, absolute = TRUE, reDo = TRUE)

# makeHeatmaps(inheritance = 3, diffcurstartbias = 1, absolute = TRUE, reDo = TRUE)
# makeHeatmaps(inheritance = 3, diffcurstartbias = 2, absolute = TRUE, reDo = TRUE)
# makeHeatmaps(inheritance = 4, diffcurstartbias = 1, absolute = TRUE, reDo = TRUE)
# makeHeatmaps(inheritance = 4, diffcurstartbias = 2, absolute = TRUE, reDo = TRUE)





  # whichInh <- c("male","moth","same","opps","sNTn","sSTf","sSFr","sFrS","sTfS","sTnN", "FfFf")


# individualfigures(5:10, 2, 1:2, 5)

# inheritance = 5
# colorrange = 2
# thisBias = 1
# numotherpopruns = 5

# thing <- c(5, 6, 7, 8, 9, 10)
# for (inhREtnce in 1:5) {
#   for (sexBIAS in 1:2) {
    
#     individualfigures(10, 2, sexBIAS, 3)

#   }
# }

##### ? WILL THIS WORK?
individualfigures(2,5,heatmapoutput)
#####

individualfigures <- function (

  # inheritance = 1, #c("maleinh", "mothinh", "sameinh", "oppsinh","sNTninh", "sSTfinh", "sSFrinh", "sFrSinh", "sTfSinh", "sTnNinh", "FfFfinh")
  colorrange = 2, # c("relative", "absolute")
  colorpalette = 5, # Numbers correspond to specific color palettes
  # thisBias = 1, # 3 or 4
  # numotherpopruns = 3, # USE THIS WHEN POP 2 HAS FEWER CONDITIONS THAN POPULATION OF INTEREST. this will force the dimensions of the data structure to include only the number of starting conditions that are present for the population of non-interest
  foldername = heatmapoutput
) {
  
  # reds, rdpu, oranges, orrd, ylorrd, ylorbr, ylgn, ylgnbu, greens, gnbu, blues, bugn, bupu, purples, purd, pubu, pubugn, greys
  #    1,    2,       3,    4,      5,      6,    7,      8,      9,   10,    11,   12,   13,      14,   15,   16,     17,    18

  # heatmap_sourcefolder <- file.path("results", "Heatmaps", "output_objects")
  heatmap_sourcefolder <- file.path("results")
  # heatmap_sourcefolder <- file.path("sameSexFigResults", "results")
  

  # Character vectors for args - indices  Not sure what else to call them, but they'll be used to reassign the args to non-numeric objects

  clrrngcontainer <- c("relative", "absolute")

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
  } else if (foldername$diffcurstartbias == "moth" || foldername$diffcurstartbias == 2) {
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
  htmparrays <- list.files(file.path(heatmap_sourcefolder, foldername$foldername), pattern = ".RData")


  temphtmparray <- readRDS(file.path(heatmap_sourcefolder, foldername$foldername, htmparrays))

  colorseqmultpalette <- list(
    reds = colorRampPalette(c("#fee0d2", "#fc9272", "#de2d26")), # 3-class reds       ### 1
    rdpu = colorRampPalette(c("#fde0dd", "#fa9fb5", "#c51b8a")), # 3-class rdpu       ### 2
    oranges = colorRampPalette(c("#fee6ce", "#fdae6b", "#e6550d")), # 3-class oranges ### 3
    orrd = colorRampPalette(c("#fee8c8", "#fdbb84", "#e34a33")), # 3-class orrd       ### 4
    ylorrd = colorRampPalette(c("#ffeda0", "#feb24c", "#f03b20")), # 3-class ylorrd   ### 5
    ylorbr = colorRampPalette(c("#fff7bc", "#fec44f", "#d95f0e")), # 3-class ylorbr   ### 6
    ylgn = colorRampPalette(c("#f7fcb9", "#addd8e", "#31a354")), # 3-class ylgn       ### 7
    ylgnbu = colorRampPalette(c("#edf8b1", "#7fcdbb", "#2c7fb8")), # 3-class ylgnbu   ### 8
    greens = colorRampPalette(c("#e5f5e0", "#a1d99b", "#31a354")), # 3-class greens   ### 9
    gnbu = colorRampPalette(c("#e0f3db", "#a8ddb5", "#43a2ca")), # 3-class gnbu       ### 10
    blues = colorRampPalette(c("#deebf7", "#9ecae1", "#3182bd")), # 3-class blues     ### 11
    bugn = colorRampPalette(c("#e5f5f9", "#99d8c9", "#2ca25f")), # 3-class bugn       ### 12
    bupu = colorRampPalette(c("#e0ecf4", "#9ebcda", "#8856a7")), # 3-class bupu       ### 13
    purples = colorRampPalette(c("#efedf5", "#bcbddc", "#756bb1")), # 3-class purples ### 14
    purd = colorRampPalette(c("#e7e1ef", "#c994c7", "#dd1c77")), # 3-class purd       ### 15
    pubu = colorRampPalette(c("#ece7f2", "#a6bddb", "#2b8cbe")), # 3-class pubu       ### 16
    pubugn = colorRampPalette(c("#ece2f0", "#a6bddb", "#1c9099")), # 3-class pubugn   ### 17
    greys = colorRampPalette(c("#f0f0f0", "#bdbdbd", "#636363")) # 3-class greys      ### 18
  )

  regularnames <- c(
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
  
  if(!(dir.exists(file.path(
    heatmap_sourcefolder, foldername$foldername, slicedpop[3] # paste0("slice_", slice)
  )))) {
    dir.create(file.path(
      heatmap_sourcefolder, foldername$foldername, slicedpop[3] # paste0("slice_", slice)
    ))
  }
  
  for (sxmtpop in 1:8) { 
    for (slice in 1:foldername$othersize) {
      
      file_name <- paste0(regularnames[sxmtpop], "_slice_", slice, "_", slicedpop[3], ".png")
      # rule of thumb: if we're splitting up htmpView _within_ slice and sxmtpop, then we need to save the output files according to the schema that will help pull back together the slices.
      png(filename = file.path(
          heatmap_sourcefolder, foldername$foldername, #inhoptions[inhstyle + 2], 
          # paste0("slice_", slice), file_name), 
          slicedpop[3], file_name), 
        width = 554, height = 554, units = "px", pointsize = 12, bg = "white")

      if(colorrange == "absolute") {
        if (sxmtpop <= 4) {
          heatmaprange <- c(0,1)
        } else {
          heatmaprange <- c(1,156)
        }
      } else {
        
        heatmaprange <- inhoptions[[inhstyle]][
          dat_array_doh[1,1,1,slice]:dat_array_doh[1,1,2,slice],
          dat_array_doh[1,2,1,slice]:dat_array_doh[1,2,2,slice],
          dat_array_doh[1,3,1,slice]:dat_array_doh[1,3,2,slice],
          sxmtpop]
        heatmap_min <- c(
          round(min(heatmaprangedatasetone), 2),
          round(min(heatmaprangedatasettwo), 2),
          round(min(heatmaprangedatasettre), 2)
        )
        heatmap_max <- c(
          round(max(heatmaprangedatasetone), 2),
          round(max(heatmaprangedatasettwo), 2),
          round(max(heatmaprangedatasettre), 2)
        )
        
        heatmaprange <- c(heatmap_min[3]-0.01,heatmap_max[3]+0.01)
        rm(heatmaprangedatasetone, heatmaprangedatasettwo, heatmaprangedatasettre,
          heatmap_min, heatmap_max)
      } # UNFINISHED - depreciated?
      # findXLab <- heatmap_axes[[3]][1]
      # findYLab <- heatmap_axes[[3]][2]
      
      # if(inhstyle == 1) {
        # dim_1 = 3
        # dim_2 = 3
        # dim_3 = 2
        numotherpopruns <- foldername$othersize
        dat_array_doh <- array(c(
          1,1,1, 1,1,1, 1,1,1, 1,3,3, 3,1,3, numotherpopruns,numotherpopruns,1,
          2,1,1, 1,2,1, 1,1,2, 2,3,3, 3,2,3, numotherpopruns,numotherpopruns,2,
          3,1,1, 1,3,1, 1,1,numotherpopruns, 3,3,3, 3,3,3, numotherpopruns,numotherpopruns,numotherpopruns
          # rep(c(1, 1, 1, 1), 2), 1, 1, rep(c(3, 3, 3, 1), 2),
          # rep(c(2, 1, 1, 1), 2), 2, 2, rep(c(3, 3, 3, 2), 2),
          # rep(c(3, 1, 1, 1), 2), 3, 3, rep(c(3, 3, 3, 3), 2)
        ), c(3,3,foldername$othersize,3))

        image(x = temphtmparray[,,,sxmtpop],
          col = colorseqmultpalette[[colorpalette]](100),
          axes = F, 
          xlab = heatmap_axes[[3]][1], 
          ylab = heatmap_axes[[3]][2],cex.lab=1.4, zlim = heatmaprange
        )

        if (!(is.null(dimnames(temphtmparray)))) {  
          temphtmpdimensions <- dimnames(temphtmparray)
          temptemp <- vector(mode = "character", length = length(temphtmpdimensions))
          for (thething in 1:length(temphtmpdimensions[[1]])) {
            temptemp[thething] <- str_extract_all(temphtmpdimensions[[1]][thething], "[:digit:]*-[:digit:]*")
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
                  c("", "1-7", "", "4-10", "", "7-13", "", "10-15", "", "13-19", "", "15-23", "", "19-26", "", "23-29", "", "26-31", "", "29-34", ""),
              T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(1,c(-0.0555, 0.0555, 0.1665, 0.2775, 0.3885, 0.5, 0.611, 0.722, 0.833, 0.944, 1.055),
              c("", "", "", "", "", "", "", "", "", "", ""),
              T,-0.03,NA,F,cex.axis=1, tck = -0.03)

            axis(2,c(-0.0555,   0, 0.0555, 0.111, 0.1665, 0.222, 0.2775, 0.333, 0.3885, 0.444, 0.4995, 0.555, 0.611, 0.6665, 0.722, 0.7775, 0.833, 0.8885, 0.944, 0.9995, 1.055),
                  c("", "1-7", "", "4-10", "", "7-13", "", "10-15", "", "13-19", "", "15-23", "", "19-26", "", "23-29", "", "26-31", "", "29-34", ""),
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
}
# thing <- c(5, 6, 7, 8, 9, 10)
# for (
#   inhREtnce in 1:6
# ) {
#   for (
#     sexBIAS in 1:8
#   ) {
#     combineeditsingles(inhREtnce[thing], 1, sexBIAS, 3, T, F)
#   }
# }

combineeditsingles(5, 1, 1, 3, T, F)

inheritancestyle = 5
bias = 1
metricssexpop = 1
otherpopstyle = 3
edit = TRUE
lmhvnw = FALSE


combineeditsingles <- function (
  inheritancestyle = 1,
  bias = 1,
  metricssexpop = 1, # only allowed 1-4 (p1m, p2m, p1f, p2f)
  otherpopstyle = 3, # 1 = low, 2 = high (for lmh & nw stuff round May 2019) # 3 = third group has same number of tested conditions (slices) as the first two groups.
  edit = FALSE,
  lmhvnw = FALSE
) {
  
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
  
  
  
  curstartpatterncontainer <- c("lowMedHigh", "narrowWide")
  
  inheritancecontainer <- c("maleinh", "mothinh", "sameinh", "oppsinh", "sNTninh", 
                            "sSTfinh", "sSFrinh", "sFrSinh", "sTfSinh", "sTnNinh", 
                            "FfFfinh")

  titlesxmtpop <- c("Pop 1 Mal", "Pop 2 Mal", 
                    "Pop 1 Fem", "Pop 2 Fem", 
                    "Pop 1 Mal", "Pop 2 Mal", 
                    "Pop 1 Fem", "Pop 2 Fem")

  titleinhstyle <- c("Male", "Female", "Same-Sex", "Opposite", "90M10F", 
                     "75M25F", "60M40F", "40M60F", "25M75F", "10M90F", 
                     "50-50")
  
  
  if (otherpopstyle == 3) { # LMH and NW are still split up, but pop 2 also has lmh and nw, so there's a new arrangement needed?
    if (lmhvnw == TRUE) {
      for (
        comparisonpattern in 1:2
      ) {
        titlebackgroundpop <- c("Low", "Med", "High")
        whichbias <- c("malebias", "femaleBias", "pop1Bias")
        whichpopbias <- c("FemalePop", "MalePop", "Popula2")
        
        subpopulation <- c("p1mAC", "p2mAC", "p1fAC", "p2fAC", "p1mSR", "p2mSR", "p1fSR", "p2fSR")
        
        folderbias <- list.files(heatmap_sourcefolder)[which(sapply(list.files(heatmap_sourcefolder), function(x) (inheritancecontainer[inheritancestyle] %in% str_split(x, "_")[[1]][4])))]
        
        
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
        
        image_write(final_set, path = file.path(heatmap_sourcefolder, folderbias[1], paste0("BothSexes_", subpopulation[metricssexpop], "_measure_", inheritancecontainer[inheritancestyle], "_", stylesforotherpop[otherpopstyle], "_background.png")))
      }
    } else {

      titlebackgroundpop <- c("Low", "Med", "High")
      whichbias <- c("malebias", "femaleBias", "pop1Bias")
      whichpopbias <- c("FemalePop", "MalePop", "Popula2")
      
      subpopulation <- c("p1mAC", "p2mAC", "p1fAC", "p2fAC", "p1mSR", "p2mSR", "p1fSR", "p2fSR")
      
      folderbias <- list.files(heatmap_sourcefolder)[which(sapply(list.files(heatmap_sourcefolder), function(x) (inheritancecontainer[inheritancestyle] %in% str_split(x, "_")[[1]][4])))]
      
      popbias <- whichpopbias[bias]
      
      #   for (i in 1:2) {
          
      namesforotherpop <- c("slice_1", "slice_2", "slice_3")
      stylesforotherpop <- c("low", "high", "lmh")
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

      image_write(final_set, path = file.path(heatmap_sourcefolder, paste0("SexBiasedCurInhRange"), paste0(inheritancecontainer[inheritancestyle], "_", subpopulation[metricssexpop], "_measure_", stylesforotherpop[otherpopstyle], "_background.png")))
    }
    
    
  } else {

    titlebackgroundpop <- c("Low", "High")

    whichbias <- c("malebias", "femaleBias", "pop1Bias")
    whichpopbias <- c("FemalePop", "MalePop", "Popula2")
    
    subpopulation <- c("p1m", "p2m", "p1f", "p2f")
    
    folderbias <- list.files(heatmap_sourcefolder)[which(sapply(list.files(heatmap_sourcefolder), function(x) (inheritancecontainer[inheritancestyle] %in% str_split(x, "_")[[1]][4] && whichbias[bias] %in% str_split(x, "_")[[1]][5])))]
    
    popbias <- whichpopbias[bias]
    
    #   for (i in 1:2) {
        
    namesforotherpop <- c("slice_1", "slice_2", "slice_3")
    stylesforotherpop <- c("low", "high")
    
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
    
    image_write(final_set, path = file.path(heatmap_sourcefolder, folderbias, paste0(popbias, "_", subpopulation[metricssexpop], "_measure_", inheritancecontainer[inheritancestyle], "_", stylesforotherpop[otherpopstyle], "_background.png")))

  }

  

  return(print("done"))
}
for (k in 1:2) {
    thing <- c(3, 11)


    for (i in 1:4) {
        for (j in 1:2) {
            combineeditsingles(thing[k],3,i,j, T)
        }
    }
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
  whichbias <- c("malebias", "femaleBias")
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





for (inhpattern in 1:4) {
  individualfigures(inhpattern, 2)

  for (spranges in 1:2) {
    stackmultiples(inhpattern, spranges)
  }
}

