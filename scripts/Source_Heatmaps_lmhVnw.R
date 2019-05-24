source(file.path("scripts", "Source_Reference_Section.R"))
referenceSection("heatmaps")

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

# heatmapLand <- HtMpDir(extraDir = "sameInh")
# heatmapLand <- file.path("results", "Heatmaps", "sameInh")
# heatmapLand <- file.path("results", "Heatmaps", "maleInh_maleBias")
# heatmapLand <- file.path("results", "Heatmaps", "femInh_maleBias")
# heatmapLand <- file.path("results", "Heatmaps", "femInh_femBias")
heatmapLand <- file.path("results")

# heatmapLand <- file.path("results", "Heatmaps")

# all_the_runs <- list.files(heatmapLand, 
all_the_runs <- extractVarDirs(heatmapLand, 
  #"_1[7-9][0-9]|2[0-9][0-9]|3[0-9][0-9]|4[0-1][0-9]_") # <- This was for the very first run - non-automated... more code to follow.
  #"190304_1[7-9][0-9]_|190304_2[0-8][0-9]_|190304_29[0-5]_")
  # "*_1[7-9][0-9]_|*_2[0-8][0-9]_|*_29[0-5]_")                # maleinh maleBias
  # "*_2[9][6-9]_|*_3[0-9][0-9]_|*_4[0-1][0-9]_|*_420_")       # mothinh maleBias
  # "*_42[1-9]_|*_4[3-9][0-9]_|*_5[0-3][0-9]_|*_54[0-5]_")      # mothinh femBias
  # "*_54[6-9]_|*_5[5-9][0-9]_|*_6[0-6][0-9]_|*_670_")     # sameinh femaleBias
  # "*_67[1-9]_|*_6[8-9][0-9]_|*_7[0-8][0-9]_|*_79[0-4]_")  # sameinh_maleBias
  # "*_79[4-9]_|*_8[0-9][0-9]_|*_90[0-9]_|*_91[0-7]_|*_1041_")   # oppinh maleBias
  # "*_794_|*_91[8-9]_|*_9[2-9][0-9]_|*_10[0-3][0-9]_|*_104[0-1]_")   # oppinh femBias
  ##### "*_104[2-9]_|*_10[5-9][0-9]_|*_11[0-5][0-9]_|*_116[0-5]_|*_1289_") # maleinh femBias
  ##### "*_116[6-9]_|*_11[7-9][0-9]_|*_12[0-8][0-9]_") # 
  # "*_129[0-9]_|*_13[0-9][0-9]_|*_140[0-9]_|*_141[0-4]_") # mixedCurInh - sNTn (males 90%, females 10%)
  # "*_141[5-9]_|*_14[2-9][0-9]_|*_15[0-9][0-9]_|*_16[0-5][0-9]_|*_166[0-2]_") # mixedCurInh_-_sSTf (males 75%, females 25%) 
  # "*_166[3-9]_|*_16[7-9][0-9]_|*_1[7-8][0-9][0-9]_|*_190[0-9]_|*_1910_") # mixedCurInh_-_sSFr (males 60%, females 40%) ### running on LeonServer
  # "*_191[1-9]_|*_19[2-9][0-9]_|*_20[1-2][0-9]_|*_203[0-5]_") # mothInh_femaleBias_SD=5 ### running on pComp
  # "*_203[6-9]_|*_20[4-9][0-9]_|*_21[0-9][0-9]_|*_22[0-7][0-9]_|*_228[0-3]_") # mixedCurInh_-_sTnN (sub curinh males - 10%, curinh females - 90%)
  "*_302[8-9]_|*_303[0-9]_|*_304[0-5]_")      # sameinh popsplit lmh
  # "*_304[6-9]_|*_305[0-3]_")      # sameinh popsplit nw
  # "*_305[4-9]_|*_30[6][0-9]_|*_307[0-1]")      # mixinh popsplit lmh
  # "*_307[2-9]_")      # mixinh popsplit nw
#   connection <- file(description = file.path("source","temp", paste0(specificSimNumber, "_sim_data.txt")), open = "rt")
#   multiRun_folderList <- as.vector(read.table(connection, -1L)[[2]])
#   close(connection)




# stuff <- vector("character", length(all_the_runs))
# for(thing in length(all_the_runs)) {
# stuff[thing] <- str_sub(all_the_runs[thing], 8L, 10L)
# }

### Opposite Inheritance - Female Bias (differing curstarts between populations)
# norm1 <- all_the_runs[1:5]  #794, 918-921
# norm2 <- all_the_runs[6:35] #1000-1029
# norm3 <- all_the_runs[36:113] #922-999
# norm4 <- all_the_runs[114:125] #1030-1041
# all_the_runs <- c(norm1, norm3, norm2, norm4)

# norm1 <- all_the_runs[1:86]  #546-631
# norm2 <- all_the_runs[87:101] #633-647
# norm3 <- all_the_runs[102:111] #649-658
# norm4 <- all_the_runs[112:115] #660-663
# norm5<- all_the_runs[116:121] #665-670
# all_the_runs <- c(norm1, all_the_runs[123], norm2, all_the_runs[125], norm3, all_the_runs[124], norm4, all_the_runs[122], norm5)

# norm1 <- all_the_runs[1:117]  #671-792
# norm2 <- all_the_runs[120:125] #633-647
# all_the_runs <- c(norm1, norm2, all_the_runs[118:119])

# norm1 <- all_the_runs[1:57]
# norm2 <- all_the_runs[58:124]
# all_the_runs <- c(norm1, all_the_runs[125], norm2)


# norm1 <- all_the_runs[1:12]  #421-432
# norm2 <- all_the_runs[13:37] #434-458
# norm3 <- all_the_runs[38:52] #460-474
# norm4 <- all_the_runs[53:67] #476-490
# norm5<- all_the_runs[68:82] #492-506
# norm6 <- all_the_runs[83:91] #508-516
# norm7 <- all_the_runs[95:123]#517-545
# # 92 - 475; 93 - 491; 94 - 507; 124 - 433; 125 - 459
# all_the_runs <- c(norm1, all_the_runs[124], norm2, all_the_runs[125], norm3, all_the_runs[92], norm4, all_the_runs[93], norm5, all_the_runs[94], norm6, norm7)
# # 


# # norm1 <- all_the_runs[1:57]  #421-432
# # norm2 <- all_the_runs[13:37] #434-458
# # norm3 <- all_the_runs[38:52] #460-474
# # norm4 <- all_the_runs[53:67] #476-490
# # norm5<- all_the_runs[68:82] #492-506
# # norm6 <- all_the_runs[83:91] #508-516
# # norm7 <- all_the_runs[95:123]#517-545
# # 92 - 475; 93 - 491; 94 - 507; 124 - 433; 125 - 459
# # all_the_runs <- c(norm1, all_the_runs[124], norm2, all_the_runs[125], norm3, all_the_runs[92], norm4, all_the_runs[93], norm5, all_the_runs[94], norm6, norm7)
# ;


# profvis({
# #   for(iteration in 1:10) {
#     extractedMeans <- extractMeans(allRunDirs = all_the_runs, 
#         dirHeatMap = heatmapLand, source_of_params = "params.yaml")
# #   }
# })





# extractedMeans <- extractMeans(allRunDirs = all_the_runs, dirHeatMap = heatmapLand, source_of_params = "params.yaml")
extractedMeans <- extractMeans(allRunDirs = all_the_runs, dirHeatMap = heatmapLand, source_of_params = "params.yaml", deeper = FALSE)

all_the_names <- remakeString(all_the_runs, "_", ".")

names(extractedMeans) <- all_the_names


# heatmapLand

# makeHeatmaps <- function (
#   inheritance = 1,
#   diffcurstartBias = 1
# )

#   whichInh <- c("male","moth","same","opps","sNTn","sSTf","sSFr","sFrS","sTfS","sTnN", "FfFf")

# whichBias <- c("male","female")

# makeHeatmapFile(inheritance = 3, diffcurstartBias = 3, absolute = TRUE, specialFigs = TRUE, lmhVnw = TRUE, extractedMeans = extractedMeans)
makeHeatmapFile(inheritance = 3, diffcurstartBias = 3, absolute = TRUE, specialFigs = TRUE, lmhVnw = FALSE, extractedMeans = extractedMeans)
# makeHeatmapFile(inheritance = 11, diffcurstartBias = 3, absolute = TRUE, specialFigs = TRUE, lmhVnw = TRUE, extractedMeans = extractedMeans)
# makeHeatmapFile(inheritance = 11, diffcurstartBias = 3, absolute = TRUE, specialFigs = TRUE, lmhVnw = FALSE, extractedMeans = extractedMeans)
# makeHeatmaps(inheritance = 1, diffcurstartBias = 1, absolute = TRUE, reDo = TRUE)
# makeHeatmaps(inheritance = 2, diffcurstartBias = 1, absolute = TRUE, reDo = TRUE)
# makeHeatmaps(inheritance = 1, diffcurstartBias = 2, absolute = TRUE, reDo = TRUE)
# makeHeatmaps(inheritance = 2, diffcurstartBias = 2, absolute = TRUE, reDo = TRUE)

# makeHeatmaps(inheritance = 3, diffcurstartBias = 1, absolute = TRUE, reDo = TRUE)
# makeHeatmaps(inheritance = 3, diffcurstartBias = 2, absolute = TRUE, reDo = TRUE)
# makeHeatmaps(inheritance = 4, diffcurstartBias = 1, absolute = TRUE, reDo = TRUE)
# makeHeatmaps(inheritance = 4, diffcurstartBias = 2, absolute = TRUE, reDo = TRUE)





  # whichInh <- c("male","moth","same","opps","sNTn","sSTf","sSFr","sFrS","sTfS","sTnN", "FfFf")






IndividualFigures <- function (

  inheritance = 1, #c("maleinh", "mothinh", "sameinh", "oppsinh","sNTninh", "sSTfinh", "sSFrinh", "sFrSinh", "sTfSinh", "sTnNinh", "FfFfinh")
  colorRange = 2, # c("relative", "absolute")
  thisBias = 1, # 3 or 4
  numOtherPopRuns = 2 # this will force the dimensions of the data structure to include only the number of starting conditions that are present for the population of non-interest
) {
  
  # heatmap_sourceFolder <- file.path("results", "Heatmaps", "output_objects")
  heatmap_sourceFolder <- file.path("results")
  # heatmap_sourceFolder <- file.path("sameSexFigResults", "results")
  

  # Character vectors for args - indices  Not sure what else to call them, but they'll be used to reassign the args to non-numeric objects

  ClrRngContainer <- c("relative", "absolute")
  
  inheritanceContainer <- c("maleinh", "mothinh", "sameinh", "oppsinh",
                            "sNTninh", "sSTfinh", "sSFrinh", "sFrSinh",
                            "sTfSinh", "sTnNinh", "FfFfinh")

  whichBias <- c("maleBias", "femaleBias", "pop1Bias", "pop2Bias", "bothBias")

  if (thisBias == 1) {
    heatmap_axes <- list(
      mp2Vfem = c("Pop 2 Male Starting Curiosity", "Female Starting Curiosity"),    # mp2Vfem
      mp1Vfem = c("Pop 1 Male Starting Curiosity", "Female Starting Curiosity"),    # mp1Vfem
      mp1Vmp2 = c("Pop 1 Male Starting Curiosity", "Pop 2 Male Starting Curiosity") # mp1Vmp2
    )
    slicedPop <- list(
      "MalPop1",
      "MalPop2",
      "FemalePop"
    )
  } else if (thisBias == 2) {
    heatmap_axes <- list(
      mf2Vmal = c("Pop 2 Female Starting Curiosity", "Male Starting Curiosity"),
      mf1Vmal = c("Pop 1 Female Starting Curiosity", "Male Starting Curiosity"),
      mf1Vmf2 = c("Pop 1 Female Starting Curiosity", "Pop 2 Female Starting Curiosity")
    )
    slicedPop <- list(
      "FemPop1",
      "FemPop2",
      "MalePop"
    )
  } else if (thisBias == 3) {
    heatmap_axes <- list(
      fp1Vpp2 = c("Pop 1 Female Starting Curiosity", "Pop 2 Starting Curiosity"),
      mp1Vpp2 = c("Pop 1 Male Starting Curiosity", "Pop 2 Starting Curiosity"),
      mp1Vfp1 = c("Pop 1 Male Starting Curiosity", "Pop 1 Female Starting Curiosity")
    )
    slicedPop <- list(
      "MalPop1",
      "FemPop1",
      "Popula2"
    )
  }

  colorRange <- ClrRngContainer[colorRange]

  inheritance <- inheritanceContainer[inheritance]

  thisBias <- whichBias[thisBias]
  
  folderName <- list.files(heatmap_sourceFolder)[which(sapply(list.files(heatmap_sourceFolder), function(x) (inheritance %in% str_split(x, "_")[[1]][4] && thisBias %in% str_split(x, "_")[[1]][5])))]

  # tempHtMpArray <- readRDS(file.path(heatmap_sourceFolder, folderName, list.files(file.path(heatmap_sourceFolder, folderName), pattern = ".RData")))
  HtMpArrays <- list.files(file.path(heatmap_sourceFolder, folderName), pattern = ".RData")

  if (length (HtMpArrays) == 1) {
    tempHtMpArray <- readRDS(file.path(heatmap_sourceFolder, folderName, HtMpArrays))

    lowMedHigh <- tempHtMpArray[1:3,1:3,1:3,]
    narrowWide <- tempHtMpArray[4:5,4:5,4:5,]
    
  } else if (length (HtMpArrays) == 2) {
    lowMedHigh <- readRDS(file.path("results", folderName, list.files(file.path("results", folderName), pattern = "lowMedHigh.RData")))
    narrowWide <- readRDS(file.path("results", folderName, list.files(file.path("results", folderName), pattern = "narrowWide.RData")))
  }

  inhOptions <- list(
    lowMedHigh = lowMedHigh,
    narrowWide = narrowWide,
    LMHtext = "lowMedHigh",
    NWtext = "narrowWide"
  )
  
  # DONE.
  # NOW WE NEED TO MAKE THE FIGURES AND SORT THEM INTO FOLDERS THAT WE'LL PULL THEM OUT OF TO MAKE THE STITCHED-TOGETHER FILES.
  # TITLES DON'T MATTER CURRENTLY, BUT WILL ONCE THEY GET STITCHED TOGETHER.
  # THIS DIRECTORY (FOR THESE UNSTITCHED ONES) SHOULD BE A SUBFOLDER WITHIN THE STITCHED FIGURE DIR

  if(!(dir.exists(file.path(
    heatmap_sourceFolder, folderName, "lowMedHigh")))
  ) {
    dir.create(file.path(
      heatmap_sourceFolder, folderName, "lowMedHigh"
    ))
    dir.create(file.path(
      heatmap_sourceFolder, folderName, "narrowWide"
    ))
    
  }

  colorSeqMultPalette <- list(
    BuGn = colorRampPalette(c("#e5f5f9", "#99d8c9", "#2ca25f")), # 3-class BuGn
    BuPu = colorRampPalette(c("#e0ecf4", "#9ebcda", "#8856a7")), # 3-class BuPu
    GnBu = colorRampPalette(c("#e0f3db", "#a8ddb5", "#43a2ca")), # 3-class GnBu
    OrRd = colorRampPalette(c("#fee8c8", "#fdbb84", "#e34a33")), # 3-class OrRd
    PuBu = colorRampPalette(c("#ece7f2", "#a6bddb", "#2b8cbe")), # 3-class PuBu
    PuBuGn = colorRampPalette(c("#ece2f0", "#a6bddb", "#1c9099")), # 3-class PuBuGn
    PuRd = colorRampPalette(c("#e7e1ef", "#c994c7", "#dd1c77")), # 3-class PuRd
    RdPu = colorRampPalette(c("#fde0dd", "#fa9fb5", "#c51b8a")), # 3-class RdPu
    YlGn = colorRampPalette(c("#f7fcb9", "#addd8e", "#31a354")), # 3-class YlGn
    YlGnBu = colorRampPalette(c("#edf8b1", "#7fcdbb", "#2c7fb8")), # 3-class YlGnBu
    YlOrBr = colorRampPalette(c("#fff7bc", "#fec44f", "#d95f0e")), # 3-class YlOrBr
    YlOrRd = colorRampPalette(c("#ffeda0", "#feb24c", "#f03b20")), # 3-class YlOrRd
    Greys = colorRampPalette(c("#f0f0f0", "#bdbdbd", "#636363"))#, # 3-class Greys
    # Reds = #fee0d2, #fc9272, #de2d26
    # Purples = #efedf5, #bcbddc, #756bb1
    # Oranges = #fee6ce, #fdae6b, #e6550d
    # Greens = #e5f5e0, #a1d99b, #31a354
    # Blues = #deebf7, #9ecae1, #3182bd

  )

  regularNames <- c(
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

  for (htmpView in 1:3) { # looking at the cubes from different angles (aka which population are we seeing one slice at a time, while the other populations are plotted on the axes?)
    for (SxMtPop in 1:8) { # curiosity and sylrep data for each subpopulation
      for (inhStyle in 1:2) { # lowMedHigh and narrowWide
        
        if(!(dir.exists(file.path(
          heatmap_sourceFolder, folderName, inhOptions[inhStyle + 2], 
          slicedPop[htmpView] # paste0("slice_", slice)
        )))) {
          dir.create(file.path(
            heatmap_sourceFolder, folderName, inhOptions[inhStyle + 2], 
            slicedPop[htmpView] # paste0("slice_", slice)
          ))
        }

        # dir.create(file.path(
        #     heatmap_sourceFolder, folderName, inhOptions[inhStyle + 2]
        # ))
        if (inhStyle == 1) {
          if (htmpView == 3) {
            sliceNum = numOtherPopRuns
          } else {
            sliceNum = 3}
        } else if (inhStyle == 2) {
          sliceNum = 2}

        for (slice in 1:sliceNum) {
          
          file_name <- paste0(regularNames[SxMtPop], "_slice_", slice, "_", slicedPop[htmpView], ".png")
          # rule of thumb: if we're splitting up htmpView _within_ slice and SxMtPop, then we need to save the output files according to the schema that will help pull back together the slices.
          png(filename = file.path(
              heatmap_sourceFolder, folderName, inhOptions[inhStyle + 2], 
              # paste0("slice_", slice), file_name), 
              slicedPop[htmpView], file_name), 
            width = 554, height = 554, units = "px", pointsize = 12, bg = "white")

          if(colorRange == "absolute") {
            if (SxMtPop <= 4) {heatmapRange <- c(0,1)
            } else {           heatmapRange <- c(1,156)}
          } else {
            
            heatmapRange <- inhOptions[[inhStyle]][
              dat_array_doh[1,1,1,slice]:dat_array_doh[1,1,2,slice],
              dat_array_doh[1,2,1,slice]:dat_array_doh[1,2,2,slice],
              dat_array_doh[1,3,1,slice]:dat_array_doh[1,3,2,slice],
              SxMtPop]
            heatmap_min <- c(
              round(min(heatmapRangeDatasetOne), 2),
              round(min(heatmapRangeDatasetTwo), 2),
              round(min(heatmapRangeDatasetTre), 2)
            )
            heatmap_max <- c(
              round(max(heatmapRangeDatasetOne), 2),
              round(max(heatmapRangeDatasetTwo), 2),
              round(max(heatmapRangeDatasetTre), 2)
            )
            
            heatmapRange <- c(heatmap_min[htmpView]-0.01,heatmap_max[htmpView]+0.01)
            rm(heatmapRangeDatasetOne, heatmapRangeDatasetTwo, heatmapRangeDatasetTre,
              heatmap_min, heatmap_max)
          } # UNFINISHED
          findXLab <- heatmap_axes[[htmpView]][1]
          findYLab <- heatmap_axes[[htmpView]][2]
          
          if(inhStyle == 1) {
            dim_1 = 3
            dim_2 = 3
            dim_3 = 2
            dat_array_doh <- array(c(
              1,1,1, 1,1,1, 1,1,1, 1,3,3, 3,1,3, numOtherPopRuns,numOtherPopRuns,1,
              2,1,1, 1,2,1, 1,1,2, 2,3,3, 3,2,3, numOtherPopRuns,numOtherPopRuns,2,
              3,1,1, 1,3,1, 1,1,numOtherPopRuns, 3,3,3, 3,3,3, numOtherPopRuns,numOtherPopRuns,numOtherPopRuns
              # rep(c(1, 1, 1, 1), 2), 1, 1, rep(c(3, 3, 3, 1), 2),
              # rep(c(2, 1, 1, 1), 2), 2, 2, rep(c(3, 3, 3, 2), 2),
              # rep(c(3, 1, 1, 1), 2), 3, 3, rep(c(3, 3, 3, 3), 2)
            ), c(3,3,2,3))

            image(x = matrix(as.numeric(
            inhOptions[[inhStyle]][
              dat_array_doh[htmpView,1,1,slice]:dat_array_doh[htmpView,1,2,slice],
              dat_array_doh[htmpView,2,1,slice]:dat_array_doh[htmpView,2,2,slice],
              dat_array_doh[htmpView,3,1,slice]:dat_array_doh[htmpView,3,2,slice],
              SxMtPop
            ]),nrow(inhOptions[[inhStyle]][
              dat_array_doh[htmpView,1,1,slice]:dat_array_doh[htmpView,1,2,slice],
              dat_array_doh[htmpView,2,1,slice]:dat_array_doh[htmpView,2,2,slice],
              dat_array_doh[htmpView,3,1,slice]:dat_array_doh[htmpView,3,2,slice],
              SxMtPop
            ]), ncol(inhOptions[[inhStyle]][
              dat_array_doh[htmpView,1,1,slice]:dat_array_doh[htmpView,1,2,slice],
              dat_array_doh[htmpView,2,1,slice]:dat_array_doh[htmpView,2,2,slice],
              dat_array_doh[htmpView,3,1,slice]:dat_array_doh[htmpView,3,2,slice],
              SxMtPop
            ])),
            col = colorSeqMultPalette$Greys(100),
            axes = F, 
            xlab = findXLab, 
            ylab = findYLab,cex.lab=1.4, zlim = heatmapRange)

            axis(1,c(-0.25 ,0      ,0.25  ,0.5      ,0.75  ,0.97    ,1.25),
                c(""    ,"0-.25",""    , ".25-.5",""    , ".45-1",""  ),
                T,0,NA,F,cex.axis=1, tck = 0)
            axis(1,c(-0.25,0.25,0.75,1.25),
                c("","","",""),
                T,-0.03,NA,F,cex.axis=1, tck = -0.03)

            if (htmpView == 3) {
              axis(2,c(-0.25 ,0      ,0.25  ,0.5      ,0.75  ,0.97    ,1.25),
                c(""    ,"0-.25",""    , ".25-.5",""    , ".45-1",""  ),
                T,0,NA,F,cex.axis=0.6, tck = 0)
              axis(2,c(-0.25,0.25,0.75,1.25),
                c("","","",""),
                T,-0.03,NA,F,cex.axis=1, tck = -0.03)
            } else {
              axis(2,c(-0.5,  0  ,0.5,    1    ,1.5),
                c(  ""   ,"0-0.25","" ,".45-1","" ),
                T,0,NA,F,cex.axis=0.8, tck = 0)
              axis(2,c(-0.5,0.5,1.5),
                c("","",""),
                T,-0.03,NA,F,cex.axis=1, tck = -0.03)
            }

          } else if (inhStyle == 2) {

            dat_array_doh <- array(c(
              rep(c(1, 1, 1, 1), 2), 1, 1, rep(c(2, 2, 2, 1), 2),
              rep(c(2, 1, 1, 1), 2), 2, 2, rep(c(2, 2, 2, 2), 2)
            ), c(3,3,2,2))

            image(x = matrix(as.numeric(
            inhOptions[[inhStyle]][
              dat_array_doh[htmpView,1,1,slice]:dat_array_doh[htmpView,1,2,slice],
              dat_array_doh[htmpView,2,1,slice]:dat_array_doh[htmpView,2,2,slice],
              dat_array_doh[htmpView,3,1,slice]:dat_array_doh[htmpView,3,2,slice],
              SxMtPop
            ]),nrow(inhOptions[[inhStyle]][
              dat_array_doh[htmpView,1,1,slice]:dat_array_doh[htmpView,1,2,slice],
              dat_array_doh[htmpView,2,1,slice]:dat_array_doh[htmpView,2,2,slice],
              dat_array_doh[htmpView,3,1,slice]:dat_array_doh[htmpView,3,2,slice],
              SxMtPop
            ]), ncol(inhOptions[[inhStyle]][
              dat_array_doh[htmpView,1,1,slice]:dat_array_doh[htmpView,1,2,slice],
              dat_array_doh[htmpView,2,1,slice]:dat_array_doh[htmpView,2,2,slice],
              dat_array_doh[htmpView,3,1,slice]:dat_array_doh[htmpView,3,2,slice],
              SxMtPop
            ])),
            col = colorSeqMultPalette$YlOrBr(100),
            axes = F, 
            xlab = findXLab, 
            ylab = findYLab,cex.lab=1.4, zlim = heatmapRange)

            axis(1,c(-0.5,  0  ,0.5,    1    ,1.5),
                c(  ""   ,"0-1","" ,".45-.55","" ),
                T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(1,c(-0.5,0.5,1.5),
                c("","",""),
                T,-0.03,NA,F,cex.axis=1, tck = -0.03)
            
            axis(2,c(-0.5,  0  ,0.5,    1    ,1.5),
                c(  ""   ,"0-1","" ,".45-.55","" ),
                T,0,NA,F,cex.axis=0.6, tck = 0)
            axis(2,c(-0.5,0.5,1.5),
                c("","",""),
                T,-0.03,NA,F,cex.axis=1, tck = -0.03)
          }
          
          dev.off()
        }

      }
    }
  }
  # }
  return(print("Done, in the specified folder"))
}

CombineEditSingles <- function (
  inheritanceStyle = 1,
  bias = 1,
  metricsSexPop = 1, # only allowed 1-4 (p1m, p2m, p1f, p2f)
  otherPopStyle = 1, # low, high (for lmh & nw stuff round May 2019)
  edit = FALSE
) {
  
#   source("/home/parker/Documents/projects/curmodel_pcomp1/Code/curiosity-code/scripts/Source_Magick_Functions.R")
# Access the same subdirectory where the individual images are stored
  

#  heatmap_sourceFolder <- file.path("results", "Heatmaps", "output_objects")
  heatmap_sourceFolder <- file.path("results")
  
  SxMtPopContainer <- c("EndCurValP1M",
                          "EndCurValP2M",
                          "EndCurValP1F",
                          "EndCurValP2F",
                          "EndSRpValP1M",
                          "EndSRpValP2M",
                          "EndSRpValP1F",
                          "EndSRpValP2F")
  
  
  
  curstartPatternContainer <- c("lowMedHigh", "narrowWide")
  
  inheritanceContainer <- c("maleinh", "mothinh", "sameinh", "oppsinh",
                              "sNTninh", "sSTfinh", "sSFrinh", "sFrSinh",
                              "sTfSinh", "sTnNinh", "FfFfinh")

  titleSxMtPop <- c("Pop 1 Mal", "Pop 2 Mal", 
                    "Pop 1 Fem", "Pop 2 Fem", 
                    "Pop 1 Mal", "Pop 2 Mal", 
                    "Pop 1 Fem", "Pop 2 Fem")

  titleInhStyle <- c("Male", "Female", "Same-Sex", "Opposite",
                     "90M10F", "75M25F", "60M40F", "40M60F", 
                     "25M75F", "10M90F", "50-50")

  titleBackgroundPop <- c("Low", "High")
  
  whichBias <- c("maleBias", "femaleBias", "pop1Bias")
  whichPopBias <- c("FemalePop", "MalePop", "Popula2")
  
  subpopulation <- c("p1m", "p2m", "p1f", "p2f")
  
  folderBias <- list.files(heatmap_sourceFolder)[which(sapply(list.files(heatmap_sourceFolder), function(x) (inheritanceContainer[inheritanceStyle] %in% str_split(x, "_")[[1]][4] && whichBias[bias] %in% str_split(x, "_")[[1]][5])))]
  
  PopBias <- whichPopBias[bias]
  
  #   for (i in 1:2) {
      
  namesForOtherPop <- c("slice_1", "slice_2")
  stylesForOtherPop <- c("low", "high")
  
  lowMedHighFolder <- file.path(heatmap_sourceFolder, folderBias, curstartPatternContainer[1], PopBias)
  narrowWideFolder <- file.path(heatmap_sourceFolder, folderBias, curstartPatternContainer[2], PopBias)

  image_1 <- image_read(file.path(lowMedHighFolder, paste0(SxMtPopContainer[metricsSexPop], "_", namesForOtherPop[otherPopStyle], "_", PopBias, ".png")))
  image_2 <- image_read(file.path(narrowWideFolder, paste0(SxMtPopContainer[metricsSexPop], "_", namesForOtherPop[otherPopStyle], "_", PopBias, ".png")))
  image_3 <- image_read(file.path(lowMedHighFolder, paste0(SxMtPopContainer[metricsSexPop+4], "_", namesForOtherPop[otherPopStyle], "_", PopBias, ".png")))
  image_4 <- image_read(file.path(narrowWideFolder, paste0(SxMtPopContainer[metricsSexPop+4], "_", namesForOtherPop[otherPopStyle], "_", PopBias, ".png")))

  if(! (edit)) {
    
    top_row <- image_append(c(image_1, image_2))
    bottom_row <- image_append(c(image_3, image_4))
    final_set <- image_append(c(top_row, bottom_row), stack = TRUE)
    # image_write(final_set, path = file.path(heatmap_sourceFolder, folderBias, paste0(PopBias, "_", subpopulation[metricsSexPop], "_measure_", stylesForOtherPop[otherPopStyle], "_background.png")))
  
  } else if(edit) {
    
    top_row <- image_append(c(image_1, image_2))
    bottom_row <- image_append(c(image_3, image_4))
    final_set <- image_append(c(top_row, bottom_row), stack = TRUE)
    
    final_set <- image_border(final_set, "white", "75x75")

    final_set <- image_annotate(
        final_set, paste0(titleSxMtPop[metricsSexPop], 
                          " Ending Traits - ", 
                          titleInhStyle[inheritanceStyle],
                          " AC Inheritance"), 
        size="50", 
        location = "+40+25")

    final_set <- image_annotate(
        final_set, paste0(titleBackgroundPop[otherPopStyle], " Background Population Starting Curiosity"), 
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
    #     heatmap_sourceFolder, folderBias, paste0(
    #                 "Popula2", "_", "p1f", 
    #                 "_measure_", "low", 
    #                 "_background.png")))
      # This is where we edit the stuff we worked on! 
      # There need to be ways to access the files made in the first half, and it should also contain everything within another control structure: "if(files exist in folder) {} else {print("Great Job, oh wait this is an error message. Um, you should make sure the function is pointed at the right files. Are the right ones perhaps absent?")}"

  }
  
  image_write(final_set, path = file.path(heatmap_sourceFolder, folderBias, paste0(PopBias, "_", subpopulation[metricsSexPop], "_measure_", inheritanceContainer[inheritanceStyle], "_", stylesForOtherPop[otherPopStyle], "_background.png")))

  return(print("done"))
}
for (k in 1:2) {
    thing <- c(3, 11)


    for (i in 1:4) {
        for (j in 1:2) {
            CombineEditSingles(thing[k],3,i,j, T)
        }
    }
}
stackMultiples <- function (
  inheritance = 1, # c("sameinh", "oppsinh", "maleinh", "mothinh")
  pattern = 1 # 1 = narrowWide, 2 = lowMedHigh
) {
  

  # maleInhMaleVFemaleBias

  SxMtPopContainer <- c("EndCurValP1M",
                        "EndCurValP2M",
                        "EndCurValP1F",
                        "EndCurValP2F",
                        "EndSRpValP1M",
                        "EndSRpValP2M",
                        "EndSRpValP1F",
                        "EndSRpValP2F")

  heatmap_sourceFolder <- file.path("results", "Heatmaps", "output_objects")
  whichBias <- c("maleBias", "femaleBias")
  whichPopBias <- c("FemalePop", "MalePop")
  # folderBias <- list.files(heatmap_sourceFolder)[which(sapply(list.files(heatmap_sourceFolder), function(x) (inheritance %in% str_split(x, "_")[[1]][4] && whichBias[bias] %in% str_split(x, "_")[[1]][5])))]
  curstartPatternContainer <- c("narrowWide", "lowMedHigh")
  # relevantFolder <- file.path(heatmap_sourceFolder, folderBias, curstartPatternContainer[pattern])

  inheritanceContainer <- c("sameinh", "oppsinh", "maleinh", "mothinh")
  inheritance <- inheritanceContainer[inheritance]
  
  heatmap_sourceFolder <- file.path("results", "Heatmaps", "output_objects")
  
  output_folder <- file.path(heatmap_sourceFolder, paste0("Combined_", inheritance))# "_pattern_", curstartPatternContainer[pattern]))
  if(!(dir.exists(output_folder))) {dir.create(output_folder)}
  if(!(dir.exists(file.path(output_folder, curstartPatternContainer[pattern])))) {dir.create(file.path(output_folder, curstartPatternContainer[pattern]))}

  maleBias <- list.files(heatmap_sourceFolder)[which(sapply(list.files(heatmap_sourceFolder), function(x) (inheritance %in% str_split(x, "_")[[1]][4] && whichBias[1] %in% str_split(x, "_")[[1]][5])))]
  femsBias <- list.files(heatmap_sourceFolder)[which(sapply(list.files(heatmap_sourceFolder), function(x) (inheritance %in% str_split(x, "_")[[1]][4] && whichBias[2] %in% str_split(x, "_")[[1]][5])))]
  
  for (metSxPop in 1:8) {
    stackOne <- CombineSingles(inheritance, 1, metSxPop, pattern)
    stackTwo <- CombineSingles(inheritance, 2, metSxPop, pattern)
    # stackOne <- image_read(file.path(heatmap_sourceFolder, maleBias, curstartPatternContainer[pattern]), paste0(SxMtPopContainer[metSxPop], "_", whichPopBias[1], ".png"))
    # stackTwo <- image_read(file.path(heatmap_sourceFolder, femsBias, curstartPatternContainer[pattern]), paste0(SxMtPopContainer[metSxPop], "_", whichPopBias[2], ".png"))
    thing <- image_append(c(stackOne, stackTwo), stack = TRUE)
    image_write(thing, path = file.path(output_folder, curstartPatternContainer[pattern]))
  }

  # stackOne <- image_read(file.path(heatmap_sourceFolder, maleBias, curstartPatternContainer[pattern]),)
  # stackTwo <- image_read(file.path(heatmap_sourceFolder, femsBias, curstartPatternContainer[pattern]),)


}





for (inhPattern in 1:4) {
  IndividualFigures(inhPattern, 2)

  for (SPranges in 1:2) {
    stackMultiples(inhPattern, SPranges)
  }
}

