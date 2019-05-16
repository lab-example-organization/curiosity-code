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
heatmapLand <- file.path("results", "Heatmaps", "femInh_femBias")

# heatmapLand <- file.path("results", "Heatmaps")

# all_the_runs <- list.files(heatmapLand, 
all_the_runs <- extractVarDirs(heatmapLand, 
  #"_1[7-9][0-9]|2[0-9][0-9]|3[0-9][0-9]|4[0-1][0-9]_") # <- This was for the very first run - non-automated... more code to follow.
  #"190304_1[7-9][0-9]_|190304_2[0-8][0-9]_|190304_29[0-5]_")
  # "*_1[7-9][0-9]_|*_2[0-8][0-9]_|*_29[0-5]_")                # maleinh maleBias
  # "*_2[9][6-9]_|*_3[0-9][0-9]_|*_4[0-1][0-9]_|*_420_")       # mothinh maleBias
  "*_42[1-9]_|*_4[3-9][0-9]_|*_5[0-3][0-9]_|*_54[0-5]_")      # mothinh femBias
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


norm1 <- all_the_runs[1:12]  #421-432
norm2 <- all_the_runs[13:37] #434-458
norm3 <- all_the_runs[38:52] #460-474
norm4 <- all_the_runs[53:67] #476-490
norm5<- all_the_runs[68:82] #492-506
norm6 <- all_the_runs[83:91] #508-516
norm7 <- all_the_runs[95:123]#517-545
# 92 - 475; 93 - 491; 94 - 507; 124 - 433; 125 - 459
all_the_runs <- c(norm1, all_the_runs[124], norm2, all_the_runs[125], norm3, all_the_runs[92], norm4, all_the_runs[93], norm5, all_the_runs[94], norm6, norm7)
# 


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


profvis({
#   for(iteration in 1:10) {
    extractedMeans <- extractMeans(allRunDirs = all_the_runs, 
        dirHeatMap = heatmapLand, source_of_params = "params.yaml")
#   }
})





# extractedMeans <- extractMeans(allRunDirs = all_the_runs, dirHeatMap = heatmapLand, source_of_params = "params.yaml")
extractedMeans <- extractMeans(allRunDirs = all_the_runs, dirHeatMap = heatmapLand, source_of_params = "params.yaml", deeper = FALSE)

all_the_names <- remakeString(all_the_runs, "_", ".")

names(extractedMeans) <- all_the_names


# heatmapLand

# makeHeatmaps <- function (
#   inheritance = 1,
#   diffcurstartBias = 1
# )

# whichInh <- c("male","moth","same","opps","sNTn","sSTf","sSFr","sFrS","sTfS","sTnN")

# whichBias <- c("male","female")

makeHeatmaps(inheritance = 2, diffcurstartBias = 2, absolute = TRUE)
# makeHeatmaps(inheritance = 1, diffcurstartBias = 1, absolute = TRUE, reDo = TRUE)
# makeHeatmaps(inheritance = 2, diffcurstartBias = 1, absolute = TRUE, reDo = TRUE)
# makeHeatmaps(inheritance = 1, diffcurstartBias = 2, absolute = TRUE, reDo = TRUE)
# makeHeatmaps(inheritance = 2, diffcurstartBias = 2, absolute = TRUE, reDo = TRUE)

# makeHeatmaps(inheritance = 3, diffcurstartBias = 1, absolute = TRUE, reDo = TRUE)
# makeHeatmaps(inheritance = 3, diffcurstartBias = 2, absolute = TRUE, reDo = TRUE)
# makeHeatmaps(inheritance = 4, diffcurstartBias = 1, absolute = TRUE, reDo = TRUE)
# makeHeatmaps(inheritance = 4, diffcurstartBias = 2, absolute = TRUE, reDo = TRUE)





IndividualFigures <- function (

  inheritance = 1, #c("sameinh", "oppsinh", "maleinh", "mothinh")
  colorRange = 2 # c("relative", "absolute")

) {
  
  ClrRngContainer <- c("relative", "absolute")
  colorRange <- ClrRngContainer[colorRange]

  inheritanceContainer <- c("sameinh", "oppsinh", "maleinh", "mothinh")
  inheritance <- inheritanceContainer[inheritance]

  heatmap_sourceFolder <- file.path("results", "Heatmaps", "output_objects")

  # heatmapSource_folderList <- c(
  #     "190421_slices_-_sameinh_maleBias",
  #     "190421_slices_-_sameinh_femaleBias",
  #     "190419_slices_-_oppsinh_maleBias",
  #     "190419_slices_-_oppsinh_femaleBias",
  #     # "190427_slices_-_maleinh_maleBias",
  #     # "190410_slices_-_maleinh_femBias",
  #     "190430_slices_-_mothinh_maleBias",
  #     "190430_slices_-_mothinh_femaleBias"
  # )

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
    YlOrRd = colorRampPalette(c("#ffeda0", "#feb24c", "#f03b20")))

  SxMtPop_list <- c(
    "Ending Curiosity Values - Pop 1 Females_slice_",
    "Ending Curiosity Values - Pop 1 Males_slice_",
    "Ending Curiosity Values - Pop 2 Females_slice_",
    "Ending Curiosity Values - Pop 2 Males_slice_",
    "Ending Sylrep Values - Pop 1 Females_slice_",
    "Ending Sylrep Values - Pop 1 Males_slice_",
    "Ending Sylrep Values - Pop 2 Females_slice_",
    "Ending Sylrep Values - Pop 2 Males_slice_"
  )

  slice_names <- c(
    "slice_1",
    "slice_2",
    "slice_3",
    "slice_4",
    "slice_5"
  )

  title_names <- c("Ending Curiosity Values - Pop 1 Females",
                   "Ending Curiosity Values - Pop 1 Males",
                 "Ending Curiosity Values - Pop 2 Females",
                 "Ending Curiosity Values - Pop 2 Males",
                   "Ending Syll Rept Values - Pop 1 Females",
                   "Ending Syll Rept Values - Pop 1 Males",
                 "Ending Syll Rept Values - Pop 2 Females",
                 "Ending Syll Rept Values - Pop 2 Males"
  )

  regularNames <- c("EndCurValP1M",
                    "EndCurValP2M",
                    "EndCurValP1F",
                    "EndCurValP2F",
                    "EndSRpValP1M",
                    "EndSRpValP2M",
                    "EndSRpValP1F",
                    "EndSRpValP2F"
  )

  # source("/home/parker/Documents/projects/curmodel_pcomp1/Code/curiosity-code/scripts/Source_Magick_Functions.R")

  whichBias <- c("maleBias", "femaleBias")

  for(thisBias in 1:2) {

    folderName <- list.files(heatmap_sourceFolder)[which(sapply(list.files(heatmap_sourceFolder), function(x) (inheritance %in% str_split(x, "_")[[1]][4] && whichBias[thisBias] %in% str_split(x, "_")[[1]][5])))]

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
    } else {
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
    }

    tempHtMpArray <- readRDS(file.path(heatmap_sourceFolder, folderName, list.files(file.path(heatmap_sourceFolder, folderName), pattern = ".RData")))

    # REDUCE THE SHIT.

    lowMedHigh <- tempHtMpArray[1:3,1:3,1:3,]
    narrowWide <- tempHtMpArray[4:5,4:5,4:5,]

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

    for (htmpView in 1:3) {
      for (SxMtPop in 1:8) {
        for (inhStyle in 1:2) {
          
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
            sliceNum = 3
            dat_array_doh <- array(c(
                rep(c(1, 1, 1, 1), 2), 1, 1, rep(c(3, 3, 3, 1), 2),
                rep(c(2, 1, 1, 1), 2), 2, 2, rep(c(3, 3, 3, 2), 2),
                rep(c(3, 1, 1, 1), 2), 3, 3, rep(c(3, 3, 3, 3), 2)
              ), c(3,3,2,3))
          } else if (inhStyle == 2) {
            sliceNum = 2
            dat_array_doh <- array(c(
                rep(c(1, 1, 1, 1), 2), 1, 1, rep(c(2, 2, 2, 1), 2),
                rep(c(2, 1, 1, 1), 2), 2, 2, rep(c(2, 2, 2, 2), 2)
              ), c(3,3,2,2))
          }
          for (slice in 1:sliceNum) {
            
            file_name <- paste0(regularNames[SxMtPop], "_slice_", slice, "_", slicedPop[htmpView], ".png")
            # rule of thumb: if we're splitting up htmpView _within_ slice and SxMtPop, then we need to save the output files according to the schema that will help pull back together the slices.
            png(filename = file.path(
                heatmap_sourceFolder, folderName, inhOptions[inhStyle + 2], 
                # paste0("slice_", slice), file_name), 
                slicedPop[htmpView], file_name), 
              width = 554, height = 554, units = "px", pointsize = 12, bg = "white")

            if(colorRange == "absolute") {
              if ("Curiosity" %in% str_split(title_names[SxMtPop], " ")[[1]]
              ) {heatmapRange <- c(0,1)} else {heatmapRange <- c(1,156)}
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

              image(x = matrix(as.numeric(
              inhOptions[[inhStyle]][
                dat_array_doh[htmpView,1,1,slice]:dat_array_doh[htmpView,1,2,slice],
                dat_array_doh[htmpView,2,1,slice]:dat_array_doh[htmpView,2,2,slice],
                dat_array_doh[htmpView,3,1,slice]:dat_array_doh[htmpView,3,2,slice],
                SxMtPop
              ]),sliceNum,sliceNum),
              col = colorSeqMultPalette$YlOrBr(100),
              axes = F, 
              xlab = findXLab, 
              ylab = findYLab,cex.lab=1.4, zlim = heatmapRange)

              axis(1,c(-0.25 ,0      ,0.25  ,0.5      ,0.75  ,0.97    ,1.25),
                  c(""    ,"0-.25",""    , ".25-.5",""    , ".45-1",""  ),
                  T,0,NA,F,cex.axis=1, tck = 0)
              axis(1,c(-0.25,0.25,0.75,1.25),
                  c("","","",""),
                  T,-0.03,NA,F,cex.axis=1, tck = -0.03)
              
              axis(2,c(-0.25 ,0      ,0.25  ,0.5      ,0.75  ,0.97    ,1.25),
                  c(""    ,"0-.25",""    , ".25-.5",""    , ".45-1",""  ),
                  T,0,NA,F,cex.axis=0.6, tck = 0)
              axis(2,c(-0.25,0.25,0.75,1.25),
                  c("","","",""),
                  T,-0.03,NA,F,cex.axis=1, tck = -0.03)
            } else if (inhStyle == 2) {

              image(x = matrix(as.numeric(
              inhOptions[[inhStyle]][
                dat_array_doh[htmpView,1,1,slice]:dat_array_doh[htmpView,1,2,slice],
                dat_array_doh[htmpView,2,1,slice]:dat_array_doh[htmpView,2,2,slice],
                dat_array_doh[htmpView,3,1,slice]:dat_array_doh[htmpView,3,2,slice],
                SxMtPop
              ]),sliceNum,sliceNum),
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
                
        # plot(matrix(c(rep(1,20),1:20),20,2),col=colorSeqMultPalette$YlOrBr(20),pch=15,cex=15, xlab = NA, ylab = NA, axes = F)
        # a <- 0.35; b <- 20.5; c <- (b-a)/10
        # axis(2, seq(a,b,c),c("","","","","","","","","","",""), line=0)
        # axis(2, c(4,17),c(range_list[1,1,ceiling(SxMtPop/4)],range_list[2,1,ceiling(SxMtPop/4)]), las=0,tck = 0, line = 0)
        # axis(4, c(1,10,19),c("min_val","mid_val","max_val"), las=1,tck = 0, lwd=0, line=0)
        # axis(4, c(17,18,19),c("min:","mid:","max:"), las=1,tck = 0, lwd=0, line=4)
        # if (absolute) {
        #   if ("Curiosity" %in% str_split(title_names[SxMtPop], " ")[[1]]
        #     ) {
        #       axis(4, c(17,18,19,20),c("0","0.5","1", "All:"), las=1,tck = 0, lwd=0, line=6)
        #     } else {
        #       axis(4, c(17,18,19,20),c("1","50.5","100", "All:"), las=1,tck = 0, lwd=0, line=6)
        #     }
          
        # } else {
        #   axis(4, c(17,18,19,20),c(heatmap_min[1],round((heatmap_min[1]+heatmap_max[1])/2,2),heatmap_max[1], "d2s"), las=1,tck = 0, lwd=0, line=6)
        #   axis(4, c(17,18,19,20),c(heatmap_min[2],round((heatmap_min[2]+heatmap_max[2])/2,2),heatmap_max[2], "d1s"), las=1,tck = 0, lwd=0, line=9)
        #   axis(4, c(17,18,19,20),c(heatmap_min[3],round((heatmap_min[3]+heatmap_max[3])/2,2),heatmap_max[3], "d12"), las=1,tck = 0, lwd=0, line=12)
        # }
        
        # mtext(c(paste0(legend_title[ceiling(SxMtPop/4)],"    ")),3,2.2,cex=1) # the fecking spaces are for keeping text center-aligned
        # mtext("Seeks Novel Songs",3,1,cex = 0.8)
        # mtext(range_list[1,2,ceiling(SxMtPop/4)],1,0.7,cex = 0.8)
        # box("outer", "solid")
        # #mtext(paste0(title_names[SxMtPop], "                                  "),3,cex = 1.5,line=30)
        # par(mfrow=c(1,1))
        # dev.off()
      }
    }
  }
  return(print("Done, in the specified folder"))
}

CombineSingles <- function (
  inheritanceStyle = 1,
  bias = 1,
  metricsSexPop = 1,
  curstartPattern = 1
) {

  library(magick)
  source("/home/parker/Documents/projects/curmodel_pcomp1/Code/curiosity-code/scripts/Source_Magick_Functions.R")
# Access the same subdirectory where the individual images are stored

  heatmap_sourceFolder <- file.path("results", "Heatmaps", "output_objects")

  SxMtPopContainer <- c("EndCurValP1M",
                        "EndCurValP2M",
                        "EndCurValP1F",
                        "EndCurValP2F",
                        "EndSRpValP1M",
                        "EndSRpValP2M",
                        "EndSRpValP1F",
                        "EndSRpValP2F")



  curstartPatternContainer <- c("narrowWide", "lowMedHigh")

  inheritanceContainer <- c("sameinh", "oppsinh", "maleinh", "mothinh")
  inheritanceStyle <- inheritanceContainer[inheritanceStyle]

  whichBias <- c("maleBias", "femaleBias")
  whichPopBias <- c("FemalePop", "MalePop")

  folderBias <- list.files(heatmap_sourceFolder)[which(sapply(list.files(heatmap_sourceFolder), function(x) (inheritanceStyle %in% str_split(x, "_")[[1]][4] && whichBias[bias] %in% str_split(x, "_")[[1]][5])))]

  PopBias <- whichPopBias[bias]
  
  singlesFolder <- file.path(heatmap_sourceFolder, folderBias, curstartPatternContainer[curstartPattern], PopBias)

  # if(!(dir.exists(singlesFolder))) {dir.create(singlesFolder)}

  if(
    curstartPattern == 1
  ) {
    image_1 <- image_read(file.path(singlesFolder, paste0(SxMtPopContainer[metricsSexPop], "_slice_1_", PopBias, ".png")))
    image_2 <- image_read(file.path(singlesFolder, paste0(SxMtPopContainer[metricsSexPop], "_slice_2_", PopBias, ".png")))
    # return(image_write(image_append(image_1, image_2)), path = file.path(heatmap_sourceFolder, folderBias, curstartPatternContainer[curstartPattern]))
    # thing <- image_append(image_1, image_2), path = file.path(heatmap_sourceFolder, folderBias, curstartPatternContainer[curstartPattern])
    thing <- image_append(c(image_1, image_2))
    # image_write(thing, path = file.path(heatmap_sourceFolder, folderBias, curstartPatternContainer[curstartPattern], paste0(SxMtPopContainer[metricsSexPop], ".png")))
  } else {
    image_1 <- image_read(file.path(singlesFolder, paste0(SxMtPopContainer[metricsSexPop], "_slice_1_", PopBias, ".png")))
    image_2 <- image_read(file.path(singlesFolder, paste0(SxMtPopContainer[metricsSexPop], "_slice_2_", PopBias, ".png")))
    image_3 <- image_read(file.path(singlesFolder, paste0(SxMtPopContainer[metricsSexPop], "_slice_3_", PopBias, ".png")))
    # return(image_write(mult_ImgAppend(image_1, image_2, image_3)), path = file.path(heatmap_sourceFolder, folderBias, curstartPatternContainer[curstartPattern]))
    thing <- mult_ImgAppend(image_1, image_2, image_3)
    # image_write(thing, path = file.path(heatmap_sourceFolder, folderBias, curstartPatternContainer[curstartPattern], paste0(SxMtPopContainer[metricsSexPop], ".png")))
  }
  return(thing)
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


# Triple 'for' loop, or triple 'sapply'?

# duh, triple 'sapply'

# sapply()

# CombineSingles(1,1,1,1)
# CombineSingles(1,1,2,1)
# CombineSingles(1,1,3,1)
# CombineSingles(1,1,4,1)
# CombineSingles(1,1,5,1)
# CombineSingles(1,1,6,1)
# CombineSingles(1,1,7,1)
# CombineSingles(1,1,8,1)
# CombineSingles(1,2,1,1)
# CombineSingles(1,2,2,1)
# CombineSingles(1,2,3,1)
# .
# .
# .
# CombineSingles(1,1,1,2)
















#   library(magick)
#   library(stringr)

#   regularNames <- c("EndCurValP1F",
#                     "EndCurValP1M",
#                     "EndCurValP2F",
#                     "EndCurValP2M",
#                     "EndSRpValP1F",
#                     "EndSRpValP1M",
#                     "EndSRpValP2F",
#                     "EndSRpValP2M"
#   )

#   SxMtPop_list <- c(
#     "Ending Curiosity Values - Pop 1 Females_slice_",
#     "Ending Curiosity Values - Pop 1 Males_slice_",
#     "Ending Curiosity Values - Pop 2 Females_slice_",
#     "Ending Curiosity Values - Pop 2 Males_slice_",
#     "Ending Sylrep Values - Pop 1 Females_slice_",
#     "Ending Sylrep Values - Pop 1 Males_slice_",
#     "Ending Sylrep Values - Pop 2 Females_slice_",
#     "Ending Sylrep Values - Pop 2 Males_slice_"
#   )

#   slice_names <- c(
#     "slice_1",
#     "slice_2",
#     "slice_3",
#     "slice_4",
#     "slice_5"
#   )

#   UpperDir <- file.path("results", "Heatmaps", "output_objects")

#   source("/home/parker/Documents/projects/curmodel_pcomp1/Code/curiosity-code/scripts/Source_Magick_Functions.R")

#     slice_1 <- image_read(file.path(UpperDir, heatmapSource_folderList[heatmap_num], 
#       slice_names[1], paste0(SxMtPop_list[metrics_num], "1.png")))
#     slice_2 <- image_read(file.path(UpperDir, heatmapSource_folderList[heatmap_num], 
#       slice_names[2], paste0(SxMtPop_list[metrics_num], "2.png")))
#     slice_3 <- image_read(file.path(UpperDir, heatmapSource_folderList[heatmap_num], 
#       slice_names[3], paste0(SxMtPop_list[metrics_num], "3.png")))
#     slice_4 <- image_read(file.path(UpperDir, heatmapSource_folderList[heatmap_num], 
#       slice_names[4], paste0(SxMtPop_list[metrics_num], "4.png")))
#     slice_5 <- image_read(file.path(UpperDir, heatmapSource_folderList[heatmap_num], 
#       slice_names[5], paste0(SxMtPop_list[metrics_num], "5.png")))
#   hashtagOutput <- mult_ImgAppend(slice_1, slice_2, slice_3, slice_4, slice_5)
#   image_write(hashtagOutput, path = file.path(UpperDir, heatmapSource_folderList[heatmap_num], str_split(SxMtPop_list[metrics_num], "_slice_")[[1]][1]))



#   # list.condition <- sapply(arguments, function(x) class(x)=="desired.class")
#   # output.list  <- input.list[list.condition]


#   for (
#     slice in 1:5
#   ) {
#     tempFigs[slice] <- image_read(file.path(
#       UpperDir, heatmap_folderList[heatmap_num], slice_names[slice], paste0(SxMtPop_list[metrics_num], slice, ".png")
#       )
#     )
#   }
#   hashtagOutput <- image_append(c(tempFigs[1], tempFigs[2], tempFigs[3], tempFigs[4], tempFigs[5]))
#   image_write(hashtagOutput, path = file.path(UpperDir, heatmap_folderList[heatmap_num]))




# }




















# source(file.path("scripts", "Source_AssignMultVar_BinaryMode.R"))

#   UpperDir <- file.path("results", "Heatmaps", "output_objects")

#   for (
#     slice in 1:5
#   ) {
#     c("temp1", "temp2", "temp3", "temp4", "temp5") %=% c(
#       image_read(file.path(UpperDir, heatmap_folderList[heatmap_num], ))
#     )
#   }

# }



# importMe <- file.path("home", "parker", "Downloads", "crowAndLorikeets1.jpg")
# image_read(importMe)

# importMe <- image_read(file.path("results", "Heatmaps", "output_objects", "190421_slices_-_sameinh_maleBias", "slice_1", "Ending Curiosity Values - Pop 1 Females.png"))

# image_write(CrLk5, path = "CrowAndLorikeetsFinal.png", format = "png")











# # Make max-and-min values in an object,
# # plot five of those objects for each slice set going 
# # through the 3d-array of heatmap data, along each dimension
# maxAndMinPlot <- function (

# ) {
  
#   # Read in the RDS file with the data array for the heatmaps

#   #   List folders that contain the .RData file (should be the only one in that dir contained in this list)

  # heatmapDB <- c(
  #   "190419_slices_-_oppsinh_femaleBias",
  #   "190419_slices_-_oppsinh_maleBias",
  #   "190421_slices_-_sameinh_femaleBias",
  #   "190421_slices_-_sameinh_maleBias"
  # )
#
  # tempHtMpArray <- readRDS(file.path("results", "Heatmaps", "output_objects", heatmapDB[1], list.files(file.path(file.path("results", "Heatmaps", "output_objects", heatmapDB[1])), pattern = ".RData")))



#   # opps = 2, 1 same = 4, 3 <-- fileOrder for making the figure!

#   maxNMinArray <- array(c(rep(0, 240)), 
#                       c(6,8,5), 
#                       list(
#                         # c("p2mVfem", "p1mVfem", "p1mVp2m", "p2fVmal", "p1fVmal", "p1fVp2f"), 
#                         c("p1m", "p2m", "fem", "p1f", "p2f", "mal"), 
#                         c("EC pop1fem", "EC pop1mal", "EC pop2fem", "EC pop2mal", "ES pop1fem", "ES pop1mal", "ES pop2fem", "ES pop2mal"), 
#                         c("slice 1", "slice 2", "slice 3", "slice 4", "slice 5")))
#   #   Read files from folderlist database
#   for (filechunk in 1:2) {
#     fileOrder <- c(2, 1)
#     tempHtMpArray <- readRDS(file.path("results", "Heatmaps", "output_objects", heatmapDB[fileOrder[filechunk]], list.files(file.path(file.path("results", "Heatmaps", "output_objects", heatmapDB[fileOrder[filechunk]])), pattern = ".RData")))

#     for (SxMtPop in 1:8) {
#       for (slice in 1:5) {
#         dat_array_doh <- array(c(
#           rep(c(slice, 1, 1, 1), 2),
#           slice, 
#           slice, 
#           rep(c(5, 5, 5, slice), 2)
#         ), c(3,3,2))
        
#         heatmapRangeDatasetOne <- tempHtMpArray[
#           dat_array_doh[1,1,1]:dat_array_doh[1,1,2],
#           dat_array_doh[1,2,1]:dat_array_doh[1,2,2],
#           dat_array_doh[1,3,1]:dat_array_doh[1,3,2],
#           SxMtPop]
#         heatmapRangeDatasetTwo <- tempHtMpArray[
#           dat_array_doh[2,1,1]:dat_array_doh[2,1,2],
#           dat_array_doh[2,2,1]:dat_array_doh[2,2,2],
#           dat_array_doh[2,3,1]:dat_array_doh[2,3,2],
#           SxMtPop]
#         heatmapRangeDatasetTre <- tempHtMpArray[
#           dat_array_doh[3,1,1]:dat_array_doh[3,1,2],
#           dat_array_doh[3,2,1]:dat_array_doh[3,2,2],
#           dat_array_doh[3,3,1]:dat_array_doh[3,3,2],
#           SxMtPop]
#         heatmap_min <- c(
#           round(min(heatmapRangeDatasetOne), 2),
#           round(min(heatmapRangeDatasetTwo), 2),
#           round(min(heatmapRangeDatasetTre), 2)
#         )
#         heatmap_max <- c(
#           round(max(heatmapRangeDatasetOne), 2),
#           round(max(heatmapRangeDatasetTwo), 2),
#           round(max(heatmapRangeDatasetTre), 2)
#         )

#         maxNMinArray[((1 + 3 * (filechunk - 1)):(3 + 3 * (filechunk - 1))), SxMtPop, slice] <- heatmap_max - heatmap_min

#       }
#     }

#   }
  
#   return (maxNMinArray)
# }
  
# plot <- maxAndMinPlot()
# ECplot <- plot[,1:4,]
# ESplot <- plot[,5:8,]

# colorSeqMultPalette <- list(
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
# ECorES <- c("EC", "ES") # split = variable name

# png(filename = paste0(ECorES[split],"_slice", file_name), width = 554, height = 467, units = "px", pointsize = 12, bg = "white")
# par(mfrow=c(2,5))

# for (split in 1:2) {
#   for (slice in 1:5) {
#     ECplot <- apply(ECplot[,,slice], 2, rev)
#     image(t(ECplot[,,slice]), col = colorSeqMultPalette$PuBuGn(100), 
#       ylab = c("p1m", "p2m", "fem", "p1f", "p2f", "mal"), 
#       xlab = c("EC pop1fem", "EC pop1mal", "EC pop2fem", "EC pop2mal"))
#   }
# }



# image(ECplot[,,1])
# image(t(ECplot[,,1]))

# # image(x, y, z, zlim, xlim, ylim, col = heat.colors(12),
# #       add = FALSE, xaxs = "i", yaxs = "i", xlab, ylab,
# #       breaks, oldstyle = FALSE, useRaster, …)

# # mat1 <- apply(mat1, 2, rev)
# # image(1:3, 1:3, t(mat1))

# # meanz <- cursitylist[[number_of_runs + 1]][10,population,]
# #     stuff <- paste0("points(cursitylist[[", 1:number_of_runs, "]][10,population,],col=\"grey\", cex=0.2)")
# #     file_name <- paste0(R$datez, "_", R$run_name, "_tutor_selections_pop", population, ".png")
# #     minY <- mins_n_maxes[2,population,1]
# #     maxY <- mins_n_maxes[2,population,2]
# #     png(filename = paste0(saving_dir, "/", file_name), width = 554, height = 467, units = "px", pointsize = 12, bg = "white")
# #     plot(meanz, xlab = "Timestep", ylab = paste0("Pop ", population, " Tutor Selection Chances"),cex=0.2, ylim=c(minY, maxY), xaxt="n")
# #     #axis(side = 1, at = c(which((1:P$num_timesteps)%%(P$num_timesteps/10)==0)), labels = which((1:P$num_timesteps)%%(P$num_timesteps/10)==0))
# #     axis(side = 1, at = c(seq.int(0,length(cursitylist[[number_of_runs + 1]][10,population,]),
# #                                   ((length(cursitylist[[number_of_runs + 1]][10,population,]))/10))), 
# #         labels = c(seq.int(0,num_timesteps,(num_timesteps/10))))
# #     eval(parse(text=stuff))
# #     lines(cursitylist[[number_of_runs + 1]][10,population,],col="black", cex=0.2)
# #     dev.off()