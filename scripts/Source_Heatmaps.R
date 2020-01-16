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

heatmapLand <- file.path("results")

# heatmapLand <- file.path("results", "Heatmaps")

# all_the_runs <- list.files(heatmapLand,

filename_range <- print_regex_num_range(
# 3901-4100
  #3901-3950 ### mal inh
  #3951-4000 ### fem inh
  #4001-4050 ### sam inh
  #4051-4100 ### mix inh
)

all_the_runs <- extractVarDirs(heatmapLand, filename_range)




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
  thisBias = 1 # 3 or 4
) {

  # heatmap_sourceFolder <- file.path("results", "Heatmaps", "output_objects")
  heatmap_sourceFolder <- file.path("results")
  # heatmap_sourceFolder <- file.path("sameSexFigResults", "results")

  ClrRngContainer <- c("relative", "absolute")

  inheritanceContainer <- c("maleinh", "mothinh", "sameinh", "oppsinh",
                            "sNTninh", "sSTfinh", "sSFrinh", "sFrSinh",
                            "sTfSinh", "sTnNinh", "FfFfinh")

  whichBias <- c("maleBias", "femaleBias", "pop1Bias", "pop2Bias", "bothBias")


  colorRange <- ClrRngContainer[colorRange]

  inheritance <- inheritanceContainer[inheritance]

  thisBias <- whichBias[thisBias]

  folderName <- list.files(heatmap_sourceFolder)[which(sapply(list.files(heatmap_sourceFolder), function(x) (inheritance %in% str_split(x, "_")[[1]][4] && thisBias %in% str_split(x, "_")[[1]][5])))]

  # tempHtMpArray <- readRDS(file.path(heatmap_sourceFolder, folderName, list.files(file.path(heatmap_sourceFolder, folderName), pattern = ".RData")))
  HtMpArrays <- list.files(file.path(heatmap_sourceFolder, folderName), pattern = ".RData")

  # REDUCE THE SHIT.



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
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
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
  } else if (thisBias == 5) {
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
  # }
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
        # dat_array_doh <- array(c(
        #   rep(c(slice, 1, 1, 1), 2),
        #   slice,
        #   slice,
        #   rep(c(5, 5, 5, slice), 2)
        # ), c(3,3,2))

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