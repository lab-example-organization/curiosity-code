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
heatmapLand <- file.path("results", "Heatmaps", "maleInh_maleBias")
# heatmapLand <- file.path("results", "Heatmaps")

# all_the_runs <- list.files(heatmapLand, 
all_the_runs <- extractVarDirs(heatmapLand, 
  #"_1[7-9][0-9]|2[0-9][0-9]|3[0-9][0-9]|4[0-1][0-9]_") # <- This was for the very first run - non-automated... more code to follow.
  #"190304_1[7-9][0-9]_|190304_2[0-8][0-9]_|190304_29[0-5]_")
  "*_1[7-9][0-9]_|*_2[0-8][0-9]_|*_29[0-5]_")                # maleinh maleBias
  #"*_2[9][6-9]_|*_3[0-9][0-9]_|*_4[0-1][0-9]_|*_420_")       # mothinh maleBias
  #"*_42[1-9]_|*_4[3-9][0-9]_|*_5[0-3][0-9]_|*_54[0-5]_")      # mothinh femBias
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


# norm1 <- all_the_runs[1:12]  #421-432
# norm2 <- all_the_runs[13:37] #434-458
# norm3 <- all_the_runs[38:52] #460-474
# norm4 <- all_the_runs[53:67] #476-490
# norm5<- all_the_runs[68:82] #492-506
# norm6 <- all_the_runs[83:91] #508-516
# norm7 <- all_the_runs[95:123]#517-545
# 92 - 475; 93 - 491; 94 - 507; 124 - 433; 125 - 459
#all_the_runs <- c(norm1, all_the_runs[124], norm2, all_the_runs[125], norm3, all_the_runs[92], norm4, all_the_runs[93], norm5, all_the_runs[94], norm6, norm7)
# 


profvis({
#   for(iteration in 1:10) {
    extractedMeans <- extractMeans(allRunDirs = all_the_runs, 
        dirHeatMap = heatmapLand, source_of_params = "params.yaml")
#   }
})





# extractedMeans <- extractMeans(allRunDirs = all_the_runs, dirHeatMap = heatmapLand, source_of_params = "params.yaml")
extractedMeans <- extractMeans(allRunDirs = all_the_runs, dirHeatMap = heatmapLand, source_of_params = "params.yaml", deeper = TRUE)

all_the_names <- remakeString(all_the_runs, "_", ".")

names(extractedMeans) <- all_the_names


# heatmapLand

# makeHeatmaps <- function (
#   inheritance = 1,
#   diffcurstartBias = 1
# )

# whichInh <- c("male","moth","same","opps","sNTn","sSTf","sSFr","sFrS","sTfS","sTnN")

# whichBias <- c("male","female")

makeHeatmaps(inheritance = 1, diffcurstartBias = 1, absolute = TRUE)



library(magick)





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

#     for (SxRpPop in 1:8) {
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
#           SxRpPop]
#         heatmapRangeDatasetTwo <- tempHtMpArray[
#           dat_array_doh[2,1,1]:dat_array_doh[2,1,2],
#           dat_array_doh[2,2,1]:dat_array_doh[2,2,2],
#           dat_array_doh[2,3,1]:dat_array_doh[2,3,2],
#           SxRpPop]
#         heatmapRangeDatasetTre <- tempHtMpArray[
#           dat_array_doh[3,1,1]:dat_array_doh[3,1,2],
#           dat_array_doh[3,2,1]:dat_array_doh[3,2,2],
#           dat_array_doh[3,3,1]:dat_array_doh[3,3,2],
#           SxRpPop]
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

#         maxNMinArray[((1 + 3 * (filechunk - 1)):(3 + 3 * (filechunk - 1))), SxRpPop, slice] <- heatmap_max - heatmap_min

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