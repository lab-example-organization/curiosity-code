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

heatmapLand <- HtMpDir()

# all_the_runs <- list.files(heatmapLand, 
all_the_runs <- extractVarDirs(heatmapLand, 
  #"_1[7-9][0-9]|2[0-9][0-9]|3[0-9][0-9]|4[0-1][0-9]_") # <- This was for the very first run - non-automated... more code to follow.
  #"190304_1[7-9][0-9]_|190304_2[0-8][0-9]_|190304_29[0-5]_")
  "*_1[7-9][0-9]_|*_2[0-8][0-9]_|*_29[0-5]_")                # maleinh maleBias
  #"*_2[9][6-9]_|*_3[0-9][0-9]_|*_4[0-1][0-9]_|*_420_")       # mothinh maleBias
  #"*_42[1-9]_|*_4[3-9][0-9]_|*_5[0-3][0-9]_|*_54[0-5]_")      # mothinh femBias
  #"*_54[6-9]_|*_5[7-9][0-9]_|*_6[0-6][0-9]_|*_670_")     # sameinh maleBias
  #"*_67[1-9]_|*_6[8-9][0-9]_|*_7[0-8][0-9]_|*_79[1-3]_"  # sameinh_femBias
  # "*_79[4-9]_|*_8[0-9][0-9]_|*_90[0-9]_|*_91[0-7]_"   # oppinh maleBias
  # "*_91[8-9]_|*_9[2-9][0-9]_|*_10[0-3][0-9]_|*_104[0-1]_"   # oppinh femBias
  # "*_104[2-9]_|*_10[5-9][0-9]_|*_11[0-5][0-9]_|*_116[0-6]_" maleinh femBias
#   connection <- file(description = file.path("source","temp", paste0(specificSimNumber, "_sim_data.txt")), open = "rt")
#   multiRun_folderList <- as.vector(read.table(connection, -1L)[[2]])
#   close(connection)




# stuff <- vector("character", length(all_the_runs))
# for(thing in length(all_the_runs)) {
# stuff[thing] <- str_sub(all_the_runs[thing], 8L, 10L)
# }



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
extractedMeans <- extractMeans(allRunDirs = all_the_runs, dirHeatMap = heatmapLand, source_of_params = "params.yaml")
all_the_names <- remakeString(all_the_runs, "_", ".")

names(extractedMeans) <- all_the_names


# heatmap_array <- array(0, dim = c(5,5,5,8), list(c("1-7mp1", "7-13mp1", "11-26mp1", "1-26mp1", "11-15mp1"), c("1-7mp2", "7-13mp2", "11-26mp2", "1-26mp2", "11-15mp2"), c("1-7f", "7-13f", "11-26f", "1-26f", "11-15f"), c("endcurm1","endcurm2","endcurf1","endcurf2","endrepm1","endrepm2","endrepf1","endrepf2")))
#heatmap_array <- array(0, dim = c(5,5,5,8), list(c("1-7mp1", "7-13mp1", "11-26mp1", "1-26mp1", "11-15mp1"), c("1-7mp2", "7-13mp2", "11-26mp2", "1-26mp2", "11-15mp2"), c("1-7f", "7-13f", "11-26f", "1-26f", "11-15f"), c("endcurm1","endcurm2","endcurf1","endcurf2","endrepm1","endrepm2","endrepf1","endrepf2")))

# DIFFERING MALE CURSTARTS

heatmap_array <- array(
  0, dim = c(5,5,5,8), list(
    c("1-7mp1", "7-13mp1", "11-26mp1", "1-26mp1", "11-15mp1"), 
    c("1-7mp2", "7-13mp2", "11-26mp2", "1-26mp2", "11-15mp2"), 
    c("1-7f", "7-13f", "11-26f", "1-26f", "11-15f"), 
    c("endcurm1","endcurm2","endcurf1","endcurf2","endrepm1","endrepm2","endrepf1","endrepf2")
  ))

for(femalez in 1:5) {
  for(malez1 in 1:5) {
    for(malez2 in 1:5) {

      tally <- malez2 + 5*(malez1 - 1) + 25*(femalez - 1)

      sumStats <- c(
        extractedMeans[[tally]]$curLvlMeans[1,1,100],
        extractedMeans[[tally]]$curLvlMeans[1,2,100],
        extractedMeans[[tally]]$curLvlMeans[2,1,100],
        extractedMeans[[tally]]$curLvlMeans[2,2,100],
        extractedMeans[[tally]]$sylRepMeans[1,1,100],
        extractedMeans[[tally]]$sylRepMeans[1,2,100],
        extractedMeans[[tally]]$sylRepMeans[2,1,100],
        extractedMeans[[tally]]$sylRepMeans[2,2,100]
      )

      heatmap_array[malez1,malez2,femalez,] <- sumStats
      
    }
  }
}


# DIFFERING FEMALE CURSTARTS

# heatmap_array <- array(
#   0, dim = c(5,5,5,8), list(
#     c("1-7fp1", "7-13fp1", "11-26fp1", "1-26fp1", "11-15fp1"), 
#     c("1-7fp2", "7-13fp2", "11-26fp2", "1-26fp2", "11-15fp2"), 
#     c("1-7m", "7-13m", "11-26m", "1-26m", "11-15m"), 
#     c("endcurm1","endcurm2","endcurf1","endcurf2","endrepm1","endrepm2","endrepf1","endrepf2")
#   ))

# for(malez in 1:5) {
#   for(femalez1 in 1:5) {
#     for(femalez2 in 1:5) {

#       tally <- femalez2 + 5*(femalez1 - 1) + 25*(malez - 1)

#       sumStats <- c(
#         extractedMeans[[tally]]$curLvlMeans[1,1,100],
#         extractedMeans[[tally]]$curLvlMeans[1,2,100],
#         extractedMeans[[tally]]$curLvlMeans[2,1,100],
#         extractedMeans[[tally]]$curLvlMeans[2,2,100],
#         extractedMeans[[tally]]$sylRepMeans[1,1,100],
#         extractedMeans[[tally]]$sylRepMeans[1,2,100],
#         extractedMeans[[tally]]$sylRepMeans[2,1,100],
#         extractedMeans[[tally]]$sylRepMeans[2,2,100]
#       )

#       heatmap_array[femalez1,femalez2,malez,] <- sumStats
      
#     }
#   }
# }

#image(x = matrix(as.numeric(heatmap_array[,,1,1]),5,5),col =colorSeqMultPalette$PuBuGn(100), xlab = "")

if(!(file.exists(file.path(
"results", "Heatmaps", "output_objects", "heatmap_output.RData"
      )))) {saveRDS(heatmap_array, file.path(
"results", "Heatmaps", "output_objects", "heatmap_output.RData"
   ))}
# heatmap_array <- readRDS("../../../../../../media/parker/A443-E926/simulation runs/heatmap_output.RData")
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

#image(x = matrix(as.numeric(heatmap_array[,,1,1]),5,5),col =colorSeqMultPalette$PuBuGn(100), xlab = "Pop 1 Male Curstart", ylab = "Pop 2 Male Curstart")

title_names <- c("Ending Curiosity Values - Pop 1 Males","Ending Curiosity Values - Pop 2 Males",
                 "Ending Curiosity Values - Pop 1 Females","Ending Curiosity Values - Pop 2 Females",
                 "Ending Syll Rept Values - Pop 1 Males","Ending Syll Rept Values - Pop 2 Males",
                 "Ending Syll Rept Values - Pop 1 Females","Ending Syll Rept Values - Pop 2 Females")
# heatmap_categories <- c("cat(\"[,,1,1]\")","cat(\"[,1,,1]\")","cat(\"[1,,,1]\")")



# MALE PATTERN INHERITANCE (BIAS) (MORE DIFFERING FEMALE CURSTARTS)

heatmap_axes <- list(
  plotOne = c("Pop 2 Male Starting Curiosity", "Female Starting Curiosity"),    # mp2Vfem
  plotTwo = c("Pop 1 Male Starting Curiosity", "Female Starting Curiosity"),    # mp1Vfem
  plotTre = c("Pop 1 Male Starting Curiosity", "Pop 2 Male Starting Curiosity") # mp1Vmp2
)


# FEMALE PATTERN INHERITANCE (BIAS) (MORE DIFFERING FEMALE CURSTARTS)

# heatmap_axes <- list(
#   mp2Vfem = c("Pop 2 Female Starting Curiosity", "Male Starting Curiosity"),
#   mp1Vfem = c("Pop 1 Female Starting Curiosity", "Male Starting Curiosity"),
#   mp1Vmp2 = c("Pop 1 Female Starting Curiosity", "Pop 2 Female Starting Curiosity")
# )

range_list <- array(data = c("Less Curiosity", "More Curiosity", "Seeks Similar Songs", "Seeks Novel Songs",
                             "Low SylRep", "High Sylrep", "Limited Song Variety", "Highly Varied Song"), c(2,2,2))

# making the layout matrix that will be populated by the figures. Named because they're arranged by column; one could conceivably arrange them by row as well.
layoutDistribution <- c(0,0,1,3,1,3,1,3,1,
                        3,1,3,1,3,1,3,1,3,
                        2,0,2,4,2,4,2,4,2,
                        5,2,5,2,5,2,5,0,0)
layoutSize <- length(layoutDistribution)
byTheCol <- vector("numeric", length = layoutSize*8)
for(i in 1:layoutSize) {
  byTheCol[(1 + (i - 1)*8):(i*8)] <- rep(layoutDistribution[i], 8)
}

for (slice in 1:5) {
  dat_array_doh <- array(c(rep(c(slice, 1, 1, 1), 2),slice, slice, rep(c(5, 5, 5, slice), 2)))

  legend_title <- c("Auditory Curiosity", "Syllable Repertoire")


  for(SxRpPop in 1:8) {

      # Start to make the file ########### still need to fix the name so they don't overwrite one another ############
    file_name <- paste0(title_names[SxRpPop], ".png")
      # dimensions? dunno; not too worried though
    png(filename = file.path(heatmapLand, "output_objects", file_name), width = 554, height = 554, units = "px", pointsize = 12, bg = "white")
    
  
      
    heatmap_min <- c(
      round(min(heatmap_array[dat_array_doh[1,1,1]:dat_array_doh[1,1,2],
                              dat_array_doh[1,2,1]:dat_array_doh[1,2,2],
                              dat_array_doh[1,3,1]:dat_array_doh[1,3,2],
                              SxRpPop]), 2),
      round(min(heatmap_array[dat_array_doh[2,1,1]:dat_array_doh[2,1,2],
                              dat_array_doh[2,2,1]:dat_array_doh[2,2,2],
                              dat_array_doh[2,3,1]:dat_array_doh[2,3,2],
                              SxRpPop]), 2),
      round(min(heatmap_array[dat_array_doh[3,1,1]:dat_array_doh[3,1,2],
                              dat_array_doh[3,2,1]:dat_array_doh[3,2,2],
                              dat_array_doh[3,3,1]:dat_array_doh[3,3,2],
                              SxRpPop]), 2)
    )
    
    heatmap_max <- c(
      round(max(heatmap_array[dat_array_doh[1,1,1]:dat_array_doh[1,1,2],
                              dat_array_doh[1,2,1]:dat_array_doh[1,2,2],
                              dat_array_doh[1,3,1]:dat_array_doh[1,3,2],
                              SxRpPop]), 2),
    round(max(heatmap_array[dat_array_doh[2,1,1]:dat_array_doh[2,1,2],
                              dat_array_doh[2,2,1]:dat_array_doh[2,2,2],
                              dat_array_doh[2,3,1]:dat_array_doh[2,3,2],
                              SxRpPop]), 2),
      round(max(heatmap_array[dat_array_doh[3,1,1]:dat_array_doh[3,1,2],
                              dat_array_doh[3,2,1]:dat_array_doh[3,2,2],
                              dat_array_doh[3,3,1]:dat_array_doh[3,3,2],
                              SxRpPop]), 2)
    )
    
    layout(matrix(byTheCol,16,18,F))
    
  # The Fake one!

    # plotNames <- array(c("heatmap_axes$plotOne[1]", "heatmap_axes$plotTwo[1]", "heatmap_axes$plotTre[1]", "heatmap_axes$plotOne[2]", "heatmap_axes$plotTwo[2]", "heatmap_axes$plotTre[2]")

    image(x = matrix(as.numeric(heatmap_array[dat_array_doh[balls,1,1]:dat_array_doh[balls,1,2],
                                              dat_array_doh[balls,2,1]:dat_array_doh[balls,2,2],
                                              dat_array_doh[balls,3,1]:dat_array_doh[balls,3,2],
                                              SxRpPop]),5,5),
          col = colorSeqMultPalette$YlOrBr(100),
          axes = F, 
          xlab = heatmap_axes$plotOne[1], 
          ylab = heatmap_axes$plotOne[2],cex.lab=1.4, zlim = c(heatmap_min[1]-0.01,heatmap_max[1]+0.01))
    
    axis(1,c(-0.125,0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1,1.12),
        c("","0-.25","", ".25-.5","", ".45-1","", "0-1","", ".45-.55",""),
        T,0,NA,F,cex.axis=0.8, tck = 0)
    axis(1,c(-0.125,0.125,0.375,0.625,0.875,1.125),
        c("","","","","",""),
        T,-0.03,NA,F,cex.axis=1, tck = -0.03)
    
    axis(2,c(-0.125,0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1,1.12),
        c("","0-.25","", ".25-.5","", ".45-1","", "0-1","", ".45-.55",""),
        T,0,NA,F,cex.axis=0.6, tck = 0)
    axis(2,c(-0.125,0.125,0.375,0.625,0.875,1.125),
        c("","","","","",""),
        T,-0.03,NA,F,cex.axis=1, tck = -0.03)

  # the first real one!

    image(x = matrix(as.numeric(heatmap_array[dat_array_doh[1,1,1]:dat_array_doh[1,1,2],
                                              dat_array_doh[1,2,1]:dat_array_doh[1,2,2],
                                              dat_array_doh[1,3,1]:dat_array_doh[1,3,2],
                                              SxRpPop]),5,5),
          col = colorSeqMultPalette$YlOrBr(100),
          axes = F, 
          xlab = heatmap_axes$plotOne[1], 
          ylab = heatmap_axes$plotOne[2],cex.lab=1.4, zlim = c(heatmap_min[1]-0.01,heatmap_max[1]+0.01))
    
    axis(1,c(-0.125,0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1,1.12),
        c("","0-.25","", ".25-.5","", ".45-1","", "0-1","", ".45-.55",""),
        T,0,NA,F,cex.axis=0.8, tck = 0)
    axis(1,c(-0.125,0.125,0.375,0.625,0.875,1.125),
        c("","","","","",""),
        T,-0.03,NA,F,cex.axis=1, tck = -0.03)
    
    axis(2,c(-0.125,0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1,1.12),
        c("","0-.25","", ".25-.5","", ".45-1","", "0-1","", ".45-.55",""),
        T,0,NA,F,cex.axis=0.6, tck = 0)
    axis(2,c(-0.125,0.125,0.375,0.625,0.875,1.125),
        c("","","","","",""),
        T,-0.03,NA,F,cex.axis=1, tck = -0.03)
    
    image(x = matrix(as.numeric(heatmap_array[dat_array_doh[2,1,1]:dat_array_doh[2,1,2],
                                              dat_array_doh[2,2,1]:dat_array_doh[2,2,2],
                                              dat_array_doh[2,3,1]:dat_array_doh[2,3,2],
                                              SxRpPop]),5,5),
          col = colorSeqMultPalette$YlOrBr(100),
          axes = F, 
          xlab = heatmap_axes$plotTwo[1], 
          ylab = heatmap_axes$plotTwo[2],cex.lab=1.4, zlim = c(heatmap_min[2]-0.01,heatmap_max[2]+0.01))
    
    axis(1,c(-0.125,0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1,1.12),
        c("","0-.25","", ".25-.5","", ".45-1","", "0-1","", ".45-.55",""),
        T,0,NA,F,cex.axis=0.8, tck = 0)
    axis(1,c(-0.125,0.125,0.375,0.625,0.875,1.125),
        c("","","","","",""),
        T,-0.03,NA,F,cex.axis=1, tck = -0.03)
    
    axis(2,c(-0.125,0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1,1.12),
        c("","0-.25","", ".25-.5","", ".45-1","", "0-1","", ".45-.55",""),
        T,0,NA,F,cex.axis=0.6, tck = 0)
    axis(2,c(-0.125,0.125,0.375,0.625,0.875,1.125),
        c("","","","","",""),
        T,-0.03,NA,F,cex.axis=1, tck = -0.03)
    
    image(x = matrix(as.numeric(heatmap_array[dat_array_doh[3,1,1]:dat_array_doh[3,1,2],
                                              dat_array_doh[3,2,1]:dat_array_doh[3,2,2],
                                              dat_array_doh[3,3,1]:dat_array_doh[3,3,2],
                                              SxRpPop]),5,5),
          col = colorSeqMultPalette$YlOrBr(100),
          axes = F, 
          xlab = heatmap_axes$plotTre[1], 
          ylab = heatmap_axes$plotTre[2],cex.lab=1.4, zlim = c(heatmap_min[3]-0.01,heatmap_max[3]+0.01))
    
    axis(1,c(-0.125,0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1,1.12),
        c("","0-.25","", ".25-.5","", ".45-1","", "0-1","", ".45-.55",""),
        T,0,NA,F,cex.axis=0.8, tck = 0)
    axis(1,c(-0.125,0.125,0.375,0.625,0.875,1.125),
        c("","","","","",""),
        T,-0.03,NA,F,cex.axis=1, tck = -0.03)
    
    axis(2,c(-0.125,0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1,1.12),
        c("","0-.25","", ".25-.5","", ".45-1","", "0-1","", ".45-.55",""),
        T,0,NA,F,cex.axis=0.6, tck = 0)
    axis(2,c(-0.125,0.125,0.375,0.625,0.875,1.125),
        c("","","","","",""),
        T,-0.03,NA,F,cex.axis=1, tck = -0.03)
    
    #file_name <- paste0(title_names[SxRpPop], ".tiff")
    # dimensions? dunno; not too worried though
    #tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
    
    #            layout(mat = matrix(practice,20,16))
    #            practice <- c(rep(0,190),rep(1,10),rep(0,10),rep(1,10),rep(0,100))
    
    
    plot(matrix(c(rep(1,20),1:20),20,2),col=colorSeqMultPalette$YlOrBr(20),pch=15,cex=15, xlab = NA, ylab = NA, axes = F)
    a <- 0.35; b <- 20.5; c <- (b-a)/10
    axis(2, seq(a,b,c),c("","","","","","","","","","",""), line=0)
    axis(2, c(4,17),c(range_list[1,1,ceiling(SxRpPop/4)],range_list[2,1,ceiling(SxRpPop/4)]), las=0,tck = 0, line = 0)
    axis(4, c(1,10,19),c("min_val","mid_val","max_val"), las=1,tck = 0, lwd=0, line=0)
    axis(4, c(17,18,19),c("min:","mid:","max:"), las=1,tck = 0, lwd=0, line=4)
    axis(4, c(17,18,19,20),c(heatmap_min[1],round((heatmap_min[1]+heatmap_max[1])/2,2),heatmap_max[1], "M2F"), las=1,tck = 0, lwd=0, line=6)
    axis(4, c(17,18,19,20),c(heatmap_min[2],round((heatmap_min[2]+heatmap_max[2])/2,2),heatmap_max[2], "M1F"), las=1,tck = 0, lwd=0, line=9)
    axis(4, c(17,18,19,20),c(heatmap_min[3],round((heatmap_min[3]+heatmap_max[3])/2,2),heatmap_max[3], "1v2"), las=1,tck = 0, lwd=0, line=12)
    mtext(c(paste0(legend_title[ceiling(SxRpPop/4)],"    ")),3,2.2,cex=1) # the fecking spaces are for keeping text center-aligned
    mtext("Seeks Novel Songs",3,1,cex = 0.8)
    mtext(range_list[1,2,ceiling(SxRpPop/4)],1,0.7,cex = 0.8)
    box("outer", "solid")
    mtext(paste0(title_names[SxRpPop], "                                  "),3,cex = 1.5,line=30)
    par(mfrow=c(1,1))
    dev.off()
  }
}

# dat_array_doh <- array(c(rep(1,9),1,5,5,5,1,5,5,5,1),c(3,3,2))
# dat_array_doh <- array(c(2,1,1,1,2,1,1,1,2,2,5,5,5,2,5,5,5,2),c(3,3,2))
# dat_array_doh <- array(c(3,1,1,1,3,1,1,1,3,3,5,5,5,3,5,5,5,3),c(3,3,2))
# dat_array_doh <- array(c(4,1,1,1,4,1,1,1,4,4,5,5,5,4,5,5,5,4),c(3,3,2))
# dat_array_doh <- array(c(5,1,1,1,5,1,1,1,5,rep(5,9)),c(3,3,2))


