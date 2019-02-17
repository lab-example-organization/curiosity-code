#setwd("~/Documents/projects/Code/Results/")
setwd("~/Downloads/Lab_Notebook/GitHub/Code/Results")
if(!(dir.exists("Heatmaps"))) {dir.create("Heatmaps")}
setwd("Heatmaps")
heatmapLand <- getwd()
#setwd("../../../../../../../../../../media/parker/A443-E926/simulation runs/shifting_curstart_for_heatmap/")
setwd("/Volumes/NO NAME/simulation runs/shifting_curstart_for_heatmap/")
heatmap_runs <- list.files()
diffMale_start <- heatmap_runs[grep("[0-9]mp[0-9]", heatmap_runs)]
  # 
  diffMale_start[c(1,2,3,4,5,6,7,31,46,9,10,11,12,47,13,14,15,48,16,17,18,19,49,20,21,22,23,24,25,26,27,28,29,32,33,34,35,36,37,30,50,38,39,40,41,42,)]
sameMale_start <- heatmap_runs[grep("[0-9]m_", heatmap_runs)]
  # sameMale_start <- sameMale_start[18,1,2,16,3,5,4,6,14,19,15,20,10,11,13,12,7,9,17,8]
allSame_start <- heatmap_runs[grep("[0-9]f", heatmap_runs, invert = T)]
  # allSame_start <- allSame_start[2,4,3,1,5]

ending_curiosity_mp1 <- array(0, c(5,5,5), list(c("1-7mp1", "7-13mp1", "11-26mp1", "1-26mp1", "11-15mp1"), c("1-7mp2", "7-13mp2", "11-26mp2", "1-26mp2", "11-15mp2"), c("1-7f", "7-13f", "11-26f", "1-26f", "11-15f")))
  # sumStats[8]
ending_curiosity_mp2 <- array(0, c(5,5,5), list(c("1-7mp1", "7-13mp1", "11-26mp1", "1-26mp1", "11-15mp1"), c("1-7mp2", "7-13mp2", "11-26mp2", "1-26mp2", "11-15mp2"), c("1-7f", "7-13f", "11-26f", "1-26f", "11-15f")))
  # sumStats[24]
ending_curiosity_f1 <- array(0, c(5,5,5), list(c("1-7mp1", "7-13mp1", "11-26mp1", "1-26mp1", "11-15mp1"), c("1-7mp2", "7-13mp2", "11-26mp2", "1-26mp2", "11-15mp2"), c("1-7f", "7-13f", "11-26f", "1-26f", "11-15f")))
  # sumStats[16]
ending_curiosity_f2 <- array(0, c(5,5,5), list(c("1-7mp1", "7-13mp1", "11-26mp1", "1-26mp1", "11-15mp1"), c("1-7mp2", "7-13mp2", "11-26mp2", "1-26mp2", "11-15mp2"), c("1-7f", "7-13f", "11-26f", "1-26f", "11-15f")))
  # sumStats[32]
ending_reperSize_mp1 <- array(0, c(5,5,5), list(c("1-7mp1", "7-13mp1", "11-26mp1", "1-26mp1", "11-15mp1"), c("1-7mp2", "7-13mp2", "11-26mp2", "1-26mp2", "11-15mp2"), c("1-7f", "7-13f", "11-26f", "1-26f", "11-15f")))
  # sumStats[4]
ending_reperSize_mp2 <- array(0, c(5,5,5), list(c("1-7mp1", "7-13mp1", "11-26mp1", "1-26mp1", "11-15mp1"), c("1-7mp2", "7-13mp2", "11-26mp2", "1-26mp2", "11-15mp2"), c("1-7f", "7-13f", "11-26f", "1-26f", "11-15f")))
  # sumStats[20]
ending_reperSize_f1 <- array(0, c(5,5,5), list(c("1-7mp1", "7-13mp1", "11-26mp1", "1-26mp1", "11-15mp1"), c("1-7mp2", "7-13mp2", "11-26mp2", "1-26mp2", "11-15mp2"), c("1-7f", "7-13f", "11-26f", "1-26f", "11-15f")))
  # sumStats[12]
ending_reperSize_f2 <- array(0, c(5,5,5), list(c("1-7mp1", "7-13mp1", "11-26mp1", "1-26mp1", "11-15mp1"), c("1-7mp2", "7-13mp2", "11-26mp2", "1-26mp2", "11-15mp2"), c("1-7f", "7-13f", "11-26f", "1-26f", "11-15f")))
  # sumStats[28]


range_mp1 <- c("1-7mp1", "7-13mp1", "11-26mp1", "1-26mp1", "11-15mp1")
range_mp2 <- c("1-7mp2", "7-13mp2", "11-26mp2", "1-26mp2", "11-15mp2")
range_fem <- c("1-7f", "7-13f", "11-26f", "1-26f", "11-15f")

union()

for(x in 1:5) {
  for(y in 1:5) {
    for(z in 1:5) {
      setwd(paste0("/multirun_output/*/"))
      #ending_curiosity_fem[x,y,z] <- 
      sumStats <- readLines(list.files()[grep("Summary_Statistics", list.files())])
    }
  }
}




multRunParamsInfo <- readLines("Multirun - Parameters and Info",25)

#"Multirun - Parameters and Info"



ending_curiosity_mp1 <- array(0, c(5,5,5), list(c("1-7mp1", "7-13mp1", "11-26mp1", "1-26mp1", "11-15mp1"), c("1-7mp2", "7-13mp2", "11-26mp2", "1-26mp2", "11-15mp2"), c("1-7Fem", "7-13Fem", "11-26Fem", "1-26Fem", "11-15Fem")))