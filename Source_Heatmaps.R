setwd("~/Documents/projects/Code/Results/")
if(!(dir.exists("Heatmaps"))) {dir.create("Heatmaps")}
setwd("Heatmaps")
heatmapLand <- getwd()
setwd("../../../../../../../../../../media/parker/A443-E926/simulation runs/shifting_curstart_for_heatmap/")
heatmap_runs <- list.files()
diffMale_start <- heatmap_runs[grep("[0-9]mp[0-9]", heatmap_runs)]
sameMale_start <- heatmap_runs[grep("[0-9]m_", heatmap_runs)]
allSame_start <- heatmap_runs[grep("[0-9]f", heatmap_runs, invert = T)]

ending_curiosity_mp1 <- array(0, c(5,5,5), list(c("1-7mp1", "7-13mp1", "11-26mp1", "1-26mp1", "11-15mp1"), c("1-7mp2", "7-13mp2", "11-26mp2", "1-26mp2", "11-15mp2"), c("1-7Fem", "7-13Fem", "11-26Fem", "1-26Fem", "11-15Fem")))
ending_curiosity_mp2 <- array(0, c(5,5,5), list(c("1-7mp1", "7-13mp1", "11-26mp1", "1-26mp1", "11-15mp1"), c("1-7mp2", "7-13mp2", "11-26mp2", "1-26mp2", "11-15mp2"), c("1-7Fem", "7-13Fem", "11-26Fem", "1-26Fem", "11-15Fem")))
ending_curiosity_fem <- array(0, c(5,5,5), list(c("1-7mp1", "7-13mp1", "11-26mp1", "1-26mp1", "11-15mp1"), c("1-7mp2", "7-13mp2", "11-26mp2", "1-26mp2", "11-15mp2"), c("1-7Fem", "7-13Fem", "11-26Fem", "1-26Fem", "11-15Fem")))


setwd(paste0("/multirun_output/2019-02-02-232614-GMT-multirun-output/"))


multRunParamsInfo <- readLines("Multirun - Parameters and Info",25)

#"Multirun - Parameters and Info"



ending_curiosity_mp1 <- array(0, c(5,5,5), list(c("1-7mp1", "7-13mp1", "11-26mp1", "1-26mp1", "11-15mp1"), c("1-7mp2", "7-13mp2", "11-26mp2", "1-26mp2", "11-15mp2"), c("1-7Fem", "7-13Fem", "11-26Fem", "1-26Fem", "11-15Fem")))