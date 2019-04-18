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

heatmapLand <- HtMpDir(c("sameInh"))

# all_the_runs <- list.files(heatmapLand, 
all_the_runs <- extractVarDirs(heatmapLand, 
  #"_1[7-9][0-9]|2[0-9][0-9]|3[0-9][0-9]|4[0-1][0-9]_") # <- This was for the very first run - non-automated... more code to follow.
  #"190304_1[7-9][0-9]_|190304_2[0-8][0-9]_|190304_29[0-5]_")
  # "*_1[7-9][0-9]_|*_2[0-8][0-9]_|*_29[0-5]_")                # maleinh maleBias
  #"*_2[9][6-9]_|*_3[0-9][0-9]_|*_4[0-1][0-9]_|*_420_")       # mothinh maleBias
  #"*_42[1-9]_|*_4[3-9][0-9]_|*_5[0-3][0-9]_|*_54[0-5]_")      # mothinh femBias
  "*_54[6-9]_|*_5[7-9][0-9]_|*_6[0-6][0-9]_|*_670_")     # sameinh femaleBias
  #"*_67[1-9]_|*_6[8-9][0-9]_|*_7[0-8][0-9]_|*_79[1-4]_")  # sameinh_maleBias
  # "*_79[4-9]_|*_8[0-9][0-9]_|*_90[0-9]_|*_91[0-7]_")   # oppinh maleBias
  # "*_91[8-9]_|*_9[2-9][0-9]_|*_10[0-3][0-9]_|*_104[0-1]_")   # oppinh femBias
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


# heatmapLand

# makeHeatmaps <- function (
#   inheritance = 1,
#   diffcurstartBias = 1
# )

# whichInh <- c("male","moth","same","opps","sNTn","sSTf","sSFr","sFrS","sTfS","sTnN")

# whichBias <- c("male","female")

makeHeatmaps(3, 1)
