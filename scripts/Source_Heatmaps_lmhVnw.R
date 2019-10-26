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



  # "*_360[8-9]_|*_36[1-4][0-9]_|*_365[0-7]_") ### oct3n8results
  # "*_365[8-9]_|*_36[6-9][0-9]_|*_370[0-7]_") ### oct3n8results
  # "*_370[8-9]_|*_37[1-4][0-9]_|*_375[0-7]_") ### oct3n8results
  # "*_375[8-9]_|*_37[6-7][0-9]_|*_378[0-4]_") ### octXresults
  # "*_378[5-9]_|*_379[0-9]_|*_38[0-3][0-9]_|*_384[0-2]_") ### oct22stuff



heatmapland <- file.path("results", "oct3n8results", "results")
heatmapland <- file.path("results", "octXresults", "results")
heatmapland <- file.path("results", "oct22stuff", "results")
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

# heatmapland <- file.path("results", "190704_SmSxHB")
# heatmapland <- file.path("results", "190705_FfFfHB")

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
  
  # "10k") # Carefully curated directory, contains all necessary runs and nothing else
  # "191008") # Carefully curated directory, contains all necessary runs and nothing else

  "*_360[8-9]_|*_36[1-4][0-9]_|*_365[0-7]_") ### oct3n8results
  # "*_365[8-9]_|*_36[6-9][0-9]_|*_370[0-7]_") ### oct3n8results
  # "*_370[8-9]_|*_37[1-4][0-9]_|*_375[0-7]_") ### oct3n8results
  # "*_375[8-9]_|*_37[6-7][0-9]_|*_378[0-4]_") ### octXresults
  # "*_378[5-9]_|*_379[0-9]_|*_38[0-3][0-9]_|*_384[0-2]_") ### oct22stuff

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


all_the_low_background <- c(all_the_runs[1:25])
all_the_high_background <- c(all_the_runs[26:50])

extractedmeans <- extractmeans(allrundirs = all_the_low_background, 
                               dirheatmap = heatmapland, 
                               source_of_params = "params.yaml", 
                               deeper = FALSE)
all_the_names <- remakestring(all_the_low_background, "_", ".")

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

# whichrunstyle <- c("lowMedHigh", "narrowWide", "lowHigh")

# source(file.path("scripts", "Source_Heatmap_Functions.R"))

heatmapoutput <- list()
heatmapoutput <- makeheatmapfile(inheritance = 1, diffcurstartbias = 3, 
                biassize = 5, othersize = 1, 
                reversedruns = TRUE, specialfigs = TRUE, 
                runstyle = 3, highres = FALSE, 
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

# inheritancestyle = 5
# bias = 1
# metricssexpop = 1
# otherpopstyle = 3
# edit = TRUE
# lmhvnw = FALSE



# for (k in 1:2) {
#     thing <- c(3, 11)


#     for (i in 1:4) {
#         for (j in 1:2) {
#             combineeditsingles(thing[k],3,i,j, T)
#         }
#     }
# }






for (inhpattern in 1:4) {
  individualfigures(inhpattern, 2)

  for (spranges in 1:2) {
    stackmultiples(inhpattern, spranges)
  }
}

