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
heatmapland <- file.path("results")



  # "*_360[8-9]_|*_36[1-4][0-9]_|*_365[0-7]_") ### oct3n8results
  # "*_365[8-9]_|*_36[6-9][0-9]_|*_370[0-7]_") ### oct3n8results
  # "*_370[8-9]_|*_37[1-4][0-9]_|*_375[0-7]_") ### oct3n8results
  # "*_375[8-9]_|*_37[6-7][0-9]_|*_378[0-4]_") ### octXresults
  # "*_378[5-9]_|*_379[0-9]_|*_38[0-3][0-9]_|*_384[0-2]_") ### oct22stuff



# heatmapland <- file.path("results", "oct3n8results", "results")
# heatmapland <- file.path("results", "octXresults", "results")
# heatmapland <- file.path("results", "oct22stuff", "results")
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

  # "*_358[1-9]_") ### fathinh, 200k setup
  # "*_359[0-8]_") ### sameinh, 200k setup
  # "*_3599_|*_360[0-7]_") ### mixedinh55, 200k setup
  # "*_378[5-9]_|*_379[0-3]_") ### mothinh, 200k setup

  # "*_360[8-9]_|*_36[1-4][0-9]_|*_365[0-7]_") ### oct3n8results ### Father Inheritance 5x5x2 lowHigh Background
  # "*_379[4-9]_|*_38[0-3][0-9]_|*_384[0-3]_") ### octXresults ### Mother Inheritance 5x5x2 lowHigh Background
  # "*_365[8-9]_|*_36[6-9][0-9]_|*_370[0-7]_") ### oct3n8results ### SameSex Inheritance 5x5x2 lowHigh Background
  # "*_370[8-9]_|*_37[1-4][0-9]_|*_375[0-7]_") ### oct3n8results ### Mixed55 Inheritance 5x5x2 lowHigh Background
  # "*_375[8-9]_|*_37[6-7][0-9]_|*_378[0-4]_") ### octXresults


  # 1, 2, 6, 7 - High Background
  # 3, 8, 9, 3889-3892 - Low Background

  ### 200k vanilla:
    ### Father Inheritance: 3581-3589, 3889
      # "*_358[3, 8, 9]_|*_3889_") ### Low Background
      # "*_358[1, 2, 6, 7]_") ### High Background
    ### Mother Inheritance: 3785-3793, 3890
      # "*_3787_|*_379[2, 3]_|*_3890_") ### Low Background
      # "*_378[5, 6]_|*_379[0, 1]_") ### High Background
    ### SameSex Inheritance: 3590-3598, 3891
      # "*_359[2, 7, 8]_|*_3891_") ### Low Background
      # "*_359[0, 1, 5, 6]_") ### High Background
    ### Mixed55 Inheritance: 3599-3607, 3892
      # "*_360[1, 6, 7]_|*_3892_") ### Low Background
      # "*_3599_|*_360[0, 4, 5]_") ### High Background
  ### 200k post-inv:
    ### Father Inheritance: 3758-3766, 3893
      # "*_376[0, 5, 6]_|*_3893_") ### Low Background
      # "*_375[8, 9]_|*_376[3, 4]_") ### High Background
    ### Mother Inheritance: 3844-3852, 3894
      # "*_3846_|*_385[1, 2]_|*_3894_") ### Low Background
      # "*_384[4, 5, 9]_|*_3850_") ### High Background
    ### SameSex Inheritance: 3767-3775, 3895
      # "*_3769_|*_377[4, 5]_|*_3895_") ### Low Background
      # "*_376[7, 8]_|*_377[2, 3]_") ### High Background
    ### Mixed55 Inheritance: 3776-3784, 3896
      # "*_3778_|*_378[3, 4]_|*_3896_") ### Low Background
      # "*_377[6, 7]_|*_378[1, 2]_") ### High Background


  ### Inv 3k; 10k total, not 200k:
    ### Father Inheritance: 3853-3861, 3897
      # "*_3855_|*_386[0,1]_|*_3897_") ### Low Background
      # "*_385[3, 4, 8, 9]_") ### High Background
    ### Mother Inheritance: 3862-3870, 3898
      # "*_386[4, 9]_|*_3870_|*_3898_") ### Low Background
      # "*_386[2, 3, 7, 8]_") ### High Background
    ### SameSex Inheritance: 3871-3879, 3899
      # "*_387[3, 8, 9]_|*_3899_") ### Low Background
      # "*_387[1, 2, 6, 7]_") ### High Background
    ### Mixed55 Inheritance: 3880-3888, 3900
      # "*_388[2, 7, 8]_|*_3900_") ### Low Background
      # "*_388[0, 1, 5, 6]_") ### High Background

  ### No invasion, 10k highLow (biassize 5, othersize 2)
    ### Father Inheritance: 3901-3950
      # "*_390[1-9]_|*_39[1-4][0-9]_|*_3950_") ###
    ### Mother Inheritance: 3951-4000
      # "*_395[1-9]_|*_39[6-9][0-9]_|*_4000_") ###
    ### SamSex Inheritance: 4001-4050
      # "*_400[1-9]_|*_40[1-4][0-9]_|*_4050_") ###
    ## Miixed Inheritance: 4051-4100
      # "*_405[1-9]_|*_40[6-9][0-9]_|*_4100_") ###

  ### No invasion, 10k highLow follow-up (biassize 5, othersize 2)
    ### Father Inheritance: 4101-4150
      # "*_410[1-9]_|*_41[1-4][0-9]_|*_4150_") ###
    ### Mother Inheritance: 4151-4200
      # "*_415[1-9]_|*_41[6-9][0-9]_|*_4200_") ###
    ### SamSex Inheritance: 4201-4250
      # "*_420[1-9]_|*_42[1-4][0-9]_|*_4250_") ###
    ### Miixed Inheritance: 4251-4300
      # "*_425[1-9]_|*_42[6-9][0-9]_|*_4300_") ###

  ### Invasion! 10k highLow follow-up HIGH (biassize 5, othersize 2)
    ### Father Inheritance: 4301-4350
      # "*_430[1-9]_|*_43[1-4][0-9]_|*_4350_") ###
    ### Mother Inheritance: 4351-4400
      # "*_435[1-9]_|*_43[6-9][0-9]_|*_4400_") ###
    ### SamSex Inheritance: 4401-4450
      # "*_440[1-9]_|*_44[1-4][0-9]_|*_4450_") ###
    ### Miixed Inheritance: 4451-4500
      # "*_445[1-9]_|*_44[6-9][0-9]_|*_4500_") ###

  ### Invasion! 10k highLow follow-up LOW (biassize 5, othersize 2)
    ### Father Inheritance: 4501-4550
      # "*_450[1-9]_|*_45[1-4][0-9]_|*_4550_") ###
    ### Mother Inheritance: 4551-4600
      # "*_455[1-9]_|*_45[6-9][0-9]_|*_4600_") ###
    ### SamSex Inheritance: 4601-4650
      # "*_460[1-9]_|*_46[1-4][0-9]_|*_4650_") ###
    ### Miixed Inheritance: 4651-4700
      "*_465[1-9]_|*_46[6-9][0-9]_|*_4700_") ###

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
    # extractedmeans <- extractmeans(allrundirs = all_the_runs,
    #     dirheatmap = heatmapland, source_of_params = "params.yaml")
# #   }
# })

# allrundirs = all_the_runs
# dirheatmap = heatmapland
# source_of_params = "params.yaml"

# extractedmeans <- extractmeans(
#   allrundirs = all_the_runs,
#   dirheatmap = heatmapland,
#   source_of_params = "params.yaml")

extractedmeans <- extractmeans(
  allrundirs = all_the_runs,
  dirheatmap = heatmapland,
  # ordering = c(1, 3, 4, 2),
  # ordering = c(1, 3, 4, 2),
  source_of_params = "params.yaml")
all_the_names <- remakestring(all_the_runs, "_", ".")


      # all_the_low_background <- c(all_the_runs[1:25])
      # all_the_high_background <- c(all_the_runs[26:50])

      # extractedmeans <- extractmeans(allrundirs = all_the_high_background,
                                    # dirheatmap = heatmapland,
                                    # source_of_params = "params.yaml",
                                    # deeper = FALSE)
      # all_the_names <- remakestring(all_the_high_background, "_", ".")

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

# whichrunstyle <- c(
  # "lowMedHigh", ### 0-0.25, 0.25-0.5, 0.5-1 (background 0-0.25, 0.5-1)
  # "narrowWide", ### 0.45-0.55 (background 0-0.25, 0.5-1)
  # "lowHigh") ### 0-0.2, 0.2-0.3, 0.4-0.6, 0.55-0.75, 07-0.8 (background 0.2-0.3, 0.7-0.8)
  # "binary") ### 0-0.18, 0.81-1 (background same)
  # "binaryHB") ### 0-0.18, 0.81-1 (background 0.81-1)
  # "binaryLB") ### 0-0.18, 0.81-1 (background 0-0.18)

  # IFF highres,
  # ### 0-0.18, 0.09-0.27, 0.18-0.36, 0.27-0.45, 0.36-0.54, 0.45-0.63, 0.54-0.72, 0.63-0.81, 0.72-0.9, 0.81-1
# source(file.path("scripts", "Source_Heatmap_Functions.R"))

heatmapoutput <- list()
### the higher-res 10k fresh invasions ("11" for 50/50 MixedInh)
# heatmapoutput <- makeheatmapfile(
#                 inheritance = 11, diffcurstartbias = "pop1",
#                 biassize = 5, othersize = 2,
#                 reversedruns = TRUE,
#                 runstyle = "lowHigh", highres = FALSE,
#                 extractedmeans = extractedmeans)

heatmapoutput <- makeheatmapfile(
                inheritance = 11, diffcurstartbias = "pop1",
                biassize = 5, othersize = 2,
                reversedruns = FALSE,
                runstyle = "lowHigh", highres = FALSE,
                extractedmeans = extractedmeans)

# inheritance = 1
# diffcurstartbias = 3
# biassize = 5
# othersize = 1
# reversedruns = TRUE
# specialfigs = TRUE
# runstyle = "pop1"
# highres = FALSE
# extractedmeans = extractedmeans

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

print("Stop here, Parker!")
print("Stop here, Parker!")

source(file.path("scripts", "Source_Heatmap_Functions.R"))
source(file.path("scripts", "Source_Batch_Heatmap_Functions.R"))

output_heatmap <- heatmap_difference (
                    source_pattern = "FfFf",
                    first_source_names = "five-by-five-vanilla_lowHigh_Background",
                    secnd_source_names = "five-by-five-Inv3k_lowHigh_Background",
                    visualization = "absolute",
                    replace = TRUE
                    # foldername =
                    )

individualfigures(2,5,list(
  foldername = output_heatmap$foldername,
  biassize = 5,
  othersize = 2,
  diffcurstartbias = "pop1"
))

### Divergent, Red vs. Blue coloring:

output_heatmap <- heatmap_difference (
                    source_pattern = "male",
                    first_source_names = "five-by-five-followUpInv1k_lowHigh_Background",
                    secnd_source_names = "five-by-five-followUpVanilla_lowHigh_Background",
                    visualization = "midpoint",
                    replace = TRUE
                    )

#### Blue values = High Number,
#### Red values = Low Number

#### So, for example, Inv(high) - Vanilla = Blue,
#### while, in contrast, Van - Inv(high) = Red

# five-by-five-followUpInvLow1k_lowHigh_Background
individualfigures(2,19,list(
  foldername = output_heatmap$foldername,
  biassize = 5,
  othersize = 2,
  diffcurstartbias = "pop1"
))

# _source_names = "five-by-five-followUpVanilla_lowHigh_Background", ### 4101-4300
# _source_names = "five-by-five-followUpInv1k_lowHigh_Background", ### 4101-4300

# source_pattern = "male" # "moth", "same", "FfFf"
# first_source_names = c("five-by-five-followUpInv1k_lowHigh_Background")
# secnd_source_names = c("five-by-five-followUpVanilla_lowHigh_Background")
# visualization = "absolute"
# replace = TRUE

  output_heatmap <- heatmap_difference (
                      source_pattern = "male",
                      first_source_names = "five-by-five-followUpVanilla_lowHigh_Background",
                      secnd_source_names = "five-by-five-followUpInv1k_lowHigh_Background",
                      visualization = "midpoint",
                      replace = TRUE
                      )

first_source_names = "five-by-five-followUpVanilla_lowHigh_Background"
secnd_source_names = "five-by-five-followUpInv1k_lowHigh_Background"

first_source_names = "five-by-five-followUpInv1k_lowHigh_Background"
secnd_source_names = "five-by-five-followUpVanilla_lowHigh_Background"

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

# combineeditsingles(5, 1, 1, 3, T, F)

combineeditsingles(1, 3, 1, 2, T, F)

# inheritancestyle = 5
# bias = 3
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

