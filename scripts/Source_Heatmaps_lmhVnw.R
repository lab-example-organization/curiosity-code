source (file.path ("scripts", "Source_Reference_Section.R"))
referencesection ("heatmaps")

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

######  setwd (file.path (strsplit (getwd (), "curiosity-code") [[1]][1], "curiosity-code"))


# Source the Functions

source (file.path ("scripts", "Source_Heatmap_Functions.R"))

############## # # ARRANGEMENT OF FUNCTIONS  # # ##############

# heatmapland <- HtMpDir (extraDir = "sameInh")
# heatmapland <- file.path ("results", "Heatmaps", "sameInh")
# heatmapland <- file.path ("results", "Heatmaps", "maleInh_maleBias")
# heatmapland <- file.path ("results", "Heatmaps", "femInh_maleBias")
# heatmapland <- file.path ("results", "Heatmaps", "femInh_femBias")
# heatmapland <- file.path ("results")
heatmapland <- file.path ("results", "tenKfiveByFive_child-lowFemSmolInv")

# heatmapland <- file.path ("results", "tenKfiveByFive_child-noInvF2")
# heatmapland <- file.path ("results", "tenKfiveByFive_child-noInvF3")
# heatmapland <- file.path ("results", "tenKfiveByFive_child-noInvF4")


# setwd ("../../../../../../media/park/A443-E926/")

# heatmapland <- file.path ("results", "Heatmaps")

# all_the_runs <- list.files (heatmapland,
all_the_runs <- extractvardirs (heatmapland,

  ### No invasion, 10k highLow (biassize 5, othersize 2)
    ### Father Inheritance: 3901-3950
      # "*_390[1-9]_|*_39[1-4][0-9]_|*_3950_") ###
    ### Mother Inheritance: 3951-4000
      # "*_395[1-9]_|*_39[6-9][0-9]_|*_4000_") ###
    ### SamSex Inheritance: 4001-4050
      # "*_400[1-9]_|*_40[1-4][0-9]_|*_4050_") ###
    ## Miixed Inheritance: 4051-4100
      # "*_405[1-9]_|*_40[6-9][0-9]_|*_4100_") ###

  ### Invasion! 10k noInv follow-up F2 (biassize 5, othersize 2)
    ### Father Inheritance: 6501-6550
      "*_650[1-9]_|*_65[1-4][0-9]_|*_6550_") ###
    ### Mother Inheritance: 6551-6600
      # "*_655[1-9]_|*_65[6-9][0-9]_|*_6600_") ###
    ### SamSex Inheritance: 6601-6650
      # "*_660[1-9]_|*_66[1-4][0-9]_|*_6650_") ###
    ### Miixed Inheritance: 6651-6700
      # "*_665[1-9]_|*_66[6-9][0-9]_|*_6700_") ###

  ### Invasion! 10k noInv follow-up F3 (biassize 5, othersize 2)
    ### Father Inheritance: 6701-6750
      # "*_670[1-9]_|*_67[1-4][0-9]_|*_6750_") ###
    ### Mother Inheritance: 6751-6800
      # "*_675[1-9]_|*_67[6-9][0-9]_|*_6800_") ###
    ### SamSex Inheritance: 6801-6850
      # "*_680[1-9]_|*_68[1-4][0-9]_|*_6850_") ###
    ### Miixed Inheritance: 6851-6900
      # "*_685[1-9]_|*_68[6-9][0-9]_|*_6900_") ###

  ### Invasion! 10k noInv follow-up F4 (biassize 5, othersize 2)
    ### Father Inheritance: 6901-6950
      # "*_690[1-9]_|*_69[1-4][0-9]_|*_6950_") ###
    ### Mother Inheritance: 6951-7000
      # "*_695[1-9]_|*_69[6-9][0-9]_|*_7000_") ###
    ### SamSex Inheritance: 7001-7050
      # "*_700[1-9]_|*_70[1-4][0-9]_|*_7050_") ###
    ### Miixed Inheritance: 7051-71 00
      # "*_705[1-9]_|*_70[6-9][0-9]_|*_7100_") ###




#   connection <- file(description = file.path ("source","temp", paste0 (specificSimNumber, "_sim_data.txt")), open = "rt")
#   multiRun_folderList <- as.vector (read.table(connection, -1L) [[2]])
#   close(connection)


# profvis ({
# #   for (iteration in 1 : 10) {
    # extractedmeans <- extractmeans (allrundirs = all_the_runs,
    #     dirheatmap = heatmapland, source_of_params = "params.yaml")
# #   }
# })

# allrundirs = all_the_runs
# dirheatmap = heatmapland
# source_of_params = "params.yaml"

# extractedmeans <- extractmeans (
#   allrundirs = all_the_runs,
#   dirheatmap = heatmapland,
#   source_of_params = "params.yaml")

extractedmeans <- extractmeans (
  allrundirs = all_the_runs,
  dirheatmap = heatmapland,
  # ordering = c (1, 3, 4, 2),
  # ordering = c (1, 3, 4, 2),
  source_of_params = "params.yaml")
all_the_names <- remakestring (all_the_runs, "_", ".")


      # all_the_low_background <- c (all_the_runs[1 : 25])
      # all_the_high_background <- c (all_the_runs[26:50])

      # extractedmeans <- extractmeans (allrundirs = all_the_high_background,
                                    # dirheatmap = heatmapland,
                                    # source_of_params = "params.yaml",
                                    # deeper = FALSE)
      # all_the_names <- remakestring (all_the_high_background, "_", ".")

names (extractedmeans) <- all_the_names





heatmapoutput <- list ()
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



# whichInh <- c ("male","moth","same","opps","sNTn","sSTf","sSFr","sFrS","sTfS","sTnN", "FfFf")


# individualfigures (5:10, 2, 1 : 2, 5)

# inheritance = 5
# colorrange = 2
# thisBias = 1
# numotherpopruns = 5

# thing <- c (5, 6, 7, 8, 9, 10)
# for (inhREtnce in 1 : 5) {
#   for (sexBIAS in 1 : 2) {

#     individualfigures (10, 2, sexBIAS, 3)

#   }
# }

##### ? WILL THIS WORK?
individualfigures (2,5,heatmapoutput)
#####

# print ("Stop here, Parker!")
# print ("Stop here, Parker!")

source (file.path ("scripts", "Source_Heatmap_Functions.R"))
source (file.path ("scripts", "Source_Difference_Heatmap_Functions.R"))

output_heatmap <- heatmap_difference (
                    source_pattern = "FfFf",
                    first_source_names = "five-by-five-vanilla_lowHigh_Background",
                    secnd_source_names = "five-by-five-Inv3k_lowHigh_Background",
                    visualization = "absolute",
                    replace = TRUE
                    # foldername =
                    )

individualfigures (2,5,list (
  foldername = output_heatmap$foldername,
  biassize = 5,
  othersize = 2,
  diffcurstartbias = "pop1"
))

### Divergent, Red vs. Blue coloring:

output_heatmap <- heatmap_difference (
                    source_pattern = "male",
                    first_source_names = "five-by-five-followUpInvHigh1k_lowHigh_Background",
                    secnd_source_names = "five-by-five-followUpVanilla_lowHigh_Background",
                    visualization = "midpoint",
                    replace = TRUE
                    )

#### Blue values = High Number,
#### Red values = Low Number

#### So, for example, Inv (high) - Vanilla = Blue,
#### while, in contrast, Van - Inv (high) = Red

# five-by-five-followUpInvLow1k_lowHigh_Background
individualfigures (2,19,list (
  foldername = output_heatmap$foldername,
  biassize = 5,
  othersize = 2,
  diffcurstartbias = "pop1"
))

# _source_names = "five-by-five-followUpVanilla_lowHigh_Background", ### 4101-4300
# _source_names = "five-by-five-followUpInv1k_lowHigh_Background", ### 4101-4300

# source_pattern = "male" # "moth", "same", "FfFf"
# first_source_names = c ("five-by-five-followUpInvHigh1k_lowHigh_Background")
# secnd_source_names = c ("five-by-five-followUpVanilla_lowHigh_Background")
# visualization = "midpoint"
# replace = TRUE

  output_heatmap <- heatmap_difference (
                      source_pattern = "male",
                      first_source_names = "five-by-five-followUpVanilla_lowHigh_Background",
                      secnd_source_names = "five-by-five-followUpInvHigh1k_lowHigh_Background",
                      visualization = "midpoint",
                      replace = TRUE
                      )

first_source_names = "five-by-five-followUpVanilla_lowHigh_Background"
secnd_source_names = "five-by-five-followUpInv1k_lowHigh_Background"

first_source_names = "five-by-five-followUpInv1k_lowHigh_Background"
secnd_source_names = "five-by-five-followUpVanilla_lowHigh_Background"

# thing <- c (5, 6, 7, 8, 9, 10)
# for (
#   inhREtnce in 1 : 6
# ) {
#   for (
#     sexBIAS in 1 : 8
#   ) {
#     combineeditsingles (inhREtnce[thing], 1, sexBIAS, 3, TRUE, FALSE)
#   }
# }

# combineeditsingles (5, 1, 1, 3, TRUE, FALSE)

combineeditsingles (1, 3, 1, 2, TRUE, FALSE)

# inheritancestyle = 5
# bias = 3
# metricssexpop = 1
# otherpopstyle = 3
# edit = TRUE
# lmhvnw = FALSE



# for (k in 1 : 2) {
#     thing <- c (3, 11)


#     for (i in 1 : 4) {
#         for (j in 1 : 2) {
#             combineeditsingles (thing[k],3,i,j, TRUE)
#         }
#     }
# }






# for (inhpattern in 1 : 4) {
#   individualfigures (inhpattern, 2)

#   for (spranges in 1 : 2) {
#     stackmultiples (inhpattern, spranges)
#   }
# }

