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
source(file.path("scripts", "Source_Batch_Heatmap_Functions.R"))

############## # # ARRANGEMENT OF FUNCTIONS  # # ##############

heatmapland <- file.path("results")

# heatmapland <- file.path("results", "Heatmaps")

# all_the_runs <- list.files(heatmapland,

somethingSomething <- list(
  run_names = c("ParentNoInv", "childF1NoInv", "childMalHighCurInv", "childMalLowCurInv", "childFemLowCurInv", "childBothLowCurInv", "childFemHighCurInv", "childBothHighCurInv", "childSmolMalHighCurInv", "childSmolMalLowCurInv", "childSmolFemHighCurInv", "childSmolFemLowCurInv", "childF2NoInv", "childF3NoInv", "childF4NoInv", "childF5NoInv", "childF6NoInv", "childF7NoInv", "childF8NoInv", "childF9NoInv", "childF10NoInv"),
  run_numbers = c("3901-4100", "4101-4300", "4301-4500", "4501-4700", "4701-4900", "5101-5300", "5301-5500", "5501-5700", "5701-5900", "5901-6100", "6101-6300", "6301-6500", "6501-6700", "6701-6900", "6901-7100", "7101-7300", "7301-7500", "7501-7700", "7701-7900", "7901-8100", "8101-8300")
)

subsetsSomethingSomething <- somethingSomething$run_numbers[Nth]


filename_range <- print_regex_num_range(

"3901-4100" ### Parent NoInv
  # "3901-3950" ### mal inh
  # "3951-4000" ### fem inh
  # "4001-4050" ### sam inh
  # "4051-4100" ### mix inh
# "4101-4300" ### Child F1 NoInv
  # "4101-4150" ### mal inh
  # "4151-4200" ### fem inh
  # "4201-4250" ### sam inh
  # "4251-4300" ### mix inh
# "4301-4500" ### Mal High Cur Inv
  # "4301-4350" ### mal inh
  # "4351-4400" ### fem inh
  # "4401-4450" ### sam inh
  # "4451-4500" ### mix inh
# "4501-4700" ### Mal Low Cur Inv
  # "4501-4550" ### mal inh
  # "4551-4600" ### fem inh
  # "4601-4650" ### sam inh
  # "4651-4700" ### mix inh
# "4701-4900" ### Fem Low Cur Inv
  # "4701-4750" ### mal inh
  # "4751-4800" ### fem inh
  # "4801-4850" ### sam inh
  # "4851-4900" ### mix inh
# "4901-5100" ### EMPTY
# "5101-5300" ### Both Low Cur Inv
  # "5101-5150" ### mal inh
  # "5151-5200" ### fem inh
  # "5201-5250" ### sam inh
  # "5251-5300" ### mix inh
# "5301-5500" ### Fem High Cur Inv
  # "5301-5350" ### mal inh
  # "5351-5400" ### fem inh
  # "5401-5450" ### sam inh
  # "5451-5500" ### mix inh
# "5501-5700" ### Both High Cur Inv
  # "5501-5550" ### mal inh
  # "5551-5600" ### fem inh
  # "5601-5650" ### sam inh
  # "5651-5700" ### mix inh
# "5701-5900" ### Smol Mal High Cur Inv
  # "5701-5750" ### mal inh
  # "5751-5800" ### fem inh
  # "5801-5850" ### sam inh
  # "5851-5900" ### mix inh
# "5901-6100" ### Smol Mal Low Cur Inv
  # "5901-5950" ### mal inh
  # "5951-6000" ### fem inh
  # "6001-6050" ### sam inh
  # "6051-6100" ### mix inh
# "6101-6300" ### Smol Fem High Cur Inv
  # "6101-6150" ### mal inh
  # "6151-6200" ### fem inh
  # "6201-6250" ### sam inh
  # "6251-6300" ### mix inh
# "6301-6500" ### Smol Fem Low Cur Inv
  # "6301-6350" ### mal inh
  # "6351-6400" ### fem inh
  # "6401-6450" ### sam inh
  # "6451-6500" ### mix inh
# "6501-6700" ### Child F2 NoInv
  # "6501-6550" ### mal inh
  # "6551-6600" ### fem inh
  # "6601-6650" ### sam inh
  # "6651-6700" ### mix inh
# "6701-6900" ### Child F3 NoInv
  # "6701-6750" ### mal inh
  # "6751-6800" ### fem inh
  # "6801-6850" ### sam inh
  # "6851-6900" ### mix inh
# "6901-7100" ### Child F4 NoInv
  # "6901-6950" ### mal inh
  # "6951-7000" ### fem inh
  # "7001-7050" ### sam inh
  # "7051-7100" ### mix inh
# "7101-7300" ### Child F5 NoInv
  # "7101-7150" ### mal inh
  # "7151-7200" ### fem inh
  # "7201-7250" ### sam inh
  # "7251-7300" ### mix inh
# "7301-7500" ### Child F6 NoInv
  # "7301-7350" ### mal inh
  # "7351-7400" ### fem inh
  # "7401-7450" ### sam inh
  # "7451-7500" ### mix inh
# "7501-7700" ### Child F7 NoInv
  # "7501-7550" ### mal inh
  # "7551-7600" ### fem inh
  # "7601-7650" ### sam inh
  # "7651-7700" ### mix inh
# "7701-7900" ### Child F8 NoInv
  # "7701-7750" ### mal inh
  # "7751-7800" ### fem inh
  # "7801-7850" ### sam inh
  # "7851-7900" ### mix inh
# "7901-8100" ### Child F9 NoInv
  # "7901-7950" ### mal inh
  # "7951-8000" ### fem inh
  # "8001-8050" ### sam inh
  # "8051-8100" ### mix inh
# "8101-8300" ### Child F10 NoInv
  # "8101-8150" ### mal inh
  # "8151-8200" ### fem inh
  # "8201-8250" ### sam inh
  # "8251-8300" ### mix inh
)

all_the_runs <- extractvardirs(heatmapland, filename_range)




# extractedMeans <- extractMeans(allRunDirs = all_the_runs, dirHeatMap = heatmapland, source_of_params = "params.yaml")
extractedmeans <- extractmeans(allrundirs = all_the_runs, dirheatmap = heatmapland, source_of_params = "params.yaml", deeper = FALSE)

all_the_names <- remakestring(all_the_runs, "_", ".")

names(extractedmeans) <- all_the_names


# heatmapland

# makeHeatmaps <- function (
#   inheritance = 1,
#   diffcurstartBias = 1
# )

#   whichInh <- c("male","moth","same","opps","sNTn","sSTf","sSFr","sFrS","sTfS","sTnN", "FfFf")

# whichBias <- c("male","female")

# makeHeatmapFile(inheritance = 3, diffcurstartBias = 3, absolute = TRUE, specialFigs = TRUE, lmhVnw = TRUE, extractedMeans = extractedMeans)
heatmapoutput <- makeheatmapfile(
                inheritance = 1, diffcurstartbias = "pop1",
                biassize = 5, othersize = 2,
                reversedruns = FALSE,
                runstyle = "lowHigh", highres = FALSE,
                extractedmeans = extractedmeans)
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






source(file.path("scripts", "Source_Reference_Section.R"))
referencesection("heatmaps")
source(file.path("scripts", "Source_Heatmap_Functions.R"))
heatmapland <- file.path("results")

all_the_runs <- extractvardirs(heatmapland,
  "*_360[8-9]_|*_36[1-4][0-9]_|*_365[0-7]_") ### oct3n8results ### Father Inheritance 5x5x2 lowHigh Background

extractedmeans <- extractmeans(
  allrundirs = all_the_runs,
  dirheatmap = heatmapland,
  # ordering = c(1, 3, 4, 2),
  source_of_params = "params.yaml")
all_the_names <- remakestring(all_the_runs, "_", ".")

names(extractedmeans) <- all_the_names

heatmapoutput <- list()

heatmapoutput <- makeheatmapfile(
                inheritance = 1, diffcurstartbias = "pop1",
                biassize = 5, othersize = 2,
                reversedruns = FALSE,
                runstyle = "lowHigh", highres = FALSE,
                extractedmeans = extractedmeans)

individualfigures(2,5,heatmapoutput)