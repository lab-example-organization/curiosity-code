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

all_the_runs <- extractvardirs(heatmapland,
  "*_379[4-9]_|*_38[0-3][0-9]_|*_384[0-3]_") ### octXresults ### Mother Inheritance 5x5x2 lowHigh Background

extractedmeans <- extractmeans(
  allrundirs = all_the_runs,
  dirheatmap = heatmapland,
  # ordering = c(1, 3, 4, 2),
  source_of_params = "params.yaml")
all_the_names <- remakestring(all_the_runs, "_", ".")

names(extractedmeans) <- all_the_names

heatmapoutput <- list()

heatmapoutput <- makeheatmapfile(
                inheritance = 2, diffcurstartbias = "pop1",
                biassize = 5, othersize = 2,
                reversedruns = FALSE,
                runstyle = "lowHigh", highres = FALSE,
                extractedmeans = extractedmeans)

individualfigures(2,5,heatmapoutput)

all_the_runs <- extractvardirs(heatmapland,
  "*_365[8-9]_|*_36[6-9][0-9]_|*_370[0-7]_") ### oct3n8results ### SameSex Inheritance 5x5x2 lowHigh Background

extractedmeans <- extractmeans(
  allrundirs = all_the_runs,
  dirheatmap = heatmapland,
  # ordering = c(1, 3, 4, 2),
  source_of_params = "params.yaml")
all_the_names <- remakestring(all_the_runs, "_", ".")

names(extractedmeans) <- all_the_names

heatmapoutput <- list()

heatmapoutput <- makeheatmapfile(
                inheritance = 3, diffcurstartbias = "pop1",
                biassize = 5, othersize = 2,
                reversedruns = FALSE,
                runstyle = "lowHigh", highres = FALSE,
                extractedmeans = extractedmeans)

individualfigures(2,5,heatmapoutput)

all_the_runs <- extractvardirs(heatmapland,
  "*_370[8-9]_|*_37[1-4][0-9]_|*_375[0-7]_") ### oct3n8results ### Mixed55 Inheritance 5x5x2 lowHigh Background

extractedmeans <- extractmeans(
  allrundirs = all_the_runs,
  dirheatmap = heatmapland,
  # ordering = c(1, 3, 4, 2),
  source_of_params = "params.yaml")
all_the_names <- remakestring(all_the_runs, "_", ".")

names(extractedmeans) <- all_the_names

heatmapoutput <- list()

heatmapoutput <- makeheatmapfile(
                inheritance = 11, diffcurstartbias = "pop1",
                biassize = 5, othersize = 2,
                reversedruns = FALSE,
                runstyle = "lowHigh", highres = FALSE,
                extractedmeans = extractedmeans)

individualfigures(2,5,heatmapoutput)

all_the_runs <- extractvardirs(heatmapland,



  ### 200k vanilla:
    ### Father Inheritance: 3581-3589, 3889
      "*_358[3, 8, 9]_|*_3889_") ### Low Background

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
                biassize = 2, othersize = 1,
                reversedruns = FALSE,
                runstyle = "binaryLB", highres = FALSE,
                extractedmeans = extractedmeans)

individualfigures(2,5,heatmapoutput)

all_the_runs <- extractvardirs(heatmapland,
      "*_358[1, 2, 6, 7]_") ### High Background #%$^&@*

extractedmeans <- extractmeans(
  allrundirs = all_the_runs,
  dirheatmap = heatmapland,
  ordering = c(1, 3, 4, 2),
  source_of_params = "params.yaml")
all_the_names <- remakestring(all_the_runs, "_", ".")

names(extractedmeans) <- all_the_names

heatmapoutput <- list()

heatmapoutput <- makeheatmapfile(
                inheritance = 1, diffcurstartbias = "pop1",
                biassize = 2, othersize = 1,
                reversedruns = FALSE,
                runstyle = "binaryHB", highres = FALSE,
                extractedmeans = extractedmeans)

individualfigures(2,5,heatmapoutput)

all_the_runs <- extractvardirs(heatmapland,
    ### Mother Inheritance: 3785-3793, 3890
      "*_3787_|*_379[2, 3]_|*_3890_") ### Low Background

extractedmeans <- extractmeans(
  allrundirs = all_the_runs,
  dirheatmap = heatmapland,
  # ordering = c(1, 3, 4, 2),
  source_of_params = "params.yaml")
all_the_names <- remakestring(all_the_runs, "_", ".")

names(extractedmeans) <- all_the_names

heatmapoutput <- list()

heatmapoutput <- makeheatmapfile(
                inheritance = 2, diffcurstartbias = "pop1",
                biassize = 2, othersize = 1,
                reversedruns = FALSE,
                runstyle = "binaryLB", highres = FALSE,
                extractedmeans = extractedmeans)

individualfigures(2,5,heatmapoutput)

all_the_runs <- extractvardirs(heatmapland,
      "*_378[5, 6]_|*_379[0, 1]_") ### High Background #%$^&@*

extractedmeans <- extractmeans(
  allrundirs = all_the_runs,
  dirheatmap = heatmapland,
  ordering = c(1, 3, 4, 2),
  source_of_params = "params.yaml")
all_the_names <- remakestring(all_the_runs, "_", ".")

names(extractedmeans) <- all_the_names

heatmapoutput <- list()

heatmapoutput <- makeheatmapfile(
                inheritance = 2, diffcurstartbias = "pop1",
                biassize = 2, othersize = 1,
                reversedruns = FALSE,
                runstyle = "binaryHB", highres = FALSE,
                extractedmeans = extractedmeans)

individualfigures(2,5,heatmapoutput)

all_the_runs <- extractvardirs(heatmapland,
    ### SameSex Inheritance: 3590-3598, 3891
      "*_359[2, 7, 8]_|*_3891_") ### Low Background

extractedmeans <- extractmeans(
  allrundirs = all_the_runs,
  dirheatmap = heatmapland,
  # ordering = c(1, 3, 4, 2),
  source_of_params = "params.yaml")
all_the_names <- remakestring(all_the_runs, "_", ".")

names(extractedmeans) <- all_the_names

heatmapoutput <- list()

heatmapoutput <- makeheatmapfile(
                inheritance = 3, diffcurstartbias = "pop1",
                biassize = 2, othersize = 1,
                reversedruns = FALSE,
                runstyle = "binaryLB", highres = FALSE,
                extractedmeans = extractedmeans)

individualfigures(2,5,heatmapoutput)

all_the_runs <- extractvardirs(heatmapland,
      "*_359[0, 1, 5, 6]_") ### High Background #%$^&@*

extractedmeans <- extractmeans(
  allrundirs = all_the_runs,
  dirheatmap = heatmapland,
  ordering = c(1, 3, 4, 2),
  source_of_params = "params.yaml")
all_the_names <- remakestring(all_the_runs, "_", ".")

names(extractedmeans) <- all_the_names

heatmapoutput <- list()

heatmapoutput <- makeheatmapfile(
                inheritance = 3, diffcurstartbias = "pop1",
                biassize = 2, othersize = 1,
                reversedruns = FALSE,
                runstyle = "binaryHB", highres = FALSE,
                extractedmeans = extractedmeans)

individualfigures(2,5,heatmapoutput)

all_the_runs <- extractvardirs(heatmapland,
    ### Mixed55 Inheritance: 3599-3607, 3892
      "*_360[1, 6, 7]_|*_3892_") ### Low Background

extractedmeans <- extractmeans(
  allrundirs = all_the_runs,
  dirheatmap = heatmapland,
  # ordering = c(1, 3, 4, 2),
  source_of_params = "params.yaml")
all_the_names <- remakestring(all_the_runs, "_", ".")

names(extractedmeans) <- all_the_names

heatmapoutput <- list()

heatmapoutput <- makeheatmapfile(
                inheritance = 11, diffcurstartbias = "pop1",
                biassize = 2, othersize = 1,
                reversedruns = FALSE,
                runstyle = "binaryLB", highres = FALSE,
                extractedmeans = extractedmeans)

individualfigures(2,5,heatmapoutput)

all_the_runs <- extractvardirs(heatmapland,
      "*_3599_|*_360[0, 4, 5]_") ### High Background #%$^&@*

extractedmeans <- extractmeans(
  allrundirs = all_the_runs,
  dirheatmap = heatmapland,
  ordering = c(1, 3, 4, 2),
  source_of_params = "params.yaml")
all_the_names <- remakestring(all_the_runs, "_", ".")

names(extractedmeans) <- all_the_names

heatmapoutput <- list()

heatmapoutput <- makeheatmapfile(
                inheritance = 11, diffcurstartbias = "pop1",
                biassize = 2, othersize = 1,
                reversedruns = FALSE,
                runstyle = "binaryHB", highres = FALSE,
                extractedmeans = extractedmeans)

individualfigures(2,5,heatmapoutput)

all_the_runs <- extractvardirs(heatmapland,
  ### 200k post-inv:
    ### Father Inheritance: 3758-3766, 3893
      "*_376[0, 5, 6]_|*_3893_") ### Low Background

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
                biassize = 2, othersize = 1,
                reversedruns = FALSE,
                runstyle = "binaryLB", highres = FALSE,
                extractedmeans = extractedmeans)

individualfigures(2,5,heatmapoutput)

all_the_runs <- extractvardirs(heatmapland,
      "*_375[8, 9]_|*_376[3, 4]_") ### High Background #%$^&@*

extractedmeans <- extractmeans(
  allrundirs = all_the_runs,
  dirheatmap = heatmapland,
  ordering = c(1, 3, 4, 2),
  source_of_params = "params.yaml")
all_the_names <- remakestring(all_the_runs, "_", ".")

names(extractedmeans) <- all_the_names

heatmapoutput <- list()

heatmapoutput <- makeheatmapfile(
                inheritance = 1, diffcurstartbias = "pop1",
                biassize = 2, othersize = 1,
                reversedruns = FALSE,
                runstyle = "binaryHB", highres = FALSE,
                extractedmeans = extractedmeans)

individualfigures(2,5,heatmapoutput)

all_the_runs <- extractvardirs(heatmapland,
    ## Mother Inheritance: 3844-3852, 3894
      "*_3846_|*_385[1, 2]_|*_3894_") ### Low Background

extractedmeans <- extractmeans(
  allrundirs = all_the_runs,
  dirheatmap = heatmapland,
  # ordering = c(1, 3, 4, 2),
  source_of_params = "params.yaml")
all_the_names <- remakestring(all_the_runs, "_", ".")

names(extractedmeans) <- all_the_names

heatmapoutput <- list()

heatmapoutput <- makeheatmapfile(
                inheritance = 2, diffcurstartbias = "pop1",
                biassize = 2, othersize = 1,
                reversedruns = FALSE,
                runstyle = "binaryLB", highres = FALSE,
                extractedmeans = extractedmeans)

individualfigures(2,5,heatmapoutput)

all_the_runs <- extractvardirs(heatmapland,
      "*_384[4, 5, 9]_|*_3850_") ### High Background #%$^&@*

extractedmeans <- extractmeans(
  allrundirs = all_the_runs,
  dirheatmap = heatmapland,
  ordering = c(1, 3, 4, 2),
  source_of_params = "params.yaml")
all_the_names <- remakestring(all_the_runs, "_", ".")

names(extractedmeans) <- all_the_names

heatmapoutput <- list()

heatmapoutput <- makeheatmapfile(
                inheritance = 2, diffcurstartbias = "pop1",
                biassize = 2, othersize = 1,
                reversedruns = FALSE,
                runstyle = "binaryHB", highres = FALSE,
                extractedmeans = extractedmeans)

individualfigures(2,5,heatmapoutput)

all_the_runs <- extractvardirs(heatmapland,
    ### SameSex Inheritance: 3767-3775, 3895
      "*_3769_|*_377[4, 5]_|*_3895_") ### Low Background

extractedmeans <- extractmeans(
  allrundirs = all_the_runs,
  dirheatmap = heatmapland,
  # ordering = c(1, 3, 4, 2),
  source_of_params = "params.yaml")
all_the_names <- remakestring(all_the_runs, "_", ".")

names(extractedmeans) <- all_the_names

heatmapoutput <- list()

heatmapoutput <- makeheatmapfile(
                inheritance = 3, diffcurstartbias = "pop1",
                biassize = 2, othersize = 1,
                reversedruns = FALSE,
                runstyle = "binaryLB", highres = FALSE,
                extractedmeans = extractedmeans)

individualfigures(2,5,heatmapoutput)

all_the_runs <- extractvardirs(heatmapland,
      "*_376[7, 8]_|*_377[2, 3]_") ### High Background #%$^&@*

extractedmeans <- extractmeans(
  allrundirs = all_the_runs,
  dirheatmap = heatmapland,
  ordering = c(1, 3, 4, 2),
  source_of_params = "params.yaml")
all_the_names <- remakestring(all_the_runs, "_", ".")

names(extractedmeans) <- all_the_names

heatmapoutput <- list()

heatmapoutput <- makeheatmapfile(
                inheritance = 3, diffcurstartbias = "pop1",
                biassize = 2, othersize = 1,
                reversedruns = FALSE,
                runstyle = "binaryHB", highres = FALSE,
                extractedmeans = extractedmeans)

individualfigures(2,5,heatmapoutput)

all_the_runs <- extractvardirs(heatmapland,
    ### Mixed55 Inheritance: 3776-3784, 3896
      "*_3778_|*_378[3, 4]_|*_3896_") ### Low Background

extractedmeans <- extractmeans(
  allrundirs = all_the_runs,
  dirheatmap = heatmapland,
  # ordering = c(1, 3, 4, 2),
  source_of_params = "params.yaml")
all_the_names <- remakestring(all_the_runs, "_", ".")

names(extractedmeans) <- all_the_names

heatmapoutput <- list()

heatmapoutput <- makeheatmapfile(
                inheritance = 11, diffcurstartbias = "pop1",
                biassize = 2, othersize = 1,
                reversedruns = FALSE,
                runstyle = "binaryLB", highres = FALSE,
                extractedmeans = extractedmeans)

individualfigures(2,5,heatmapoutput)

all_the_runs <- extractvardirs(heatmapland,
      "*_377[6, 7]_|*_378[1, 2]_") ### High Background #%$^&@*

extractedmeans <- extractmeans(
  allrundirs = all_the_runs,
  dirheatmap = heatmapland,
  ordering = c(1, 3, 4, 2),
  source_of_params = "params.yaml")
all_the_names <- remakestring(all_the_runs, "_", ".")

names(extractedmeans) <- all_the_names

heatmapoutput <- list()

heatmapoutput <- makeheatmapfile(
                inheritance = 11, diffcurstartbias = "pop1",
                biassize = 2, othersize = 1,
                reversedruns = FALSE,
                runstyle = "binaryHB", highres = FALSE,
                extractedmeans = extractedmeans)

individualfigures(2,5,heatmapoutput)

all_the_runs <- extractvardirs(heatmapland,
  ### Inv no 200k:
    ### Father Inheritance: 3853-3861, 3897
      "*_3855_|*_386[0,1]_|*_3897_") ### Low Background

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
                biassize = 2, othersize = 1,
                reversedruns = FALSE,
                runstyle = "binaryLB", highres = FALSE,
                extractedmeans = extractedmeans)

individualfigures(2,5,heatmapoutput)

all_the_runs <- extractvardirs(heatmapland,
      "*_385[3, 4, 8, 9]_") ### High Background #%$^&@*

extractedmeans <- extractmeans(
  allrundirs = all_the_runs,
  dirheatmap = heatmapland,
  ordering = c(1, 3, 4, 2),
  source_of_params = "params.yaml")
all_the_names <- remakestring(all_the_runs, "_", ".")

names(extractedmeans) <- all_the_names

heatmapoutput <- list()

heatmapoutput <- makeheatmapfile(
                inheritance = 1, diffcurstartbias = "pop1",
                biassize = 2, othersize = 1,
                reversedruns = FALSE,
                runstyle = "binaryHB", highres = FALSE,
                extractedmeans = extractedmeans)

individualfigures(2,5,heatmapoutput)

all_the_runs <- extractvardirs(heatmapland,
    ### Mother Inheritance: 3862-3870, 3898
      "*_386[4, 9]_|*_3870_|*_3898_") ### Low Background

extractedmeans <- extractmeans(
  allrundirs = all_the_runs,
  dirheatmap = heatmapland,
  # ordering = c(1, 3, 4, 2),
  source_of_params = "params.yaml")
all_the_names <- remakestring(all_the_runs, "_", ".")

names(extractedmeans) <- all_the_names

heatmapoutput <- list()

heatmapoutput <- makeheatmapfile(
                inheritance = 2, diffcurstartbias = "pop1",
                biassize = 2, othersize = 1,
                reversedruns = FALSE,
                runstyle = "binaryLB", highres = FALSE,
                extractedmeans = extractedmeans)

individualfigures(2,5,heatmapoutput)

all_the_runs <- extractvardirs(heatmapland,
      "*_386[2, 3, 7, 8]_") ### High Background #%$^&@*

extractedmeans <- extractmeans(
  allrundirs = all_the_runs,
  dirheatmap = heatmapland,
  ordering = c(1, 3, 4, 2),
  source_of_params = "params.yaml")
all_the_names <- remakestring(all_the_runs, "_", ".")

names(extractedmeans) <- all_the_names

heatmapoutput <- list()

heatmapoutput <- makeheatmapfile(
                inheritance = 2, diffcurstartbias = "pop1",
                biassize = 2, othersize = 1,
                reversedruns = FALSE,
                runstyle = "binaryHB", highres = FALSE,
                extractedmeans = extractedmeans)

individualfigures(2,5,heatmapoutput)

all_the_runs <- extractvardirs(heatmapland,
    ### SameSex Inheritance: 3871-3879, 3899
      "*_387[3, 8, 9]_|*_3899_") ### Low Background

extractedmeans <- extractmeans(
  allrundirs = all_the_runs,
  dirheatmap = heatmapland,
  # ordering = c(1, 3, 4, 2),
  source_of_params = "params.yaml")
all_the_names <- remakestring(all_the_runs, "_", ".")

names(extractedmeans) <- all_the_names

heatmapoutput <- list()

heatmapoutput <- makeheatmapfile(
                inheritance = 3, diffcurstartbias = "pop1",
                biassize = 2, othersize = 1,
                reversedruns = FALSE,
                runstyle = "binaryLB", highres = FALSE,
                extractedmeans = extractedmeans)

individualfigures(2,5,heatmapoutput)

all_the_runs <- extractvardirs(heatmapland,
      "*_387[1, 2, 6, 7]_") ### High Background #%$^&@*

extractedmeans <- extractmeans(
  allrundirs = all_the_runs,
  dirheatmap = heatmapland,
  ordering = c(1, 3, 4, 2),
  source_of_params = "params.yaml")
all_the_names <- remakestring(all_the_runs, "_", ".")

names(extractedmeans) <- all_the_names

heatmapoutput <- list()

heatmapoutput <- makeheatmapfile(
                inheritance = 3, diffcurstartbias = "pop1",
                biassize = 2, othersize = 1,
                reversedruns = FALSE,
                runstyle = "binaryHB", highres = FALSE,
                extractedmeans = extractedmeans)

individualfigures(2,5,heatmapoutput)

all_the_runs <- extractvardirs(heatmapland,
    ### Mixed55 Inheritance: 3880-3888, 3900
      "*_388[2, 7, 8]_|*_3900_") ### Low Background

extractedmeans <- extractmeans(
  allrundirs = all_the_runs,
  dirheatmap = heatmapland,
  # ordering = c(1, 3, 4, 2),
  source_of_params = "params.yaml")
all_the_names <- remakestring(all_the_runs, "_", ".")

names(extractedmeans) <- all_the_names

heatmapoutput <- list()

heatmapoutput <- makeheatmapfile(
                inheritance = 11, diffcurstartbias = "pop1",
                biassize = 2, othersize = 1,
                reversedruns = FALSE,
                runstyle = "binaryLB", highres = FALSE,
                extractedmeans = extractedmeans)

individualfigures(2,5,heatmapoutput)

all_the_runs <- extractvardirs(heatmapland,
      "*_388[0, 1, 5, 6]_") ### High Background #%$^&@*

extractedmeans <- extractmeans(
  allrundirs = all_the_runs,
  dirheatmap = heatmapland,
  ordering = c(1, 3, 4, 2),
  source_of_params = "params.yaml")
all_the_names <- remakestring(all_the_runs, "_", ".")

names(extractedmeans) <- all_the_names

heatmapoutput <- list()

heatmapoutput <- makeheatmapfile(
                inheritance = 11, diffcurstartbias = "pop1",
                biassize = 2, othersize = 1,
                reversedruns = FALSE,
                runstyle = "binaryHB", highres = FALSE,
                extractedmeans = extractedmeans)

individualfigures(2,5,heatmapoutput)


# extractedmeans <- extractmeans(
#   allrundirs = all_the_runs,
#   dirheatmap = heatmapland,
#   # ordering = c(1, 3, 4, 2),
#   source_of_params = "params.yaml")
# all_the_names <- remakestring(all_the_runs, "_", ".")

# names(extractedmeans) <- all_the_names

# heatmapoutput <- list()

# heatmapoutput <- makeheatmapfile(
#                 inheritance = 3, diffcurstartbias = "pop1",
#                 biassize = 2, othersize = 1,
#                 reversedruns = FALSE,
#                 runstyle = "binaryLB", highres = FALSE,
#                 extractedmeans = extractedmeans)

# individualfigures(2,5,heatmapoutput)

# all_the_runs <- extractvardirs(heatmapland,

source(file.path("scripts", "Source_Reference_Section.R"))
referencesection("heatmaps")

source(file.path("scripts", "Source_Heatmap_Functions.R"))
source(file.path("scripts", "Source_Difference_Heatmap_Functions.R"))

thing <- c("male", "moth", "same", "FfFf")
# das_dinge <- c(1,2,3,11)
stuff <- c("five-by-five-Inv3k_lowHigh_Background",                       #1
           "five-by-five-vanilla_lowHigh_Background",                     #2
           "five-by-five-followUpVanilla_lowHigh_Background",             #3
           "five-by-five-followUpInvLow1k_lowHigh_Background",            #4
           "five-by-five-followUpInvHigh1k_lowHigh_Background",           #5
           "five-by-five-followUpFemInvLow1k_lowHigh_Background",         #6
           "five-by-five-followUpBothInvLow1k_lowHigh_Background",        #7
           "five-by-five-followUpFemInvHigh1k_lowHigh_Background",        #8
           "five-by-five-followUpMalSmolInvHigh1k_lowHigh_Background",    #9
           "five-by-five-followUpMalSmolInvLow1k_lowHigh_Background",    #10
           "five-by-five-followUpBothInvHigh1k_lowHigh_Background",      #11
           "five-by-five-followUpBothInvHighAgain1k_lowHigh_Background", #12
           "five-by-five-followUpFemSmolInvHigh1k_lowHigh_Background"#,  #13
        #    "five-by-five-followUpFemSmolInvLow1k_lowHigh_Background",  #14
          #  "tenKfiveByFive_child-lowFemInvtK/five-by-five-followUpFemHigh1k_lowHigh_Background",
         #  "",)
)

# stuff_n_things <- array (c (1, 1, 1, 2, 3, 3, 4, 2, 3, 5, 3, 4, 5, 5), c (7,2))
# stuff_n_things <- array (c (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13), c (6,2))
stuff_n_things <- array (c (1, 2, rep(8,7), rep(9,8), rep(10,9), rep(11,10), rep(12,11), rep(13,12), 4, 4, c(1:7), c(1:8), c(1:9), c(1:10), c(1:11), c(1:12)), c (59,2))
stuff_n_things <- array (c(1,2), c(1,2))
for (bs in 1:dim(stuff_n_things)[1]) {
  for(whaaat in 1:4) {

    output_heatmap <- heatmap_difference (
                        source_pattern = thing[whaaat],
                        first_source_names = stuff[stuff_n_things[bs, 1]],
                        secnd_source_names = stuff[stuff_n_things[bs, 2]],
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
                        source_pattern = thing[whaaat],
                        first_source_names = stuff[stuff_n_things[bs, 2]],
                        secnd_source_names = stuff[stuff_n_things[bs, 1]],
                        visualization = "midpoint",
                        replace = TRUE
                        )

    individualfigures(2,19,list(
      foldername = output_heatmap$foldername,
      biassize = 5,
      othersize = 2,
      diffcurstartbias = "pop1"
    ))

  }
}


# image(x = array(c(39:63)/100, c(5,5)),
thing <- as.matrix((first_heatmap[,,1,1] - second_heatmap[,,1,1])/2 + 0.5)
thing2 <- as.matrix((second_heatmap[,,1,1] - first_heatmap[,,1,1])/2 + 0.5)
thing3 <- output_heatmap[,,1,1]
# image(x = thing,
image(x = thing3,
# image(x = array(c(39:63)/100, c(5,5)),
          col = colorseqmultpalette[[19]](100),
          axes = F, zlim = c(0, 1))








# 777

# x <- array(c(1:100, rep(1,100)), c(100, 2))
# x <- array(c(1:100, rep(9.5,100), rep(9.7,100), rep(9.9,100), rep(10.1,100), rep(10.3,100), rep(10.5,100), rep(10.7,100), rep(10.9,100), rep(11.1,100), rep(11.3,100)), c(100, 11))

# plot(x[,1], x[,2], col = colorseqmultpalette[[19]](100), pch = 15, cex = 5, axes = F, xlab = "", ylab = "")
# plot(x[,1], x[,2], col = colorseqmultpalette[[19]](100), pch = 15, cex = 5, axes = F, xlab = "", ylab = "")
# axis(1, c(0, 0.1, 1, 10, 100), c("0", "0.1", "1", "10", "100"),T,0,NA,F,cex.axis = 0.8, tck = 0)

colorpalette = 19
midpoint_size <- 1

if (colorpalette == 19) {

    whatever <- array(c(2, 16, 30, 44, 58, 72, 86, 7, 6, 5, 4, 3, 2, 1), c(7,2))
    stuff <- whatever[midpoint_size,]
  } else {
    stuff <- c(2, 7)
  }


  colorseqmultpalette <- list (
    reds = colorRampPalette (c ("#fee0d2", "#fc9272", "#de2d26")), # 3-class reds                                        ### 1
    rdpu = colorRampPalette (c ("#fde0dd", "#fa9fb5", "#c51b8a")), # 3-class rdpu                                        ### 2
    oranges = colorRampPalette (c ("#fee6ce", "#fdae6b", "#e6550d")), # 3-class oranges                                  ### 3
    orrd = colorRampPalette (c ("#fee8c8", "#fdbb84", "#e34a33")), # 3-class orrd                                        ### 4
    ylorrd = colorRampPalette (c ("#ffeda0", "#feb24c", "#f03b20")), # 3-class ylorrd                                    ### 5
    ylorbr = colorRampPalette (c ("#fff7bc", "#fec44f", "#d95f0e")), # 3-class ylorbr                                    ### 6
    ylgn = colorRampPalette (c ("#f7fcb9", "#addd8e", "#31a354")), # 3-class ylgn                                        ### 7
    ylgnbu = colorRampPalette (c ("#edf8b1", "#7fcdbb", "#2c7fb8")), # 3-class ylgnbu                                    ### 8
    greens = colorRampPalette (c ("#e5f5e0", "#a1d99b", "#31a354")), # 3-class greens                                    ### 9
    gnbu = colorRampPalette (c ("#e0f3db", "#a8ddb5", "#43a2ca")), # 3-class gnbu                                        ### 10
    blues = colorRampPalette (c ("#deebf7", "#9ecae1", "#3182bd")), # 3-class blues                                      ### 11
    bugn = colorRampPalette (c ("#e5f5f9", "#99d8c9", "#2ca25f")), # 3-class bugn                                        ### 12
    bupu = colorRampPalette (c ("#e0ecf4", "#9ebcda", "#8856a7")), # 3-class bupu                                        ### 13
    purples = colorRampPalette (c ("#efedf5", "#bcbddc", "#756bb1")), # 3-class purples                                  ### 14
    purd = colorRampPalette (c ("#e7e1ef", "#c994c7", "#dd1c77")), # 3-class purd                                        ### 15
    pubu = colorRampPalette (c ("#ece7f2", "#a6bddb", "#2b8cbe")), # 3-class pubu                                        ### 16
    pubugn = colorRampPalette (c ("#ece2f0", "#a6bddb", "#1c9099")), # 3-class pubugn                                    ### 17
    greys = colorRampPalette (c ("#f0f0f0", "#bdbdbd", "#636363")), # 3-class greys                                      ### 18
    # midpoint = colorRampPalette (c ("#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac"))

    eval (parse (text = paste0 ("midpoint = colorRampPalette (c (rep(\"#67001f\", ",
                                                   stuff[2], "), rep(\"#b2182b\", ",
                                                   stuff[2], "), rep(\"#ca0020\", ",
                                                   stuff[2], "), rep(\"#d6604d\", ",
                                                   stuff[2], "), rep(\"#ef8a62\", ",
                                                   stuff[2], "), rep(\"#f4a582\", ",
                                                   stuff[2], "), rep(\"#fddbc7\", ",
                                                   stuff[2], "), rep(\"#f7f7f7\", ",
                                                   stuff[1], "), rep(\"#d1e5f0\", ",
                                                   stuff[2], "), rep(\"#92c5de\", ",
                                                   stuff[2], "), rep(\"#67a9cf\", ",
                                                   stuff[2], "), rep(\"#4393c3\", ",
                                                   stuff[2], "), rep(\"#0571b0\", ",
                                                   stuff[2], "), rep(\"#2166ac\", ",
                                                   stuff[2], "), rep(\"#053061\", ",
                                                   stuff[2], ")))")))
  )



# c(-2.5, 4.5, 11.5, 18.5, 25.5, 32.5, 39.5, 46.5, 48.5, 55.5, 62.5, 69.5, 76.5, 83.5, 90.5, 97.5, 103.5)
# length(c(1, 8, 15, 22, 29, 36, 43, 47.5, 52, 59, 66, 73, 80, 87, 94, 100.5))

source(file.path("scripts", "Source_Reference_Section.R"))
referencesection("heatmaps")
source(file.path("scripts", "Source_Heatmap_Functions.R"))


# heatmapland <- file.path("results", "tenKfiveByFive_child-highFemInv")
# all_the_runs <- extractvardirs(heatmapland,

### Invasion! 10k Fem_HL follow-up HIGH (biassize 5, othersize 2)
    ### Father Inheritance: 5301-5350
    #   "*_530[1-9]_|*_53[1-4][0-9]_|*_5350_") ###

        # extractedmeans <- extractmeans(
        #     allrundirs = all_the_runs,
        #     dirheatmap = heatmapland,
        #     # ordering = c(1, 3, 4, 2),
        #     # ordering = c(1, 3, 4, 2),
        #     source_of_params = "params.yaml")
        #     all_the_names <- remakestring(all_the_runs, "_", ".")
        #     names(extractedmeans) <- all_the_names

        #     heatmapoutput <- list()

        #     heatmapoutput <- makeheatmapfile(
        #                     inheritance = 1, diffcurstartbias = "pop1",
        #                     biassize = 5, othersize = 2,
        #                     reversedruns = FALSE,
        #                     runstyle = "lowHigh", highres = FALSE,
        #                     extractedmeans = extractedmeans)

        #     individualfigures(2,5,heatmapoutput)
    ### Mother Inheritance: 5351-5400
    #   "*_535[1-9]_|*_53[6-9][0-9]_|*_5400_") ###

    #     extractedmeans <- extractmeans(
    #         allrundirs = all_the_runs,
    #         dirheatmap = heatmapland,
    #         # ordering = c(1, 3, 4, 2),
    #         # ordering = c(1, 3, 4, 2),
    #         source_of_params = "params.yaml")
    #         all_the_names <- remakestring(all_the_runs, "_", ".")
    #         names(extractedmeans) <- all_the_names

    #         heatmapoutput <- list()

    #         heatmapoutput <- makeheatmapfile(
    #                         inheritance = 2, diffcurstartbias = "pop1",
    #                         biassize = 5, othersize = 2,
    #                         reversedruns = FALSE,
    #                         runstyle = "lowHigh", highres = FALSE,
    #                         extractedmeans = extractedmeans)

    #         individualfigures(2,5,heatmapoutput)
    ### SamSex Inheritance: 5401-5450
    #   "*_540[1-9]_|*_54[1-4][0-9]_|*_5450_") ###

    #         extractedmeans <- extractmeans(
    #             allrundirs = all_the_runs,
    #             dirheatmap = heatmapland,
    #             # ordering = c(1, 3, 4, 2),
    #             # ordering = c(1, 3, 4, 2),
    #             source_of_params = "params.yaml")
    #             all_the_names <- remakestring(all_the_runs, "_", ".")
    #             names(extractedmeans) <- all_the_names

    #             heatmapoutput <- list()

    #             heatmapoutput <- makeheatmapfile(
    #                             inheritance = 3, diffcurstartbias = "pop1",
    #                             biassize = 5, othersize = 2,
    #                             reversedruns = FALSE,
    #                             runstyle = "lowHigh", highres = FALSE,
    #                             extractedmeans = extractedmeans)

    #             individualfigures(2,5,heatmapoutput)

    ### Mixed Inheritance: 5451-5500
    #   "*_545[1-9]_|*_54[6-9][0-9]_|*_5500_") ###

    #     extractedmeans <- extractmeans(
    #         allrundirs = all_the_runs,
    #         dirheatmap = heatmapland,
    #         # ordering = c(1, 3, 4, 2),
    #         # ordering = c(1, 3, 4, 2),
    #         source_of_params = "params.yaml")
    #         all_the_names <- remakestring(all_the_runs, "_", ".")
    #         names(extractedmeans) <- all_the_names

    #         heatmapoutput <- list()

    #         heatmapoutput <- makeheatmapfile(
    #                         inheritance = 11, diffcurstartbias = "pop1",
    #                         biassize = 5, othersize = 2,
    #                         reversedruns = FALSE,
    #                         runstyle = "lowHigh", highres = FALSE,
    #                         extractedmeans = extractedmeans)

    #         individualfigures(2,5,heatmapoutput)

# heatmapland <- file.path("results", "tenKfiveByFive_child-highMalSmolInv")
# all_the_runs <- extractvardirs(heatmapland,

  ### Invasion! 10k Mal_HL follow-up HIGH (Small Invasion Population Size) (biassize 5, othersize 2)
    ### Father Inheritance: 5701-5750
#       "*_570[1-9]_|*_57[1-4][0-9]_|*_5750_") ###

#         extractedmeans <- extractmeans(
#             allrundirs = all_the_runs,
#             dirheatmap = heatmapland,
#             # ordering = c(1, 3, 4, 2),
#             # ordering = c(1, 3, 4, 2),
#             source_of_params = "params.yaml")
#             all_the_names <- remakestring(all_the_runs, "_", ".")
#             names(extractedmeans) <- all_the_names

#             heatmapoutput <- list()

#             heatmapoutput <- makeheatmapfile(
#                             inheritance = 1, diffcurstartbias = "pop1",
#                             biassize = 5, othersize = 2,
#                             reversedruns = FALSE,
#                             runstyle = "lowHigh", highres = FALSE,
#                             extractedmeans = extractedmeans)

# individualfigures(2,5,heatmapoutput)

    ### Mother Inheritance: 5751-5800
    #   "*_575[1-9]_|*_57[6-9][0-9]_|*_5800_") ###

    #       extractedmeans <- extractmeans(
    #         allrundirs = all_the_runs,
    #         dirheatmap = heatmapland,
    #         # ordering = c(1, 3, 4, 2),
    #         # ordering = c(1, 3, 4, 2),
    #         source_of_params = "params.yaml")
    #         all_the_names <- remakestring(all_the_runs, "_", ".")
    #         names(extractedmeans) <- all_the_names

    #         heatmapoutput <- list()

    #         heatmapoutput <- makeheatmapfile(
    #                         inheritance = 2, diffcurstartbias = "pop1",
    #                         biassize = 5, othersize = 2,
    #                         reversedruns = FALSE,
    #                         runstyle = "lowHigh", highres = FALSE,
    #                         extractedmeans = extractedmeans)

    #         individualfigures(2,5,heatmapoutput)

    ### SamSex Inheritance: 5801-5850
    #   "*_580[1-9]_|*_58[1-4][0-9]_|*_5850_") ###

    #       extractedmeans <- extractmeans(
    #         allrundirs = all_the_runs,
    #         dirheatmap = heatmapland,
    #         # ordering = c(1, 3, 4, 2),
    #         # ordering = c(1, 3, 4, 2),
    #         source_of_params = "params.yaml")
    #         all_the_names <- remakestring(all_the_runs, "_", ".")
    #         names(extractedmeans) <- all_the_names

    #         heatmapoutput <- list()

    #         heatmapoutput <- makeheatmapfile(
    #                         inheritance = 3, diffcurstartbias = "pop1",
    #                         biassize = 5, othersize = 2,
    #                         reversedruns = FALSE,
    #                         runstyle = "lowHigh", highres = FALSE,
    #                         extractedmeans = extractedmeans)

    #         individualfigures(2,5,heatmapoutput)

    ### Miixed Inheritance: 5851-5900
    #   "*_585[1-9]_|*_58[6-9][0-9]_|*_5900_") ###

        # extractedmeans <- extractmeans(
        #     allrundirs = all_the_runs,
        #     dirheatmap = heatmapland,
        #     # ordering = c(1, 3, 4, 2),
        #     # ordering = c(1, 3, 4, 2),
        #     source_of_params = "params.yaml")
        #     all_the_names <- remakestring(all_the_runs, "_", ".")
        #     names(extractedmeans) <- all_the_names

        #     heatmapoutput <- list()

        #     heatmapoutput <- makeheatmapfile(
        #                     inheritance = 11, diffcurstartbias = "pop1",
        #                     biassize = 5, othersize = 2,
        #                     reversedruns = FALSE,
        #                     runstyle = "lowHigh", highres = FALSE,
        #                     extractedmeans = extractedmeans)

        #     individualfigures(2,5,heatmapoutput)

heatmapland <- file.path("results", "tenKfiveByFive_child-lowMalSmolInv")
all_the_runs <- extractvardirs(heatmapland,

  ### Invasion! 10k Mal_HL follow-up LOW (Small Invasion Population Size) (biassize 5, othersize 2)
    ### Father Inheritance: 5901-5950
    #   "*_590[1-9]_|*_59[1-4][0-9]_|*_5950_") ###

        # extractedmeans <- extractmeans(
        #     allrundirs = all_the_runs,
        #     dirheatmap = heatmapland,
        #     # ordering = c(1, 3, 4, 2),
        #     # ordering = c(1, 3, 4, 2),
        #     source_of_params = "params.yaml")
            # all_the_names <- remakestring(all_the_runs, "_", ".")
            # names(extractedmeans) <- all_the_names

            # heatmapoutput <- list()

            # heatmapoutput <- makeheatmapfile(
            #                 inheritance = 1, diffcurstartbias = "pop1",
            #                 biassize = 5, othersize = 2,
            #                 reversedruns = FALSE,
            #                 runstyle = "lowHigh", highres = FALSE,
            #                 extractedmeans = extractedmeans)

            # individualfigures(2,5,heatmapoutput)

    ### Mother Inheritance: 5951-6000
    #   "*_595[1-9]_|*_59[6-9][0-9]_|*_6000_") ###

        # extractedmeans <- extractmeans(
        #     allrundirs = all_the_runs,
        #     dirheatmap = heatmapland,
        #     # ordering = c(1, 3, 4, 2),
        #     # ordering = c(1, 3, 4, 2),
        #     source_of_params = "params.yaml")
            # all_the_names <- remakestring(all_the_runs, "_", ".")
            # names(extractedmeans) <- all_the_names

            # heatmapoutput <- list()

            # heatmapoutput <- makeheatmapfile(
            #                 inheritance = 2, diffcurstartbias = "pop1",
            #                 biassize = 5, othersize = 2,
            #                 reversedruns = FALSE,
            #                 runstyle = "lowHigh", highres = FALSE,
            #                 extractedmeans = extractedmeans)

            # individualfigures(2,5,heatmapoutput)

    ### SamSex Inheritance: 6001-6050
# all_the_runs <- extractvardirs(heatmapland,
#       "*_600[1-9]_|*_60[1-4][0-9]_|*_6050_") ###

#         extractedmeans <- extractmeans(
#             allrundirs = all_the_runs,
#             dirheatmap = heatmapland,
#             # ordering = c(1, 3, 4, 2),
#             # ordering = c(1, 3, 4, 2),
#             source_of_params = "params.yaml")
#             all_the_names <- remakestring(all_the_runs, "_", ".")
#             names(extractedmeans) <- all_the_names

#             heatmapoutput <- list()

#             heatmapoutput <- makeheatmapfile(
#                             inheritance = 3, diffcurstartbias = "pop1",
#                             biassize = 5, othersize = 2,
#                             reversedruns = FALSE,
#                             runstyle = "lowHigh", highres = FALSE,
#                             extractedmeans = extractedmeans)

#             individualfigures(2,5,heatmapoutput)

#     ### Miixed Inheritance: 6051-6100
#  all_the_runs <- extractvardirs(heatmapland,
#      "*_605[1-9]_|*_60[6-9][0-9]_|*_6100_") ###

#         extractedmeans <- extractmeans(
#             allrundirs = all_the_runs,
#             dirheatmap = heatmapland,
#             # ordering = c(1, 3, 4, 2),
#             # ordering = c(1, 3, 4, 2),
#             source_of_params = "params.yaml")
#             all_the_names <- remakestring(all_the_runs, "_", ".")
#             names(extractedmeans) <- all_the_names

#             heatmapoutput <- list()

#             heatmapoutput <- makeheatmapfile(
#                             inheritance = 11, diffcurstartbias = "pop1",
#                             biassize = 5, othersize = 2,
#                             reversedruns = FALSE,
#                             runstyle = "lowHigh", highres = FALSE,
#                             extractedmeans = extractedmeans)

#             individualfigures(2,5,heatmapoutput)


# heatmapland <- file.path("results", "tenKfiveByFive_child-highBothInv")
# all_the_runs <- extractvardirs(heatmapland,

#   ### Invasion! 10k Both_HL follow-up HIGH (biassize 5, othersize 2)
#     ### Father Inheritance: 5501-5550
#       "*_550[1-9]_|*_55[1-4][0-9]_|*_5550_") ###

#         extractedmeans <- extractmeans(
#             allrundirs = all_the_runs,
#             dirheatmap = heatmapland,
#             # ordering = c(1, 3, 4, 2),
#             # ordering = c(1, 3, 4, 2),
#             source_of_params = "params.yaml")
#             all_the_names <- remakestring(all_the_runs, "_", ".")
#             names(extractedmeans) <- all_the_names

#             heatmapoutput <- list()

#             heatmapoutput <- makeheatmapfile(
#                             inheritance = 1, diffcurstartbias = "pop1",
#                             biassize = 5, othersize = 2,
#                             reversedruns = FALSE,
#                             runstyle = "lowHigh", highres = FALSE,
#                             extractedmeans = extractedmeans)

#             individualfigures(2,5,heatmapoutput)

#     ### Mother Inheritance: 5351-5400
#       all_the_runs <- extractvardirs(heatmapland,
# "*_555[1-9]_|*_55[6-9][0-9]_|*_5600_") ###

#         extractedmeans <- extractmeans(
#             allrundirs = all_the_runs,
#             dirheatmap = heatmapland,
#             # ordering = c(1, 3, 4, 2),
#             # ordering = c(1, 3, 4, 2),
#             source_of_params = "params.yaml")
#             all_the_names <- remakestring(all_the_runs, "_", ".")
#             names(extractedmeans) <- all_the_names

#             heatmapoutput <- list()

#             heatmapoutput <- makeheatmapfile(
#                             inheritance = 2, diffcurstartbias = "pop1",
#                             biassize = 5, othersize = 2,
#                             reversedruns = FALSE,
#                             runstyle = "lowHigh", highres = FALSE,
#                             extractedmeans = extractedmeans)

#             individualfigures(2,5,heatmapoutput)

#     ### SamSex Inheritance: 5601-5650
# all_the_runs <- extractvardirs(heatmapland,
#       "*_560[1-9]_|*_56[1-4][0-9]_|*_5650_") ###

#         extractedmeans <- extractmeans(
#             allrundirs = all_the_runs,
#             dirheatmap = heatmapland,
#             # ordering = c(1, 3, 4, 2),
#             # ordering = c(1, 3, 4, 2),
#             source_of_params = "params.yaml")
#             all_the_names <- remakestring(all_the_runs, "_", ".")
#             names(extractedmeans) <- all_the_names

#             heatmapoutput <- list()

#             heatmapoutput <- makeheatmapfile(
#                             inheritance = 3, diffcurstartbias = "pop1",
#                             biassize = 5, othersize = 2,
#                             reversedruns = FALSE,
#                             runstyle = "lowHigh", highres = FALSE,
#                             extractedmeans = extractedmeans)

#             individualfigures(2,5,heatmapoutput)

#     ### Miixed Inheritance: 5651-5700
# all_the_runs <- extractvardirs(heatmapland,
#       "*_565[1-9]_|*_56[6-9][0-9]_|*_5700_") ###

#         extractedmeans <- extractmeans(
#             allrundirs = all_the_runs,
#             dirheatmap = heatmapland,
#             # ordering = c(1, 3, 4, 2),
#             # ordering = c(1, 3, 4, 2),
#             source_of_params = "params.yaml")
#             all_the_names <- remakestring(all_the_runs, "_", ".")
#             names(extractedmeans) <- all_the_names

#             heatmapoutput <- list()

#             heatmapoutput <- makeheatmapfile(
#                             inheritance = 11, diffcurstartbias = "pop1",
#                             biassize = 5, othersize = 2,
#                             reversedruns = FALSE,
#                             runstyle = "lowHigh", highres = FALSE,
#                             extractedmeans = extractedmeans)

#             individualfigures(2,5,heatmapoutput)


# heatmapland <- file.path("results", "tenKfiveByFive_49-51s - bothHigh?")
# all_the_runs <- extractvardirs(heatmapland,

# ### Invasion! 10k Both_HL follow-up HIGH (biassize 5, othersize 2)
#       ### Father Inheritance: 4901-4950
#         "*_490[1-9]_|*_49[1-4][0-9]_|*_4950_") ###

#           extractedmeans <- extractmeans(
#             allrundirs = all_the_runs,
#             dirheatmap = heatmapland,
#             # ordering = c(1, 3, 4, 2),
#             # ordering = c(1, 3, 4, 2),
#             source_of_params = "params.yaml")
#             all_the_names <- remakestring(all_the_runs, "_", ".")
#             names(extractedmeans) <- all_the_names

#             heatmapoutput <- list()

#             heatmapoutput <- makeheatmapfile(
#                             inheritance = 1, diffcurstartbias = "pop1",
#                             biassize = 5, othersize = 2,
#                             reversedruns = FALSE,
#                             runstyle = "lowHigh", highres = FALSE,
#                             extractedmeans = extractedmeans)

#             individualfigures(2,5,heatmapoutput)

#       ### Mother Inheritance: 4951-5000
# all_the_runs <- extractvardirs(heatmapland,
#         "*_495[1-9]_|*_49[6-9][0-9]_|*_5000_") ###

#           extractedmeans <- extractmeans(
#             allrundirs = all_the_runs,
#             dirheatmap = heatmapland,
#             # ordering = c(1, 3, 4, 2),
#             # ordering = c(1, 3, 4, 2),
#             source_of_params = "params.yaml")
#             all_the_names <- remakestring(all_the_runs, "_", ".")
#             names(extractedmeans) <- all_the_names

#             heatmapoutput <- list()

#             heatmapoutput <- makeheatmapfile(
#                             inheritance = 2, diffcurstartbias = "pop1",
#                             biassize = 5, othersize = 2,
#                             reversedruns = FALSE,
#                             runstyle = "lowHigh", highres = FALSE,
#                             extractedmeans = extractedmeans)

#             individualfigures(2,5,heatmapoutput)

#       ### SamSex Inheritance: 5001-5050
# all_the_runs <- extractvardirs(heatmapland,
#         "*_500[1-9]_|*_50[1-4][0-9]_|*_5050_") ###

#           extractedmeans <- extractmeans(
#             allrundirs = all_the_runs,
#             dirheatmap = heatmapland,
#             # ordering = c(1, 3, 4, 2),
#             # ordering = c(1, 3, 4, 2),
#             source_of_params = "params.yaml")
#             all_the_names <- remakestring(all_the_runs, "_", ".")
#             names(extractedmeans) <- all_the_names

#             heatmapoutput <- list()

#             heatmapoutput <- makeheatmapfile(
#                             inheritance = 3, diffcurstartbias = "pop1",
#                             biassize = 5, othersize = 2,
#                             reversedruns = FALSE,
#                             runstyle = "lowHigh", highres = FALSE,
#                             extractedmeans = extractedmeans)

#             individualfigures(2,5,heatmapoutput)

#       ### Miixed Inheritance: 5051-5100
# all_the_runs <- extractvardirs(heatmapland,
#         "*_505[1-9]_|*_50[6-9][0-9]_|*_5100_") ###

#           extractedmeans <- extractmeans(
#             allrundirs = all_the_runs,
#             dirheatmap = heatmapland,
#             # ordering = c(1, 3, 4, 2),
#             # ordering = c(1, 3, 4, 2),
#             source_of_params = "params.yaml")
#             all_the_names <- remakestring(all_the_runs, "_", ".")
#             names(extractedmeans) <- all_the_names

#             heatmapoutput <- list()

#             heatmapoutput <- makeheatmapfile(
#                             inheritance = 11, diffcurstartbias = "pop1",
#                             biassize = 5, othersize = 2,
#                             reversedruns = FALSE,
#                             runstyle = "lowHigh", highres = FALSE,
#                             extractedmeans = extractedmeans)

#             individualfigures(2,5,heatmapoutput)


# heatmapland <- file.path("results", "tenKfiveByFive_child-highFemSmolInv")
# all_the_runs <- extractvardirs(heatmapland,

#   ### Invasion! 10k Fem_HL follow-up HIGH (Small Invasion Population Size) (biassize 5, othersize 2)
#     ### Father Inheritance: 6101-6150
#       "*_610[1-9]_|*_61[1-4][0-9]_|*_6150_") ###

#         extractedmeans <- extractmeans(
#             allrundirs = all_the_runs,
#             dirheatmap = heatmapland,
#             # ordering = c(1, 3, 4, 2),
#             # ordering = c(1, 3, 4, 2),
#             source_of_params = "params.yaml")
#             all_the_names <- remakestring(all_the_runs, "_", ".")
#             names(extractedmeans) <- all_the_names

#             heatmapoutput <- list()

#             heatmapoutput <- makeheatmapfile(
#                             inheritance = 1, diffcurstartbias = "pop1",
#                             biassize = 5, othersize = 2,
#                             reversedruns = FALSE,
#                             runstyle = "lowHigh", highres = FALSE,
#                             extractedmeans = extractedmeans)

#             individualfigures(2,5,heatmapoutput)

#     ### Mother Inheritance: 6151-6200
# all_the_runs <- extractvardirs(heatmapland,
#       "*_615[1-9]_|*_61[6-9][0-9]_|*_6200_") ###

#         extractedmeans <- extractmeans(
#             allrundirs = all_the_runs,
#             dirheatmap = heatmapland,
#             # ordering = c(1, 3, 4, 2),
#             # ordering = c(1, 3, 4, 2),
#             source_of_params = "params.yaml")
#             all_the_names <- remakestring(all_the_runs, "_", ".")
#             names(extractedmeans) <- all_the_names

#             heatmapoutput <- list()

#             heatmapoutput <- makeheatmapfile(
#                             inheritance = 2, diffcurstartbias = "pop1",
#                             biassize = 5, othersize = 2,
#                             reversedruns = FALSE,
#                             runstyle = "lowHigh", highres = FALSE,
#                             extractedmeans = extractedmeans)

#             individualfigures(2,5,heatmapoutput)

#     ### SamSex Inheritance: 6201-6250
# all_the_runs <- extractvardirs(heatmapland,
#       "*_620[1-9]_|*_62[1-4][0-9]_|*_6250_") ###

#         extractedmeans <- extractmeans(
#             allrundirs = all_the_runs,
#             dirheatmap = heatmapland,
#             # ordering = c(1, 3, 4, 2),
#             # ordering = c(1, 3, 4, 2),
#             source_of_params = "params.yaml")
#             all_the_names <- remakestring(all_the_runs, "_", ".")
#             names(extractedmeans) <- all_the_names

#             heatmapoutput <- list()

#             heatmapoutput <- makeheatmapfile(
#                             inheritance = 3, diffcurstartbias = "pop1",
#                             biassize = 5, othersize = 2,
#                             reversedruns = FALSE,
#                             runstyle = "lowHigh", highres = FALSE,
#                             extractedmeans = extractedmeans)

#             individualfigures(2,5,heatmapoutput)

#     ### Miixed Inheritance: 6251-6300
# all_the_runs <- extractvardirs(heatmapland,
#       "*_625[1-9]_|*_62[6-9][0-9]_|*_6300_") ###

#         extractedmeans <- extractmeans(
#             allrundirs = all_the_runs,
#             dirheatmap = heatmapland,
#             # ordering = c(1, 3, 4, 2),
#             # ordering = c(1, 3, 4, 2),
#             source_of_params = "params.yaml")
            # all_the_names <- remakestring(all_the_runs, "_", ".")
            # names(extractedmeans) <- all_the_names

            # heatmapoutput <- list()

            # heatmapoutput <- makeheatmapfile(
            #                 inheritance = 11, diffcurstartbias = "pop1",
            #                 biassize = 5, othersize = 2,
            #                 reversedruns = FALSE,
            #                 runstyle = "lowHigh", highres = FALSE,
            #                 extractedmeans = extractedmeans)

            # individualfigures(2,5,heatmapoutput)


heatmapland <- file.path("results", "tenKfiveByFive_child-lowFemSmolInv")
all_the_runs <- extractvardirs(heatmapland,

  ### Invasion! 10k Fem_HL follow-up LOW (Small Invasion Population Size) (biassize 5, othersize 2)
    ### Father Inheritance: 6301-6350
      "*_630[1-9]_|*_63[1-4][0-9]_|*_6350_") ###

        extractedmeans <- extractmeans(
            allrundirs = all_the_runs,
            dirheatmap = heatmapland,
            # ordering = c(1, 3, 4, 2),
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

    ### Mother Inheritance: 6351-6400
all_the_runs <- extractvardirs(heatmapland,
      "*_635[1-9]_|*_63[6-9][0-9]_|*_6400_") ###

        extractedmeans <- extractmeans(
            allrundirs = all_the_runs,
            dirheatmap = heatmapland,
            # ordering = c(1, 3, 4, 2),
            # ordering = c(1, 3, 4, 2),
            source_of_params = "params.yaml")
            all_the_names <- remakestring(all_the_runs, "_", ".")
            names(extractedmeans) <- all_the_names

            heatmapoutput <- list()

            heatmapoutput <- makeheatmapfile(
                            inheritance = 2, diffcurstartbias = "pop1",
                            biassize = 5, othersize = 2,
                            reversedruns = FALSE,
                            runstyle = "lowHigh", highres = FALSE,
                            extractedmeans = extractedmeans)

            individualfigures(2,5,heatmapoutput)

    ### SamSex Inheritance: 6401-6450
all_the_runs <- extractvardirs(heatmapland,
      "*_640[1-9]_|*_64[1-4][0-9]_|*_6450_") ###

        extractedmeans <- extractmeans(
            allrundirs = all_the_runs,
            dirheatmap = heatmapland,
            # ordering = c(1, 3, 4, 2),
            # ordering = c(1, 3, 4, 2),
            source_of_params = "params.yaml")
            all_the_names <- remakestring(all_the_runs, "_", ".")
            names(extractedmeans) <- all_the_names

            heatmapoutput <- list()

            heatmapoutput <- makeheatmapfile(
                            inheritance = 3, diffcurstartbias = "pop1",
                            biassize = 5, othersize = 2,
                            reversedruns = FALSE,
                            runstyle = "lowHigh", highres = FALSE,
                            extractedmeans = extractedmeans)

            individualfigures(2,5,heatmapoutput)

    ### Miixed Inheritance: 6451-6500
all_the_runs <- extractvardirs(heatmapland,
      "*_645[1-9]_|*_64[6-9][0-9]_|*_6500_") ###

        extractedmeans <- extractmeans(
        allrundirs = all_the_runs,
        dirheatmap = heatmapland,
        # ordering = c(1, 3, 4, 2),
        # ordering = c(1, 3, 4, 2),
        source_of_params = "params.yaml")
        all_the_names <- remakestring(all_the_runs, "_", ".")
        names(extractedmeans) <- all_the_names

        heatmapoutput <- list()

        heatmapoutput <- makeheatmapfile(
                        inheritance = 11, diffcurstartbias = "pop1",
                        biassize = 5, othersize = 2,
                        reversedruns = FALSE,
                        runstyle = "lowHigh", highres = FALSE,
                        extractedmeans = extractedmeans)

        individualfigures(2,5,heatmapoutput)


#   extractedmeans <- extractmeans(
#   allrundirs = all_the_runs,
#   dirheatmap = heatmapland,
#   # ordering = c(1, 3, 4, 2),
#   # ordering = c(1, 3, 4, 2),
#   source_of_params = "params.yaml")
# all_the_names <- remakestring(all_the_runs, "_", ".")
# names(extractedmeans) <- all_the_names

# heatmapoutput <- list()

# heatmapoutput <- makeheatmapfile(
#                 inheritance = 1, diffcurstartbias = "pop1",
#                 biassize = 5, othersize = 2,
#                 reversedruns = FALSE,
#                 runstyle = "lowHigh", highres = FALSE,
#                 extractedmeans = extractedmeans)

# individualfigures(2,5,heatmapoutput)