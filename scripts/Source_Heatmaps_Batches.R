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
source(file.path("scripts", "Source_Batch_Heatmap_Functions.R"))

thing <- c("male", "moth", "same", "FfFf")
# das_dinge <- c(1,2,3,11)
stuff <- c("five-by-five-Inv3k_lowHigh_Background",
           "five-by-five-vanilla_lowHigh_Background",
           "five-by-five-followUpVanilla_lowHigh_Background",
           "five-by-five-followUpInvLow1k_lowHigh_Background",
           "five-by-five-followUpInvHigh1k_lowHigh_Background",
           "five-by-five-followUpFemInvHigh1k_lowHigh_Background"#,
          #  "tenKfiveByFive_child-lowFemInvtK/five-by-five-followUpFemHigh1k_lowHigh_Background",
         #  "",)
)

# stuff_n_things <- array (c (1, 1, 1, 2, 3, 3, 4, 2, 3, 5, 3, 4, 5, 5), c (7,2))
stuff_n_things <- array (c (1, 2, 3, 4, 5, 6, 6, 6, 6, 6), c (5,2))

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

# plot(x[,1], x[,2], col = colorseqmultpalette[[19]](100), pch = 15, cex = 5, axes = F, xlab = "", ylab = "")
# plot(x[,1], x[,2], col = colorseqmultpalette[[19]](100), pch = 15, cex = 5, axes = F, xlab = "", ylab = "")
# axis(1, c(0, 0.1, 1, 10, 100), c("0", "0.1", "1", "10", "100"),T,0,NA,F,cex.axis = 0.8, tck = 0)


plot_that_shit <- function () {

plot(x[,1], x[,2], col = colorseqmultpalette[[19]](100), pch = 15, cex = 1, axes = F, xlab = "", ylab = "")
# axis(1, c(-2.5, 4.5, 11.5, 18.5, 25.5, 32.5, 39.5, 46.5, 48.5, 55.5, 62.5, 69.5, 76.5, 83.5, 90.5, 97.5, 103.5), c("-1", "-0.86", "-0.72", "-0.58", "-0.44", "-0.3", "-0.16", "-0.08", "-0.02", "0.02", "0.16", "0.3", "0.44", "0.58", "0.72", "0.86", "1"),T,-11.5,NA,F,cex.axis = 0.7, tck = 0.015)
axis(1, c(0.5, 7.5, 14.5, 21.5, 28.5, 35.5, 42.5, 49.5, 51.5, 58.5, 65.5, 72.5, 79.5, 86.5, 93.5, 100.5), c("-1", "-0.86", "-0.72", "-0.58", "-0.44", "-0.3", "-0.16", "-0.02", "0.02", "0.16", "0.3", "0.44", "0.58", "0.72", "0.86", "1"),T,-11.5,NA,F,cex.axis = 0.7, tck = -0.015)
axis(1, c(50), c(""), )
# axis(1, c(1, 8, 15, 22, 29, 36, 43, 47.5, 52, 59, 66, 73, 80, 87, 94, 100.5), c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""),T,-10,NA,F,cex.axis = 0.8, tck = 0) # Curiosity Range
axis(1, c(4.5, 11.5, 18.5, 25.5, 32.5, 39.5, 46.5, 48.5, 55.5, 62.5, 69.5, 76.5, 83.5, 90.5, 97.5), c("", "", "", "", "", "", "-1", "1", "", "", "", "", "", "", ""),T,-10,NA,F,cex.axis = 0.3, tck = 0) # Syllable Range

}

c(-2.5, 4.5, 11.5, 18.5, 25.5, 32.5, 39.5, 46.5, 48.5, 55.5, 62.5, 69.5, 76.5, 83.5, 90.5, 97.5, 103.5)
length(c(1, 8, 15, 22, 29, 36, 43, 47.5, 52, 59, 66, 73, 80, 87, 94, 100.5))


