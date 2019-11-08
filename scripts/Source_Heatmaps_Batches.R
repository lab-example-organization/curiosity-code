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