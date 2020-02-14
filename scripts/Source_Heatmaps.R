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






# Source the Functions

source(file.path("scripts", "Source_Heatmap_Functions.R"))
source(file.path("scripts", "Source_Difference_Heatmaps.R"))
source(file.path("scripts", "Source_Variance_Heatmaps.R"))

############## # # ARRANGEMENT OF FUNCTIONS  # # ##############

whatevers <- list.files (file.path ("results"), pattern = "tenK")

heatmapland <- file.path("results")

# heatmapland <- file.path("results", "Heatmaps")

# all_the_runs <- list.files(heatmapland,

# somethingSomething <- list(
#   run_names = c("ParentNoInv", "childF1NoInv", "childMalHighCurInv", "childMalLowCurInv", "childFemLowCurInv", "childBothLowCurInv", "childFemHighCurInv", "childBothHighCurInv", "childSmolMalHighCurInv", "childSmolMalLowCurInv", "childSmolFemHighCurInv", "childSmolFemLowCurInv", "childF2NoInv", "childF3NoInv", "childF4NoInv", "childF5NoInv", "childF6NoInv", "childF7NoInv", "childF8NoInv", "childF9NoInv", "childF10NoInv"),
#   run_numbers = c("3901-4100", "4101-4300", "4301-4500", "4501-4700", "4701-4900", "5101-5300", "5301-5500", "5501-5700", "5701-5900", "5901-6100", "6101-6300", "6301-6500", "6501-6700", "6701-6900", "6901-7100", "7101-7300", "7301-7500", "7501-7700", "7701-7900", "7901-8100", "8101-8300")
# )

# THIS ARRAY IS THE FEEDER FOR THE FOR LOOP. EVERYTHING FOR ROWS 7 AND UNDER IS DONE; 8 IS READY TO FINISH
somethingSomething <- array(c(
  "parentNoInv",        "childF1NoInv",       "childMalHihInv",  "childMalLowInv",     "childFemLowInv",
  "childBothLowInv",    "childFemHihInv",     "childBothHihInv", "childSmolMalHihInv", "childSmolMalLowInv",
  "childSmolFemHihInv", "childSmolFemLowInv", "childNoInvF2",    "childNoInvF3",       "childNoInvF4",
  "childNoInvF5",       "childNoInvF6",       "childNoInvF7",    "childNoInvF8",       "childNoInvF9",
  "childNoInvF10",
  3901, 4101, 4301, 4501, 4701,
  5101, 5301, 5501, 5701, 5901,
  6101, 6301, 6501, 6701, 6901,
  7101, 7301, 7501, 7701, 7901,
  8101), c(21,2)
)

# subsetsSomethingSomething <- somethingSomething$run_numbers[Nth]

# SEE? NUMBER 8.
for (run in 1:length (somethingSomething[,1])) {
  if (!(dir.exists(file.path(heatmapland, paste0("tenKfiveByFive_", somethingSomething[run,1]))))) {
    stop (paste0("Simulation ", somethingSomething[run,1], " is not in the directory. Stopping heatmap processing."))
  }
  specific_folder <- which(whatevers == paste0 ("tenKfiveByFive_", somethingSomething[run,1]))
  placeholder <- as.numeric  (somethingSomething[run,2])
  object_converter <- c(paste0(as.character (placeholder), "-", as.character (placeholder + 49)),
                        paste0(as.character (placeholder + 50), "-", as.character (placeholder + 99)),
                        paste0(as.character (placeholder + 100), "-", as.character (placeholder + 149)),
                        paste0(as.character (placeholder + 150), "-", as.character (placeholder + 199))
                        )
  inheritance_converter <- c(1,2,3,11)
  for (subset in 1:4) {
    temp_heatmapland <- file.path(heatmapland, whatevers[specific_folder])
    all_the_runs <- extractvardirs(temp_heatmapland, object_converter[subset])

    extractedmeans <- extractmeans(allrundirs = all_the_runs,
                                   dirheatmap = temp_heatmapland,
                                   source_of_params = "params.yaml",
                                   ordering = FALSE,
                                   deeper = FALSE
                                  )

    all_the_names <- remakestring(all_the_runs, "_", ".")

    names(extractedmeans) <- all_the_names

    heatmapoutput <- makeheatmapfile(
                    output_foldername = paste0 ("five-by-five-", somethingSomething[run,1]),
                    inheritance = inheritance_converter[subset],
                    diffcurstartbias = "pop1",
                    biassize = 5,
                    othersize = 2,
                    reversedruns = FALSE,
                    runstyle = "lowHigh",
                    highres = FALSE,
                    extractedmeans = extractedmeans)

    individualfigures(
      output_foldername = paste0 ("five-by-five-", somethingSomething[run,1]),
      colorrange = 2,
      colorpalette = "five_by_five",
      input_list = heatmapoutput,
      midpoint_size = 1,
      variance_treatment = FALSE
    )
  }

  differenceheatmaps(new_runs_to_compare = somethingSomething[run,1], guide = somethingSomething)
  varianceheatmaps(list_of_sims = somethingSomething[,1], sim_in_question = run)
}

