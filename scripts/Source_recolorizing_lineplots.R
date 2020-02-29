
source(file.path("scripts", "Source_Reference_Section.R"))
referencesection("heatmaps")

source(file.path("scripts", "Source_Figure_Produxn_Multiple_Runs.R"))

whatevers <- list.files (file.path ("results"), pattern = "tenK")

resultsland <- file.path("results")

somethingSomething <- array(c(
  "parentNoInv",        "childF1NoInv",       "childMalHihInv",  "childMalLowInv",     "childFemLowInv",
  "childBothLowInv",    "childFemHihInv",     "childBothHihInv", "childSmolMalHihInv", "childSmolMalLowInv",
  "childSmolFemHihInv", "childSmolFemLowInv", "childNoInvF2",    "childNoInvF3",       "childNoInvF4",
  "childNoInvF5",       "childNoInvF6",       "childNoInvF7",    "childNoInvF8",       "childNoInvF9",
  "childNoInvF10", "childLateInvMalHih", "childLateInvMalLow", "childLateInvFemHih", "childLateInvFemLow",
  "childLateInvBothHih", "childLateInvBothLow", "childLateSmolInvMalHih", "childLateSmolInvMalLow",  "childLateSmolInvFemHih",
   "childLateSmolInvFemLow",
  3901, 4101, 4301, 4501, 4701,
  5101, 5301, 5501, 5701, 5901,
  6101, 6301, 6501, 6701, 6901,
  7101, 7301, 7501, 7701, 7901,
  8101, 8301, 8501, 8701, 8901,
  9101, 9301, 9501, 9701, 9901,
  10101), c(31,2)
)

for (z_specificsimnumber in 3901:10300) {
  figprodmultrun(specificsimnumber = z_specificsimnumber, number_of_repeats = 50, paramssource = c("paramsSmolInvFemHighHrTenK.yaml"), redo = TRUE)
}


# specificsimnumber = 1, number_of_repeats,
# paramssource = paramssource, redo = FALSE

# paramsfile <- c("paramsSmolInvFemHighHrTenK.yaml")
# params <- yaml.load_file (file.path ("parameters", paramssource))
