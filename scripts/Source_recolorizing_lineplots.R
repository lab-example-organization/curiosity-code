
source(file.path("scripts", "Source_Reference_Section.R"))
referencesection("parallelHeatmaps")

source(file.path("scripts", "Source_Figure_Produxn_Multiple_Runs.R"))

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
  10101,
  "paramsparentNoInv", "paramschildNoInvF1",
    "paramsInvMalHighHrTenK", "paramsInvMalLowHrTenK",
    "paramsInvFemLowHrTenK", "paramsInvBothLowHrTenK",
    "paramsInvFemHighHrTenK", "paramsInvBothHighHrTenK",
    "paramsSmolInvMalHighHrTenK", "paramsSmolInvMalLowHrTenK",
    "paramsSmolInvFemHighHrTenK", "paramsSmolInvFemLowHrTenK",
    "paramschildNoInvF2", "paramschildNoInvF3", "paramschildNoInvF4",
    "paramschildNoInvF5", "paramschildNoInvF6", "paramschildNoInvF7",
    "paramschildNoInvF8", "paramschildNoInvF9", "paramschildNoInvF10",
    "paramsLateInvMalHighHrTenK", "paramsLateInvMalLowHrTenK",
    "paramsLateInvFemHighHrTenK", "paramsLateInvFemLowHrTenK",
    "paramsLateInvBothHighHrTenK", "paramsLateInvBothLowHrTenK",
    "paramsLateSmolInvMalHighHrTenK", "paramsLateSmolInvMalLowHrTenK",
    "paramsLateSmolInvFemHighHrTenK", "paramsLateSmolInvFemLowHrTenK"), c(31,3)
)

n_cores <- 4
z_specificsimnumber <- 3901:4100
new_folder <- "recolorizedCurMeans_rangeMedian"
simple_setup <- function (z_specificsimnumber, somethingSomething, recolorize = TRUE, number_of_repeats = 50, fdsa = new_folder) {#, temp_something) {
  if (z_specificsimnumber >= 4901 && z_specificsimnumber <= 5100) {return ("done")}
  something_subset <- which(as.numeric(somethingSomething[,2]) <= z_specificsimnumber)[length(which(as.numeric(somethingSomething[,2]) <= z_specificsimnumber))]
  folder_200_k <- somethingSomething[which(as.numeric(somethingSomething[,2]) <= z_specificsimnumber)[length(which(as.numeric(somethingSomething[,2]) <= z_specificsimnumber))]]
  param_thing <- list.files (file.path ("parameters"), pattern = paste0(somethingSomething[something_subset,3], ".yaml"))

  figprodmultrun(specificsimnumber = z_specificsimnumber, number_of_repeats = number_of_repeats,
  paramssource = param_thing, redo = TRUE, recolorize = recolorize, results_dir = folder_200_k, lineplots = fdsa, curMeans_only = TRUE, recolorize_style = "range-median") # variance, variance-median, range-biased, ###NOTdoneYET### clustering

  return ("done")
}

mclapply(z_specificsimnumber,
         simple_setup,
         somethingSomething = somethingSomething,
         mc.cores = n_cores)


tenKs <- list.files (file.path ("results"), pattern = "tenK")
dir.create (file.path("results", new_folder))


for (twoHundies in 1 : length (tenKs)) {
  indSims <- list.files (file.path ("results", tenKs [twoHundies]))
  if (
    ! (dir.exists (file.path ("results", new_folder, tenKs [twoHundies])))
  ) {dir.create (file.path ("results", new_folder, tenKs [twoHundies]))}
  for (recolorized_figs in 1 : length (indSims)) {
    recolorized_fig_list <- list.files (file.path ("results", tenKs [twoHundies], indSims [recolorized_figs], "multirun_output", new_folder), pattern = "mean_curiosity")
    for (whatever in 1 : length (recolorized_fig_list)) {
      file.copy (
        file.path ("results", tenKs [twoHundies], indSims [recolorized_figs], "multirun_output", new_folder, recolorized_fig_list [whatever]),
        file.path ("results", new_folder, tenKs [twoHundies])
      )
    }
  }
}








# temp_data_sing_selection <- update_selexn_data (
#               main_parameters = parameters_sing_selection,
#               temp_data_update_selexndata = temp_data_sing_selection,
#       ###     suitor_choices = selection.index,
#       ###     preferred_bird = singer,
#       ###     selector_bird = selector.index,
#               curiosity_value = curiosity_level,
#               selector_population = population,
#               selection_context = select_type,
#       ###     sylreps_choices = selection.sylreps,
#       ###     sylrep_selector = selector.sylrep,
#               selection_count = chance_for_selection



      ||||| selection.index ||||| auto.teachers [1,]
      ||||| singer ||||| MTsylrep_filter
      ||||| selector.index ||||| auto.teachers [2,MTsylrep_filter]
      ||||| selection.sylreps ||||| sylrep_object [auto.teachers [1,],,population]
      ||||| selector.sylrep ||||| sylrep_object [auto.teachers [2,MTsylrep_filter],,population]




      selector.index <- sample (parameters_sing_selection$pop_calls_matrix [2, ], 1)

      selection.index <- sample (parameters_sing_selection$pop_calls_matrix [1,], parameters_sing_selection$one_pop_singers [1])
      selection.sylreps <- cpp_rowSums (sylrep_object [parameters_sing_selection$pop_calls_matrix [1,],,1])[selection.index]
      # bigSylrep <- max(cpp_rowSums (sylrep_object[parameters_sing_selection$pop_calls_matrix [1,],,1])[selection.index])
      if (length (which (selection.sylreps == max (selection.sylreps))) > 1) {
        singer <- selection.index [which (selection.sylreps == max (selection.sylreps)) [sample (c (1 : length (which (selection.sylreps == max (selection.sylreps)))), 1)]]
      } else if (length (which (selection.sylreps == max (selection.sylreps))) == 1) {
        singer <- selection.index [which (selection.sylreps == max (selection.sylreps))]
      } else {stop ("max sylrep selection problem")}
      selector.sylrep <- "sssssssuper doesn't matter for syllable repertoire"




# mate_selection_type = "repertoire_size"




