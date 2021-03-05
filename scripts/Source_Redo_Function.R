rm(list=objects())
# redo figures
source(file.path("scripts", "Source_Reference_Section.R"))
referencesection("multirun")

paramssource = "paramsparentNoInv"
params <- yaml.load_file (file.path ("parameters", paste0(paramssource, ".yaml")))
number_of_reps <- as.numeric (params$number_of_reps)

    specificsimnumber = 4000
    number_of_repeats = number_of_reps
    paramssource = paramssource
    redo = FALSE
    recolorize = TRUE
    results_dir = "parentNoInv"
    lineplots = TRUE
    curMeans_only = FALSE
    absolute_y = TRUE
    recolorize_style = "range-median"


#   figprodmultrun <- function (
#       specificsimnumber = 1,
#       number_of_repeats,
#       paramssource = paramssource,
#       redo = FALSE,
#       recolorize = FALSE,
#       results_dir = FALSE,
#       lineplots = FALSE,
#       curMeans_only = FALSE,
#       absolute_y = TRUE,
#       recolorize_style = "variance")

results_table <- array(c(
  "parentNoInv",        "childF1NoInv",       "childMalHihInv",  "childMalLowInv",     "childFemLowInv",
  "childBothLowInv",    "childFemHihInv",     "childBothHihInv", "childSmolMalHihInv", "childSmolMalLowInv",
  "childSmolFemHihInv", "childSmolFemLowInv", "childNoInvF2",    "childNoInvF3",       "childNoInvF4",
  "childNoInvF5",       "childNoInvF6",       "childNoInvF7",    "childNoInvF8",       "childNoInvF9",
  "childNoInvF10", "childLateInvMalHih", "childLateInvMalLow", "childLateInvFemHih", "childLateInvFemLow",
  "childLateInvBothHih", "childLateInvBothLow", "childLateSmolInvMalHih", "childLateSmolInvMalLow",  "childLateSmolInvFemHih",
  "childLateSmolInvFemLow", "parentNoInvSylRepSize", "parentNoInvSylRepNoRoundUp", "parentNoInvNoRoundUp",
  3901, 4101, 4301, 4501, 4701,
  5101, 5301, 5501, 5701, 5901,
  6101, 6301, 6501, 6701, 6901,
  7101, 7301, 7501, 7701, 7901,
  8101, 8301, 8501, 8701, 8901,
  9101, 9301, 9501, 9701, 9901,
  10101, 10301, 10501, 10701,
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
  "paramsLateSmolInvFemHighHrTenK", "paramsLateSmolInvFemLowHrTenK",
  "paramsparentNoInvSylRep", "paramsparentNoInvSylRepNoRoundUp", "paramsparentNoInvNoRoundUp"), c(34,3)
)

graph_settings <- par (no.readonly = TRUE)
par_plot <- function() {
  par(cex.lab = 1.5, cex.main = 2)
  thing <- plot(1:5)
  dev.off()
  return(thing)
}
par(cex.lab = 1.5, cex.main = 2)

dev.off()

load_sim_for_plots <- function (
    simnumber_lsfp = 3901,
    results_dir_lsfp = "parentNoInv",
    params_file_lsfp = "paramsparentNoInv",

) {

}

recolorize_style = recolorize_style
parameters = params
plot_info = plot_info
number_of_runs = number_of_repeats
cursitylist = cursitylist
sdstbxnlist = sdstbxnlist
curhistlist = curhistlist
sylrepzlist = sylrepzlist
mins_n_maxes = mins_n_maxes
saving_dir = multirun_directory
lineplots = lineplots
curMeans_only = curMeans_only
absolute_y = absolute_y



parameters = parameters
number_of_runs = number_of_runs
population = population
cursitylist = cursitylist
plot_info = plot_info
mins_n_maxes = mins_n_maxes
saving_dir = saving_dir
recolorize = recolorize
lineplots = lineplots