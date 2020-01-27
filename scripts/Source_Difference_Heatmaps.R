
source(file.path("scripts", "Source_Reference_Section.R"))
referencesection("heatmaps")

source(file.path("scripts", "Source_Heatmap_Functions.R"))
source(file.path("scripts", "Source_Batch_Heatmap_Functions.R"))

differenceheatmaps <- function (
  new_runs_to_compare = "childF4NoInv"
) {

  thing <- c("male", "moth", "same", "FfFf")
  # das_dinge <- c(1,2,3,11)
  # stuff <- c("five-by-five-Inv3k_lowHigh_Background",
  #            "five-by-five-vanilla_lowHigh_Background",
  #            "five-by-five-followUpVanilla_lowHigh_Background",
  #            "five-by-five-followUpInvLow1k_lowHigh_Background",
  #            "five-by-five-followUpInvHigh1k_lowHigh_Background",
  #            "five-by-five-followUpFemInvLow1k_lowHigh_Background",
  #            "five-by-five-followUpBothInvLow1k_lowHigh_Background",
  #            "",
  #           #  "tenKfiveByFive_child-lowFemInvtK/five-by-five-followUpFemHigh1k_lowHigh_Background",
  #          #  "",)
  # )

  newly_finished_run <- c()

  # stuff <- c("parentNoInv", "childF1NoInv", "childMalHihInv", "childMalLowInv",
  #           "childFemLowInv", "childBothLowInv", "childFemHihInv", "childBothHihInv",
  #           "childSmolMalHihInv", "childSmolMalLowInv", "childSmolFemHihInv", "childSmolFemLowInv",
  #           "childF2NoInv", "childF3NoInv", "childF4NoInv", "childF5NoInv",
  #           "childF6NoInv", "childF7NoInv", "childF8NoInv", "childF9NoInv",
  #           "childF10NoInv")

  stuff <- list.files (file.path ("results"), pattern = "five-by-five")

  for (i in 1 : length (new_runs_to_compare)) {
    newly_finished_run <- append (newly_finished_run, which (new_runs_to_compare[i] == stuff))
  }




  # stuff_n_things <- array (c (1, 1, 1, 2, 3, 3, 4, 2, 3, 5, 3, 4, 5, 5), c (7,2))

  # stuff_n_things <- array (c (1, 1, 1, 1, 1, 1, 1,
  #                             1, 2, 2, 2, 2, 2, 2,
  #                             2, 3, 3, 3, 3, 3, 3,
  #                             4, 4, 4, 4, 4, 5, 5,
  #                             5, 5, 6, 6, 6, 7, 7, 8,

  #                             2, 3, 4, 5, 6, 7, 8,
  #                             9, 3, 4, 5, 6, 7, 8,
  #                             9, 4, 5, 6, 7, 8, 9,
  #                             5, 6, 7, 8, 9, 6, 7,
  #                             8, 9, 7, 8, 9, 8, 9, 9), c (36,2))


  # newly_finished_run <- 10
  # newly_finished_run <- c(10, 11)
  # newly_finished_run <- c(10, 11, 12)
  # newly_finished_run <- c(10, 11, 12, 13)
  # newly_finished_run <- c(10, 11, 12, 13, 14)

  # the bad one
  # c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7, 8, 8, 8, 9, 9, 9, 10, 11, 12, 10, 11, 12, 10, 11, 12, 10, 11, 12, 10, 11, 12, 10, 11, 12, 10, 11, 12, 10, 11, 12, 10, 11, 12)
    # stuff_n_things <- array (c (1:(newly_finished_run - 1), rep(newly_finished_run, newly_finished_run - 1)), c (newly_finished_run - 1, 2))
  # the good one - cutting down on the amount of repeated repetition commands...
  # c(1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 12, 12, 12, 12, 12, 12, 12, 12, 12)
    # stuff_n_things <- array (c (rep(1:(newly_finished_run - 1), length (newly_finished_run)), rep(newly_finished_run, newly_finished_run - 1)), c (newly_finished_run - 1, 2))

  if (length(newly_finished_run) == 1) {
    stuff_n_things <- array (c (1:(newly_finished_run - 1), rep(newly_finished_run, newly_finished_run - 1)), c (newly_finished_run - 1, 2))
  } else if (length (newly_finished_run) > 1) {

    number_new_runs <- length (newly_finished_run)

    #

    # rep (newly_finished_run [1], length = (number_new_runs - 1)), rep (newly_finished_run [2], length = (number_new_runs - 2))

    while_placeholder <- number_new_runs - 1

    the_first_cheese <- vector("list", while_placeholder)
    the_secnd_cheese <- vector("list", while_placeholder)

    counter <- while_placeholder

    reps_new_finished <- rep (newly_finished_run[number_new_runs - while_placeholder], newly_finished_run[1] - 1)

    while (while_placeholder > 0) {
      the_first_cheese[[number_new_runs- while_placeholder]] <- append ( the_first_cheese[[number_new_runs - while_placeholder]], rep (newly_finished_run[number_new_runs - while_placeholder], length = while_placeholder))
      the_secnd_cheese[[number_new_runs- while_placeholder]] <- append ( the_secnd_cheese[[number_new_runs - while_placeholder]], tail (newly_finished_run, n = while_placeholder))
      reps_new_finished <- append (reps_new_finished, rep (newly_finished_run[number_new_runs - while_placeholder + 1], newly_finished_run[1] - 1))
      while_placeholder = while_placeholder - 1
      counter <- append (counter, while_placeholder)
    }

    # abind(all the pieces of the_first_cheese, stacking the rows on top of one another (so that it still ends with only two columns))



    stuff_n_things <- array (
      c (
        rep(1:(newly_finished_run[1] - 1), length (newly_finished_run)), unlist (the_first_cheese),
        reps_new_finished, unlist (the_secnd_cheese)#,
      ),
      c ((newly_finished_run[1] -1) * length(newly_finished_run) + sum (counter), 2)
    )

  }

  for (bs in 1:dim(stuff_n_things)[1]) {
    for(whaaat in 1:4) {

      output_heatmap <- heatmap_difference (
                          source_pattern = thing[whaaat],
                          first_source_names = stuff[stuff_n_things[bs, 1]],
                          secnd_source_names = stuff[stuff_n_things[bs, 2]],
                          visualization = "midpoint", # absolute, midpoint, and midpoint_but_high_res
                          replace = TRUE
                          )

      #### Blue values = High Number,
      #### Red values = Low Number

      #### So, for example, Inv(high) - Vanilla = Blue,
      #### while, in contrast, Van - Inv(high) = Red

      # five-by-five-followUpInvLow1k_lowHigh_Background
      individualfigures(
        output_foldername = "DifferenceHeatmaps",
        difference = TRUE,
        colorrange = 2,
        colorpalette = 19,
        foldername = list(
          foldername = output_heatmap$foldername,
          biassize = 5,
          othersize = 2,
          diffcurstartbias = "pop1"
        )
      )

      # _source_names = "five-by-five-followUpVanilla_lowHigh_Background", ### 4101-4300
      # _source_names = "five-by-five-followUpInv1k_lowHigh_Background", ### 4101-4300

      # source_pattern = "male" # "moth", "same", "FfFf"
      # first_source_names = c("five-by-five-followUpInv1k_lowHigh_Background")
      # secnd_source_names = c("five-by-five-followUpVanilla_lowHigh_Background")
      # visualization = "absolute"
      # replace = TRUE



    }
  }
  return (print ("New Difference Heatmaps Added"))
}

print ("differenceheatmaps function loaded")