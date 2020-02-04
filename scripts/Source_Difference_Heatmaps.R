source(file.path("scripts", "Source_Difference_Heatmap_Functions.R"))

differenceheatmaps <- function (
  new_runs_to_compare = "childF4NoInv",
  guide = somethingSomething
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

  stuff <- guide[,1]
  stuff <- paste0("five-by-five-", stuff)

  newly_finished_run <- c()

  for (i in 1 : length (new_runs_to_compare)) {
    newly_finished_run <- append (newly_finished_run, which (paste0 ("five-by-five-", new_runs_to_compare[i]) == stuff))
  }

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
        colorpalette = "difference_spectrum",
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