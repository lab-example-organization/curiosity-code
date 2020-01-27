source(file.path("scripts", "Source_Reference_Section.R"))
referencesection("heatmaps")
source(file.path("scripts", "Source_Heatmap_Functions.R"))
source(file.path("scripts", "Source_Variance_Heatmap_Functions.R"))

varianceheatmaps <- function (
    list_of_sims = somethingSomething[,1],
    sim_in_question = "something"
) {

    # for (thing in 1:length(list_of_sims)) {
    #     finding_some_cross_sections_for_mean_and_variance_calculations(stack_directory = paste0 ("tenKfiveByFive_", list_of_sims[thing]))
    # }


    thing <- finding_some_cross_sections_for_mean_and_variance_calculations(stack_directory = list_of_sims[sim_in_question])


    # for (sim_in_question in 1:length (list_of_sims)) {
        for (sake_of_pete in 1:2) {
            for (out_loud_crying in 1:2) {
                for (pony in 1:4) {
                    # sim_in_question <- 1
                    # sake_of_pete <- 1
                    # out_loud_crying <- 1
                    # pony <- 1
                    # thing <- file.path ("results", "VarianceHeatmaps", paste0 (list_of_sims[sim_in_question], ".RData"))
                    subsets_folder <- extract_subset (the_file_path = file.path(thing), subsetta = paste0 ("1:5,1:5,1:4,", sake_of_pete, ",", out_loud_crying, ",", pony))
                    stuff <- file.path ("VarianceHeatmaps", list_of_sims[sim_in_question], subsets_folder)

                    foldername <- list(
                        foldername = stuff,
                        inheritance = 1,
                        diffcurstartbias = "pop1",
                        biassize = 5,
                        othersize = 1
                    )

                    individualfigures (2,21,foldername = foldername, var = TRUE)
                }
            }
        }
    # }
}
