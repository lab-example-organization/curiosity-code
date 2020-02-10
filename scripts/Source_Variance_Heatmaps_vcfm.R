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


    # thing <- finding_some_cross_sections_for_mean_and_variance_calculations(stack_directory = list_of_sims[sim_in_question])

# str_replace_all (the_file_path,"fullData", str_split(str_split(the_file_path, "/")[[1]][2], ".R")[[1]][1])

    # for (sim_in_question in 1:length (list_of_sims)) {
        for (lowbg_vs_hihbg in 1:2) {
            # for (between_vs_within in 1:2) {
                for (curinh_style in 1:4) {
                    # sim_in_question <- 1
                    # lowbg_vs_hihbg <- 1
                    # between_vs_within <- 1
                    # curinh_style <- 1
                    # thing <- file.path ("results", "VarianceHeatmaps", paste0 (list_of_sims[sim_in_question], ".RData"))


                    # subsets_folder <- extract_subset (
                    #     the_file_path = file.path(thing),
                    #     subsetta = paste0 ("1:5,1:5,1:4,", lowbg_vs_hihbg, ",", between_vs_within, ",", curinh_style)
                    # )

                    subsets_folder <- var_calc_from_moran (
                        path = file.path("results", paste0("tenKfiveByFive_", list_of_sims[sim_in_question])),
                        lbhb = lowbg_vs_hihbg,
                        cur_inh = curinh_style
                    ) # return (file.path(bg_fd_path, paste0(str_split (path, "tenKfiveByFive_")[[1]][2], "_", bg_container[lbhb], "_bkgd.RData")))

                    foldername <- list(
                        foldername = subsets_folder,
                        inheritance = 1,
                        diffcurstartbias = "pop1",
                        biassize = 5,
                        othersize = 1
                    )

                    individualfigures (
                        output_foldername = FALSE,
                        colorrange = 2,
                        colorpalette = "variance_spectrum",
                        input_list = foldername,
                        midpoint_size = 1,
                        variance_treatment = "var_calc_from_moran"
                    )
                }
            # }
        }
    # }
}
