source(file.path("scripts", "Source_Reference_Section.R"))
referencesection("heatmaps")
source(file.path("scripts", "Source_Heatmap_Functions.R"))

# args for this function: curiosity_or_sylrep = "curiosity", heatmap_stack = TRUE, stack_directory = "tenKfiveByFive_[ET CETERA]" (only matters if heatmap_stack == TRUE)

# stack_directory junk...
    # parent-noInv, child-noInv, child-highMalInv, child-lowMalInv, child-lowFemInv, child-highFemInv,
    # child-highMalSmolInv, child-lowMalSmolInv, child-highBothInv, child-lowBothInv, child-highFemSmolInv, child-lowFemSmolInv

finding_some_cross_sections_for_mean_and_variance_calculations <- function (
    # curiosity_or_sylrep = "curiosity",
    # heatmap_stack = TRUE,
    pop_size = 400,
    num_pop = 2,
    output_dims = c(5,5,4,2,2,4),
    stack_directory = "child-lowMalInv"
) {

    path_results <- file.path("results")
    dirStackDir <- list.files(path_results, pattern = stack_directory)
    twoHundyKdirs <- list.files(file.path(path_results, dirStackDir))
    # that_stacked_object <- array (0, c(400, 157, 2, 50))
    # dimnames(that_stacked_object) <- list("individuals", "traits", "population", "reps", "inheritance_pattern", "curstart_patterns")

    that_stacked_object <- array (0, c (pop_size, num_pop, 50))

    output_object <- array (0, output_dims)

    if (length (output_object[,,,,1,1]) == length (twoHundyKdirs)) {

        for (oneSimsDir in 1:length(twoHundyKdirs)) {
            # oneSimsDir <- 2 ### 1:200

            # if (oneSimsDir == 1) {that_stacked_object <- array (0, c (pop_size, num_pop, 50))}

            path_dirs <- file.path(path_results, dirStackDir, twoHundyKdirs[oneSimsDir], "variable_store")

            repDirs <- list.files(file.path(path_dirs))

            for (singleRep in 1:length(repDirs)) {
                # singleRep <- 1 ### 1:50
                # path_runs <- file.path(path_dirs, twoHundyRuns[singleRep], "variable_store")

                # twoHundyVarStore <- list.files(path_runs)

                # for hundyVar in 1:length(twoHundyVarStore) {
                    # hundyVar <- 1
                path_vars <- file.path(path_dirs, repDirs[singleRep])

                # twoHundyEndData <- list.files(file.path(path_vars), pattern = "end*")

                end_cursity <- readRDS (file.path(path_vars, "end_cursty.RData"))
                    # > dim(end_cursity)
                    # [1] 400   2

    # For the time being, forget about sylreps.

    #             end_sylreps <- readRDS (file.path(path_vars, "end_sylbls.RData"))
    #                 # > dim(end_sylreps)
    #                 # [1] 400 156   2
    #             end_sylreps_mean <- array(sapply(1:2, function(x) mean(which(end_sylreps[1,,x] == 1))), c(1,2))
    #             end_sylreps_varn <- array(sapply(1:2, function(x) var(which(end_sylreps[1,,x] == 1)/156)*156), c(1,2))
    #             list.condition <- sapply(arguments, function(x) class(x)=="desired.class")
    #             sapply(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE)
    #             thing <- sapply(1:number_of_runs, function(x) str_split(allrundirs[x], "_")[[1]][2])
    #             variance_file_path <- file.path("results", )

    #             # if (singleRep == 1) {end_data <- array (0, c (400, 157, 2))}
    # # var(which(end_sylreps[1,,1] == 1)/156)*156
    # # mean(which(end_sylreps[1,,1] == 1))

    #             that_stacked_object[,1,,singleRep] <- end_sylreps_mean
                that_stacked_object[,,singleRep] <- end_cursity

                # that_stacked_object[,,,singleRep] <- end_data
                # }
            }

            variance_between_sims <- array(0, c(2, 2, 50))
            variance_btween_sims <- array(0, c(2, 2))
            variance_witheen_sims <- array(0, c(2, 2, 50))
            variance_within_sims <- array(0, c(2, 2))

            osd_p1mc <- if(oneSimsDir%%50 < 26) {floor((oneSimsDir%%50)/5)} else {floor(((oneSimsDir%%50) - 25)/5)}
            osd_p1fc <- if(oneSimsDir %% 5 != 0) {oneSimsDir %% 5} else {5}
            osd_lbhb <- ifelse(oneSimsDir %in% c(1:25,51:75,101:125,151:175),1,2)
            osd_curh <- ceiling(oneSimsDir/50)

            for (pop in 1:num_pop) {
                for (sex in 1:2) {
                    #
                    variance_between_sims[pop, sex,] <- sapply(1:50, function(x) mean(that_stacked_object[(1 + ((sex - 1)*(pop_size/2))):(sex*(pop_size/2)),pop,x]))
                    variance_btween_sims[pop, sex] <- var(variance_between_sims[pop, sex,])
                    output_object[osd_p1mc,osd_p1fc,(((sex - 1) * 2) + pop),osd_lbhb,1,osd_curh] <- variance_btween_sims[pop,sex]
                    variance_witheen_sims[pop, sex,] <- sapply(1:50, function(x) var(that_stacked_object[(1 + ((sex - 1)*(pop_size/2))):(sex*(pop_size/2)),pop,x]))
                    variance_within_sims[pop, sex] <- mean(variance_witheen_sims[pop, sex,])
                    output_object[osd_p1mc,osd_p1fc,(((sex - 1) * 2) + pop),osd_lbhb,2,osd_curh] <- variance_within_sims[pop,sex]
                    # output_object - dim = c(5, 5, 4, 2, 2, 4) <- 5x5 for different pop 1 curstarts, 4 for trait being measured (pop-then-sex), and 2 for low background, high background curstart. Added addn'l dim for which variance (between or within). Added addn'l dim for curinh pattern - male, moth, same, FfFf
                    # output_object[floor(oneSimsDir/5),if(oneSimsDir %% 5 != 0) {oneSimsDir %% 5} else {5}},pop,sex,LBHB,which_variance,(ceilng(oneSimsDir/50))]
                }
            }

            # variance_between_sims <- sapply(1:50, function(x) var(that_stacked_object[,,x]))
            # apply mean() across rows (dim 1) of that_stacked_object, make a data structure to hold all the reps' , then var() on whichever object the mean() results were saved to.
            # variance_within_sims ### Or maybe "variance_between_reps"?
            # apply var()

            # output_object[] <-

        }

    } else {
        stop ("find out if the output_dir or the output_dim is wrong; they're not equal so one of them is likely wrong")
    }
    if (! (dir.exists (file.path (path_results, "variance_heatmap_output")))) {dir.create (file.path (path_results, "variance_heatmap_output"))}
    # if (! (dir.exists (file.path (path_results, "variance_heatmap_output", stack_directory)))) {dir.create (file.path (path_results, "variance_heatmap_output", stack_directory))}
    saveRDS(output_object, file.path(path_results, "variance_heatmap_output", paste0(stack_directory, ".RData")))

    return ("output_object is in folder")

}

# stuff <- readRDS(file.path("results", "variance_heatmap_output", "child-noInv.RData"))

# dimnames(stuff) <- list(c("vLowMalC","lowMalC","midMalC","highMalC","vHighMalC"),
                        # c("vLowFemC","lowFemC","midFemC","highFemC","vHighFemC"),
                        # c("pop1Sex1","pop1Sex2","pop2Sex1","pop2Sex2"),
                        # c("LowBG","HighBG"),
                        # c("varBetween","varWithin"),
                        # c("curInhMale","curInhMoth","curInhSame","curInhFfFf"))

things_need_doin <- c("parent-noInv", "child-noInv", "child-highMalInv", "child-lowMalInv", "child-lowFemInv", "child-highFemInv", "child-highMalSmolInv", "child-lowMalSmolInv", "child-highBothInv", "child-lowBothInv", "child-highFemSmolInv", "child-lowFemSmolInv")

for (thing in 1:length(things_need_doin)) {
    finding_some_cross_sections_for_mean_and_variance_calculations(stack_directory = things_need_doin[thing])
}


extract_subset <- function (
    the_file_path,
    subsetta
) {

    # This function is for when I need to pull out the subset of the variance
    # function output that individualfigures needs to make a heatmap.

    output <- readRDS(file = file.path (the_file_path))

    eval(parse(text = paste0("output <- output[", subsetta, "]")))

    subset_categories <- list (
        c("LowBG","HighBG"),
        c("varBetween","varWithin"),
        c("curInhMale","curInhMoth","curInhSame","curInhFfFf")
    )

    # str_split(subsetta, ",")[[1]][4]
    # str_split(subsetta, ",")[[1]][5]
    # str_split(subsetta, ",")[[1]][6]

    subset_folder <- str_replace_all (
        the_file_path,"fullData", paste0 (
            subset_categories[[1]][as.numeric(str_split(subsetta, ",")[[1]][4])],
            subset_categories[[2]][as.numeric(str_split(subsetta, ",")[[1]][5])],
            subset_categories[[3]][as.numeric(str_split(subsetta, ",")[[1]][6])]
        )
    )

    gawd <- str_split (subset_folder, "/")
    subset_folder <- str_remove (subset_folder, gawd[[1]][5])

    if (! (dir.exists (subset_folder))) {dir.create(subset_folder)}

    saveRDS(output, file = file.path (subset_folder, paste0 (gawd[[1]][4], ".RData")))

    return (gawd[[1]][4])
}

things_need_doin <- c("parent-noInv", "child-noInv", "child-highMalInv", "child-lowMalInv", "child-lowFemInv", "child-highFemInv", "child-highMalSmolInv", "child-lowMalSmolInv", "child-highBothInv", "child-lowBothInv", "child-highFemSmolInv", "child-lowFemSmolInv")

for (ordering in 1:length (things_need_doin)) {
    for (sake_of_pete in 1:2) {
        for (out_loud_crying in 1:2) {
            for (pony in 1:4) {
                # ordering <- 1
                # sake_of_pete <- 1
                # out_loud_crying <- 1
                # pony <- 1
                thing <- file.path ("results", "variance_heatmap_output", things_need_doin[ordering], "fullData", paste0 (things_need_doin[ordering], ".RData"))
                subsets_folder <- extract_subset (the_file_path = file.path(thing), subsetta = paste0 ("1:5,1:5,1:4,", sake_of_pete, ",", out_loud_crying, ",", pony))
                stuff <- file.path ("variance_heatmap_output", things_need_doin[ordering], subsets_folder)

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
    # # ordering <- 1
    # thing <- file.path ("results", "variance_heatmap_output", things_need_doin[ordering], "fullData", paste0 (things_need_doin[ordering], ".RData"))

    # # subsets_folder <- extract_subset (the_file_path = file.path(thing), subsetta = c("5:1,5:1,1:4,1,1,1"))
    # subsets_folder <- extract_subset (the_file_path = file.path(thing), subsetta = paste0 ("5:1,5:1,1:4,", sake_of_pete, ",", out_loud_crying, ",", pony))

    # stuff <- file.path ("results", "variance_heatmap_output", things_need_doin[ordering], subsets_folder)

    # foldername <- list(
    #     foldername <- stuff,
    #     inheritance = 1,
    #     diffcurstartbias = "pop1",
    #     biassize = 5,
    #     othersize = 1
    # )

    # individualfigures (2,5,foldername, var = TRUE)
}


# finding_some_cross_sections_for_mean_and_variance_calculations(stack_directory = "parent-noInv")


# # input:

# # output: 2 different matrices with variance info,
#     # 1 with variance between the means of each dir in twoHundyVarStore,
#     # 1 with mean of the variance within each twoHundyVarStore.

# # output location:
#     # New directory in results
#     # variance_heatmaps
#         # between sims (variance between run means)
#         # within sims (between runs) (mean of run variances)
# variance_output_dir <- file.path ("results", "variance_heatmaps")
# if (! (dir.exists (variance_output_dir))) {dir.create (variance_output_dir)}

# if (! (dir.exists (file.path (variance_output_dir, "between_sims")))) {
#     dir.create(file.path (variance_output_dir, "between_sims"))
# }

# if (! (dir.exists (file.path (variance_output_dir, "within_sims")))) {
#     dir.create(file.path (variance_output_dir, "within_sims"))
# }

# b_variance_heatmap <- variance_matrix_function ()
# w_variance_heatmap <- variance_matrix_function ()

# saveRDS(b_variance_heatmap, file.path (variance_output_dir, "between_sims", "blahblahblah.RData"))
# saveRDS(w_variance_heatmap, file.path (variance_output_dir, "within_sims", "blahblahblah.RData"))

# variance_matrix_function <- function (
#     input_matrix_object,
#     between_or_within
# ) {

#     return (output_matrix)
# }