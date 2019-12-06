# args for this function: curiosity_or_sylrep = "curiosity", heatmap_stack = TRUE, stack_directory = "tenKfiveByFive_[ET CETERA]" (only matters if heatmap_stack == TRUE)

# stack_directory junk...
    # parent-noInv, child-noInv, child-highMalInv, child-lowMalInv, child-lowFemInv, child-highFemInv,
    # child-highMalSmolInv, child-lowMalSmolInv, child-highBothInv, child-lowBothInv, child-highFemSmolInv, child-lowFemSmolInv

finding_some_cross_sections_for_mean_and_variance_calculations <- function (
    # curiosity_or_sylrep = "curiosity",
    # heatmap_stack = TRUE,
    pop_size = 400,
    num_pop = 2,
    stack_directory = "child-lowMalInv"
) {

    path_results <- file.path("results")
    dirStackDir <- list.files(path_results, pattern = stack_directory)
    twoHundyKdirs <- list.files(file.path(path_results, dirStackDir))
    # that_stacked_object <- array (0, c(400, 157, 2, 50))
    # dimnames(that_stacked_object) <- list("individuals", "traits", "population", "reps", "inheritance_pattern", "curstart_patterns")
    for oneSimsDir in 1:length(twoHundyKdirs) {
        # oneSimsDir <- 2 ### 1:200

        if (oneSimsDir == 1) {that_stacked_object <- array (0, c (pop_size, num_pop, 50))}

        path_dirs <- file.path(path_results, dirStackDir, twoHundyKdirs[oneSimsDir], "variable_store")

        repDirs <- list.files(file.path(path_dirs))

        for singleRep in 1:length(repDirs) {
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


        # this is where the var() and mean() functions will be called :P
        # variance_between_sims
        variance_between_sims <- array(0, c(2, 2, 50))
        variance_between_sims <- array(0, c(2, 2))
        variance_within_sims <- array(0, c(2, 2, 50))
        for (pop in 1:num_pop) {
            for (sex in 1:2) {
                #
                variance_between_sims[pop, sex,] <- sapply(1:50, function(x) mean(that_stacked_object[1 + ((sex - 1)*(pop_size/2)):(sex*(pop_size/2)),pop,x]))
                variance_btween_sims[pop, sex] <- var(variance_between_sims[pop, sex,])
                variance_within_sims[pop, sex,] <- sapply(1:50, function(x) var(that_stacked_object[1 + ((sex - 1)*(pop_size/2)):(sex*(pop_size/2)),pop,x]))
            }
        }
            # variance_between_sims <- sapply(1:50, function(x) var(that_stacked_object[,,x]))
            # apply mean() across rows (dim 1) of that_stacked_object, make a data structure to hold all the reps' , then var() on whichever object the mean() results were saved to.
        # variance_within_sims ### Or maybe "variance_between_reps"?
            # apply var()
            varn_curiosities[] <-

    }

    return (output_object)

}
# input:

# output: 2 different matrices with variance info,
    # 1 with variance between the means of each dir in twoHundyVarStore,
    # 1 with mean of the variance within each twoHundyVarStore.

# output location:
    # New directory in results
    # variance_heatmaps
        # between sims (variance between run means)
        # within sims (between runs) (mean of run variances)
variance_output_dir <- file.path ("results", "variance_heatmaps")
if (! (dir.exists (variance_output_dir))) {dir.create (variance_output_dir)}

if (! (dir.exists (file.path (variance_output_dir, "between_sims")))) {
    dir.create(file.path (variance_output_dir, "between_sims"))
}

if (! (dir.exists (file.path (variance_output_dir, "within_sims")))) {
    dir.create(file.path (variance_output_dir, "within_sims"))
}

b_variance_heatmap <- variance_matrix_function ()
w_variance_heatmap <- variance_matrix_function ()

saveRDS(b_variance_heatmap, file.path (variance_output_dir, "between_sims", "blahblahblah.RData"))
saveRDS(w_variance_heatmap, file.path (variance_output_dir, "within_sims", "blahblahblah.RData"))

variance_matrix_function <- function (
    input_matrix_object,
    between_or_within
) {

    return (output_matrix)
}