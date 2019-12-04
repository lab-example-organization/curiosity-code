# args for this function: curiosity_or_sylrep = "curiosity", heatmap_stack = TRUE, stack_directory = "tenKfiveByFive_[ET CETERA]" (only matters if heatmap_stack == TRUE)

# stack_directory junk...
    # parent-noInv, child-noInv, child-highMalInv, child-lowMalInv, child-lowFemInv, child-highFemInv,
    # child-highMalSmolInv, child-lowMalSmolInv, child-highBothInv, child-lowBothInv, child-highFemSmolInv, child-lowFemSmolInv

finding_some_cross_sections_for_mean_and_variance_calculations <- function (
    # curiosity_or_sylrep = "curiosity",
    # heatmap_stack = TRUE,
    stack_directory = "child-lowMalInv"
) {

    path_results <- file.path("results")
    twoHundyKdir <- list.files(path_results, pattern = stack_directory)
    twoHundyK_dir <- list.files(file.path(path_results, twoHundyKdir))
    # that_stacked_object <- array (0, c(400, 157, 2, 50))
    # dimnames(that_stacked_object) <- list("individuals", "traits", "population", "reps", "inheritance_pattern", "curstart_patterns")
    for hundyDir in 1:length(twoHundyK_dir) {
        # hundyDir <- 2

        if (hundyDir == 1) {that_stacked_object <- array (0, c (400, 157, 2, 50))}

        path_dirs <- file.path(path_results, twoHundyKdir, twoHundyK_dir[hundyDir], "variable_store")

        twoHundyReps <- list.files(file.path(path_dirs))

        for hundyRun in 1:length(twoHundyReps) {
            # hundyRun <- 1
            # path_runs <- file.path(path_dirs, twoHundyRuns[hundyRun], "variable_store")

            # twoHundyVarStore <- list.files(path_runs)

            # for hundyVar in 1:length(twoHundyVarStore) {
                # hundyVar <- 1
                path_vars <- file.path(path_dirs, twoHundyReps[hundyRun])

                twoHundyEndData <- list.files(file.path(path_vars), pattern = "end*")

                end_cursity <- readRDS (file.path(path_vars, "end_cursty.RData"))
                    # > dim(end_cursity)
                    # [1] 400   2
                end_sylreps <- readRDS (file.path(path_vars, "end_sylbls.RData"))
                    # > dim(end_sylreps)
                    # [1] 400 156   2
                # variance_file_path <- file.path("results", )

                end_data <- array (0, c (400, 157, 2))
                end_data[,1:156,] <- end_sylreps
                end_data[,157,] <- end_cursity

                that_stacked_object[,,,] <- end_data
            # }
        }

        # this is where the var() and mean() functions will be called :P

    }

    return (that_stacked_object)

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