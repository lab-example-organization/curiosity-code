# source(file.path("scripts", "Source_Reference_Section.R"))
# referencesection("heatmaps")
# source(file.path("scripts", "Source_Heatmap_Functions.R"))

# # args for this function: curiosity_or_sylrep = "curiosity", heatmap_stack = TRUE, stack_directory = "tenKfiveByFive_[ET CETERA]" (only matters if heatmap_stack == TRUE)

# # stack_directory junk...
#     # parent-noInv, child-noInv, child-highMalInv, child-lowMalInv, child-lowFemInv, child-highFemInv,
#     # child-highMalSmolInv, child-lowMalSmolInv, child-highBothInv, child-lowBothInv, child-highFemSmolInv, child-lowFemSmolInv

finding_some_cross_sections_for_mean_and_variance_calculations <- function (
    # curiosity_or_sylrep = "curiosity",
    # heatmap_stack = TRUE,
    pop_size = 400,
    num_pop = 2,
    output_dims = c(5,5,4,2,2,4),
    stack_directory = "child-lowMalInv"
) {

    path_results <- file.path("results")
    dirStackDir <- list.files(path_results, pattern = paste0("tenKfiveByFive_", stack_directory))
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

    } else {stop ("find out if output_dir or output_dim is wrong; they're not equal")}

    if (! (dir.exists (file.path (path_results, "VarianceHeatmaps")))) {dir.create (file.path (path_results, "VarianceHeatmaps"))}
    if (! (dir.exists (file.path (path_results, "VarianceHeatmaps", "fullData")))) {dir.create (file.path (path_results, "VarianceHeatmaps", "fullData"))}

    # if (! (dir.exists (file.path (path_results, "VarianceHeatmaps", stack_directory)))) {dir.create (file.path (path_results, "VarianceHeatmaps", stack_directory))}
    saveRDS(output_object, file.path(path_results, "VarianceHeatmaps", "fullData", paste0(stack_directory, ".RData")))

    return (file.path(path_results, "VarianceHeatmaps", "fullData", paste0(stack_directory, ".RData")))

}

# the_file_path = file.path(thing), subsetta = paste0 ("1:5,1:5,1:4,", sake_of_pete, ",", out_loud_crying, ",", pony)

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

    subset_of_interest <- paste0 (
            subset_categories[[1]][as.numeric(str_split(subsetta, ",")[[1]][4])],
            subset_categories[[2]][as.numeric(str_split(subsetta, ",")[[1]][5])],
            subset_categories[[3]][as.numeric(str_split(subsetta, ",")[[1]][6])]
        )

    # golly <- c (str_split (the_file_path, "fullData/")[[1]][1], strsplit (str_split (the_file_path, "fullData/")[[1]][2], ".", fixed = T)[[1]][1])
    golly <- str_split(the_file_path, "/")

    # c (str_split (the_file_path, "fullData/")[[1]][1], strsplit (str_split (the_file_path, "fullData/")[[1]][2], ".", fixed = T)[[1]][1])

    # golly <- paste(golly, collapse = "")

    # subset_folder <- str_remove (subset_folder, golly[[1]][5])
    the_parent_path <- strsplit (str_split (the_file_path, "fullData/")[[1]][2], ".", fixed = T)[[1]][1]
    sim_folder <- file.path (golly[[1]][2], the_parent_path)
    subset_folder <- file.path (sim_folder, subset_of_interest)

    if (! (dir.exists (file.path ("results", sim_folder)))) {dir.create(file.path ("results", sim_folder))}
    if (! (dir.exists (file.path("results", subset_folder)))) {dir.create(file.path("results", subset_folder))}

    saveRDS(output, file = file.path ("results", subset_folder, paste0 (subset_of_interest, ".RData")))

    return (subset_folder)
}

print("finding_some_cross_sections_for_mean_and_variance_calculations and extract_subset loaded")

var_calc_from_moran <- function (
    path,
    lbhb,
    cur_inh
) {
    placeholder = rep(1,200)

    twohundruns <- list.files(file.path(path))

    if (lbhb == 1) {
        placeholder[c(26:50, 76:100, 126:150, 176:200)] = 0
    } else {
        placeholder[c(1:25, 51:75, 101:125, 151:175)] = 0
    }

    if (cur_inh == 1) {placeholder[c(51:200)] = 0
    } else if (cur_inh == 2) {placeholder[c(1:50,101:200)] = 0
    } else if (cur_inh == 3) {placeholder[c(1:100,151:200)] = 0
    } else if (cur_inh == 4) {placeholder[c(1:150)] = 0}

    subsetruns <- twohundruns[which (placeholder == 1)]


    # replicates <- vector("list", length(subsetruns))
    replicates <- array(0, c (4, 2, length(subsetruns)))
    for(swanky in 1:length(subsetruns)) {
        thing <- array (0, c (4, 2, 50))
        for (stuff in 1:50) {
            placeholder_two <- readRDS(file.path(path, subsetruns[swanky], "multirun_output", paste0 ("Cursity", stuff, ".RData")))
            thing[,,stuff] <- placeholder_two[c(1,2,13,14),,100]
        }
        # arrays don't have a byrow = TRUE arg, so have to fill in this way.
        processed_thing <- array (c (
            var(thing[1,1,]),
            var(thing[2,1,]),
            mean(thing[3,1,]),
            mean(thing[4,1,]),
            var(thing[1,2,]),
            var(thing[2,2,]),
            mean(thing[3,2,]),
            mean(thing[4,2,])#,

            # sapply(1:50, function(x) mean(that_stacked_object[(1 + ((sex - 1)*(pop_size/2))):(sex*(pop_size/2)),pop,x]))
        ), c (4,2))
        replicates[,,swanky] <- processed_thing
    }

    # variance_between_replicates <- array (c (), c (5, 5, 2, 2)) # take the means (rows 1 and 2) of replicates
    # variance_within_replicate <- array (c (), c (5, 5, 2, 2))
            # c ("0-0.2mp1", "0.2-0.3mp1", "0.4-0.6mp1", "0.55-0.75mp1", "0.7-0.8mp1"),
            # c ("0-0.2fp1", "0.2-0.3fp1", "0.4-0.6fp1", "0.55-0.75fp1", "0.7-0.8fp1"),
            # c ("0.2-0.3p2", "0.7-0.8p2"),
    output_object <- array(0, c(5, 5, 4, 2), list(
        c("0-0.2mp1", "0.2-0.3mp1", "0.4-0.6mp1", "0.55-0.75mp1", "0.7-0.8mp1"),
        c("0-0.2fp1", "0.2-0.3fp1", "0.4-0.6fp1", "0.55-0.75fp1", "0.7-0.8fp1"),
        c("VB_mal", "VB_fem", "VW_mal", "VW_fem"),
        c("pop1", "pop2"))
    )

    for (i in 1:5) {
        for (j in 1:5) {
            output_object[i, j,,] <- replicates[,,j + 5*(i - 1)]
        }
    }
    # saveRDS, path,
    path_results <- file.path("results")

    vh_path <- file.path (path_results, "VarianceHeatmaps")
    if (! (dir.exists (vh_path))) {dir.create (vh_path)}

    sim_path <- file.path (vh_path, strsplit(path, "tenKfiveByFive_")[[1]][2]) # "childNoInvF1"
    if (! (dir.exists (sim_path))) {dir.create (sim_path)}

    fd_path <- file.path (sim_path, "fullData")
    if (! (dir.exists (fd_path))) {dir.create (fd_path)}


    curinh_container <- c("Male","Moth","Same","FfFf")

    curinh_path <- file.path (sim_path, paste0(curinh_container[cur_inh], "_curInh"))
    curinh_fd_path <- file.path (fd_path, paste0(curinh_container[cur_inh], "_curInh"))
    if (! (dir.exists (file.path (curinh_path)))) {dir.create (file.path (curinh_path))}
    if (! (dir.exists (file.path (curinh_fd_path)))) {dir.create (file.path (curinh_fd_path))}


    bg_container <- c("low", "hih")

    bg_path <- file.path (curinh_path, paste0(bg_container[lbhb], "_bkgd_curstart"))
    bg_fd_path <- file.path (curinh_fd_path, paste0(bg_container[lbhb], "_bkgd_curstart"))
    if (! (dir.exists (file.path (bg_path)))) {dir.create (file.path (bg_path))}
    if (! (dir.exists (file.path (bg_fd_path)))) {dir.create (file.path (bg_fd_path))}


    # if (! (dir.exists (file.path (path_results, "VarianceHeatmaps", stack_directory)))) {dir.create (file.path (path_results, "VarianceHeatmaps", stack_directory))}
    saveRDS(output_object, file.path(bg_fd_path, paste0(str_split (path, "tenKfiveByFive_")[[1]][2], "_", curinh_container[cur_inh], "_cI_", bg_container[lbhb], "_bkgd.RData")))

    return (file.path(str_split(bg_fd_path, "results/")[[1]][2], paste0(str_split (path, "tenKfiveByFive_")[[1]][2], "_", curinh_container[cur_inh], "_cI_", bg_container[lbhb], "_bkgd.RData")))
}

# thingie <- "childNoInvF1"

# var_calc_from_moran(
    # path = file.path("results", "VarianceHeatmaps", paste0("tenKfiveByFive_", thingie)),
    # lbhb = ,
    # cur_inh =
# )


