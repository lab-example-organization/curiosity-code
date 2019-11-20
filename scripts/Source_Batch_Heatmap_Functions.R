find_the_dir <- function (
  pattern, #male
  source_dir # c("results", "five-by-five-vanilla_lowHigh_Background")
) {

  dirs_within_sourcedir <- list.dirs(file.path ("results", source_dir), recursive = FALSE)

  thing <- FALSE

  for (i in 1 : length (dirs_within_sourcedir)) {
    if (! (thing)) {
      stuff <- strsplit(strsplit(dirs_within_sourcedir, "inh")[[i]], "_")
      for (j in 1:length (stuff)) {
        whatever <- which (stuff[[j]] == pattern)
        if (length(whatever > 0)) {
          # print ("you found it!")
          # i <- append(i, j)
          # whatever <- append(whatever, i)
          found_it <- i
          thing <- TRUE
          break
        }
        # print ("thing")
      }
      if (thing) {break}
      # print ("stuff")
    } else {
      break
    }
    if (thing) {break}
    # print ("whatever")
  }
  # pattern_interrogation <- dirs_within_sourcedir[[1:length(dirs_within_sourcedir)]]

  final_dir <- file.path (dirs_within_sourcedir[[found_it]])

  return (final_dir) # "results/five-by-five-vanilla_lowHigh_Background/191111_slices_-_maleinh_pop1Bias"
}

heatmap_difference <- function (
  source_pattern = "male", # "moth", "same", "FfFf"
  first_source_names = c("five-by-five-vanilla_lowHigh_Background"),
  secnd_source_names = c("five-by-five-Inv3k_lowHigh_Background"),
  visualization = "absolute", # "absolute" - takes absolute value of differences between different conditions. "midpoint" - grades differences according to which of the two heatmaps has a greater value!
  replace = FALSE,
  special_source = FALSE,
#   foldername = list(
    #   diffcurstartbias = ,
    #   biassize = ,
    #   othersize =
#   ),
  UL = 10
) {
  if (length (source_pattern) > 1) {
    stop ("Trying to compare things along a categorical axis? Then pick one!")
  }
  if (first_source_names[1] != secnd_source_names[1]) {
    if (length (first_source_names) == 1) {
      first_source_directory <- file.path(first_source_names[1])
    } else if (length (first_source_names) == 2) {
      first_source_directory <- file.path(first_source_names[1], first_source_names[2])
    } else if (length (first_source_names) == 3) {
      first_source_directory <- file.path(first_source_names[1], first_source_names[2], first_source_names[3])
    } else if (length (first_source_names) == 4) {
      first_source_directory <- file.path(first_source_names[1], first_source_names[2], first_source_names[3], first_source_names[4])
    } else if (length (first_source_names) == 5) {
      first_source_directory <- file.path(first_source_names[1], first_source_names[2], first_source_names[3], first_source_names[4], first_source_names[5])
    } else if (length (first_source_names) == 6) {
      first_source_directory <- file.path(first_source_names[1], first_source_names[2], first_source_names[3], first_source_names[4], first_source_names[5], first_source_names[6])
    } else if (length (first_source_names) == 7) {
      first_source_directory <- file.path(first_source_names[1], first_source_names[2], first_source_names[3], first_source_names[4], first_source_names[5], first_source_names[6], first_source_names[7])
    } else if (length (first_source_names) == 8) {
      first_source_directory <- file.path(first_source_names[1], first_source_names[2], first_source_names[3], first_source_names[4], first_source_names[5], first_source_names[6], first_source_names[7], first_source_names[8])
    } else if (length (first_source_names) == 9) {
      first_source_directory <- file.path(first_source_names[1], first_source_names[2], first_source_names[3], first_source_names[4], first_source_names[5], first_source_names[6], first_source_names[7], first_source_names[8], first_source_names[9])
    } else if (length (first_source_names) == 10) {
      first_source_directory <- file.path(first_source_names[1], first_source_names[2], first_source_names[3], first_source_names[4], first_source_names[5], first_source_names[6], first_source_names[7], first_source_names[8], first_source_names[9], first_source_names[10])
    } else if (length (first_source_names) > UL) {
      stop ("make more categories for first directory! Too much directory for this lil function!")
    }

    if (length (secnd_source_names) == 1) {
      secnd_source_directory <- file.path(secnd_source_names[1])
    } else if (length (secnd_source_names) == 2) {
      secnd_source_directory <- file.path(secnd_source_names[1], secnd_source_names[2])
    } else if (length (secnd_source_names) == 3) {
      secnd_source_directory <- file.path(secnd_source_names[1], secnd_source_names[2], secnd_source_names[3])
    } else if (length (secnd_source_names) == 4) {
      secnd_source_directory <- file.path(secnd_source_names[1], secnd_source_names[2], secnd_source_names[3], secnd_source_names[4])
    } else if (length (secnd_source_names) == 5) {
      secnd_source_directory <- file.path(secnd_source_names[1], secnd_source_names[2], secnd_source_names[3], secnd_source_names[4], secnd_source_names[5])
    } else if (length (secnd_source_names) == 6) {
      secnd_source_directory <- file.path(secnd_source_names[1], secnd_source_names[2], secnd_source_names[3], secnd_source_names[4], secnd_source_names[5], secnd_source_names[6])
    } else if (length (secnd_source_names) == 7) {
      secnd_source_directory <- file.path(secnd_source_names[1], secnd_source_names[2], secnd_source_names[3], secnd_source_names[4], secnd_source_names[5], secnd_source_names[6], secnd_source_names[7])
    } else if (length (secnd_source_names) == 8) {
      secnd_source_directory <- file.path(secnd_source_names[1], secnd_source_names[2], secnd_source_names[3], secnd_source_names[4], secnd_source_names[5], secnd_source_names[6], secnd_source_names[7], secnd_source_names[8])
    } else if (length (secnd_source_names) == 9) {
      secnd_source_directory <- file.path(secnd_source_names[1], secnd_source_names[2], secnd_source_names[3], secnd_source_names[4], secnd_source_names[5], secnd_source_names[6], secnd_source_names[7], secnd_source_names[8], secnd_source_names[9])
    } else if (length (secnd_source_names) == 10) {
      secnd_source_directory <- file.path(secnd_source_names[1], secnd_source_names[2], secnd_source_names[3], secnd_source_names[4], secnd_source_names[5], secnd_source_names[6], secnd_source_names[7], secnd_source_names[8], secnd_source_names[9], secnd_source_names[10])
    } else if (length (secnd_source_names) > UL) {
      stop ("make more categories for second directory! Too much directory for this lil function!")
    }
  }

  # heatmap_array <- readRDS(file.path("results", "Heatmaps", "output_objects",
  #   foldername, list.files(path = file.path("results", "Heatmaps", "output_objects",
  #     foldername), pattern =
  #       "heatmap_output_-_")
  #   )
  # )



  first_heatmap <- readRDS (file.path(find_the_dir (pattern = source_pattern, source_dir = first_source_directory), list.files (find_the_dir (pattern = source_pattern, source_dir = first_source_directory), pattern = ".RData")))
  second_heatmap <- readRDS (file.path(find_the_dir (pattern = source_pattern, source_dir = secnd_source_directory), list.files (find_the_dir (pattern = source_pattern, source_dir = secnd_source_directory), pattern = ".RData")))
  if (visualization == "absolute") {
    output_curiosity <- abs(first_heatmap[,,,1:4] - second_heatmap[,,,1:4])
    output_sylreps <- (abs(first_heatmap[,,,5:8] - second_heatmap[,,,5:8]) / (156 - 1))
    output_heatmap <- abind(output_curiosity, output_sylreps, along = 4)
  } else if (visualization == "midpoint") {
    output_curiosity <- ((first_heatmap[,,,1:4] - second_heatmap[,,,1:4]) / 2) + 0.5
    output_sylreps <- ((first_heatmap[,,,5:8] - second_heatmap[,,,5:8]) / 2) + (155/2)
    output_heatmap <- abind(output_curiosity, output_sylreps, along = 4)
  }


  # if(!(dir.exists(file.path("results", foldername)))) {

  #     dir.create(file.path("results", foldername))}

  # if(!(file.exists(file.path(
  #   "results", foldername, paste0("heatmap_output_-_", inheritance,
  #   "inh_", diffcurstartbias, "Bias_", runstyle, ".RData")
  # )))) {

  # saveRDS(heatmap_array, file.path(
  #   "results",foldername, paste0("heatmap_output_-_", inheritance,
  #   "inh_", diffcurstartbias, "Bias_", runstyle, ".RData")

  # ))}

  fsd <- setdiff(strsplit(first_source_directory, "_")[[1]], strsplit(secnd_source_directory, "_")[[1]])
  ssd <- setdiff(strsplit(secnd_source_directory, "_")[[1]], strsplit(first_source_directory, "_")[[1]])

  first_source_directory <- setdiff(strsplit(fsd, "-")[[1]], strsplit(ssd, "-")[[1]])
  secnd_source_directory <- setdiff(strsplit(ssd, "-")[[1]], strsplit(fsd, "-")[[1]])

  # differences <-
  if (! (dir.exists (file.path ("results", paste0("Difference_Heatmaps_", first_source_directory, "_versus_", secnd_source_directory))))) {
      dir.create(file.path ("results", paste0("Difference_Heatmaps_", first_source_directory, "_versus_", secnd_source_directory)))
    }

    if (! (dir.exists (file.path ("results", paste0("Difference_Heatmaps_", first_source_directory, "_versus_", secnd_source_directory), paste0(source_pattern, "_inh"))))) {
      dir.create(file.path("results", paste0("Difference_Heatmaps_", first_source_directory, "_versus_", secnd_source_directory), paste0(source_pattern, "_inh")))
    }

  if (replace) {
    saveRDS(output_heatmap, file.path ("results", paste0("Difference_Heatmaps_", first_source_directory, "_versus_", secnd_source_directory), paste0(source_pattern, "_inh"), paste0("Difference_Heatmap_", source_pattern, "_inheritance.RData")))
  } else {
    if (! (file.exists (file.path ("results", paste0("Difference_Heatmaps_", first_source_directory, "_versus_", secnd_source_directory), paste0(source_pattern, "_inh"), paste0("Difference_Heatmap_", source_pattern, "_inheritance.RData"))))) {
      saveRDS(output_heatmap, file.path ("results", paste0("Difference_Heatmaps_", first_source_directory, "_versus_", secnd_source_directory), paste0(source_pattern, "_inh"), paste0("Difference_Heatmap_", source_pattern, "_inheritance.RData")))
    }
  }

  output_heatmap <- list(
    # biassize = foldername$biassize,
    # othersize = foldername$othersize,
    # diffcurstartbias = foldername$diffcurstartbias,
    # output_heatmap = output_heatmap,
    foldername = file.path (paste0("Difference_Heatmaps_", first_source_directory, "_versus_", secnd_source_directory), paste0(source_pattern, "_inh"))
  )

  return (output_heatmap)
}

print("find_the_dir, heatmap_difference loaded")
