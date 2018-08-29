##### setwd("/home/labuser/Documents/Parker Scratch Folder/Code/Curiosity Code")
mult_file_length <- 12
reg_spresh <- "2018"
offset <- 1
for(multiple_files in 1:mult_file_length) {
  parent_directory <- getwd()
  FolderName <- list.files(pattern = reg_spresh)[multiple_files + offset]
  
  data_visuals <- paste0("source(\"", parent_directory, "/", "Source_Visualizing_Data.R\")")
  #data_visuals <- paste0("source(\"", parent_directory, "/", "Source_Visualizing_Data.R\")")
  eval(parse(text = data_visuals))
  
  parent_directory <- str_replace_all(FolderName, paste0("/", str_split(FolderName, "/")[[1]][8]),"")
  setwd(parent_directory)
  info <- readRDS(file = "metadata.RData")
  setwd(FolderName)
  #converted_data <- convert_stored_data(P = P, num_timechunks = thousand_timesteps)
  
  thing <- paste0("converted_data", multiple_files, " <- convert_stored_data(P = P, num_timechunks = thousand_timesteps)")
  eval(parse(text = thing))
  
  too_complicated <- paste0("R", multiple_files, " <- create_plot_info(\"", info[[1]], "\", \"", info[[2]], "\")")
  eval(parse(text=too_complicated))
  
  sink(file = paste0(multiple_files, " - Parameters and Info"))
  #print(paste0("Number of Timesteps: ", info[[3]][1], ",\n Number of Populations: ", info[[3]][2], ",\n Population Size: ", info[[3]][3], ",\n Number of Syllables: ", info[[3]][4], ",\n Number of Syllable Positions Assigned to Specific Probability Levels: ", info[[3]][5], ",\n Number of Singers Sampled from One Population for Mating: ", info[[3]][7], ",\n Number of Singers Sampled from One Population for Tutoring: ", info[[3]][6], "Probability of Inheriting Curiosity Accurately: ", info[[3]][8], ",\n Probability of Learning Syllables Accurately from Parent: ", info[[3]][10], ",\n Probability of Learning Syllables Accurately from Tutor", info[[3]][9], "\n, Probability of Picking up "))
  print(info[[3]])
  sink()
  
}



FolderName <- list.files(pattern = reg_spresh)[offset + 1]

results_directory <- str_replace_all(str_replace_all(FolderName, paste0("/", str_split(FolderName, "/")[[1]][8]),""), paste0("/", str_split(FolderName, "/")[[1]][7]),"")
setwd(results_directory)
dir.create(path = paste0(results_directory, "Results/", str_split(FolderName, "/")[[1]][8]))
results_directory <- paste0(results_directory, "Results/", str_split(FolderName, "/")[[1]][8])
setwd(results_directory)


simple_plots(R = R, Q = converted_data, simplification_factor = 10, extra_lines = TRUE)
full_plots(R = R, Q = converted_data, extra_lines = TRUE)


#library(rstudioapi)
#documentSave(getActiveDocumentContext()$id)

