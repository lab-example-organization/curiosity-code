##### setwd("/home/labuser/Documents/Parker Scratch Folder/Code/Curiosity Code")
  parent_directory <- getwd()
  #FolderName <- list.files(pattern = reg_spresh)[multiple_files + offset]
  
  data_visuals <- paste0("source(\"", parent_directory, "/", "Source_Visualizing_Data.R\")")
  #data_visuals <- paste0("source(\"", parent_directory, "/", "Source_Visualizing_Data.R\")")
  eval(parse(text = data_visuals))
  
  parent_directory <- str_replace_all(FolderName, paste0("/", str_split(FolderName, "/")[[1]][8]),"")
  setwd(parent_directory)
  info <- readRDS(file = "metadata.RData")
  setwd(FolderName)
  #converted_data <- convert_stored_data(P = P, num_timechunks = thousand_timesteps)
  
  converted_data <- convert_stored_data(P = P, num_timechunks = thousand_timesteps)
  
  
  too_complicated <- paste0("R <- create_plot_info(\"", info[[1]], "\", \"", info[[2]], "\")")
  eval(parse(text=too_complicated))
  
  sink(file = paste0("Parameters and Info.txt"))
  cat(paste0("Number of Timesteps: ", info[[3]][1], ",\n Number of Populations: ", info[[3]][2], ",\n Population Size: ", info[[3]][3], ",\n Number of Syllables: ", info[[3]][4], ",\n Number of Syllable Positions Assigned to Specific Probability Levels: ", info[[3]][5], ",\n Number of Singers Sampled from One Population for Mating: ", info[[3]][7], ",\n Number of Singers Sampled from One Population for Tutoring: ", info[[3]][6], "Probability of Inheriting Curiosity Accurately: ", info[[3]][8], ",\n Probability of Learning Syllables Accurately from Parent: ", info[[3]][10], ",\n Probability of Learning Syllables Accurately from Tutor: ", info[[3]][9], ",\n Probability of Picking up Random Extra Syllables from Parent: ", info[[3]][12], ",\n Probability of Picking up Random Extra Syllables from Tutor: ", info[[3]][11], ",\n Standard Deviation of Randomly-picked-up Sylls from Established Mean: ", info[[3]][13], ",\n Number of Rows in Population Calls Matrix: ", info[[3]][14], ",\n Number of Columns in Pop Calls Matrix: ", info[[3]][15], ",\n Pairing Pool Rows: ", info[[3]][16], ",\n Pairing Pool Columns: ", info[[3]][17], ",\n Pairing Pool Slices: ", info[[3]][18], ",\n Curiosity Counter Rows: ", info[[3]][19], ",\n Curiosity Counter Columns: ", info[[3]][20], ",\n Population Syllable Probability Rows: ", info[[3]][21], ",\n Population Probability Columns: ", info[[3]][22], ",\n Length of Curiosity Breaks Vector: ", info[[3]][23], ",\n Length of Zero to One Template: ", info[[3]][24], ",\n Learning Pool Rows: ", info[[3]][25], ",\n Learning Pool Columns: ", info[[3]][26], ",\n Learning Pool Slices: ", info[[3]][27]))
  sink()
  



FolderName <- list.files(pattern = reg_spresh)[offset + 1]

results_directory <- str_replace_all(str_replace_all(FolderName, paste0("/", str_split(FolderName, "/")[[1]][8]),""), paste0("/", str_split(FolderName, "/")[[1]][7]),"")
setwd(results_directory)
dir.create(path = paste0(results_directory, "Results/", str_split(FolderName, "/")[[1]][8]))
results_directory <- paste0(results_directory, "Results/", str_split(FolderName, "/")[[1]][8])
setwd(results_directory)


simple_plots(Q = converted_data, simplification_factor = 10, extra_lines = FALSE)
full_plots(R = R, Q = converted_data, extra_lines = FALSE)


#library(rstudioapi)
#documentSave(getActiveDocumentContext()$id)

