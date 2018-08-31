##### setwd("/home/labuser/Documents/Parker Scratch Folder/Code/Curiosity Code")
mult_file_length <- 12
reg_spresh <- "2018"
offset <- 1

parent_directory <- getwd()
FolderName <- list.files(pattern = reg_spresh)[1:mult_file_length + offset]

data_visuals <- paste0("source(\"", parent_directory, "/", "Source_Visualizing_Data.R\")")

#data_visuals <- paste0("source(\"", parent_directory, "/", "Source_Visualizing_Data.R\")")
eval(parse(text = data_visuals))
parent_directory <- getwd()
results_directory <- paste0(str_split(parent_directory, "Curiosity")[[1]][1], "Results/")

for(multiple_files in 1:mult_file_length) {
  
  
  #parent_directory <- str_replace_all(FolderName, paste0("/", str_split(FolderName, "/")[[1]][8]),"")
  #setwd(parent_directory)
  setwd(paste0(parent_directory, FolderName[multiple_files], "/"))
  info_maker <- paste0("info", multiple_files, " <- readRDS(file = \"metadata.RData\")")
  eval(parse(text=info_maker))
  
  #converted_data <- convert_stored_data(P = P, num_timechunks = thousand_timesteps)
  
  thing <- paste0("converted_data", multiple_files, " <- convert_stored_data(P = P, num_timechunks = thousand_timesteps)")
  eval(parse(text = thing))
  
  too_complicated <- paste0("R", multiple_files, " <- create_plot_info(", paste0("info", multiple_files, "[[2]]"), ", ", paste0("info", multiple_files, "[[1]]"), ")")
  eval(parse(text=too_complicated))
  setwd(results_directory)
  if(multiple_files==1) {
    dir.create(path = paste0(results_directory, info[[2]], "_-_", info[[1]]))
    tenrun_directory <- paste0(results_directory, info[[2]], "_-_", info[[1]])
  }
  setwd(tenrun_directory)
  stuff <- paste(paste0("sink(file = \"", multiple_files, " - Parameters and Info\")"), 
                 "cat(paste0(\"Number of Timesteps: \", info[[3]][1], \"\nNumber of Populations: \", info[[3]][2], \"\nPopulation Size: \", info[[3]][3], \"\nNumber of Syllables: \", info[[3]][4], \"\nNumber of Syllable Positions Assigned to Specific Probability Levels: \", info[[3]][5], \"\nNumber of Singers Sampled from One Population for Mating: \", info[[3]][7], \"\nNumber of Singers Sampled from One Population for Tutoring: \", info[[3]][6], \"Probability of Inheriting Curiosity Accurately: \", info[[3]][8], \"\nProbability of Learning Syllables Accurately from Parent: \", info[[3]][10], \"\nProbability of Learning Syllables Accurately from Tutor: \", info[[3]][9], \"\nProbability of Picking up Random Extra Syllables from Parent: \", info[[3]][12], \"\nProbability of Picking up Random Extra Syllables from Tutor: \", info[[3]][11], \"\nStandard Deviation of Randomly-picked-up Sylls from Established Mean: \", info[[3]][13], \"\nNumber of Rows in Population Calls Matrix: \", info[[3]][14], \"\nNumber of Columns in Pop Calls Matrix: \", info[[3]][15], \"\nPairing Pool Rows: \", info[[3]][16], \"\nPairing Pool Columns: \", info[[3]][17], \"\nPairing Pool Slices: \", info[[3]][18], \"\nCuriosity Counter Rows: \", info[[3]][19], \"\nCuriosity Counter Columns: \", info[[3]][20], \"\nPopulation Syllable Probability Rows: \", info[[3]][21], \"\nPopulation Probability Columns: \", info[[3]][22], \"\nLength of Curiosity Breaks Vector: \", info[[3]][23], \"\nLength of Zero to One Template: \", info[[3]][24], \"\nLearning Pool Rows: \", info[[3]][25], \"\nLearning Pool Columns: \", info[[3]][26], \"\nLearning Pool Slices: \", info[[3]][27]))", 
                 "sink()", sep = "\n")
  eval(parse(text=stuff))
}



FolderName <- list.files(pattern = reg_spresh)[offset + 1]


setwd(results_directory)



simple_plots(R = R1, Q = converted_data, simplification_factor = 10, extra_lines = TRUE)
#full_plots(R = R, Q = converted_data, extra_lines = TRUE)


#library(rstudioapi)
#documentSave(getActiveDocumentContext()$id)

