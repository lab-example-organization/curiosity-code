##### setwd("/home/labuser/Documents/Parker Scratch Folder/Code/Curiosity Code")
parent_directory <- getwd()
for(run_visual in 1:number_of_runs) {
  if(run_visual == 1) {
    FolderName <- format(Sys.time(), "%F-%H%M%S")
    dir.create(file.path(parent_directory, paste0(FolderName, "-GMT-multirun-output")))
    FolderName <- paste0(parent_directory, "/", FolderName, "-GMT-multirun-output")
    saveRDS(object = multiRun_folderList, file = paste0(FolderName, "/folderList.RData"))
    
  } # makes the folder for multirun results, saves the multiRun_folderList there.
  
  
  setwd(multiRun_folderList[run_visual])
  data_visuals <- paste0("source(\"", parent_directory, "/", "Source_Visualizing_Data.R\")") ####### rm(list=objects())!!!!!!
  eval(parse(text = data_visuals))
  run_number_directory <- getwd()
  setwd(strsplit(run_number_directory, "20")[[1]][1])
  multiRun_folderList <- readRDS(file = paste0(getwd(), "/", tail(list.files(pattern = "multirun"),1), "/folderList.RData"))
  parent_directory <- getwd()
  #results_directory <- paste0(str_split(parent_directory, "Curiosity")[[1]][1], "Results/")
  setwd(run_number_directory)
  
  info <- readRDS(file = "metadata.RData")
  #converted_data <- convert_stored_data(P = P, num_timechunks = thousand_timesteps)
  data_converter <- paste0("converted_data", run_visual, " <- convert_stored_data(P = P, num_timechunks = thousand_timesteps)")
  eval(parse(text=data_converter))
  
  R <- create_plot_info(info[[2]], info[[1]])
  
  stuff <- paste(paste0("sink(file = \"", multiple_files, " - Parameters and Info\")"), 
                 "cat(paste0(\"Number of Timesteps: \", info[[3]][1], \"\nNumber of Populations: \", info[[3]][2], \"\nPopulation Size: \", info[[3]][3], \"\nNumber of Syllables: \", info[[3]][4], \"\nNumber of Syllable Positions Assigned to Specific Probability Levels: \", info[[3]][5], \"\nNumber of Singers Sampled from One Population for Mating: \", info[[3]][7], \"\nNumber of Singers Sampled from One Population for Tutoring: \", info[[3]][6], \"Probability of Inheriting Curiosity Accurately: \", info[[3]][8], \"\nProbability of Learning Syllables Accurately from Parent: \", info[[3]][10], \"\nProbability of Learning Syllables Accurately from Tutor: \", info[[3]][9], \"\nProbability of Picking up Random Extra Syllables from Parent: \", info[[3]][12], \"\nProbability of Picking up Random Extra Syllables from Tutor: \", info[[3]][11], \"\nStandard Deviation of Randomly-picked-up Sylls from Established Mean: \", info[[3]][13], \"\nNumber of Rows in Population Calls Matrix: \", info[[3]][14], \"\nNumber of Columns in Pop Calls Matrix: \", info[[3]][15], \"\nPairing Pool Rows: \", info[[3]][16], \"\nPairing Pool Columns: \", info[[3]][17], \"\nPairing Pool Slices: \", info[[3]][18], \"\nCuriosity Counter Rows: \", info[[3]][19], \"\nCuriosity Counter Columns: \", info[[3]][20], \"\nPopulation Syllable Probability Rows: \", info[[3]][21], \"\nPopulation Probability Columns: \", info[[3]][22], \"\nLength of Curiosity Breaks Vector: \", info[[3]][23], \"\nLength of Zero to One Template: \", info[[3]][24], \"\nLearning Pool Rows: \", info[[3]][25], \"\nLearning Pool Columns: \", info[[3]][26], \"\nLearning Pool Slices: \", info[[3]][27]))", 
                 "sink()", sep = "\n")
  eval(parse(text=stuff))
  

  
  sylrepblahz <- paste0("sylrepz", run_visual, " <- split_data(converted_data, 1)")
  sdstbxblahn <- paste0("sdstbxn", run_visual, " <- split_data(converted_data, 2)")
  cursitblahy <- paste0("cursity", run_visual, " <- split_data(converted_data, 3)")
  curhisblaht <- paste0("curhist", run_visual, " <- split_data(converted_data, 4)")
  
  eval(parse(text=sylrepblahz))
  eval(parse(text=sdstbxblahn))
  eval(parse(text=cursitblahy))
  eval(parse(text=curhisblaht))
  
  
  paste_split_data_runs(data_subset, num_runs = 10, also_mean = TRUE)
  
  
  simple_multiplots(R = R1, Q = converted_data, simplification_factor = 100, extra_lines = TRUE)
  
  




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

sylrepblahz <- paste0("sylrepz", 1:mult_file_length, " <- split_data(converted_data", 1:mult_file_length, ", 1)")
sdstbxblahn <- paste0("sdstbxn", 1:mult_file_length, " <- split_data(converted_data", 1:mult_file_length, ", 2)")
cursitblahy <- paste0("cursity", 1:mult_file_length, " <- split_data(converted_data", 1:mult_file_length, ", 3)")
curhisblaht <- paste0("curhist", 1:mult_file_length, " <- split_data(converted_data", 1:mult_file_length, ", 4)")

eval(parse(text=sylrepblahz))
eval(parse(text=sdstbxblahn))
eval(parse(text=cursitblahy))
eval(parse(text=curhisblaht))

#split_data(data_conglomerate = , data_subset = )
#sylrepz <- split_data(data_subset = 1)
#sdstbxn <- split_data(data_subset = 2)
#cursity <- split_data(data_subset = 3)
#curhist <- split_data(data_subset = 4)

paste_split_data_runs(data_subset, num_runs = 10, also_mean = TRUE)



#FolderName <- list.files(pattern = reg_spresh)[offset + 1]
#setwd(results_directory)

simple_multiplots(R = R1, Q = converted_data, simplification_factor = 100, extra_lines = TRUE)
#full_plots(R = R, Q = converted_data, extra_lines = TRUE)

#library(rstudioapi)
#documentSave(getActiveDocumentContext()$id)
