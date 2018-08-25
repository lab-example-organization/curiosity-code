# Amalgamation of functions; current nexus of code.
  # 

# The following text is used to select the curiosity values for initialize.curiosity below:
#zero_to_one_template <- c(0.00,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,
                             #1,  #2,  #3, #4,  #5, #6,  #7, #8,  #9,#10,

                          #0.45,0.49,0.5,0.51,0.55,0.59,0.6,0.65,0.7,0.75,
                            #11, #12,#13, #14, #15, #16,#17, #18,#19, #20,

                           #0.8,0.85,0.9,0.95,0.99,1.0)
                            #21, #22,#23, #24, #25,#26
#setwd(getwd())
setwd("/home/labuser/Documents/Parker Scratch Folder/Code/Curiosity Code")

rm(list=objects())
parent_directory <- getwd()
init_params <- paste0("source(\"", parent_directory, "/", "Source_Initial_Functions_Parameters.R\")")
eval(parse(text = init_params))

P <- Define.Parameters(num_timesteps = 10000, num_pop = 2, 
                       pop_size = 400, sylnum = 156, nsspl = 24, 
                       num_one.pop_singers_sampled = c(10,10), 
                       curlearnprob = 0.95, learnprob = c(0.1, 0.95), randlearnprob = c(0.01, 0.1), 
                       stand.dev = 2, curflux = 1, new.cur.threshold = 100)

sylreps <- initialize.sylrep(P, c(1, 2), T, T)


docnamez <- c("02_-_initial_tests")


curiosity_level <- initialize.curiosity(P, 
                                          #popXmale,popXfemale,popYmale,popYfemale...
                                        c(1,1,13,13), 
                                        c(13,13,26,26))

day.tuh <- recordvariable.initialize(P, timestep_fraction = (P$num_timesteps/1000))

funx_n_params <- paste0("source(\"", parent_directory, "/", "Source_Functions_Parameters.R\")")
eval(parse(text = funx_n_params))



datez <- Sys.Date()
deetz <- c(P$num_timesteps, P$num_pop, P$pop_size, P$sylnum, P$nsspl, P$num_one.pop_singers_sampled, P$curlearnprob, P$learnprob, P$randlearnprob, P$stand.dev, P$curflux, P$new.cur.threshold, dim(P$pop_calls_matrix), dim(P$pairing.pool), dim(P$curiosity_counter), dim(P$population_syll_probs), length(P$curiositybreaks), length(P$zero_to_one_template), dim(P$learning.pool))
names(deetz) <- c("P$num_timesteps", "P$num_pop", "P$pop_size", "P$sylnum", 
                      "P$nsspl", rep("P$num_one.pop_singers_sampled", 2), "P$curlearnprob", rep("P$learnprob", 2), 
                      rep("P$randlearnprob", 2), "P$stand.dev", "P$curflux", "P$new.cur.threshold", 
                      rep("dim(P$pop_calls_matrix)", 2), rep("dim(P$pairing.pool)", 4), 
                      rep("dim(P$curiosity_counter)", 2), rep("dim(P$population_syll_probs)", 2), 
                      "length(P$curiositybreaks)", "length(P$zero_to_one_template)", rep("dim(P$learning.pool)", 4))
stuff_to_save <- list(
  docnamez,
  datez,
  deetz
)

saveRDS(object = stuff_to_save, file = "metadata.RData")
rm(init_params, funx_n_params, datez, deetz, docnamez, stuff_to_save)

for(thousand_timesteps in 1:(P$num_timesteps/1000)) {
  for(single_timestep in 1:1000) {
    P <- sing.selection(P = P, curiosity_level = curiosity_level, context = 2, num_select_chances = c(100, 100), verbose_output = F)
    
    P <- make.offspring.calls(P = P)
    
    # curinh.row - calling either the row number or name of row for different curiosity inheritance patterns - 
      # 1: father; 2: mother; 3: same; 4:opposite
    P <- curiosity_learn(P = P, curlearnprob = 0.95, timestep = single_timestep, curinh.row = 1) 
    
    P <- syll_learn(P = P, context = 2, totally_new = FALSE) # context decides whether the learning is vertical (2) or oblique (1)
    
    P <- sing.selection(P = P, curiosity_level = curiosity_level, context = 1, num_select_chances = c(100, 100), verbose_output = F)
    
    P <- syll_learn(P = P, context = 1, totally_new = FALSE) # context decides whether the learning is vertical (2) or oblique (1)
    
    curiosity_level <- recuriosity.offspring(P = P)
    
    sylreps <- resylreps.offspring(P = P)
    
    day.tuh <- variable.archive(P = P, timestep = single_timestep)
    
  }
  #thousand_timesteps <- 1
  sink(file = "console_copy.txt", append = TRUE, split = TRUE)
  print(paste0("storing data packet ", thousand_timesteps, " at ", Sys.time()))
  sink()
  FolderName <- store_timesteps(filename = thousand_timesteps, object_record = day.tuh)
  if((thousand_timesteps==(P$num_timesteps/1000))&&(single_timestep==1000)) {
    #file_sink = paste0("180814", "_", thousand_timesteps, ".txt")
    
    sink(file = paste0(parent_directory, "/sim_data.txt"))
    print(P)
    print(FolderName)
    sink()
    stop("It's Done, Yo!")
    }
}

#setwd("/home/labuser/Documents/Parker Scratch Folder/Code/Curiosity Code")
data_visuals <- paste0("source(\"", parent_directory, "/", "Source_Visualizing_Data.R\")")
#data_visuals <- paste0("source(\"", parent_directory, "/", "Source_Visualizing_Data.R\")")
eval(parse(text = data_visuals))

parent_directory <- str_replace_all(FolderName, paste0("/", str_split(FolderName, "/")[[1]][8]),"")
setwd(parent_directory)
info <- readRDS(file = "metadata.RData")
setwd(FolderName)
converted_data <- convert_stored_data(P = P, num_timechunks = thousand_timesteps)
too_complicated <- paste0("R <- create_plot_info(\"", info[[1]], "\", \"", info[[2]], "\")")
eval(parse(text=too_complicated))

results_directory <- str_replace_all(str_replace_all(FolderName, paste0("/", str_split(FolderName, "/")[[1]][8]),""), paste0("/", str_split(FolderName, "/")[[1]][7]),"")
setwd(results_directory)
dir.create(path = paste0(results_directory, "Results/", str_split(FolderName, "/")[[1]][8]))
results_directory <- paste0(results_directory, "Results/", str_split(FolderName, "/")[[1]][8])
setwd(results_directory)

simple_plots(R = R, Q = converted_data, simplification_factor = 10)
full_plots(R = R, Q = converted_data)

sink(file = paste0("Parameters and Info"))
#print(paste0("Number of Timesteps: ", info[[3]][1], ",\n Number of Populations: ", info[[3]][2], ",\n Population Size: ", info[[3]][3], ",\n Number of Syllables: ", info[[3]][4], ",\n Number of Syllable Positions Assigned to Specific Probability Levels: ", info[[3]][5], ",\n Number of Singers Sampled from One Population for Mating: ", info[[3]][7], ",\n Number of Singers Sampled from One Population for Tutoring: ", info[[3]][6], "Probability of Inheriting Curiosity Accurately: ", info[[3]][8], ",\n Probability of Learning Syllables Accurately from Parent: ", info[[3]][10], ",\n Probability of Learning Syllables Accurately from Tutor", info[[3]][9], "\n, Probability of Picking up "))
print(info[[3]])
sink()
#library(rstudioapi)
#documentSave(getActiveDocumentContext()$id)

