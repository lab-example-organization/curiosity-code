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
#setwd("/home/labuser/Documents/Parker Scratch Folder/Code/Curiosity Code") <- Lab Public Computer
#setwd("/home/rundstpj/projects/curiosity_model/Code/Curiosity_Code/curiosity-code") <- Server
#setwd("/home/rundstpj/projects/curiosity_model/Code/Curiosity_Code") <- old server address
#setwd("/Users/bryangitschlag/Downloads/Lab_Notebook/GitHub/curiosity-code") <- macbook air
#print("Life Cycle Start")
rm(list=objects())
parent_directory <- getwd()
source("Source_Initial_Functions_Parameters.R")

simParams <- define_parameters(num_timesteps = 10000, num_pop = 2, 
                       pop_size = 400, sylnum = 156, nsspl = 12, 
                       one_pop_singers = c(10,10), 
                       curlearnprob = 0.95, learnprob = c(0.1, 0.95), 
                       randlearnprob = c(0.01, 0.1), stand.dev = 2)

moranObjects <- define_temp_data(simParams)

sylreps <- initialize.sylrep(simParams, c(1, 2), T, T)


docnamez <- c("190201_37_-_10k_nsL_1_1_V_1_1_O_oppsyl_1-7_c") # equal syllable range
#100k_nsL_7_0.316_V_10_1.5_O_eq_sylrng

curiosity_level <- initialize.curiosity(simParams, 
                                          #popXmale,popXfemale,popYmale,popYfemale...
                                        c(1,1,1,1), 
                                        c(26,26,26,26))

day.tuh <- recordvariable.initialize(simParams, timestep_fraction = (simParams$num_timesteps/1000))

source("Source_Functions_Parameters.R")

datez <- Sys.Date()
deetz <- c(simParams$num_timesteps, simParams$num_pop, simParams$pop_size, simParams$sylnum, simParams$nsspl, simParams$one_pop_singers, simParams$curlearnprob, 
           simParams$learnprob, simParams$randlearnprob, simParams$stand.dev, dim(simParams$pop_calls_matrix), dim(moranObjects$pairing.pool), 
           dim(simParams$curiosity_counter), dim(simParams$population_syll_probs), length(simParams$curiositybreaks), length(simParams$zero_to_one_template), dim(moranObjects$learning.pool))
names(deetz) <- c("simParams$num_timesteps", "simParams$num_pop", "simParams$pop_size", "simParams$sylnum", 
                      "simParams$nsspl", rep("simParams$one_pop_singers", 2), "simParams$curlearnprob", rep("simParams$learnprob", 2), 
                      rep("simParams$randlearnprob", 2), "simParams$stand.dev", 
                      rep("dim(simParams$pop_calls_matrix)", 2), rep("dim(moranObjects$pairing.pool)", 3), 
                      rep("dim(simParams$curiosity_counter)", 2), rep("dim(simParams$population_syll_probs)", 2), 
                      "length(simParams$curiositybreaks)", "length(simParams$zero_to_one_template)", rep("dim(moranObjects$learning.pool)", 3))
stuff_to_save <- list(
  docnamez=docnamez,
  datez=datez,
  deetz=deetz
)
#cat(paste0("Number of Timesteps: ", info[[3]][1], ",\n Number of Populations: ", info[[3]][2], ",\n Population Size: ", info[[3]][3], ",\n Number of Syllables: ", info[[3]][4], ",\n Number of Syllable Positions Assigned to Specific Probability Levels: ", info[[3]][5], ",\n Number of Singers Sampled from One Population for Mating: ", info[[3]][7], ",\n Number of Singers Sampled from One Population for Tutoring: ", info[[3]][6], "Probability of Inheriting Curiosity Accurately: ", info[[3]][8], ",\n Probability of Learning Syllables Accurately from Parent: ", info[[3]][10], ",\n Probability of Learning Syllables Accurately from Tutor: ", info[[3]][9], "\n, Probability of Picking up Random Extra Syllables from Parent: ", info[[3]][12], "\n, Probability of Picking up Random Extra Syllables from Tutor: ", info[[3]][11], ",\n Standard Deviation of Randomly-picked-up Sylls from Established Mean: ", info[[3]][13], ",\n Number of Rows in Population Calls Matrix: ", info[[3]][14], ",\n Number of Columns in Pop Calls Matrix: ", info[[3]][15], ",\n Pairing Pool Rows: ", info[[3]][16], ",\n Pairing Pool Columns: ", info[[3]][17], ",\n Pairing Pool Slices: ", info[[3]][18], ",\n Curiosity Counter Rows: ", info[[3]][19], ",\n Curiosity Counter Columns: ", info[[3]][20], ",\n Population Syllable Probability Rows: ", info[[3]][21], ",\n Population Probability Columns: ", info[[3]][22], ",\n Length of Curiosity Breaks Vector: ", info[[3]][23], ",\n Length of Zero to One Template: ", info[[3]][24], ",\n Learning Pool Rows: ", info[[3]][25], ",\n Learning Pool Columns: ", info[[3]][26], ",\n Learning Pool Slices: ", info[[3]][27]))

for(thousand_timesteps in 1:(simParams$num_timesteps/1000)) {
  for(single_timestep in 1:1000) {
    moranObjects <- sing.selection(uniparmaters = simParams, 
                                   moran = moranObjects, 
                                   curiosity_level = curiosity_level, 
                                   select_type = 2, 
                                   sylrep_object = sylreps, 
                                   num_select_chances = c(100, 100), 
                                   verbose_output = F, 
                                   interbreed = F)
    
    moranObjects <- make.offspring.calls(parmters = simParams, 
                                         moran = moranObjects)
    
    # curinh.row - calling either the row number or name of row for different curiosity inheritance patterns - 
      # 1: father; 2: mother; 3: same; 4:opposite
    moranObjects <- curiosity_learn(patamerers = simParams, 
                                    moran = moranObjects, 
                                    timestep = single_timestep, 
                                    inheritance_pattern = 1) 
    
    moranObjects <- syll_learn(params = simParams, 
                            moran = moranObjects, 
                            select_type = 2, 
                            totally_new = FALSE, 
                            randlearn_context = 2, 
                            verbose = F) # context decides whether the learning is vertical (2) or oblique (1)
    
    moranObjects <- sing.selection(uniparmaters = simParams, 
                                   moran = moranObjects, 
                                   curiosity_level = curiosity_level, 
                                   select_type = 1, 
                                   sylrep_object = sylreps, 
                                   num_select_chances = c(100, 100), 
                                   verbose_output = F, 
                                   interbreed = F)
    
    moranObjects <- syll_learn(params = simParams, 
                            moran = moranObjects, 
                            select_type = 1, 
                            totally_new = FALSE, 
                            randlearn_context = 2, 
                            verbose = F) # context decides whether the learning is vertical (2) or oblique (1)
    
    curiosity_level <- recuriosity.offspring(parmaters = simParams, 
                                             moran = moranObjects, 
                                             curiosity_object = curiosity_level)
    
    sylreps <- resylreps.offspring(paraterms = simParams, 
                                   moran = moranObjects)
    
    day.tuh <- variable.archive(parameters = simParams, 
                                moran = moranObjects, 
                                syllable_object = sylreps, 
                                curiosity_object = curiosity_level, 
                                data_container = day.tuh, 
                                timestep = single_timestep)
    
  }
  #thousand_timesteps <- 1
  sink(file = "console_copy.txt", append = TRUE, split = TRUE)
  print(paste0("storing data packet ", thousand_timesteps, " at ", Sys.time()))
  sink()
  FolderName <- store_timesteps(prameters = simParams, filename = thousand_timesteps, object_record = day.tuh)
  if((thousand_timesteps==(simParams$num_timesteps/1000))&&(single_timestep==1000)) {
    #file_sink = paste0("180814", "_", thousand_timesteps, ".txt")
    sink(file = paste0(parent_directory, "/sim_data.txt"), append = TRUE)
    print(FolderName)
    sink()
    #stop("It's Done, Yo!")
    }
}