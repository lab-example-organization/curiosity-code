life_cycle <- function(scMin, scMax, simStartDate, simNumber, runLength, 
                       SylLearnStyle, vertOblLearn, sylDist, curinh_value, 
                       number_populations, population_size, syllable_number,
                       number_of_syllables_per_probability_level, shifting_curstart) {
  
  VOtext = paste0(
    if(round(vertOblLearn[2]/0.1)==1) {
      round(vertOblLearn[2]/0.1,1)} else {round(vertOblLearn[2]/0.1,2)},"_",
    if(round(vertOblLearn[1]/0.95)==1) {
      round(vertOblLearn[1]/0.95,1)} else {round(vertOblLearn[1]/0.95,2)},"_V_",
    if(round(vertOblLearn[4]/0.01)==1) {
      round(vertOblLearn[4]/0.01,1)} else {round(vertOblLearn[4]/0.01,2)},"_",
    if(round(vertOblLearn[3]/0.1)==1) {
      round(vertOblLearn[3]/0.1,1)} else {round(vertOblLearn[3]/0.1,2)},"_O") # this is the text insert for the docnamez VO subsection
  
  if(VOtext == "1_1_V_1_1_O") {VOtext = "normVO"}
  
  if(scMin[1] == scMin[2] && scMin[2] == scMin[3] && scMin[3] == scMin[4] &&
     scMax[1] == scMax[2] && scMax[2] == scMax[3] && scMax[3] == scMax[4]) {
    
    curstart_ranges = paste0(scMin[1], "-", scMax[1])
    
  } else {
    
    #femrange = paste0(scMin[2], "-", scMax[2], "f")
    
    if(scMin[1] == scMin[3] && scMax[1] == scMax[3] && (scMax[2] != scMax[3] ||  scMin[2] != scMin[3])) {
      
      curstart_ranges <- paste0(scMin[2], "-", scMax[2], "f", "_", scMin[1], "-", scMax[1], "m")
    } else if(scMin[1] != scMin[3] || scMax[1] != scMax[3]) {
      
      curstart_ranges <- paste0(scMin[2], "-", scMax[2], "f", "_", scMin[1], "-", scMax[1], "mp1", "_", scMin[3], "-", scMax[3], "mp2")
    } else if(scMin[1] == scMin[2] && scMax[1] == scMax[2] && scMin[3] == scMin[4] && scMax[3] == scMax[4]) {
      
      curstart_ranges <- paste0(scMin[1], "-", scMax[1], "p1", "_", scMin[3], "-", scMax[3], "p2")
    }
  } # this is the text insert for the docnamez curstart ranges subsection
  
  if(curinh_value != 0.95) {curinh_output <- paste0(round(curinh_value/0.95,2), "_curinh")} else {curinh_output = ""}
  
  #rm(list=objects())
  parent_directory <- getwd()
  source("Source_Initial_Functions_Parameters.R")
  
  simParams <- define_parameters(num_timesteps = as.numeric(strsplit(runLength, "k")[[1]][1])*1000, num_pop = number_populations, 
                                 pop_size = population_size, sylnum = syllable_number, nsspl = number_of_syllables_per_probability_level, 
                                 one_pop_singers = c(10,10), 
                                 curlearnprob = curinh_value, learnprob = c(vertOblLearn[2], vertOblLearn[1]), 
                                 randlearnprob = c(vertOblLearn[4], vertOblLearn[3]), stand.dev = 2)
  
  moranObjects <- define_temp_data(simParams)
  
  sylreps <- initialize.sylrep(simParams, c(1,2), T, T)
  
  
  docnamez <- paste0(simStartDate,"_", simNumber, "_-_", runLength, "_",
                     SylLearnStyle, "_", VOtext, "_", sylDist, "_",
                     curstart_ranges,"_c", curinh_output) # equal syllable range
  #190211_160_100k_nsL_7_0.316_V_10_1.5_O_eq_sylrng_c
  
  curiosity_level <- initialize.curiosity(simParams, 
                                          #popXmale,popXfemale,popYmale,popYfemale...
                                          scMin, 
                                          scMax)
  
  day.tuh <- recordvariable.initialize(simParams, timestep_fraction = (simParams$num_timesteps/1000))
  
  source("Source_Life_Cycle_Functions.R")
  
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
                                     moran = moranObjects,
                                     sylrep_object = sylreps)
      
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
    FolderName <- store_timesteps(prameters = simParams,
                                  filename = thousand_timesteps, 
                                  object_record = day.tuh, 
                                  saved_stuff = stuff_to_save)
    if((thousand_timesteps==(simParams$num_timesteps/1000))&&(single_timestep==1000)) {
      #file_sink = paste0("180814", "_", thousand_timesteps, ".txt")
      sink(file = paste0(parent_directory, "/", shifting_curstart, "sim_data.txt"), append = TRUE)
      print(FolderName)
      sink()
      #stop("It's Done, Yo!")
    }
  }
}
#print("it's starting!")
multi_runs <- function(shifting_curstart) {
  
  #for(shifting_curstart in 1:250) 
  params <- yaml.load_file("params.yaml")
  #source("Source_Life_Cycle.R")
  number_of_runs <- as.numeric(params[[13]]$number_of_runs)
  cat(number_of_runs, file = paste0(shifting_curstart,"_number_of_runs.txt"), append = F)
  
  
  
  if(file.exists(paste0(shifting_curstart,"console_copy.txt"))) {file.remove(paste0(shifting_curstart,"console_copy.txt"))}
  if(file.exists(paste0(shifting_curstart,"sim_data.txt"))) {file.remove(paste0(shifting_curstart,"sim_data.txt"))}
  for(run_number in 1:number_of_runs) {
    saveRDS(object = run_number, file = "holdover_line.RData")
    if(run_number == 1) {
      sink(file = paste0(shifting_curstart,"sim_data.txt"), append = TRUE)
      #print(P)
      print("/please/ignore/this/line/like/you/always/do")
      sink()
    }
    life_cycle(
      scMin = c(
        params[[1]]$curstarts[[shifting_curstart]]$scMin[1],
        params[[1]]$curstarts[[shifting_curstart]]$scMin[2],
        params[[1]]$curstarts[[shifting_curstart]]$scMin[3],
        params[[1]]$curstarts[[shifting_curstart]]$scMin[4]),
      scMax = c(
        params[[1]]$curstarts[[shifting_curstart]]$scMax[1],
        params[[1]]$curstarts[[shifting_curstart]]$scMax[2],
        params[[1]]$curstarts[[shifting_curstart]]$scMax[3],
        params[[1]]$curstarts[[shifting_curstart]]$scMax[4]),
      simStartDate = params[[2]]$simStartDate,
      simNumber = params[[3]]$simNumber[[shifting_curstart]],
      runLength = params[[4]]$runLength,
      SylLearnStyle = params[[5]]$SylLearnStyle,
      vertOblLearn = c(
        params[[6]]$vertObLearn[[1]]$vertical[[1]]$learn,
        params[[6]]$vertObLearn[[1]]$vertical[[2]]$invent,
        params[[6]]$vertObLearn[[2]]$oblique[[1]]$learn,
        params[[6]]$vertObLearn[[2]]$oblique[[2]]$invent),
      sylDist = params[[7]]$sylDist, 
      curinh_value = params[[8]]$curinh_value,
      number_populations = params[[9]]$num_pop,
      population_size = params[[10]]$pop_size,
      syllable_number = params[[11]]$sylnum,
      number_of_syllables_per_probability_level = params[[12]]$num_sylls_per_prob_lvl,
      shifting_curstart = shifting_curstart
    )
    #rm(list=objects())
    run_number <- readRDS(file = "holdover_line.RData")
    #number_of_runs <- 10
    print(paste0("Run Number: ", run_number, ", comes right before (YYYY-MM-DD-HHMMSS): ", format(Sys.time(), "%F-%H%M%S")))
  }
  
  file.copy(from = paste0(shifting_curstart, "console_copy.txt"), to = paste0("../Results/",format(Sys.time(), "%F-%H%M%S"), shifting_curstart, "_console_copy.txt"))
  
  file.copy(from = pasate0(shifting_curstart, "sim_data.txt"), to = paste0("../Results/",format(Sys.time(), "%F-%H%M%S"), shifting_curstart, "_sim_data.txt"))
  
  source("Source_Figure_Produxn_Multiple_Runs.R")
  figProdMultRun()
}

#rm(list=objects())
# Transform name of data packet to folder name (example folder name: 2018-10-09-010315-GMT-variable-store)
# run loop that processes fn_doc_line from start of run to the end; 
# stitches together the pieces of individual runs, and plots them
# with an average line in black and the rest in various shades of grey.
###2018-10-09-010315-GMT-variable-store
#"2018-10-09" <- 6
#"01:55:51" <- 7
#"-GMT-variable-store"
