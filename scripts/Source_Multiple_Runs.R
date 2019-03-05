print("SMR")
savinStuff <- function(Parameters, Output_Filename, timestepCharacteristics) {
    datez <- Sys.Date()
    deetz <- c(Parameters$num_timesteps, Parameters$num_pop, Parameters$pop_size, Parameters$sylnum, 
               Parameters$nsspl, Parameters$one_pop_singers, Parameters$curlearnprob, 
               Parameters$learnprob, Parameters$randlearnprob, Parameters$stand.dev, dim(Parameters$pop_calls_matrix), 
               dim(timestepCharacteristics$pairing.pool), dim(Parameters$curiosity_counter), dim(Parameters$population_syll_probs), 
               length(Parameters$curiositybreaks), length(Parameters$zero_to_one_template), dim(timestepCharacteristics$learning.pool))
    names(deetz) <- c("Parameters$num_timesteps", "Parameters$num_pop", "Parameters$pop_size", "Parameters$sylnum", 
                      "Parameters$nsspl", rep("Parameters$one_pop_singers", 2), "Parameters$curlearnprob", 
                       rep("Parameters$learnprob", 2), rep("Parameters$randlearnprob", 2), 
                      "Parameters$stand.dev", rep("dim(Parameters$pop_calls_matrix)", 2), 
                       rep("dim(timestepCharacteristics$pairing.pool)", 3), rep("dim(Parameters$curiosity_counter)", 2), 
                       rep("dim(Parameters$population_syll_probs)", 2), "length(Parameters$curiositybreaks)", 
                      "length(Parameters$zero_to_one_template)", rep("dim(timestepCharacteristics$learning.pool)", 3))
    stuff_to_save <- list(
      docnamez=Output_Filename,
      datez=datez,
      deetz=deetz
    )
    return(stuff_to_save)
}


makeDocnamez <- function(scMin, scMax, simStartDate, simNumber, 
                         runLength, SylLearnStyle, vertOblLearn, 
                         sylDist, curinh_value, standDev) {

    VOtext = paste0(
      if(round(vertOblLearn[2]/0.1)==1) {
        round(vertOblLearn[2]/0.1,1)} else {round(vertOblLearn[2]/0.1,2)},"_",
      if(round(vertOblLearn[1]/0.95)==1) {
        round(vertOblLearn[1]/0.95,1)} else {round(vertOblLearn[1]/0.95,2)},"_V_",
      if(round(vertOblLearn[4]/0.01)==1) {
        round(vertOblLearn[4]/0.01,1)} else {round(vertOblLearn[4]/0.01,2)},"_",
      if(round(vertOblLearn[3]/0.1)==1) {
        round(vertOblLearn[3]/0.1,1)} else {round(vertOblLearn[3]/0.1,2)},"_O") # this is the text insert for the Output_Filename VO subsection
  
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
    
    if(curinh_value != 0.95) {curinh_output <- paste0(round(curinh_value/0.95,2), "_curinh")} else {curinh_output <- ""}
    
    if(standDev != 2) {stdDevDocName = paste0("_sd_", round(standDev/2,2))} else {stdDevDocName = ""}

    docnamez <- paste0(simStartDate,"_", simNumber, "_-_", runLength, "_",
                     SylLearnStyle, "_", VOtext, "_", sylDist, "_",
                     curstart_ranges,"_c", curinh_output, stdDevDocName) 
    #190211_160_100k_nsL_7_0.316_V_10_1.5_O_eq_sylrng_c

    return(docnamez)
  }


life_cycle <- function(scMin, scMax, simStartDate, simNumber, runLength, 
                       SylLearnStyle, vertOblLearn, sylDist, curinh_value, 
                       number_populations, population_size, syllable_number,
                       number_of_syllables_per_probability_level, standDev, 
                       shifting_curstart) {
  
  # argg <- c(as.list(environment()), list(...))
  # cat(argg, file = "doctemp.R")
  # simple_args_pipe <- source("doctemp.R")

  docnamez <- makeDocnamez(
            scMin = scMin, scMax = scMax, simStartDate = simStartDate, 
            simNumber = simNumber, runLength = runLength, 
            SylLearnStyle = SylLearnStyle, vertOblLearn = vertOblLearn, 
            sylDist = sylDist, curinh_value = curinh_value, standDev = standDev)

  #parent_directory <- getwd()
  source("Source_Initial_Functions_Parameters.R")
  
  simParams <- define_parameters(
    num_timesteps = as.numeric(strsplit(runLength, "k")[[1]][1])*1000, 
    num_pop = number_populations, 
    pop_size = population_size, 
    sylnum = syllable_number, 
    nsspl = number_of_syllables_per_probability_level, 
    one_pop_singers = c(10,10), 
    curlearnprob = curinh_value, 
    learnprob = c(vertOblLearn[2], vertOblLearn[1]), 
    randlearnprob = c(vertOblLearn[4], vertOblLearn[3]), 
    stand.dev = standDev
  )
  
  moranObjects <- define_temp_data(simParams)
  
  sylreps <- initialize.sylrep(simParams, c(1,2), T, T)

  #docnamez <- makeDocnamez(simple_args_pipe = simple_args_pipe)
  
  curiosity_level <- initialize.curiosity(simParams, scMin, scMax)
  
  day.tuh <- recordvariable.initialize(
    simParams, timestep_fraction = (simParams$num_timesteps/1000))
  
  source("Source_Life_Cycle_Functions.R")
  
  

  stuff_to_save <- savinStuff(Parameters = simParams, Output_Filename = docnamez, timestepCharacteristics = moranObjects)
  
  
  for(thousand_timesteps in 1:(simParams$num_timesteps/1000)) {
    for(single_timestep in 1:1000) {
      moranObjects <- sing.selection(parameters = simParams, moran = moranObjects, 
                                     curiosity_level = curiosity_level, 
                                     select_type = 2, sylrep_object = sylreps, 
                                     num_select_chances = c(100, 100), 
                                     verbose_output = F, interbreed = F)
      
      moranObjects <- make.offspring.calls(parameters = simParams, 
                                           moran = moranObjects)
      
      
      moranObjects <- curiosity_learn(parameters = simParams, moran = moranObjects, 
        # curinh.row - calling either the row number or name of row for different curiosity inheritance patterns - 
        # 1: father; 2: mother; 3: same; 4:opposite
                        timestep = single_timestep, inheritance_pattern = 1) 
      
      moranObjects <- syll_learn(parameters = simParams, moran = moranObjects, 
        # context decides whether the learning is vertical (2) or oblique (1)
                        select_type = 2, totally_new = FALSE, 
                        randlearn_context = 2, verbose = F) 
      
      moranObjects <- sing.selection(parameters = simParams, moran = moranObjects, 
                        curiosity_level = curiosity_level, select_type = 1, 
                        sylrep_object = sylreps, num_select_chances = c(100, 100), 
                        verbose_output = F, interbreed = F)
      
      moranObjects <- syll_learn(parameters = simParams, moran = moranObjects, 
        # context decides whether the learning is vertical (2) or oblique (1)
                        select_type = 1, totally_new = FALSE, 
                        randlearn_context = 2, verbose = F) 
      
      curiosity_level <- recuriosity.offspring(parameters = simParams, moran = moranObjects, 
                          curiosity_object = curiosity_level)
      
      sylreps <- resylreps.offspring(parameters = simParams, moran = moranObjects,
                  sylrep_object = sylreps)
      
      day.tuh <- variable.archive(parameters = simParams, moran = moranObjects, 
                  syllable_object = sylreps, curiosity_object = curiosity_level, 
                  data_container = day.tuh, timestep = single_timestep)
      
    }
    #thousand_timesteps <- 1
    project_directory <- paste0(strsplit(getwd(), "Code")[[1]][1], "Code/curiosity-code/")
    sink(file = paste0(project_directory, "source/temp/", shifting_curstart, "_console_copy.txt"),
     append = TRUE, split = TRUE)
    print(paste0("storing data packet ", thousand_timesteps, " at ", Sys.time()))
    sink()
    FolderName <- store_timesteps(
                    parameters = simParams,
                    filename = thousand_timesteps, 
                    object_record = day.tuh, 
                    saved_stuff = stuff_to_save,
                    syll_container = sylreps,
                    cur_container = curiosity_level)
    if((thousand_timesteps==(simParams$num_timesteps/1000))&&(single_timestep==1000)) {
      sink(file = paste0(project_directory, "source/temp/", shifting_curstart, "_sim_data.txt"), append = TRUE)
      print(FolderName)
      sink()
    }
  }
}
#print("it's starting!")

yamlDirLoad <- function(file, path = getwd()) {
  start <- getwd()
  setwd(path)
  yaml_container <- yaml.load_file(file)
  setwd(start)
  return(yaml_container)
}

smartRemove <- function(path){
  if(file.exists(path)) {
    file.remove(path)
  } else{
    print("coolsies")
  }
}

multi_runs <- function(shifting_curstart) {
  project_directory <- paste0(strsplit(getwd(), "Code")[[1]][1], "Code/curiosity-code/")
  setwd(paste0(strsplit(getwd(), "curiosity-code")[[1]][1], "curiosity-code/parameters/"))
  params <- yamlDirLoad(file = "params.yaml", path = getwd())
  setwd(paste0(project_directory, "scripts/"))
  number_of_runs <- as.numeric(params[[13]]$number_of_runs)
  
  print("number_of_runs is started")
  
  smartRemove(paste0(project_directory, "source/temp/", shifting_curstart,"_console_copy.txt"))
  smartRemove(paste0(project_directory, "source/temp/", shifting_curstart,"_sim_data.txt"))
  for(run_number in 1:number_of_runs) {
    if(run_number == 1) {
      setwd(paste0(project_directory, "source/temp/"))
      sink(file = paste0(shifting_curstart,"_sim_data.txt"), append = TRUE)
      print("/please/ignore/this/line/like/you/always/do")
      sink()
      setwd(paste0(project_directory, "scripts/"))
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
      standDev = as.numeric(params[[14]]$standard_deviation),
      shifting_curstart = shifting_curstart
    )
    print(paste0("Run Number: ", run_number, ", done at (YYYY-MM-DD-HHMMSS): ", (format(Sys.time(), "%F-%H%M%S"))))
  }
  print("about to archive console copy")
  file.copy(from = paste0(project_directory, "source/temp/", shifting_curstart, "_console_copy.txt"), 
              to = paste0(project_directory, "source/archive/", shifting_curstart, "_console_copy.txt"), overwrite = T)
  print("about to archive sim data")
  file.copy(from = paste0(project_directory, "source/temp/", shifting_curstart, "_sim_data.txt"), 
              to = paste0(project_directory, "source/archive/", shifting_curstart, "_sim_data.txt"), overwrite = T)
  
  source("Source_Figure_Produxn_Multiple_Runs.R")
  print("thing10")
  figProdMultRun(shifting_curstart = shifting_curstart, 
                 number_of_runs = number_of_runs)
}
