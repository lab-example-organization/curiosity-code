savinStuff <- function (Parameters, Output_Filename, moran) {
    datez <- Sys.Date ()
    deetz <- c (Parameters$num_timesteps, Parameters$num_pop, 
               Parameters$pop_size, Parameters$sylnum, Parameters$nsspl, 
               Parameters$one_pop_singers, Parameters$curlearnprob, 
               Parameters$learnprob, Parameters$randlearnprob, 
               Parameters$stand.dev, dim (Parameters$pop_calls_matrix), 
               dim (moran), dim (Parameters$curiosity_counter), 
               dim (Parameters$population_syll_probs), 
               length (Parameters$curiositybreaks), 
               length (Parameters$zero_to_one_template), dim (moran))
    names (deetz) <- c ("Parameters$num_timesteps", "Parameters$num_pop", 
                      "Parameters$pop_size", "Parameters$sylnum", 
                      "Parameters$nsspl", rep ("Parameters$one_pop_singers", 
                       2), "Parameters$curlearnprob", 
                       rep ("Parameters$learnprob", 2), 
                       rep ("Parameters$randlearnprob", 2), 
                       "Parameters$stand.dev", 
                       rep ("dim(Parameters$pop_calls_matrix)", 2), 
                       rep ("dim(moran)", 3), 
                       rep ("dim(Parameters$curiosity_counter)", 2), 
                       rep ("dim(Parameters$population_syll_probs)", 2), 
                       "length(Parameters$curiositybreaks)", 
                      "length(Parameters$zero_to_one_template)", 
                      rep ("dim(moran)", 3))
    stuff_to_save <- list (
      docnamez=Output_Filename,
      datez=datez,
      deetz=deetz
    )
    return (stuff_to_save)
}


makeDocnamez <- function (scMin, scMax, simNumber, 
                         runLength, SylLearnStyle, vertOblLearn, 
                         sylDist, curinh_value, standDev, simDate) {

  VOtext = paste0 (
    if (vertOblLearn[2]/0.1==1) {round (vertOblLearn[2]/0.1,1)
    } else {round (vertOblLearn[2]/0.1,2)},"_",
    if (vertOblLearn[1]/0.95==1) {round (vertOblLearn[1]/0.95,1)
    } else {round (vertOblLearn[1]/0.95,2)},"_V_",
    if (vertOblLearn[4]/0.01==1) {round (vertOblLearn[4]/0.01,1)
    } else {round (vertOblLearn[4]/0.01,2)},"_",
    if (vertOblLearn[3]/0.1==1) {round (vertOblLearn[3]/0.1,1)
    } else {round (vertOblLearn[3]/0.1,2)},"_O") 
      # this is the text insert for the Output_Filename VO subsection

  if (VOtext == "1_1_V_1_1_O") {VOtext = "normVO"}
  
  if (scMin[1] == scMin[2] && 
      scMin[2] == scMin[3] && 
      scMin[3] == scMin[4] &&
      scMax[1] == scMax[2] && 
      scMax[2] == scMax[3] && 
      scMax[3] == scMax[4]) {
    curstart_ranges = paste0(scMin[1], "-", scMax[1])
  } else {
    
    if (scMin[1] == scMin[3] && 
        scMax[1] == scMax[3] && 
       (scMax[2] != scMax[3] || 
        scMin[2] != scMin[3]) &&
        scMin[2] == scMin[4] &&
        scMax[2] == scMax[4]) {
      # 1-7_f_7-13m
      curstart_ranges <- paste0(scMin[2], "-", scMax[2], "f", "_", scMin[1], "-", scMax[1], "m")
    
    } else if (scMin[2] == scMin[4] &&
               scMax[2] == scMax[4] &&
              (scMin[1] != scMin[3] ||
               scMax[1] != scMax[3])) {
      # 1-7f_1-7mp1_7-13mp2
      curstart_ranges <- paste0(scMin[2], "-", scMax[2], "f", "_", scMin[1], "-", 
                          scMax[1], "mp1", "_", scMin[3], "-", scMax[3], "mp2")
    
    } else if (scMin[1] == scMin[3] &&
               scMax[1] == scMax[3] &&
              (scMin[2] != scMin[4] || 
               scMax[2] != scMax[4])) {
      # 1-7f_1-7mp1_7-13mp2
      curstart_ranges <- paste0(scMin[2], "-", scMax[2], "fp1", "_", scMin[4], "-", 
                          scMax[4], "fp2", "_", scMin[3], "-", scMax[3], "m")
    
    } else if (scMin[1] == scMin[2] && 
               scMax[1] == scMax[2] && 
               scMin[1] != scMin[4] && 
               scMax[1] != scMax[4] &&
               scMin[3] == scMin[4] && 
               scMax[3] == scMax[4] &&
               scMin[3] != scMin[1] && 
               scMax[3] != scMax[1] &&
               scMin[2] != scMin[3] && 
               scMax[2] != scMax[3] && 
               scMin[2] != scMin[4] && 
               scMax[2] != scMax[4]) {
      # 1-7p1_7-13p2
      curstart_ranges <- paste0(scMin[1], "-", scMax[1], "p1", "_", scMin[3], "-", scMax[3], "p2")
    
    }
  } # this is the text insert for the docnamez curstart ranges subsection
  
  if(curinh_value != 0.95) {curinh_output <- paste0(round(curinh_value/0.95,2), "_curinh")} else {curinh_output <- ""}
  
  if(standDev != 2) {stdDevDocName = paste0("_sd_", round(standDev/2,2))} else {stdDevDocName <- ""}

  simStartDate <- simDate

  DocumentName <- paste0(simStartDate,"_", simNumber, "_-_", runLength, "_",
                    SylLearnStyle, "_", VOtext, "_", sylDist, "_",
                    curstart_ranges,"_c", curinh_output, stdDevDocName) 
  #190211_160_100k_nsL_7_0.316_V_10_1.5_O_eq_sylrng_c

  return(DocumentName)
}


life_cycle <- function(
  scMin, scMax, simNumber, runLength, SylLearnStyle, vertOblLearn, sylDist, 
  curinh_value, number_populations, population_size, syllable_number,
  number_sylls_probability_level, standDev, SimNumberLC, curinh_style, 
  recordingSimpFact, one_pop_singers = c(10,10), curinhProportion, 
  directoryDate, invasion, invPopSize, invStyle) {
  
  docnamez <- makeDocnamez(
    scMin = scMin, scMax = scMax, simNumber = simNumber, runLength = runLength,
    SylLearnStyle = SylLearnStyle, vertOblLearn = vertOblLearn, 
    sylDist = sylDist, curinh_value = curinh_value, standDev = standDev, 
    simDate = directoryDate)

  #parent_directory <- getwd()
  source(file.path("scripts", "Source_Initial_Functions_Parameters.R"))
  
  simParams <- define_parameters(
    num_timesteps = as.numeric(strsplit(runLength, "k")[[1]][1])*1000, 
    num_pop = number_populations, pop_size = population_size, 
    sylnum = syllable_number, nsspl = number_sylls_probability_level, 
    one_pop_singers = one_pop_singers, curlearnprob = curinh_value, 
    learnprob = c(vertOblLearn[2], vertOblLearn[1]), 
    randlearnprob = c(vertOblLearn[4], vertOblLearn[3]), 
    stand.dev = standDev, curinhProportion = curinhProportion
  )
  
  ##### Timestep Data Object (TDO)

  moranObjects <- define_temp_data(simParams)
  # pairing_pool <- define_temp_data(simParams, 2)
    
  sylreps <- initialize.sylrep(simParams, c(1,2), T, T)
  
  curiosity_level <- initialize.curiosity(simParams, scMin, scMax)
  
  sylrep_rowcol <- recordvariable.initialize(
      simParams, recordingSimpFact, variableID = 1)
  
  sylrep_dstbxn <- recordvariable.initialize(
      simParams, recordingSimpFact, variableID = 2)

  curity_mean_t <- recordvariable.initialize(
      simParams, recordingSimpFact, variableID = 3)

  curity_repert <- recordvariable.initialize(
      simParams, recordingSimpFact, variableID = 4)

  source(file.path("scripts", "Source_Life_Cycle_Functions.R"))
  
  

  stuff_to_save <- savinStuff(Parameters = simParams, 
                              Output_Filename = docnamez, 
                              moran = moranObjects)
  
  
  
  for(thousand_timesteps in 1:(simParams$num_timesteps/1000)) {
    
    # Invasion Setup

    # if (
    #   invasion &&
    #   thousand_timesteps == 2
    # ) {
    #   for (population in 1:simParams$num_pop) {
    #     pop_subset <- sample(simParams$pop_calls_matrix[1,],invasion)
    #     if (invStyle = "curiosity") {
    #       curiosity_level
    #     } else if (invStyle = "sylrep") {}
    #   }
      
    # }

    for(simplify in 1:(1000/recordingSimpFact)) {
      for(single_timestep in 1:recordingSimpFact) {

        # Mate selection based on song characteristics
        moranObjects <- sing.selection(parameters = simParams, 
                                      tempMoran = moranObjects, 
                                      curiosity_level = curiosity_level, 
                                      select_type = 2, 
                                      sylrep_object = sylreps, 
                                      num_select_chances = c(100, 100), 
                                      verbose_output = F, 
                                      interbreed = F)
        
        # Locate new birb positions in population data, store in TDO
        moranObjects <- make.offspring.calls(parameters = simParams, 
                                            temporMan = moranObjects)
        
        # Add noise to inherited curiosity trait, store temporarily
        moranObjects <- curiosity_learn(parameters = simParams, 
                                        tempObjects = moranObjects, 
                                        inheritance_pattern = curinh_style) 
        
        # 
        moranObjects <- syll_learn(parameters = simParams, 
                                  moranData = moranObjects, 
                                  select_type = 2,
                                  totally_new = FALSE, 
                                  randlearn_context = 2, 
                                  verbose = F) 
        
        moranObjects <- sing.selection(parameters = simParams, 
                                      tempMoran = moranObjects, 
                                      curiosity_level = curiosity_level, 
                                      select_type = 1, 
                                      sylrep_object = sylreps, 
                                      num_select_chances = c(100, 100), 
                                      verbose_output = F, 
                                      interbreed = F)
        
        moranObjects <- syll_learn(parameters = simParams, 
                                  moranData = moranObjects, 
                                  select_type = 1, 
                                  totally_new = FALSE, 
                                  randlearn_context = 2, 
                                  verbose = F) 
        
        curiosity_level <- recuriosity.offspring(parameters = simParams, objectMoran = moranObjects, 
                            curiosity_object = curiosity_level)
        
        sylreps <- resylreps.offspring(parameters = simParams, 
                                       moranObjectTemp = moranObjects,
                                       sylrep_object = sylreps)
        
        # recordvariable archiving
        
      }

      sylrep_rowcol <- sylrep_rowcol.archive(
        parameters = simParams, 
        data_container = sylrep_rowcol, 
        syllable_object = sylreps,
        timestep = simplify)

      sylrep_dstbxn <- sylrep_dstbxn.archive(
        parameters = simParams, 
        data_container = sylrep_dstbxn, 
        syllable_object = sylreps,
        timestep = simplify)

      curity_mean_t <- curity_mean_t.archive(
        parameters = simParams, 
        tempData = moranObjects, 
        data_container = curity_mean_t, 
        curiosity_object = curiosity_level,
        timestep = simplify)

      curity_repert <- curity_repert.archive(
        parameters = simParams,
        data_container = curity_repert, 
        curiosity_object = curiosity_level,
        timestep = simplify)


    }



    # print("console_copy_sink")
    sink(file = file.path("source", "temp", paste0(SimNumberLC, "_console_copy.txt")), 
      append = TRUE, split = TRUE)
    print(paste0("Sim Number ", strsplit(docnamez, "_")[[1]][2], " - storing data packet ", 
      thousand_timesteps, " at ", Sys.time()))
    sink()

    if (thousand_timesteps == 1) {
      run_timedate <- format(Sys.time(), "%F-%H%M%S")
    }

    # print("FolderName_make")
    FolderName <- store_timesteps(
                    parameters = simParams,
                    filename = thousand_timesteps, 
                    rowcol = sylrep_rowcol,
                    dstbxn = sylrep_dstbxn,
                    mean_t = curity_mean_t,
                    repert = curity_repert,
                    saved_stuff = stuff_to_save,
                    syll_container = sylreps,
                    cur_container = curiosity_level,
                    run_timedate = run_timedate,
                    FolderName = file.path("results", stuff_to_save$docnamez, "variable_store", paste0(run_timedate, "-GMT-variable-store")))

      #     FolderName <- 

    # print("sim_data_sink")
    if((thousand_timesteps==(simParams$num_timesteps/1000))&&(simplify==1000/recordingSimpFact)&&(single_timestep==recordingSimpFact)) {
      sink(file = file.path("source", "temp", paste0(SimNumberLC, "_sim_data.txt")), append = TRUE)
      print(FolderName)
      sink()
    }
  }
}

# for(simplify in 1:(1000/recordingSimpFact)) {
#       for(single_timestep in 1:recordingSimpFact) {


archiveSimFiles <- function(path, filename, archive = FALSE, new_dir = FALSE){
  if(file.exists(path)) {
    if(archive) {
      archivePrefix <- gsub('[-: ]', '', substring(Sys.time(), 3))
      if(new_dir) {
        file.copy(from=file.path(path, filename), to=file.path("source", "archive", new_dir, filename))
        file.rename(from=file.path("source", "archive", new_dir, filename), 
          to=file.path("source", "archive", new_dir, paste0(archivePrefix, "_", filename)))  
      } else {
        file.copy(from=file.path(path, filename), to=file.path("source", "archive", filename))
        file.rename(from=file.path("source", "archive", filename), 
          to=file.path("source", "archive", paste0(archivePrefix, "_", filename)))
      }
    } else {
      if(new_dir) {
        file.copy(from=file.path(path, filename), to=file.path("source", "archive", new_dir, filename))
      } else {
        file.copy(from=file.path(path, filename), to=file.path("source", "archive", filename))
      }
    }
    file.remove(file.path(path, filename))
    # print("")
  } # else{print("")}
}

multi_runs <- function(shifting_curstart, paramsSource, dirDate, seedNumber) {
  # # Load the C++ functions
  #sourceCpp(file.path('cpp_source', 'median.cpp'))
  #sourceCpp(file.path('cpp_source', 'rowSums.cpp'))
  #sourceCpp(file.path('cpp_source', 'sort.cpp'))

  # dirDate and seedNumber space
  set.seed(seedNumber + shifting_curstart)
  # shifting_curstart <- 1
  # paramsFile <- c("params.yaml")
  # paramsSource = paramsFile
  params <- yaml.load_file(file.path("parameters", paramsSource))
  number_of_reps <- as.numeric(params$number_of_reps)
  
  archiveSimFiles(path=file.path("source", "temp"), 
    filename=paste0(shifting_curstart,"_console_copy.txt"), 
    archive=TRUE, new_dir = F)
  archiveSimFiles(path=file.path("source", "temp"), 
    filename=paste0(shifting_curstart,"_sim_data.txt"), 
    archive=TRUE, new_dir = F)
  
  for(rep_number in 1:number_of_reps) {
    if(rep_number == 1) {
      sink(file = file.path(
          "source", "temp", paste0(shifting_curstart,"_sim_data.txt")
        ), append = FALSE)
      print("/please/ignore/this/line/like/you/always/do")
      sink()
    }
  if (params$screwedUp) {
   subsetOrSequence <- params$simNumberStart[shifting_curstart]
  } else {
    subsetOrSequence <- params$simNumberStart + (shifting_curstart - 1)
  }

   life_cycle(
      scMin = c(
        params$curstarts[[shifting_curstart]]$scMin[1],
        params$curstarts[[shifting_curstart]]$scMin[2],
        params$curstarts[[shifting_curstart]]$scMin[3],
        params$curstarts[[shifting_curstart]]$scMin[4]),
      scMax = c(
        params$curstarts[[shifting_curstart]]$scMax[1],
        params$curstarts[[shifting_curstart]]$scMax[2],
        params$curstarts[[shifting_curstart]]$scMax[3],
        params$curstarts[[shifting_curstart]]$scMax[4]),
      simNumber = subsetOrSequence,
<<<<<<< HEAD
      # simNumber = params$simNumberStart + (shifting_curstart - 1),
=======
>>>>>>> 4f6b2cc0cebfc0fdb1a52e4ec8a41104ea8a6a4e
      runLength = params$runLength,
      SylLearnStyle = params$SylLearnStyle,
      vertOblLearn = c(
        params$vertObLearn$vertical$learn,
        params$vertObLearn$vertical$invent,
        params$vertObLearn$oblique$learn,
        params$vertObLearn$oblique$invent),
      sylDist = params$sylDist,
      curinh_value = params$curinh_value,
      number_populations = params$num_pop,
      population_size = params$pop_size,
      syllable_number = params$sylnum,
      number_sylls_probability_level = params$num_sylls_per_prob_lvl,
      standDev = as.numeric(params$standard_deviation),
      SimNumberLC = shifting_curstart,
      curinh_style = params$curinh_pattern,
      recordingSimpFact = params$RecordSimplifyFactor,
      one_pop_singers = params$one_pop_singers,
      curinhProportion = params$curinhDistribution,
      directoryDate = dirDate,
      invasion = params$traitInvasion,
      invPopSize = invasionPopSize,
      invStyle = invasionStyle
    )
    print(paste0("Rep Number: ", rep_number, ", done at (YYYY-MM-DD-HHMMSS): ", (format(Sys.time(), "%F-%H%M%S"))))
  }
  
  source(file.path("scripts", "Source_Figure_Produxn_Multiple_Runs.R"))
  figProdMultRun(specificSimNumber = shifting_curstart, 
                 number_of_repeats = number_of_reps,
                 paramsSource = paramsSource)
}






