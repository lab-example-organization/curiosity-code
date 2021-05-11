savinstuff <- function (parameters, output_filename, moran) {
    datez <- Sys.Date ()
    deetz <- c (parameters$num_timesteps, parameters$num_pop,
               parameters$pop_size, parameters$sylnum, parameters$nsl,
               parameters$one_pop_singers, parameters$curlearnprob,
               parameters$learnprob, parameters$randlearnprob,
               parameters$stand.dev, dim (parameters$pop_calls_matrix),
               dim (moran), dim (parameters$curiosity_counter),
               dim (parameters$population_syll_probs),
               length (parameters$curiositybreaks),
               length (parameters$zero_to_one_template), dim (moran))
    names (deetz) <- c ("parameters$num_timesteps", "parameters$num_pop",
                      "parameters$pop_size", "parameters$sylnum",
                      "parameters$nsl", rep ("parameters$one_pop_singers",
                       2), "parameters$curlearnprob",
                       rep ("parameters$learnprob", 2),
                       rep ("parameters$randlearnprob", 2),
                       "parameters$stand.dev",
                       rep ("dim (parameters$pop_calls_matrix)", 2),
                       rep ("dim (moran)", 3),
                       rep ("dim (parameters$curiosity_counter)", 2),
                       rep ("dim (parameters$population_syll_probs)", 2),
                       "length (parameters$curiositybreaks)",
                      "length (parameters$zero_to_one_template)",
                      rep ("dim (moran)", 3))
    stuff_to_save <- list (
      docnamez = output_filename,
      datez = datez,
      deetz = deetz
    )
    return (stuff_to_save)
}
print ("line 33")

makedocnamez <- function (scmin, scmax, simnumber,
                         runlength, syllearnstyle, vertoblearn,
                         syldist, curinh_value, standdev, simdate) {

  votext <- paste0 (
    if (vertoblearn[2] / 0.1 == 1) {
      round (vertoblearn[2 ] /0.1, 1)
    } else {
      round (vertoblearn[2] / 0.1, 2)}, "_",
    if (vertoblearn[1] / 0.95 == 1) {
      round (vertoblearn[1] / 0.95, 1)
    } else {
      round (vertoblearn[1] / 0.95, 2)}, "_V_",
    if (vertoblearn[4] / 0.01 == 1) {
      round (vertoblearn[4] / 0.01, 1)
    } else {
      round (vertoblearn[4] / 0.01, 2)}, "_",
    if (vertoblearn[3] / 0.1 == 1) {
      round (vertoblearn[3] / 0.1, 1)
    } else {
      round (vertoblearn[3] / 0.1, 2)
    }
  )
  votext <- paste0 (votext, "_O")
      # this is the text insert for the output_filename VO subsection

  if (votext == "1_1_V_1_1_O") {
    votext <- "normVO"
  }
print ("line 64")
  if (scmin[1] == scmin[2] &&
      scmin[2] == scmin[3] &&
      scmin[3] == scmin[4] &&
      scmax[1] == scmax[2] &&
      scmax[2] == scmax[3] &&
      scmax[3] == scmax[4]) {
    curstart_ranges = paste0 (scmin[1], "-", scmax[1])
  } else {

    if (scmin[1] == scmin[3] &&
        scmax[1] == scmax[3] &&
       (scmax[2] != scmax[3] ||
        scmin[2] != scmin[3]) &&
        scmin[2] == scmin[4] &&
        scmax[2] == scmax[4]) {
      # 1-7_f_7-13m
      curstart_ranges <- paste0 (scmin[2], "-", scmax[2], "f", "_",
                                scmin[1], "-", scmax[1], "m")

    } else if (scmin[2] == scmin[4] &&
               scmax[2] == scmax[4] &&
              (scmin[1] != scmin[3] ||
               scmax[1] != scmax[3])) {
      # 1-7f_1-7mp1_7-13mp2
      curstart_ranges <- paste0 (scmin[2], "-", scmax[2], "f", "_",
                                scmin[1], "-", scmax[1], "mp1", "_",
                                scmin[3], "-", scmax[3], "mp2")

    } else if (scmin[1] == scmin[3] &&
               scmax[1] == scmax[3] &&
              (scmin[2] != scmin[4] ||
               scmax[2] != scmax[4])) {
      # 1-7f_1-7mp1_7-13mp2
      curstart_ranges <- paste0 (scmin[2], "-", scmax[2], "fp1", "_",
                                scmin[4], "-", scmax[4], "fp2", "_",
                                scmin[3], "-", scmax[3], "m")

    } else if ( (scmin[1] == scmin[2] &&
                scmax[1] == scmax[2]) &&
               (scmin[3] == scmin[4] &&
                scmax[3] == scmax[4]) &&
               (scmin[1] != scmin[4] ||
                scmax[1] != scmax[4])) {
      # 1-7p1_7-13p2
      curstart_ranges <- paste0 (scmin[1], "-", scmax[1], "p1", "_",
                                scmin[3], "-", scmax[3], "p2")

    } else if (scmin[3] == scmin[4] &&
               scmax[3] == scmax[4] &&
              (scmin[1] != scmin[4] ||
               scmax[1] != scmax[4]) &&
              (scmin[2] != scmin[1] ||
               scmax[2] != scmax[1])) {
      # 1-7p1_7-13p2
      curstart_ranges <- paste0 (scmin[1], "-", scmax[1], "mp1", "_",
                                scmin[2], "-", scmax[2], "fp1", "_",
                                scmin[4], "-", scmax[4], "p2")

    } else if (scmin[1] == scmin[2] &&
               scmax[1] == scmax[2] &&
             ( (scmin[1] != scmin[4] ||
               scmax[1] != scmax[4]) ||
              (scmin[2] != scmin[4] ||
               scmax[2] != scmax[4])) &&
              (scmin[3] != scmin[4] ||
               scmax[3] != scmax[4])) {
      # 1-7p1_7-13p2
      curstart_ranges <- paste0 (scmin[2], "-", scmax[2], "p1", "_",
                                 scmin[3], "-", scmax[3], "mp2", "_",
                                 scmin[4], "-", scmax[4], "fp2")

    } else {
      curstart_ranges <- paste0 ("wellThisIsUnexpected")
    }
  } # this is the text insert for the docnamez curstart ranges subsection

  if (curinh_value != 0.95) {
    curinh_output <- paste0 (round (curinh_value / 0.95, 2), "_curinh")
  } else {
    curinh_output <- ""
  }

  if (standdev != 2) {
    stddevdocname = paste0 ("_sd_", round (standdev / 2, 2))
  } else {
    stddevdocname <- ""
  }

  simstartdate <- simdate

  documentname <- paste0 (simstartdate, "_", simnumber, "_-_", runlength, "_",
                    syllearnstyle, "_", votext, "_", syldist, "_",
                    curstart_ranges, "_c", curinh_output, stddevdocname)
  #190211_160_100k_nsL_7_0.316_V_10_1.5_O_eq_sylrng_c

  return (documentname)
}
print ("line 162")
restart_from_save <- function (
  parameters, # "params" in multi_runs
  inputpattern
) {

  if (typeof (inputpattern) != "character") {
    stop ("input pattern must be data type 'string'")
  }
  if (length (inputpattern) > 1) {
    stop ("length of inputpattern should be 1. Right?")
  }

  relevantpaths <- file.path ("results", list.files (
    file.path ("results"), pattern = inputpattern))

  pathlist <- list.files (
    file.path (relevantpaths, "variable_store"))

  # somekindaoutput <- array (0, c (parameters[[8]],
  #   parameters[[9]], parameters[[10]] + 1, length (pathlist)))

  somekindaoutput <- vector ("list", length (pathlist))

  # num_pop, pop_size,    sylnum
  for (i in 1 : length (pathlist)) {

    somekindaoutput[[i]] <- vector ("list", 2)

    endcur <- readRDS (file.path (relevantpaths, "variable_store",
      paste0 (pathlist [i], "/end_cursty.RData")))
    endrep <- readRDS (file.path (relevantpaths, "variable_store",
      paste0 (pathlist [i], "/end_sylbls.RData")))

    # somekindaoutput[, , 1 : parameters[[10]], i] <- aperm (endrep, c (2, 1, 3))
    # somekindaoutput[, , parameters[[10]] + 1, i] <- aperm (endcur, c (2, 1))

    somekindaoutput[[i]][[1]] <- endrep
    somekindaoutput[[i]][[2]] <- endcur

  }
  return (somekindaoutput)
}
print ("line 205")
invasion_parameters_curiosity <- function (
  ip = invpop,
  isx = invsex,
  ips = invpopsize,
  itv = invtraitvalue,
  curiosity_container = curiosity_level,
  someparameters = simparams
) {

  ifelse (ip == "focal", population <- 1, population <- 2)

  if (isx != "both") {

    ifelse (isx == "male", thesex <- 1, thesex <- 2)

  } else {
    thesex <- c (1, 2)
    ips <- ips / 2
  }

  pop_subset <- sample (someparameters$pop_calls_matrix [thesex,], ips)

  if (! (itv)) {

    # this substitutes the current curiosity value
    # for the population subset, with "1 - current curiosity value"
    curiosity_container [pop_subset [
      1 : ips], population] <- 1 - curiosity_container [
      pop_subset [1 : ips], population]

    # return (curiosity_container)

  } else {

    # this substitutes the current curiosity value
    # for the population subset, with "1 - current curiosity value"
    curiosity_container [pop_subset [
      1 : ips], population] <- sample ( (
        100 * (itv [1] - (itv [2] / 2))) : (100 * (
          itv [1] + (itv [2] / 2))), length (pop_subset), replace = TRUE) / 100

    # return (curiosity_container)

  }

  return (curiosity_container)

}
print ("line 254")
invasion_parameters_sylrep <- function (
  ip = invpop,
  isx = invsex,
  ips = invpopsize,
  itv = invtraitvalue,
  sylrep_container = sylreps,
  someparameters = simparams
) {

  ifelse (ip == 'focal', population <- 1, population <- 2)

  if (isx != 'both') {

    ifelse (isx == 'male', thesex <- 1, thesex <- 2)

  } else {
    thesex <- c (1, 2)
    ips <- ips / 2
  }

  pop_subset <- sample (someparameters$pop_calls_matrix [thesex,], ips)

  # if (ifocus == 'sylrep') {

    if (! (itv)) {
      for (variable in 1 : ips) {
        thing <- (someparameters$sylnum + 1) - which (
          sylrep_container [pop_subset [variable], , population] == 1)
        # stuff <- (someparameters$sylnum + 1) - thing
        sylrep_container [pop_subset [variable], thing [
          1 : length (thing)], population] <- 1

      # return (sylrep_container)

      }
    } else {

      # this is the point at which the string of values (
        # c (1,2)) produces the necessary

      for (variable in 1 : ips) {
        sylrep_size <- length (which (sylrep_container [
          pop_subset [variable], , population] == 1))
        sylrep_mean <- round (itv [1] * someparameters$sylnum)
        sample_size <- itv [2]

        building_a_sylrep = unique (
          sort (
            sample (1 : 156,sample_size,TRUE,c (
              rep (0.001,(sylrep_mean - (sample_size/2))),
              # from point in sylnum where the target is
              # located (fraction * sylnum), with 1/2 the
              # sylrep range subracted, = the number of syllables
              # that fill in the beginning of the hypothetical sylrep
              # before the ones desired (defined by itv[1] and itv[2])

              rep (0.75,(itv[2]*(1/0.75))),
              # the syllables desired, populating a slightly larger
              # area with a slightly lower chance of any one syllable
              # being selected, so hopefully the number evens out to
              # the same range of the target sylrep size

              rep (0.001,someparameters$sylnum - (
                itv[2]*(1/0.75)) - (sylrep_mean - (sample_size/2)))
              # other two subtracted from total = leftovers
        ))))

        while (! (length (building_a_sylrep) %in% c (
          sample_size - 10 : sample_size + 10))) {

          if (length (building_a_sylrep) > (sample_size + 10)) {

            sample_size <- sample_size - 1

            building_a_sylrep = unique (
              sort (
                sample(1 : 156,sample_size - 5,TRUE,c (
                  rep (0.001,(sylrep_mean - ( (sample_size - 5)/2))),

                  rep (0.75,(itv[2]*(1/0.75))),

                  rep (0.001,someparameters$sylnum - (
                    itv[2]*(1/0.75)) - (
                      sylrep_mean - ( (sample_size - 5)/2)))
            ))))

          } else if (length (building_a_sylrep) < (sample_size - 10)) {

            sample_size <- sample_size + 1

            building_a_sylrep = unique (
              sort (
                sample(1 : 156,sample_size + 5,TRUE,c (
                  rep (0.001,(sylrep_mean - ( (sample_size + 5)/2))),

                  rep (0.75,(itv[2]*(1/0.75))),

                  rep (0.001,someparameters$sylnum - (
                    itv[2]*(1/0.75)) - (
                      sylrep_mean - ( (sample_size + 5)/2)))
            ))))
          }
          # building_a_sylrep <- unique(sort (sample(
            # 1 : 156,50,TRUE,c (rep (0.001,20),rep (0.75,80),rep (0.001,56)))))
        }
        # thing <- (someparameters$sylnum + 1) - which (
          # sylrep_container [pop_subset [variable], , population] == 1)
        # stuff <- (someparameters$sylnum + 1) - thing
        sylrep_container [pop_subset [variable], building_a_sylrep[
          1 : length (building_a_sylrep)], population] <- 1
      }

    # return (sylrep_container)
      # sample(x, size, replace = FALSE, prob = NULL)

      # > unique(sort (sample(1 : 156,50,TRUE,c (
        # rep (0.01,20),rep (0.5,80),rep (0.01,56)))))
      #  [1]  21  24  25  26  27  29  31  37  39  48
      #       49  50  52  54  56  57  58  59  64
      # [20]  65  66  73  74  76  77  78  81  82  83
      #       85  86  90  93  94  95  96  99 100
    }

  return (sylrep_container)

}
print ("line 381")
life_cycle <- function (
  scmin, scmax, simnumber, runlength, syllearnstyle, vertoblearn, syldist,
  curinh_value, number_populations, population_size, syllable_number,
  number_sylls_probability_level, standdev, curinh_style,
  recordingsimpfact, one_pop_singers = c (10,10), curinhproportion,
  directorydate, invasion, invpopsize, invstyle, invpop, invsex,
  invtraitvalue, invktmstps, initfromlastrun = FALSE, lastrunobject = FALSE) {

  docnamez <- makedocnamez (
    scmin = scmin, scmax = scmax, simnumber = simnumber, runlength = runlength,
    syllearnstyle = syllearnstyle, vertoblearn = vertoblearn,
    syldist = syldist, curinh_value = curinh_value, standdev = standdev,
    simdate = directorydate)

  #parent_directory <- getwd ()
  source (file.path ("scripts", "Source_Initial_Functions_Parameters.R"))

  simparams <- define_parameters (
    num_timesteps = as.numeric (strsplit (runlength, "k") [[1]][1]) * 1000,
    num_pop = number_populations, pop_size = population_size,
    sylnum = syllable_number, nsl = number_sylls_probability_level,
    one_pop_singers = one_pop_singers, curlearnprob = curinh_value,
    learnprob = c (vertoblearn[2], vertoblearn[1]),
    randlearnprob = c (vertoblearn[4], vertoblearn[3]),
    stand.dev = standdev, curinhproportion = curinhproportion
  )

  ##### Timestep Data Object (TDO)

  moranobjects <- define_temp_data (simparams)
  # pairing_pool <- define_temp_data (simparams, 2)
  if (initfromlastrun) {
    sylreps <- initialize.sylrep (parameters_is = simparams,
      population.pattern = c (1,2), pastrunobject_is = lastrunobject,
      eqpop = TRUE, eqsex = TRUE, pastruninit_is = TRUE)
    curiosity_level <- initialize.curiosity (
      parameters_ic = simparams, cur.min = scmin, cur.max = scmax,
      pastrunobject_ic = lastrunobject, pastruninit_ic = TRUE)
  } else {
    sylreps <- initialize.sylrep (parameters_is = simparams,
      population.pattern = c (1,2), eqpop = TRUE, eqsex = TRUE)
    curiosity_level <- initialize.curiosity (
      parameters_ic = simparams, cur.min = scmin, cur.max = scmax)
  }

print ("line 427")
  sylrep_rowcol <- recordvariable.initialize (
      parameters_rvi = simparams, recsimfct = recordingsimpfact, variableid = 1)

  sylrep_dstbxn <- recordvariable.initialize (
      parameters_rvi = simparams, recsimfct = recordingsimpfact, variableid = 2)

  curity_mean_t <- recordvariable.initialize (
      parameters_rvi = simparams, recsimfct = recordingsimpfact, variableid = 3)

  # let's make indices 13 and 14 on dimension 1... these are the measures of variance in curiosity level in both male and female subpopulations

  curity_repert <- recordvariable.initialize (
      parameters_rvi = simparams, recsimfct = recordingsimpfact, variableid = 4)

  source (file.path ("scripts", "Source_Life_Cycle_Functions.R"))



  stuff_to_save <- savinstuff (parameters = simparams,
                              output_filename = docnamez,
                              moran = moranobjects)



  for (thousand_timesteps in 1 : (simparams$num_timesteps/1000)) {

    if (invasion && (thousand_timesteps == invktmstps)) {
      if (invstyle == 'curiosity') {
        curiosity_level <- invasion_parameters_curiosity (
          ip = invpop,
          isx = invsex,
          ips = invpopsize,
          itv = invtraitvalue,
          curiosity_container = curiosity_level,
          someparameters = simparams
        )
      } else {
        sylreps <- invasion_parameters_sylrep (
          ip = invpop,
          isx = invsex,
          ips = invpopsize,
          itv = invtraitvalue,
          sylrep_container = sylreps,
          someparameters = simparams
        )
      }
    }

    for (simplify in 1 : (1000/recordingsimpfact)) {
      for (single_timestep in 1 : recordingsimpfact) {

        # Mate selection based on song characteristics
        moranobjects <- sing.selection (parameters_sing_selection = simparams,
                                      temp_data_sing_selection = moranobjects,
                                      curiosity_level = curiosity_level,
                                      select_type = "mate",
                                      sylrep_object = sylreps,
                                      num_select_chances = c (40, 40),
                                      verbose_output = FALSE,
                                      interbreed = FALSE)

        # Locate new birb positions in population data, store in TDO
        moranobjects <- make.offspring.calls (params_OC = simparams,
                                            temp_data_OC = moranobjects)

        # Add noise to inherited curiosity trait, store temporarily
        moranobjects <- curiosity_learn (parameters_curiosity_learn = simparams,
                                        temp_data_curiosity_learn = moranobjects,
                                        curinh_pattern = curinh_style)

        #
        moranobjects <- syll_learn (params_SL = simparams,
                                  temp_data_SL = moranobjects,
                                  select_type = "mate",
                                  totally_new = FALSE,
                                  randlearn_context = 2,
                                  verbose = FALSE)

        moranobjects <- sing.selection (parameters_sing_selection = simparams,
                                      temp_data_sing_selection = moranobjects,
                                      curiosity_level = curiosity_level,
                                      select_type = "tutor",
                                      sylrep_object = sylreps,
                                      num_select_chances = c (40, 40),
                                      verbose_output = FALSE,
                                      interbreed = FALSE)

        moranobjects <- syll_learn (params_SL = simparams,
                                  temp_data_SL = moranobjects,
                                  select_type = "tutor",
                                  totally_new = FALSE,
                                  randlearn_context = 2,
                                  verbose = FALSE)

        curiosity_level <- recuriosity.offspring (params_RC = simparams,
                                            temp_data_RC = moranobjects,
                                            curiosity_object = curiosity_level)

        sylreps <- resylreps.offspring (params_RS = simparams,
                                       temp_data_RS = moranobjects,
                                       sylrep_object = sylreps)

        # recordvariable archiving

      }

      sylrep_rowcol <- sylrep_rowcol.archive(
        parameters_src_archive = simparams,
        data_container = sylrep_rowcol,
        syllable_object = sylreps,
        timestep = simplify)

      sylrep_dstbxn <- sylrep_dstbxn.archive(
        parameters_sdb_archive = simparams,
        data_container = sylrep_dstbxn,
        syllable_object = sylreps,
        timestep = simplify)

      curity_mean_t <- curity_mean_t.archive(
        parameters_cmt_archive = simparams,
        temp_data_cmt_archive = moranobjects,
        data_container = curity_mean_t,
        curiosity_object = curiosity_level,
        timestep = simplify)

      # let's make indices 13 and 14 on dimension 1... these are the measures of variance in curiosity level in both male and female subpopulations

      curity_repert <- curity_repert.archive(
        parameters_crp_archive = simparams,
        data_container = curity_repert,
        curiosity_object = curiosity_level,
        timestep = simplify)


    }



    # print ("console_copy_sink")
    sink (file = file.path ("source", "temp", paste0 (
      simnumber, "_console_copy.txt")), append = TRUE, split = TRUE)
    print (paste0 ("Sim Number ", strsplit (docnamez,
      "_") [[1]][2], " - storing data packet ",
      thousand_timesteps, " at ", Sys.time()))
    sink ()

    if (thousand_timesteps == 1) {
      run_timedate <- format (Sys.time(), "%F-%H%M%S")
    }


    foldername <- store_timesteps (
                    parameters_storetimesteps = simparams,
                    filename = thousand_timesteps,
                    rowcol = sylrep_rowcol,
                    dstbxn = sylrep_dstbxn,
                    mean_t = curity_mean_t,
                    # let's make indices 13 and 14 on dimension 1... these are the measures of variance in curiosity level in both male and female subpopulations
                    repert = curity_repert,
                    saved_stuff = stuff_to_save,
                    syll_container = sylreps,
                    cur_container = curiosity_level,
                    run_timedate = run_timedate,
                    foldername = file.path (
                      "results", stuff_to_save$docnamez, "variable_store",
                      paste0 (run_timedate, "-GMT-variable-store")))

    if ( (thousand_timesteps==(simparams$num_timesteps/1000)
      )&&(
        simplify==1000/recordingsimpfact
      )&&(single_timestep==recordingsimpfact)) {
      sink (file = file.path ("source", "temp",
        paste0 (simnumber, "_sim_data.txt")), append = TRUE)
      print (foldername)
      sink ()
    }
  }
}

print ("line 607")

archivesimfiles <- function (path, filename,
  archive = FALSE, new_dir = FALSE) {
  if (file.exists (path)) {
    if (archive) {
      archiveprefix <- gsub ('[-: ]', '', substring (Sys.time (), 3))
      if (new_dir) {
        file.copy (from = file.path (path, filename), to = file.path (
          "source", "archive", new_dir, filename))
        file.rename (from = file.path ("source", "archive", new_dir,
          filename), to=file.path ("source", "archive", new_dir,
          paste0 (archiveprefix, "_", filename)))
      } else {
        file.copy (from=file.path (path, filename),
          to=file.path ("source", "archive", filename))
        file.rename(from=file.path ("source", "archive", filename),
          to=file.path ("source",
          "archive", paste0 (archiveprefix, "_", filename)))
      }
    } else {
      if (new_dir) {
        file.copy (from = file.path (path, filename),
          to = file.path ("source", "archive", new_dir, filename))
      } else {
        file.copy (from = file.path (path, filename),
          to = file.path ("source", "archive", filename))
      }
    }
    file.remove(file.path (path, filename))

  }
}
print ("line 640")
multi_runs <- function (shifting_curstart, paramssource,
  dirdate, seednumber) {
print ("line 643")
  set.seed (seednumber + shifting_curstart)

  params <- yaml.load_file (file.path ("parameters", paramssource))
  number_of_reps <- as.numeric (params$number_of_reps)

  archivesimfiles (path = file.path ("source", "temp"),
    filename = paste0 (shifting_curstart,"_console_copy.txt"),
    archive = TRUE, new_dir = FALSE)
  archivesimfiles (path = file.path ("source", "temp"),
    filename = paste0 (shifting_curstart,"_sim_data.txt"),
    archive = TRUE, new_dir = FALSE)

  # This wrapped up the restart_from_save function,
  # so that life_cycle has last-run data as an accessible object
  # lastrun_init <- array (0, c (1,1,1,number_of_reps))

  lastrun_init <- list ()

  if (params$lastruninit) {
    if (length (params$lastrunid) > 1) {
      lastrun_init <- restart_from_save (parameters = params,
        inputpattern = params$lastrunid [shifting_curstart])
    } else {
      lastrun_init <- restart_from_save (parameters = params,
        inputpattern = params$lastrunid)
    }
  }

  for (rep_number in 1 : number_of_reps) {

    if (params$indrunredo == TRUE) {
      subsetorsequence <- params$simnumberstart [shifting_curstart]
      singleormixture <- params$curinhdistribution [shifting_curstart]
    } else {
      subsetorsequence <- params$simnumberstart + (shifting_curstart - 1)
      singleormixture <- params$curinhdistribution
    }

    if (length (params$curinh_pattern) != 1) {
      curinh_binary <- params$curinh_pattern[shifting_curstart]
    } else {
      curinh_binary <- params$curinh_pattern
    }

    if (rep_number == 1) {

      sink (file = file.path (
          "source", "temp", paste0 (subsetorsequence,"_sim_data.txt")
        ), append = FALSE)
      print ("empty line that helps the code work - figure it out later, if it's worth it")
      sink ()

      # file.create (file.path ("source", "temp", paste0 (
      #   shifting_curstart,"_sim_data.txt")))
    }

    life_cycle(
      scmin = c (
        params$curstarts [[shifting_curstart]]$scmin [1],
        params$curstarts [[shifting_curstart]]$scmin [2],
        params$curstarts [[shifting_curstart]]$scmin [3],
        params$curstarts [[shifting_curstart]]$scmin [4]),
      scmax = c (
        params$curstarts [[shifting_curstart]]$scmax [1],
        params$curstarts [[shifting_curstart]]$scmax [2],
        params$curstarts [[shifting_curstart]]$scmax [3],
        params$curstarts [[shifting_curstart]]$scmax [4]),
      simnumber = subsetorsequence,
      # simnumber = params$simnumberstart + (shifting_curstart - 1),
      runlength = params$runlength,
      syllearnstyle = params$syllearnstyle,
      vertoblearn = c (
        params$vertoblearn$vertical$learn,
        params$vertoblearn$vertical$invent,
        params$vertoblearn$oblique$learn,
        params$vertoblearn$oblique$invent),
      syldist = params$syldist,
      curinh_value = params$curinh_value,
      number_populations = params$num_pop,
      population_size = params$pop_size,
      syllable_number = params$sylnum,
      number_sylls_probability_level = params$num_sylls_per_prob_lvl,
      standdev = as.numeric (params$standard_deviation),
      curinh_style = curinh_binary,
      recordingsimpfact = params$recordsimplifyfactor,
      one_pop_singers = params$one_pop_singers,
      curinhproportion = singleormixture, # only used if curinh_pattern = 5
      directorydate = dirdate,
      invasion = params$traitinvasion,
      invktmstps = params$invasionthoutmstps,
      invpopsize = params$invasionpopsize,
      invstyle = params$invasionstyle,
      invpop = params$invasionpop,
      invsex = params$invasionsex,
      invtraitvalue = params$invasiontraitvalue,
      initfromlastrun = params$lastruninit,
      lastrunobject = lastrun_init[[rep_number]]
      # lastrunobject = lastrun_init[, , , rep_number]
    )
    print (paste0 ("Replicate Run # ",
      rep_number, ", done at: ",
      (format (Sys.time(), "%F-%H%M%S"))))
  }

  source (file.path ("scripts", "Source_Figure_Produxn_Multiple_Runs.R"))
  figprodmultrun (specificsimnumber = subsetorsequence,
                 number_of_repeats = number_of_reps,
                 paramssource = paramssource)
}
print ("line 753")
