# You Should be Here To: Repeat A Lot of Simulations in Parallel
#
# __________  _____ __________    _____   ____     ____     ___________ ____
# \______   \/  _  \\______   \  /  _  \ |    |   |    |    \_   _____/|    |
#  |     ___/  /_\  \|       _/ /  /_\  \|    |   |    |     |    __)_ |    |
#  |    |  /    |    \    |   \/    |    \    |___|    |___  |        \|    |___
#  |____|  \____|__  /____|_  /\____|__  /_______ \_______ \/_______  /|_______ \
#                  \/       \/         \/        \/       \/        \/         \/





#____________________________________________________________________________________

rm (list = objects ()) # Clear environment
#


# This bout takes care of first-time remote repos (from OP's perspective)
# that don't have the "extra folders" that are still vital for a Github
# clone of master branch.-> Code/curiosity-code/
#                                     - results/
#                                     - source/temp/

if (! (dir.exists (file.path (strsplit (getwd (),
        "curiosity-code", ) [[1]][1], "curiosity-code", "results")))) {

          dir.create (file.path (strsplit (getwd (),
        "curiosity-code", ) [[1]][1], "curiosity-code", "results"))}

if (! (dir.exists (file.path (strsplit (getwd (),
        "curiosity-code", ) [[1]][1], "curiosity-code", "source", "temp")))) {

          dir.create (file.path (strsplit (getwd (),
        "curiosity-code", ) [[1]][1], "curiosity-code", "source", "temp"))}

if (! (dir.exists (file.path (strsplit (getwd (),
        "curiosity-code", ) [[1]][1], "curiosity-code", "source", "RtempFiles")))) {

          dir.create (file.path (strsplit (getwd (),
        "curiosity-code", ) [[1]][1], "curiosity-code", "source", "RtempFiles"))
          dir.create (file.path (strsplit (getwd (),
        "curiosity-code", ) [[1]][1], "curiosity-code", "source", "archive", "RtempFiles"))
        }


# This line will source packagaes, either:
    # by loading them from the computer, or
    # by downloading them from the internet.
source (file.path ("scripts", "Source_Reference_Section.R"))
referencesection ("multirun")
# referenceSection ("profiler")

n_cores <- 2
# Specify the number of cores/workers we want to use
    # n_cores <- detectCores () - 3 # built around a maximum allowance
# n_cores <- 2
# n_cores <- 1

  sourceCpp (file.path ('cpp_source', 'median.cpp'))
  sourceCpp (file.path ('cpp_source', 'rowSums.cpp'))
  sourceCpp (file.path ('cpp_source', 'sort.cpp'))

source (file.path ("scripts", "Source_Multiple_Runs.R"))


stop ("shifting_curstart <- 1 : 8")

shifting_curstart <- 1

paramsfile <- c ("paramsequalStartMatInh_retry.yaml")
# paramsFile <- c ("diffZwischensTnN.yaml")
simdate <- gsub ('-', '', substring (Sys.Date(), 3))
secretcode <- 58418

paramssource = paramsfile
dirdate = simdate
seednumber = secretcode
mc.cores = n_cores
recolorize = FALSE

stop ('multi_runs <- function - shifting_curstart, paramssource,
  dirdate, seednumber, recolorize = FALSE ')

set.seed (seednumber + shifting_curstart)

  params <- yaml.load_file (file.path ("parameters", paramssource))
  number_of_reps <- as.numeric (params$number_of_reps)

  if (recolorize != FALSE) {

    if (params$indrunredo == TRUE) {
      subsetorsequence <- params$simnumberstart [shifting_curstart]
      singleormixture <- params$curinhdistribution [shifting_curstart]
    } else {
      subsetorsequence <- params$simnumberstart + (shifting_curstart - 1)
      singleormixture <- params$curinhdistribution
    }
    print ("Note for Parker: only simnumberstart, curinhdistribution and number_of_reps are needed from a 'params.yaml' type file - provided that the multirun arg 'recolorize' == TRUE")
    source (file.path ("scripts", "Source_Figure_Produxn_Multiple_Runs.R"))
    return (figprodmultrun (specificsimnumber = subsetorsequence,
      number_of_repeats = number_of_reps,
      paramssource = paramssource, recolorize = TRUE))

  } else {
    stop ("recolorize = false")
    ### add a "first time" clause here, to keep the "yo, there aren't any old archive files" issue at bay
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
    length (lastrun_init) <- number_of_reps

    if (params$lastruninit) {
      if (length (params$lastrunid) > 1) {
        lastrun_init <- restart_from_save (parameters = params,
          inputpattern = params$lastrunid [shifting_curstart])
      } else {
        lastrun_init <- restart_from_save (parameters = params,
          inputpattern = params$lastrunid)
      }
    }

    stop ("for (rep_number in 1 : number_of_reps) {")
    ### for (rep_number in 1 : number_of_reps) {

        rep_number <- 1

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

    stop ("invoking life_cycle: args")

    scmin = c (
      params$curstarts [[shifting_curstart]]$scmin [1],
      params$curstarts [[shifting_curstart]]$scmin [2],
      params$curstarts [[shifting_curstart]]$scmin [3],
      params$curstarts [[shifting_curstart]]$scmin [4])
    scmax = c (
      params$curstarts [[shifting_curstart]]$scmax [1],
      params$curstarts [[shifting_curstart]]$scmax [2],
      params$curstarts [[shifting_curstart]]$scmax [3],
      params$curstarts [[shifting_curstart]]$scmax [4])
    simnumber = subsetorsequence
    # simnumber = params$simnumberstart + (shifting_curstart - 1),
    runlength = params$runlength
    syllearnstyle = params$syllearnstyle
    vertoblearn = c (
      params$vertoblearn$vertical$learn,
      params$vertoblearn$vertical$invent,
      params$vertoblearn$oblique$learn,
      params$vertoblearn$oblique$invent)
    syldist = params$syldist
    curinh_value = params$curinh_value
    number_populations = params$num_pop
    population_size = params$pop_size
    syllable_number = params$sylnum
    number_sylls_probability_level = params$num_sylls_per_prob_lvl
    standdev = as.numeric (params$standard_deviation)
    curinh_style = curinh_binary
    recordingsimpfact = params$recordsimplifyfactor
    one_pop_singers = params$one_pop_singers
    curinhproportion = singleormixture # only used if curinh_pattern = 5
    directorydate = dirdate
    invasion = params$traitinvasion
    invktmstps = params$invasionthoutmstps
    invpopsize = params$invasionpopsize
    invstyle = params$invasionstyle
    invpop = params$invasionpop
    invsex = params$invasionsex
    invtraitvalue = params$invasiontraitvalue
    initfromlastrun = params$lastruninit
    lastrunobject = lastrun_init[[rep_number]]
    mate_selection_type = params$mate_selection_type
    selection_round_up = params$selection_round_up

    stop ("life_cycle start")

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
    stand.dev = standdev, curinhproportion = curinhproportion,
    mate_selection_type = mate_selection_type,
    selection_round_up = selection_round_up
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

  stop ("start thousand_timesteps")

  thousand_timesteps <- 1

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

  simplify <- 1
  single_timestep <- 1

  moranobjects <- sing.selection (parameters_sing_selection = simparams,
                                      temp_data_sing_selection = moranobjects,
                                      curiosity_level = curiosity_level,
                                      select_type = "mate",
                                      sylrep_object = sylreps,
                                      num_select_chances = c (40, 40),
                                      verbose_output = FALSE,
                                      interbreed = FALSE)

  # Locate new birb positions in population data, store in TDO
  moranobjects <- make.offspring.calls (parameters_offspring_calls = simparams,
                                      temp_data_offspring_calls = moranobjects)

  # Add noise to inherited curiosity trait, store temporarily
  moranobjects <- curiosity_learn (parameters_curiosity_learn = simparams,
                                  temp_data_curiosity_learn = moranobjects,
                                  inheritance_pattern = curinh_style)

  #
  moranobjects <- syll_learn (parameters_sylllearn = simparams,
                              temp_data_sylllearn = moranobjects,
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

  moranobjects <- syll_learn (parameters_sylllearn = simparams,
                              temp_data_sylllearn = moranobjects,
                              select_type = "tutor",
                              totally_new = FALSE,
                              randlearn_context = 2,
                              verbose = FALSE)

  curiosity_level <- recuriosity.offspring (parameters_recuriosity = simparams,
                                      temp_data_recuriosity = moranobjects,
                                      curiosity_object = curiosity_level)

  sylreps <- resylreps.offspring (parameters_resylreps = simparams,
                                  temp_data_resylreps = moranobjects,
                                  sylrep_object = sylreps)

  stop ("end of single_timestep")

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

  stop ("end of simplify")

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

    stop ("end of thousand_timesteps")

    print (paste0 ("Replicate Run # ",
            rep_number, ", done at: ",
            (format (Sys.time(), "%F-%H%M%S"))))

    stop ("end of rep_number")

    source (file.path ("scripts", "Source_Figure_Produxn_Multiple_Runs.R"))

    # figprodmultrun (specificsimnumber = subsetorsequence,
    #               number_of_repeats = number_of_reps,
    #               paramssource = paramssource,
    #               redo = FALSE,
    #               recolorize = TRUE,
    #               results_tenK_dir = FALSE,
    #               lineplots = TRUE,
    #               curMeans_only = FALSE,
    #               absolute_y = params$absolute_yAxis,
    #               recolorize_lineplots = "range-median")

    stop ("invoke figprodmultrun args")
    specificsimnumber = subsetorsequence
    number_of_repeats = number_of_reps
    paramssource = paramssource
    redo = FALSE
    recolorize = TRUE
    results_tenK_dir = FALSE
    lineplots = TRUE
    curMeans_only = FALSE
    absolute_y = params$absolute_yAxis
    recolorize_lineplots = "range-median"

    stop("figprodmultrun START")

    source (file.path ("scripts", "Source_Visualizing_Data.R"))

    params = yaml.load_file (file.path ("parameters", paramssource))
    #     print ("params load")
    converted_data <- vector ("list", number_of_repeats)

    if (redo != FALSE) {
      if (results_tenK_dir != FALSE) {
        if (! (file.exists (file.path ("results", paste0 ("tenKfiveByFive_", results_tenK_dir), (
            list.files (file.path ("results", paste0 ("tenKfiveByFive_", results_tenK_dir)), pattern = paste0 ("*_", specificsimnumber, "*_")) [
              length (list.files (file.path ("results", paste0 ("tenKfiveByFive_", results_tenK_dir)), pattern = paste0 ("*_", specificsimnumber, "*_")))
            ] # This selects the latest iteration of this sim number, so this is the line to change if that is no longer true
          ), (paste0 ("Group_", specificsimnumber, "_folderList.RData")))))) {
            # stop ("results_tenK_dir doesn't know where to find Group_#_folderList.RData")
            connection <- file (description = file.path ("source","temp", paste0 (specificsimnumber, "_sim_data.txt")), open = "rt")
            multirun_folderlist <- as.vector (read.table (connection, -1L) [[2]])
            close (connection)
        } else {
          multirun_folderlist <- readRDS (file.path ("results", paste0 ("tenKfiveByFive_", results_tenK_dir), (
            list.files (file.path ("results", paste0 ("tenKfiveByFive_", results_tenK_dir)), pattern = paste0 ("*_", specificsimnumber, "*_")) [
              length (list.files (file.path ("results", paste0 ("tenKfiveByFive_", results_tenK_dir)), pattern = paste0 ("*_", specificsimnumber, "*_")))
            ] # This selects the latest iteration of this sim number, so this is the line to change if that is no longer true
          ), (paste0 ("Group_", specificsimnumber, "_folderList.RData"))))
  
          for (run_visual in 1 : number_of_repeats) {
            multirun_folderlist [run_visual] <- paste0 (
              file.path ("results", paste0 ("tenKfiveByFive_", results_tenK_dir)),
              strsplit (multirun_folderlist, "results") [[1]][2]
            )
          }
        }
      } else {
         if (! (file.exists (file.path ("results", (
          list.files (file.path ("results"), pattern = paste0 ("*_", specificsimnumber, "*_")) [
            length (list.files (file.path ("results"), pattern = paste0 ("*_", specificsimnumber, "*_")))
          ] # this selects... (see above)
        ), (paste0 ("Group_", specificsimnumber, "_folderList.RData")))))) {
            stop ("non results_tenK_dir doesn't know where to find Group_#_folderList.RData")
        } else {
          multirun_folderlist <- readRDS (file.path ("results", (
            list.files (file.path ("results"), pattern = paste0 ("*_", specificsimnumber, "*_")) [
              length (list.files (file.path ("results"), pattern = paste0 ("*_", specificsimnumber, "*_")))
            ] # this selects... (see above)
          ), (paste0 ("Group_", specificsimnumber, "_folderList.RData"))))
        }
      }
    } else {
      connection <- file (description = file.path ("source","temp", paste0 (specificsimnumber, "_sim_data.txt")), open = "rt")
      multirun_folderlist <- as.vector (read.table (connection, -1L) [[2]])
      close (connection)
  
      # for (run_visual in 1 : number_of_repeats) {
        stop("start of run_visual loop")
        run_visual <- 1
        if (results_tenK_dir != FALSE) {
          multirun_folderlist [run_visual] <- paste0 (
            file.path ("results", paste0 ("tenKfiveByFive_", results_tenK_dir)),
            strsplit (multirun_folderlist, "results") [[1]][2]
          )
        }
        
        if (run_visual == 1) {
          multiRunTime <- format (Sys.time (), "%F-%H%M%S")
      #     print ("run_visual == 1")
          if (! (dir.exists (file.path (strsplit (
            multirun_folderlist [run_visual], "/variable_store", ) [[1]][1],
            "multirun_output")))) {
      #     print ("makin multirun_output")
            dir.create (file.path (strsplit (
              multirun_folderlist [run_visual], "/variable_store", ) [[1]][1],
              "multirun_output"))
      #     print ("makin specific multirun_output folder")
            dir.create (file.path (strsplit (
              multirun_folderlist [run_visual], "/variable_store", ) [[1]][1],
              "multirun_output", paste0 (multiRunTime,
              "-GMT-multirun-output")))
          }
      #     print ("Do we need to record the list of folders?")
          if (! (file.exists (file.path (strsplit (
            multirun_folderlist [run_visual], "variable_store", ) [[1]][1],
            paste0 ("Group_", specificsimnumber, "_folderList.RData"))))) {
              # print ("yes we do.")
              saveRDS (multirun_folderlist, file.path (strsplit (
                  multirun_folderlist [run_visual], "variable_store",
                ) [[1]][1], paste0 (
                  "Group_", specificsimnumber, "_folderList.RData")))
          }
        }
      #     print ("multirun_directory")
        multirun_directory <- paste0 (
          strsplit (multirun_folderlist [run_visual],
          "variable") [[1]][1], "multirun_output/",
          list.files (
          path = paste0 (
          strsplit (multirun_folderlist [run_visual],
          "variable") [[1]][1], "multirun_output/"),
          pattern = "multirun-output$"))
      #     print ("converted_data time")
        converted_data <- concatenate_data (
          specific_run = run_visual,
          converteddata = converted_data,
          parms = params,
          data_dir = multirun_folderlist
        )
      #     print ("process_data time")
      }
      process_data (
        data_conglomerate = converted_data,
        specificrepeat = number_of_repeats,
        path = multirun_directory
      )
    }
    #     print ("multiRunFolderList")
  
  
  
    # params = yaml.load_file (file.path ("parameters", paramssource))
    # #     print ("params load")
    # converted_data <- vector ("list", number_of_repeats)
  
    #  print (paste0 ("source SVD"))
    # source (file.path ("scripts", "Source_Visualizing_Data.R"))
  
    # if (recolorize == FALSE) {
    #   for (run_visual in 1 : number_of_repeats) {
    #     if (results_tenK_dir != FALSE) {
    #       multirun_folderlist [run_visual] <- paste0 (
    #         file.path ("results", paste0 ("tenKfiveByFive_", results_tenK_dir)),
    #         strsplit (multirun_folderlist, "results") [[1]][2]
    #       )
    #     }
    #     # str_split (multirun_folderlist [1], "/") [[1]][1]
    #     # tenKfiveByFive_
    #     # paste0 (str_split (multirun_folderlist [run_visual], "/") [[1]][1], "tenKfiveByFive_", results_tenK_dir[run_visual])
    #     # run_visual=1
    #     if (run_visual == 1) {
    #       multiRunTime <- format (Sys.time (), "%F-%H%M%S")
    #   #     print ("run_visual == 1")
    #       if (! (dir.exists (file.path (strsplit (
    #         multirun_folderlist [run_visual], "/variable_store", ) [[1]][1],
    #         "multirun_output")))) {
    #   #     print ("makin multirun_output")
    #         dir.create (file.path (strsplit (
    #           multirun_folderlist [run_visual], "/variable_store", ) [[1]][1],
    #           "multirun_output"))
    #   #     print ("makin specific multirun_output folder")
    #         dir.create (file.path (strsplit (
    #           multirun_folderlist [run_visual], "/variable_store", ) [[1]][1],
    #           "multirun_output", paste0 (multiRunTime,
    #           "-GMT-multirun-output")))
    #       }
    #   #     print ("Do we need to record the list of folders?")
    #       if (! (file.exists (file.path (strsplit (
    #         multirun_folderlist [run_visual], "variable_store", ) [[1]][1],
    #         paste0 ("Group_", specificsimnumber, "_folderList.RData"))))) {
    #           # print ("yes we do.")
    #           saveRDS (multirun_folderlist, file.path (strsplit (
    #               multirun_folderlist [run_visual], "variable_store",
    #             ) [[1]][1], paste0 (
    #               "Group_", specificsimnumber, "_folderList.RData")))
    #       }
    #     }
    #   #     print ("multirun_directory")
    #     multirun_directory <- paste0 (
    #       strsplit (multirun_folderlist [run_visual],
    #       "variable") [[1]][1], "multirun_output/",
    #       list.files (
    #       path = paste0 (
    #       strsplit (multirun_folderlist [run_visual],
    #       "variable") [[1]][1], "multirun_output/"),
    #       pattern = "multirun-output$"))
    #   #     print ("converted_data time")
    #     converted_data <- concatenate_data (
    #       specific_run = run_visual,
    #       converteddata = converted_data,
    #       parms = params,
    #       data_dir = multirun_folderlist
    #     )
    #   #     print ("process_data time")
    #   }
    #   process_data (
    #     data_conglomerate = converted_data,
    #     specificrepeat = number_of_repeats,
    #     path = multirun_directory
    #   )
    # }
    #     print ("sourcing info")
    info <- readRDS (file = file.path (
      multirun_folderlist [1], "metadata.RData"))
    #     print ("makin' lists")
    datanames <- c ("CurHist","Cursity","SylDist","SylReps")
    listnames <- c ("hist","sity","sdst","repz")
    for (i in 1 : 4) {
      listlister <- paste0 (listnames [i], "list <- vector (mode = \"character\", length = number_of_repeats)")
      listmaker <- paste0 (listnames [i], "list [", 1 : number_of_repeats, "] <- \"", datanames[i], 1 : number_of_repeats, ".RData\"")
      eval (parse (text = c (listlister, listmaker)))
    }
  
    curhistlist <- vector (mode = "list", length = number_of_repeats + 1)
    sylrepzlist <- vector (mode = "list", length = number_of_repeats + 1)
    sdstbxnlist <- vector (mode = "list", length = number_of_repeats + 2)
    cursitylist <- vector (mode = "list", length = number_of_repeats + 1)
  
    multirun_directory <- paste0 (strsplit (multirun_folderlist [1], "variable") [[1]][1],
                                  "multirun_output/")
  
    if (length (list.files (multirun_directory, pattern = "multirun-output")) != 0) {
      if (length (list.files (file.path (multirun_directory, list.files (multirun_directory, pattern = "multirun-output")))) != 0) {
        multirun_directory <- paste0 (multirun_directory, list.files (multirun_directory, pattern = "multirun-output"), "/")
      }
    }
    # else if (length (list.files (multirun_directory, pattern = "multirun-output")) == 0) {
    #   multirun_directory <- paste0 (strsplit (multirun_directory[1], "multirun_output") [[1]][1],
    #                               "multirun_output/")
    # }
  
    #     print ("sourcing from lists")
    for (i in 1 : number_of_repeats) {
      # print (paste0 (multirun_directory, "/", histlist [i]))
      curhistlist [[i]] <- readRDS (paste0 (multirun_directory, histlist [i]))
      # print (paste0 (multirun_directory, "/", sitylist [i]))
      cursitylist [[i]] <- readRDS (paste0 (multirun_directory, sitylist [i]))
      # print (paste0 (multirun_directory, "/", sdstlist [i]))
      sdstbxnlist [[i]] <- readRDS (paste0 (multirun_directory, sdstlist [i]))
      # print (paste0 (multirun_directory, "/", repzlist [i]))
      sylrepzlist [[i]] <- readRDS (paste0 (multirun_directory, repzlist [i]))
    }
    #     print ("histlist")
    for (i in 1 : length (curhistlist [[1]])) {
      eval (parse (text = paste0 ("curhistlist [[number_of_repeats + 1]][i] <- mean (c (curhistlist [[",
                             paste0 (1 : (number_of_repeats - 1),"]][i],curhistlist [[", collapse=''),
                             number_of_repeats, "]][i]))")))
    }
  
    #     print (paste0 ("dimensions of curhistlist [[1]] - ", dim (curhistlist [[1]])))
    dim (curhistlist [[number_of_repeats + 1]]) <- dim (curhistlist [[1]])
  
    #     print ("sitylist")
    for (i in 1 : length (cursitylist [[1]])) {
      eval (parse (text = paste0 ("cursitylist [[number_of_repeats + 1]][i] <- mean (c (cursitylist [[",
                             paste0 (1 : (number_of_repeats - 1),"]][i],cursitylist [[", collapse=''),
                             number_of_repeats, "]][i]))")))
    }
  
    #     print (paste0 ("dimensions of cursitylist [[1]] - ", dim (cursitylist [[1]])))
    dim (cursitylist [[number_of_repeats + 1]]) <- dim (cursitylist [[1]])
  
    #     print ("sdstlist")
  
    for (i in 1 : length (sdstbxnlist [[1]])) {
      eval (parse (text = paste0 ("sdstbxnlist [[number_of_repeats + 1]][i] <- mean (c (sdstbxnlist [[",
                             paste0 (1 : (number_of_repeats - 1),"]][i],sdstbxnlist [[", collapse=''),
                             number_of_repeats, "]][i]))")))
    }
  
    #     print (paste0 ("dimensions of sdstbxnlist [[1]] - ", dim (sdstbxnlist [[1]])))
    dim (sdstbxnlist [[number_of_repeats + 1]]) <- dim (sdstbxnlist [[1]])
  
    #     print ("repzlist")
  
    for (i in 1 : length (sylrepzlist [[1]])) {
      eval (parse (text = paste0 ("sylrepzlist [[number_of_repeats + 1]][i] <- mean (c (sylrepzlist [[",
                             paste0 (1 : (number_of_repeats - 1),"]][i],sylrepzlist [[", collapse=''),
                             number_of_repeats, "]][i]))")))
    }
  
    #     print (paste0 ("dimensions of sylrepzlist [[1]] - ", dim (sylrepzlist [[1]])))
    dim (sylrepzlist [[number_of_repeats + 1]]) <- dim (sylrepzlist [[1]])
  
    #     print ("last_stats")
    last_stats <- paste0 ("rm (sylrepz", number_of_repeats, ", sdstbxn", number_of_repeats,
                         ", cursity", number_of_repeats, ", curhist", number_of_repeats,
                         ", last_stats, histlist, sitylist, sdstlist, repzlist",
                         ", listlister, listmaker, listnames, datanames)")
  
    eval (parse (text = last_stats))
  
    plot_info <- create_plot_info (info[[2]], info[[1]])
    info_make <- paste(paste0 ("sink (file = paste0 (multirun_directory, \"Multirun - Parameters and Info\"))"),
                       "cat (paste0 (\"Number of Timesteps: \", info[[3]][1], \"\nNumber of Populations: \", info[[3]][2], \"\nPopulation Size: \", info[[3]][3], \"\nNumber of Syllables: \", info[[3]][4], \"\nNumber of Syllable Positions Assigned to Specific Probability Levels: \", info[[3]][5], \"\nNumber of Singers Sampled from One Population for Mating: \", info[[3]][7], \"\nNumber of Singers Sampled from One Population for Tutoring: \", info[[3]][6], \"\nProbability of Inheriting Curiosity Accurately: \", info[[3]][8], \"\nProbability of Learning Syllables Accurately from Parent: \", info[[3]][10], \"\nProbability of Learning Syllables Accurately from Tutor: \", info[[3]][9], \"\nProbability of Picking up Random Extra Syllables from Parent: \", info[[3]][12], \"\nProbability of Picking up Random Extra Syllables from Tutor: \", info[[3]][11], \"\nStandard Deviation of Randomly-picked-up Sylls from Established Mean: \", info[[3]][13], \"\nNumber of Rows in Population Calls Matrix: \", info[[3]][14], \"\nNumber of Columns in Pop Calls Matrix: \", info[[3]][15], \"\nPairing Pool Rows: \", info[[3]][16], \"\nPairing Pool Columns: \", info[[3]][17], \"\nPairing Pool Slices: \", info[[3]][18], \"\nCuriosity Counter Rows: \", info[[3]][19], \"\nCuriosity Counter Columns: \", info[[3]][20], \"\nPopulation Syllable Probability Rows: \", info[[3]][21], \"\nPopulation Probability Columns: \", info[[3]][22], \"\nLength of Curiosity Breaks Vector: \", info[[3]][23], \"\nLength of Zero to One Template: \", info[[3]][24], \"\nLearning Pool Rows: \", info[[3]][25], \"\nLearning Pool Columns: \", info[[3]][26], \"\nLearning Pool Slices: \", info[[3]][27]))",
                       "sink ()", sep = "\n")
    eval (parse (text = info_make))
    #     print ("info made")
    mins_n_maxes <- min_n_max (parameters = params, number_of_runs_mnx = number_of_repeats,
                              cursitylist = cursitylist, sdstbxnlist = sdstbxnlist,
                              curhistlist = curhistlist, sylrepzlist = sylrepzlist)
    #     print (paste0 ("mins_n_maxes: ", mins_n_maxes))
    #     print (paste0 ("length of cursitylist: ", dim (cursitylist [[number_of_repeats + 1]])))
    if (recolorize == FALSE) {
      simple_plots (parameters = params, plot_info = plot_info,
                   number_of_runs = number_of_repeats, cursitylist = cursitylist,
                   sdstbxnlist = sdstbxnlist, curhistlist = curhistlist, sylrepzlist = sylrepzlist,
                   mins_n_maxes = mins_n_maxes, saving_dir = multirun_directory, lineplots = lineplots, curMeans_only = curMeans_only, absolute_y = params$absolute_yAxis)
    } else {
      if (compare_subsets == TRUE) {
        output_variable <- recolorized_simple_plots (recolorize_lineplots = recolorize_lineplots, # "clustering"
                                                     parameters = params, plot_info = plot_info,
                                                     number_of_runs = number_of_repeats, cursitylist = cursitylist,
                                                     sdstbxnlist = sdstbxnlist, curhistlist = curhistlist, sylrepzlist = sylrepzlist,
                                                     mins_n_maxes = mins_n_maxes, saving_dir = multirun_directory, lineplots = lineplots, curMeans_only = curMeans_only, absolute_y = params$absolute_yAxis, compare_subsets = TRUE)
  
        return (output_variable)
      } else {
        recolorized_simple_plots (recolorize_lineplots = recolorize_lineplots, # "clustering"
                                  parameters = params, plot_info = plot_info,
                                  number_of_runs = number_of_repeats, cursitylist = cursitylist,
                                  sdstbxnlist = sdstbxnlist, curhistlist = curhistlist, sylrepzlist = sylrepzlist,
                                  mins_n_maxes = mins_n_maxes, saving_dir = multirun_directory, lineplots = lineplots, curMeans_only = curMeans_only, absolute_y = params$absolute_yAxis)
      }
  
    }
    #     print ("simple_plots done")
  
    # if (redo != FALSE) {
    
    if (redo == FALSE) {
      srcdir = file.path ("scripts")
      # file.names = dir (srcdir) [grep ("Source", dir (srcdir))]
      if (! (dir.exists (file.path (strsplit (multirun_folderlist [1], "/variable_store", ) [[1]][1], "copy_of_scripts")))) {
        dir.create (file.path (strsplit (multirun_folderlist [1], "/variable_store", ) [[1]][1], "copy_of_scripts"))
      }
      zipr (file.path (strsplit (multirun_folderlist [1], "/variable_store", ) [[1]][1], "copy_of_scripts", "scriptsFiles.zip"), file.path (srcdir), TRUE, 9, TRUE)
  
      parmdir = file.path ("parameters")
      # file.names = dir (parmdir) [grep ("*.yaml", dir (parmdir))]
      if (! (dir.exists (file.path (strsplit (multirun_folderlist [1], "/variable_store", ) [[1]][1], "copy_of_params")))) {
        dir.create (file.path (strsplit (multirun_folderlist [1], "/variable_store", ) [[1]][1], "copy_of_params"))
      }
      zipr (file.path (strsplit (multirun_folderlist [1], "/variable_store", ) [[1]][1], "copy_of_params", "paramsFiles.zip"), file.path (parmdir), TRUE, 9, TRUE)
  
      saveRDS (params, file.path (strsplit (multirun_folderlist [1], "/variable_store", ) [[1]][1], "copy_of_params", "paramsSource.RData"))
    }
    return (print (paste0 (specificsimnumber," - Exit Status: 0")))

  }                  
