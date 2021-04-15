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

paramsfile <- c ("paramsdebugList.yaml")
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

    if (params$indrunredo == T) {
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
      archive = TRUE, new_dir = F)
    archivesimfiles (path = file.path ("source", "temp"),
      filename = paste0 (shifting_curstart,"_sim_data.txt"),
      archive = TRUE, new_dir = F)

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

    if (params$indrunredo == T) {
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
        print ("/please/ignore/this/line/like/you/always/do")
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
      eqpop = T, eqsex = T, pastruninit_is = T)
    curiosity_level <- initialize.curiosity (
      parameters_ic = simparams, cur.min = scmin, cur.max = scmax,
      pastrunobject_ic = lastrunobject, pastruninit_ic = T)
  } else {
    sylreps <- initialize.sylrep (parameters_is = simparams,
      population.pattern = c (1,2), eqpop = T, eqsex = T)
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
                                      verbose_output = F,
                                      interbreed = F)

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
                              verbose = F)

  moranobjects <- sing.selection (parameters_sing_selection = simparams,
                                  temp_data_sing_selection = moranobjects,
                                  curiosity_level = curiosity_level,
                                  select_type = "tutor",
                                  sylrep_object = sylreps,
                                  num_select_chances = c (40, 40),
                                  verbose_output = F,
                                  interbreed = F)

  moranobjects <- syll_learn (parameters_sylllearn = simparams,
                              temp_data_sylllearn = moranobjects,
                              select_type = "tutor",
                              totally_new = FALSE,
                              randlearn_context = 2,
                              verbose = F)

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

    figprodmultrun (specificsimnumber = subsetorsequence,
                  number_of_repeats = number_of_reps,
                  paramssource = paramssource,
                  redo = FALSE,
                  recolorize = TRUE,
                  results_dir = FALSE,
                  lineplots = TRUE,
                  curMeans_only = FALSE,
                  absolute_y = params$absolute_yAxis,
                  recolorize_style = "range-median")

  }                  
