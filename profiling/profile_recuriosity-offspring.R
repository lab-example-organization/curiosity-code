
source (file.path ("scripts", "Source_Reference_Section.R"))
referenceSection ("profvyaml"

source (file.path ('scripts', 'Source_Initial_Functions_Parameters.R'))
source (file.path ('scripts', 'Source_Life_Cycle_Functions.R'))

params <- yaml.load_file(file.path ("parameters", "params.yaml"))
simParams <- define_parameters (
  num_timesteps = params$runlength,
  num_pop = params$num_pop,
  pop_size = params$pop_size,
  sylnum = params$sylnum,
  nSL = params$num_sylls_per_prob_lvl,
  one_pop_singers = c (10,10),
  curlearnprob = params$curinh_value,
  learnprob = c (params$vertoblearn$vertical$invent, params$vertoblearn$vertical$learn),
  randlearnprob = c (params$vertoblearn$oblique$invent, params$vertoblearn$oblique$learn),
  stand.dev = as.numeric (params$standard_deviation)
)
scmin <- c (params$curstarts[[1]]$scmin[1],
           params$curstarts[[1]]$scmin[2],
           params$curstarts[[1]]$scmin[3],
           params$curstarts[[1]]$scmin[4])
scmax <- c (params$curstarts[[1]]$scmax[1],
           params$curstarts[[1]]$scmax[2],
           params$curstarts[[1]]$scmax[3],
           params$curstarts[[1]]$scmax[4])
moranObjects <- define_temp_data (simParams)
curiosity_level <- initialize.curiosity (simParams, scmin, scmax)
sylreps <- initialize.sylrep (simParams, c (1,2), TRUE, TRUE)

moranObjects <- sing.selection (parameters = simParams, moran = moranObjects,
                              curiosity_level = curiosity_level,
                              select_type = "mate", sylrep_object = sylreps,
                              num_select_chances = c (100, 100),
                              verbose_output = FALSE, interbreed = FALSE)

moranObjects <- make.offspring.calls (parameters = simParams,
                                     moran = moranObjects)

moranObjects <- curiosity_learn (parameters = simParams, moran = moranObjects,
                                timestep = single_timestep,
                                curinh_pattern = params$curinh_pattern)

moranObjects <- syll_learn (parameters = simParams, moran = moranObjects,
                           select_type = "mate", totally_new = FALSE,
                           randlearn_context = 2, verbose = FALSE)

moranObjects <- sing.selection (parameters = simParams, moran = moranObjects,
                               curiosity_level = curiosity_level,
                               select_type = "tutor", sylrep_object = sylreps,
                               num_select_chances = c (100, 100),
                               verbose_output = FALSE, interbreed = FALSE)

moranObjects <- syll_learn (parameters = simParams, moran = moranObjects,
                           select_type = "tutor", totally_new = FALSE,
                           randlearn_context = 2, verbose = FALSE)

profvis ({
  for (iteration in 1 : 100000) {
    x <- recuriosity.offspring (parameters = simParams, moran = moranObjects,
                               curiosity_object = curiosity_level)
  }
})
