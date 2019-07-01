
source(file.path("scripts", "Source_Reference_Section.R"))
referenceSection("profvyaml"

source(file.path('scripts', 'Source_Initial_Functions_Parameters.R'))
source(file.path('scripts', 'Source_Life_Cycle_Functions.R'))

params <- yaml.load_file(file.path("parameters", "params.yaml"))
simParams <- define_parameters(
  num_timesteps = as.numeric(strsplit(params$runLength, "k")[[1]][1]) * 1000, 
  num_pop = params$num_pop, 
  pop_size = params$pop_size, 
  sylnum = params$sylnum, 
  nSL = params$num_sylls_per_prob_lvl, 
  one_pop_singers = c(10,10), 
  curlearnprob = params$curinh_value, 
  learnprob = c(params$vertObLearn$vertical$invent, params$vertObLearn$vertical$learn), 
  randlearnprob = c(params$vertObLearn$oblique$invent, params$vertObLearn$oblique$learn), 
  stand.dev = as.numeric(params$standard_deviation)
)
scMin <- c(params$curstarts[[1]]$scMin[1],
           params$curstarts[[1]]$scMin[2],
           params$curstarts[[1]]$scMin[3],
           params$curstarts[[1]]$scMin[4])
scMax <- c(params$curstarts[[1]]$scMax[1],
           params$curstarts[[1]]$scMax[2],
           params$curstarts[[1]]$scMax[3],
           params$curstarts[[1]]$scMax[4])
moranObjects <- define_temp_data(simParams)
curiosity_level <- initialize.curiosity(simParams, scMin, scMax)
sylreps <- initialize.sylrep(simParams, c(1,2), T, T)
moranObjects <- sing.selection(parameters = simParams, moran = moranObjects, 
                                curiosity_level = curiosity_level, 
                                select_type = 2, sylrep_object = sylreps, 
                                num_select_chances = c(100, 100), 
                                verbose_output = F, interbreed = F)

moranObjects <- make.offspring.calls(parameters = simParams, 
                                      moran = moranObjects)

profvis({
  for(iteration in 1:100000) {
    x <- curiosity_learn(parameters = simParams, moran = moranObjects,
                         timestep = single_timestep,
                         inheritance_pattern = params$curinh_pattern)
  }
})
