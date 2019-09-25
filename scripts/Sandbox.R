# library(R.cache) # Caching tool
library(memo) # Better caching tool (LRU Cache) 
library(profvis) # Code profiling tool

# Source Parker code
source(file.path('~', 'Documents', 'projects', 'Code', 'curiosity-code', 'scripts', 'Source_Initial_Functions_Parameters.R'))

# Make a cache
cache = lru_cache(5000)

memoized.rep.frac <- memo(rep.frac, cache)

# Standalone
profvis({
  for(i in rep(1, each = 50000)) {
    rep.frac(10, 5, 1)
  }
})

# With caching
profvis({
  for(i in rep(1, each = 50000)) {
    memoized.rep.frac(10, 5, 1)
  }
})

devtools::install_github("hadley/lineprof")


source(file.path("scripts", "Source_Reference_Section.R"))
referenceSection("multirun")

shifting_curstart <- 1
paramsFile <- c("IA_proof.yaml")
simDate <- gsub('-', '', substring(Sys.Date(), 3))
secretCode <- 58418
paramsSource = paramsFile
dirDate = simDate
seedNumber = secretCode
set.seed (seedNumber + shifting_curstart)
params <- yaml.load_file (file.path ("parameters", paramsSource))
number_of_reps <- as.numeric (params$number_of_reps)
if (params$lastRunInit) {
  if (length (params$lastRunID) > 1) {
    lastRun_init <- restart_from_save (parameters = params, 
      inputPattern = params$lastRunID [shifting_curstart])
  } else {
    lastRun_init <- restart_from_save (parameters = params, 
      inputPattern = params$lastRunID)
  }

} else {
  lastRun_init <- array(0, c(1,1,1,number_of_reps))
}
rep_number = 1

if (params$IndRunRedo == T) {
  subsetOrSequence <- params$simNumberStart [shifting_curstart]
  singleOrMixture <- params$curinhDistribution [shifting_curstart]
} else {
  subsetOrSequence <- params$simNumberStart + (shifting_curstart - 1)
  singleOrMixture <- params$curinhDistribution
}




scMin = c (
  params$curstarts [[shifting_curstart]]$scMin [1],
  params$curstarts [[shifting_curstart]]$scMin [2],
  params$curstarts [[shifting_curstart]]$scMin [3],
  params$curstarts [[shifting_curstart]]$scMin [4])
scMax = c(
  params$curstarts [[shifting_curstart]]$scMax [1],
  params$curstarts [[shifting_curstart]]$scMax [2],
  params$curstarts [[shifting_curstart]]$scMax [3],
  params$curstarts [[shifting_curstart]]$scMax [4])
simNumber = subsetOrSequence
# simNumber = params$simNumberStart + (shifting_curstart - 1),
runLength = params$runLength
SylLearnStyle = params$SylLearnStyle
vertOblLearn = c(
  params$vertObLearn$vertical$learn,
  params$vertObLearn$vertical$invent,
  params$vertObLearn$oblique$learn,
  params$vertObLearn$oblique$invent)
sylDist = params$sylDist
curinh_value = params$curinh_value
number_populations = params$num_pop
population_size = params$pop_size
syllable_number = params$sylnum
number_sylls_probability_level = params$num_sylls_per_prob_lvl
standDev = as.numeric(params$standard_deviation)
SimNumberLC = shifting_curstart
curinh_style = params$curinh_pattern
recordingSimpFact = params$RecordSimplifyFactor
one_pop_singers = params$one_pop_singers
curinhProportion = singleOrMixture # only used if curinh_pattern = 5
directoryDate = dirDate
invasion = params$traitInvasion
invKTmstps = params$invasionThouTmstps
invPopSize = params$invasionPopSize
invFocus = params$invasionFocus
invPop = params$invasionPop
invSex = params$invasionSex
invTraitValue = params$invasionTraitValue
initFromLastRun = params$lastRunInit
lastRunObject = lastRun_init[,,,rep_number]


source (file.path ("scripts", "Source_Initial_Functions_Parameters.R"))

simParams <- define_parameters (
    num_timesteps = as.numeric (strsplit (runLength, "k")[[1]][1]) * 1000, 
    num_pop = number_populations, pop_size = population_size, 
    sylnum = syllable_number, nSL = number_sylls_probability_level, 
    one_pop_singers = one_pop_singers, curlearnprob = curinh_value, 
    learnprob = c (vertOblLearn[2], vertOblLearn[1]), 
    randlearnprob = c (vertOblLearn[4], vertOblLearn[3]), 
    stand.dev = standDev, curinhProportion = curinhProportion
  )
  
  ##### Timestep Data Object (TDO)

  moranObjects <- define_temp_data (simParams)
  # pairing_pool <- define_temp_data(simParams, 2)
  if (initFromLastRun) {
    sylreps <- initialize.sylrep (P = simParams, 
      population.pattern = c (1,2), pastRunObject = lastRunObject, 
      eqpop = T, eqsex = T, pastRunInit = T)
    curiosity_level <- initialize.curiosity (
      P = simParams, cur.min = scMin, cur.max = scMax, 
      pastRunObject = lastRunObject, pastRunInit = T)
  } else {
    sylreps <- initialize.sylrep (P = simParams, 
      population.pattern = c (1,2), eqpop = T, eqsex = T)
    curiosity_level <- initialize.curiosity (
      simParams, scMin, scMax)
  }

  
  sylrep_rowcol <- recordvariable.initialize (
      simParams, recordingSimpFact, variableID = 1)
  
  sylrep_dstbxn <- recordvariable.initialize (
      simParams, recordingSimpFact, variableID = 2)

  curity_mean_t <- recordvariable.initialize (
      simParams, recordingSimpFact, variableID = 3)

  curity_repert <- recordvariable.initialize (
      simParams, recordingSimpFact, variableID = 4)

  source (file.path ("scripts", "Source_Life_Cycle_Functions.R"))
  source (file.path ("scripts", "Source_Multiple_Runs.R"))
  
  

  stuff_to_save <- savinStuff (Parameters = simParams, 
                              Output_Filename = docnamez, 
                              moran = moranObjects)

thousand_timesteps <- 2

if (invasion && (thousand_timesteps == invKTmstps)) {
      if (invFocus == 'curiosity') {
        curiosity_level <- invasion_parameters_curiosity(
          iP = invPop,
          iSx = invSex,
          iPs = invPopSize,
          iF = invFocus,
          iT = invTraitValue,
          curiosity_container = curiosity_level,
          someParameters = simParams
        )
      } else {
        sylreps <- invasion_parameters_sylrep(
          iP = invPop,
          iSx = invSex,
          iPs = invPopSize,
          iF = invFocus,
          iT = invTraitValue,
          sylrep_container = sylreps,
          someParameters = simParams
        )
      }  
    }

  iP = invPop
  iSx = invSex
  iPs = invPopSize
  iF = invFocus
  iT = invTraitValue
  sylrep_container = sylreps
  curiosity_container = curiosity_level
  someParameters = simParams


break_tester <- function (levels) {

  levels

  for (top_level in 1:)

}

v <- c("Hello","loop")
cnt <- 2
thing <- c("one", "two", "three", "four", "five")
for (ad_nauseum in 1:5) {
  repeat {
    print(v)
    cnt <- cnt + 1
    
    if(cnt > 5) {
        break
    }
  }
  print (thing[ad_nauseum])
}
