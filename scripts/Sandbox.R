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