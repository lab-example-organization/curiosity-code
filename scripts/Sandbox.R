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



