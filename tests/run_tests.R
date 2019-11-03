script_dir <- 'scripts'
reference_file <- 'Source_Reference_Section.R'

source(file.path(script_dir, reference_file))
referenceSection('testings')

# library(testthat) 

# All the stuff that's going to happen in the tests has to be reachable 
# from this script and the environemnt it feeds into when it's being read.

# So, include source calls to relevant scripts!
# Leon-style!

test_dir <- 'tests'
test_scripts <- 'testthat'

test_file <- 'test-That_shouldPickNeighbor.R'
script_file <- ''

source(file.path(test_dir, test_scripts, test_file))

test_results <- test_dir(file.path(test_dir, test_scripts), reporter="summary")