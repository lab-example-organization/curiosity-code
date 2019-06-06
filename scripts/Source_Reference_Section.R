
# A great lil function stolen off stackexchange (https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them)
    # This function loads packages that the code depends on, BUT:
    # if a package is not installed, it installs it first, then loads it.
    load.installIfRequired <- function(package_list = package_list[[which (reference_list == referencing_script)]]){
        

        for( i in 1:length(package_list) ){
            
            #  require returns TRUE invisibly if it was able to load package
            if( ! require( package_list[i] , character.only = TRUE ) ){
                
                #  If package was not able to be loaded then re-install
                install.packages( package_list[i] , dependencies = TRUE )
                
                #  Load package after installing
                require( package_list[i] , character.only = TRUE )
            
            }
        }
    }


referenceSection <- function (referencing_script) {
    # A list of all the packages needed for this code to function:
    
    reference_list <- list(
        'multirun',
        'heatmaps',
        'profiler',
        'profvyaml',
        'pfvSrrYml',
        'profTest'#,
        #'',
        #'',
        #'',
        #'',
        #'',
        #'',
        #'',
        
    )

    package_list <- list(
        multirun = c('doParallel', 'abind', 'stringr', 'Hmisc', 'yaml', 'R.utils', 'dplyr', 'Rcpp', 'tidyr', 'data.table'),
        heatmaps = c('stringr', 'yaml', 'magick'),
        profiler = c('doParallel', 'abind', 'stringr', 'Hmisc', 'yaml', 'R.utils', 'dplyr', 'Rcpp', 'tidyr', 'data.table', 'profvis'),
        profvyaml = c('profvis', 'yaml'),
        pfvSrrYml = c('profvis', 'stringr', 'yaml'),
        profTest = c('profvis'),
        testings = c('testthat', '')#,
        
        #c('') #,
        
    )

    #  Then try/install packages...
    load.installIfRequired(package_list = package_list[[which (reference_list == referencing_script)]])

    # # # This is how this file used to look:

    # library(doParallel)
    # library(abind)
    # library(stringr)
    # library(Hmisc)
    # library(yaml)
    # library(R.utils)
    # library(dplyr)
    return (print('See You Space Cowhand'))
    # return (sessionInfo(package = NULL))
    
}

# sessionInfo(package = NULL)
