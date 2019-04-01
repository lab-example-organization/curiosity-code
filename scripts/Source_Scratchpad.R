sing.selection <- function(universal_parameters, moran, curiosity_level, select_type, sylrep_object, num_select_chances = c(10, 42), sylrep_fill_chances = 10, verbose_output = TRUE, interbreed = FALSE){
  
  for(population in 1 : universal_parameters$num_pop) { #population <- 1 rm(population)
    #print(paste("this is population",population,sep=" "))
    chance_for_selection = 1
    while(chance_for_selection <= num_select_chances[select_type]) {
      stop = FALSE
      if(chance_for_selection == num_select_chances[select_type]) {
        auto.teachers <- matrix(c(sample(universal_parameters$pop_calls_matrix[1, ], sylrep_fill_chances),sample(universal_parameters$pop_calls_matrix[2, ], sylrep_fill_chances)),2,sylrep_fill_chances,T)
        for(MTsylrep_filter in 1:sylrep_fill_chances){
          #c((sample(universal_parameters$pop_calls_matrix[1, ], 1)), (sample(universal_parameters$pop_calls_matrix[2, ], 1)))
          if((
            sum(sylrep_object[auto.teachers[1,MTsylrep_filter], , population]) != 0) && (
            sum(sylrep_object[auto.teachers[2,MTsylrep_filter], , population]) != 0)) {
            if(verbose_output == TRUE) {
              context.name <- c("Tutor", "Mate")
              warning(print(paste0("Automatic Teacher(s) = ", auto.teachers[,MTsylrep_filter], " for Population ", population, " ", context.name[select_type], " Selection")))
            }
            moran = update_selexn_data(universal_parameters, moran, auto.teachers, 1, auto.teachers[2], 
                curiosity_level, population, select_type,
                sylrep_object[auto.teachers[1]:200,,population], sylrep_object[auto.teachers[2],,population], chance_for_selection)

            # should probably fill in some spots in moran$pairing.pool with MTsylrep_filter, provided the value exceeds 1.
            #if(MTsylrep_filter >= 1) {}
            stop = TRUE
            break
          }
        }
        if(stop) {break}
      }
      
      if(select_type == 1) {
        #This statement separates specific mating and tutoring selection qualities:
        # singSuccessFilter will inform the selection of a mate by restricting the successful mate 
        # to those individuals from the same population as the selector. Similarly, 
        # selector.index distinguishes between mating and tutoring, except here it uses
        # a randomly-selected female for the mating context, and the offspring for tutoring.

        singSuccessFilter <- 1 : ((universal_parameters$one_pop_singers[select_type]) * (universal_parameters$num_pop)) # "1-20"
        selector.index <- moran$pairing.pool[3, 1, population]
      } else {
        singSuccessFilter <- (1 + ((population - 1) * (universal_parameters$one_pop_singers[select_type]))) : (population * universal_parameters$one_pop_singers[select_type]) # "1-10," or "11-20"
        selector.index <- sample(universal_parameters$pop_calls_matrix[2, ], 1)
      }
      
      selector.sylrep <- sylrep_object[selector.index, , population]
            
      selection.index <- (
        # This creates sample calls for each population; each population has a sample size of
        # universal_parameters$one_pop_singers, which comes from the male half of the population.
        # probability defined by the fraction of syllable repertoires of each member of 
        # each population divided by the maximum syllrep of the population.
        sapply(
          1:universal_parameters$num_pop, function(x) {
            sample(
              x = universal_parameters$pop_calls_matrix[1,], 
              size = universal_parameters$one_pop_singers[select_type], 
              replace = FALSE, prob = ((
                  apply(sylrep_object[universal_parameters$pop_calls_matrix[1,],,x],1,sum)
                )/max(
                  apply(sylrep_object[universal_parameters$pop_calls_matrix[1,],,x],1,sum)
                )
              )
            )
          }
        ) # probability = the number of times each individual's syllable 
          # repertoire has a 1 in it (sum(sylrep_object[universal_parameters$pop_calls_matrix[1,]])), 
          # divided by the biggest repertoire's total.
      )
      
      # create a matrix of all the sylrep_object of the sample males from selection.index
      selection.sylreps <- t(
        cbind(
          sapply(
            1:universal_parameters$one_pop_singers[select_type], 
            function(x) {sylrep_object[selection.index[x,1],,1]}
          ),
          sapply(
            1:universal_parameters$one_pop_singers[select_type], 
            function(x) {sylrep_object[selection.index[x,2],,2]}
           )
         )
      )
      
      # applies the standard deviation scoring to the males in selection.sylrep_object; 
      # larger score means greater difference between male sylrep and selector's sylrep.
      golf_score <- sort(apply(X = selection.sylreps, MARGIN = 1, FUN = score_similarity, selector_vector = selector.sylrep),index.return = T)$ix
      
      # orders the scored list of suitors; subsets one suitor from the rest,
      # according to the value of the selector's (auditory) curiosity.
      singer <- golf_score[round(curiosity_level[selector.index, population] * (universal_parameters$one_pop_singers[select_type] * universal_parameters$num_pop) + 0.5)]
      if(sum(selection.sylreps[singer,])==0) {
        chance_for_selection = chance_for_selection + 1
        next}
      
      #should_pick_neighbor <- function(index,lower,upper=Inf) {
      
      if(!interbreed) {
        should_continue <- TRUE
        if(singer %in% singSuccessFilter) {
          
          moran = update_selexn_data(universal_parameters, moran, selection.index, singer, selector.index, curiosity_level, 
                             population, select_type, selection.sylreps, selector.sylrep, 
                             chance_for_selection)
          
          should_continue <- FALSE
        }
        
        if(should_continue) {
          for(neighbor in c(1, -1)) {
            if(should_pick_neighbor(neighbor,num_select_chances,select_type,chance_for_selection,golf_score,singSuccessFilter,singer,lower=0.5,upper=0.75)) {
              singer <- golf_score[singer+neighbor]
              
              moran = update_selexn_data(universal_parameters, moran, selection.index, singer, selector.index, curiosity_level, 
                             population, select_type, selection.sylreps, selector.sylrep, 
                             chance_for_selection)
              
              should_continue <- FALSE
              break
            }
          }
        }
          
        if(should_continue) {
          for(neighbor in c(1, -1, 2, -2)) {
            if(should_pick_neighbor(neighbor,num_select_chances,select_type,chance_for_selection,golf_score,singSuccessFilter,singer,lower=0.75)) {
              singer <- golf_score[singer+neighbor]

              moran = update_selexn_data(universal_parameters, moran, selection.index, singer, selector.index, curiosity_level, 
                             population, select_type, selection.sylreps, selector.sylrep, 
                             chance_for_selection)
              
              should_continue <- FALSE
              break
            }
          }
        }
        
        if(!should_continue) {
          break
        }
      } else {
        if(sum(sylrep_object[selection.index[singer], , population]) != 0) {

          moran = update_selexn_data(universal_parameters, moran, selection.index, singer, selector.index, curiosity_level, 
                             population, select_type, selection.sylreps, selector.sylrep, 
                             chance_for_selection)
          
          break
        }
      }
      chance_for_selection = chance_for_selection + 1
    }
  }
  return(moran)
}



  for(population in 1 : 2) {
    print(paste0("population = ", population))
    chance_for_selection = 100
    while(chance_for_selection <= 100) {
      stop = FALSE
      if(chance_for_selection == 100) {
        #auto.teachers <- matrix(c(sample(universal_parameters$pop_calls_matrix[1, ], sylrep_fill_chances),sample(universal_parameters$pop_calls_matrix[2, ], sylrep_fill_chances)),2,sylrep_fill_chances,T)
        for(MTsylrep_filter in 1:10){
          
          if(MTsylrep_filter==2) { # IF parents have syllables in their repertoire; otherwise, cycle back
            print(paste0("MTsylrep_filter = ", MTsylrep_filter))
            print("here it comes")
            stop = TRUE
            break
          } else {print("T _ T")}
        }
        if(stop) {
          print("oh yeah!")
          break}
      }
      print("the break worked! This doesn't get printed!")
    }
    print("This text prints after 'oh yeah!' as long as the break works")
  }

  

  # Generic form
'%=%' = function(l, r, ...) UseMethod('%=%')

# Binary Operator
'%=%.lbunch' = function(l, r, ...) {
  Envir = as.environment(-1)

  if (length(r) > length(l))
    warning("RHS has more args than LHS. Only first", length(l), "used.")

  if (length(l) > length(r))  {
    warning("LHS has more args than RHS. RHS will be repeated.")
    r <- extendToMatch(r, l)
  }

  for (II in 1:length(l)) {
    do.call('<-', list(l[[II]], r[[II]]), envir=Envir)
  }
}

# Used if LHS is larger than RHS
extendToMatch <- function(source, destin) {
  s <- length(source)
  d <- length(destin)

  # Assume that destin is a length when it is a single number and source is not
  if(d==1 && s>1 && !is.null(as.numeric(destin)))
    d <- destin

  dif <- d - s
  if (dif > 0) {
    source <- rep(source, ceiling(d/s))[1:d]
  }
  return (source)
}

# Grouping the left hand side
g = function(...) {
  List = as.list(substitute(list(...)))[-1L]
  class(List) = 'lbunch'
  return(List)
}


shifting_curstart <- 1
paramsFile <- c("params.yaml")
paramsSource = paramsFile
params <- yaml.load_file(file.path("parameters", paramsSource))

scMin = c(
        params$curstarts[[shifting_curstart]]$scMin[1],
        params$curstarts[[shifting_curstart]]$scMin[2],
        params$curstarts[[shifting_curstart]]$scMin[3],
        params$curstarts[[shifting_curstart]]$scMin[4])
      scMax = c(
        params$curstarts[[shifting_curstart]]$scMax[1],
        params$curstarts[[shifting_curstart]]$scMax[2],
        params$curstarts[[shifting_curstart]]$scMax[3],
        params$curstarts[[shifting_curstart]]$scMax[4])
      simNumber = params$simNumberStart + (shifting_curstart - 1)
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

"/home/parker/Documents/projects/Code/curiosity-code/results/190327_296_-_2k_nsL_normVO_oppsyl_1-7_c/variable_store/2019-03-27-144523-GMT-variable-store/"
