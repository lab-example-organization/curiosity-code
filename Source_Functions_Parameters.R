# REPEATED-USE FUNCTIONS ##################################
library(R.utils)
library(dplyr)
syll_learn <- function(P, moran, select_type = 2, totally_new = FALSE, randlearn_context = 1, verbose = FALSE){ # context decides whether the learning is vertical (2) or oblique (1)
  randlearncontext_container <- c("mean(source_of_ONEs)", "source_of_ONEs[sylls_to_learn]")
  for(population in 1 : P$num_pop) {
    # Make the reference objects for the teacher - the indices for the syllables unique to the teacher's repertoire, and a set of probabilities for each syllable to be learned
    
    #Vertical Learning;  params (set up source_of_ONEs), and considerations
    if(select_type == 2) { #params and considerations for VERTICAL LEARNING
      
      source_of_ONEs <- which(moran$learning.pool[1, , population] == 1) # calls for sylls vertical tutor (father) has
      if(length(source_of_ONEs) == 0) {
        saveRDS(object = P, file = "parent with no sylls.txt")
        print(moran$learning.pool[1, , population])
        stop("wot? parent has no syllables?!")
        } #address syll loss by stopping script if parent has no sylls
      for(sex in 1 : 2) {
          moran$learning.pool[(sex + 2), , population] <- rep(0, P$sylnum)
      } # clear the sylreps rows about to be filled in :D
      
      
    } else { #Oblique Learning; source_of_ONEs setup, and considerations 
      
      # double-check tutor isn't out of sylls before comparing repertoire to pupil.
      source_of_ONEs <- which(moran$learning.pool[5, , population] == 1)
      pupil_has_ONEs <- which(moran$learning.pool[3, , population] == 1)
      if(length(source_of_ONEs) == 0) {
        stop("wot? tutor has no syllables?!")
      }
      
      # as often happens with super self-philes, the tutor may not have anything new to give the pupil. If this is the case, this skips to the next step in the for loop.
      #source_of_ONEs <- which(moran$learning.pool[5, , population] == 1)[which(!(which(moran$learning.pool[5, , population] == 1) %in% which(moran$learning.pool[3, , population] == 1)))]
      source_of_ONEs <- setdiff(source_of_ONEs,pupil_has_ONEs)
      if(length(source_of_ONEs) == 0) {
        if(verbose == TRUE) {
          print(paste0("tutor has no syllables for population ", population))
        }
        next} # if curiosity is so low that tutor can teach nothing, just skip this population's tutor learning step
    } # Oblique Learning params and considerations
    #if(randlearn_context == 1) {
    #  teacher.mean <- mean(source_of_ONEs)
    #}
    
    
    #sink(file = paste("syll_learn pop", population, "probs.txt", sep = " "), append = T)
    #print(probs)
    #sink()
    
    for (sex in 1:select_type) {
      average_rate_randlearn_overlap <- c()
      #print(source_of_ONEs)
      probs <- runif(source_of_ONEs, 0, 1)
      for (sylls_to_learn in 1:length(source_of_ONEs)) {
        #moran$learning.pool[(sex + 2), source_of_ONEs[sylls_to_learn], population] <- 0
        if(probs[sylls_to_learn] <= (P$learnprob[select_type])) {
          moran$learning.pool[(sex + 2), source_of_ONEs[sylls_to_learn], population] <- 1
        }
        if(probs[sylls_to_learn] > (1 - P$randlearnprob[select_type])) {
          r_norm <- rnorm(1, mean = eval(parse(text=randlearncontext_container[randlearn_context])), sd = P$stand.dev)
          if(r_norm > P$sylnum) {
            r_norm <- P$sylnum
          } else if(r_norm < 1) {
              r_norm <- 1
          }
          #totally_new refers to the idea that if a pupil is learning a sound
          if(totally_new == TRUE) {
            counter <- 1
            r_norm_pool <- rnorm(100, mean = eval(parse(text=randlearncontext_container[randlearn_context])), sd = P$stand.dev)
            while(moran$learning.pool[(sex + 2), floor(r_norm), population] == 1) {
              r_norm <- r_norm_pool[counter]
              if(r_norm > P$sylnum) {
                r_norm <- P$sylnum
              } else if(r_norm < 1) {
                r_norm <- 1
              }
              counter = counter + 1
            }
            moran$learning.pool[(sex + 2), floor(r_norm), population] <- 1
            average_rate_randlearn_overlap <- append(average_rate_randlearn_overlap, counter)
          } else {
            moran$learning.pool[(sex + 2), floor(r_norm), population] <- 1
          }
        }
      }
    if(totally_new == TRUE) {moran$pairing.pool[sex, 5, population] <- mean(average_rate_randlearn_overlap)}
    }
  }
  return(P)
}

variable.archive <- function(P, moran, syllable_object, curiosity_object, data_container, timestep) {
  #context_name <- c("parents&offspring","replacedindividuals")
  #if(context == 1) {
  for(population in 1 : P$num_pop) {
    data_container[["curity_mean_t"]][3, population, timestep] <- moran$pairing.pool[2, 3, population]
    data_container[["curity_mean_t"]][10, population, timestep] <- moran$pairing.pool[3, 3, population]
    
    for(sex in 1:2) {
      data_container[["sylrep_rowcol"]][sex, population, timestep] <- mean(rowSums(syllable_object[((1 + ((sex - 1) * (P$pop_size / 2))) : (sex * (P$pop_size / 2))), , population]))
      data_container[["sylrep_dstbxn"]][(((population - 1) * 2) + sex), , timestep] <- colSums(syllable_object[((1 + ((sex - 1) * (P$pop_size / 2))) : (sex * (P$pop_size / 2))), , population]) # data_container[sylnums filled in by population[sex]] <- colSums(sylnums called by population[sex])
      data_container[["curity_repert"]][(sex + ((population - 1) * 2)), , timestep] <- hist(curiosity_object[((1 + ((sex-1) * P$pop_size / 2)):(sex * P$pop_size / 2)), population], breaks = P$curiositybreaks, plot = FALSE)$counts
      
      data_container[["curity_mean_t"]][sex, population, timestep] <- mean(curiosity_object[((1 + ((sex-1) * P$pop_size/2)):(sex * P$pop_size/2)), population])
      data_container[["curity_mean_t"]][(sex + 3), population, timestep] <- moran$pairing.pool[sex, 2, population]
      data_container[["curity_mean_t"]][(sex + 5), population, timestep] <- moran$pairing.pool[(sex + 2), 2, population]
      data_container[["curity_mean_t"]][(sex + 7), population, timestep] <- moran$pairing.pool[(sex + 2), 4, population]
      data_container[["curity_mean_t"]][11, population, timestep] <- moran$pairing.pool[(sex + 2), 5, population]
      data_container[["curity_mean_t"]][12, population, timestep] <- moran$pairing.pool[sex, 5, population]
    }
  }
  return(data_container)
}

make.offspring.calls <- function(P, moran){
    for(population in 1:P$num_pop){
      for(sex in 1:2){
        new_index <- c(sample(P$pop_calls_matrix[sex, ], 1, replace=F))
        moran$pairing.pool[(sex + 2), 1, population] <-  new_index
      }
    }
  return(P)
}

recuriosity.offspring <- function(P, moran, curiosity_object) {
  for(population in 1:P$num_pop) {
    for(sex in 1:2) {
      #index <- moran$pairing.pool[(sex + 2), 1, population]
      curiosity_object[moran$pairing.pool[(sex + 2), 1, population], population] <- moran$pairing.pool[(sex + 2), 2, population]
    }
  }
  return(curiosity_object)
}

resylreps.offspring <- function(P, moran) {
  for(population in 1:P$num_pop) {
    for(sex in 1:2) {
      #index <- moran$pairing.pool[(sex + 2), 1, population]
      #index_sylrep <- moran$learning.pool[(sex + 2), , population]
      sylreps[moran$pairing.pool[(sex + 2), 1, population], , population] <- moran$learning.pool[(sex + 2), , population]
    }
  }
  return(sylreps)
}

output_checker <- function(printer) {
  dir <- getwd()
  if(!dir.exists(file.path(dir, "outputChecking"))) {dir.create("outputChecking")}
  setwd(paste0(dir, "/outputChecking"))
  temp <- paste("sink(file = ", paste0("\"", deparse(substitute(printer)), "_", Sys.Date(), ".txt\","), " append = T)", "print(printer)", "sink()", sep = "\n")
  return(eval(parse(text=temp)))
  setwd(dir)
}

jank_data_generator <- function(universal_parameters, curiosity_level) {
  reference_materials <- list(
    sylnum <- universal_parameters$sylnum,
    num_pop <- universal_parameters$num_pop
  )
  population_data <- list(
    pop_curiosity_values <- array(sample(c(0:1),(universal_parameters$pop_size * universal_parameters$num_pop)),c())
  )
  pool_objects <- list(
    learning.pool = array(sample(c(0,1),5*sylnum*num_pop),c(5,sylnum,num_pop)),
    pairing.pool = array(c(sample(401:500,(25*num_pop))),c(5,5,num_pop))
  )
  jank <- list(
    pool_objects = pool_objects,
    reference_materials = reference_materials
  )
  return(jank)
}

update_selexn_data <- function(universal_parameters, moran, suitor_choices, preferred_bird, selector_bird,
                                    curiosity_value, selector_population, selection_context, 
                                    sylreps_choices, sylrep_selector, selection_count) {
  singer_population <- ceiling(preferred_bird/universal_parameters$one_pop_singers[selection_context])
  selected_pair <- c(suitor_choices[preferred_bird], # Bird being selected
                     selector_bird)          # Bird doing the selecting
  sylrep_pairs <- rbind(sylreps_choices[preferred_bird,],
                        sylrep_selector)
  curiosities <- c(curiosity_value[selected_pair[1],singer_population],
                   curiosity_value[selected_pair[2],selector_population])
  
  for(bird in 1:selection_context) {
    pool.row <- (5^(2-selection_context)) * bird
    moran$pairing.pool[pool.row, 1, selector_population] <- selected_pair[bird]
    moran$learning.pool[pool.row, , selector_population] <- sylrep_pairs[bird,]
    moran$pairing.pool[pool.row, 2, selector_population] <- curiosities[bird]
  }
  moran$pairing.pool[(4 - selection_context), 3, selector_population] <- selection_count
  return(moran)
}

# P = update_selexn_data(P, selection.index, singer, selector.index, 
#                   curiosity_level, population, select_type,
#                   selection.sylreps, selector.sylrep, chance_for_selection)

#P = update_selexn_data(P, auto.teachers[1,], MTsylrep_filter, auto.teachers[2,MTsylrep_filter], 
#                curiosity_level, population, select_type,
#                sylrep_object[auto.teachers[1]:200,,population], sylrep_object[auto.teachers[2,MTsylrep_filter],,population], num_select_chances[select_type])

### goddammit, fix P so that: the truly universal stuff is its own object, (num_timesteps, num_pop, pop_size, sylnum, one_pop_singers, pop_calls_matrix, etc.)
                            # the temporary data is in a separate object, (learning.pool and pairing.pool)
                            # the initializing stuff is in a separate object. (zero_to_one_template, population_syll_probs, nsspl, etc.)

should_pick_neighbor <- function(index,total_chances,selection_context,current_chance,sorted_selections,selection_filter,preferred_bird,lower=0,upper=Inf) {
  lower_bound <- round(total_chances[selection_context] * lower)
  upper_bound <- round(total_chances[selection_context] * upper)
  is_desperate <- between(current_chance, lower_bound, upper_bound)
  is_neighbor_better <- sorted_selections[preferred_bird+index] %in% selection_filter
  return(is_desperate && is_neighbor_better)
}

score_similarity <- function(suitor_vector, selector_vector) {
        # Standard Deviation Scoring:
        # The basic sylrep comparison caluclation that finds the differences 
        # between the suitor and selector sylreps, then assigns a weighted 
        # value based on suitor syllable distance from median of selector's sylrep.
        selector_median <- median(which(selector_vector == 1)) # Finds the median for the selector's sylrep - will be useful for establishing similarity of suitors.
        vector_diff <- which(suitor_vector-selector_vector != 0)
        AbsVal_diffs <- abs(vector_diff - selector_median)
        
        return(sum(AbsVal_diffs)) # Output: value of similarity/dissimilarity between sylrep of suitors and selector.
      }

# This function allows a type of Selector (either a female in mating phase, 
# or a pupil in tutor phase) to choose a singer according to 
# the selector's auditory curiosity value.
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

# curinh.row - calling either the row number or name of row for different curiosity inheritance patterns - 1: father; 2: mother; 3: same; 4:opposite

curiosity_learn <- function(P, moran, timestep = single_timestep, curinh.row = 1){
  
  curinh_patterns <- array(data = c(1, 2, 1, 2, 1, 2, 2, 1), dim = c(4,2))
  # For posterity: curinh_patterns <- array(data = c(1, 2, 1, 2, 1, 2, 2, 1), dim = c(4,2), 
    # dimnames = list(c("father", "mother", "same", "opposite"), c("male birb", "female birb")))
  
  newcuriosity <- array(data = runif((P$num_pop * 2), -1, 1), dim = c(2, P$num_pop))
  
  for(population in 1 : (P$num_pop)) {
    
    for(sex in 1:2) {
      if(moran$pairing.pool[curinh_patterns[curinh.row,sex], 2, population] == 0) {stop("probably not the best time to be learning curiosity from your parents right now...")}
      
      curinh_attempts <- 1
      while((moran$pairing.pool[curinh_patterns[curinh.row,sex], 2, population] + ((1 - P$curlearnprob) * (newcuriosity[sex, population]))) < 0) {
        newcuriosity[sex, population] <- runif(1, 0, 1)
        curinh_attempts <- curinh_attempts + 1
      }
      while((moran$pairing.pool[curinh_patterns[curinh.row,sex], 2, population] + ((1 - P$curlearnprob) * (newcuriosity[sex, population]))) > 1) {
        newcuriosity[sex, population] <- runif(1, -1, 0)
        curinh_attempts <- curinh_attempts + 1
      }
      
      new.curiosity <- moran$pairing.pool[curinh_patterns[curinh.row,sex], 2, population] + ((1 - P$curlearnprob) * (newcuriosity[sex, population])) # Adding small proportion of noise
      
      moran$pairing.pool[(sex + 2), 4, population] <- moran$pairing.pool[(sex + 2), 2, population]
      moran$pairing.pool[(sex + 2), 2, population] <- new.curiosity
      moran$pairing.pool[(sex + 2), 5, population] <- curinh_attempts
    }
  }
  return(moran)
}
  


store_timesteps <- function(filename = thousand_timesteps, object_record = day.tuh){
  directory <- getwd()
  results_directory <- paste0(strsplit(directory, "Code")[[1]][1],"Code/Results")
  if(filename == 1) {
    run_timedate <- format(Sys.time(), "%F-%H%M%S")
    dir.create(file.path(results_directory, stuff_to_save$docnamez))
    dir.create(file.path(results_directory, stuff_to_save$docnamez, paste0(run_timedate, "-GMT-variable-store/")))
    FolderName <- paste0(results_directory, "/", stuff_to_save$docnamez, "/", run_timedate, "-GMT-variable-store/")
    setwd(FolderName)
    saveRDS(object = stuff_to_save, file = "metadata.RData")
    #rm(init_params, funx_n_params, datez, deetz, docnamez, stuff_to_save)
    setwd(directory)
  }
  setwd(paste0(results_directory, "/", stuff_to_save$docnamez, "/"))
  #FolderName <- paste0(getwd(), "/", list.files()[length(list.files())])
  setwd(list.files()[length(list.files())])
  FolderName <- getwd()
  
  for(deyteh in 1:length(object_record)) {
    zfilename <- file.create(paste0("variable-store-", filename, "-", names(object_record)[[deyteh]], ".RData"))
    objekshun <- object_record[[deyteh]]
    saveRDS(object = objekshun, file = paste0(getwd(), paste0("/variable-store-", filename, "-", names(object_record)[[deyteh]], ".RData")))
  }
  
  #saveRDS(object = FolderName, file = "harvest_info.RData")
  saveRDS(object = P, file = "parameters.RData")
  saveRDS(object = thousand_timesteps, file = "timestep_grps.RData")
  
  setwd(directory)
  return(FolderName)
}