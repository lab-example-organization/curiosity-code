# REPEATED-USE FUNCTIONS ##################################
library(R.utils)
syll_learn <- function(P, context = 2, totally_new = FALSE, randlearn_context = 1){ # context decides whether the learning is vertical (2) or oblique (1)
  randlearncontext_container <- c("teacher.mean", "source_of_ONEs[sylls_to_learn]")
  for(population in 1 : P$num_pop) {
    # Make the reference objects for the teacher - the indices for the syllables unique to the teacher's repertoire, and a set of probabilities for each syllable to be learned
    if(context == 2) {
      source_of_ONEs <- which(P$learning.pool[1, , population] == 1) # calls for sylls vertical tutor (father) has
      if(length(source_of_ONEs) == 0) {
        saveRDS(object = P, file = "parent with no sylls.txt")
        print(P$learning.pool[1, , population])
        stop("wot? parent has no syllables?!")
        } #address syll loss by stopping script if parent has no sylls
      for(sex in 1 : 2) {
          P$learning.pool[(sex + 2), , population] <- rep(0, P$sylnum)
      } # Vertical Learning; clear the sylreps rows about to be filled in :D
    } else {
      source_of_ONEs <- which(!(which(P$learning.pool[5, , population] == 1) %in% which(P$learning.pool[3, , population] == 1)))
      if(length(source_of_ONEs) == 0) {
        print(paste0("tutor has no syllables for population ", population))
        next} # if curiosity is so low that tutor can teach nothing, just skip this population's tutor learning step
    } # Oblique Learning
    if(randlearn_context == 1) {
      teacher.mean <- mean(source_of_ONEs)
    } else {
      
    }
    probs <- runif(source_of_ONEs, 0, 1)
    
    #sink(file = paste("syll_learn pop", population, "probs.txt", sep = " "), append = T)
    #print(probs)
    #sink()
    
    for (sex in 1:context) {
      average_rate_randlearn_overlap <- c()
      for (sylls_to_learn in 1:length(source_of_ONEs)) {
        P$learning.pool[(sex + 2), source_of_ONEs[sylls_to_learn], population] <- 0
        if(probs[sylls_to_learn] <= (P$learnprob[context])) {
          P$learning.pool[(sex + 2), source_of_ONEs[sylls_to_learn], population] <- 1 # nropsp!!! come on! still have to figure that one out i guess
          if(probs[sylls_to_learn] > (1 - P$randlearnprob[context])) {
            r_norm <- rnorm(1, mean = eval(parse(text=randlearncontext_container[randlearn_context])), sd = P$stand.dev)
            if(r_norm > P$sylnum) {
              r_norm <- P$sylnum
            } else if(r_norm < 1) {
                r_norm <- 1
            }
            if(totally_new == TRUE) {
              counter <- 1
              while(P$learning.pool[(sex + 2), floor(r_norm), population] == 1) {
                r_norm_pool <- rnorm(100, mean = eval(parse(text=randlearncontext_container[randlearn_context])), sd = P$stand.dev)
                r_norm <- r_norm_pool[counter]
                if(r_norm > P$sylnum) {
                  r_norm <- P$sylnum
                } else if(r_norm < 1) {
                  r_norm <- 1
                }
                counter = counter + 1
              }
              P$learning.pool[(sex + 2), floor(r_norm), population] <- 1
              average_rate_randlearn_overlap <- append(average_rate_randlearn_overlap, counter)
            } else {
              P$learning.pool[(sex + 2), floor(r_norm), population] <- 1
            }
          }
        }
      }
    if(totally_new == TRUE) {P$pairing.pool[sex, 5, population] <- mean(average_rate_randlearn_overlap)}
    }
  }
  return(P)
}

variable.archive <- function(P, timestep) {
  #context_name <- c("parents&offspring","replacedindividuals")
  #if(context == 1) {
  for(population in 1 : P$num_pop) {
    day.tuh[["curity_mean_t"]][3, population, timestep] <- P$pairing.pool[2, 3, population]
    day.tuh[["curity_mean_t"]][10, population, timestep] <- P$pairing.pool[3, 3, population]
    
    for(sex in 1:2) {
      day.tuh[["sylrep_rowcol"]][sex, population, timestep] <- mean(rowSums(sylreps[((1 + ((sex - 1) * (P$pop_size / 2))) : (sex * (P$pop_size / 2))), , population]))
      day.tuh[["sylrep_dstbxn"]][(((population - 1) * 2) + sex), , timestep] <- colSums(sylreps[((1 + ((sex - 1) * (P$pop_size / 2))) : (sex * (P$pop_size / 2))), , population]) # day.tuh[sylnums filled in by population[sex]] <- colSums(sylnums called by population[sex])
      day.tuh[["curity_repert"]][(sex + ((population - 1) * 2)), , timestep] <- hist(curiosity_level[((1 + ((sex-1) * P$pop_size / 2)):(sex * P$pop_size / 2)), population], breaks = P$curiositybreaks, plot = FALSE)$counts
      
      day.tuh[["curity_mean_t"]][sex, population, timestep] <- mean(curiosity_level[((1 + ((sex-1) * P$pop_size/2)):(sex * P$pop_size/2)), population])
      day.tuh[["curity_mean_t"]][(sex + 3), population, timestep] <- P$pairing.pool[sex, 2, population]
      day.tuh[["curity_mean_t"]][(sex + 5), population, timestep] <- P$pairing.pool[1, 1, population]
      day.tuh[["curity_mean_t"]][(sex + 7), population, timestep] <- P$pairing.pool[(sex + 2), 4, population]
      day.tuh[["curity_mean_t"]][11, population, timestep] <- P$pairing.pool[(sex + 2), 5, population]
      day.tuh[["curity_mean_t"]][12, population, timestep] <- P$pairing.pool[sex, 5, population]
    }
  }
  return(day.tuh)
}

make.offspring.calls <- function(P){
    for(population in 1:P$num_pop){
      for(sex in 1:2){
        new_index <- c(sample(P$pop_calls_matrix[sex, ], 1, replace=F))
        P$pairing.pool[(sex + 2), 1, population] <-  new_index
      }
    }
  return(P)
}

#make.offspring.calls <- function(P){
#  
#  for(population in 1:P$num_pop){
#    for(sex in 1:2){
#      new_index <- c(sample(P$pop_calls_matrix[sex, ], 1, replace=F))
#      P$pairing.pool[(sex + 2), 1, population, ] <-  new_index
#    }
#  }
#  
#  return(P)
#}

recuriosity.offspring <- function(P) {
  for(population in 1:P$num_pop) {
    for(sex in 1:2) {
      #index <- P$pairing.pool[(sex + 2), 1, population]
      curiosity_level[P$pairing.pool[(sex + 2), 1, population], population] <- P$pairing.pool[(sex + 2), 2, population]
    }
  }
  return(curiosity_level)
}

resylreps.offspring <- function(P) {
  for(population in 1:P$num_pop) {
    for(sex in 1:2) {
      #index <- P$pairing.pool[(sex + 2), 1, population]
      #index_sylrep <- P$learning.pool[(sex + 2), , population]
      sylreps[P$pairing.pool[(sex + 2), 1, population], , population] <- P$learning.pool[(sex + 2), , population]
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

sing.selection <- function(P, curiosity_level, context, num_select_chances = c(10, 42), ohsit = 10, verbose_output = TRUE, interbreed = FALSE){ 
  context.name <- c("Tutor", "Mate")
  for(population in 1 : P$num_pop) { #population <- 1 rm(population)
    #print(paste("this is population",population,sep=" "))
    chance_for_selection = 1
    while(chance_for_selection <= num_select_chances[context]) {
      stop = FALSE
      if(chance_for_selection == num_select_chances[context]) {
        for(hope_not_necessary in 1:ohsit){
          auto.teachers <- c((sample(P$pop_calls_matrix[1, ], 1, replace = T)), (sample(P$pop_calls_matrix[2, ], 1, replace = T))) # this will be referenced later using the curiosity_level, which separates females and males by row, so their column reference index number is limited to 1:200; so the first row of the pop_calls_matrix is appropriate for both of them.
          if((sum(sylreps[auto.teachers[1], , population]) != 0) && (sum(sylreps[auto.teachers[2], , population]) != 0)) {
            if(verbose_output == TRUE) {
              warning(print(paste0("Automatic Teacher(s) = ", auto.teachers, " for Population ", population, " ", context.name[context], " Selection")))
            }
            for(sex in 1:context) {
              P$learning.pool[((5^(2-context)) * sex), , population] <- sylreps[auto.teachers[sex], , population]
              P$pairing.pool[((5^(2-context)) * sex), 1, population] <- auto.teachers[sex] + ((sex - 1) * P$pop_size/2)
              P$pairing.pool[((5^(2-context)) * sex), 2, population] <- curiosity_level[auto.teachers[sex], population]
            } # this will fill pairing.pool with (Mate) male and female metadata, or (Tutor) male metadata
            P$pairing.pool[(4-context), 3, population] <- chance_for_selection
            stop = TRUE
            break
          } else {next}
        }
        if(stop) {break}
      }
      
      #This statement separates specific mating and tutoring selection qualities:
        # singer_eval will inform the selection of a mate by restricting the successful mate 
        # to those individuals from the same population as the selector. Similarly, 
        # selector.index distinguishes between mating and tutoring, except here it uses
        # a randomly-selected female for the mating context, and the offspring for tutoring.
      if(context == 1) {
        singer_eval <- (1 : ((P$num_one.pop_singers_sampled[context]) * (P$num_pop)))
        selector.index <- P$pairing.pool[3, 1, population]
      } else {
        singer_eval <- ((1 + ((population - 1) * (P$num_one.pop_singers_sampled[context]))) : (population * P$num_one.pop_singers_sampled[context]))
        selector.index <- sample(P$pop_calls_matrix[2, ], 1)
      }
      
      # Finds the syllable repertoire (sylrep) of the selector, by referencing the sylrep array.
      selector.sylrep <- sylreps[selector.index, , population]
      
      # Finds the median for the selector's sylrep - will be useful for establishing similarity of suitors.
      median_s.r_sylrep <- median(which(selector.sylrep == 1))
      
      # The basic sylrep comparison caluclation- finds the differences 
      # between the suitor and selector sylreps, then weights them by 
      # their distance from the selector's median.
      stdev_s.n_vs_s.r_sylrep <- function(vector_to_compare) {
        return(
          sum(
            abs(
              which((vector_to_compare - selector.sylrep) != 0) - median_s.r_sylrep)
          )
        )
      } #Output: value of similarity/dissimilarity between sylrep of suitors and selector.
      
      # This creates sample calls for each population; each population has a sample size of
      # P$num_one.pop_singers_sampled, which comes from the male half of the population.
      # probability defined by the fraction of syllable repertoires of each member of each population divided by the maximum syllrep of the population.
      selection.index <- c(
        sapply(
          1:P$num_pop, function(x) {
            sample(
              x = P$pop_calls_matrix[1,], 
              size = P$num_one.pop_singers_sampled[context], 
              replace = FALSE, prob = (
                (apply(sylreps[P$pop_calls_matrix[1,],,x],1,sum)
                 )/max(
                    apply(sylreps[P$pop_calls_matrix[1,],,x],1,sum)
                  )
                )
              )
            }
          )
        ) # probability = the number of times each individual's syllable repertoire has a 1 in it (sum(sylreps[P$pop_calls_matrix[1,]])), divided by the biggest repertoire's total.
      
      # create a matrix of all the sylreps of the sample males from selection.index
      selection.sylreps <- apply(sylreps[selection.index[1:(P$num_pop * P$num_one.pop_singers_sampled[1])],,1],2,c)
      
      # applies the standard deviation scoring to the males in selection.sylreps; 
      # larger score means greater difference between male sylrep and selector's sylrep.
      golf_score <- apply(selection.sylreps, 1, stdev_s.n_vs_s.r_sylrep)
      
      # orders the scored list of suitors; subsets one suitor from the rest,
      # according to the value of the selector's (auditory) curiosity.
      singer <- ((sort(golf_score, index.return = TRUE))$ix)[round(curiosity_level[selector.index, population] * (P$num_one.pop_singers_sampled[context] * P$num_pop) + 0.5)]
      
      # This
      if(interbreed == FALSE) {
        if((singer %in% singer_eval) && (sum(sylreps[selection.index[singer], , population]) != 0)) {
          singer.index <- selection.index[singer]
          indices <- c(singer.index, selector.index)
          
          for(sex in 1:context) {
            P$learning.pool[((5^(2-context)) * sex), , population] <- sylreps[indices[sex], , population]
            P$pairing.pool[((5^(2-context)) * sex), 1, population] <- indices[sex]
            P$pairing.pool[((5^(2-context)) * sex), 2, population] <- curiosity_level[indices[sex], population]
          }
          P$pairing.pool[(4 - context), 3, population] <- chance_for_selection
          break
        }
      } else {
        singer.index <- selection.index[singer]
        indices <- c(singer.index, selector.index)
        
        for(sex in 1:context) {
          P$learning.pool[((5^(2-context)) * sex), , population] <- sylreps[indices[sex], , population]
          P$pairing.pool[((5^(2-context)) * sex), 1, population] <- indices[sex]
          P$pairing.pool[((5^(2-context)) * sex), 2, population] <- curiosity_level[indices[sex], population]
        }
        P$pairing.pool[(4 - context), 3, population] <- chance_for_selection
        break
      }
      chance_for_selection = chance_for_selection + 1
    }
  }
  return(P)
}

# curinh.row - calling either the row number or name of row for different curiosity inheritance patterns - 1: father; 2: mother; 3: same; 4:opposite

curiosity_learn <- function(P, curlearnprob = 0.95, timestep = single_timestep, curinh.row = 1){
  #print("blah 1")
  curinh_patterns <- array(data = c(1, 2, 1, 2, 1, 2, 2, 1), dim = c(4,2), dimnames = list(c("father", "mother", "same", "opposite"), c("male birb", "female birb")))
  #print("blah 2")
  newcuriosity <- array(data = runif((P$num_pop * 2), -1, 1), dim = c(2, P$num_pop))
  #print("blah 3")
  for(population in 1 : (P$num_pop)) {
    #print(paste("blah 4 - population ", population))
    for(sex in 1:2) {
      if(P$pairing.pool[curinh_patterns[curinh.row,sex], 2, population] == 0) {stop("probably not the best time to be learning curiosity from your parents right now...")}
      #print(paste("blah 4 - population ", population, " and sex ", sex))
      curinh_attempts <- 1
      while((P$pairing.pool[curinh_patterns[curinh.row,sex], 2, population] + ((1 - curlearnprob) * (newcuriosity[sex, population]))) < 0) {
        newcuriosity[sex, population] <- runif(1, 0, 1)
        curinh_attempts <- curinh_attempts + 1
      }
      while((P$pairing.pool[curinh_patterns[curinh.row,sex], 2, population] + ((1 - curlearnprob) * (newcuriosity[sex, population]))) > 1) {
        newcuriosity[sex, population] <- runif(1, -1, 0)
        curinh_attempts <- curinh_attempts + 1
      }
      
      new.curiosity <- P$pairing.pool[curinh_patterns[curinh.row,sex], 2, population] + ((1 - curlearnprob) * (newcuriosity[sex, population]))
      
      #new.curiosity <- P$pairing.pool[curinh_patterns[curinh.row,sex], 2, population] + ((1 - curlearnprob) * (newcuriosity[sex, population])) # Adding small proportion of noise 
      #print(paste(new.curiosity, " = new.curiosity"))
      
      #print(curinh_attempts)
      
      
      P$pairing.pool[(sex + 2), 4, population] <- P$pairing.pool[(sex + 2), 2, population]
      P$pairing.pool[(sex + 2), 2, population] <- new.curiosity
      P$pairing.pool[(sex + 2), 5, population] <- curinh_attempts
    }
  }
  return(P)
}
  


store_timesteps <- function(filename = thousand_timesteps, object_record = day.tuh){
  directory <- getwd()
  if(filename == 1) {
    FolderName <- format(Sys.time(), "%F-%H%M%S")
    dir.create(file.path(directory, paste0(FolderName, "-GMT-variable-store")))
    FolderName <- paste0(directory, "/", FolderName, "-GMT-variable-store/")
    setwd(FolderName)
    saveRDS(object = stuff_to_save, file = "metadata.RData")
    rm(init_params, funx_n_params, datez, deetz, docnamez, stuff_to_save)
    setwd(directory)
  }
  setwd(FolderName)
  for(deyteh in 1:length(object_record)) {
    zfilename <- file.create(paste("variable-store-", filename, "-", names(object_record)[[deyteh]], ".RData", sep = ""))
    objekshun <- object_record[[deyteh]]
    saveRDS(object = objekshun, file = paste(FolderName, paste("variable-store-", filename, "-", names(object_record)[[deyteh]], ".RData", sep = ""), sep = ""))
  }
  
  saveRDS(object = FolderName, file = "harvest_info.RData")
  saveRDS(object = P, file = "parameters.RData")
  saveRDS(object = thousand_timesteps, file = "timestep_grps.RData")
  
  setwd(directory)
  return(FolderName)
}

