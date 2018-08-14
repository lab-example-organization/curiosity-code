# REPEATED-USE FUNCTIONS ##################################
library(R.utils)
syll_learn <- function(P, context = 2){ # context decides whether the learning is vertical (2) or oblique (1)
  
  for(population in 1 : P$num_pop) {
    for(number_renewed in 1 : P$nropsp) {
      # Make the reference objects for the teacher - the indices for the syllables unique to the teacher's repertoire, and a set of probabilities for each syllable to be learned
      
      if(context == 2) { # clear the sylreps rows about to be filled in :D
        source_of_ONEs <- which(P$learning.pool[1, , population, number_renewed] == 1)
        for(sex in 1 : 2) {
          for(syllable in 1 : P$sylnum) {
            P$learning.pool[(sex + 2), syllable, population, number_renewed] <- 0
          }
        }
      } else { # Oblique Learning
        source_of_ONEs <- which(!(which(P$learning.pool[1, , population, number_renewed] == 1) %in% which(P$learning.pool[3, , population, number_renewed] == 1)))
      }
      
      teacher.mean <- mean(source_of_ONEs)
      probs <- runif(source_of_ONEs, 0, 1)
      
      for (sex in 1:context) {
        for (sylls_to_learn in 1:length(source_of_ONEs)) {
          if((probs[sylls_to_learn]) <= (P$learnprob[context]-P$randlearnprob[context])) {
            P$learning.pool[(sex + 2), source_of_ONEs[sylls_to_learn], population, number_renewed] <- 1 # nropsp!!! come on! still have to figure that one out i guess
          } else if(probs[sylls_to_learn] <= P$learnprob[context]) {
              r_norm <- rnorm(1, mean = teacher.mean, sd = P$stand.dev)
              if(r_norm > P$sylnum) {
                r_norm <- P$sylnum
              } else if(r_norm < 1) {
                  r_norm <- 1
                }
              P$learning.pool[(sex + 2), floor(r_norm), population, number_renewed] <- 1
          }
        }
      }
    }
  }
return(P)
}

variable.archive <- function(P, timestep) {
  #context_name <- c("parents&offspring","replacedindividuals")
  #if(context == 1) {
    for(number_renewed in 1:P$nropsp) {
      for(population in 1 : P$num_pop) {
        day.tuh[["curity_mean_t"]][3, (population + ((number_renewed-1) * P$num_pop)), timestep] <- P$pairing.pool[2, 3, population, number_renewed]
        day.tuh[["curity_mean_t"]][10, (population + ((number_renewed-1) * P$num_pop)), timestep] <- P$pairing.pool[2, 1, population, number_renewed]
        
        for(sex in 1:2) {
          day.tuh[["sylrep_rowcol"]][sex, population, timestep] <- mean(rowSums(sylreps[((1 + ((sex - 1) * (P$pop_size / 2))) : (sex * (P$pop_size / 2))), , population]))
          day.tuh[["sylrep_dstbxn"]][(((population - 1) * 2) + sex), , timestep] <- colSums(sylreps[((1 + ((sex - 1) * (P$pop_size / 2))) : (sex * (P$pop_size / 2))), , population]) # day.tuh[sylnums filled in by population[sex]] <- colSums(sylnums called by population[sex])
          day.tuh[["curity_mean_t"]][sex, population, timestep] <- mean(curiosity_level[((1 + ((sex-1) * P$pop_size/2)):(sex * P$pop_size/2)), population])
          day.tuh[["curity_repert"]][(sex + ((population - 1) * 2)), , timestep] <- hist(curiosity_level[((1 + ((sex-1) * P$pop_size / 2)):(sex * P$pop_size / 2)), population], breaks = P$curiositybreaks, plot = FALSE)$counts
          day.tuh[["curity_mean_t"]][(sex + 3), population, timestep] <- P$pairing.pool[sex, 2, population, number_renewed]
          day.tuh[["curity_mean_t"]][(sex + 5), (population + ((number_renewed-1) * P$num_pop)), timestep] <- P$pairing.pool[1, 1, population, number_renewed]
          day.tuh[["curity_mean_t"]][11, (population + ((number_renewed-1) * P$num_pop)), timestep] <- P$pairing.pool[(sex + 2), 5, population, number_renewed]
          day.tuh[["curity_mean_t"]][(sex + 7), (population + ((number_renewed-1) * P$num_pop)), timestep] <- P$pairing.pool[(sex + 2), 4, population, number_renewed]
        }
      }
    }
 #}# else {
    #for(number_renewed in 1:P$nropsp) {
    #  for(population in 1 : P$num_pop) {
    #    for(sex in 1:2) {
    #      replaced_index <- P$pairing.pool[(sex + 2 ), 1, population, number_renewed]
    #      day.tuh[["curity_mean_t"]][(sex + 7), (population + ((number_renewed-1) * P$num_pop)), timestep] <- curiosity_level[replaced_index, population]
    #    }
    #  }
    #}
  #}
  return(day.tuh)
}

make.offspring.calls <- function(P, no.parent.turnover = TRUE){
  
    for(population in 1:P$num_pop){
      for(sex in 1:2){
        new_index <- c(sample(P$pop_calls_matrix[sex, ], P$nropsp, replace=F))
        if(no.parent.turnover == TRUE) {
          for(number_renewed in 1:P$nropsp) {
            while(new_index[number_renewed] %in% P$pairing.pool[sex, 1, population,]) {
              new_index[number_renewed] <- c(sample(P$pop_calls_matrix[sex, ], 1, replace=F))
            }
          }
        }
        P$pairing.pool[(sex + 2), 1, population, ] <-  new_index
      }
    }
  
  return(P)
}

recuriosity.offspring <- function(P) {
  for(number_renewed in 1:P$nropsp) {
    for(population in 1:P$num_pop) {
      for(sex in 1:2) {
        index <- P$pairing.pool[(sex + 2), 1, population, number_renewed]
        curiosity_level[index, population] <- P$pairing.pool[(sex + 2), 2, population, number_renewed]
      }
    }
  }
  return(curiosity_level)
}

resylreps.offspring <- function(P) {
  index <- rep(0, P$sylnum)
  for(number_renewed in 1:P$nropsp) {
    for(population in 1:P$num_pop) {
      for(sex in 1:2) {
        index <- P$pairing.pool[(sex + 2), 1, population, number_renewed]
        index_sylrep <- P$learning.pool[(sex + 2), , population, number_renewed]
        sylreps[index, , population] <- index_sylrep
      }
    }
  }
  return(sylreps)
}

sing.selection <- function(P, curiosity_level, context, num_select_chances = c(42, 10), verbose_output = TRUE){ 
  # context could either be females choosing a mate, or young males choosing tutors. 1 == mate selection; not-1 == tutor selection
  # not important unless the output file "Female Select Chances (1) Per Timestep.tiff" is this number for long stretches - then something is WRONG. :P
  # Curiosity Variables 2: derivative variables (potential to eliminate)
  similarity_golf.score <- rep(0, (P$num_one.pop_singers_sampled[context] * P$num_pop))
  selection_sylreps <- array(0, c((P$num_one.pop_singers_sampled[context] * P$num_pop), P$sylnum))
  selection.index <- array(0, P$num_pop * P$num_one.pop_singers_sampled[context])
  context.name <- c("Tutor", "Mate")
  
  for(population in 1 : P$num_pop) { #population <- 1 rm(population)
    #print(paste("this is population",population,sep=" "))
    for(number_renewed in 1:P$nropsp) {
      stop = FALSE
      for(k in 1 : num_select_chances[context]) {
        if(k == num_select_chances[context]) {
          if(verbose_output == TRUE) {
            warning(print(paste0("Maxed out num_select_chances for Population ", population, " ", context.name[context], " Selection")))
          }
          auto.teachers <- c((sample(P$pop_calls_matrix[1, ], 1, replace = T)), (sample(P$pop_calls_matrix[2, ], 1, replace = T))) # this will be referenced later using the curiosity_level, which separates females and males by row, so their column reference index number is limited to 1:200; so the first row of the pop_calls_matrix is appropriate for both of them.
          if(verbose_output == TRUE) {
            warning(print(paste0("Automatic Teacher(s) = ", auto.teachers, " ", context.name[context], " Selection")))
          }
          for(sex in 1:context) {
            P$learning.pool[sex, , population, number_renewed] <- sylreps[auto.teachers[sex], , population]
            P$pairing.pool[sex, 1, population, number_renewed] <- auto.teachers[sex] + ((sex - 1) * P$pop_size/2)
            P$pairing.pool[sex, 2, population, number_renewed] <- curiosity_level[auto.teachers[sex], population]
          }
          P$pairing.pool[2, 3, population, number_renewed] <- k
          break
        }
        
        selector_context <- list(
          tutor_selector = paste0("selector.index <- P$pairing.pool[3, 1, ", population, ", ", number_renewed, "]"),
          mate_selector = paste0("selector.index <- sample(P$pop_calls_matrix[2, ], 1)")
        )
        eval(parse(text=selector_context[[context]]))
        selector.sylrep <- sylreps[selector.index, , population]
        
        for(singerpop in 1 : P$num_pop) {
          selection.index[(1 + ((singerpop - 1) * P$num_one.pop_singers_sampled[context])) : (singerpop * P$num_one.pop_singers_sampled[context])] <- sample(P$pop_calls_matrix[1, ], P$num_one.pop_singers_sampled[context], replace = FALSE)
          #####Include a check here to make sure that the sampled singers aren't going to include the father (for the tutor context)
          selection_sylreps[(1 + ((singerpop - 1) * P$num_one.pop_singers_sampled[context])) : (singerpop * P$num_one.pop_singers_sampled[context]), ] <- sylreps[selection.index[(1 + ((singerpop - 1) * P$num_one.pop_singers_sampled[context])):(singerpop * P$num_one.pop_singers_sampled[context])], , singerpop]
          for(singer.pool in 1 : P$num_one.pop_singers_sampled[context]) {
            similarity_golf.score[singer.pool + ((singerpop - 1) * P$num_one.pop_singers_sampled[context])] <- sum(abs(which((selection_sylreps[(singer.pool + ((singerpop - 1) * P$num_one.pop_singers_sampled[context])), ] - selector.sylrep) != 0) - median(which(selector.sylrep == 1))))
          }
        }
        singer <- ((sort(similarity_golf.score, index.return = TRUE))$ix)[round(curiosity_level[selector.index, population] * (P$num_one.pop_singers_sampled[context] * P$num_pop) + 0.5)]
        #print(paste("singer =",singer,sep=" "))
        #BUT FIRST: Put in instructions to interrupt the process if her mate is from the other species
        if(singer %in% ( (1 + ((population - 1) * P$num_one.pop_singers_sampled[context])) : (population * P$num_one.pop_singers_sampled[context]) ) == TRUE ) {     # ((((population-1)*num_one.pop_singers_sampled)+1):(population*num_one.pop_singers_sampled))
          singer.index <- selection.index[singer]
          indices <- c(singer.index, selector.index)
          
          for(sex in 1:context) {
            P$learning.pool[sex, , population, number_renewed] <- sylreps[indices[sex], , population]
            P$pairing.pool[sex, 1, population, number_renewed] <- indices[sex]
            P$pairing.pool[sex, 2, population, number_renewed] <- curiosity_level[indices[sex], population]
          }
          P$pairing.pool[2, 3, population, number_renewed] <- k
          stop = TRUE
          break
        }
      }
      if(stop) {next} 
    }
  }
  return(P)
}

# curinh.row - calling either the row number or name of row for different curiosity inheritance patterns - 1: father; 2: mother; 3: same; 4:opposite

curiosity_learn <- function(P, curlearnprob = 0.95, timestep = single_timestep, curinh.row = 1){
  curinh_patterns <- array(data = c(1, 2, 1, 2, 1, 2, 2, 1), dim = c(4,2), dimnames = list(c("father", "mother", "same", "opposite"), c("male birb", "female birb")))
  newcuriosity <- array(data = runif((P$nropsp * P$num_pop * 2), 1 - P$curflux, 1 + P$curflux), dim = c(2, P$num_pop, P$nropsp))
  for(number_renewed in 1 : P$nropsp) {
    for(population in 1 : (P$num_pop)) {
      if(P$pairing.pool[1, 2, population, number_renewed] == 0) {stop("probably not the best time to be learning curiosity from your parents right now...")}
      for(sex in 1:2) {
        new.curiosity <- ((1 - curlearnprob) * (newcuriosity[sex, population, number_renewed])) +
                         ((curlearnprob) * (P$pairing.pool[curinh_patterns[curinh.row,sex], 2, population, number_renewed])) # Adding small proportion of noise 
        curinh_attempts <- 1
        while((new.curiosity <= 0 | new.curiosity >= 1) && (curinh_attempts <= P$new.cur.threshold)) {
          crap <- runif(1, 1 - P$curflux, 1 + P$curflux)
          new.curiosity <- ((1 - curlearnprob) * crap) + 
                           ((curlearnprob) * (P$pairing.pool[1, 1, population, number_renewed]))
          curinh_attempts <- curinh_attempts + 1
        }
        P$pairing.pool[(sex + 2), 4, population, number_renewed] <- P$pairing.pool[(sex + 2), 2, population, number_renewed]
        P$pairing.pool[(sex + 2), 2, population, number_renewed] <- new.curiosity
        P$pairing.pool[(sex + 2), 5, population, number_renewed] <- curinh_attempts
      }
    }
  }
  return(P)
}

store_timesteps <- function(filename = thousand_timesteps, object_record = day.tuh){
  directory <- getwd()
  if(filename == 1) {
    FolderName <- format(Sys.time(), "%F-%H%M%S")
    dir.create(file.path(directory, paste(FolderName, "-GMT-variable-store", sep="")))
    FolderName <- paste(directory, "/", FolderName, "-GMT-variable-store/", sep = "")
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

