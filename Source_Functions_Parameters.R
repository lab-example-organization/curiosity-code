# REPEATED-USE FUNCTIONS ##################################
library(R.utils)
syll_learn <- function(P, context = 2){ # context decides whether the learning is vertical (2) or oblique (1)
  
  for(population in 1 : P$num_pop) {
    for(number_renewed in 1 : P$nropsp) {
      # Make the reference objects for the teacher - the indices for the syllables unique to the teacher's repertoire, and a set of probabilities for each syllable to be learned
      
      if(context == 2) { # clear the sylreps rows about to be filled in :D
        source_of_ONEs <- which(P$learning.pool[1, , population, number_renewed] == 1)
        if(length(source_of_ONEs) == 0) {
          saveRDS(object = P, file = "parent with no sylls.txt")
          print(P$learning.pool[1, , population, number_renewed])
          stop("wot? parent has no syllables?!")
          }
        for(sex in 1 : 2) {
            P$learning.pool[(sex + 2), , population, number_renewed] <- rep(0, P$sylnum)
        }
      } else { # Oblique Learning
        source_of_ONEs <- which(!(which(P$learning.pool[1, , population, number_renewed] == 1) %in% which(P$learning.pool[3, , population, number_renewed] == 1)))
        if(length(source_of_ONEs) == 0) {next}
      }
      
      teacher.mean <- mean(source_of_ONEs)
      probs <- runif(source_of_ONEs, 0, 1)
      #sink(file = paste("syll_learn pop", population, "probs.txt", sep = " "), append = T)
      #print(probs)
      #sink()
      
      for (sex in 1:context) {
        for (sylls_to_learn in 1:length(source_of_ONEs)) {
          P$learning.pool[(sex + 2), source_of_ONEs[sylls_to_learn], population, number_renewed] <- 1
          if(probs[sylls_to_learn] >= (P$learnprob[context])) {
            P$learning.pool[(sex + 2), source_of_ONEs[sylls_to_learn], population, number_renewed] <- 0 # nropsp!!! come on! still have to figure that one out i guess
          } else if(probs[sylls_to_learn] <= (P$learnprob[context])) {
              P$learning.pool[(sex + 2), source_of_ONEs[sylls_to_learn], population, number_renewed] <- 0
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
        day.tuh[["curity_mean_t"]][10, (population + ((number_renewed-1) * P$num_pop)), timestep] <- P$pairing.pool[3, 3, population, number_renewed]
        
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

#example <- paste0(P$num_pop, " can be as bad as ", P$nropsp)
#cat(example)

#zap <- 1:10
#example1.5 <- paste(zap, "heck", collapse = " ")
#cat(example1.5)

#example_2 <- paste("sink(file = \"example_2.txt\", append = T)", "cat(example)", "sink()", sep = "\n")
#cat(example_2)
#eval(parse(text=example_2))

output_checker <- function(printer) {
  dir <- getwd()
  if(!dir.exists(file.path(dir, "outputChecking"))) {dir.create("outputChecking")}
  setwd(paste0(dir, "/outputChecking"))
  #temp <- paste("sink(file = ", paste0(deparse(substitute(printer)), Sys.Date(), ".txt,"),"append = T)", "print(printer)", "sink()", sep = "\n")
  temp <- paste("sink(file = ", paste0("\"", deparse(substitute(printer)), "_", Sys.Date(), ".txt\","), " append = T)", "print(printer)", "sink()", sep = "\n")
  return(eval(parse(text=temp)))
  setwd(dir)
  #cat(temp)
  #return(eval(parse(text=temp)))
}

sing.selection <- function(P, curiosity_level, context, num_select_chances = c(42, 10), verbose_output = TRUE){ 
  #record <- paste("sink(file = \"sing_selection.txt\", append = T)", "print(paste(Sys.time(), \"Similarity Golf Score:\", similarity_golf.score, \"Selection Sylreps:\", selection_sylreps, \"Selection Index:\", selection.index, \"Context:\", context.name[context], sep = \"\\n\"))", "sink()", sep = "\n")
  #cat(record)
  #eval(parse(text=record))
  
  # context could either be females choosing a mate, or young males choosing tutors. 1 == mate selection; not-1 == tutor selection
  # not important unless the output file "Female Select Chances (1) Per Timestep.tiff" is this number for long stretches - then something is WRONG. :P
  # Curiosity Variables 2: derivative variables (potential to eliminate)
  context.name <- c("Tutor", "Mate")
  
  for(population in 1 : P$num_pop) { #population <- 1 rm(population)
    #print(paste("this is population",population,sep=" "))
    for(number_renewed in 1:P$nropsp) {
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
          P$pairing.pool[(4-context), 3, population, number_renewed] <- k
          break
        }
        stop <- FALSE
        selector_context <- list(
          tutor_selector = paste0("selector.index <- P$pairing.pool[3, 1, ", population, ", ", number_renewed, "]"),
          mate_selector = paste0("selector.index <- sample(P$pop_calls_matrix[2, ], 1)")
        )
        eval(parse(text=selector_context[[context]]))
        selector.sylrep <- sylreps[selector.index, , population]
        
        # populate selection.index, selection_sylreps, and similarity_golf_score - sets the stage for the sorting-assignment of singer
        for(singerpop in 1 : P$num_pop) {
          
          #####Include a check here to make sure that the sampled singers aren't going to include the father (for the tutor context)
          selection_sylreps[(1 + ((singerpop - 1) * P$num_one.pop_singers_sampled[context])) : (singerpop * P$num_one.pop_singers_sampled[context]), ] <- sylreps[selection.index[(1 + ((singerpop - 1) * P$num_one.pop_singers_sampled[context])):(singerpop * P$num_one.pop_singers_sampled[context])], , singerpop]
          for(singer.pool in 1 : P$num_one.pop_singers_sampled[context]) {
            similarity_golf.score[singer.pool + ((singerpop - 1) * P$num_one.pop_singers_sampled[context])] <- sum(abs(which((selection_sylreps[(singer.pool + ((singerpop - 1) * P$num_one.pop_singers_sampled[context])), ] - selector.sylrep) != 0) - median(which(selector.sylrep == 1))))
          }
        } 
        singer <- ((sort(similarity_golf.score, index.return = TRUE))$ix)[round(curiosity_level[selector.index, population] * (P$num_one.pop_singers_sampled[context] * P$num_pop) + 0.5)]
        #sink(file = "selection_output.txt", append = T)
        #print(paste("population", population, "selected singer ",singer, "for a", context.name[context], sep=" "))
        #sink()
        #BUT FIRST: Put in instructions to interrupt the process if her mate is from the other species
        if(context == 1) {
          singer_eval <- (1 : ((P$num_one.pop_singers_sampled[context]) * (P$num_pop)))
        } else {
          singer_eval <- ((1 + ((population - 1) * (P$num_one.pop_singers_sampled[context]))) : (population * P$num_one.pop_singers_sampled[context]))
        }
        #singer_eval <- list(
        #  tutor <- (1 : ((P$num_one.pop_singers_sampled[context]) * (P$num_pop))),
        #  mate <- ((1 + ((population - 1) * (P$num_one.pop_singers_sampled[context]))) : (population * P$num_one.pop_singers_sampled[context]))
        #)
        if(singer %in% singer_eval) {     # ((((population-1)*num_one.pop_singers_sampled)+1):(population*num_one.pop_singers_sampled))
          singer.index <- selection.index[singer]
          indices <- c(singer.index, selector.index)
          
          for(sex in 1:context) {
            P$learning.pool[sex, , population, number_renewed] <- sylreps[indices[sex], , population]
            P$pairing.pool[sex, 1, population, number_renewed] <- indices[sex]
            P$pairing.pool[sex, 2, population, number_renewed] <- curiosity_level[indices[sex], population]
          }
          P$pairing.pool[2, 3, population, number_renewed] <- k
          stop <- TRUE
        }
        if(stop == TRUE) {break}
      }
      if(number_renewed == P$nropsp) {break}
    }
  }
  return(P)
}

build_suitors <- function(P, selector, sylreps, context) {
  similarity_golf.score <- rep(0, (P$num_one.pop_singers_sampled[context] * P$num_pop))
  selection_sylreps <- array(0, c((P$num_one.pop_singers_sampled[context] * P$num_pop), P$sylnum))
  selection.index <- rep(0, P$num_pop * P$num_one.pop_singers_sampled[1])
  
  selection.index[(1 + ((singerpop - 1) * P$num_one.pop_singers_sampled[context])) : (singerpop * P$num_one.pop_singers_sampled[context])] <- sample(P$pop_calls_matrix[1, ], P$num_one.pop_singers_sampled[context], replace = FALSE)
  
}


#input: a number, 1, 2, or 3...
#output: 10 sampled numbers per positive integer value of input
# sample_source = P
# sample_retainer <- rep(0, input_value * sample_value)
sampled_suitors <- function(sample_source, sample_retainer, input_value, sample_value) {
  return(sample_retainer <- lapply(1:input_value, function(x) {sample(x = sample_source, size = sample_value, replace = FALSE)}))
}

#selection.index <- unlist(sampled_suitors(sample_source = P$pop_calls_matrix[1,], sample_retainer = selection.index, input_value = P$num_pop, sample_value = P$num_one.pop_singers_sampled))
selection.index <- sampled_suitors(sample_source = P$pop_calls_matrix[1,], sample_retainer = selection.index, input_value = P$num_pop, sample_value = P$num_one.pop_singers_sampled)

score_golf <- function(a_vector, b_vector) {
  return(sum(abs(which((a_vector - b_vector) != 0) - median(which(b_vector == 1)))))
}

golf_score <- score_golf(sylreps[selector.index,,1],sylreps[sample(P$pop_calls_matrix[1,],1),,1])

#input: a list of vectors: row calls, organized such that each element in the list corresponds to a different subset of the array from which it pulls values.
#output: a list of matrices with rows pulled using each row call in the input list- these matrices also separated by element in the list.
suitor_sylreps <- function(P, selector.index, selection.index, sylreps, selector_population) {
  return(sylrep_grp <- mapply(score_golf, sylreps[selection.index[[1:length(selection.index)]]], sylreps[selector.index,,selector_population]))
}

thing <- suitor_sylreps(P = P, selector.index = selector.index, selection.index = selection.index, sylreps = sylreps, selector_population = 1)

suitor_similarity_score <- function(P, sylreps, selector.index, sample_retainer, sample_value, selector_population) {
  return(golf.score <- lapply(selection.index, function(x) {
    sapply(1:sample_value, function(y) {
      sum(abs(which((sylreps[sample_retainer[[x]][y], , x] - sylreps[selector.index, ,selector_population]) != 0) - median(which(sylreps[selector.index, ,selector_population] == 1))))
    })
  })
)}

similarity_golf_score <- suitor_similarity_score(P = P, sylreps = sylreps, selector.index = selector.index, sample_retainer = selection.index, sample_value = P$num_one.pop_singers_sampled[1], selector_population = 1)

# curinh.row - calling either the row number or name of row for different curiosity inheritance patterns - 1: father; 2: mother; 3: same; 4:opposite

curiosity_learn <- function(P, curlearnprob = 0.95, timestep = single_timestep, curinh.row = 1){
  curinh_patterns <- array(data = c(1, 2, 1, 2, 1, 2, 2, 1), dim = c(4,2), dimnames = list(c("father", "mother", "same", "opposite"), c("male birb", "female birb")))
  newcuriosity <- array(data = runif((P$nropsp * P$num_pop * 2), 0 - P$curflux, 0 + P$curflux), dim = c(2, P$num_pop, P$nropsp))
  for(number_renewed in 1 : P$nropsp) {
    for(population in 1 : (P$num_pop)) {
      if(P$pairing.pool[1, 2, population, number_renewed] == 0) {stop("probably not the best time to be learning curiosity from your parents right now...")}
      for(sex in 1:2) {
        new.curiosity <- P$pairing.pool[curinh_patterns[curinh.row,sex], 2, population, number_renewed] + 
                         ((1 - curlearnprob) * (newcuriosity[sex, population, number_renewed])) # Adding small proportion of noise 
        curinh_attempts <- 1
        while((new.curiosity <= 0 | new.curiosity >= 1) && (curinh_attempts < P$new.cur.threshold)) {
          crap <- runif(1, 0 - P$curflux, 0 + P$curflux)
          new.curiosity <- ((1 - curlearnprob) * crap) + 
                           P$pairing.pool[1, 1, population, number_renewed]
          curinh_attempts <- curinh_attempts + 1
        }
        
        if(new.curiosity <= 0) {
          new.curiosity <- 0
        } else if(new.curiosity >= 1) {
          new.curiosity <- 1
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

