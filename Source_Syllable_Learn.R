syll_learn <- function(params, moran, select_type = 2, totally_new = FALSE, randlearn_context = 1, verbose = FALSE){ # context decides whether the learning is vertical (2) or oblique (1)
  randlearncontext_container <- c("mean(source_of_ONEs)", "source_of_ONEs[sylls_to_learn]")
  for(population in 1 : params$num_pop) {
    # Make the reference objects for the teacher - the indices for the syllables unique to the teacher's repertoire, and a set of probabilities for each syllable to be learned
    
    #Vertical Learning;  params (set up source_of_ONEs), and considerations
    if(select_type == 2) { #params and considerations for VERTICAL LEARNING
      
      source_of_ONEs <- which(moran$learning.pool[1, , population] == 1) # calls for sylls vertical tutor (father) has
      if(length(source_of_ONEs) == 0) {
        saveRDS(object = params, file = "parent with no sylls.txt")
        print(moran$learning.pool[1, , population])
        stop("wot? parent has no syllables?!")
      } #address syll loss by stopping script if parent has no sylls
      for(sex in 1 : 2) {
        moran$learning.pool[(sex + 2), , population] <- rep(0, params$sylnum)
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
        if(probs[sylls_to_learn] <= (params$learnprob[select_type])) {
          moran$learning.pool[(sex + 2), source_of_ONEs[sylls_to_learn], population] <- 1
        }
        if(probs[sylls_to_learn] > (1 - params$randlearnprob[select_type])) {
          r_norm <- rnorm(1, mean = eval(parse(text=randlearncontext_container[randlearn_context])), sd = params$stand.dev)
          if(r_norm > params$sylnum) {
            r_norm <- params$sylnum
          } else if(r_norm < 1) {
            r_norm <- 1
          }
          #totally_new refers to the idea that if a pupil is learning a sound
          if(totally_new == TRUE) {
            counter <- 1
            r_norm_pool <- rnorm(100, mean = eval(parse(text=randlearncontext_container[randlearn_context])), sd = params$stand.dev)
            while(moran$learning.pool[(sex + 2), floor(r_norm), population] == 1) {
              r_norm <- r_norm_pool[counter]
              if(r_norm > params$sylnum) {
                r_norm <- params$sylnum
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
  return(moran)
}