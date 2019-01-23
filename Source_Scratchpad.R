sing.selection <- function(P, curiosity_level, tutor1_or_mate2, num_select_chances = c(10, 42), ohsit = 10, verbose_output = TRUE, interbreed = FALSE){
  
  for(population in 1 : P$num_pop) { #population <- 1 rm(population)
    #print(paste("this is population",population,sep=" "))
    chance_for_selection = 1
    while(chance_for_selection <= num_select_chances[tutor1_or_mate2]) {
      stop = FALSE
      if(chance_for_selection == num_select_chances[tutor1_or_mate2]) {
        for(hope_not_necessary in 1:ohsit){
          auto.teachers <- c((sample(P$pop_calls_matrix[1, ], 1)), (sample(P$pop_calls_matrix[2, ], 1)))
          if((sum(sylreps[auto.teachers[1], , population]) != 0) && (sum(sylreps[auto.teachers[2], , population]) != 0)) {
            if(verbose_output == TRUE) {
              context.name <- c("Tutor", "Mate")
              warning(print(paste0("Automatic Teacher(s) = ", auto.teachers, " for Population ", population, " ", context.name[tutor1_or_mate2], " Selection")))
            }
            P = update_selexn_data(P, auto.teachers, 1, auto.teachers[2], 
                curiosity_level, population, tutor1_or_mate2,
                sylreps[auto.teachers[1]:200,,population], sylreps[auto.teachers[2],,population], chance_for_selection)

            # should probably fill in some spots in P$pairing.pool with hope_not_necessary, provided the value exceeds 1.
            if(hope_not_necessary < 1) {
              
            }
            stop = TRUE
            break
          } else {hope_not_necessary = hope_not_necessary + 1}
        }
        if(stop) {break}
      }
        
      if(tutor1_or_mate2 == 1) {
        #This statement separates specific mating and tutoring selection qualities:
        # singSuccessFilter will inform the selection of a mate by restricting the successful mate 
        # to those individuals from the same population as the selector. Similarly, 
        # selector.index distinguishes between mating and tutoring, except here it uses
        # a randomly-selected female for the mating context, and the offspring for tutoring.

        singSuccessFilter <- 1 : ((P$num_one.pop_singers_sampled[tutor1_or_mate2]) * (P$num_pop)) # "1-20"
        selector.index <- P$pairing.pool[3, 1, population]
      } else {
        singSuccessFilter <- (1 + ((population - 1) * (P$num_one.pop_singers_sampled[tutor1_or_mate2]))) : (population * P$num_one.pop_singers_sampled[tutor1_or_mate2]) # "1-10," or "11-20"
        selector.index <- sample(P$pop_calls_matrix[2, ], 1)
      }
      
      selector.sylrep <- sylreps[selector.index, , population]
            
      selection.index <- (
        # This creates sample calls for each population; each population has a sample size of
        # P$num_one.pop_singers_sampled, which comes from the male half of the population.
        # probability defined by the fraction of syllable repertoires of each member of 
        # each population divided by the maximum syllrep of the population.
        sapply(
          1:P$num_pop, function(x) {
            sample(
              x = P$pop_calls_matrix[1,], 
              size = P$num_one.pop_singers_sampled[tutor1_or_mate2], 
              replace = FALSE, prob = ((
                  apply(sylreps[P$pop_calls_matrix[1,],,x],1,sum)
                )/max(
                  apply(sylreps[P$pop_calls_matrix[1,],,x],1,sum)
                )
              )
            )
          }
        ) # probability = the number of times each individual's syllable 
          # repertoire has a 1 in it (sum(sylreps[P$pop_calls_matrix[1,]])), 
          # divided by the biggest repertoire's total.
      )
      
      # create a matrix of all the sylreps of the sample males from selection.index
      selection.sylreps <- t(
        cbind(
          sapply(
            1:P$num_one.pop_singers_sampled[tutor1_or_mate2], 
            function(x) {sylreps[selection.index[x,1],,1]}
          ),
          sapply(
            1:P$num_one.pop_singers_sampled[tutor1_or_mate2], 
            function(x) {sylreps[selection.index[x,2],,2]}
           )
         )
      )
      
      # applies the standard deviation scoring to the males in selection.sylreps; 
      # larger score means greater difference between male sylrep and selector's sylrep.
      golf_score <- sort(apply(X = selection.sylreps, MARGIN = 1, FUN = score_similarity, selector_vector = selector.sylrep),index.return = T)$ix
      
      # orders the scored list of suitors; subsets one suitor from the rest,
      # according to the value of the selector's (auditory) curiosity.
      singer <- golf_score[round(curiosity_level[selector.index, population] * (P$num_one.pop_singers_sampled[tutor1_or_mate2] * P$num_pop) + 0.5)]
      if(sum(selection.sylreps[singer,])==0) {
        chance_for_selection = chance_for_selection + 1
        next}
      
      #should_pick_neighbor <- function(index,lower,upper=Inf) {
      
      if(!interbreed) {
        should_continue <- TRUE
        if(singer %in% singSuccessFilter) {
          
          P = update_selexn_data(P, selection.index, singer, selector.index, curiosity_level, 
                             population, tutor1_or_mate2, selection.sylreps, selector.sylrep, 
                             chance_for_selection)
          
          should_continue <- FALSE
        }
        
        if(should_continue) {
          for(neighbor in c(1, -1)) {
            if(should_pick_neighbor(neighbor,num_select_chances,tutor1_or_mate2,chance_for_selection,golf_score,singSuccessFilter,singer,lower=0.5,upper=0.75)) {
              singer <- golf_score[singer+neighbor]
              
              P = update_selexn_data(P, selection.index, singer, selector.index, curiosity_level, 
                             population, tutor1_or_mate2, selection.sylreps, selector.sylrep, 
                             chance_for_selection)
              
              should_continue <- FALSE
              break
            }
          }
        }
          
        if(should_continue) {
          for(neighbor in c(1, -1, 2, -2)) {
            if(should_pick_neighbor(neighbor,num_select_chances,tutor1_or_mate2,chance_for_selection,golf_score,singSuccessFilter,singer,lower=0.75)) {
              singer <- golf_score[singer+neighbor]

              P = update_selexn_data(P, selection.index, singer, selector.index, curiosity_level, 
                             population, tutor1_or_mate2, selection.sylreps, selector.sylrep, 
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
        if(sum(sylreps[selection.index[singer], , population]) != 0) {

          P = update_selexn_data(P, selection.index, singer, selector.index, curiosity_level, 
                             population, tutor1_or_mate2, selection.sylreps, selector.sylrep, 
                             chance_for_selection)
          
          break
        }
      }
      chance_for_selection = chance_for_selection + 1
    }
  }
  return(P)
}