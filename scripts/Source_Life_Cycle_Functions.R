# Load the C++ functions
#SourceCpp(file.path('cpp_source','should_pick_neighbor.cpp'))



syll_learn <- function (parameters_sylllearn, temp_data_sylllearn, select_type = 2,
                       totally_new = FALSE, randlearn_context = 1,
                       verbose = FALSE) {

  # select_type decides whether the learning is vertical (2) or oblique (1)

  for (population in 1 : parameters_sylllearn$num_pop) {
    # Make the reference objects for the teacher - the indices for the
    # syllables unique to the teacher's repertoire, and a set of
    # probabilities for each syllable to be learned

    #Vertical Learning;  parameters_sylllearn (set up source_of_ones), and considerations

    if (select_type == 2) {

      #parameters_sylllearn and considerations for VERTICAL LEARNING
      source_of_ones <- which (
        temp_data_sylllearn [1, 1 : parameters_sylllearn$sylnum, population] == 1)
        # calls for sylls vertical tutor (father) has

      if (length (source_of_ones) == 0) {
        saveRDS (parameters_sylllearn,"parent with no sylls.RData")
        print (temp_data_sylllearn [1, 1 : parameters_sylllearn$sylnum, population])
        stop ("wot? parent has no syllables?!")
      } #address syll loss by stopping script if parent has no sylls

      for (sex in 1 : 2) {
        temp_data_sylllearn [(sex + 2), 1 : parameters_sylllearn$sylnum, population] <- 0
      } # clear the sylreps rows about to be filled in :D


    } else { #Oblique Learning; source_of_ones setup, and considerations


      # double-check that the tutor isn't out
      # of sylls before comparing repertoire to pupil.
      source_of_ones <- which (
        temp_data_sylllearn [5, 1 : parameters_sylllearn$sylnum, population] == 1)
      pupil_has_ONEs <- which (
        temp_data_sylllearn [3, 1 : parameters_sylllearn$sylnum, population] == 1)

      if (length (source_of_ones) == 0) {
        stop ("wot? tutor has no syllables?!")
      }

      # as often happens with super self-philes, the tutor may not have
      # anything new to give the pupil. If this is the case, this skips
      # to the next step in the for loop.
      #source_of_ones <- which(moran$learning.pool[5, , population] == 1)[
        #which(!(which(moran$learning.pool[5, , population] == 1) %in% which(
          #moran$learning.pool[3, , population] == 1)))]
      source_of_ones <- setdiff (source_of_ones, pupil_has_ONEs)
      if (length (source_of_ones) == 0) {
        if (verbose == TRUE) {
          print (paste0 ("tutor has no syllables for population ", population))
        }
        next
      } # if curiosity is so low that tutor can teach nothing, just skip
              # this population's tutor learning step
    } # Oblique Learning parameters_sylllearn and considerations
    #if(randlearn_context == 1) {
    #  teacher.mean <- mean(source_of_ones)
    #}


    #sink(file = paste("syll_learn pop", population, "probs.txt", sep = " "),
    # append = T)
    #print(probs)
    #sink()

    for (sex in 1 : select_type) {
      average_rate_randlearn_overlap <- c ()
      #print(source_of_ones)
      probs <- runif (source_of_ones, 0, 1)
      for (sylls_to_learn in 1 : length (source_of_ones)) {
        # moran$learning.pool[(sex + 2),
        # source_of_ones[sylls_to_learn], population] <- 0
        if (probs [sylls_to_learn] <= (parameters_sylllearn$learnprob [select_type])) {
          temp_data_sylllearn [(sex + 2), source_of_ones [sylls_to_learn], population] <- 1
        }
        if (probs [sylls_to_learn] > (
          1 - parameters_sylllearn$randlearnprob [select_type])) {
          r_norm <- rnorm (1, mean = ifelse (randlearn_context == 1,
                                           mean (source_of_ones),
                                           source_of_ones [sylls_to_learn]),
                          sd = parameters_sylllearn$stand.dev)
          if (r_norm > parameters_sylllearn$sylnum) {
            r_norm <- parameters_sylllearn$sylnum
          } else if (r_norm < 1) {
            r_norm <- 1
          }
          #totally_new refers to the idea that if a pupil is learning a sound
          if (totally_new == TRUE) {
            counter <- 1
            r_norm_pool <- rnorm (100, mean = ifelse (randlearn_context == 1,
                                           mean (source_of_ones),
                                           source_of_ones [sylls_to_learn]),
                                 sd = parameters_sylllearn$stand.dev)

            while (temp_data_sylllearn [(sex + 2), floor (r_norm), population] == 1) {

              r_norm <- r_norm_pool [counter]
              if (r_norm > parameters_sylllearn$sylnum) {
                r_norm <- parameters_sylllearn$sylnum
              } else if (r_norm < 1) {
                r_norm <- 1
              }
              counter = counter + 1
            }
            temp_data_sylllearn [(sex + 2), floor (r_norm), population] <- 1
            average_rate_randlearn_overlap <- append (
              average_rate_randlearn_overlap, counter)
          } else {
            temp_data_sylllearn [(sex + 2), floor (r_norm), population] <- 1
          }
        }
      }
      if (totally_new == TRUE) {
        temp_data_sylllearn [
          sex, parameters_sylllearn$sylnum + 5, population
        ] <- mean (average_rate_randlearn_overlap)}
    }
  }
  return (temp_data_sylllearn)
}


make.offspring.calls <- function (parameters_offspring_calls, temp_data_offspring_calls) {
  for (sex in 1 : 2){
    new_index <- c (sample (parameters_offspring_calls$pop_calls_matrix [sex, ], 2, replace = T))
    temp_data_offspring_calls [(sex + 2), parameters_offspring_calls$sylnum + 1, ] <-  new_index
  }
  return (temp_data_offspring_calls)
}


update_selexn_data <- function (
  main_parameters, temp_data_update_selexndata, suitor_choices, preferred_bird,
  selector_bird, curiosity_value, selector_population,
  selection_context, sylreps_choices, sylrep_selector,
  selection_count) {#}), giving_up = FALSE) {

  selected_pair <- c (suitor_choices [preferred_bird], # Bird being selected
                       selector_bird)          # Bird doing the selecting

  #if(!(giving_up)) {
    singer_population <- ceiling (
    preferred_bird / main_parameters$one_pop_singers [selection_context])

    sylrep_pairs <- rbind (sylreps_choices [preferred_bird,], sylrep_selector)
  #}# else {
  #   singer_population <- selector_population

  #   sylrep_pairs <- rbind(sylreps_choices[preferred_bird,], sylrep_selector)
  # } # This happens if giving_up == TRUE. Not ideal for tutor selection,
    # but I guess that's the point of giving up... also, this should
    # basically NEVER happen for tutor context anyway.

  curiosities <- c (curiosity_value [selected_pair [1],singer_population],
                     curiosity_value [selected_pair [2],selector_population])

  if (selection_context == 1) {
    pool.row <- 5
  } else if (selection_context == 2) {
    pool.row <- 1
  }
  for (bird in 1 : selection_context) {
    pool.row <- pool.row * bird

    temp_data_update_selexndata [
      pool.row, main_parameters$sylnum + 1, selector_population
    ] <- selected_pair [bird]

    temp_data_update_selexndata [
      pool.row, 1 : main_parameters$sylnum, selector_population
    ] <- sylrep_pairs [bird,]

    temp_data_update_selexndata [
      pool.row, main_parameters$sylnum + 2, selector_population
    ] <- curiosities [bird]
  }

  temp_data_update_selexndata [
    (4 - selection_context), main_parameters$sylnum + 3, selector_population
  ] <- selection_count

  return (temp_data_update_selexndata)
}

# should_pick_neighbor <- function (index,total_chances,selection_context,
#                                  current_chance, sortSimlr,
#                                  repBarrier,chosenBird,
#                                  lower = 0,upper = 1) {
#   stuff = FALSE
#   lower_bound <- round (total_chances [selection_context] * lower) # 50
#   lower_bound <- round (num_select_chances [select_type] * 0.25) # 50

#   upper_bound <- round (total_chances [selection_context] * upper) # 75
#   print(paste0("sortSimlr = ", sortSimlr, collapse = ""))
#   print(paste0("chosenBird = ", chosenBird))
#   print(paste0("index = ", index))
#   print(paste0("current_chance = ", current_chance))
#   print(paste0("upper_bound = ", upper_bound))
#   print(paste0("lower_bound = ", lower_bound))

#   # is_desperate <- between(current_chance, lower_bound, upper_bound)

#   # is_neighbor_better <- sortSimlr[chosenBird+index] %in% repBarrier
#   if (
#     sortSimlr[which(sortSimlr == chosenBird) + index] %in% repBarrier
#   ) {
#     stuff <- (dplyr::between (current_chance, lower_bound, upper_bound))
#   }

#   return (stuff)
# }

score_similarity <- function (suitor_vector, selector_vector) {
  # Standard Deviation Scoring:
  # The basic sylrep comparison calculation that finds the differences
  # between the suitor and selector sylreps, then assigns a weighted
  # value based on suitor syllable distance from median of selector's sylrep.
  selector_median <- cpp_med (which (selector_vector == 1))
  vector_diff <- which (suitor_vector - selector_vector != 0)
  AbsVal_diffs <- abs (vector_diff - selector_median)

  return (sum (AbsVal_diffs))
  # Output: value of similarity/dissimilarity between
  # sylrep of suitors and selector.
}

sing.selection <- function (parameters_sing_selection, temp_data_sing_selection,
                           curiosity_level, select_type,
                           sylrep_object,
                           num_select_chances = c (16, 40),
                           sylrep_fill_chances = 10,
                           verbose_output = TRUE,
                           interbreed = FALSE) {

  for (divisible in 1 : 2) {
    if (num_select_chances[divisible] %% 4 != 0) {
    stop ("Error management system needs to split num_select_chances into 4
          equal integer values. Make it mod4=0 to address this MIGHTY NEED."
          )
    }
  }
  #print("sing.selection beginning")
  for (population in 1 : parameters_sing_selection$num_pop) { #population <- 1 rm(population)
    #print(paste("this is population",population,sep=" "))
    chance_for_selection = 1
    while (chance_for_selection <= num_select_chances [select_type]) {
      stop = FALSE
      if (chance_for_selection == num_select_chances [select_type]) {
        auto.teachers <- matrix (c (sample (parameters_sing_selection$pop_calls_matrix [1, ],
          sylrep_fill_chances),sample (parameters_sing_selection$pop_calls_matrix [2, ],
          sylrep_fill_chances)),2,sylrep_fill_chances,T)
        for (MTsylrep_filter in 1:sylrep_fill_chances) {
          #c((sample(parameters_sing_selection$pop_calls_matrix[1, ], 1)), (
            #sample(parameters_sing_selection$pop_calls_matrix[2, ], 1)))
          if ((
            sum (sylrep_object [auto.teachers [1,MTsylrep_filter
                ], , population]) != 0) && (
            sum (sylrep_object [auto.teachers [2,MTsylrep_filter
                ], , population]) != 0)) {
            if (verbose_output == TRUE) {
              context.name <- c ("Tutor", "Mate")
              warning (print (paste0 ("Automatic Teacher(s) = ",
                             auto.teachers [,MTsylrep_filter],
                              " for Population ", population,
                              " ", context.name [select_type],
                              " Selection")))
            }

            temp_data_sing_selection = update_selexn_data (
              parameters_sing_selection, temp_data_sing_selection, auto.teachers [1,], MTsylrep_filter,
              auto.teachers [2,MTsylrep_filter], curiosity_level, population,
              select_type, sylrep_object [auto.teachers [1,],,population],
              sylrep_object [auto.teachers [2,MTsylrep_filter],,population],
              num_select_chances [select_type])#, T)

            # if(MTsylrep_filter >= 1) {}
            stop = TRUE
            break
          }
        }
        if (stop) {break
        }
      }

      if (select_type == 1) {
        #This statement separates specific mating and tutoring selection
        # qualities: singsuccessfilter will inform the selection of a
        # mate by restricting the successful mate to those individuals
        # from the same population as the selector. Similarly, selector.index
        # distinguishes between mating and tutoring, except here it uses
        # a randomly-selected female for the mating context, and the
        # offspring for tutoring.

        # "1-20"
        singsuccessfilter <- 1 : (
          (parameters_sing_selection$one_pop_singers [select_type]) * (parameters_sing_selection$num_pop))
        # male offspring from this timestep, lookin for a tutor
        selector.index <- temp_data_sing_selection [3, parameters_sing_selection$sylnum + 1, population]

      } else {
        singsuccessfilter <- (1 + ((population - 1) * (
          parameters_sing_selection$one_pop_singers [select_type]))) : (
            population * parameters_sing_selection$one_pop_singers [select_type])
            # "1-10," or "11-20"
        selector.index <- sample (parameters_sing_selection$pop_calls_matrix [2, ], 1)
            # randomly sample a female from the population
      }

      selector.sylrep <- sylrep_object [selector.index, , population]
      if (sum(selector.sylrep) == 0) {
        stop ("selector didn't have any syllables in the sylrep")
      }
      #print("sapply")
      selection.index <- (
        # This creates sample calls for each population;
        # each population has a sample size of parameters_sing_selection$one_pop_singers,
        # which comes from the male half of the population. probability
        # defined by the fraction of syllable repertoires of each member of
        # each population divided by the maximum syllrep of the population.
        vapply(1:parameters_sing_selection$num_pop,
               function(x) {
                 temp <- cpp_rowSums (sylrep_object[
                   parameters_sing_selection$pop_calls_matrix [1,],,x])
                 sample (x = parameters_sing_selection$pop_calls_matrix [1,],
                        size = parameters_sing_selection$one_pop_singers [select_type],
                        replace = FALSE,
                        prob = temp / max (temp))
                },
               rep (0, parameters_sing_selection$one_pop_singers [select_type])
              )
        ) # probability = the number of times each individual's syllable
        # repertoire has a 1 in it (sum(sylrep_object[
        # parameters_sing_selection$pop_calls_matrix[1,]])),
        # divided by the biggest repertoire's total.

      # create a matrix of all the sylrep_object of the sample
      # males from selection.index
      selection.sylreps <- t (
        cbind (
          vapply (
            1:parameters_sing_selection$one_pop_singers [select_type],
            function (x) {sylrep_object [selection.index[x,1],,1]},
            rep (0, dim (sylrep_object) [2])
          ),
          vapply (
            1:parameters_sing_selection$one_pop_singers [select_type],
            function (x) {sylrep_object [selection.index [x,2],,2]},
            rep (0, dim (sylrep_object) [2])
          )
        )
      )

      # applies the standard deviation scoring to the males in
      # selection.sylrep_object; larger score means greater
      # difference between male sylrep and selector's sylrep.
      # temp <- apply(X = selection.sylreps, MARGIN = 1,
      #               FUN = score_similarity,
      #               selector_vector = selector.sylrep)
      # golf_score <- sort(apply(X = selection.sylreps, MARGIN = 1,
      #               FUN = score_similarity,
      #               selector_vector = selector.sylrep))$ix

      # Golf Score: orders selection of males according to
      # the value of their selection.sylreps[row] measured
      # against the selector.sylrep vector, according to score_similarity,
      # but spits out a vector of their indices within selection.sylreps and selection.index
      golf_score <- cpp_sort_indices (apply (X = selection.sylreps, MARGIN = 1,
                          FUN = score_similarity,
                          selector_vector = selector.sylrep))
      # orders the scored list of suitors; subsets one suitor from the rest,
      # according to the value of the selector's (auditory) curiosity.
      singer <- golf_score [round (curiosity_level [
        selector.index, population] *(parameters_sing_selection$one_pop_singers [
        select_type] * parameters_sing_selection$num_pop) + 0.5)]
      if (sum (selection.sylreps [singer,])==0) {
        chance_for_selection = chance_for_selection + 1
        next
      }

      #should_pick_neighbor <- function(index,lower,upper=Inf) {

      if (!interbreed) {
        should_continue <- TRUE
        if (singer %in% singsuccessfilter) {

          temp_data_sing_selection <- update_selexn_data (
            main_parameters = parameters_sing_selection,
            temp_data_update_selexndata = temp_data_sing_selection,
            suitor_choices = selection.index,
            preferred_bird = singer,
            selector_bird = selector.index,
            curiosity_value = curiosity_level,
            selector_population = population,
            selection_context = select_type,
            sylreps_choices = selection.sylreps,
            sylrep_selector = selector.sylrep,
            selection_count = chance_for_selection
          )#, F)

          should_continue <- FALSE
        }


        # should_pick_neighbor(1, num_select_chances, select_type,
        #                             chance_for_selection, golf_score,
        #                             singsuccessfilter, singer, lower=0.5,
        #                             upper=0.75)

        if (should_continue == TRUE) {
          if (between(chance_for_selection, num_select_chances[select_type] * 0.25, num_select_chances[select_type] * 0.5)) {
            for (neighbor in c (1, -1, 2, -2)) {

              # sink(file = "InvasionAnalysisInitTest1.txt", append = T)
              # print("here is a print of the attempted object")
              # print((golf_score[which(golf_score == singer) + neighbor]))
              # print(paste0("or the individuals: singer = ", singer, ", neighbor = ", neighbor))
              # print(golf_score)
              # sink()

              if (!(length(golf_score[which(golf_score == singer) + neighbor]) == 0)) {
                if (golf_score[which(golf_score == singer) + neighbor] %in% singsuccessfilter) {

                  singer <- golf_score [which (golf_score == singer) + neighbor]

                  temp_data_sing_selection <- update_selexn_data (
                    main_parameters = parameters_sing_selection,
                    temp_data_update_selexndata = temp_data_sing_selection,
                    suitor_choices = selection.index,
                    preferred_bird = singer,
                    selector_bird = selector.index,
                    curiosity_value = curiosity_level,
                    selector_population = population,
                    selection_context = select_type,
                    sylreps_choices = selection.sylreps,
                    sylrep_selector = selector.sylrep,
                    selection_count = chance_for_selection
                  )#, F)

                  should_continue <- FALSE

                  break
                }
              }
              if (!(should_continue)) {
                break
              }
            }
          }
        }

        if (should_continue == TRUE) {
          if (between(chance_for_selection, num_select_chances[select_type] * 0.5, num_select_chances[select_type] * 0.75)) {
            for (neighbor in c (1, -1, 2, -2, 3, -3, 4, -4, 5, -5)) {

              # sink(file = "InvasionAnalysisInitTest1.txt", append = T)
              # print("here is a print of the attempted object")
              # print((golf_score[which(golf_score == singer) + neighbor]))
              # print(paste0("or the individuals: singer = ", singer, ", neighbor = ", neighbor))
              # print(golf_score)
              # sink()

              if (!(length(golf_score[which(golf_score == singer) + neighbor]) == 0)) {
                if (golf_score[which(golf_score == singer) + neighbor] %in% singsuccessfilter) {

                  singer <- golf_score [which (golf_score == singer) + neighbor]

                  temp_data_sing_selection <- update_selexn_data (
                    main_parameters = parameters_sing_selection,
                    temp_data_update_selexndata = temp_data_sing_selection,
                    suitor_choices = selection.index,
                    preferred_bird = singer,
                    selector_bird = selector.index,
                    curiosity_value = curiosity_level,
                    selector_population = population,
                    selection_context = select_type,
                    sylreps_choices = selection.sylreps,
                    sylrep_selector = selector.sylrep,
                    selection_count = chance_for_selection
                  )#, F)

                  should_continue <- FALSE

                  break
                }
              }
              if (!(should_continue)) {
                break
              }
            }
          }
        }

        if (should_continue == TRUE) {
          if (between(chance_for_selection, num_select_chances[select_type] * 0.75, num_select_chances[select_type] * 1)) {
            for (neighbor in c (1, -1, 2, -2, 3, -3, 4, -4, 5, -5,
                              6, -6, 7, -7, 8, -8, 9, -9, 10, -10)) {

              # sink(file = "InvasionAnalysisInitTest1.txt", append = T)
              # print("here is a print of the attempted object")
              # print((golf_score[which(golf_score == singer) + neighbor]))
              # print(paste0("or the individuals: singer = ", singer, ", neighbor = ", neighbor))
              # print(golf_score)
              # sink()

              if (!(length(golf_score[which(golf_score == singer) + neighbor]) == 0)) {
                if (golf_score[which(golf_score == singer) + neighbor] %in% singsuccessfilter) {

                  singer <- golf_score [which (golf_score == singer) + neighbor]

                  temp_data_sing_selection <- update_selexn_data (
                    main_parameters = parameters_sing_selection,
                    temp_data_update_selexndata = temp_data_sing_selection,
                    suitor_choices = selection.index,
                    preferred_bird = singer,
                    selector_bird = selector.index,
                    curiosity_value = curiosity_level,
                    selector_population = population,
                    selection_context = select_type,
                    sylreps_choices = selection.sylreps,
                    sylrep_selector = selector.sylrep,
                    selection_count = chance_for_selection
                  )#, F)

                  should_continue <- FALSE

                  break
                }
              }
              if (!(should_continue)) {
                break
              }
            }
          }
        }

        if (!should_continue) {
          break
        }
      } else {
        if (sum (sylrep_object [selection.index [singer], , population]) != 0) {

          temp_data_sing_selection <- update_selexn_data (
            main_parameters = parameters_sing_selection,
            temp_data_update_selexndata = temp_data_sing_selection,
            suitor_choices = selection.index,
            preferred_bird = singer,
            selector_bird = selector.index,
            curiosity_value = curiosity_level,
            selector_population = population,
            selection_context = select_type,
            sylreps_choices = selection.sylreps,
            sylrep_selector = selector.sylrep,
            selection_count = chance_for_selection
          )#, F)

          break
        }
      }
      chance_for_selection = chance_for_selection + 1
    }
  }
  return (temp_data_sing_selection)
}


curiosity_learn <- function (parameters_curiosity_learn,
                             temp_data_curiosity_learn,
                             inheritance_pattern = 1#,
                            #  invasion = FALSE,
                            #  invPopSize = 5
                            ) {

  if (inheritance_pattern == 5) {

    malecurinh <- parameters_curiosity_learn$curinhproportion

    newcuriosity <- c (runif ((parameters_curiosity_learn$num_pop * 2), -1, 1))

    for (population in 1 : (parameters_curiosity_learn$num_pop)) {

      for (sex in 1:2) {
        if (
            temp_data_curiosity_learn [1,
              parameters_curiosity_learn$sylnum + 2,
              population
            ] == 0 ||
            temp_data_curiosity_learn [2,
              parameters_curiosity_learn$sylnum + 2,
              population
            ] == 0
           ) {stop (
          "not the time for learning curiosity from parents right now..."
          )
        }

        curinh_attempts <- 1

        while (((
            (malecurinh) * temp_data_curiosity_learn [1, parameters_curiosity_learn$sylnum + 2, population] +

            (1 - malecurinh) * temp_data_curiosity_learn [2, parameters_curiosity_learn$sylnum + 2, population]
          ) +
          ((1 - parameters_curiosity_learn$curlearnprob) * (newcuriosity [2 * (population - 1) + sex
          ]))) < 0) {

          newcuriosity [2 * (population - 1) + sex] <- runif (1, 0, 1)
          curinh_attempts <- curinh_attempts + 1

        }

        while (((
            (malecurinh) * temp_data_curiosity_learn [1, parameters_curiosity_learn$sylnum + 2, population] +

            (1 - malecurinh) * temp_data_curiosity_learn [2, parameters_curiosity_learn$sylnum + 2, population]
        ) +
        ((1 - parameters_curiosity_learn$curlearnprob) * (newcuriosity [2 * (population - 1) + sex
        ]))) > 1) {

          newcuriosity [2 * (population - 1) + sex] <- runif (1, -1, 0)
          curinh_attempts <- curinh_attempts + 1

        }

        new.curiosity <- (
            (malecurinh) * temp_data_curiosity_learn [1, parameters_curiosity_learn$sylnum + 2, population] +

            (1-malecurinh) * temp_data_curiosity_learn [2, parameters_curiosity_learn$sylnum + 2, population]
          ) +
          ((1 - parameters_curiosity_learn$curlearnprob) * (newcuriosity [2 * (population - 1) + sex
          ])) # Adding small proportion of noise

        temp_data_curiosity_learn [
          (sex + 2), parameters_curiosity_learn$sylnum + 4, population
        ] <- temp_data_curiosity_learn [(sex + 2), parameters_curiosity_learn$sylnum + 2, population]
        temp_data_curiosity_learn [
          (sex + 2), parameters_curiosity_learn$sylnum + 2, population
        ] <- new.curiosity
        temp_data_curiosity_learn [
          (sex + 2), parameters_curiosity_learn$sylnum + 5, population
        ] <- curinh_attempts
      }
    }

  } else {

    curinh_patterns <- array (
      data = c (
        1, 2,
        1, 2,
        1, 2,
        2, 1
      ),
      dim = c (
        4,2
      ),
      dimnames = list (c ("father", "mother", "same", "opposite"),
                      c ("male birb", "female birb")
                    )
    )

    newcuriosity <- c (runif ((parameters_curiosity_learn$num_pop * 2), -1, 1))

    for (population in 1 : (parameters_curiosity_learn$num_pop)) {

      for (sex in 1:2) {
        if (temp_data_curiosity_learn [
          curinh_patterns [inheritance_pattern,sex],
          parameters_curiosity_learn$sylnum + 2,
          population
        ] == 0) {stop (
          "not the time for learning curiosity from parents right now...")}

        curinh_attempts <- 1

        while ((temp_data_curiosity_learn [curinh_patterns[
            inheritance_pattern,sex
          ], parameters_curiosity_learn$sylnum + 2, population] +
          ((1 - parameters_curiosity_learn$curlearnprob) * (newcuriosity [2 * (population - 1) + sex
          ]))) < 0) {

          newcuriosity [2 * (population - 1) + sex] <- runif (1, 0, 1)
          curinh_attempts <- curinh_attempts + 1

        }

        while ((temp_data_curiosity_learn [curinh_patterns[
          inheritance_pattern,sex
        ], parameters_curiosity_learn$sylnum + 2, population] +
        ((1 - parameters_curiosity_learn$curlearnprob) * (newcuriosity [2 * (population - 1) + sex
        ]))) > 1) {

          newcuriosity [2 * (population - 1) + sex] <- runif (1, -1, 0)
          curinh_attempts <- curinh_attempts + 1

        }

        new.curiosity <- temp_data_curiosity_learn [curinh_patterns [
            inheritance_pattern,sex
          ], parameters_curiosity_learn$sylnum + 2, population] +
          ((1 - parameters_curiosity_learn$curlearnprob) * (
            newcuriosity [2 * (population - 1) + sex
          ])) # Adding small proportion of noise

        temp_data_curiosity_learn [
          (sex + 2), parameters_curiosity_learn$sylnum + 4, population
        ] <- temp_data_curiosity_learn [(sex + 2), parameters_curiosity_learn$sylnum + 2, population]
        temp_data_curiosity_learn [
          (sex + 2), parameters_curiosity_learn$sylnum + 2, population
        ] <- new.curiosity
        temp_data_curiosity_learn [
          (sex + 2), parameters_curiosity_learn$sylnum + 5, population
        ] <- curinh_attempts
      }
    }
  }

  # if (invasion) {
  #   invaderIDs <- sample(,invPopSize,)

  # }

  return (temp_data_curiosity_learn)
}

recuriosity.offspring <- function (parameters_recuriosity, temp_data_recuriosity, curiosity_object) {
  for (population in 1 : parameters_recuriosity$num_pop) {
    for (sex in 1 : 2) {

      curiosity_object [
        temp_data_recuriosity [
          (sex + 2), parameters_recuriosity$sylnum + 1, population
        ], population] <- temp_data_recuriosity [
          (sex + 2), parameters_recuriosity$sylnum + 2, population]
    }
  }
  return (curiosity_object)
}

resylreps.offspring <- function (parameters_resylreps, temp_data_resylreps, sylrep_object) {
  for (population in 1 : parameters_resylreps$num_pop) {
    for (sex in 1 : 2) {

      sylrep_object [temp_data_resylreps[
        (sex + 2), parameters_resylreps$sylnum + 1, population
        ], , population] <- temp_data_resylreps [
          (sex + 2), 1 : parameters_resylreps$sylnum, population
        ]
    }
  }
  return (sylrep_object)
}


recordvariable.initialize <- function (parameters_rvi, recsimfct, variableid) {
  if (variableid == 1) {
    record.variable <- array (0, c (
      2, parameters_rvi$num_pop, (1000 / recsimfct)))
  } else if (variableid == 2) {
    record.variable <- array (0, c (
      (2 * parameters_rvi$num_pop), parameters_rvi$sylnum, (1000 / recsimfct)))
  } else if (variableid == 3) {
    record.variable <- array (0, c (
      12, parameters_rvi$num_pop, (1000 / recsimfct)))
  } else if (variableid == 4) {
    record.variable <- array (0, c (
      (2 * parameters_rvi$num_pop), (parameters_rvi$num_pop * parameters_rvi$one_pop_singers [1]),
      (1000 / recsimfct)))
  }
  return (record.variable)
}


sylrep_rowcol.archive <- function (parameters_src_archive,
                                   data_container,
                                   syllable_object,
                                   timestep) {
  for (population in 1 : parameters_src_archive$num_pop) {
    for (sex in 1 : 2) {
      #sylrep_rowcol
      data_container [sex, population, timestep] <- mean (rowSums (
       syllable_object [
        ((
           1 + ((sex - 1) * (parameters_src_archive$pop_size / 2))
         ) : (
           sex * (parameters_src_archive$pop_size / 2)
        )), , population]
      ))
    }
  }
  return (data_container)
}


sylrep_dstbxn.archive <- function (parameters_sdb_archive,
                                   data_container,
                                   syllable_object,
                                   timestep) {

  for (population in 1 : parameters_sdb_archive$num_pop) {
    for (sex in 1 : 2) {
      # sylrep_dstbxn
      data_container [(((population - 1) * 2) + sex), , timestep] <- colSums (
        syllable_object [((
          1 + ((sex - 1) * (parameters_sdb_archive$pop_size / 2))
          ) : (
          sex * (parameters_sdb_archive$pop_size / 2)
          )), , population]
      )
    }
  }
  return (data_container)
}


curity_mean_t.archive <- function (parameters_cmt_archive,
                                   temp_data_cmt_archive,
                                   data_container,
                                   curiosity_object,
                                   timestep) {
  for (population in 1 : parameters_cmt_archive$num_pop) {
    # curity_mean_t
    data_container [3, population, timestep] <- temp_data_cmt_archive [
      2, parameters_cmt_archive$sylnum + 3, population]
    data_container [10, population, timestep] <- temp_data_cmt_archive [
      3, parameters_cmt_archive$sylnum + 3, population]

    for (sex in 1 : 2) {
      data_container [sex, population, timestep] <- mean(
        curiosity_object [((
          1 + ((sex-1) * parameters_cmt_archive$pop_size / 2)
        ) : (
          sex * parameters_cmt_archive$pop_size / 2)), population]
        )

      # Individual Curiosity Values
      data_container [
        (sex + 3), population, timestep
      ] <- temp_data_cmt_archive [sex, parameters_cmt_archive$sylnum + 2, population]

      data_container [
        (sex + 5), population, timestep
      ] <- temp_data_cmt_archive [(sex + 2), parameters_cmt_archive$sylnum + 2, population]

      data_container [
        (sex + 7), population, timestep
      ] <- temp_data_cmt_archive [(sex + 2), parameters_cmt_archive$sylnum + 4, population]

      data_container [
        11, population, timestep
      ] <- temp_data_cmt_archive [(sex + 2), parameters_cmt_archive$sylnum + 5, population] # problems: this value degenerates two into one, by not leaving "sex" variable on left of equation

      data_container [
        12, population, timestep
      ] <- temp_data_cmt_archive [sex, parameters_cmt_archive$sylnum + 5, population] # same as above
    }
  }
  return (data_container)
}


#       data_container[
#         12, population, timestep
#       ] <- temp_data_cmt_archive[sex, parameters$sylnum + 5, population]
#     }
#   }
#   return(data_container)
# }

curity_repert.archive <- function (parameters_crp_archive,
                                   data_container,
                                   curiosity_object,
                                   timestep) {

  for (population in 1 : parameters_crp_archive$num_pop) {
    for (sex in 1 : 2) {
      # curity_repert
      data_container [
        (sex + ((population - 1) * 2)), , timestep
      ] <- hist (curiosity_object [((
        1 + ((sex-1) * parameters_crp_archive$pop_size / 2)
      ) : (
        sex * parameters_crp_archive$pop_size / 2
      )), population], breaks =
      parameters_crp_archive$curiositybreaks, plot = FALSE)$counts
    }
  }
  return (data_container)
}


store_timesteps <- function (parameters_storetimesteps, filename = thousand_timesteps,
  rowcol, dstbxn, mean_t, repert, saved_stuff, syll_container,
  cur_container, run_timedate, foldername = foldername#,
  # simnumber = simnumber
  ) {
   # # # #  #directory <- getwd()
  results_directory <- file.path ('results')
  if (filename == 1) {
    # run_timedate <- format(Sys.time(), "%F-%H%M%S")
    if (! (dir.exists (file.path (results_directory, saved_stuff$docnamez)))) {
      dir.create (file.path (results_directory, saved_stuff$docnamez))
      dir.create (file.path (results_directory, saved_stuff$docnamez,
        "variable_store"))
    }
    dir.create (file.path (results_directory, saved_stuff$docnamez,
      "variable_store", paste0 (run_timedate, "-GMT-variable-store")))
    # foldername <- file.path(results_directory, saved_stuff$docnamez,
    #   "variable_store", paste0(run_timedate, "-GMT-variable-store"))
    saveRDS (saved_stuff, file.path (foldername, "metadata.RData"))
  }
  # else {
  #   foldername <- readRDS (file.path(
  #       "source", "temp", paste0(
  #         "foldername_", parameters$simnumber, ".RData")
  # ))}

  thing <- c ("sylrep_rowcol", "sylrep_dstbxn",
              "curity_mean_t", "curity_repert")

  for (data_categories in 1:4) {

    file.create (file.path (foldername, paste0 (
      "variable-store-", filename, "-", thing[data_categories], ".RData")))
    if (data_categories == 1) {
      objekshun <- rowcol
    } else if (data_categories == 2) {
      objekshun <- dstbxn
    } else if (data_categories == 3) {
      objekshun <- mean_t
    } else if (data_categories == 4) {
      objekshun <- repert
    }
    # print("tryna save")
    saveRDS (objekshun, file.path (foldername, paste0 (
      "variable-store-", filename, "-", thing[data_categories], ".RData")))
    # print("saved")
  }
  saveRDS (parameters_storetimesteps, file.path (
    foldername, "defined_parameters.RData"))
  saveRDS (syll_container, file.path (
    foldername, "end_sylbls.RData"))
  saveRDS (cur_container, file.path (
    foldername, "end_cursty.RData"))

  return (foldername)
}
