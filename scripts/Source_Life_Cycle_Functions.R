# Load the C++ functions
#SourceCpp (file.path ('cpp_source','should_pick_neighbor.cpp'))



syll_learn <- function (p_SL, 
                        temp_data_SL, 
                        select_type = "mate",
                        totally_new = FALSE, 
                        randlearn_context = 1,
                        verbose = FALSE,
                        innate_bias_SL = FALSE) {

  # select_type decides whether the learning is vertical (2) or oblique (1)
  
  standard_deviation <- as.numeric (p_SL$standard_deviation)

  for (population in 1 : p_SL$num_pop) {
    # Make the reference objects for the teacher - the indices for the
    # syllables unique to the teacher's repertoire, and a set of
    # probabilities for each syllable to be learned

    #Vertical Learning;  p_SL (set up source_of_ones), and considerations

    if (select_type == "mate") {

      #p_SL and considerations for VERTICAL LEARNING
      source_of_ones <- which (
        temp_data_SL [1, 1 : p_SL$sylnum, population] == 1)
        # calls for sylls vertical tutor (father) has

      if (length (source_of_ones) == 0) {
        saveRDS (p_SL,"parent with no sylls.RData")
        print (temp_data_SL [1, 1 : p_SL$sylnum, population])
        stop ("wot? parent has no syllables?!")
      } #address syll loss by stopping script if parent has no sylls

      for (sex in 1 : 2) {
        temp_data_SL [(sex + 2), 1 : p_SL$sylnum, population] <- 0
      } # clear the sylreps rows about to be filled in :D


    } else if (select_type == "tutor") { #Oblique Learning; source_of_ones setup, and considerations


      # double-check that the tutor isn't out
      # of sylls before comparing repertoire to pupil.
      source_of_ones <- which (
        temp_data_SL [5, 1 : p_SL$sylnum, population] == 1)
      pupil_has_ONEs <- which (
        temp_data_SL [3, 1 : p_SL$sylnum, population] == 1)

      if (length (source_of_ones) == 0) {
        stop ("wot? tutor has no syllables?!")
      }

      # as often happens with super self-philes, the tutor may not have
      # anything new to give the pupil. If this is the case, this skips
      # to the next step in the for loop.
      #source_of_ones <- which (moran$learning.pool[5, , population] == 1) [
        #which (! (which (moran$learning.pool[5, , population] == 1) %in% which (
          #moran$learning.pool[3, , population] == 1)))]
      source_of_ones <- setdiff (source_of_ones, pupil_has_ONEs)
      if (length (source_of_ones) == 0) {
        if (verbose == TRUE) {
          print (paste0 ("tutor has no syllables for population ", population))
        }
        next
      } # if curiosity is so low that tutor can teach nothing, just skip
              # this population's tutor learning step
    } # Oblique Learning p_SL and considerations
    #if (randlearn_context == 1) {
    #  teacher.mean <- mean (source_of_ones)
    #}


    #sink (file = paste("syll_learn pop", population, "probs.txt", sep = " "),
    # append = TRUE)
    #print (probs)
    #sink ()
    vertoblearn_subset <- as.numeric(c(p_SL$vertoblearn[[1]][2], p_SL$vertoblearn[[1]][1]))
    vertobrand_subset <- as.numeric(c(p_SL$vertoblearn[[2]][2], p_SL$vertoblearn[[2]][1]))

    if (select_type == "mate") {
      loop_size <- 2
      # loop_size <- 1

    } else if (select_type == "tutor") {
      loop_size <- 1
      # loop_size <- 2

      #vertoblearn_subset <- as.numeric(c(p_SL$vertoblearn[[2]][2], p_SL$vertoblearn[[2]][1]))
    }

    for (sex in 1 : loop_size) {
      average_rate_randlearn_overlap <- c ()
      #print (source_of_ones)
      probs <- runif (source_of_ones, 0, 1)
      for (sylls_to_learn in 1 : length (source_of_ones)) {
        # moran$learning.pool[(sex + 2),
        # source_of_ones[sylls_to_learn], population] <- 0
        # if (probs [sylls_to_learn] <= (p_SL$learnprob [loop_size])) {
        if (probs [sylls_to_learn] <= vertoblearn_subset[loop_size]) {
          temp_data_SL [(sex + 2), source_of_ones [sylls_to_learn], population] <- 1
        }
        if (probs [sylls_to_learn] > (
          # 1 - p_SL$randlearnprob [loop_size])) {
          1 - vertobrand_subset[loop_size])) {
          r_norm <- rnorm (1, mean = ifelse (randlearn_context == 1,
                                           mean (source_of_ones),
                                           source_of_ones [sylls_to_learn]),
                          sd = standard_deviation)
          if (r_norm > p_SL$sylnum) {
            r_norm <- p_SL$sylnum
          } else if (r_norm < 1) {
            r_norm <- 1
          }
          #totally_new refers to the idea that if a pupil is learning a sound
          if (totally_new == TRUE) {
            counter <- 1
            r_norm_pool <- rnorm (100, mean = ifelse (randlearn_context == 1,
                                           mean (source_of_ones),
                                           source_of_ones [sylls_to_learn]),
                                 sd = standard_deviation)

            while (temp_data_SL [(sex + 2), floor (r_norm), population] == 1) {

              r_norm <- r_norm_pool [counter]
              if (r_norm > p_SL$sylnum) {
                r_norm <- p_SL$sylnum
              } else if (r_norm < 1) {
                r_norm <- 1
              }
              counter = counter + 1
            }
            temp_data_SL [(sex + 2), floor (r_norm), population] <- 1
            average_rate_randlearn_overlap <- append (
              average_rate_randlearn_overlap, counter)
          } else {
            temp_data_SL [(sex + 2), floor (r_norm), population] <- 1
          }
        }
      }
      if (totally_new == TRUE) {
        temp_data_SL [
          sex, p_SL$sylnum + 5, population
        ] <- mean (average_rate_randlearn_overlap)}
    }
  }
  return (temp_data_SL)
}


make.offspring.calls <- function (p_OC, temp_data_OC, ro_OC) {
  for (sex in 1 : 2){
    new_index <- c (sample (ro_OC [sex, ], 2, replace = TRUE))
    temp_data_OC [(sex + 2), p_OC$sylnum + 1, ] <-  new_index
  }
  return (temp_data_OC)
}


update_selexn_data <- function (
  p_US, temp_data_US, suitor_choices, preferred_bird,
  selector_bird, curiosity_value, selector_population,
  selection_context, sylreps_choices, sylrep_selector,
  selection_count, selection_type = 1) {#}), giving_up = FALSE) {

  selected_pair <- c (suitor_choices [preferred_bird], # Bird being selected
                       selector_bird)          # Bird doing the selecting

  #if (! (giving_up)) {
    singer_population <- ceiling (
    preferred_bird / p_US$one_pop_singers [selection_context])
    # if (selection_type == 1) {
      sylrep_pairs <- rbind (sylreps_choices [preferred_bird,], sylrep_selector)
    # } else if (selection_type == 2) {
    #   sylrep_pairs <- rbind (sylreps_choices [preferred_bird], sylrep_selector)
    # }

  #}# else {
  #   singer_population <- selector_population

  #   sylrep_pairs <- rbind (sylreps_choices[preferred_bird,], sylrep_selector)
  # } # This happens if giving_up == TRUE. Not ideal for tutor selection,
    # but I guess that's the point of giving up... also, this should
    # basically NEVER happen for tutor context anyway.

  curiosities <- c (curiosity_value [selected_pair [1],singer_population],
                     curiosity_value [selected_pair [2],selector_population])

  if (selection_context == 1) { # tutor select_type
    pool.row <- 5
  } else if (selection_context == 2) { # mate select_type
    pool.row <- 1
  }
  for (bird in 1 : selection_context) {
    pool.row <- pool.row * bird # mating: 1, then 2; tutor: 5, never 10.

    temp_data_US [
      pool.row, p_US$sylnum + 1, selector_population
    ] <- selected_pair [bird]

    tryCatch({temp_data_US [
      pool.row, 1 : p_US$sylnum, selector_population
    ] <- sylrep_pairs [bird,]}, error = function (e) {stop(
      paste0(dim (temp_data_US), paste0(sylrep_pairs [bird,]), collapse = '')
    )})

    temp_data_US [
      pool.row, p_US$sylnum + 2, selector_population
    ] <- curiosities [bird]
  }

  temp_data_US [
    (4 - selection_context), p_US$sylnum + 3, selector_population
  ] <- selection_count

  return (temp_data_US)
}

# should_pick_neighbor <- function (index,total_chances,selection_context,
#                                  current_chance, sortSimlr,
#                                  repBarrier,chosenBird,
#                                  lower = 0,upper = 1) {
#   stuff = FALSE
#   lower_bound <- round (total_chances [selection_context] * lower) # 50
#   lower_bound <- round (num_select_chances [select_type] * 0.25) # 50

#   upper_bound <- round (total_chances [selection_context] * upper) # 75
#   print (paste0 ("sortSimlr = ", sortSimlr, collapse = ""))
#   print (paste0 ("chosenBird = ", chosenBird))
#   print (paste0 ("index = ", index))
#   print (paste0 ("current_chance = ", current_chance))
#   print (paste0 ("upper_bound = ", upper_bound))
#   print (paste0 ("lower_bound = ", lower_bound))

#   # is_desperate <- between (current_chance, lower_bound, upper_bound)

#   # is_neighbor_better <- sortSimlr[chosenBird+index] %in% repBarrier
#   if (
#     sortSimlr[which (sortSimlr == chosenBird) + index] %in% repBarrier
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

sing.selection <- function (p_SS, 
                            ro_SS,
                            temp_data_SS,
                            curiosity_level_SS, 
                            select_type,
                            sylrep_object,
                            num_select_chances = c (16, 40),
                            sylrep_fill_chances = 10,
                            verbose_output = TRUE,
                            interbreed = FALSE,
                            round_up = TRUE,
                            innate_bias = FALSE
                          #  selection_strategy = "similarity score",
                           ) {

  if (select_type == "mate") {select_type <- 2} else if (select_type == "tutor") {select_type <- 1}

  if (select_type == 2) {
    if (p_SS$mate_selection_type == "curiosity") {
      selection_path <- 1
    } else if (p_SS$mate_selection_type == "repertoire_size") {
      selection_path <- 2
    } else if (p_SS$mate_selection_type == "SRS_then_curiosity") {
      selection_path <- 3
    }
  } else if (select_type == 1) {
    if (p_SS$tutor_selection_type == "curiosity") {
      selection_path <- 1
    } else if (p_SS$tutor_selection_type == "repertoire_size") {
      selection_path <- 2
    } else if (p_SS$tutor_selection_type == "SRS_then_curiosity") {
      selection_path <- 3
    }#selection_path <- 1
  }

  for (divisible in 1 : 2) {
    if (num_select_chances[divisible] %% 4 != 0) {
    stop ("Need to be able to split num_select_chances into 4
          equal integer values. Make it mod4=0."
          )
    }
  }
  #print ("sing.selection beginning")
  for (population in 1 : p_SS$num_pop) { #population <- 1 rm (population)
    #print (paste("this is population",population,sep=" "))
    chance_for_selection = 1

    if (selection_path == 1) {
      while (chance_for_selection <= num_select_chances [select_type]) {
        stop = FALSE
        if (chance_for_selection == num_select_chances [select_type]) {
          auto.teachers <- matrix (c (sample (ro_SS [1, ],
            sylrep_fill_chances),sample (ro_SS [2, ],
            sylrep_fill_chances)),2,sylrep_fill_chances,TRUE)
          for (MTsylrep_filter in 1 : sylrep_fill_chances) {
            #c ((sample(ro_SS[1, ], 1)), (
              #sample(ro_SS[2, ], 1)))
            if ((
              sum (sylrep_object [auto.teachers [1,MTsylrep_filter
                  ], , population]) != 0) && (
              sum (sylrep_object [auto.teachers [2,MTsylrep_filter
                  ], , population]) != 0)) {
              if (verbose_output == TRUE) {
                context.name <- c ("Tutor", "Mate")
                warning (print (paste0 ("Automatic Teacher (s) = ",
                              auto.teachers [,MTsylrep_filter],
                                " for Population ", population,
                                " ", context.name [select_type],
                                " Selection")))
              }

              temp_data_SS = update_selexn_data (
                p_SS, temp_data_SS, auto.teachers [1,], MTsylrep_filter,
                auto.teachers [2,MTsylrep_filter], curiosity_level_SS, population,
                select_type, sylrep_object [auto.teachers [1,],,population],
                sylrep_object [auto.teachers [2,MTsylrep_filter],,population],
                num_select_chances [select_type])#, TRUE)

              # if (MTsylrep_filter >= 1) {}
              stop = TRUE
              break
            }
          }
          if (stop) {break
          }
        }

        if (select_type == 1) {
          #This statement separates specific mating and tutoring selection (resp.)
          # manipulations: singsuccessfilter will inform the selection of a
          # mate by restricting the successful mate to those individuals
          # from the same population as the selector. Similarly, selector.index
          # distinguishes between mating and tutoring, except here it uses
          # a randomly-selected female for the mating context, and the
          # offspring for tutoring.

          # "1-20"
          singsuccessfilter <- 1 : (
            (p_SS$one_pop_singers [select_type]) * (p_SS$num_pop))
          # male offspring from this timestep, lookin for a tutor
          selector.index <- temp_data_SS [3, p_SS$sylnum + 1, population]

        } else if (select_type == 2) {
          singsuccessfilter <- (1 + ((population - 1) * (
            p_SS$one_pop_singers [select_type]))) : (
              population * p_SS$one_pop_singers [select_type])
              # "1-10," or "11-20"
          selector.index <- sample (ro_SS [2, ], 1)
              # randomly sample a female from the population
        }

        selector.sylrep <- sylrep_object [selector.index, , population]
        if (sum (selector.sylrep) == 0) {
          stop (print (paste0 (rowSums(sylrep_object[,,population]), ", ")))
        }
        #print ("vapply")
        if (round_up == TRUE) {
          selection.index <- (
            # This creates sample calls for each population;
            # each population has a sample size of p_SS$one_pop_singers,
            # which comes from the male half of the population. Probability
            # defined by the fraction of syllable repertoires of each member of
            # each population divided by the maximum syllrep of the population.
            vapply (1 : p_SS$num_pop,
              function (x) {
                temp <- cpp_rowSums (sylrep_object[
                  ro_SS [1,],,x])
                sample (x = ro_SS [1,],
                        size = p_SS$one_pop_singers [select_type],
                        replace = FALSE,
                        prob = temp / max (temp))
              }, rep (0, p_SS$one_pop_singers [select_type])
            )
          ) # probability = the number of times each individual's syllable
            # repertoire has a 1 in it (sum (sylrep_object[
            # ro_SS[1,]])),
            # divided by the biggest repertoire's total.
        } else {
          selection.index <- (
            # This creates sample calls for each population;
            # each population has a sample size of p_SS$one_pop_singers,
            # which comes from the male half of the population. Probability
            # defined by the fraction of syllable repertoires of each member of
            # each population divided by the maximum syllrep of the population.
            vapply (1 : p_SS$num_pop,
              function (x) {
                temp <- cpp_rowSums (sylrep_object[
                  ro_SS [1,],,x])
                sample (x = ro_SS [1,],
                        size = p_SS$one_pop_singers [select_type],
                        replace = FALSE)
              }, rep (0, p_SS$one_pop_singers [select_type])
            )
          ) # probability = the number of times each individual's syllable
            # repertoire has a 1 in it (sum (sylrep_object[
            # ro_SS[1,]])),
            # divided by the biggest repertoire's total.
        }

        # create a matrix of all the sylrep_object of the sample
        # males from selection.index
        selection.sylreps <- t (
          cbind (
            vapply (
              1 : p_SS$one_pop_singers [select_type],
              function (x) {sylrep_object [selection.index[x,1],,1]},
              rep (0, dim (sylrep_object) [2])
            ),
            vapply (
              1 : p_SS$one_pop_singers [select_type],
              function (x) {sylrep_object [selection.index [x,2],,2]},
              rep (0, dim (sylrep_object) [2])
            )
          )
        )

        # applies the standard deviation scoring to the males in
        # selection.sylrep_object; larger score means greater
        # difference between male sylrep and selector's sylrep.
        # temp <- apply (X = selection.sylreps, MARGIN = 1,
        #               FUN = score_similarity,
        #               selector_vector = selector.sylrep)
        # golf_score <- sort (apply (X = selection.sylreps, MARGIN = 1,
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
        singer <- golf_score [round (curiosity_level_SS [
          selector.index, population] *(p_SS$one_pop_singers [
          select_type] * p_SS$num_pop) + 0.5)]
        if (sum (selection.sylreps [singer,])==0) {
          chance_for_selection = chance_for_selection + 1
          next
        }

        #should_pick_neighbor <- function (index,lower,upper=Inf) {

        if (!interbreed) {
          should_continue <- TRUE
          if (singer %in% singsuccessfilter) {

            temp_data_SS <- update_selexn_data (
              p_US = p_SS,
              temp_data_US = temp_data_SS,
              suitor_choices = selection.index,
              preferred_bird = singer,
              selector_bird = selector.index,
              curiosity_value = curiosity_level_SS,
              selector_population = population,
              selection_context = select_type,
              sylreps_choices = selection.sylreps,
              sylrep_selector = selector.sylrep,
              selection_count = chance_for_selection
            )#, FALSE)

            should_continue <- FALSE
          }

          if (should_continue == TRUE) {
            if (between (chance_for_selection, num_select_chances[select_type] * 0.25, num_select_chances[select_type] * 0.5)) {
              for (neighbor in c (1, -1, 2, -2)) {

                if (! (length (golf_score[which (golf_score == singer) + neighbor]) == 0)) {
                  if (golf_score[which (golf_score == singer) + neighbor] %in% singsuccessfilter) {

                    singer <- golf_score [which (golf_score == singer) + neighbor]

                    temp_data_SS <- update_selexn_data (
                      p_US = p_SS,
                      temp_data_US = temp_data_SS,
                      suitor_choices = selection.index,
                      preferred_bird = singer,
                      selector_bird = selector.index,
                      curiosity_value = curiosity_level_SS,
                      selector_population = population,
                      selection_context = select_type,
                      sylreps_choices = selection.sylreps,
                      sylrep_selector = selector.sylrep,
                      selection_count = chance_for_selection
                    )#, FALSE)

                    should_continue <- FALSE

                    break
                  }
                }
                if (! (should_continue)) {
                  break
                }
              }
            }
          }

          if (should_continue == TRUE) {
            if (between (chance_for_selection, num_select_chances[select_type] * 0.5, num_select_chances[select_type] * 0.75)) {
              for (neighbor in c (1, -1, 2, -2, 3, -3, 4, -4, 5, -5)) {

                if (! (length (golf_score[which (golf_score == singer) + neighbor]) == 0)) {
                  if (golf_score[which (golf_score == singer) + neighbor] %in% singsuccessfilter) {

                    singer <- golf_score [which (golf_score == singer) + neighbor]

                    temp_data_SS <- update_selexn_data (
                      p_US = p_SS,
                      temp_data_US = temp_data_SS,
                      suitor_choices = selection.index,
                      preferred_bird = singer,
                      selector_bird = selector.index,
                      curiosity_value = curiosity_level_SS,
                      selector_population = population,
                      selection_context = select_type,
                      sylreps_choices = selection.sylreps,
                      sylrep_selector = selector.sylrep,
                      selection_count = chance_for_selection
                    )#, FALSE)

                    should_continue <- FALSE

                    break
                  }
                }
                if (! (should_continue)) {
                  break
                }
              }
            }
          }

          if (should_continue == TRUE) {
            if (between (chance_for_selection, num_select_chances[select_type] * 0.75, num_select_chances[select_type] * 1)) {
              for (neighbor in c (1, -1, 2, -2, 3, -3, 4, -4, 5, -5,
                                6, -6, 7, -7, 8, -8, 9, -9, 10, -10)) {

                if (! (length (golf_score[which (golf_score == singer) + neighbor]) == 0)) {
                  if (golf_score[which (golf_score == singer) + neighbor] %in% singsuccessfilter) {

                    singer <- golf_score [which (golf_score == singer) + neighbor]

                    temp_data_SS <- update_selexn_data (
                      p_US = p_SS,
                      temp_data_US = temp_data_SS,
                      suitor_choices = selection.index,
                      preferred_bird = singer,
                      selector_bird = selector.index,
                      curiosity_value = curiosity_level_SS,
                      selector_population = population,
                      selection_context = select_type,
                      sylreps_choices = selection.sylreps,
                      sylrep_selector = selector.sylrep,
                      selection_count = chance_for_selection
                    )#, FALSE)

                    should_continue <- FALSE

                    break
                  }
                }
                if (! (should_continue)) {
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

            temp_data_SS <- update_selexn_data (
              p_US = p_SS,
              temp_data_US = temp_data_SS,
              suitor_choices = selection.index,
              preferred_bird = singer,
              selector_bird = selector.index,
              curiosity_value = curiosity_level_SS,
              selector_population = population,
              selection_context = select_type,
              sylreps_choices = selection.sylreps,
              sylrep_selector = selector.sylrep,
              selection_count = chance_for_selection
            )#, FALSE)

            break
          }
        }
        chance_for_selection = chance_for_selection + 1
      }
    } else if (selection_path == 2) {
      # We need these variables to run update_selexn_data

      selector.index <- sample (ro_SS [2, ], 1)

      if (round_up == TRUE) {
        selection.index <- (
          vapply (1 : p_SS$num_pop,
            function (x) {
              temp <- cpp_rowSums (sylrep_object[
                ro_SS [1,],,x])
              sample (x = ro_SS [1,],
                      size = p_SS$one_pop_singers [1],
                      replace = FALSE,
                      prob = temp / max (temp))
            }, rep (0, p_SS$one_pop_singers [select_type])
          )
        )
      } else {
        selection.index <- sample (ro_SS [1,], p_SS$one_pop_singers [1])
      }

      selection.index <- sample (ro_SS [1,], p_SS$one_pop_singers [1])
      selection.sylreps <- sylrep_object [selection.index,,population]
      selection.sylrepSums <- cpp_rowSums (sylrep_object [ro_SS [1,],,1]) [selection.index]
      # bigSylrep <- max (cpp_rowSums (sylrep_object[ro_SS [1,],,1]) [selection.index])
      if (length (which (selection.sylrepSums == max (selection.sylrepSums))) > 1) {
        singer <- which (selection.sylrepSums == max (selection.sylrepSums)) [sample (c (1 : length (which (selection.sylrepSums == max (selection.sylrepSums)))), 1)]
      } else if (length (which (selection.sylrepSums == max (selection.sylrepSums))) == 1) {
        singer <- which (selection.sylrepSums == max (selection.sylrepSums))
      } else {stop ("max sylrep selection problem")}

      selector.sylrep <- sylrep_object [selector.index, , population]
        if (sum (selector.sylrep) == 0) {
          stop (print (paste0 (rowSums(sylrep_object[,,population]), ", ")))
        }

      temp_data_SS <- update_selexn_data (
        p_US = p_SS,
        temp_data_US = temp_data_SS,
        suitor_choices = selection.index,
        preferred_bird = singer,
        selector_bird = selector.index,
        curiosity_value = curiosity_level_SS,
        selector_population = population,
        selection_context = select_type,
        sylreps_choices = selection.sylreps,
        sylrep_selector = selector.sylrep,
        selection_count = chance_for_selection,
        selection_type = selection_path
      )

    } else if (selection_path == 3) {
      selector.index <- sample (ro_SS [2, ], 1)
      selector.sylrep <- sylrep_object [selector.index, , population]
        if (sum (selector.sylrep) == 0) {
          stop (print (paste0 (rowSums(sylrep_object[,,population]), ", ")))
        }
      # if (round_up == TRUE) {
      #   temp <- cpp_rowSums (sylrep_object[ro_SS [1,],,x])
      #   selection.index <- sample (ro_SS [1,], p_SS$one_pop_singers [1])
      # }

      if (round_up == TRUE) {
          selection.index <- (
            vapply (1 : p_SS$num_pop,
              function (x) {
                temp <- cpp_rowSums (sylrep_object[
                  ro_SS [1,],,x])
                sample (x = ro_SS [1,],
                        size = p_SS$one_pop_singers [1],
                        replace = FALSE,
                        prob = temp / max (temp))
              }, rep (0, p_SS$one_pop_singers [select_type])
            )
          )
      } else {
        selection.index <- sample (ro_SS [1,], p_SS$one_pop_singers [1])
      }

      selection.sylreps <- sylrep_object [selection.index,,population]
      selection.sylrepSums <- cpp_rowSums (sylrep_object [ro_SS [1,],,1]) [selection.index]
      # bigSylrep <- max (cpp_rowSums (sylrep_object[ro_SS [1,],,1]) [selection.index])
      if (length (which (selection.sylrepSums == max (selection.sylrepSums))) > 1) {
        THING <- which (selection.sylrepSums == max (selection.sylrepSums))
        stuff <- array (as.numeric (paste0 (selection.sylreps[THING[1 : length (THING)],])), c (length (THING),length (selector.sylrep)))

        golf_score <- cpp_sort_indices (apply (X = stuff, MARGIN = 1,
                            FUN = score_similarity,
                            selector_vector = selector.sylrep))
        # orders the scored list of suitors; subsets one suitor from the rest,
        # according to the value of the selector's (auditory) curiosity.
        singer <- golf_score [round (curiosity_level_SS [
          selector.index, population] *length (golf_score) + 0.5)]
        if (sum (selection.sylreps [singer,])==0) {
          stop (paste0 ("singer ", singer, " doesn't have syllables"))
        }
        #
        # singer <- which (selection.sylrepSums == max (selection.sylrepSums)) [THING]
      } else if (length (which (selection.sylrepSums == max (selection.sylrepSums))) == 1) {
        singer <- which (selection.sylrepSums == max (selection.sylrepSums))
      } else {stop ("max sylrep selection problem")}


      temp_data_SS <- update_selexn_data (
        p_US = p_SS,
        temp_data_US = temp_data_SS,
        suitor_choices = selection.index,
        preferred_bird = singer,
        selector_bird = selector.index,
        curiosity_value = curiosity_level_SS,
        selector_population = population,
        selection_context = select_type,
        sylreps_choices = selection.sylreps,
        sylrep_selector = selector.sylrep,
        selection_count = chance_for_selection,
        selection_type = selection_path
      )
    }

  }
  return (temp_data_SS)
}


curiosity_learn <- function (p_CL,
                             temp_data_CL,
                             curinhproportion = singleormixture,
                             curinh_pattern = 1#,
                            #  invasion = FALSE,
                            #  invPopSize = 5
                            ) {
  p_CL$curinh_value
  if (curinh_pattern == 5) {

    paternalCurInh <- curinhproportion#p_CL$curinhproportion
    maternalCurInh <- 1 - paternalCurInh
    newcuriosity <- c (runif ((p_CL$num_pop * 2), -1, 1))

    for (population in 1 : (p_CL$num_pop)) {

      for (sex in 1 : 2) {
        if (
            temp_data_CL [1,
              p_CL$sylnum + 2,
              population
            ] == 0 ||
            temp_data_CL [2,
              p_CL$sylnum + 2,
              population
            ] == 0
           ) {stop (
          "not the time for learning curiosity from parents right now..."
          )
        }

        curinh_attempts <- 1

        tryCatch({while (((
              paternalCurInh * temp_data_CL [1, p_CL$sylnum + 2, population] +

              maternalCurInh * temp_data_CL [2, p_CL$sylnum + 2, population]
            ) +
            ((1 - p_CL$curinh_value) * (newcuriosity [2 * (population - 1) + sex
            ]))) < 0) {

            newcuriosity [2 * (population - 1) + sex] <- runif (1, 0, 1)
            curinh_attempts <- curinh_attempts + 1

          }}, error = function(e) {stop(print(c(paternalCurInh, temp_data_CL [1, p_CL$sylnum + 2, population], maternalCurInh, temp_data_CL [2, p_CL$sylnum + 2, population], p_CL$curinh_value, newcuriosity)))})

        while (((
            (paternalCurInh) * temp_data_CL [1, p_CL$sylnum + 2, population] +

            (maternalCurInh) * temp_data_CL [2, p_CL$sylnum + 2, population]
        ) +
        ((1 - p_CL$curinh_value) * (newcuriosity [2 * (population - 1) + sex
        ]))) > 1) {

          newcuriosity [2 * (population - 1) + sex] <- runif (1, -1, 0)
          curinh_attempts <- curinh_attempts + 1

        }

        new.curiosity <- (
            (paternalCurInh) * temp_data_CL [1, p_CL$sylnum + 2, population] +

            (1-paternalCurInh) * temp_data_CL [2, p_CL$sylnum + 2, population]
          ) +
          ((1 - p_CL$curinh_value) * (newcuriosity [2 * (population - 1) + sex
          ])) # Adding small proportion of noise

        temp_data_CL [
          (sex + 2), p_CL$sylnum + 4, population
        ] <- temp_data_CL [(sex + 2), p_CL$sylnum + 2, population]
        temp_data_CL [
          (sex + 2), p_CL$sylnum + 2, population
        ] <- new.curiosity
        temp_data_CL [
          (sex + 2), p_CL$sylnum + 5, population
        ] <- curinh_attempts
      }
    }

  } else {
    # pupil (column) learns from either father (1) or mother (2), and several possible patterns are listed (rows)
    curinh_teaching_patterns <- array (
      data = c (
        1, 2,
        1, 2,
        1, 2,
        2, 1
      ),
      dim = c (
        4,2
      ),
      dimnames = list (c ("father-only-(1)", "mother-only-(2)", "same-sex", "opposite-sex"),
                      c ("male bird", "female bird")
                    )
    )

    newcuriosity <- c (runif ((p_CL$num_pop * 2), -1, 1))

    for (population in 1 : (p_CL$num_pop)) {

      for (sex in 1 : 2) {
        if (temp_data_CL [
          curinh_teaching_patterns [curinh_pattern,sex],
          p_CL$sylnum + 2,
          population
        ] == 0) {stop (
          "not the time for learning curiosity from parents right now...")}

        curinh_attempts <- 1

        while ((temp_data_CL [curinh_teaching_patterns[
            curinh_pattern,sex
          ], p_CL$sylnum + 2, population] +
          ((1 - p_CL$curinh_value) * (newcuriosity [2 * (population - 1) + sex
          ]))) < 0) { # curiosity level below 0

          newcuriosity [2 * (population - 1) + sex] <- runif (1, 0, 1)
          curinh_attempts <- curinh_attempts + 1

        }

        while ((temp_data_CL [curinh_teaching_patterns[
            curinh_pattern,sex
          ], p_CL$sylnum + 2, population] +
          ((1 - p_CL$curinh_value) * (newcuriosity [2 * (population - 1) + sex
          ]))) > 1) { # curiosity level above 1

          newcuriosity [2 * (population - 1) + sex] <- runif (1, -1, 0)
          curinh_attempts <- curinh_attempts + 1

        }

        new.curiosity <- temp_data_CL [curinh_teaching_patterns [
            curinh_pattern,sex
          ], p_CL$sylnum + 2, population] +
          ((1 - p_CL$curinh_value) * (
            newcuriosity [2 * (population - 1) + sex
          ])) # Adding small proportion of noise

        temp_data_CL [
          (sex + 2), p_CL$sylnum + 4, population
        ] <- temp_data_CL [(sex + 2), p_CL$sylnum + 2, population]
        temp_data_CL [
          (sex + 2), p_CL$sylnum + 2, population
        ] <- new.curiosity
        temp_data_CL [
          (sex + 2), p_CL$sylnum + 5, population
        ] <- curinh_attempts
      }
    }
  }

  # if (invasion) {
  #   invaderIDs <- sample(,invPopSize,)

  # }

  return (temp_data_CL)
}

recuriosity.offspring <- function (p_RC, temp_data_RC, curiosity_object) {
  for (population in 1 : p_RC$num_pop) {
    for (sex in 1 : 2) {

      curiosity_object [
        temp_data_RC [
          (sex + 2), p_RC$sylnum + 1, population
        ], population] <- temp_data_RC [
          (sex + 2), p_RC$sylnum + 2, population]
    }
  }
  return (curiosity_object)
}


resylreps.offspring <- function (p_RS, temp_data_RS, sylrep_object) {
  for (population in 1 : p_RS$num_pop) {
    for (sex in 1 : 2) {

      sylrep_object [temp_data_RS[
        (sex + 2), p_RS$sylnum + 1, population
        ], , population] <- temp_data_RS [
          (sex + 2), 1 : p_RS$sylnum, population
        ]
    }
  }
  return (sylrep_object)
}


sylrep_rowcol.archive <- function (p_src_archive,
                                   data_container,
                                   syllable_object,
                                   timestep) {
  for (population in 1 : p_src_archive$num_pop) {
    for (sex in 1 : 2) {
      #sylrep_rowcol
      data_container [sex, population, timestep] <- mean (rowSums (
       syllable_object [
        ((
           1 + ((sex - 1) * (p_src_archive$pop_size / 2))
         ) : (
           sex * (p_src_archive$pop_size / 2)
        )), , population]
      ))
    }
  }
  return (data_container)
}


sylrep_dstbxn.archive <- function (p_sdb_archive,
                                   data_container,
                                   syllable_object,
                                   timestep) {

  for (population in 1 : p_sdb_archive$num_pop) {
    for (sex in 1 : 2) {
      # sylrep_dstbxn
      data_container [(((population - 1) * 2) + sex), , timestep] <- colSums (
        syllable_object [((
          1 + ((sex - 1) * (p_sdb_archive$pop_size / 2))
          ) : (
          sex * (p_sdb_archive$pop_size / 2)
          )), , population]
      )
    }
  }
  return (data_container)
}


curity_mean_t.archive <- function (p_cmt_archive,
                                   temp_data_cmt_archive,
                                   data_container,
                                   curiosity_object,
                                   timestep) {
  for (population in 1 : p_cmt_archive$num_pop) {
    # curity_mean_t
    data_container [3, population, timestep] <- temp_data_cmt_archive [
      2, p_cmt_archive$sylnum + 3, population]
    data_container [10, population, timestep] <- temp_data_cmt_archive [
      3, p_cmt_archive$sylnum + 3, population]

    for (sex in 1 : 2) {

      # let's make indices 13 and 14 on dimension 1... these are the measures of variance in curiosity level in both male and female subpopulations
      # first, we need to make sure that everywhere else it's referenced, ALSO, gets accounted for and changed accordingly.

      data_container [sex, population, timestep] <- mean (
        curiosity_object [((1 + ((sex-1) * p_cmt_archive$pop_size / 2)
        ) : (sex * p_cmt_archive$pop_size / 2)), population])

      # Individual Curiosity Values
      data_container [(sex + 3), population, timestep
      ] <- temp_data_cmt_archive [sex, p_cmt_archive$sylnum + 2, population]

      data_container [(sex + 5), population, timestep
      ] <- temp_data_cmt_archive [(sex + 2), p_cmt_archive$sylnum + 2, population]

      data_container [(sex + 7), population, timestep
      ] <- temp_data_cmt_archive [(sex + 2), p_cmt_archive$sylnum + 4, population]

      data_container [11, population, timestep
      ] <- temp_data_cmt_archive [(sex + 2), p_cmt_archive$sylnum + 5, population] # problems: this value degenerates two into one, by not leaving "sex" variable on left of equation

      data_container [12, population, timestep
      ] <- temp_data_cmt_archive [sex, p_cmt_archive$sylnum + 5, population] # same as above

      # data_container [
      #   (sex + 12), population, timestep
      # ] <- temp_data_cmt_archive [sex + 3, p_cmt_archive$sylnum + 3, population] # once curiosity variance is recorded, it'll be forwarded to the permanent data object

      data_container [(sex + 12), population, timestep
      ] <- var (
        curiosity_object [((1 + ((sex-1) * p_cmt_archive$pop_size / 2)
        ) : (sex * p_cmt_archive$pop_size / 2)), population]
        ) # once curiosity variance is recorded, it'll be forwarded to the permanent data object

      # data_container [
      #   14, population, timestep
      # ] <- temp_data_cmt_archive [sex, p_cmt_archive$sylnum + 5, population] # same as above

      data_container [(sex + 14), population, timestep
      ] <- temp_data_cmt_archive [(sex + 2), p_cmt_archive$sylnum + 6, population] # 

    }
  }
  return (data_container)
}


#       data_container[
#         12, population, timestep
#       ] <- temp_data_cmt_archive[sex, parameters$sylnum + 5, population]
#     }
#   }
#   return (data_container)
# }

curity_repert.archive <- function (p_crp_archive,
                                   data_container,
                                   curiosity_object,
                                   timestep) {

  histbreaks <- (0 : (p_crp_archive$num_pop * p_crp_archive$one_pop_singers [1])) * (1 / (p_crp_archive$num_pop * p_crp_archive$one_pop_singers [1]))

  for (population in 1 : p_crp_archive$num_pop) {
    for (sex in 1 : 2) {
      # curity_repert
      data_container [
        (sex + ((population - 1) * 2)), , timestep
      ] <- hist (curiosity_object [((
        1 + ((sex-1) * p_crp_archive$pop_size / 2)
      ) : (
        sex * p_crp_archive$pop_size / 2
      )), population], breaks =
      histbreaks, plot = FALSE)$counts # (0 : (p_crp_archive$num_pop * p_crp_archive$one_pop_singers [1])) * (1 / (p_crp_archive$num_pop * p_crp_archive$one_pop_singers [1]))
    }
  }
  return (data_container)
}


store_timesteps <- function (p_storetimesteps, filename = thousand_timesteps,
  rowcol, dstbxn, mean_t, repert, saved_stuff, syll_container,
  cur_container, run_timedate, foldername = foldername#,
  # simnumber = simnumber
  ) {
   # # # #  #directory <- getwd ()
  results_directory <- file.path ('results')
  if (filename == 1) {
    # run_timedate <- format (Sys.time(), "%F-%H%M%S")
    if (! (dir.exists (file.path (results_directory, saved_stuff$docnamez)))) {
      dir.create (file.path (results_directory, saved_stuff$docnamez))
      dir.create (file.path (results_directory, saved_stuff$docnamez,
        "variable_store"))
    }
    dir.create (file.path (results_directory, saved_stuff$docnamez,
      "variable_store", paste0 (run_timedate, "-GMT-variable-store")))
    # foldername <- file.path (results_directory, saved_stuff$docnamez,
    #   "variable_store", paste0 (run_timedate, "-GMT-variable-store"))
    saveRDS (saved_stuff, file.path (foldername, "metadata.RData"))
  }
  # else {
  #   foldername <- readRDS (file.path (
  #       "source", "temp", paste0 (
  #         "foldername_", parameters$simnumber, ".RData")
  # ))}

  data_categories <- c ("sylrep_rowcol", "sylrep_dstbxn",
              "curity_mean_t", "curity_repert")

  for (data_record in 1 : length(data_categories)) {

    file.create (file.path (foldername, paste0 (
      "variable-store-", filename, "-", data_categories[data_record], ".RData")))
    if (data_record == 1) {
      objekshun <- rowcol
    } else if (data_record == 2) {
      objekshun <- dstbxn
    } else if (data_record == 3) {
      objekshun <- mean_t
    } else if (data_record == 4) {
      objekshun <- repert
    }
    # print ("tryna save")
    saveRDS (objekshun, file.path (foldername, paste0 (
      "variable-store-", filename, "-", data_categories[data_record], ".RData")))
    # print ("saved")
  }
  saveRDS (p_storetimesteps, file.path (
    foldername, "defined_parameters.RData"))
  saveRDS (syll_container, file.path (
    foldername, "end_sylbls.RData"))
  saveRDS (cur_container, file.path (
    foldername, "end_cursty.RData"))

  return (foldername)
}
