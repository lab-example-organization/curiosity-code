
#

# define_parameters <- function (
#                                #num_timesteps, 
#                                num_pop, 
#                                pop_size#, 
#                               #  sylnum#, 
#                               #  nsl#, ### only used for this function
#                               #  one_pop_singers,
#                                #curlearnprob, 
#                               #  learnprob, 
#                               #  randlearnprob, 
#                               #  stand.dev#, 
#                                #curinhproportion,
#                               #  mate_selection_type, 
#                               #  tutor_selection_type, 
#                               #  selection_round_up
#                               ) {
#   # Here the if-statements help organize and restrict the arguments such that the Weirdness Works (TM) :P
#   if (num_pop %% 1 != 0 || pop_size %% 1 != 0 || nsl %% 1 != 0) {
#     stop ("(num_pop, pop_size, nsl) need to be integers")}
#   if ((num_pop == 3 || num_pop == 4) &&
#       ((sylnum - 4 * nsl) %% 2 != 0 || (nsl) %% 2 != 0)) {
#     stop ("sylnum and popnum incompat w NSL value")}
#   if ((num_pop == 5 || num_pop == 6) &&
#       ((sylnum - 4 * nsl) %% 6 != 0 || (nsl) %% 4 != 0)) {
#     stop ("sylnum and popnum incompat w NSL value")}
#   if ((num_pop == 7 || num_pop == 8) &&
#       ((sylnum - 4 * nsl) %% 12 != 0 || (nsl) %% 12 != 0)) {
#     stop ("sylnum and popnum incompat w NSL value")}
#   if ((num_pop == 9 || num_pop == 10) &&
#       ((sylnum - 4 * nsl) %% 60 != 0 || (nsl) %% 24 != 0)) {
#     stop ("sylnum and popnum incompat w NSL value")}
#   if (num_timesteps %% 1000 != 0) {
#     stop ("num_timesteps needs to be divisible by 1000. It's for recording purposes.")
#   }

#   pop_calls_matrix <- matrix (data = c (1 : pop_size), nrow = 2, ncol = (pop_size / 2), byrow = TRUE)

#   # zero_to_one_template <- c ( 0.00,0.01,0.05,0.09, 0.1,0.15,0.18, 0.2,0.25,0.27,
#   # #                             #1,  #2,  #3,  #4,  #5,  #6,  #7,  #8,  #9, #10,
#   #                              0.3,0.35,0.36, 0.4,0.45,0.49, 0.5,0.51,0.54,0.55,
#   # #                            #11, #12, #13, #14, #15, #16, #17, #18, #19, #20,
#   #                             0.59, 0.6,0.63,0.65, 0.7,0.72,0.75, 0.8,0.81,0.85,
#   # #                            #21, #22, #23, #24, #25, #26, #27, #28, #29, #30,
#   #                              0.9,0.95,0.99,1.0)
#   # #                            #31, #32, #33,#34

#   parameters <- list (
#                     #  num_timesteps = num_timesteps, ###
#                     #  num_pop = num_pop, ###
#                     #  pop_size = pop_size, ###
#                     #  sylnum = sylnum, ###
#                     #  nsl = nsl, ###
#                     #  one_pop_singers = one_pop_singers, ###
#                      pop_calls_matrix = pop_calls_matrix#,
#                      #curiositybreaks = curiositybreaks,
#                     #  curiosity_counter = curiosity_counter,
#                     #  zero_to_one_template = zero_to_one_template#,
#                     #  population_syll_probs = population_syll_probs, ### 
#                      #curlearnprob = curlearnprob, ###
#                     #  learnprob = learnprob, ###
#                     #  randlearnprob = randlearnprob, ###
#                     #  stand.dev = stand.dev#, ###
#                      #curinhproportion = curinhproportion,
#                     #  mate_selection_type = mate_selection_type, ###
#                     #  tutor_selection_type = tutor_selection_type, ###
#                     #  selection_round_up = selection_round_up ###
#                      )

#   return (parameters)
# }

define_temp_data <- function (universal_parameters) {
  # tempCatgry = 1 (learning.pool); tempCatgry = 2 (pairing.pool)
  # if (tempCatgry ==1) {
    temp_data <- array (
      0, c (
        5, universal_parameters$sylnum + 6, universal_parameters$num_pop))
    ######   The first params$sylnum columns are learning_pool;
    ######   the last 5 are pairing_pool
  # } else {
    # temp_data <- array (0, c (5, 5, universal_parameters$num_pop))
  # }

  # REALLY WANT TO HAVE:
  #   SPLIT DATA INTO ARRAY OF 2-D SYLL DATA
  #   AND THE OTHER, AC-RELATED METRICS
  #   IS A LIST GOING TO BE THE BEST OPTION HERE?
  return (temp_data)
}

# INITIALIZING FUNCTIONS ##################################

recordvariable.initialize <- function (p_rvi, recsimfct, variableid) {

  timestepRecordLength <- 1000 / recsimfct

  if (variableid == 1) {
    record_variable <- array (
      0, c (2, p_rvi$num_pop, timestepRecordLength))
  } else if (variableid == 2) {
    record_variable <- array (
      0, c ((2 * p_rvi$num_pop), p_rvi$sylnum, timestepRecordLength))
  } else if (variableid == 3) {
    record_variable <- array (
      0, c (16, p_rvi$num_pop, timestepRecordLength))
      # let's make indices 13 and 14 on dimension 1... these are the measures of variance in curiosity level in both male and female subpopulations
  } else if (variableid == 4) {
    record_variable <- array (
      0, c ((2 * p_rvi$num_pop), (p_rvi$num_pop * p_rvi$one_pop_singers [1]),
        timestepRecordLength))
  }
  # record_variable <- list (sylrep_rowcol=array (0, c (2, P$num_pop, (P$num_timesteps/simplificationFactor))), ### rows: num_sexes, num_measurements: rowSums and colSums ### cols: num_pop ### 3rd-dim: timesteps
  #                         sylrep_dstbxn=array (0, c ((2 * P$num_pop), P$sylnum, (P$num_timesteps/simplificationFactor))), ### rows: num_pop, num_sexes ### cols: sylnum ### 3rd-dim: timesteps
  #                         curity_mean_t=array (0, c (14, P$num_pop, (P$num_timesteps/simplificationFactor))), ### rows: num_sexes ### cols: num_pop ### 3rd-dim: timesteps
  #                         curity_repert=array (0, c ((2 * P$num_pop), (P$num_pop * P$one_pop_singers[1]), (P$num_timesteps/simplificationFactor))) ### rows: num_sexes ### cols: num_pop, num_singers_sampled ### 3rd-dim: timesteps
  #                         )
    # 1 & 2: Rows 1 and 2 are curiosity values for the mean of the males (row 1) and females (row 2) from each population, per timestep.
    # 3: Row 3 covers the number of selections made by females from each population, per timestep.
    # 4 & 5: Rows 4-9 cover the individual curiosity values recorded, regarding the individuals that source the curiosity inheritance(father (row 4) and mother (row 5));
    # 6, 7, 8, & 9: The curiosity values inherited by the offspring (son (row 6) and daughter (row 7)); and the curiosity values of those killed off (dead male (row 8) and female (row 9)),
    # per timestep.
    # Then the amount of times curiosity inheritance had to run (see while loop in curiosity_learn) is recorded here
    #
    # 13 & 14: variance values for each subpopulation, males (row 13) and females (row 14)
  return (record_variable)
}

#day.tuh <- recordvariable.initialize

initialize.sylrep <- function (p_is = params, population.pattern, pastrunobject_is = FALSE,
                            eqpop = TRUE, eqsex = TRUE, pastruninit_is = FALSE) {
  
  # print(p_is$num_pop)

  if (length (population.pattern) != p_is$num_pop) {
    stop ("This determines the initial syllable distributions of each
           subpopulation. It is a vector of row calls for
           population_syll_probs, so it must match the number of populations")
  }


  # if (xor ((pastrunobject_is == FALSE), (pastruninit_is == FALSE))) {
  #   stop ("Both pastrunobject_is and pastruninit_is need to both
  #   be engaged together... if they aren't, it won't work!")
  # }

  # making the object that will hold each instance of the function,
  # hopefully to-be-assigned to specific variables for an
  # instantiation of the model ¯\_(ツ)_/¯
  if (pastruninit_is) {
    # if (! (pastrunobject_is)) {
    #   stop ("Both pastrunobject_is and pastruninit_is need to both
    #     be engaged together... if they aren't, it won't work!")
    #   }
    # sylreps <- aperm (pastrunobject_is [, , 1 : p_is$sylnum], c (2, 3, 1),
    #             na.rm = TRUE) # pop_size (2), p_is$sylnum (3), num_pop (1)

    sylreps <- pastrunobject_is[[1]]

  } else {
    p_z <- c ( 0.00,0.01,0.05,0.09, 0.1,0.15,0.18, 0.2,0.25,0.27,
    #            #1,  #2,  #3,  #4,  #5,  #6,  #7,  #8,  #9, #10,
                0.3,0.35,0.36, 0.4,0.45,0.49, 0.5,0.51,0.54,0.55,
    #           #11, #12, #13, #14, #15, #16, #17, #18, #19, #20,
              0.59, 0.6,0.63,0.65, 0.7,0.72,0.75, 0.8,0.81,0.85,
    #           #21, #22, #23, #24, #25, #26, #27, #28, #29, #30,
                0.9,0.95,0.99,1.0)
    #           #31, #32, #33,#34
    # Syllable probability distribution stuff; ends with reference matrix where each row defines a different pattern of syllable probability distributions
    if (p_is$num_pop == 1) {
      syllprob_vector <- c (
        c (rep (p_z[1], (p_is$sylnum - 4 * p_is$num_sylls_per_prob_lvl)/2),rep (p_is$num_sylls_per_prob_lvl,2,2),rep (p_is$num_sylls_per_prob_lvl,2,5),rep (p_is$num_sylls_per_prob_lvl,2,31),rep (p_is$num_sylls_per_prob_lvl,1,33),rep (p_is$num_sylls_per_prob_lvl,2,31),rep (p_is$num_sylls_per_prob_lvl,2,5),rep (p_is$num_sylls_per_prob_lvl,2,2),rep (p_is$sylnum - 4 * p_is$num_sylls_per_prob_lvl,2,1))
      )###### rep (p_is$sylnum - 4 * p_is$num_sylls_per_prob_lvl,2,1)
    } else if (p_is$num_pop == 2) {

      p_s <- p_is$sylnum
      p_n <- p_is$num_sylls_per_prob_lvl

      # syllprob_vector <- c (
        r_1 <- list(
          c(0,0.01, 0.1,0.25,0.75,0.9,0.99,   1,0),
          c(0,   1,0.99,0.9,0.75,0.25, 0.1,0.01,0),
          c(75,8,8,8,8,8,8,8,25),
          c(25,8,8,8,8,8,8,8,75))

        r_2 <- list(
          c(0,   0.01,0.1, 0.9,0.99, 0.9,0.1,0.01,   0),
          c(0,0.99, 0.9,0.1,0.01,   0,0.01,0.1, 0.9,0.99,0),
          c(62,4,4,4,8,4,4,4,62),
          c(31,4,4,4,4,62,4,4,4,4,31))

        r_3 <- list(
          c(0,0.01,0.1,0.9,0.99,0.9,0.1,0.01,0,0.01,0.1,0.9,0.99,0.9,0.1,0.01,0), 
          c(0.99,0.9,0.1,0.01,0,0.01,0.1,0.9,0.99,0.9,0.1,0.01,0,0.01,0.1,0.9,0.99),
          c(42,2,2,2,2,2,2,2,44,2,2,2,2,2,2,2,42),
          c(2,2,2,2,63,2,2,2,2,2,2,2,63,2,2,2,2))

        r_4 <- list(
          c(0,0.01,0.1,0.9,0.99,0.9,0.1,0.01,0,0.01,0.1,0.9,0.99,0.9,0.1,0.01,0,0.01,0.1,0.9,0.99,0.9,0.1,0.01,0), 
          c(0.99,0.9,0.1,0.01,0,0.01,0.1,0.9,0.99,0.9,0.1,0.01,0,0.01,0.1,0.9,0.99,0.9,0.1,0.01,0,0.01,0.1,0.9,0.99),
          c(33,1,1,1,2,1,1,1,33,1,1,1,2,1,1,1,33,1,1,1,2,1,1,1,33),
          c(3,1,1,1,42,1,1,1,3,1,1,1,42,1,1,1,3,1,1,1,42,1,1,1,3))

        r_5 <- list(
          c(0,0.01,0.1,0.9,0.99,0.9,0.1,0.01,0,0.01,0.1,0.9,0.99,0.9,0.1,0.01,0,0.01,0.1,0.9,0.99,0.9,0.1,0.01,0,0.01,0.1,0.9,0.99,0.9,0.1,0.01,0), 
          c(0.99,0.9,0.1,0.01,0,0.01,0.1,0.9,0.99,0.9,0.1,0.01,0,0.01,0.1,0.9,0.99,0.9,0.1,0.01,0,0.01,0.1,0.9,0.99,0.9,0.1,0.01,0,0.01,0.1,0.9,0.99),
          c(24,1,1,1,3,1,1,1,24,1,1,1,3,1,1,1,24,1,1,1,3,1,1,1,24,1,1,1,3,1,1,1,24),
          c(4,1,1,1,28,1,1,1,4,1,1,1,28,1,1,1,4,1,1,1,28,1,1,1,4,1,1,1,28,1,1,1,4))

        # r_6 <- list(
        #   c(), 
        #   c())

        # r_7 <- list(
        #   c(), 
        #   c())
        r1_vector <- c()
        r2_vector <- c()
        r3_vector <- c()
        r4_vector <- c()
        r5_vector <- c()
        
        for (i in 1:length(r_1[[1]])) {r1_vector <- append(r1_vector, rep (r_1[[1]][i], r_1[[3]][i]))}
        for (ii in 1:length(r_1[[2]])) {r1_vector <- append(r1_vector, rep (r_1[[2]][ii], r_1[[4]][ii]))}
        for (iii in 1:length(r_2[[1]])) {r2_vector <- append(r2_vector, rep (r_2[[1]][iii], r_2[[3]][iii]))}
        for (iv in 1:length(r_2[[2]])) {r2_vector <- append(r2_vector, rep (r_2[[2]][iv], r_2[[4]][iv]))}
        for (v in 1:length(r_3[[2]])) {r3_vector <- append(r3_vector, rep (r_3[[1]][v], r_3[[3]][v]))}
        for (vi in 1:length(r_3[[2]])) {r3_vector <- append(r3_vector, rep (r_3[[2]][vi], r_3[[4]][vi]))}
        for (vii in 1:length(r_4[[2]])) {r4_vector <- append(r4_vector, rep (r_4[[1]][vii], r_4[[3]][vii]))}
        for (viii in 1:length(r_4[[2]])) {r4_vector <- append(r4_vector, rep (r_4[[2]][viii], r_4[[4]][viii]))}
        for (ix in 1:length(r_5[[2]])) {r5_vector <- append(r5_vector, rep (r_5[[1]][ix], r_5[[3]][ix]))}
        for (x in 1:length(r_5[[2]])) {r5_vector <- append(r5_vector, rep (r_5[[2]][x], r_5[[4]][x]))}
        syllprob_vector <- c(r1_vector, r2_vector, r3_vector, r4_vector, r5_vector)
    }

    population_syll_probs <- matrix (data = syllprob_vector,
                                    nrow = length (syllprob_vector) / p_s, #number of rows to complement the number of combinations I've come up with; so if I come up with more, fix it.
                                    ncol = p_s,
                                    byrow = TRUE
    )

    sylreps <- array (0, c (p_is$pop_size, p_is$sylnum, p_is$num_pop))

    for (i in 1 : p_is$num_pop) {
      sylreps [, , i] <- t (replicate (p_is$pop_size, rbinom (
        length (population_syll_probs [population.pattern [i], ]),
        size = 1, prob = population_syll_probs [population.pattern [i], ])))
    }
  }
  return (sylreps)
}

initialize.curiosity <- function (p_ic, ro_ic, cur.min, cur.max,
            pastrunobject_ic = FALSE, pastruninit_ic = FALSE) {

  # if (xor ((pastrunobject_ic == FALSE), (pastruninit_ic == FALSE))) {
  #   stop ("Both pastrunobject_ic and pastruninit_ic need to both
  #   be engaged together... if they aren't, it won't work!")
  # }

  warning ("These arguments must be ordered - highest level
            population, then role- singers before choosers")
  if (length (cur.min) != length (cur.max) ||
     length (cur.min) != (p_ic$num_pop * 2)) {
    print ("Error Log #0003: each argument needs to be a
           vector that matches the number of populations
           AND the number of sexes - Make sure the number
           of elements matches the number of starting
           curiosity values.")
    stop ("cur.max and cur.min have to be the same length.")
  }

  p_z <- c (0.00,0.01,0.05,0.09,0.1,0.15,0.18,0.2,0.25,0.27,
            0.3,0.35,0.36,0.4,0.45,0.49,0.5,0.51,0.54,0.55,
            0.59,0.6,0.63,0.65,0.7,0.72,0.75,0.8,0.81,0.85,
            0.9,0.95,0.99,1.0)

  for (i in 1 : length (cur.min)) {
    if (p_z [cur.max [i]
    ] <= p_z [cur.min [i]] # ||
      #  cur.max[i] %% 1 != 0 ||
      #  cur.min[i] %% 1 != 0
       ) {
      stop ("maximum value needs to be bigger than minimum value.
            They need to be integers too - these are reference
            calls to zero_to_one_template- check out the values")
    }
  }
  curiosity_level <- array (0, c (p_ic$pop_size, p_ic$num_pop))
  if (pastruninit_ic) {
    # curiosity_object <- pastrunobject_ic [, , p_ic$sylnum + 1]
    # curiosity_level <- aperm (curiosity_object, c (2, 1), na.rm = TRUE)

    curiosity_level <- pastrunobject_ic[[2]]

  } else {
    curiosity_counter <- matrix (data = 1 : (p_ic$num_pop * 2), nrow = 2, ncol = p_ic$num_pop, byrow = FALSE)
    for (pop.num in 1 : p_ic$num_pop) {
      for (sexes in 1 : 2) {
        curiosity_level [((
            1 + ((sexes - 1) * (p_ic$pop_size / 2))
          ) : (
            sexes * p_ic$pop_size / 2
          )), pop.num
        ] <- runif (
          p_ic$pop_size / 2,
          p_z [cur.min [curiosity_counter [sexes, pop.num]]],
          p_z [cur.max [curiosity_counter [sexes, pop.num]]]
        )
      }
    }
  }
  return (curiosity_level)
}

# invasion_function <- function (sylreps, curiosity_level,
# population_s_affected) { # for now, population_s_affected
# will refer to subpopulations by their pop-sex number: if
# two pops, the numbers are 1) pop1 male, 2) pop1 female,
# 3) pop2 male, 4) pop2 female

#}
