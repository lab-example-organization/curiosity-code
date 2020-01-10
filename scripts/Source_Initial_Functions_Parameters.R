
rpf <- function (numberofrepeats, divisions_per_repeat, zeroonevalue) {
  zero_to_one_template <- c ( 0.00,0.01,0.05,0.09, 0.1,0.15,0.18, 0.2,0.25,0.27,
  #                             #1,  #2,  #3,  #4,  #5,  #6,  #7,  #8,  #9, #10,
                               0.3,0.35,0.36, 0.4,0.45,0.49, 0.5,0.51,0.54,0.55,
  #                            #11, #12, #13, #14, #15, #16, #17, #18, #19, #20,
                              0.59, 0.6,0.63,0.65, 0.7,0.72,0.75, 0.8,0.81,0.85,
  #                            #21, #22, #23, #24, #25, #26, #27, #28, #29, #30,
                               0.9,0.95,0.99,1.0)
  #                            #31, #32, #33,#34
  if (numberofrepeats %% divisions_per_repeat > 1e-10) {
    stop ("first element must be divisible by the second element")}
  if (! (zeroonevalue %in% c (1 : length (zero_to_one_template)))) {
    stop ("in order to work, zeroonevalue must be contained within zero_to_one_template")}
  return (replicate (
    c (length = numberofrepeats / divisions_per_repeat), zero_to_one_template [zeroonevalue]))
}

define_parameters <- function (num_timesteps, num_pop, pop_size, sylnum, nsl, one_pop_singers,
                              curlearnprob, learnprob, randlearnprob, stand.dev, curinhproportion) {
  # Here the if-statements help organize and restrict the arguments such that the Weirdness Works(TM) :P
  if (num_pop %% 1 != 0 || pop_size %% 1 != 0 || nsl %% 1 != 0) {
    stop ("(num_pop, pop_size, nsl) need to be integers")}
  if ((num_pop == 3 || num_pop == 4) &&
      ((sylnum - 4 * nsl) %% 2 != 0 || (nsl) %% 2 != 0)) {
    stop ("Don't be a fool; check error log #_0001")}
  if ((num_pop == 5 || num_pop == 6) &&
      ((sylnum - 4 * nsl) %% 6 != 0 || (nsl) %% 4 != 0)) {
    stop ("Don't be a fool; check error log #_0001")}
  if ((num_pop == 7 || num_pop == 8) &&
      ((sylnum - 4 * nsl) %% 12 != 0 || (nsl) %% 12 != 0)) {
    stop ("Don't be a fool; check error log #_0001")}
  if ((num_pop == 9 || num_pop == 10) &&
      ((sylnum - 4 * nsl) %% 60 != 0 || (nsl) %% 24 != 0)) {
    stop ("Don't be a fool; check error log #_0001")}
  if (num_timesteps %% 1000 != 0) {
    stop ("num_timesteps needs to be divisible by 1000. It's for recording purposes.")
  }

  pop_calls_matrix <- matrix (data = c (1 : pop_size), nrow = 2, ncol = (pop_size / 2), byrow = T)


  #new.curiosity <- array(0,c(2,num_pop))
  curiositybreaks <- (0 : (num_pop * one_pop_singers [1])) * (1 / (num_pop * one_pop_singers [1]))
  curiosity_counter <- matrix (data = 1 : (num_pop * 2), nrow = 2, ncol = num_pop, byrow = F)
  # zero_to_one_template <- c(0.00,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,
  # #                           #1,  #2, #3,  #4,  #5,  #6, #7,  #8, #9,#10,
  #                           0.45,0.49,0.5,0.51,0.55,0.59,0.6,0.65,0.7,0.75,
  # #                          #11, #12,#13, #14, #15, #16,#17, #18,#19, #20,
  #                            0.8,0.85,0.9,0.95,0.99,1.0,0.09,0.18,0.27,0.36,
  # #                          #21, #22,#23, #24, #25,#26, #27, #28, #29, #30,
  #                            0.45,0.54,0.63,0.72,0.81)
  # #                           #31, #32, #33, #34, #35

  zero_to_one_template <- c ( 0.00,0.01,0.05,0.09, 0.1,0.15,0.18, 0.2,0.25,0.27,
  #                             #1,  #2,  #3,  #4,  #5,  #6,  #7,  #8,  #9, #10,
                               0.3,0.35,0.36, 0.4,0.45,0.49, 0.5,0.51,0.54,0.55,
  #                            #11, #12, #13, #14, #15, #16, #17, #18, #19, #20,
                              0.59, 0.6,0.63,0.65, 0.7,0.72,0.75, 0.8,0.81,0.85,
  #                            #21, #22, #23, #24, #25, #26, #27, #28, #29, #30,
                               0.9,0.95,0.99,1.0)
  #                            #31, #32, #33,#34

  # Syllable probability distribution stuff; ends with reference matrix where each row defines a different pattern of syllable probability distributions
  if (num_pop == 1) {
    syllprob_vector <- c (
      c (rpf (sylnum - 4 * nsl,2,1),rpf (nsl,2,2),rpf (nsl,2,5),rpf (nsl,2,31),rpf (nsl,1,33),rpf (nsl,2,31),rpf (nsl,2,5),rpf (nsl,2,2),rpf (sylnum - 4 * nsl,2,1))
    )
  } else if (num_pop > 1) {
    syllprob_vector <- c (
      c (rpf (sylnum - 7 * nsl,(4/3),1),rpf (nsl,1,2),rpf (nsl,1,5),rpf (nsl,1,9),rpf (nsl,1,27),rpf (nsl,1,31),rpf (nsl,1,33),rpf (nsl,1,34),rpf (sylnum - 7 * nsl,4,1)),
      c (rpf (sylnum - 7 * nsl,4,1),rpf (nsl,1,34),rpf (nsl,1,33),rpf (nsl,1,31),rpf (nsl,1,27),rpf (nsl,1,9),rpf (nsl,1,5),rpf (nsl,1,2),rpf (sylnum - 7 * nsl,(4/3),1)),
      c (rpf (sylnum - 4 * nsl,2,1),rpf (nsl,2,2),rpf (nsl,2,5),rpf (nsl,2,31),rpf (nsl,1,33),rpf (nsl,2,31),rpf (nsl,2,5),rpf (nsl,2,2),rpf (sylnum - 4 * nsl,2,1)),
      c (rpf (sylnum - 4 * nsl,4,1),rpf (nsl,2,33),rpf (nsl,2,31),rpf (nsl,2,5),rpf (nsl,2,2),rpf (sylnum - 4 * nsl,2,1),rpf (nsl,2,2),rpf (nsl,2,5),rpf (nsl,2,31),rpf (nsl,2,33),rpf (sylnum - 4 * nsl,4,1))
      #c (rpf (sylnum - 4 * nsl,3,1),rpf (nsl,4,2),rpf (nsl,4,5),rpf (nsl,4,31),rpf (nsl,2,33),rpf (nsl,4,31),rpf (nsl,4,5),rpf (nsl,4,2),rpf (sylnum - 4 * nsl,3,1),rpf (nsl,4,2),rpf (nsl,4,5),rpf (nsl,4,31),rpf (nsl,2,33),rpf (nsl,4,31),rpf (nsl,4,5),rpf (nsl,4,2),rpf (sylnum - 4 * nsl,3,1))
      #c (rpf (nsl,4,33),rpf (nsl,4,31),rpf (nsl,4,5),rpf (nsl,4,2),rpf (sylnum - 4 * nsl,2,1),rpf (nsl,4,2),rpf (nsl,4,5),rpf (nsl,4,31),rpf (nsl,2,33),rpf (nsl,4,31),rpf (nsl,4,5),rpf (nsl,4,2),rpf (sylnum - 4 * nsl,2,1),rpf (nsl,4,2),rpf (nsl,4,5),rpf (nsl,4,31),rpf (nsl,4,33)),
      #c (rpf (sylnum - 4 * nsl,4,1),rpf (nsl,6,2),rpf (nsl,6,5),rpf (nsl,6,31),rpf (nsl,3,33),rpf (nsl,6,31),rpf (nsl,6,5),rpf (nsl,6,2),rpf (sylnum - 4 * nsl,4,1),rpf (nsl,6,2),rpf (nsl,6,5),rpf (nsl,6,31),rpf (nsl,3,33),rpf (nsl,6,31),rpf (nsl,6,5),rpf (nsl,6,2),rpf (sylnum - 4 * nsl,4,1),rpf (nsl,6,2),rpf (nsl,6,5),rpf (nsl,6,31),rpf (nsl,3,33),rpf (nsl,6,31),rpf (nsl,6,5),rpf (nsl,6,2),rpf (sylnum - 4 * nsl,4,1)),
      #c (rpf (nsl,6,33),rpf (nsl,6,31),rpf (nsl,6,5),rpf (nsl,6,2),rpf (sylnum - 4 * nsl,3,1),rpf (nsl,6,2),rpf (nsl,6,5),rpf (nsl,6,31),rpf (nsl,3,33),rpf (nsl,6,31),rpf (nsl,6,5),rpf (nsl,6,2),rpf (sylnum - 4 * nsl,3,1),rpf (nsl,6,2),rpf (nsl,6,5),rpf (nsl,6,31),rpf (nsl,3,33),rpf (nsl,6,31),rpf (nsl,6,5),rpf (nsl,6,2),rpf (sylnum - 4 * nsl,3,1),rpf (nsl,6,2),rpf (nsl,6,5),rpf (nsl,6,31),rpf (nsl,6,33)),
      #c (rpf (sylnum - 4 * nsl,5,1),rpf (nsl,8,2),rpf (nsl,8,5),rpf (nsl,8,31),rpf (nsl,4,33),rpf (nsl,8,31),rpf (nsl,8,5),rpf (nsl,8,2),rpf (sylnum - 4 * nsl,5,1),rpf (nsl,8,2),rpf (nsl,8,5),rpf (nsl,8,31),rpf (nsl,4,33),rpf (nsl,8,31),rpf (nsl,8,5),rpf (nsl,8,2),rpf (sylnum - 4 * nsl,5,1),rpf (nsl,8,2),rpf (nsl,8,5),rpf (nsl,8,31),rpf (nsl,4,33),rpf (nsl,8,31),rpf (nsl,8,5),rpf (nsl,8,2),rpf (sylnum - 4 * nsl,5,1),rpf (nsl,8,2),rpf (nsl,8,5),rpf (nsl,8,31),rpf (nsl,4,33),rpf (nsl,8,31),rpf (nsl,8,5),rpf (nsl,8,2),rpf (sylnum - 4 * nsl,5,1)),
      #c (rpf (nsl,8,33),rpf (nsl,8,31),rpf (nsl,8,5),rpf (nsl,8,2),rpf (sylnum - 4 * nsl,4,1),rpf (nsl,8,2),rpf (nsl,8,5),rpf (nsl,8,31),rpf (nsl,4,33),rpf (nsl,8,31),rpf (nsl,8,5),rpf (nsl,8,2),rpf (sylnum - 4 * nsl,4,1),rpf (nsl,8,2),rpf (nsl,8,5),rpf (nsl,8,31),rpf (nsl,4,33),rpf (nsl,8,31),rpf (nsl,8,5),rpf (nsl,8,2),rpf (sylnum - 4 * nsl,4,1),rpf (nsl,8,2),rpf (nsl,8,5),rpf (nsl,8,31),rpf (nsl,4,33),rpf (nsl,8,31),rpf (nsl,8,5),rpf (nsl,8,2),rpf (sylnum - 4 * nsl,4,1),rpf (nsl,8,2),rpf (nsl,8,5),rpf (nsl,8,31),rpf (nsl,8,33))
      #c (rpf (sylnum - 4 * nsl,3,1),rpf (nsl,2,2),rpf (nsl,2,5),rpf (nsl,2,31),rpf (nsl,1,33),rpf (nsl,2,31),rpf (nsl,2,5),rpf (nsl,2,2),rpf (sylnum - 4 * nsl,(3/2),1)),
      #c (rpf (sylnum - 4 * nsl,(3/2),1),rpf (nsl,2,2),rpf (nsl,2,5),rpf (nsl,2,31),rpf (nsl,1,33),rpf (nsl,2,31),rpf (nsl,2,5),rpf (nsl,2,2),rpf (sylnum - 4 * nsl,3,1))
      #c (rpf (sylnum - 4 * nsl,4,1),rpf (nsl,2,2),rpf (nsl,2,5),rpf (nsl,2,31),rpf (nsl,1,33),rpf (nsl,2,31),rpf (nsl,2,5),rpf (nsl,2,2),rpf (sylnum - 4 * nsl,(4/3),1)),
      #c (rpf (sylnum - 4 * nsl,(4/3),1),rpf (nsl,2,2),rpf (nsl,2,5),rpf (nsl,2,31),rpf (nsl,1,33),rpf (nsl,2,31),rpf (nsl,2,5),rpf (nsl,2,2),rpf (sylnum - 4 * nsl,4,1))
    )
  }

  population_syll_probs <- matrix (data = syllprob_vector,
                                  nrow = length (syllprob_vector) / sylnum, #number of rows to complement the number of combinations I've come up with; so if I come up with more, fix it.
                                  ncol = sylnum,
                                  byrow = TRUE
  )

  # population_syll_probs <- array(data = syllprob_vector,
  #                                dim = c(length(syllprob_vector) / sylnum,
  #                                        sylnum,

  #                                ),
  #                                dimnames =
  # )

  parameters <- list (num_timesteps = num_timesteps,
                     num_pop = num_pop,
                     pop_size = pop_size,
                     sylnum = sylnum,
                     nsl = nsl,
                     one_pop_singers = one_pop_singers,
                     pop_calls_matrix = pop_calls_matrix,
                     curiositybreaks = curiositybreaks,
                     curiosity_counter = curiosity_counter,
                     zero_to_one_template = zero_to_one_template,
                     population_syll_probs = population_syll_probs,
                     curlearnprob = curlearnprob,
                     learnprob = learnprob,
                     randlearnprob = randlearnprob,
                     stand.dev = stand.dev,
                     curinhproportion = curinhproportion
                     )

  return (parameters)
}
#Results of Function:
#Parameters <- list(
  # 1 - num_timesteps,   # 8 - curiositybreaks,         # 15- stand.dev,
  # 2 - num_pop,         # 9 - curiosity_counter,      # 16- ,
  # 3 - pop_size,        # 10- zero_to_one_template,    # 17- ,
  # 4 - sylnum,          # 11- population_syll_probs, # 18- ,
  # 5 - nsl,           # 12- curlearnprob,# 19-
  # 6 - one_pop_singers, # 13- learnprob,
  # 7 - pop_calls_matrix,# 14- randlearnprob,
#return(Parameters)

define_temp_data <- function (universal_parameters) {
  # tempCatgry = 1 (learning.pool); tempCatgry = 2 (pairing.pool)
  # if (tempCatgry ==1) {
    temp_data <- array (
      0, c (
        5, universal_parameters$sylnum + 5, universal_parameters$num_pop))
    ######   The first params$sylnum columns are learning_pool;
    ######   the last 5 are pairing_pool
  # } else {
    # temp_data <- array(0, c(5, 5, universal_parameters$num_pop))
  # }

  # REALLY WANT TO HAVE:
  #   SPLIT DATA INTO ARRAY OF 2-D SYLL DATA
  #   AND THE OTHER, AC-RELATED METRICS
  #   IS A LIST GOING TO BE THE BEST OPTION HERE?
  return (temp_data)
}

# INITIALIZING FUNCTIONS ##################################

recordvariable.initialize <- function (parameters_rvi, recsimfct, variableid) {

  timestepRecordLength <- 1000 / recsimfct

  if (variableid == 1) {
    record_variable <- array (
      0, c (2, parameters_rvi$num_pop, timestepRecordLength))
  } else if (variableid == 2) {
    record_variable <- array (
      0, c ((2 * parameters_rvi$num_pop), parameters_rvi$sylnum, timestepRecordLength))
  } else if (variableid == 3) {
    record_variable <- array (
      0, c (14, parameters_rvi$num_pop, timestepRecordLength))
      # let's make indices 13 and 14 on dimension 1... these are the measures of variance in curiosity level in both male and female subpopulations
  } else if (variableid == 4) {
    record_variable <- array (
      0, c ((2 * parameters_rvi$num_pop), (parameters_rvi$num_pop * parameters_rvi$one_pop_singers [1]),
        timestepRecordLength))
  }
  # record_variable <- list(sylrep_rowcol=array(0, c(2, P$num_pop, (P$num_timesteps/simplificationFactor))), ### rows: num_sexes, num_measurements: rowSums and colSums ### cols: num_pop ### 3rd-dim: timesteps
  #                         sylrep_dstbxn=array(0, c((2 * P$num_pop), P$sylnum, (P$num_timesteps/simplificationFactor))), ### rows: num_pop, num_sexes ### cols: sylnum ### 3rd-dim: timesteps
  #                         curity_mean_t=array(0, c(12, P$num_pop, (P$num_timesteps/simplificationFactor))), ### rows: num_sexes ### cols: num_pop ### 3rd-dim: timesteps
  #                         curity_repert=array(0, c((2 * P$num_pop), (P$num_pop * P$one_pop_singers[1]), (P$num_timesteps/simplificationFactor))) ### rows: num_sexes ### cols: num_pop, num_singers_sampled ### 3rd-dim: timesteps
  #                         )
    # Rows 1 and 2 are curiosity values for the mean of the males (row 1) and females (row 2) from each population, per timestep.
    # Row 3 covers the number of selections made by females from each population, per timestep.
    # Rows 4-9 cover the individual curiosity values recorded, regarding the individuals that source the curiosity inheritance(father (row 4) and mother (row 5));
    # The curiosity values inherited by the offspring (son (row 6) and daughter (row 7)); and the curiosity values of those killed off (dead male (row 8) and female (row 9)),
    # per timestep.
    # Then the amount of times curiosity inheritance had to run (see while loop in curiosity_learn) is recorded here
  return (record_variable)
}

#day.tuh <- recordvariable.initialize

initialize.sylrep <- function (parameters_is, population.pattern, pastrunobject_is = FALSE,
                            eqpop = TRUE, eqsex = TRUE, pastruninit_is = FALSE) {

  if (length (population.pattern) != parameters_is$num_pop) {
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
    # if (!(pastrunobject_is)) {
    #   stop ("Both pastrunobject_is and pastruninit_is need to both
    #     be engaged together... if they aren't, it won't work!")
    #   }
    # sylreps <- aperm (pastrunobject_is [, , 1 : parameters_is$sylnum], c (2, 3, 1),
    #             na.rm = TRUE) # pop_size (2), sylnum (3), num_pop (1)

    sylreps <- pastrunobject_is[[1]]

  } else {
    sylreps <- array (0, c (parameters_is$pop_size, parameters_is$sylnum, parameters_is$num_pop))

    for (i in 1 : parameters_is$num_pop) {
      sylreps [, , i] <- t (replicate (parameters_is$pop_size, rbinom (
        length (parameters_is$population_syll_probs [population.pattern [i], ]),
        size = 1, prob = parameters_is$population_syll_probs [population.pattern [i], ])))
    }
  }
  return (sylreps)
}

initialize.curiosity <- function (parameters_ic, cur.min, cur.max,
            pastrunobject_ic = FALSE, pastruninit_ic = FALSE) {

  # if (xor ((pastrunobject_ic == FALSE), (pastruninit_ic == FALSE))) {
  #   stop ("Both pastrunobject_ic and pastruninit_ic need to both
  #   be engaged together... if they aren't, it won't work!")
  # }

  warning ("These arguments must be ordered - highest level
            population, then role- singers before choosers")
  if (length (cur.min) != length (cur.max) ||
     length (cur.min) != (parameters_ic$num_pop * 2)) {
    print ("Error Log #0003: each argument needs to be a
           vector that matches the number of populations
           AND the number of sexes - Make sure the number
           of elements matches the number of starting
           curiosity values.")
    stop ("cur.max and cur.min have to be the same length.")
  }
  for (i in 1 : length (cur.min)) {
    if (parameters_ic$zero_to_one_template [cur.max [i]
    ] <= parameters_ic$zero_to_one_template [cur.min [i]] # ||
      #  cur.max[i] %% 1 != 0 ||
      #  cur.min[i] %% 1 != 0
       ) {
      stop ("maximum value needs to be bigger than minimum value.
            They need to be integers too - these are reference
            calls to zero_to_one_template- check out the values")
    }
  }
  curiosity_level <- array (0, c (parameters_ic$pop_size, parameters_ic$num_pop))
  if (pastruninit_ic) {
    # curiosity_object <- pastrunobject_ic [, , parameters_ic$sylnum + 1]
    # curiosity_level <- aperm (curiosity_object, c (2, 1), na.rm = TRUE)

    curiosity_level <- pastrunobject_ic[[2]]

  } else {
    for (pop.num in 1 : parameters_ic$num_pop) {
      for (sexes in 1 : 2) {
        curiosity_level [((
            1 + ((sexes - 1) * (parameters_ic$pop_size / 2))
          ) : (
            sexes * parameters_ic$pop_size / 2
          )), pop.num
        ] <- runif (
          parameters_ic$pop_size / 2, parameters_ic$zero_to_one_template [
            cur.min [parameters_ic$curiosity_counter [sexes, pop.num]]
          ], parameters_ic$zero_to_one_template [cur.max [parameters_ic$curiosity_counter [
            sexes, pop.num
        ]]])
      }
    }
  }
  return (curiosity_level)
}

# invasion_function <- function(sylreps, curiosity_level,
# population_s_affected) { # for now, population_s_affected
# will refer to subpopulations by their pop-sex number: if
# two pops, the numbers are 1) pop1 male, 2) pop1 female,
# 3) pop2 male, 4) pop2 female

#}
