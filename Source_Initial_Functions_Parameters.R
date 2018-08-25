# # # Parameter-Making Function

Define.Parameters <- function(num_timesteps, num_pop, pop_size, sylnum, nsspl, num_one.pop_singers_sampled, curlearnprob, learnprob, randlearnprob, stand.dev, curflux, new.cur.threshold){
  # Here the if-statements help organize and restrict the arguments such that the Weirdness Works(TM) :P
  if(num_pop %% 1 != 0 || pop_size %% 1 != 0 || nsspl %% 1 != 0) {
    stop("Check error log #_0002")}
  if((num_pop == 3 || num_pop == 4) && ((sylnum - 4 * nsspl) %% 2 != 0 || (nsspl) %% 2 != 0)) {
    stop("Don't be a fool; check error log #_0001")}
  if((num_pop == 5 || num_pop == 6) && ((sylnum - 4 * nsspl) %% 6 != 0 || (nsspl) %% 4 != 0)) {
    stop("Don't be a fool; check error log #_0001")}
  if((num_pop == 7 || num_pop == 8) && ((sylnum - 4 * nsspl) %% 12 != 0 || (nsspl) %% 12 != 0)) {
    stop("Don't be a fool; check error log #_0001")}
  if((num_pop == 9 || num_pop == 10) && ((sylnum - 4 * nsspl) %% 60 != 0 || (nsspl) %% 24 != 0)) {
    stop("Don't be a fool; check error log #_0001")}
  if(num_timesteps %% 1000 != 0) {stop("num_timesteps needs to be divisible by 1000. It's for recording purposes.")}
  
  pop_calls_matrix <- matrix(data = c(1 : pop_size), nrow = 2, ncol = (pop_size / 2), byrow = T)
  learning.pool <- array(0, c(5, sylnum, num_pop))
  pairing.pool <- array(0, c(5, 5, num_pop))
  
  #new.curiosity <- array(0,c(2,num_pop))
  curiositybreaks <- (0 : (num_pop * num_one.pop_singers_sampled[1])) * (1 / (num_pop * num_one.pop_singers_sampled[1]))
  curiosity_counter <- matrix(data = 1 : (num_pop * 2), nrow = 2, ncol = num_pop, byrow = F)
  zero_to_one_template <- c(0.00,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,
  #                           #1,  #2,  #3, #4,  #5, #6,  #7, #8,  #9,#10,
                            0.45,0.49,0.5,0.51,0.55,0.59,0.6,0.65,0.7,0.75,
  #                          #11, #12,#13, #14, #15, #16,#17, #18,#19, #20,
                             0.8,0.85,0.9,0.95,0.99,1.0)
  #                          #21, #22,#23, #24, #25,#26
  # Syllable probability distribution stuff; ends with reference matrix where each row defines a different pattern of syllable probability distributions
  rep.frac <- function(number_repeats, divisions_per_repeat, value_entered) {
    if(number_repeats %% divisions_per_repeat != 0) {stop("first element must be divisible by the second element")}
    if(value_entered %in% c(1 : length(zero_to_one_template)) == FALSE) {stop("in order to work, value_entered must be contained within zero_to_one_template")}
    return(replicate(c(length = number_repeats / divisions_per_repeat), zero_to_one_template[value_entered]))
  }
  if(num_pop >= 2) {
    syllprob_vector <- c(
      c(rep.frac(sylnum-4*nsspl,1,1),rep.frac(nsspl,1,2),rep.frac(nsspl,1,4),rep.frac(nsspl,1,23),rep.frac(nsspl,1,25)),
      c(rep.frac(nsspl,1,25),rep.frac(nsspl,1,23),rep.frac(nsspl,1,4),rep.frac(nsspl,1,2),rep.frac(sylnum-4*nsspl,1,1)),
      c(rep.frac(sylnum-4*nsspl,2,1),rep.frac(nsspl,2,2),rep.frac(nsspl,2,4),rep.frac(nsspl,2,23),rep.frac(nsspl,1,25),rep.frac(nsspl,2,23),rep.frac(nsspl,2,4),rep.frac(nsspl,2,2),rep.frac(sylnum-4*nsspl,2,1)),
      c(rep.frac(nsspl,2,25),rep.frac(nsspl,2,23),rep.frac(nsspl,2,4),rep.frac(nsspl,2,2),rep.frac(sylnum-4*nsspl,1,1),rep.frac(nsspl,2,2),rep.frac(nsspl,2,4),rep.frac(nsspl,2,23),rep.frac(nsspl,2,25)),
      c(rep.frac(sylnum-4*nsspl,3,1),rep.frac(nsspl,4,2),rep.frac(nsspl,4,4),rep.frac(nsspl,4,23),rep.frac(nsspl,2,25),rep.frac(nsspl,4,23),rep.frac(nsspl,4,4),rep.frac(nsspl,4,2),rep.frac(sylnum-4*nsspl,3,1),rep.frac(nsspl,4,2),rep.frac(nsspl,4,4),rep.frac(nsspl,4,23),rep.frac(nsspl,2,25),rep.frac(nsspl,4,23),rep.frac(nsspl,4,4),rep.frac(nsspl,4,2),rep.frac(sylnum-4*nsspl,3,1)),
      c(rep.frac(nsspl,4,25),rep.frac(nsspl,4,23),rep.frac(nsspl,4,4),rep.frac(nsspl,4,2),rep.frac(sylnum-4*nsspl,2,1),rep.frac(nsspl,4,2),rep.frac(nsspl,4,4),rep.frac(nsspl,4,23),rep.frac(nsspl,2,25),rep.frac(nsspl,4,23),rep.frac(nsspl,4,4),rep.frac(nsspl,4,2),rep.frac(sylnum-4*nsspl,2,1),rep.frac(nsspl,4,2),rep.frac(nsspl,4,4),rep.frac(nsspl,4,23),rep.frac(nsspl,4,25)),
      c(rep.frac(sylnum-4*nsspl,4,1),rep.frac(nsspl,6,2),rep.frac(nsspl,6,4),rep.frac(nsspl,6,23),rep.frac(nsspl,3,25),rep.frac(nsspl,6,23),rep.frac(nsspl,6,4),rep.frac(nsspl,6,2),rep.frac(sylnum-4*nsspl,4,1),rep.frac(nsspl,6,2),rep.frac(nsspl,6,4),rep.frac(nsspl,6,23),rep.frac(nsspl,3,25),rep.frac(nsspl,6,23),rep.frac(nsspl,6,4),rep.frac(nsspl,6,2),rep.frac(sylnum-4*nsspl,4,1),rep.frac(nsspl,6,2),rep.frac(nsspl,6,4),rep.frac(nsspl,6,23),rep.frac(nsspl,3,25),rep.frac(nsspl,6,23),rep.frac(nsspl,6,4),rep.frac(nsspl,6,2),rep.frac(sylnum-4*nsspl,4,1)),
      c(rep.frac(nsspl,6,25),rep.frac(nsspl,6,23),rep.frac(nsspl,6,4),rep.frac(nsspl,6,2),rep.frac(sylnum-4*nsspl,3,1),rep.frac(nsspl,6,2),rep.frac(nsspl,6,4),rep.frac(nsspl,6,23),rep.frac(nsspl,3,25),rep.frac(nsspl,6,23),rep.frac(nsspl,6,4),rep.frac(nsspl,6,2),rep.frac(sylnum-4*nsspl,3,1),rep.frac(nsspl,6,2),rep.frac(nsspl,6,4),rep.frac(nsspl,6,23),rep.frac(nsspl,3,25),rep.frac(nsspl,6,23),rep.frac(nsspl,6,4),rep.frac(nsspl,6,2),rep.frac(sylnum-4*nsspl,3,1),rep.frac(nsspl,6,2),rep.frac(nsspl,6,4),rep.frac(nsspl,6,23),rep.frac(nsspl,6,25)),
      c(rep.frac(sylnum-4*nsspl,5,1),rep.frac(nsspl,8,2),rep.frac(nsspl,8,4),rep.frac(nsspl,8,23),rep.frac(nsspl,4,25),rep.frac(nsspl,8,23),rep.frac(nsspl,8,4),rep.frac(nsspl,8,2),rep.frac(sylnum-4*nsspl,5,1),rep.frac(nsspl,8,2),rep.frac(nsspl,8,4),rep.frac(nsspl,8,23),rep.frac(nsspl,4,25),rep.frac(nsspl,8,23),rep.frac(nsspl,8,4),rep.frac(nsspl,8,2),rep.frac(sylnum-4*nsspl,5,1),rep.frac(nsspl,8,2),rep.frac(nsspl,8,4),rep.frac(nsspl,8,23),rep.frac(nsspl,4,25),rep.frac(nsspl,8,23),rep.frac(nsspl,8,4),rep.frac(nsspl,8,2),rep.frac(sylnum-4*nsspl,5,1),rep.frac(nsspl,8,2),rep.frac(nsspl,8,4),rep.frac(nsspl,8,23),rep.frac(nsspl,4,25),rep.frac(nsspl,8,23),rep.frac(nsspl,8,4),rep.frac(nsspl,8,2),rep.frac(sylnum-4*nsspl,5,1)),
      c(rep.frac(nsspl,8,25),rep.frac(nsspl,8,23),rep.frac(nsspl,8,4),rep.frac(nsspl,8,2),rep.frac(sylnum-4*nsspl,4,1),rep.frac(nsspl,8,2),rep.frac(nsspl,8,4),rep.frac(nsspl,8,23),rep.frac(nsspl,4,25),rep.frac(nsspl,8,23),rep.frac(nsspl,8,4),rep.frac(nsspl,8,2),rep.frac(sylnum-4*nsspl,4,1),rep.frac(nsspl,8,2),rep.frac(nsspl,8,4),rep.frac(nsspl,8,23),rep.frac(nsspl,4,25),rep.frac(nsspl,8,23),rep.frac(nsspl,8,4),rep.frac(nsspl,8,2),rep.frac(sylnum-4*nsspl,4,1),rep.frac(nsspl,8,2),rep.frac(nsspl,8,4),rep.frac(nsspl,8,23),rep.frac(nsspl,4,25),rep.frac(nsspl,8,23),rep.frac(nsspl,8,4),rep.frac(nsspl,8,2),rep.frac(sylnum-4*nsspl,4,1),rep.frac(nsspl,8,2),rep.frac(nsspl,8,4),rep.frac(nsspl,8,23),rep.frac(nsspl,8,25)),
      c(rep.frac(sylnum-4*nsspl,3,1),rep.frac(nsspl,2,2),rep.frac(nsspl,2,4),rep.frac(nsspl,2,23),rep.frac(nsspl,1,25),rep.frac(nsspl,2,23),rep.frac(nsspl,2,4),rep.frac(nsspl,2,2),rep.frac(sylnum-4*nsspl,(3/2),1)),
      c(rep.frac(sylnum-4*nsspl,(3/2),1),rep.frac(nsspl,2,2),rep.frac(nsspl,2,4),rep.frac(nsspl,2,23),rep.frac(nsspl,1,25),rep.frac(nsspl,2,23),rep.frac(nsspl,2,4),rep.frac(nsspl,2,2),rep.frac(sylnum-4*nsspl,3,1)),
      c(rep.frac(sylnum-4*nsspl,4,1),rep.frac(nsspl,2,2),rep.frac(nsspl,2,4),rep.frac(nsspl,2,23),rep.frac(nsspl,1,25),rep.frac(nsspl,2,23),rep.frac(nsspl,2,4),rep.frac(nsspl,2,2),rep.frac(sylnum-4*nsspl,(4/3),1)),
      c(rep.frac(sylnum-4*nsspl,(4/3),1),rep.frac(nsspl,2,2),rep.frac(nsspl,2,4),rep.frac(nsspl,2,23),rep.frac(nsspl,1,25),rep.frac(nsspl,2,23),rep.frac(nsspl,2,4),rep.frac(nsspl,2,2),rep.frac(sylnum-4*nsspl,4,1))
      
    )
  } else if(num_pop == 1) {
    syllprob_vector <- c(
      c(rep.frac(sylnum-4*nsspl,1,1),rep.frac(nsspl,1,2),rep.frac(nsspl,1,4),rep.frac(nsspl,1,23),rep.frac(nsspl,1,25))
    )
  }
  
  
  population_syll_probs <- matrix(data = syllprob_vector,
                                  nrow = length(syllprob_vector) / sylnum, #number of rows to complement the number of combinations I've come up with; so if I come up with more, fix it doofus.
                                  ncol = sylnum,
                                  byrow = TRUE
  )
  
  zero_range <- function(x, tol = .Machine$double.eps ^ 0.5) {
    if (length(x) == 1) return(TRUE)
    x <- range(x) / mean(x)
    isTRUE(all.equal(x[1], x[2], tolerance = tol))
  } # Thnx Hadley Wickham... https://stackoverflow.com/questions/4752275/test-for-equality-among-all-elements-of-a-single-vector
  
  
  Parameters <- list(num_timesteps = num_timesteps, 
                     num_pop = num_pop, 
                     pop_size = pop_size, 
                     sylnum = sylnum, 
                     nsspl = nsspl, 
                     num_one.pop_singers_sampled = num_one.pop_singers_sampled,  
                     pop_calls_matrix = pop_calls_matrix, 
                     pairing.pool = pairing.pool, 
                     #new.curiosity = new.curiosity, 
                     curiositybreaks = curiositybreaks, 
                     curiosity_counter = curiosity_counter, 
                     zero_to_one_template = zero_to_one_template, 
                     population_syll_probs = population_syll_probs,
                     curlearnprob = curlearnprob,
                     learnprob = learnprob,
                     randlearnprob = randlearnprob,
                     stand.dev = stand.dev,
                     curflux = curflux,
                     new.cur.threshold = new.cur.threshold,
                     learning.pool = learning.pool)
  
  return(Parameters)
}
#Results of Function:
  #Parameters <- list(      num_timesteps, 
                                 #num_pop, 
                                #pop_size, 
                                  #sylnum, 
                                   #nsspl, 
             #num_one.pop_singers_sampled, 
                         #curiosity_level, 
                        #pop_calls_matrix, 
                            #pairing.pool, 
                           #new.curiosity, 
                         #curiositybreaks, 
                       #curiosity_counter,
                    #zero_to_one_template, 
                   #population_syll_probs)
  #return(Parameters)

#P <- Define.Parameters(1000,1,2,400,156,24,10)
#rm(list=objects())


# INITIALIZING FUNCTIONS ##################################

recordvariable.initialize <- function(P, timestep_fraction) {
  record.variable <- list(sylrep_rowcol=array(0, c(2, P$num_pop, (P$num_timesteps/timestep_fraction))), ### rows: num_sexes, num_measurements: rowSums and colSums ### cols: num_pop ### 3rd-dim: timesteps
                          sylrep_dstbxn=array(0, c((2 * P$num_pop), P$sylnum, (P$num_timesteps/timestep_fraction))), ### rows: num_pop, num_sexes ### cols: sylnum ### 3rd-dim: timesteps
                          curity_mean_t=array(0, c(12, P$num_pop, (P$num_timesteps/timestep_fraction))), ### rows: num_sexes ### cols: num_pop ### 3rd-dim: timesteps
                          curity_repert=array(0, c((2 * P$num_pop), (P$num_pop * P$num_one.pop_singers_sampled[1]), (P$num_timesteps/timestep_fraction))) ### rows: num_sexes ### cols: num_pop, num_singers_sampled ### 3rd-dim: timesteps
                          )
    # Rows 1 and 2 are curiosity values for the mean of the males (row 1) and females (row 2) from each population, per timestep.
    # Row 3 covers the number of selections made by females from each population, per timestep.
    # Rows 4-9 cover the individual curiosity values recorded, regarding the individuals that source the curiosity inheritance(father (row 4) and mother (row 5));
    # The curiosity values inherited by the offspring (son (row 6) and daughter (row 7)); and the curiosity values of those killed off (dead male (row 8) and female (row 9)), 
    # per timestep.
    # Then the amount of times curiosity inheritance had to run (see while loop in curiosity_learn) is recorded here
  return(record.variable)
}

#day.tuh <- recordvariable.initialize

initialize.sylrep <- function(P, population.pattern, eqpop = TRUE, eqsex = TRUE) {
  
  if(length(population.pattern) != P$num_pop) {
    stop("This determines the initial syllable distributions of each subpopulation. It is a vector of row calls for population_syll_probs, so it must match the number of populations")}
  
  # making the object that will hold each instance of the function, hopefully to-be-assigned to specific variables for an instantiation of the model ¯\_(ツ)_/¯
  sylreps <- array(0, c(P$pop_size, P$sylnum, P$num_pop))
  for(i in 1:P$num_pop){
    sylreps[, , i] <- t(replicate(P$pop_size, rbinom(length(P$population_syll_probs[population.pattern[i], ]), size = 1, prob = P$population_syll_probs[population.pattern[i], ])))
  }
  return(sylreps)
}

initialize.curiosity <- function(P, cur.min, cur.max, invasion = FALSE) {
  warning("These arguments must be ordered - highest level population, next sex- singers, then choosers")
  if(length(cur.min) != length(cur.max) || length(cur.min) != (P$num_pop * 2)) {
    print("Error Log #0003: each argument needs to be a vector that matches the number of populations AND the number of sexes - Make sure the number of elements matches the number of starting curiosity values.")
    stop("cur.max and cur.min have to be the same length.")
  }
  for(i in 1:length(cur.min)) {
    if(cur.max[i] <= cur.min[i] || cur.max[i] %% 1 != 0 || cur.min[i] %% 1 != 0) {
      stop("maximum value needs to be bigger than minimum value. They need to be integers too - these are reference calls to zero_to_one_template- check out the values")
    }
  }
  curiosity_level <- array(0, c(P$pop_size, P$num_pop))
  for(pop.num in 1:P$num_pop) {
    for(sexes in 1:2) {
      curiosity_level[((1 + ((sexes - 1) * (P$pop_size/2))):(sexes * P$pop_size/2)), pop.num] <- runif(P$pop_size / 2, P$zero_to_one_template[cur.min[P$curiosity_counter[sexes, pop.num]]], P$zero_to_one_template[cur.max[P$curiosity_counter[sexes, pop.num]]])
    }
  }
  if(invasion) {
    invasion_function(curiosity_level)
  }
  return(curiosity_level)
}

#invasion_function <- function(sylreps, curiosity_level, population_s_affected) { # for now, population_s_affected will refer to subpopulations by their pop-sex number: if two pops, the numbers are 1) pop1 male, 2) pop1 female, 3) pop2 male, 4) pop2 female
  
#}