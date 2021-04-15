
script_dir <- 'scripts'

source_file <- 'Source_Life_Cycle_Functions.R'

source (file.path ('..', '..', script_dir, source_file))
context ('should pick neighbor')


test_that ('curiosityLearn outputs TRUE or FALSE when working normally', {
  
  
  ##### These shouldn't throw any errors.

  # upper specified
  expect_true (curiosity_learn (index = 1,
                                     total_chances = c (100, 100),
                                     selection_context = 1,
                                     current_chance = 47,
                                     sortSimlr = c (7,10,15,20,18, 6, 1, 5, 9,12, 
                                                    3,11,19, 8, 4, 2,14,16,13,17),
                                     repBarrier = c (1 : 10),
                                     chosenBird = 1,
                                     lower = 0.46,
                                     upper = 0.75)
  ) #current_chance is between (lower * total_chances[selection_context]) and (upper * total_chances[selection_context])
    #sortSimlr[chosenBird + index] is found within repBarrier

  # unspecified upper limit
  expect_true (curiosity_learn (index = 1,
                                     total_chances = c (100, 100),
                                     selection_context = 1,
                                     current_chance = 47,
                                     sortSimlr = c (7,10,15,20,18, 6, 1, 5, 9,12, 
                                                    3,11,19, 8, 4, 2,14,16,13,17),
                                     repBarrier = c (1 : 10),
                                     chosenBird = 1,
                                     lower = 0.46)
  ) #current_chance is between (lower * total_chances[selection_context]) and (upper * total_chances[selection_context])
    #sortSimlr[chosenBird + index] is found within repBarrier
  

  ##### FALSE Results - 
  
  # neighbor is not in repBarrier
  expect_false (curiosity_learn (index = 1,
                                     total_chances = c (100, 100),
                                     selection_context = 1,
                                     current_chance = 47,
                                     sortSimlr = c (7,10,15,20,18, 6, 1, 5, 9,12, 
                                                    3,11,19, 8, 4, 2,14,16,13,17),
                                     repBarrier = c (1 : 10),
                                     chosenBird = 3,
                                     lower = 0.46,
                                     upper = 0.75)
  )

  # current chance is not within the lower and upper bounds - upper specified 
  # (the ones that let you know you've reached the appropriate level of desperation.)
  expect_false (curiosity_learn (index = 1,
                                     total_chances = c (100, 100),
                                     selection_context = 1,
                                     current_chance = 45,
                                     sortSimlr = c (7,10,15,20,18, 6, 1, 5, 9,12, 
                                                    3,11,19, 8, 4, 2,14,16,13,17),
                                     repBarrier = c (1 : 20),
                                     chosenBird = 3,
                                     lower = 0.46,
                                     upper = 0.75)
  )
  
  # current chance is not within the lower and upper bounds - upper specified 
  # (the ones that let you know you've reached the appropriate level of desperation.)
  expect_false (curiosity_learn (index = 1,
                                     total_chances = c (100, 100),
                                     selection_context = 1,
                                     current_chance = 45,
                                     sortSimlr = c (7,10,15,20,18, 6, 1, 5, 9,12, 
                                                    3,11,19, 8, 4, 2,14,16,13,17),
                                     repBarrier = c (1 : 20),
                                     chosenBird = 3,
                                     lower = 0.46)
  )
  


  # # This should throw an error since 5 % 6 = 1.
  # expect_error (rep.frac (5, 6, 1),
  #   'first element must be divisible by the second element')

  # # This should throw an error since 100 > length (zero_to_one_template).
  # expect_error (rep.frac (10, 5, 100),
  #   'in order to work, value_entered must be contained within zero_to_one_template')
})
