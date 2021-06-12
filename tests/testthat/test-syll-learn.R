
script_dir <- 'scripts'

source_file <- 'Source_Life_Cycle_Functions.R'

source (file.path ('..', '..', script_dir, source_file))
context ('should pick neighbor')


test_that ('syll learn outputs TRUE or FALSE when working normally', {
  
  
  ##### These shouldn't throw any errors.

  # upper specified
  expect_true (syll_learn (params_SL, 
                           temp_data_SL, 
                           select_type = "mate",
                           totally_new = FALSE, 
                           randlearn_context = 1,
                           verbose = FALSE)
  ) #current_chance is between (lower * total_chances[selection_context]) and (upper * total_chances[selection_context])
    #sortSimlr[chosenBird + index] is found within repBarrier

  # unspecified upper limit
  expect_true (syll_learn (params_SL, 
                           temp_data_SL, 
                           select_type = "mate",
                           totally_new = FALSE, 
                           randlearn_context = 1,
                           verbose = FALSE)
  ) #current_chance is between (lower * total_chances[selection_context]) and (upper * total_chances[selection_context])
    #sortSimlr[chosenBird + index] is found within repBarrier
  

  ##### FALSE Results - 
  
  # neighbor is not in repBarrier
  expect_false (syll_learn (params_SL, 
                           temp_data_SL, 
                           select_type = "mate",
                           totally_new = FALSE, 
                           randlearn_context = 1,
                           verbose = FALSE)
  )

  # current chance is not within the lower and upper bounds - upper specified 
  # (the ones that let you know you've reached the appropriate level of desperation.)
  expect_false (syll_learn (params_SL, 
                           temp_data_SL, 
                           select_type = "mate",
                           totally_new = FALSE, 
                           randlearn_context = 1,
                           verbose = FALSE)
  )
  
  # current chance is not within the lower and upper bounds - upper specified 
  # (the ones that let you know you've reached the appropriate level of desperation.)
  expect_false (syll_learn (params_SL, 
                           temp_data_SL, 
                           select_type = "mate",
                           totally_new = FALSE, 
                           randlearn_context = 1,
                           verbose = FALSE)
  )
  


  # # This should throw an error since 5 % 6 = 1.
  # expect_error (rep.frac (5, 6, 1),
  #   'first element must be divisible by the second element')

  # # This should throw an error since 100 > length (zero_to_one_template).
  # expect_error (rep.frac (10, 5, 100),
  #   'in order to work, value_entered must be contained within zero_to_one_template')
})
