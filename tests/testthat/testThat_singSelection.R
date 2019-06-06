context('sing selection')

script_dir <- 'scripts'
source_file <- 'Source_Life_Cycle_Functions.R'
source(file.path('..', '..', script_dir, source_file))

test_that('rep.frac errors correctly thrown', {
  # This shouldn't throw any errors.
  expect_silent(rep.frac(1, 1, 1))

  # This should throw an error since 5 % 6 = 1.
  expect_error(rep.frac(5, 6, 1),
    'first element must be divisible by the second element')

  # This should throw an error since 100 > length(zero_to_one_template).
  expect_error(rep.frac(10, 5, 100),
    'in order to work, value_entered must be contained within zero_to_one_template')
})
