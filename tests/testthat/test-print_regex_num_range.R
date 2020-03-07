
script_dir <- 'scripts'
source_file <- 'Source_Initial_Functions_Parameters.R'

source(file.path('..', '..', script_dir, source_file))
context('curiosity')

test_that('print_regex_num_range errors correctly thrown', {
  # This shouldn't throw any errors.
  expecct_equal(print_regex_num_range("0-1"))

  # This should throw an error since the second element is bigger than the first element.
  expect_error(print_regex_num_range("11-10"),
    'first number needs to be smaller than the second number')

#   # This should throw an error since 100 > length(zero_to_one_template).
#   expect_error(print_regex_num_range(10, 5, 100),
#     'first number needs to be smaller than the second number')
})


