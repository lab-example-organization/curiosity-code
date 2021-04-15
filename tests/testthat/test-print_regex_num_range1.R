source (file.path ("scripts", "Source_Reference_Section.R"))
referencesection ("testings")

script_dir <- 'scripts'
source_file <- 'Source_Heatmap_Functions.R'


print_regex_num_range("1907-2020")




source (file.path (script_dir, source_file))

# context ('curiosity') ### Use of ‘context ()’ is NO LONGER RECOMMENDED. Instead omit it, and messages will use the name of the file instead. This ensures that the context and test file name are always in sync.

test_that ('print_regex_num_range errors correctly thrown', {
  # This shouldn't throw any errors.
  expect_equal (print_regex_num_range("0-1"), "*_[0-1]_")

  #
  expect_equal (print_regex_num_range("10151-10200", "1015[1-9]_|*_101[6-9][0-9]_|*_10200_"))

  # This should throw an error since the second element is bigger than the first element.
  expect_error (print_regex_num_range("11-10"),
    'first number needs to be smaller than the second number')

#   # This should throw an error since 100 > length (zero_to_one_template).
#   expect_error (print_regex_num_range(10, 5, 100),
#     'first number needs to be smaller than the second number')
})

