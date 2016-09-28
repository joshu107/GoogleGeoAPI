# Input parameters --------------------------------------------------------
context('Checking adrs input parameters:')
test_that('Wrong input type:',  {
  expect_that(coord('a', 'b'), throws_error())
  expect_that(coord('a', 50.4), throws_error())
  expect_that(coord(-17.3, 'b'), throws_error())
})
test_that('Wrong class:', {
  tmp <- adrs("This is a test 50")
  expect_that(tmp, is_a('adrs'))
})

# Output ------------------------------------------------------------------

context('Checking adrs output:')
test_that('Wrong output:', {
  tmp <- adrs("This is a test")
  expect_that(typeof(tmp), is_a('character'))
})
