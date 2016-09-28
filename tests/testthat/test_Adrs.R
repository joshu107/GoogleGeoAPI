# Input parameters --------------------------------------------------------
context('Checking input parameters:')
test_that('Wrong input type:',  {
  expect_that(coord('a', 'b'), throws_error())
  expect_that(coord('a', 50.4), throws_error())
  expect_that(coord(-17.3, 'b'), throws_error())
})


# Output ------------------------------------------------------------------


