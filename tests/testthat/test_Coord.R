# Input parameters --------------------------------------------------------
context('Checking coord input parameters:')
test_that('Wrong input type:',  {
  expect_that(coord('a', 'b'), throws_error())
  expect_that(coord('a', 50.4), throws_error())
  expect_that(coord(-17.3, 'b'), throws_error())
})
test_that('Real values out of range:', {
  expect_that(coord(91, 72.01), throws_error())
  expect_that(coord(-91, 72.01), throws_error())
  expect_that(coord(14.82, 181), throws_error())
  expect_that(coord(14.82, -181), throws_error())
})
test_that('Wrong class:', {
  tmp <- coord(30.5, -82.0)
  expect_that(tmp, is_a('coord'))
})



# Output ------------------------------------------------------------------
context('Checking coord output:')
test_that('Wrong output:', {
  tmp <- coord(30.5, -82.0)
  expect_that(typeof(tmp), is_a('character'))
})
