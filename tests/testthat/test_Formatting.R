# Input parameters --------------------------------------------------------
context('formatting input parameters:')
test_that('Wrong input type:',  {
  expect_that(formatting('a'), throws_error())
  expect_that(formatting(15), throws_error())
  expect_that(formatting(TRUE), throws_error())
})


# Output ------------------------------------------------------------------

context('formatting output:')
test_that('Wrong output:',  {
  tmp_address <- adrs('This is a test, Test')
  tmp_coord <- coord(15, 5)

  expect_equal(formatting(tmp_address), 'address=This+is+a+test+Test')
  expect_equal(formatting(tmp_coord), 'latlng=15,5')
})
