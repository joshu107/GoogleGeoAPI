# Input parameters --------------------------------------------------------
context('googlegeo_api input parameters:')
test_that('Wrong input type:',  {
  expect_that(adr('a'), throws_error())
  expect_that(adr(50.4), throws_error())
  expect_that(adr(TRUE), throws_error())
})

# Output ------------------------------------------------------------------
context('googlegeo_api output:')
test_that('Wrong output:', {
  tmp_address <- adrs('This is a test 50, Test')
  tmp_coord <- coord(50.32, -15.89)

  expect_equal(class(tmp_address), 'adrs')
  expect_equal(class(tmp_coord), 'coord')
  expect_that(is.character(tmp_address$address), is_true())
  expect_that(is.double(tmp_coord$lat), is_true())
  expect_that(is.double(tmp_coord$lng), is_true())

})
