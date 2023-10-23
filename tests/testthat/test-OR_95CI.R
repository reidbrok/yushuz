test_that("OR_95CI works", {
  expect_equal(OR_95CI(0,0,0.95,2), "1.00 (1.00, 1.00)")
})
