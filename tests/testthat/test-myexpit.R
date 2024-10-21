test_that("CI works", {
  expect_equal(OR_95CI(0,1,0.95,2), "1.00 (0.94, 1.06)")
})
