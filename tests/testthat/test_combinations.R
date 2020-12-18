context("Partvars/partbatch combinations")

var_set1 <- c("var1", "var2", "var3")
var_set2 <- c("var1:var2", "var3")

test_that("predictor combinations are correct", {
  expect_equal(length(make_combs(
    partvars = var_set1,
    partbatch = NULL, max_level = NULL
  )), 7)
  expect_equal(length(unlist(make_combs(
    partvars = var_set1,
    partbatch = NULL, max_level = NULL
  ))), 12)
  expect_equal(length(unique(unlist(make_combs(
    partvars = var_set1,
    partbatch = NULL, max_level = NULL
  )))), 3)
  expect_equal(length(make_combs(
    partvars = var_set1,
    partbatch = NULL, max_level = 2
  )), 6)


  expect_equal(length(make_combs(
    partvars = var_set2,
    partbatch = NULL, max_level = NULL
  )), 3)
})

batch1 <- list(b1 = c("var1", "var2"), b2 = c("var4", "var5"))
test_that("predictor combinations with partbatch are correct", {
  expect_equal(length(make_combs(
    partvars = NULL,
    partbatch = batch1, max_level = NULL
  )), 3)
  expect_equal(length(unlist(make_combs(
    partvars = NULL,
    partbatch = batch1, max_level = NULL
  ))), 8)
})
