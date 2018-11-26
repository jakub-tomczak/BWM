context("test-example-data")

test_that("calculate properly consistent comparisons", {
  expected_weights <- c(0.21052632, 0.42105263, 0.10526316, 0.21052632, 0.05263158)
  expected_consistency_ratio <- 0.0
  criteriaNames <- c("quality", "price", "comfort", "safety", "style")

  bestToOthers <- c(2, 1, 4, 2, 8)
  worstToOthers <- c(4, 8, 2, 4, 1)
  solution <- calculateWeights(criteriaNames, bestToOthers, worstToOthers)
  expect_equal(expected_weights, solution$result$criteriaWeights, tolerance=1e-8)
  expect_equal(expected_consistency_ratio, solution$result$consistencyRatio, tolerance=1e-8)
})

test_that("calculate properly inconsistent comparisons", {
  expected_weights <- c(0.22950820, 0.44808743, 0.11475410, 0.15300546, 0.05464481)
  expected_consistency_ratio <- 0.002444958
  criteriaNames <- c("quality", "price", "comfort", "safety", "style")

  bestToOthers <- c(2, 1, 4, 3, 8)
  worstToOthers <- c(4, 8, 2, 3, 1)
  solution <- calculateWeights(criteriaNames, bestToOthers, worstToOthers)
  expect_equal(expected_weights, solution$result$criteriaWeights, tolerance=1e-8)
  expect_equal(expected_consistency_ratio, solution$result$consistencyRatio, tolerance=1e-8)
})
