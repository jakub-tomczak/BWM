context("test-model")

createSimpleModel <- function(){
  criteriaNames <- c("quality", "price", "comfort", "safety", "style")
  bestToOthers <- c(2, 1, 4, 2, 8)
  othersToWorst <- c(4, 8, 2, 4, 1)
  validateData(bestToOthers, othersToWorst, criteriaNames)
}

test_that("should validate model", {
  criteriaNames <- c("quality", "price", "comfort", "safety", "style")
  bestToOthers <- c(2, 1, 4, 2, 8)
  othersToWorst <- c(4, 8, 2, 4, 1)

  #should validate
  expect_type(validateData(bestToOthers, othersToWorst, criteriaNames), typeof(list()))

  #should not validate
  bestToOthersNotValid <- c(2, 1, 4, 2, 10)
  expect_error(validateData(bestToOthersNotValid, othersToWorst, criteriaNames))
  bestToOthersNotValid <- c()
  expect_error(validateData(bestToOthersNotValid, othersToWorst, criteriaNames))
  bestToOthersNotValid <- c(2, 3, 4, 2, 9)
  expect_error(validateData(bestToOthersNotValid, othersToWorst, criteriaNames))

  criteriaNamesNotValid <- c() #too few
  expect_error(validateData(bestToOthers, othersToWorst, criteriaNamesNotValid))
  criteriaNamesNotValid <- c("a", "b", "c", "d", "e", "f") #too many
  expect_error(validateData(bestToOthers, othersToWorst, criteriaNamesNotValid))

})

test_that("should combine constraints", {
  constraints <- list()
  constraint <- list(lhs = c(1, 2, 3, 4), dir = "<=", rhs = 1)

  result <- combineConstraints(constraints, constraint)
  constraints <- result$constraints
  expect_equal(length(constraints), 1)
  expect_true(result$added)

  result <- combineConstraints(constraints, constraint)
  expect_equal(length(result$constraints), 1)
  expect_false(result$added)

  #adding a constraint without lhs or rhs should fail
  constraint <- list(dir = "<=", rhs = 1)
  expect_error(combineConstraints(constraints, constraint))
  constraint <- list(lhs = c(1, 2, 3, 4), rhs = 1)
  expect_error(combineConstraints(constraints, constraint))
  constraint <- list(lhs = c(1, 2, 3, 4), dir = "<=")
  expect_error(combineConstraints(constraints, constraint))
})

test_that("Should create base constraints", {
  model <- createSimpleModel()
  constraints <- list()

  result <- createBaseModelConstraints(model, constraints, vectorType = "worst", dir = "<=", ksiIndexValue = -1)

  expect_true(length(result$constraints) == result$numberOfAddedConstraints)
})
