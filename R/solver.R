#' @import Rglpk
#' @export
calculateWeights <- function(criteriaNames, bestToOthers, othersToWorst){
  model <- buildModel(bestToOthers, othersToWorst, criteriaNames)
  #const values that are listed in https://doi.org/10.1016/j.omega.2015.12.001
  consistencyIndex <- c(0, .44, 1.0, 1.63, 2.3, 3., 3.73, 4.47, 5.23)

  #unique optimal solution
  result <- solveLP(model)
  weights <- result$solution[1:model$ksiIndex-1]
  consistencyRatio <- result$solution[model$ksiIndex] / consistencyIndex[as.integer(model$a_bw)]

  result <- list(criteriaNames = criteriaNames, criteriaWeights = weights, consistencyRatio = consistencyRatio)
  list(result = result, model = model)
}

solveLP <- function(model){
  Rglpk_solve_LP(model$objective, model$constraints$lhs, model$constraints$dir, model$constraints$rhs, max = model$maximize)
}
