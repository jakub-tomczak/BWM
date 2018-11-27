#' calculateWeights
#'
#' Implementation based on https://doi.org/10.1016/j.omega.2015.12.001.
#' Calculates weights of the criteria using a linear model.
#' Steps:
#' 1. Build model (consists of validating model and constructing necessary constraints for LP problem).
#' 2. Solve LP problem.
#' 3. Calculate consistency ratio.
#'
#' @name calculateWeights
#' @param criteriaNames Names of the criteria
#' @param bestToOthers Vector of pairwise comparisons. Best criterion should be 1, others <2, 9>.
#' @param othersToWorst Vector of pairwise comparisons. Worst criterion should be 1, others <2, 9>.
#' @return Result that consist of \code{criteriaNames}, \code{criteriaWeights}, \code{consistencyRatio} and a model that was used to calculate weights.
#' @examples
#' criteriaNames <- c("quality", "price", "comfort", "safety", "style")
#' bestToOthers <- c(2, 1, 4, 2, 8)
#' othersToWorst <- c(4, 8, 2, 4, 1)
#' calculateWeights(criteriaNames, bestToOthers, othersToWorst)
#' @import Rglpk
#' @export
calculateWeights <- function(criteriaNames, bestToOthers, othersToWorst){
  model <- buildModel(bestToOthers, othersToWorst, criteriaNames)
  #const values that are listed in the article
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
