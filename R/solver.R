#' @import Rglpk
#' @export
solveProblem <- function(model){
  assert(!is.null(model), 'Model cannot be null')
  solutionFromModel <- solveLP(model)
  weights <- getWeights(solutionFromModel, model)
  ranking <- getRanking(model, weights)

  result <- list(weights = weights, ranking = ranking['ix'], alternativesValues = ranking['x'])
}

getRanking <- function(model, weights){
  if(model$isConsistent){
    alternativesValues <- apply(model$alternatives, MARGIN = 1, function(x){
      sum(x*weights)
    })
    sort(alternativesValues, decreasing = TRUE, index.return=TRUE)
  } else {
    NULL
  }
}

solveLP <- function(model){
  Rglpk_solve_LP(model$objective, model$constraints$lhs, model$constraints$dir, model$constraints$rhs, max = model$maximize,
                            types = model$constraints$variablesType)
}

getWeights <- function(lpSolution, model) {
  if(model$isConsistent){
    lpSolution$solution[1:model$zetaIndex-1]
  } else {
    NULL
  }
}
