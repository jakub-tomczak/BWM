#' @import Rglpk
#' @export
solveProblem <- function(model){
  assert(!is.null(model), 'Model cannot be null')
  consistencyIndex <- c(0, .44, 1.0, 1.63, 2.3, 3., 3.73, 4.47, 5.23)
  if(model$isConsistent || (!model$isConsistent && !model$createMultipleOptimalSolutions)){
    #unique optimal solution
    result <- solveLP(model)
    weights <- result$solution[1:model$ksiIndex-1]
    consistencyRatio <- result$solution[model$ksiIndex] / consistencyIndex[as.integer(model$a_bw)]
  } else {
    #multi-optimality, get intervals
    nrCriteria <- length(model$bestToOthers)
    # returns list of length equal to the number of the weights
    # that contains lists of two elements - lower an upper bound
    weights <- lapply(seq(nrCriteria), function(x){
      model$objective <- createModelsObjective(model, x)

      #find lower bound
      model$maximize <- FALSE
      lowerBound <- solveLP(model)$solution[x]

      #find upper bound
      model$maximize <- TRUE
      upperBound <- solveLP(model)$solution[x]

      list(lowerBound, upperBound)
    })
    consistencyRatio <- model$ksiValue / consistencyIndex[as.integer(model$a_bw)]
  }

  #ranking <- getRanking(model, weights)
  result <- list(criteriaNames, criteriaWeights = weights, consistencyRatio = consistencyRatio)
}

getRanking <- function(model, weights){
  if(model$isConsistent || (!model$isConsistent && !model$createMultipleOptimalSolutions)){
    sorted <- sort(weights, decreasing = TRUE, index.return=TRUE)
  } else {
    if(model$rankBasedOnCenterOfInterval){
      # TODO: rank the criteria or alternatives based on the center of intervals
      stop("Ranking based on the center of intervals is not implemented")
    } else {
      #rank the criteria or alternatives based on the interval weights
      DJ_ij <- sapply(weights, function(a){
        sapply(weights, function(b){
          # a and b in numerator are exchanged, otherwise R creates transposed matrix
          ( max(0, b[[2]] - a[[1]]) - max(0, b[[1]] - a[[2]]) ) / ( (a[[2]] - a[[1]]) + (b[[2]]-b[[1]]) )
        })
      })
      P_ij <- ifelse(DJ_ij > .5, 1, 0)
      rank <- apply(P, MARGIN = 1, function(x){sum(x)})
      sorted <- sort(rank, index.return=TRUE)
    }
  }
  sorted$ix <- model$criteriaNames[sorted$ix]
  sorted
}

solveLP <- function(model){
  Rglpk_solve_LP(model$objective, model$constraints$lhs, model$constraints$dir, model$constraints$rhs, max = model$maximize)
}
