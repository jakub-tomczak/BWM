validateData <- function(bestToOthers, worstToOthers, alternatives){
  assert(length(bestToOthers) > 1, "Length of the best-to-others or worst-to-others vector should have at least 2 elements.")
  assert(length(bestToOthers) == length(worstToOthers), "Lengths of best-to-others and others-to-worst vectors must be the same.")
  assert(ncol(alternatives) == length(bestToOthers), "Number of criteria over alternatives must match size of the best-to-others vector.")
  bestToOthersOneIndex <- match(1, bestToOthers)
  worstToOthersOneIndex <- match(1, worstToOthers)
  assert(!is.na(bestToOthersOneIndex) && !is.na(worstToOthersOneIndex), "bestToOthers and worstToOthers vectors must contain number `1`.")
  list(bestToOthers = bestToOthers, worstToOthers = worstToOthers, alternatives = alternatives)
}

isConsistent <- function(model){
  worstCriterionIndex <- match(1, model$worstToOthers)
  bestOverWorstPreferenceValue <- model$bestToOthers[worstCriterionIndex]

  # a_bj x a_jw = a_bw for all j
  all(bestToOthers*worstToOthers == bestOverWorstPreferenceValue)
}
# tries to combine constraint, if constraint already belongs to the constraints set then
# it resturns constraints and a flag that indicates that constraints' state hasn't been changed
combineConstraints <- function(constraints, constraint){
  index <- length(constraints)+1
  #return when such constraint is already in constraints list
  for(x in constraints){
    if( length(setdiff(x, constraint)) == 0 ){
      return(list(constraints = constraints, added = FALSE))
    }
  }

  constraints[[index]] <- constraint
  list(constraints = constraints, added = TRUE)
}

# complementary constraint that should be added in case of abs
absConstraint <- function(constraint){
  lhs <- constraint$lhs
  lhs[length(lhs)] <- lhs[length(lhs)] * -1
  abs <- list(lhs = lhs,
              dir = ifelse(constraint$dir == "<=", ">=", ifelse(constraint$dir == ">=", "<=", "==")),
              rhs = constraint$rhs * (-1))
}

# creates constraints, for each j, for w_b - a_bj*w_j or for w_j-a_jw*w_w
# first equation referes to the best-to-others vector, the second one to the others-to-worst vector
createBaseModelConstraints <- function(model, constraints, vectorType, modelType, dir, rhs = 0, ksiIndexValue = 0){
  assert(vectorType %in% c("best", "worst"), "vectorType should be either 'best' or 'worst'.")
  assert(modelType %in% c("inconsistent_final", "inconsistent_auxiliary"), "methodType should be either 'inconsistent_final' or 'inconsistent_auxiliary'.")
  vector <- if(vectorType == "best") model$bestToOthers else model$worstToOthers

  # weight that has a number 1 on its index in the vector
  # should be ommited
  weightWithOneIndex <- match(1, vector)

  # number of added constraints is
  # useful for creating constraints opposite to these ones
  numberOfAddedConstraints <-0

  additionalKsiValue <- ifelse(modelType == "inconsistent_final", 0, model$ksiValue)

  for(j in seq(length(vector))){
    if(j != weightWithOneIndex){
      lhs <- rep(0, length(vector) + 1)

      if(vectorType == "best"){
        # add w_b - a_bj*w_j = 0
        lhs[weightWithOneIndex] <- 1
        lhs[j] <- -vector[j]
      } else {
        # add w_j - a_jw*w_w = 0
        lhs[weightWithOneIndex] <- -vector[j]
        lhs[j] <- 1
      }


      lhs[model$ksiIndex] <- ksiIndexValue
      result <- combineConstraints(constraints, list(lhs = lhs, dir = dir, rhs = rhs))
      if(result$added){
        constraints <- result$constraints
        numberOfAddedConstraints <- numberOfAddedConstraints + 1
      }
    }
  }
  list(constraints = constraints, addedNumber = numberOfAddedConstraints)
}

#constraints for weights' sum and their minimal value (w >= 0)
buildBasicConstraints <- function(model){
  # n variables for weights, 1 for ksi index
  numberOfVariables <- length(model$bestToOthers) + 1

  lhs <- rep(0, numberOfVariables)
  # sum up all weights to 1
  lhs[1:length(lhs)-1] <- 1
  dir <- "=="
  rhs <- 1

  constraints <- list()
  constraints <- combineConstraints(constraints, list(lhs = lhs, dir = dir, rhs = rhs))$constraints
  # all weights must be >= 0
  for(j in seq(length(model$bestToOthers))){
    lhs <- rep(0, numberOfVariables)
    lhs[j] <- 1
    constraints <- combineConstraints(constraints, list(lhs = lhs, direction = ">=", rhs = 0))$constraints
  }
  constraints
}

addConstraintsFromResult <- function(constraints, result){
  if(result$addedNumber > 0){
    constraints <- result$constraints
    #add constraints that arise from removing abs
    #get all constraints that have just been added and multiply them by -1
    constraintsToScale <- tail(constraints, n=result$addedNumber)
    lapply(constraintsToScale, function(x){
      constraints <<- combineConstraints(constraints, absConstraint(x))$constraints # '<<-' refers to outer scope
    })
  }
  constraints
}

constraintsListToMatrix <- function(constraints){
  result <- list()
  #format constraints
  result$lhs <- t(sapply(constraints, function(x){
    x$lhs
  }))
  result$dir <- sapply(constraints, function(x){
    x$dir
  })
  result$rhs <- unlist(sapply(constraints, function(x){
    x$rhs
  }))
  result
}

createModelsObjective <- function(model, objectiveIndex, objectiveValue = 1){
  objective <- rep(0, length(model$bestToOthers) + 1)
  objective[objectiveIndex] <- objectiveValue
  objective
}

#' @export
buildModel <- function(bestToOthers, worstToOthers, alternatives, dontCreateMultipleOptimalSolutions = TRUE, rankBasedOnCenterOfInterval = FALSE){
  model <- validateData(bestToOthers, worstToOthers, alternatives)
  model$isConsistent <- isConsistent(model)

  # when true, calculated weights are always scalars, not intervals
  model$dontCreateMultipleOptimalSolutions = dontCreateMultipleOptimalSolutions

  # flag used in getRanking function, when creating final ranking,
  # indicates whether or not to rank by the center of intervals
  # if not, rank based on the interval weights
  model$rankBasedOnCenterOfInterval <- rankBasedOnCenterOfInterval

  #weights' sum and weights' limit value (w >= 0)
  constraints <- list()

  # ksi index
  model$ksiIndex <- length(model$bestToOthers)+1

  if(model$isConsistent){
    #add best-to-others constraints
    result <- createBaseModelConstraints(model, constraints, vectorType = "best", dir = "==")
    if(result$addedNumber > 0){
      constraints <- result$constraints
    }
  }  else {
    if(model$dontCreateMultipleOptimalSolutions){
      #add best-to-others constraints
      result <- createBaseModelConstraints(model, constraints, vectorType = "best", modelType = "inconsistent_final", dir = "<=", ksiIndexValue = -1)
      constraints <- addConstraintsFromResult(constraints, result)

      #add others-to-worst constraints
      result <- createBaseModelConstraints(model, constraints, vectorType = "worst", modelType = "inconsistent_final", dir = "<=", ksiIndexValue = -1)
      constraints <- addConstraintsFromResult(constraints, result)

      constraints_2 <- buildBasicConstraints(model)
      for(constraint in constraints_2){
        constraints <- combineConstraints(constraints, constraint)$constraints
      }

      model$constraints = constraintsListToMatrix(constraints)
      model$objective <- createModelsObjective(model, model$ksiIndex)
      #minimize objective's value
      model$maximize <- FALSE

      model$ksiValue <- solveLP(model)$optimum

      # find minimal values

      #constraints sum of weights to 1, all weights non-negative
      constraints <- buildBasicConstraints(model)

      #add best-to-others constraints
      result <- createBaseModelConstraints(model, constraints, vectorType = "best", modelType = "inconsistent_auxiliary", dir = "<=", rhs = model$ksiValue)
      constraints <- addConstraintsFromResult(constraints, result)

      #add others-to-worst constraints
      result <- createBaseModelConstraints(model, constraints, vectorType = "worst", modelType = "inconsistent_auxiliary", dir = "<=", rhs = model$ksiValue)
      constraints <- addConstraintsFromResult(constraints, result)
    } else {
      stop("Calculating weights as intervals is not implemented yet.")
    }
  }

  model$constraints = constraintsListToMatrix(constraints)
  model$objective <- createModelsObjective(model, model$ksiIndex)
  #minimize objective's value
  model$maximize <- FALSE

  model
}
