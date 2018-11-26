validateData <- function(bestToOthers, othersToWorst, criteriaNames){
  assert(length(bestToOthers) > 1, "Length of the best-to-others or others-to-worst vector should have at least 2 elements.")
  assert(length(bestToOthers) == length(othersToWorst), "Lengths of best-to-others and others-to-worst vectors must be the same.")
  assert(length(bestToOthers) == length(criteriaNames), "Lengths of best-to-others and criteriaNames must be the same.")
  assert(1 %in% bestToOthers, "best-to-others vector should contain number 1.")
  assert(1 %in% othersToWorst, "others-to-worst vector should contain number 1.")
  assert(all(bestToOthers >= 1) && all(bestToOthers <= 9), "Numbers in best-to-others vector should be in range <1, 9>.")
  assert(all(othersToWorst >= 1) && all(othersToWorst <= 9), "Numbers in others-to-worst vector should be in range <1, 9>.")
  bestToOthersOneIndex <- match(1, bestToOthers)
  othersToWorstOneIndex <- match(1, othersToWorst)
  assert(!is.na(bestToOthersOneIndex) && !is.na(othersToWorstOneIndex), "best-to-others and others-to-worst vectors must contain number `1`.")
  list(bestToOthers = bestToOthers, othersToWorst = othersToWorst, criteriaNames = criteriaNames)
}

isConsistent <- function(model){
  worstCriterionIndex <- match(1, model$othersToWorst)
  bestOverWorstPreferenceValue <- model$bestToOthers[worstCriterionIndex]

  # a_bj x a_jw = a_bw for all j
  list(isConsistent = all(model$bestToOthers*model$othersToWorst == bestOverWorstPreferenceValue), a_bw = bestOverWorstPreferenceValue)
}
# tries to combine constraint, if constraint already belongs to the constraints set then
# it resturns constraints and a flag that indicates that constraints' state hasn't been changed
combineConstraints <- function(constraints, constraint){
  assert(!is.null(constraint$lhs), "Constraint should contain lhs vector")
  assert(!is.null(constraint$rhs), "Constraint should contain rhs vector")
  assert(!is.null(constraint$dir), "Constraint should contain direction sign")
  assert(constraint$dir %in% c("<=", "==", ">="), "Constraint should be one of the following `<=, ==, >=`")
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
createBaseModelConstraints <- function(model, constraints, vectorType, dir, rhs = 0, ksiIndexValue = 0){
  assert(vectorType %in% c("best", "worst"), "vectorType should be either 'best' or 'worst'.")
  vector <- if(vectorType == "best") model$bestToOthers else model$othersToWorst

  # weight that has a number 1 on its index in the vector
  # should be ommited
  weightWithOneIndex <- match(1, vector)

  # number of added constraints is
  # useful for creating constraints opposite to these ones
  numberOfAddedConstraints <-0

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
  list(constraints = constraints, numberOfAddedConstraints = numberOfAddedConstraints)
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
  if(result$numberOfAddedConstraints > 0){
    constraints <- result$constraints
    # add constraints that stem from removing abs
    # take only result$numberOfAddedConstraints constraints that has just been added (there may have been some duplicates)
    # and multiply them by -1
    constraintsToScale <- tail(constraints, n=result$numberOfAddedConstraints)
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

buildModel <- function(bestToOthers, othersToWorst, criteriaNames){
  model <- validateData(bestToOthers, othersToWorst, criteriaNames)
  consistency <- isConsistent(model)
  model$isConsistent <- consistency$isConsistent
  model$a_bw <- consistency$a_bw

  #weights' sum and weights' limit value (w >= 0)
  constraints <- buildBasicConstraints(model)

  # ksi index
  model$ksiIndex <- length(model$bestToOthers)+1

  if(model$isConsistent){
    #add best-to-others constraints
    result <- createBaseModelConstraints(model, constraints, vectorType = "best", dir = "==")
    if(result$numberOfAddedConstraints > 0){
      constraints <- result$constraints
    }
  }  else {
      #add best-to-others constraints
      result <- createBaseModelConstraints(model, constraints, vectorType = "best", dir = "<=", ksiIndexValue = -1)
      constraints <- addConstraintsFromResult(constraints, result)

      #add others-to-worst constraints
      result <- createBaseModelConstraints(model, constraints, vectorType = "worst", dir = "<=", ksiIndexValue = -1)
      constraints <- addConstraintsFromResult(constraints, result)
  }

  model$constraints = constraintsListToMatrix(constraints)
  model$objective <- createModelsObjective(model, model$ksiIndex)
  #minimize objective's value by default
  model$maximize <- FALSE

  model
}
