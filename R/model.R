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

# multiply constraint by a scalar
scaleConstraint <- function(constraint, scaleFactor){
  assert(is.list(constraint) && !is.null(constraint$lhs) && !is.null(constraint$dir) && !is.null(constraint$rhs),
         "Constraint parameter should be a list of lhs, dir, rhs.")

  scaled <- list(
    lhs = constraint$lhs * scaleFactor,
    dir = constraint$dir,
    rhs = constraint$rhs * scaleFactor)
  if (scaleFactor < 0) {
    scaled$dir <- ifelse(scaled$dir == "<=", ">=", ifelse(scaled$dir == ">=", "<=", "=="))
  }
  scaled
}

# creates constraints, for each j, for w_b - a_bj*w_j or for w_j-a_jw*w_w
# first equation referes to the best-to-others vector, the second one to the others-to-worst vector
createBaseModelConstraints <- function(model, constraints, vectorType, dir, rhs, zetaIndexValue = 0){
  assert(vectorType %in% c("best", "worst"), "vectorType should be either 'best' or 'worst'.")
  vector <- if(vectorType == "best") model$bestToOthers else model$worstToOthers
  # +1 means zeta index
  numberOfVariables <- length(vector) + 1
  numberOfCriteria <- length(vector)
  # weight that has a number 1 on its index in the vector
  # should be ommited
  weightWithOneIndex <- match(1, vector)

  # number of added constraints is
  # useful for creating constraints opposite to these ones
  numberOfAddedConstraints <-0

  for(j in seq(numberOfCriteria)){
    if(j != weightWithOneIndex){
      lhs <- rep(0, numberOfVariables)

      if(vectorType == "best"){
        # add w_b - a_bj*w_j = 0
        lhs[weightWithOneIndex] <- 1
        lhs[j] <- -vector[j]
      } else {
        # add w_j - a_jw*w_w = 0
        lhs[weightWithOneIndex] <- -vector[j]
        lhs[j] <- 1
      }

      lhs[model$zetaIndex] <- zetaIndexValue
      result <- combineConstraints(constraints, list(lhs = lhs, dir = dir, rhs = rhs))
      if(result$added){
        constraints <- result$constraints
        numberOfAddedConstraints <- numberOfAddedConstraints + 1
      }
    }
  }
  list(constraints = constraints, addedNumber = numberOfAddedConstraints)
}

#' @export
buildModel <- function(bestToOthers, worstToOthers, alternatives){
  model <- validateData(bestToOthers, worstToOthers, alternatives)
  # n variables for weights, 1 for zeta index
  numberOfVariables <- length(bestToOthers) + 1

  numberOfCriteria <- length(bestToOthers)

  # zeta index
  model$zetaIndex <- numberOfVariables

  lhs <- rep(0, numberOfVariables)
  # sum up all weights to 1
  lhs[1:length(lhs)-1] <- 1
  dir <- "=="
  rhs <- 1

  constraints <- list()
  constraints <- combineConstraints(constraints, list(lhs = lhs, dir = dir, rhs = rhs))$constraints
  #all weights must be >= 0
  for(j in seq(numberOfCriteria)){
    lhs <- rep(0, numberOfVariables)
    lhs[j] <- 1
    constraints <- combineConstraints(constraints, list(lhs = lhs, direction = ">=", rhs = 0))$constraints
  }

  model$isConsistent <- isConsistent(model)

  if(model$isConsistent){
    #add best-to-others constraints
    result <- createBaseModelConstraints(model, constraints, vectorType = "best", dir = "==", rhs = 0)
    if(result$addedNumber > 0){
      constraints <- result$constraints
    }
  } else {
    #add constraints for a inconsistent comparisons

    #add best-to-others constraints
    result <- createBaseModelConstraints(model, constraints, vectorType = "best", dir = "<=", rhs = 0, zetaIndexValue = -1)
    if(result$addedNumber > 0){
      constraints <- result$constraints
      #add constraints that arise from removing abs value
      constraintsToScale <- tail(constraints, n = result$addedNumber) #weight with 1 is not taken into consideration
      lapply(constraintsToScale, function(x){
        constraints <<- combineConstraints(constraints, scaleConstraint(x, -1))$constraints # '<<-' refers to outer scope
      })
    }

    #add others-to-worst constraints
    result <- createBaseModelConstraints(model, constraints, vectorType = "worst", dir = "<=", rhs = 0, zetaIndexValue = -1)
    if(result$addedNumber > 0){
      constraints <- result$constraints
      #add constraints that arise from removing abs value
      constraintsToScale <- tail(constraints, n=result$addedNumber) #weight with 1 is not taken into consideration
      lapply(constraintsToScale, function(x){
        constraints <<- combineConstraints(constraints, scaleConstraint(x, -1))$constraints # '<<-' refers to outer scope
      })
    }
  }

  model$constraints = list()
  #format constraints
  model$constraints$lhs <- t(sapply(constraints, function(x){
    x$lhs
  }))
  model$constraints$dir <- sapply(constraints, function(x){
    x$dir
  })
  model$constraints$rhs <- sapply(constraints, function(x){
    x$rhs
  })


  model$objective = rep(0, numberOfVariables)
  model$objective[model$zetaIndex] <- 1
  #minimize objective's value
  model$maximize <- FALSE

  model
}
