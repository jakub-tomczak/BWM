validateData <- function(bestToOthers, worstToOthers, alternatives){
  assert(length(bestToOthers) > 1, "Length of the best-to-others or worst-to-others vector should have at least 2 elements.")
  assert(length(bestToOthers) == length(worstToOthers), "Lengths of best-to-others and others-to-worst vectors must be the same.")
  assert(ncol(alternatives) == length(bestToOthers), "Number of criteria over alternatives must match size of the best-to-others vector.")
  list(bestToOthers = bestToOthers, worstToOthers = worstToOthers, alternatives = alternatives)
}

isConsistent <- function(model){
  worstCriterionIndex <- match(model$worstToOthers, 1)
  bestOverWorstPreferenceValue <- model$bestCriterion[worstCriterionIndex]

  # a_bj x a_jw = a_bw for all j
  all(bestToOthers*worstToOthers == bestOverWorstPreferenceValue)
}

combineConstraints <- function(...) {
  allConst <- list(...)

  lhs <- c()
  dir <- c()
  rhs <- c()
  variablesTypes <- c()

  for (const in allConst) {
    if (!is.null(const)) {
      lhs <- rbind(lhs, const$lhs)
      dir <- c(dir, const$dir)
      rhs <- c(rhs, const$rhs)
      #continous type variables
      variablesTypes <- c(variablesTypes, "C")
    }
  }

  return (list(lhs = lhs, dir = dir, rhs = rhs))
}

#' @export
buildModel <- function(bestToOthers, worstToOthers, alternatives){
  model <- validateData(bestToOthers, worstToOthers, alternatives)

  bestWeightIndex <- match(1, model$bestToOthers)
  worstWeightIndex <- match(max(model$worstToOthers), model$worstToOthers)

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

  constraints <- list(lhs = lhs, dir = dir, rhs = rhs)

  #all weights must be >= 0
  for(j in seq(numberOfCriteria)){
    lhs <- rep(0, numberOfVariables)
    lhs[j] <- 1
    constraints <- combineConstraints(constraints, list(lhs = lhs, direction = ">=", rhs = 0))
  }

  model$isConsistent <- isConsistent(model)

  if(model$isConsistent){

    #add best-to-others constraints
    for(j in seq(numberOfCriteria)){
      if(j != bestWeightIndex){
        # add w_b - a_bj*w_j = 0
        lhs <- rep(0, numberOfVariables)

        lhs[bestWeightIndex] <- 1
        lhs[j] <- -model$bestToOthers[j]

        constraints <- combineConstraints(constraints, list(lhs = lhs, dir="==", rhs = 0))
      }
    }

    #others-to-worst constraints are dependend on beto-to-others constraints
    #so we don't add them to the model's constraints

    model$objective = rep(0, numberOfVariables)
    model$objective[model$zetaIndex]

    #minimize objective's value
    model$maximize <- FALSE
  } else {
    #add constraints for a non-consistent model
  }

  model$constraints = constraints

  model
}
