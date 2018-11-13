validateData <- function(bestToOthers, worstToOthers, alternatives){
  assert(length(bestToOthers) > 1, "Length of the best-to-others or worst-to-others vector should have at least 2 elements.")
  assert(length(bestToOthers) == length(worstToOthers), "Lengths of best-to-others and others-to-worst vectors must be the same.")
  assert(nol(alternatives) == length(bestToOthers), "Number of criteria over alternatives must match size of the best-to-others vector.")
  list(bestToOthers <- bestToOthers, worstToOthers <- worstToOthers, alternatives <- alternatives)
}

isConsistent <- function(model){
  worstIndexCriterion <- match(model$worstToOthers, 1)
  bestOverWorstPreferenceValue <- model$bestCriterion[worstCriterionIndex]

  if(length(bestToOthers) == 2)
  {
    TRUE
  } else {
    # a_bj x a_jw = a_bw for all j
    all(bestToOthers*worstToOthers == bestOverWorstPreferenceValue)
  }
}


buildModel <- function(bestToOthers, worstToOthers, alternatives){
  model <- validateData(bestToOthers, worstToOthers, alternatives)

  if(isConsistent(model)){
    #add constraints for a consistent model
  } else {
    #add constraints for a non-consistent model
  }
}
