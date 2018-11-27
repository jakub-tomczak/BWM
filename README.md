# BWM
This is an implementation of the Best-worst multi-criteria decision-making method. 
Current version implements only a linear model for inconsistent comparisons. 
Implementation based on https://doi.org/10.1016/j.omega.2015.12.001

# Example usage:
```R
criteriaNames <- c("quality", "price", "comfort", "safety", "style")
bestToOthers <- c(2, 1, 4, 3, 8)
worstToOthers <- c(4, 8, 4, 2, 1)

solution <- calculateWeights(criteriaNames, bestToOthers, worstToOthers)
print(solution$result$criteriaWeights)
```
