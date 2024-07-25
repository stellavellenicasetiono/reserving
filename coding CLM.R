# Function to convert incremental run-off triangle to cumulative run-off triangle
incremental_to_cumulative <- function(triangle) {
  cumulative_triangle <- triangle
  for (i in 1:nrow(triangle)) {
    for (j in 2:ncol(triangle)) {
      if (!is.na(cumulative_triangle[i, j])) {
        cumulative_triangle[i, j] <- cumulative_triangle[i, j] + cumulative_triangle[i, j - 1]
      }
    }
  }
  return(cumulative_triangle)
}

# Function to calculate the development factors
development_factors <- function(triangle) {
  n <- ncol(triangle)
  factors <- numeric(n - 1)
  for (j in 1:(n - 1)) {
    sum_numer <- 0
    sum_denom <- 0
    for (i in 1:(nrow(triangle) - j)) {
      if (!is.na(triangle[i, j]) && !is.na(triangle[i, j + 1])) {
        sum_numer <- sum_numer + triangle[i, j + 1]
        sum_denom <- sum_denom + triangle[i, j]
      }
    }
    factors[j] <- sum_numer / sum_denom
  }
  return(factors)
}

# Function to complete the run-off triangle
complete_triangle <- function(triangle, factors) {
  n <- ncol(triangle)
  completed_triangle <- triangle
  for (j in 1:(n - 1)) {
    for (i in (nrow(triangle) - j + 1):nrow(triangle)) {
      if (is.na(completed_triangle[i, j + 1])) {
        completed_triangle[i, j + 1] <- completed_triangle[i, j] * factors[j]
      }
    }
  }
  return(completed_triangle)
}

# Function to find the reported cumulative claims
reported_claims <- function(x) {
  return(tail(na.omit(x), 1))
}


# Convert incremental run-off triangle to cumulative run-off triangle
(cumulative_triangle_paid <- incremental_to_cumulative(data_paid))
(cumulative_triangle_nb <- incremental_to_cumulative(data_nb))

# Calculate the development factors
(factors_paid <- development_factors(cumulative_triangle_paid))
(factors_nb <- development_factors(cumulative_triangle_nb))

# Complete the run-off triangle
(completed_triangle_paid <- complete_triangle(cumulative_triangle_paid, factors_paid))
(completed_triangle_nb <- complete_triangle(cumulative_triangle_nb, factors_nb))

# Calculate ultimate claims for each accident year
(ultimate_claims_paid <- completed_triangle_paid[, ncol(completed_triangle_paid)])
(ultimate_claims_nb <- completed_triangle_nb[, ncol(completed_triangle_nb)])

# Calculate reported cumulative claims for each accident year
(reported_claims_paid <- apply(cumulative_triangle_paid, 1, reported_claims))
(reported_claims_nb <- apply(cumulative_triangle_nb, 1, reported_claims))

# Calculate outstanding claims
(oustanding_claims_paid <- ultimate_claims_paid - reported_claims_paid)
(oustanding_claims_nb <- ultimate_claims_nb - reported_claims_nb)

# Calculate IBNR reserves
(ibnr_paid <- sum(oustanding_claims_paid))
(ibnr_nb <- sum(oustanding_claims_nb))


# Bootstrapping
#install.packages("ChainLadder")
library(ChainLadder)
BootChainLadder(cumulative_triangle_paid, R=999, process.distr = c("gamma", "od.pois"), seed = NULL)
