# Data ####

data_paid <- austri1autoBI7895[["paid"]]
data_nb <- austri1autoBI7895[["nb"]]

# Functions for BF #####

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

# Function to find the reported cumulative claims
reported_claims <- function(x) {
  return(tail(na.omit(x), 1))
}

# Function to calculate CDF to ultimate
CDF <- function(factors){
  n <- length(factors)+1
  CDF <- numeric(n)
  CDF[1] <- factors[n-1]
  for (j in 2:n){
    CDF[j] <- CDF[j-1]*factors[n+1-j]
  }
  return(CDF)
}

# BF #####

# Convert incremental run-off triangle to cumulative run-off triangle
(cumulative_triangle_paid <- incremental_to_cumulative(data_paid))

# Calculate the development factors
(factors_paid <- development_factors(cumulative_triangle_paid))

# Calculate reported cumulative claims for each accident year
(reported_claims_paid <- apply(cumulative_triangle_paid, 1, reported_claims))

# Calculate CDF to ultimate
(CDF_paid <- CDF(factors_paid))

# Calculate ultimate claims for each accident year
(ultimate_claims_BF <- reported_claims_paid*CDF_paid)

# Calculate outstanding claims
(outstanding_claims_BF <- ultimate_claims_BF - reported_claims_paid)

# Calculate IBNR reserves
(ibnr_paid <- sum(outstanding_claims_BF))

# Visualization #####

plot(ibnr_paid, type="b", main="Outstanding Claims per Accident Year (Bornhuetter-Ferguson Method)", xlab="Accident Year", ylab="Outstanding Claims")

# Bootstrapping #####

outstanding_claims_list <- vector("list", 999)
reported_claims_list <- vector("list", 999)
ultimate_claims_list <- vector("list", 999)

for (i in 1:999){
  pseudo <- pseudo_list[[i]]
  boot_cumulative_triangle <- incremental_to_cumulative(pseudo)
  boot_factors <- development_factors(boot_cumulative_triangle)
  boot_reported_claims <- apply(boot_cumulative_triangle, 1, reported_claims)
  boot_CDF <- CDF(boot_factors)
  boot_ultimate_claims <- boot_reported_claims*boot_CDF
  ultimate_claims_list[[i]] <- boot_ultimate_claims
  boot_reported_claims <- apply(boot_cumulative_triangle, 1, reported_claims)
  reported_claims_list[[i]] <- boot_reported_claims
  boot_outstanding_claims <- boot_ultimate_claims - boot_reported_claims
  outstanding_claims_list[[i]] <- boot_outstanding_claims
}

array <- array(unlist(outstanding_claims_list), dim = c(18,1,999))
array2 <- array(unlist(reported_claims_list), dim = c(18,1,999))
array3 <- array(unlist(ultimate_claims_list), dim = c(18,1,999))

(mean_BF_ay <- apply(array, 1, mean))
(mean_BF <- sum(mean_BF_ay))
(sd_BF_ay <- apply(array, 1, sd))
(sd_BF <- sum(sd_BF_ay))
(quantile_BF_ay <- apply(array, 1, quantile, probs = c(0.75, 0.95)))

(RC_BF_ay <- apply(array2, 1, mean))
(RC_BF <- sum(RC_BF_ay))
(UC_BF_ay <- apply(array3, 1, mean))
(UC_BF <- sum(UC_BF_ay))