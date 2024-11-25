# Data & Packages #####

data_paid <- austri1autoBI7895[["paid"]]
data_nb <- austri1autoBI7895[["nb"]]

#install.packages("ChainLadder")
library(ChainLadder)
#MackChainLadder(incr2cum(data_paid, na.rm=FALSE))

# Functions for CLM #####

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

# CLM #####

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

# Calculate outstanding claims
(outstanding_claims_paid <- ultimate_claims_paid - reported_claims_paid)

# Calculate IBNR reserves
(ibnr_paid <- sum(outstanding_claims_paid))

# Other development factors #####

# Function to calculate age-to-age development factors
age_to_age_factors <- function(triangle) {
  n <- ncol(triangle)
  age_to_age_factors <- matrix(NA, nrow = nrow(triangle) - 1, ncol = n - 1)
  for (j in 1:(n - 1)) {
    for (i in 1:(nrow(triangle) - j)) {
      if (!is.na(triangle[i, j]) && !is.na(triangle[i, j + 1])) {
        age_to_age_factors[i, j] <- triangle[i, j + 1] / triangle[i, j]
      }
    }
  }
  return(age_to_age_factors)
}

# Function to calculate the average development factors
factors_average <- function(factors) {
  factors_average <- apply(factors, 2, function(x) mean(x, na.rm = TRUE))
  return(factors_average)
}

# Function to calculate the 5-year average development factors
factors_5year <- function(factors) {
  n <- ncol(factors)
  factors_5year <- numeric(n)
  for (j in 1:n) {
    non_na_factors <- na.omit(factors[, j])
    if (length(non_na_factors) >= 5) {
      valid_factors <- tail(non_na_factors, 5)
    } else {
      valid_factors <- non_na_factors
    }
    factors_5year[j] <- mean(valid_factors, na.rm = TRUE)
  }
  return(factors_5year)
}

(age_to_age_factors <- age_to_age_factors(cumulative_triangle_paid))
(factors_average <- factors_average(age_to_age_factors))
(factors_5year <- factors_5year(age_to_age_factors))

(completed_triangle_paid5 <- complete_triangle(cumulative_triangle_paid, factors_5year))
(ultimate_claims_paid5 <- completed_triangle_paid5[, ncol(completed_triangle_paid5)])
(outstanding_claims_paid5 <- ultimate_claims_paid5 - reported_claims_paid)
(ibnr_paid5 <- sum(outstanding_claims_paid5))

# Visualizations #####

# library(ggplot2)
barplot(data_paid[,1], main= expression("Besar Klaim " * italic("Development Year") * " Pertama"), xlab=expression(italic("Accident Year")), ylab="Klaim")
plot(data_paid[1,], type="b", main= expression("Besar Klaim " * italic("Accident Year") * " 1978"), xlab=expression(italic("Development Year")), ylab="Klaim")
barplot(data_nb[,1], main= expression("Frekuensi Klaim " * italic("Development Year") * " Pertama"), xlab=expression(italic("Accident Year")), ylab="Klaim")
plot(data_nb[1,], type="b", main= expression("Frekuensi Klaim " * italic("Accident Year") * " 1978"), xlab=expression(italic("Development Year")), ylab="Klaim")

matplot(cbind(factors_paid, factors_average, factors_5year), type = "l", lty = 1, 
        col = c("red", "green", "blue"), xlab = "Development Year", ylab = "Development Factors", main = "Comparison of Different Development Factors", xaxt="n")
axis(1, at=1:length(factors_paid), labels=2:(length(factors_paid)+1))
legend("topright", legend = c("mean", "average", "5-year average"), col = c("red", "green", "blue"), lty = 1, cex=.75)
plot(age_to_age_factors[,1], type="b", main="Second Development Year Age-to-age Development Factors", xlab="Accident Year", ylab="Age-to-age Development Factors")

plot(outstanding_claims_paid, type="b", main= expression(italic("Outstanding Claims") * " per " * italic("Accident Year") * " (Metode " * italic("Chain Ladder") * ")"), 
     xlab=expression(italic("Accident Year")), ylab=expression(italic("Outstanding Claims")))
incremental_CL <- cumulative_to_incremental(completed_triangle_paid)
CL_diag <- numeric(17)
for (i in 20:36) {
  matching_indices <- which(row(incremental_CL) + col(incremental_CL) == i, arr.ind = TRUE)
  CL_diag[i-19] <- sum(incremental_CL[matching_indices])
}
CL_diag
plot(CL_diag, type="b", main= expression(italic("Outstanding Claims") * " per " * italic("Future Year") * " (Metode " * italic("Chain Ladder") * ")"), 
     xlab=expression(italic("Future Year")), ylab=expression(italic("Outstanding Claims")))

# Bootstrapping #####

boot_CL_package <- BootChainLadder(cumulative_triangle_paid, R=999, process.distr = c("gamma","od.pois"), seed = 1)

# Function to inverse the triangle (fitted values)
inverse_triangle <- function(triangle, factors) {
  n <- ncol(triangle)
  inverse_triangle <- matrix(NA,n,n)
  for (i in 2:n){
    for (j in 1:n){
      if (i+j >= 20){
        inverse_triangle[i,j] <- triangle[i,j]
      }
    }
  }
  inverse_triangle[1,n] <- triangle[1,n]
  for (i in 1:n) {
    for (j in seq(n,2)) {
      if (is.na(inverse_triangle[i, j-1])) {
        inverse_triangle[i,j-1] <- inverse_triangle[i,j]/factors[j-1]
      }
    }
  }
  for (i in 1:n){
    for (j in 1:n){
      if (i+j >= 20){
        inverse_triangle[i,j] <- NA
      }
    }
  }
  return(inverse_triangle)
}
(inverse_triangle <- inverse_triangle(completed_triangle_paid, factors_paid))
  
# Function to convert cumulative triangle to incremental triangle
cumulative_to_incremental <- function(triangle) {
  incremental_triangle <- triangle
  for (i in 2:ncol(triangle)) {
    incremental_triangle[, i] <- triangle[, i] - triangle[, i - 1]
  }
  return(incremental_triangle)
}
(incremental_inverse_triangle <- cumulative_to_incremental(inverse_triangle))  

# Function to calculate Pearson residual
residual <- function(triangle, incremental_inverse_triangle) {
  residual <- triangle
  for (i in 1:nrow(triangle)) {
    for (j in 1:ncol(triangle)) {
      if (!is.na(residual[i, j])) {
        residual[i, j] <- (triangle[i, j] - incremental_inverse_triangle[i,j])/sqrt(incremental_inverse_triangle[i,j])
      }
    }
  }
  return(residual)
}
(residual <- residual(data_paid, incremental_inverse_triangle))

# Function to calculate adjusted residual
residual_adjusted <- function(triangle) {
  residual_adjusted <- triangle
  for (i in 1:nrow(triangle)) {
    for (j in 1:ncol(triangle)) {
      residual_adjusted[i,j] <- sqrt(171/(0.5*171*172-(2*171)+1))*residual_adjusted[i,j]
    }
  }
  return(residual_adjusted)
}
(residual_adjusted <- residual_adjusted(residual))

(non_na_values <- residual_adjusted[!is.na(residual_adjusted)])
residual_list <- vector("list", 999)
set.seed(1)

for (i in 1:999){
  bootstrap_sample <- sample(non_na_values, size = length(non_na_values), replace = TRUE)
  bootstrapped_residual <- residual_adjusted
  bootstrapped_residual[!is.na(residual_adjusted)] <- bootstrap_sample
  residual_list[[i]] <- bootstrapped_residual
}

pseudo_list <- vector("list", 999)
for (i in 1:999){
  bootstrapped_residual <- residual_list[[i]]
  pseudo <- bootstrapped_residual
  pseudo[!is.na(residual_adjusted)] <- bootstrapped_residual[!is.na(residual_adjusted)] * sqrt(incremental_inverse_triangle[!is.na(residual_adjusted)]) + incremental_inverse_triangle[!is.na(residual_adjusted)]
  pseudo_list[[i]] <- pseudo
}

outstanding_claims_list <- vector("list", 999)
reported_claims_list <- vector("list", 999)
ultimate_claims_list <- vector("list", 999)

for (i in 1:999){
  pseudo <- pseudo_list[[i]]
  boot_cumulative_triangle <- incremental_to_cumulative(pseudo)
  boot_factors <- development_factors(boot_cumulative_triangle)
  boot_completed_triangle <- complete_triangle(boot_cumulative_triangle, boot_factors)
  boot_ultimate_claims <- boot_completed_triangle[, ncol(boot_completed_triangle)]
  ultimate_claims_list[[i]] <- boot_ultimate_claims
  boot_reported_claims <- apply(boot_cumulative_triangle, 1, reported_claims)
  reported_claims_list[[i]] <- boot_reported_claims
  boot_outstanding_claims <- boot_ultimate_claims - boot_reported_claims
  outstanding_claims_list[[i]] <- boot_outstanding_claims
}

array <- array(unlist(outstanding_claims_list), dim = c(18,1,999))
array2 <- array(unlist(reported_claims_list), dim = c(18,1,999))
array3 <- array(unlist(ultimate_claims_list), dim = c(18,1,999))

(mean_CL_ay <- apply(array, 1, mean))
(mean_CL <- sum(mean_CL_ay))
(sd_CL_ay <- apply(array, 1, sd))
(sd_CL <- sum(sd_CL_ay))
(quantile_CL_ay <- apply(array, 1, quantile, probs = c(0.75, 0.95)))

(RC_CL_ay <- apply(array2, 1, mean))
(RC_CL <- sum(RC_CL_ay))
(UC_CL_ay <- apply(array3, 1, mean))
(UC_CL <- sum(UC_CL_ay))
