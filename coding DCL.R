# Data & Packages #####

data_paid = austri1autoBI7895[["paid"]]
data_nb = austri1autoBI7895[["nb"]]

#install.packages("DCL")
library(DCL)

# Classical chain ladder parameters #####

clm_parameter = clm(data_paid)
Plot.clm.par(clm_parameter)

# Estimation of the DCL parameters (break-down of the chain ladder parameters) #####

dcl_parameter = dcl.estimation(data_paid, data_nb, adj=1, Tables=TRUE, num.dec=4)
Plot.dcl.par(dcl_parameter, type.inflat = 'DCL')

# DCL predictions #####

# DCL predictions by diagonals (future calendar years)

dcl_prediction_diag2 = dcl.predict(dcl.par=dcl_parameter, Ntriangle=data_nb, Model=2, Tail=TRUE,
                             Tables=TRUE, summ.by="diag", num.dec=2)
dcl_prediction_diag0 = dcl.predict(dcl.par=dcl_parameter, Ntriangle=data_nb, Model=0, Tail=TRUE,
                                   Tables=TRUE, summ.by="diag", num.dec=2)

# DCL predictions by rows (underwriting period)
dcl_prediction_row2 = dcl.predict(dcl.par=dcl_parameter, Ntriangle=data_nb, Model=2, Tail=TRUE,
                             Tables=TRUE, summ.by="row", num.dec=2)
dcl_prediction_row0 = dcl.predict(dcl.par=dcl_parameter, Ntriangle=data_nb, Model=0, Tail=TRUE,
                                  Tables=TRUE, summ.by="row", num.dec=2)

# Splitting the chain ladder reserve into RBNR and IBNR claims (ignoring the tail) #####

# by diagonal
triangle_rbns_no_tail <- dcl_prediction_diag0[["Xrbns"]][, 1:18]
rbns_dcl_no_tail_diag <- numeric(17)
for (i in 20:36) {
  matching_indices <- which(row(triangle_rbns_no_tail) + col(triangle_rbns_no_tail) == i, arr.ind = TRUE)
  rbns_dcl_no_tail_diag[i-19] <- sum(triangle_rbns_no_tail[matching_indices])
}
rbns_dcl_no_tail_diag
triangle_ibnr_no_tail <- dcl_prediction_diag0[["Xibnr"]][, 1:18]
ibnr_dcl_no_tail_diag <- numeric(17)
for (i in 20:36) {
  matching_indices <- which(row(triangle_ibnr_no_tail) + col(triangle_ibnr_no_tail) == i, arr.ind = TRUE)
  ibnr_dcl_no_tail_diag[i-19] <- sum(triangle_ibnr_no_tail[matching_indices])
}
ibnr_dcl_no_tail_diag
(total_dcl_no_tail_diag = rbns_dcl_no_tail_diag + ibnr_dcl_no_tail_diag)

# by row
(rbns_dcl_no_tail_row <- rowSums(dcl_prediction_diag0[["Xrbns"]][, 1:18], na.rm=TRUE))
(ibnr_dcl_no_tail_row <- rowSums(dcl_prediction_diag0[["Xibnr"]][, 1:18], na.rm=TRUE))
(total_dcl_no_tail_row <- rbns_dcl_no_tail_row+ibnr_dcl_no_tail_row)
(sum(total_dcl_no_tail_row))

# Visualizations #####

plot(dcl_prediction_diag0[["Drbns"]][1:34], type="b", main="RBNS Reserve per Future Year", xlab="Future Year", ylab="RBNS Reserve")
plot(dcl_prediction_diag0[["Dibnr"]][1:34], type="b", main="IBNR Reserve per Future Year", xlab="Future Year", ylab="IBNR Reserve")
plot(dcl_prediction_diag0[["Dtotal"]][1:34], type="b", main="Total Reserve per Future Year", xlab="Future Year", ylab="Total Reserve")

plot(dcl_prediction_row0[["Rrbns"]][1:18], type="b", main="RBNS Reserve per Accident Year", xlab="Accident Year", ylab="RBNS Reserve")
plot(dcl_prediction_row0[["Ribnr"]][1:18], type="b", main="IBNR Reserve per Accident Year", xlab="Accident Year", ylab="IBNR Reserve")
plot(dcl_prediction_row0[["Rtotal"]][1:18], type="b", main="Total Reserve per Accident Year", xlab="Accident Year", ylab="Total Reserve")

plot(dcl_prediction_diag2[["Drbns"]][1:34], type="b", main="RBNS Reserve per Future Year (with Adjusted Delay Parameter)", xlab="Future Year", ylab="RBNS Reserve")
plot(dcl_prediction_diag2[["Dibnr"]][1:34], type="b", main="IBNR Reserve per Future Year (with Adjusted Delay Parameter)", xlab="Future Year", ylab="IBNR Reserve")
plot(dcl_prediction_diag2[["Dtotal"]][1:34], type="b", main="Total Reserve per Future Year (with Adjusted Delay Parameter)", xlab="Future Year", ylab="Total Reserve")

plot(dcl_prediction_row2[["Rrbns"]][1:18], type="b", main="RBNS Reserve per Accident Year (with Adjusted Delay Parameter)", xlab="Accident Year", ylab="RBNS Reserve")
plot(dcl_prediction_row2[["Ribnr"]][1:18], type="b", main="IBNR Reserve per Accident Year (with Adjusted Delay Parameter)", xlab="Accident Year", ylab="IBNR Reserve")
plot(dcl_prediction_row2[["Rtotal"]][1:18], type="b", main="Total Reserve per Accident Year (with Adjusted Delay Parameter)", xlab="Accident Year", ylab="Total Reserve")

matplot(cbind(outstanding_claims_paid, outstanding_claims_BF, dcl_prediction_row0[["Rtotal"]][1:18], dcl_prediction_row2[["Rtotal"]][1:18]), type = "b",
        col = c("#0072B2", "#D55E00", "#009E73", "#CC79A7"), xlab = expression(italic("Accident Year")), ylab = "Cadangan Klaim", main = expression("Estimasi Cadangan Klaim per " * italic("Accident Year")), 
        lty = c(1, 2, 3, 4), lwd = 2, pch = c(16, 17, 18, 15), cex.main = 2, cex.lab = 1.5, cex.axis = 1.5)
legend("topleft", legend = c(expression(italic("Chain Ladder")), expression(italic("Bornhuetter-Ferguson")), expression(italic("Double Chain Ladder")), expression(italic("Double Chain Ladder (adjusted)"))),
       col = c("#0072B2", "#D55E00", "#009E73", "#CC79A7"), lwd = 2, pch=c(16, 17, 18, 15), cex = 1.5, pt.cex = 1, x.intersp = 0.5, y.intersp = 0.5, text.width = 8)

CL_diag <- c(CL_diag, rep(NA, 17))
matplot(cbind(CL_diag, dcl_prediction_diag0[["Dtotal"]][1:34], dcl_prediction_diag2[["Dtotal"]][1:34]), type = "b",
        col = c("#0072B2", "#009E73", "#CC79A7"), xlab = expression(italic("Future Year")), ylab = "Cadangan Klaim", main = expression("Estimasi Cadangan Klaim per " * italic("Future Year")), 
        lty = c(1, 3, 4), lwd = 2, pch = c(16, 18, 15), cex.main = 2, cex.lab = 1.5, cex.axis = 1.5)
legend("topright", legend = c(expression(italic("Chain Ladder")), expression(italic("Double Chain Ladder")), expression(italic("Double Chain Ladder (adjusted)"))),
       col = c("#0072B2", "#009E73", "#CC79A7"), lwd = 2, pch=c(16, 18, 15), cex = 1.5, pt.cex = 1, x.intersp = 0.5, y.intersp = 0.5, text.width = 15)
lines(dcl_prediction_diag0[["Dtotal"]][1:34], type = "b", col = "#009E73", lty = 3, lwd = 2, pch = 18)
lines(dcl_prediction_diag2[["Dtotal"]][1:34], type = "b", col = "#CC79A7", lty = 4, lwd = 2, pch = 15)


# Full cashflow considering the tail (only the variance process) - bootstrap #####

dcl_distribution = dcl.boot(dcl.par=dcl_parameter, Ntriangle=data_nb, boot.type=2, B=999,
                            Tail=TRUE, summ.by="diag", Tables=TRUE, num.dec=2)
Plot.cashflow(dcl_distribution)


N_list <- list()
X_list <- list()
full_list <- list()
set.seed(1)

for (k in 1:999) {
  boot_full <- matrix(NA,18,18)
  boot_N <- matrix(NA,18,18)
  boot_X <- matrix(NA,18,18)
  
  for (i in 1:18) {
    alpha <- runif(1, min = 0.001, max = 0.008558)
    prob <- (dcl_parameter[["pj"]]+alpha)/(1+18*alpha)
    boot_full[i,] <- rmultinom(1, size = round(dcl_parameter[["alpha.N"]][i]), prob = prob)
  }
  full_list[[k]] <- boot_full
  
  for (i in 1:18){
    for (j in 1:18){
      if (i+j <= 19){
        boot_N[i,j] <- round(boot_full[i,j])
      }
    }
  }
  N_list[[k]] <- boot_N
  
  lambda <- (dcl_parameter[["mu"]])^2 / dcl_parameter[["sigma2"]]
  kappa <- dcl_parameter[["sigma2"]] / dcl_parameter[["mu"]]
  
  for (i in 1:18) {
    for (j in 1:18) {
      if (!is.na(boot_N[i,j])) {
        boot_X[i,j] <- rgamma(1, shape = kappa, scale = boot_N[i,j]*lambda)
      }
    }
  }
  X_list[[k]] <- boot_X
}

ibnr_list <- vector("list", 999)
rbns_list <- vector("list", 999)
total_list <- vector("list", 999)

for (i in 1:999){
  boot_X <- X_list[[i]]
  boot_N <- N_list[[i]]
  boot_parameter <- dcl.estimation(boot_X, boot_N, adj=1, Tables=TRUE, num.dec=4)
  boot_prediction <- dcl.predict(dcl.par=boot_parameter, Ntriangle=data_nb, Model=2, Tail=TRUE,
                                 Tables=TRUE, summ.by="row", num.dec=2)
  boot_ibnr <- boot_prediction[["Ribnr"]][1:18]
  ibnr_list[[i]] <- boot_ibnr
  boot_rbns <- boot_prediction[["Rrbns"]][1:18]
  rbns_list[[i]] <- boot_rbns
  boot_total <- boot_prediction[["Rtotal"]][1:18]
  total_list[[i]] <- boot_total
}

array <- array(unlist(total_list), dim = c(18,1,999))
(mean_DCL_ay <- apply(array, 1, mean))
(mean_DCL <- sum(mean_DCL_ay))
(sd_DCL_ay <- apply(array, 1, sd))
(sd_DCL <- sum(sd_DCL_ay))
(quantile_DCL_ay <- apply(array, 1, quantile, probs = c(0.75, 0.95)))


# ibnr_list <- vector("list", 999)
# rbns_list <- vector("list", 999)
# total_list <- vector("list", 999)
# 
# for (i in 1:999){
#   pseudo <- pseudo_list[[i]]
#   boot_parameter <- dcl.estimation(pseudo, data_nb, adj=1, Tables=TRUE, num.dec=4)
#   boot_prediction <- dcl.predict(dcl.par=boot_parameter, Ntriangle=data_nb, Model=2, Tail=TRUE,
#                                  Tables=TRUE, summ.by="row", num.dec=2)
#   boot_ibnr <- boot_prediction[["Ribnr"]][1:18]
#   ibnr_list[[i]] <- boot_ibnr
#   boot_rbns <- boot_prediction[["Rrbns"]][1:18]
#   rbns_list[[i]] <- boot_rbns
#   boot_total <- boot_prediction[["Rtotal"]][1:18]
#   total_list[[i]] <- boot_total
# }
# 
# array <- array(unlist(total_list), dim = c(18,1,999))
# (mean_DCL_ay <- apply(array, 1, mean))
# (mean_DCL <- sum(mean_DCL_ay))
# (sd_DCL_ay <- apply(array, 1, sd))
# (sd_DCL <- sum(sd_DCL_ay))
# (quantile_DCL_ay <- apply(array, 1, quantile, probs = c(0.75, 0.95)))