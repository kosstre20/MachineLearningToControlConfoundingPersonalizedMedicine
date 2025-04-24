
# Set working directory
setwd("...")
load("m_out_of_n_bootstrap_Study4_SL.Rdata")

# Number of Monte Carlo replications used in the simulation study
nrep = 200

# True values of the target parameters used for evaluating estimator performance
true.psi = c(0.504513694, 0.001588589, -0.5, 1.0)

#(Results section of the main manuscript)

round(colMeans(res.m, na.rm = TRUE),0)
round(colMeans(res.psi, na.rm = TRUE) - true.psi,2)
round(colMeans(res.beta, na.rm = TRUE),2)

# Computes the empirical coverage probability of the 95% confidence intervals for each psi parameter.
# This is calculated as the proportion of Monte Carlo samples in which the true parameter value lies 
# within the estimated confidence interval [res.ll, res.ul].
# Logical matrices are created to check whether `true.psi` is strictly between the lower (ll) and upper (ul) bounds,
# and column-wise means provide the average coverage per parameter.

colMeans(res.ll < matrix(true.psi, byrow = TRUE, nrow = nrep, ncol = 4) & 
           res.ul > matrix(true.psi, byrow = TRUE, nrow = nrep, ncol = 4),
         na.rm = TRUE)*100
