

# Set working directory
 setwd("...")
# Set a specific seed for random number generation
set.seed(43578578)

# Define logistic function
expit = plogis


# SuperLearner function
SuperLearner_function = function(Y, X, family) {
  return(SuperLearner(Y = Y, X = X, family = family, 
                      SL.library = c("SL.glm", "SL.randomForest", "SL.nnet",
                                     "SL.glm.interaction")))
}

# Psi parameters
psi1 = c(-0.5, 1)
psi2 = c(-0.5, 1)

## Counterfactual simulation to estimate true psi1 value
# n = 10000000;
# X1 = rnorm(n)
# A1opt = 1*(cbind(1, X1)%*%psi1 > 0)
# mu1_1 = (A1opt - 1)*cbind(1, X1)%*%psi1  
# mu1_0 = (A1opt - 0)*cbind(1, X1)%*%psi1  
# # # 
# X2_1 = X1 + 1 + rnorm(n)
# X2_0 = X1 + 0 + rnorm(n)
# #Simple
# Y_1 = rnorm(n, mean = X1 + X2_1 - mu1_1, 1) #Y^{1,opt}
# Y_0 = rnorm(n, mean = X1 + X2_0 - mu1_0, 1) #Y^{0,opt}
# true.psi = c(lm(Y_1 - Y_0 ~ X1)$coef, psi2)
# #true.psi = c(0.4999415, 0.9988612, -0.5, 1.0)
# #Medium
# Y_1 = rnorm(n, mean = X1 +  X2_1 + X1*X1 +  X2_1* X2_1 - mu1_1, 1) #Y^{1,opt}
# Y_0 = rnorm(n, mean = X1 + X2_0 + X1*X1 + X2_0*X2_0 - mu1_0, 1) #Y^{0,opt}
# true.psi = c(lm(Y_1 - Y_0 ~ X1)$coef, psi2)
# true.psi = c(1.497140, 2.996765, -0.5, 1.0)
# #Complex
# Y_1 = rnorm(n, mean = X1 + X2_1 - X1*X2_1 -0.1*sin(X1)-cos(sin(X1*X2_1)) + 0.5*abs(cos(X1*X2_1))  - mu1_1, 1) #Y^{1,opt}
# Y_0 = rnorm(n, mean = X1 + X2_0 - X1*X2_0 -0.1*sin(X1)-cos(sin(X1*X2_0)) + 0.5*abs(cos(X1*X2_0))  - mu1_0, 1) #Y^{0,opt}
# true.psi = c(lm(Y_1 - Y_0 ~ X1)$coef, psi2)
# #true.psi = c(0.504513694, 0.001588589, -0.5, 1.0)
 

# Function to run  for each scenarios for each size
run_scenario = function(n, scenario, k_folds) {
  # Initialize objects
    results.SL = matrix(NA, nrow = nrep, ncol = 4)
  
  for(i in 1:nrep){
    # Generate data based on the chosen scenario
    if(scenario == "simple"){
      #Stage 1 
      X1 = rnorm(n)
      A1 = rbinom(n, 1, p=expit(0.5 - X1))
      # Optimal treatment assignment stage 1 
      A1opt = 1*(cbind(1, X1)%*%psi1 > 0)
      mu1 = (A1opt - A1)*cbind(1, X1)%*%psi1
      #Stage 2
      X2 = X1 + A1 + rnorm(n)
      A2 = rbinom(n, 1, p=expit(0.5-X2))
      # Optimal treatment assignment stage 2
      A2opt = 1*(cbind(1, X2)%*%psi2 > 0)
      mu2 = (A2opt - A2)*cbind(1, X2)%*%psi2
      #Outcome generation
      Y = rnorm(n, mean = X1 + X2 - mu1 - mu2, 1)
    } else if (scenario == "medium"){
      #Stage 1
      X1 = rnorm(n)
      A1 = rbinom(n, 1, p=expit( - X1 + 0.5*X1*X1))
      # Optimal treatment assignment stage 1 
      A1opt = 1*(cbind(1, X1)%*%psi1 > 0)
      mu1 = (A1opt - A1)*cbind(1, X1)%*%psi1
      #Stage 2 
      X2 = X1 + A1 + rnorm(n)
      A2 = rbinom(n, 1, p = expit( - X2 + 0.5*X2*X2))
      # Optimal treatment assignment stage 2
      A2opt = 1*(cbind(1, X2)%*%psi2 > 0)
      mu2 = (A2opt - A2)*cbind(1, X2)%*%psi2
      #Outcome generation
      Y = rnorm(n, mean = X1 + X2 + X1*X1 + X2*X2 - mu1 - mu2, 1)
    } else if (scenario == "complex"){
      #Stage 1
      X1 = rnorm(n)
      A1 = rbinom(n, 1, p=expit(1.2*X1 - 0.25*X1*X1 - abs(X1) + 0.5*abs(sin(X1))))
      # Optimal treatment assignment stage 1 
      A1opt = 1*(cbind(1, X1)%*%psi1 > 0)
      mu1 = (A1opt - A1)*cbind(1, X1)%*%psi1
      #Stage 2
      X2 = X1 + A1 + rnorm(n)
      A2 = rbinom(n, 1, p = expit(1.2*X2 - 0.25*X2*X2 - abs(X2) + 0.5*abs(sin(X2))))
      # Optimal treatment assignment stage 2
      A2opt = 1*(cbind(1, X2)%*%psi2 > 0)
      mu2 = (A2opt - A2)*cbind(1, X2)%*%psi2
      #Outcome generation
      Y = rnorm(n, mean = X1 + X2 - X1*X2 - 0.1*sin(X1) - cos(sin(X1*X2)) + 0.5*abs(cos(X1*X2)) - mu1 - mu2, 1)
    }
    
    #Complete data  
    dat = data.frame(X1, X2, A1, A2, Y)
    
    #Data sets
    # Create a data frame H1 with a single column X1
    H1 = data.frame(X1)
    # Create a data frame H2 with columns A1, X1, and X2
    H2 = data.frame(A1, X1, X2) 
    # Create a matrix L2 with two columns: a column of ones and X2
    L2 = cbind(1, X2)
    # Generate a random sample of indices s, representing the indices of the selected rows
    
    # Generate k-fold indices
    if(k_folds != 1){
      folds = cut(sample(1:n), breaks = k_folds, labels = FALSE)
    } else{
      folds = rep(1, n)
    }

    
    for (fold in 1:k_folds) {
      
      if(k_folds != 1){
        train_indices = which(folds != fold)
        test_indices = which(folds == fold)
      } else {
        test_indices = seq(1, n, 1)
        train_indices = seq(1, n, 1)
      }
      
      
    ### dWOLS_SL
    tryCatch({
      ## Stage 2
      # Estimate treatment effect for A2 using Super Learner
      mu.t.hat1 = SuperLearner_function(Y = A2[train_indices], X = H2[train_indices,, drop = FALSE], family = "binomial")
      mu.t.hat2 = SuperLearner_function(Y = A2[test_indices], X = H2[test_indices,, drop = FALSE], family = "binomial")
      mu.t.h = dat$A2
      mu.t.h[train_indices] = predict(mu.t.hat2, type = "raw",newdata = data.frame(H2[train_indices,, drop = FALSE]))$pred
      mu.t.h[test_indices] = predict(mu.t.hat1, type = "raw",newdata = data.frame(H2[test_indices,, drop = FALSE]))$pred
      # Calculate weights
      w2 = abs(dat$A2 - mu.t.h)
      # Weighted least squares regression for Stage 2
      lm.dwols2 = lm(Y ~ X1 + X2 + A1*(1 + X1 + X2) + A2*(1 + X2), weights = w2, data = dat)
      psi2.hat.dwols = lm.dwols2$coef[c("A2", "X2:A2")]
      
      ### Stage 1
      # Estimate treatment effect for A1 using Super Learner
      mu.t.hat1 = SuperLearner_function(Y = A1[train_indices], X = H1[train_indices,, drop = FALSE], family = "binomial")
      mu.t.hat2 = SuperLearner_function(Y = A1[test_indices], X = H1[test_indices,, drop = FALSE], family = "binomial")
      mu.t.h = dat$A1
      mu.t.h[train_indices] = predict(mu.t.hat2, type = "raw",
                         newdata = data.frame(H1[train_indices,, drop = FALSE]))$pred
      mu.t.h[test_indices] = predict(mu.t.hat1, type = "raw",
                           newdata = data.frame(H1[test_indices,, drop = FALSE]))$pred
      # Calculate weights
      w1 = abs(dat$A1 - mu.t.h)
      # Optimal treatment assignment for Stage 1
      A2opt.hat = 1*(L2%*%psi2.hat.dwols > 0)
      mu2.hat.dwols = (A2opt.hat - dat$A2)*L2%*%psi2.hat.dwols
      Yopt.hat = Y + mu2.hat.dwols
      # Weighted least squares regression for Stage 1
      lm.dwols1 = lm(Yopt.hat ~ X1 + A1*(1 + X1), weights = w1, data = dat)
      psi1.hat.dwols = lm.dwols1$coef[c("A1", "X1:A1")]
      results.SL[i,] = c(psi1.hat.dwols, psi2.hat.dwols)
    }, error = function(e){})
      
    }
    
    
    print(data.frame(n, i, k_folds,time = Sys.time()))
    
    # Save results based on the scenario
    file.name = paste("TwoTimeStudy4_V2_scenario_", scenario, "_", n,"_", k_folds, "folds.Rdata", sep="")
    save( results.SL, file = file.name)
  }

}

# Number of repetitions  
nrep = 1000

# List of scenarios
scenarios = c("simple", "medium", "complex")


# Loop through scenarios, sample sizes, and folds
for (n in c(300, 1000)) {
  for (scenario in scenarios) {
    for (k_folds in 1:5) {  
      run_scenario(n, scenario, k_folds)
    }
  }
}


