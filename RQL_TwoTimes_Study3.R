# Package installation
# install.packages("randomForest")
# install.packages("SuperLearner")
# install.packages("e1071")
# install.packages("nnet")
# install.packages("MASS")
# install.packages("earth")
# install.packages("resample")
#install.packages("openxlsx")

# Load necessary libraries
library(randomForest) # For random forest modeling
library(SuperLearner)# For SuperLearner modeling
library(e1071) # For SVM modeling
library(nnet) # For neural networks modeling
library(MASS) # For Naive Bayes modeling
library(earth) # For MARS (Multivariate Adaptive Regression Splines) modeling
library(resample)  # For resampling methods
library(openxlsx) # For Excel output

# Set working directory
setwd("C:\\Users\\kossi\\Dropbox\\RS_hormonotherapie\\Result_TwoTimes\\RQL_TwoTimes\\Study 3")

# Set a specific seed for random number generation
set.seed(43578578)

# Define logistic function
expit =plogis

# Function to calculate bias
bias = function(estimate, truth) {
  (colMeans(estimate) - truth)
}

# Function to calculate standard deviation (sigma)
sig = function(estimate){
  sqrt(colVars(estimate))
}

# Function to calculate root mean squared error (rmse)
rmse = function(estimate, truth) {
  sqrt((colMeans(estimate) - truth)^2 + colVars(estimate))
}

# SuperLearner function
SuperLearner_function =function(Y, X, family) {
  return(SuperLearner(Y = Y, X = X, family = family, 
                      SL.library = c("SL.glm", "SL.randomForest", "SL.nnet",
                                     "SL.glm.interaction", "SL.svm")))
}

# Psi parameters
psi1 =c(-0.5,1)
psi2 =c(-0.5,1)

###
## Counterfactual simulation to estimate true psi1 value
#  n =10000000;
#  X1 =runif(n, -1, 1)
#  A1opt =1*(cbind(1, X1)%*%psi1 > 0)
#  mu1_1 =(A1opt - 1)*cbind(1, X1)%*%psi1  
#  mu1_0 =(A1opt - 0)*cbind(1, X1)%*%psi1  
# # 
#  X2_1 =X1 + 1 + runif(n, -1, 1)
# X2_0 =X1 + 0 + runif(n, -1, 1)
# Y_1 =rnorm(n, mean = X1 + X2_1 - mu1_1, 1) #Y^{1,opt}
# Y_0 =rnorm(n, mean = X1 + X2_0 - mu1_0, 1) #Y^{0,opt}
# true.psi =c(lm(Y_1 - Y_0 ~ X1)$coef, psi2)
#Simple
true.psi = c(0.5007186, 0.9998634, -0.5, 1.0)
#Medium
# Y_1 =rnorm(n, mean = X1 +  X2_1 + X1*X1 +  X2_1* X2_1 - mu1_1, 1) #Y^{1,opt}
# Y_0 =rnorm(n, mean = X1 + X2_0 + X1*X1 + X2_0*X2_0 - mu1_0, 1) #Y^{0,opt}
# true.psi =c(lm(Y_1 - Y_0 ~ X1)$coef, psi2)
# true.psi = c(1.500146, 3.002304, -0.5, 1.0)
#Complex
#Y_1 =rnorm(n, mean = X1 + X2_1 - X1*X2_1 -0.1*sin(X1)-cos(sin(X1*X2_1)) + 0.5*abs(cos(X1*X2_1))  - mu1_1, 1) #Y^{1,opt}
#Y_0 =rnorm(n, mean = X1 + X2_0 - X1*X2_0 -0.1*sin(X1)-cos(sin(X1*X2_0)) + 0.5*abs(cos(X1*X2_0))  - mu1_0, 1) #Y^{0,opt}
#true.psi =c(lm(Y_1 - Y_0 ~ X1)$coef, psi2)
#true.psi = c(0.50965347, 0.02020462, -0.5, 1.0)


# Function to run  for each scenarios for each size
run_scenario =function(n, scenario) {
  # Initialize objects
  results.logit =results.RQL = matrix(NA, nrow = nrep, ncol = 4)
  
  for(i in 1:nrep){
    # Generate data based on the chosen scenario
    if(scenario == "simple"){
      #Stage 1 
      X1 = runif(n, -1, 1)
      A1 = rbinom(n, 1, p=expit(0.5-X1))
      # Optimal treatment assignment stage 1 
      A1opt = 1*(cbind(1, X1)%*%psi1 > 0)
      mu1 = (A1opt - A1)*cbind(1, X1)%*%psi1
      #Stage 2
      X2 = X1 + A1 + runif(n, -1, 1)
      A2 = rbinom(n, 1, p=expit(0.5-X2))
      # Optimal treatment assignment stage 2
      A2opt = 1*(cbind(1, X2)%*%psi2 > 0)
      mu2 = (A2opt - A2)*cbind(1, X2)%*%psi2
      #Outcome generation
      Y = rnorm(n, mean = X1+X2-mu1-mu2, 1)
    } else if (scenario == "medium"){
      #Stage 1
      X1 = runif(n, -1, 1)
      A1 = rbinom(n, 1, p=expit(-X1+0.5*X1*X1))
      # Optimal treatment assignment stage 1 
      A1opt = 1*(cbind(1, X1)%*%psi1 > 0)
      mu1 = (A1opt - A1)*cbind(1, X1)%*%psi1
      #Stage 2 
      X2 = X1 + A1 + runif(n, -1, 1)
      A2 = rbinom(n, 1, p = expit(-X2 + 0.5*X2*X2))
      # Optimal treatment assignment stage 2
      A2opt = 1*(cbind(1, X2)%*%psi2 > 0)
      mu2 = (A2opt - A2)*cbind(1, X2)%*%psi2
      #Outcome generation
      Y = rnorm(n, mean = X1 + X2 + X1*X1 + X2*X2 - mu1 - mu2, 1)
    } else if (scenario == "complex"){
      #Stage 1
      X1 = runif(n, -1, 1)
      A1 = rbinom(n, 1, p=expit(1.2*X1-0.25*X1*X1-abs(X1)+0.5*abs(sin(X1))))
      # Optimal treatment assignment stage 1 
      A1opt = 1*(cbind(1, X1)%*%psi1 > 0)
      mu1 = (A1opt - A1)*cbind(1, X1)%*%psi1
      #Stage 2
      X2 = X1 + A1 + runif(n, -1, 1)
      A2 = rbinom(n, 1, p = expit(1.2*X2-0.25*X2*X2-abs(X2)+0.5*abs(sin(X2))))
      # Optimal treatment assignment stage 2
      A2opt = 1*(cbind(1, X2)%*%psi2 > 0)
      mu2 = (A2opt - A2)*cbind(1, X2)%*%psi2
      #Outcome generation
      Y = rnorm(n, mean = X1 + X2 - X1*X2 -0.1*sin(X1)-cos(sin(X1*X2)) + 0.5*abs(cos(X1*X2)) - mu1 - mu2, 1)
      
    }
    
    #Complete data  
    dat<-data.frame(X1,X2,A1,A2,Y)
    
    #Data sets
    # Create a data frame H1 with a single column X1
    H1 = data.frame(X1)
    # Create a data frame H2 with columns A1, X1, and X2
    H2 = data.frame(A1, X1, X2) 
    # Create a matrix L2 with two columns: a column of ones and X2
    L2=cbind(1,X2)
    # Generate a random sample of indices s, representing the indices of the selected rows
    # The sample is obtained by randomly selecting n/2 indices from the range 1 to n and sorting them
    s=sort(sample(1:n,n/2))
    
    # RQL
    ## Stage 2
    mu.t.hat1 = SuperLearner_function(Y = A2[s], X = H2[s,, drop = FALSE], family = "binomial")
    mu.t.hat2 = SuperLearner_function(Y = A2[-s], X = H2[-s,, drop = FALSE], family = "binomial")
    mu.t.h = dat$A2
    mu.t.h[s] = predict(mu.t.hat2, type = "raw",newdata = data.frame(H2[s,, drop = FALSE]))$pred
    mu.t.h[-s] = predict(mu.t.hat1, type = "raw",newdata = data.frame(H2[-s,, drop = FALSE]))$pred
    mu.y.hat1 = SuperLearner_function(Y = Y[s], X = H2[s,], family = "gaussian")
    mu.y.hat2 = SuperLearner_function(Y = Y[-s], X = H2[-s,], family = "gaussian")
    mu.y.h =dat$Y
    mu.y.h[s] =predict(mu.y.hat2, type = "raw", newdata = data.frame(H2[s,, drop = FALSE]))$pred
    mu.y.h[-s] = predict(mu.y.hat1, type = "raw", newdata = data.frame(H2[-s,, drop = FALSE]))$pred
    Y.tilde=dat$Y - mu.y.h
    A.tilde =dat$A2 - mu.t.h
    X.tilde =scale(H2, scale = FALSE, center = FALSE)*A.tilde
    X.tilde1 =scale(H2, scale = FALSE)*(1 - mean(A2))
    X.tilde0 =scale(H2, scale = FALSE)*(0 - mean(A2))
    X.tilde.scale = apply(X.tilde, 2, sd)
    lm.Qp2 = lm(Y.tilde ~ A.tilde + X.tilde[,3])
    psi2.RQL =lm.Qp2$coef[-1]
    
    # Stage 1
    blip = apply(cbind(cbind(1, 0 - mu.t.h, H2[,3]*(0 - mu.t.h))%*%coef(lm.Qp2) -
                         cbind(1, A.tilde, X.tilde[,3])%*%coef(lm.Qp2),
                       cbind(1, 1 - mu.t.h, H2[,3]*(1 - mu.t.h))%*%coef(lm.Qp2) -
                         cbind(1, A.tilde, X.tilde[,3])%*%coef(lm.Qp2)),1, max)
    s2.yopt<-as.numeric(Y) + blip
    mu.t.hat1 = SuperLearner_function(Y = A1[s], X = H1[s,, drop = FALSE], family = "binomial")
    mu.t.hat2 = SuperLearner_function(Y = A1[-s], X = H1[-s,, drop = FALSE], family = "binomial")
    mu.t.h = dat$A1
    mu.t.h[s] = predict(mu.t.hat2, type = "raw",newdata = data.frame(H1[s,, drop = FALSE]))$pred
    mu.t.h[-s] = predict(mu.t.hat1, type = "raw",newdata = data.frame(H1[-s,, drop = FALSE]))$pred
    mu.y.hat1 =SuperLearner_function(Y = s2.yopt[s], X = H1[s,, drop = FALSE],family = "gaussian")
    mu.y.hat2 =SuperLearner_function(Y = s2.yopt[-s], X = H1[-s,, drop = FALSE],family = "gaussian")
    mu.y.h =dat$Y
    mu.y.h[s] =predict(mu.y.hat2, type = "raw",newdata = data.frame(H1[s,, drop = FALSE]))$pred
    mu.y.h[-s] =predict(mu.y.hat1, type = "raw", newdata = data.frame(H1[-s,, drop = FALSE]))$pred
    Y.tilde =as.numeric(s2.yopt) - as.numeric(mu.y.h)
    A.tilde =dat$A1 - mu.t.h
    X.tilde<-scale(H1, scale = FALSE, center = FALSE)*A.tilde
    lm.Qp1 =lm(Y.tilde ~ A.tilde + X.tilde)
    psi1.RQL=lm.Qp1$coef[-1]
    #Result matrix 
    results.logit[i,]<-c(psi.hat.logit.1,psi.hat.logit.2)
    print(data.frame(n, i, time = Sys.time()))
    
    # Save results based on the scenario
    file.name = paste("RQL_TwoTimeStudy1_scenario_", scenario, "_", n, ".Rdata", sep="")
    save(results.RQL, file = file.name)
    
  }
  
}

# Number of repetitions  
nrep = 1000

# List of scenarios
# For each execution, the scenario needs to be changed. "simple", "medium", "complex"
scenarios =c("simple", "medium", "complex")

# Loop through scenarios and sample sizes
for (n in c(300,10000)){
  for (scenario in scenarios)  {
    run_scenario(n, scenario)
  }
}


### Load the recorded results
setwd("C:\\Users\\kossi\\Dropbox\\RS_hormonotherapie\\Result_TwoTimes\\RQL_TwoTimes\\Study 3")
load("RQL_TwoTimeStudy3_scenario_complex_300.Rdata")

models = c("RQL")
variables_a_imprimer = list(
  RQL=results.RQL )
# 
calculate_metrics = function(results, true.psi) {
  bias_result = bias(results, true.psi)  # Bias
  sigma_result = sig(results)  # Standard Deviation (sigma)
  rmse_result = rmse(results, true.psi)  # Root Mean Squared Error (rmse)
  ratio_result = rmse_result / rmse(results.logit, true.psi)  # Ratio
  Monte_Carlo_SE_Bias= max(sigma_result/sqrt(nrep)) # Monte Carlo standard error (Bias)
  Monte_Carlo_SE_sigma= max(sigma_result/sqrt(2*(nrep-1)))# Monte Carlo standard error (Sigma)
  return(list(bias = bias_result, sigma = sigma_result, rmse = rmse_result, ratio = ratio_result,
              SE_Bias=Monte_Carlo_SE_Bias, SE_sigma=Monte_Carlo_SE_sigma))
}
# 
# # Loop through model results
for (model_name in models) {
  model_results = variables_a_imprimer[[model_name]]
  metrics = calculate_metrics(model_results, true.psi)
  print(paste(model_name, "Bias:", metrics$bias))
  print(paste(model_name, "Sigma:", metrics$sigma))
  print(paste(model_name, "RMSE:", metrics$rmse))
  print(paste(model_name, "Ratio:", metrics$ratio))
  print(paste(model_name, "SE_Bias:", metrics$SE_Bias))
  print(paste(model_name, "SE_sigma:", metrics$SE_sigma))
  cat("\n")
}

