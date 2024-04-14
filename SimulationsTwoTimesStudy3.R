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
setwd("C:\\Users\\kossi\\Dropbox\\RS_hormonotherapie\\Result_TwoTimes\\Study 3")

# Set a specific seed for random number generation
set.seed(43578578)

# Define logistic function
expit = plogis

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
SuperLearner_function = function(Y, X, family) {
  return(SuperLearner(Y = Y, X = X, family = family, 
                      SL.library = c("SL.glm", "SL.randomForest", "SL.nnet",
                                     "SL.glm.interaction", "SL.svm")))
}

# Psi parameters
psi1 = c(-0.5,1)
psi2 = c(-0.5,1)

###
## Counterfactual simulation to estimate true psi1 value
#  n = 10000000;
#  X1 = runif(n, -1, 1)
#  A1opt = 1*(cbind(1, X1)%*%psi1 > 0)
#  mu1_1 = (A1opt - 1)*cbind(1, X1)%*%psi1  
#  mu1_0 = (A1opt - 0)*cbind(1, X1)%*%psi1  
# # 
#  X2_1 = X1 + 1 + runif(n, -1, 1)
# X2_0 = X1 + 0 + runif(n, -1, 1)
# Y_1 = rnorm(n, mean = X1 + X2_1 - mu1_1, 1) #Y^{1,opt}
# Y_0 = rnorm(n, mean = X1 + X2_0 - mu1_0, 1) #Y^{0,opt}
# true.psi = c(lm(Y_1 - Y_0 ~ X1)$coef, psi2)
#Simple
true.psi = c(0.5007186, 0.9998634, -0.5, 1.0)
#Medium
  # Y_1 = rnorm(n, mean = X1 +  X2_1 + X1*X1 +  X2_1* X2_1 - mu1_1, 1) #Y^{1,opt}
  # Y_0 = rnorm(n, mean = X1 + X2_0 + X1*X1 + X2_0*X2_0 - mu1_0, 1) #Y^{0,opt}
  # true.psi = c(lm(Y_1 - Y_0 ~ X1)$coef, psi2)
#true.psi = c(1.500146, 3.002304, -0.5, 1.0)
 #Complex
  #Y_1 = rnorm(n, mean = X1 + X2_1 - X1*X2_1 -0.1*sin(X1)-cos(sin(X1*X2_1)) + 0.5*abs(cos(X1*X2_1))  - mu1_1, 1) #Y^{1,opt}
  #Y_0 = rnorm(n, mean = X1 + X2_0 - X1*X2_0 -0.1*sin(X1)-cos(sin(X1*X2_0)) + 0.5*abs(cos(X1*X2_0))  - mu1_0, 1) #Y^{0,opt}
  #true.psi = c(lm(Y_1 - Y_0 ~ X1)$coef, psi2)
  #true.psi = c(0.50965347, 0.02020462, -0.5, 1.0)
  
  
# Function to run  for each scenarios for each size
run_scenario = function(n, scenario) {
  # Initialize objects
  results.logit = results.random = results.bayes = results.neural =
    results.SL = results.SVM =  matrix(NA, nrow = nrep, ncol = 4)
  
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
    dat=data.frame(X1,X2,A1,A2,Y)
    
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
    
    ### Logit
    ##Stage 2
    model.logit.2.train=glm(A2~ X1+X2+A1, family = "binomial",data=dat[s,] ) # Train logistic regression model for A2 based on X1, X2, and A1 using the training set
    model.logit.2.test=glm(A2~ X1+X2+A1, family = "binomial",data=dat[-s,] ) # Test logistic regression model for A2 on the test set
    pred.logit.2=dat$A2 # Initialize vector with original A2 values
    # Insert predictions for the test set and training set
    pred.logit.2[s]=predict(model.logit.2.test,type="res",dat[s,])
    pred.logit.2[-s]=predict(model.logit.2.train,type="res", dat[-s,])
    w.logit.2=abs(dat$A2-pred.logit.2) # Calculate absolute differences between A2 and predicted values
    model.res.logit.2=summary(glm(Y~X1+X2+A1*(1+X1+X2)+A2*(1+X2), weights= w.logit.2,family = gaussian,data=dat))# Fit weighted least squares model for Y using predictors X1, X2, A1*(1+X1+X2), A2*(1+X2)
    psi.hat.logit.2= model.res.logit.2$coef[c("A2", "X2:A2"),1]# Extract coefficients for A2 and interaction term X2:A2 from the model results
    ## Stage 1
    model.logit.1.train=glm(A1 ~ X1, family = "binomial",data=dat[s,] )# Train logistic regression model for A1 based on X1 using the training set
    model.logit.1.test=glm(A1 ~ X1, family = "binomial",data=dat[-s,] )# Test logistic regression model for A1 on the test set
    pred.logit.1=dat$A1 # Initialize vector with original A1 values
    # Insert predictions for the test set and training set
    pred.logit.1[s]=predict(model.logit.1.test,type="res",dat[s,])
    pred.logit.1[-s]=predict(model.logit.1.train,type="res",dat[-s,])
    w.logit.1=abs(dat$A1-pred.logit.1)# Calculate absolute differences between A1 and predicted values
    A2opt.hat.logit = 1*(L2%*%psi.hat.logit.2 > 0) # Estimate optimal assignment for A2 based on the second stage's results
    mu2.hat.logit =(A2opt.hat.logit - dat$A2)*L2%*%psi.hat.logit.2 # Calculate the treatment effect for A1
    Yopt.hat.logit.2 =dat$Y + mu2.hat.logit# Calculate the outcome with the estimated treatment effect for A1
    model.res.logit.1 = summary(glm( Yopt.hat.logit.2  ~ X1+ A1*(1 + X1), weights = w.logit.1, dat, family = gaussian))  # Fit weighted least squares model for Yopt using predictors X1, A1*(1 + X1)
    psi.hat.logit.1= model.res.logit.1$coef[c("A1", "X1:A1"),1]# Extract coefficients for A1 and interaction term X1:A1 from the model results
    
    ### Random forest
    #Stage 2
    model.random.2.train=randomForest(factor(A2)~ X1+X2+A1,  mtry=2,importance = TRUE,data=dat[s,])# Train a random forest model for predicting A2
    model.random.2.test=randomForest(factor(A2)~ X1+X2+A1,  mtry=2,importance = TRUE,data=dat[-s,])
    # Make predictions for A2 using the test and train models
    pred.random.2=dat$A2
    pred.random.2[s]=predict(model.random.2.test, type = "prob", dat[s,])[,2]
    pred.random.2[-s]=predict(model.random.2.train, type = "prob", dat[-s,])[,2]
    w.random.2=abs(dat$A2-pred.random.2)# Calculate weights based on the absolute difference between true and predicted A2
    model.res.random.2=summary(glm(Y~X1+X2+A1*(1+X1+X2)+A2*(1+X2), weights= w.random.2,family = gaussian,data=dat)) # Fit a weighted linear regression model for Y using A2 predictions and other features
    psi.hat.random.2=model.res.random.2$coef[c("A2", "X2:A2"),1]# Extract coefficients related to A2 and interaction term X2:A2
    #Stage1
    model.random.1.train=randomForest(factor(A1) ~ X1,  importance = TRUE,data=dat[s,])# Train a random forest model for predicting A1
    model.random.1.test=randomForest(factor(A1) ~ X1,  importance = TRUE,data=dat[-s,])
    # Make predictions for A1 using the test and train models
    pred.random.1=dat$A1
    pred.random.1[s]=predict(model.random.1.test, type = "prob", dat[s,])[,2]
    pred.random.1[-s]=predict(model.random.1.train, type = "prob", dat[-s,])[,2]
    w.random.1=abs(dat$A1-pred.random.1)# Calculate weights based on the absolute difference between true and predicted A1
    A2opt.hat.random = 1*(L2%*%psi.hat.random.2 > 0)# Determine the optimal A2 values based on learned coefficients from Stage 2
    mu2.hat.random = ( A2opt.hat.random - dat$A2)*L2%*%psi.hat.random.2 # Calculate the adjusted predictions for Y using A1 predictions and optimal A2 values
    Yopt.hat.random.2 = dat$Y + mu2.hat.random
    model.res.random.1=summary(glm(Yopt.hat.random.2 ~ X1+ A1*(1 + X1), weights= w.random.1,family = gaussian,data=dat))# Fit a weighted linear regression model for adjusted Y using A1 predictions and other features
    psi.hat.random.1=model.res.random.1$coef[c("A1", "X1:A1"),1]# Extract coefficients related to A1 and interaction term X1:A1
    
    ### Naive bayes model 
    ###Stage 2
    model.bayes.2.train=naiveBayes(A2~ X1+X2+A1,data=dat[s,])# Train Naive Bayes model for A2 using features X1, X2, and A1 on the training set
    model.bayes.2.test=naiveBayes(A2~ X1+X2+A1,data=dat[-s,])# Train Naive Bayes model for A2 using features X1, X2, and A1 on the test set
    # Make predictions for A2 on the entire dataset using the test and train models
    pred.bayes.2=dat$A2
    pred.bayes.2[s]=predict(model.bayes.2.test, dat[s,],"raw")[,2]
    pred.bayes.2[-s]=predict(model.bayes.2.train, dat[-s,],"raw")[,2]
    # Calculate weights based on the absolute difference between true A2 values and predicted values
    w.bayes.2=abs(dat$A2-pred.bayes.2)
    # Fit a Gaussian model for Y using X1, X2, A1, and predicted A2 values, with weights
    model.res.bayes.2=summary(glm(Y~X1+X2+A1*(1+X1+X2)+A2*(1+X2), weights= w.bayes.2,family = gaussian,data=dat))
    psi.hat.bayes.2 =  model.res.bayes.2$coef[c("A2", "X2:A2"),1]# Extract coefficients for A2 and interaction term X2:A2
    ###Stage 1
    model.bayes.1.train=naiveBayes(A1~ X1,data=dat[s,])# Train Naive Bayes model for A1 using feature X1 on the training set
    model.bayes.1.test=naiveBayes(A1~ X1,data=dat[-s,])# Train Naive Bayes model for A1 using feature X1 on the test set
    # Make predictions for A1 on the entire dataset using the test and train models
    pred.bayes.1=dat$A1
    pred.bayes.1[s]=predict(model.bayes.1.test, dat[s,],type ="raw")[,2]
    pred.bayes.1[-s]=predict(model.bayes.1.train, dat[-s,],type ="raw")[,2]
    # Calculate weights based on the absolute difference between true A1 values and predicted values
    w.bayes.1=abs(dat$A1-pred.bayes.1)
    A2opt.hat.bayes =1*(L2%*%psi.hat.bayes.2 > 0)# Determine the optimal A2 using the learned coefficients for A2 and X2:A2
    mu2.hat.bayes =(A2opt.hat.bayes - dat$A2)*L2%*%psi.hat.bayes.2 # Calculate the optimal mu2 using the optimal A2 and coefficients for X2:A2
    Yopt.hat.bayes.2 = dat$Y + mu2.hat.bayes # Calculate Yopt by adding mu2 to the original Y
    model.res.bayes.1=summary(glm(Yopt.hat.bayes.2 ~ X1+ A1*(1 + X1), weights= w.bayes.1,family = gaussian,data=dat))# Fit a Gaussian model for Yopt using X1 and A1, with weights
    psi.hat.bayes.1 =  model.res.bayes.1$coef[c("A1", "X1:A1"),1]# Extract coefficients for A1 and interaction term X1:A1
    
    ### Neural networks
    #Stage 2
    model.neurone.2.train=nnet(A2~X1+X2+A1, size = 3, decay = 0.1,data=dat[s,])# Train Neural Network model for A2 using features X1, X2, and A1 on the training set
    model.neurone.2.test=nnet(A2~X1+X2+A1, size = 3, decay = 0.1,data=dat[-s,])# Train Neural Network model for A2 using features X1, X2, and A1 on the test set
    # Make predictions for A2 on the entire dataset using the test and train models
    pred.neurone.2=dat$A2
    pred.neurone.2[s]=predict(model.neurone.2.test,type ="raw", dat[s,] )
    pred.neurone.2[-s]=predict(model.neurone.2.train,type ="raw", dat[-s,] )
    # Calculate weights based on the absolute difference between true A2 values and predicted values
    w.neurone.2=abs(dat$A2-pred.neurone.2)
    model.res.neurone.2=summary(glm(Y~X1+X2+A1*(1+X1+X2)+A2*(1+X2), weights=  w.neurone.2,family = gaussian,data=dat))# Fit a Gaussian model for Y using X1, X2, A1, and predicted A2 values, with weights
    psi.hat.neurone.2 =model.res.neurone.2$coef[c("A2", "X2:A2"),1]# Extract coefficients for A2 and interaction term X2:A2
    ### Stage 1
    model.neurone.1.train=nnet(A1~ X1, size = 3, decay = 0.1,data=dat[s,])# Train Neural Network model for A1 using feature X1 on the training set
    model.neurone.1.test=nnet(A1~ X1, size = 3, decay = 0.1,data=dat[-s,])# Train Neural Network model for A1 using feature X1 on the test set
    # Make predictions for A1 on the entire dataset using the test and train model
    pred.neurone.1=dat$A1
    pred.neurone.1[s]=predict(model.neurone.1.test,type ="raw", dat[s,] )
    pred.neurone.1[-s]=predict(model.neurone.1.train,type ="raw", dat[-s,] )
    # Calculate weights based on the absolute difference between true A1 values and predicted values
    w.neurone.1=abs(dat$A1-pred.neurone.1)
    A2opt.hat.neurone =1*(L2%*%psi.hat.neurone.2 > 0)# Determine the optimal A2 using the learned coefficients for A2 and X2:A2
    mu2.hat.neurone = (A2opt.hat.neurone - dat$A2)*L2%*%psi.hat.neurone.2 # Calculate the optimal mu2 using the optimal A2 and coefficients for X2:A2
    Yopt.hat.neurone.2 =dat$Y + mu2.hat.neurone # Calculate Yopt by adding mu2 to the original Y
    model.res.neurone.1=summary(glm(Yopt.hat.neurone.2  ~ X1+ A1*(1 + X1), weights=  w.neurone.1,family = gaussian,data=dat))# Fit a Gaussian model for Yopt using X1 and A1, with weights
    psi.hat.neurone.1 = model.res.neurone.1$coef[c("A1", "X1:A1"),1] # Extract coefficients for A1 and interaction term X1:A1
    
    ### SVM
    ### Stage 2
    model.SVM.2.train=svm(A2~ X1+X2+A1,data=dat[s,], kernel = "linear")# Train SVM model for A2 using features X1, X2, and A1 on the training set
    model.SVM.2.test=svm(A2~ X1+X2+A1,data=dat[-s,], kernel = "linear")# Train SVM model for A2 using features X1, X2, and A1 on the test set
    # Make predictions for A2 on the entire dataset using the test and train models
    pred.SVM.2=dat$A2
    pred.SVM.2[s]=predict(model.SVM.2.test, probability=TRUE,  newdata =dat[s,])
    pred.SVM.2[-s]=predict(model.SVM.2.train, probability=TRUE,  newdata =dat[-s,])
    w.SVM.2=abs(dat$A2-pred.SVM.2)# Calculate weights based on the absolute difference between true A2 values and predicted values
    model.res.SVM.2=summary(glm(Y~X1+X2+A1*(1+X1+X2)+A2*(1+X2), weights=  w.SVM.2,family = gaussian, data = dat))# Fit a Gaussian model for Y using X1, X2, A1, and predicted A2 values, with weights
    psi.hat.SVM.2 = model.res.SVM.2$coef[c("A2", "X2:A2"),1]# Extract coefficients for A2 and interaction term X2:A2
    #Stage 1
    model.SVM.1.train=svm(A1~ X1,data=dat[s,], kernel = "linear")# Train SVM model for A1 using feature X1 on the training set
    model.SVM.1.test=svm(A1~ X1,data=dat[-s,], kernel = "linear")# Train SVM model for A1 using feature X1 on the test set
    # Make predictions for A1 on the entire dataset using the test and train models
    pred.SVM.1=dat$A1
    pred.SVM.1[s]=predict(model.SVM.1.test, probability=TRUE,  newdata =dat[s,])
    pred.SVM.1[-s]=predict(model.SVM.1.train, probability=TRUE,  newdata =dat[-s,])
    w.SVM.1=abs(dat$A1-pred.SVM.1)# Calculate weights based on the absolute difference between true A1 values and predicted values
    A2opt.hat.SVM = 1*(L2%*%psi.hat.SVM.2 > 0)# Determine the optimal A2 using the learned coefficients for A2 and X2:A2
    mu2.hat.SVM =(A2opt.hat.SVM - dat$A2)*L2%*%psi.hat.SVM.2# Calculate the optimal mu2 using the optimal A2 and coefficients for X2:A2
    Yopt.hat.SVM.2 = dat$Y + mu2.hat.SVM# Calculate Yopt by adding mu2 to the original Y
    model.res.SVM.1 =summary(glm(Yopt.hat.SVM.2~ X1+ A1*(1 + X1), weights=  w.SVM.1,family = gaussian, data=dat))# Fit a Gaussian model for Yopt using X1 and A1, with weights
    psi.hat.SVM.1 = model.res.SVM.1$coef[c("A1", "X1:A1"),1] # Extract coefficients for A1 and interaction term X1:A1
    
    ### dWOLS_SL
    ## Stage 2
    # Estimate treatment effect for A2 using Super Learner
    mu.t.hat1=SuperLearner_function(Y = A2[s], X = H2[s,, drop = FALSE], family = "binomial")
    mu.t.hat2=SuperLearner_function(Y = A2[-s], X = H2[-s,, drop = FALSE], family = "binomial")
    mu.t.h = dat$A2
    mu.t.h[s] = predict(mu.t.hat2, type = "raw",newdata = data.frame(H2[s,, drop = FALSE]))$pred
    mu.t.h[-s] = predict(mu.t.hat1, type = "raw",newdata = data.frame(H2[-s,, drop = FALSE]))$pred
    # Calculate weights
    w2 = abs(dat$A2 - mu.t.h)
    # Weighted least squares regression for Stage 2
    lm.dwols2 = lm(Y ~ X1 + X2 + A1*(1 + X1 + X2) + A2*(1 + X2), weights = w2, data=dat)
    psi2.hat.dwols =lm.dwols2$coef[c("A2", "X2:A2")]
    
    ### Stage 1
    # Estimate treatment effect for A1 using Super Learner
    mu.t.hat1 = SuperLearner_function(Y = A1[s], X = H1[s,, drop = FALSE], family = "binomial")
    mu.t.hat2 = SuperLearner_function(Y = A1[-s], X = H1[-s,, drop = FALSE], family = "binomial")
    mu.t.h = dat$A1
    mu.t.h[s]= predict(mu.t.hat2, type = "raw",
                        newdata = data.frame(H1[s,, drop = FALSE]))$pred
    mu.t.h[-s] = predict(mu.t.hat1, type = "raw",
                          newdata = data.frame(H1[-s,, drop = FALSE]))$pred
    # Calculate weights
    w1 = abs(dat$A1 - mu.t.h)
    # Optimal treatment assignment for Stage 1
    A2opt.hat = 1*(L2%*%psi2.hat.dwols > 0)
    mu2.hat.dwols = (A2opt.hat - dat$A2)*L2%*%psi2.hat.dwols
    Yopt.hat = Y + mu2.hat.dwols
    # Weighted least squares regression for Stage 1
    lm.dwols1 =lm(Yopt.hat ~ X1 + A1*(1 + X1), weights = w1, data=dat)
    psi1.hat.dwols = lm.dwols1$coef[c("A1", "X1:A1")]
    
    #Result matrix 
    results.logit[i,]=c(psi.hat.logit.1,psi.hat.logit.2)
    results.random[i,]=c(psi.hat.random.1,psi.hat.random.2)
    results.bayes[i,]=c(psi.hat.bayes.1,psi.hat.bayes.2)
    results.neural[i,]=c(psi.hat.neurone.1,psi.hat.neurone.2)
    results.SVM[i,]=c(psi.hat.SVM.1,psi.hat.SVM.2)
    results.SL[i,]=c(psi1.hat.dwols,psi2.hat.dwols)
    print(data.frame(n, i, time = Sys.time()))
    
    # Save results based on the scenario
    file.name = paste("TwoTimeStudy3_scenario_", scenario, "_", n, ".Rdata", sep="")
    save(results.logit,results.random,results.bayes,results.neural,results.SVM,
         results.SL, file = file.name)
  }
}

# Number of repetitions  
nrep = 1000

# List of scenarios
scenarios = c("simple", "medium", "complex")

# Loop through scenarios and sample sizes
for (n in c(300,1000)){
  for (scenario in scenarios)  {
    run_scenario(n, scenario)
  }
}



### Load the recorded results
setwd("C:\\Users\\kossi\\Dropbox\\RS_hormonotherapie\\Result_TwoTimes\\Study 3")
load("TwoTimeStudy3_scenario_complex_1000.Rdata")

### List of model results and their corresponding names
models = c("logit", "random", "bayes", "neural", "SVM", "SL")
variables_a_imprimer = list(
  logit = results.logit,
  random = results.random,
  bayes = results.bayes,
  neural = results.neural,
  SVM = results.SVM,
  SL = results.SL
)

calculate_metrics = function(results, true.psi) {
  bias_result = bias(results, true.psi)  # Bias
  sigma_result = sig(results)  # Standard Deviation (sigma)
  rmse_result = rmse(results, true.psi)  # Root Mean Squared Error (rmse)
  ratio_result = rmse_result / rmse(results.logit, true.psi)  # Ratio
  Monte_Carlo_SE_Bias= max(sigma_result/sqrt(nrep))
  Monte_Carlo_SE_sigma= max(sigma_result/sqrt(2*(nrep-1)))
  return(list(bias = bias_result, sigma = sigma_result, rmse = rmse_result, ratio = ratio_result,
              SE_Bias=Monte_Carlo_SE_Bias, SE_sigma=Monte_Carlo_SE_sigma))
}

# Loop through each model's results
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


#RQL
setwd("C:\\Users\\kossi\\Dropbox\\RS_hormonotherapie\\Result_TwoTimes\\RQL_TwoTimes\\Study 3")
load("RQL_TwoTimeStudy3_scenario_complex_1000.Rdata")

### List of model results and their corresponding names
models = c("RQL")
variables_a_imprimer = list(
  RQL=results.RQL
)

calculate_metrics = function(results, true.psi) {
  bias_result = bias(results, true.psi)  # Bias
  sigma_result = sig(results)  # Standard Deviation (sigma)
  rmse_result = rmse(results, true.psi)  # Root Mean Squared Error (rmse)
  ratio_result = rmse_result / rmse(results.logit, true.psi)  # Ratio
  Monte_Carlo_SE_Bias= max(sigma_result/sqrt(nrep))
  Monte_Carlo_SE_sigma= max(sigma_result/sqrt(2*(nrep-1)))
  return(list(bias = bias_result, sigma = sigma_result, rmse = rmse_result, ratio = ratio_result,
              SE_Bias=Monte_Carlo_SE_Bias, SE_sigma=Monte_Carlo_SE_sigma))
}

# Loop through each model's results
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

