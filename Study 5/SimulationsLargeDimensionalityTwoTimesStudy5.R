

# Set working directory
  setwd("...")

# Define logistic function
expit = plogis


# SuperLearner function
SuperLearner_function = function(Y, X, family) {
  return(SuperLearner(Y = Y, X = X, family = family, 
                      SL.library = c("SL.glm", "SL.randomForest", "SL.nnet",
                                     "SL.glm.interaction")))}

# Psi parameters
psi1 = c(-3, 1, -0.2, 0.3, 1, 0.5, 1, -0.2, 0.3, 1, 0.5)
psi2 = c(-0.5,1, -0.2, 0.3, 1, 0.5, 1, -0.2, 0.3, 1, 0.5)


## Counterfactual simulation to estimate true psi1 value
# set.seed(43578578)
# n = 10000000
# Stage 1
# X11 = rbinom(n, 1, p = 0.5)                                 
# X12 = rbinom(n, 1, p = 0.05 + 0.5 * X11)                    
# X13 = rnorm(n, mean = 0, sd = 0.5)                            
# X14 = rbinom(n, 1, p = expit(-0.4 + X12))                 
# X15 = runif(n, min = 0, max = 1)
# X16 = as.integer(cut(runif(n, 0, 1), breaks = 3))          
# X17 = as.integer(cut(runif(n, 0, 1), breaks = 3))
# X18 = X11*X14 + rnorm(n, 0, 0.5)                          
# X19 = log(X15 + 1) + runif(n, min = 0, max = 1)                                          
# X110 = -X13 + X12*X15 + runif(n, min = 0, max = 1) 
# X1= cbind(1, X11, X12, X13, X14, X15, X16, X17, X18, X19, X110)
#  A1opt = 1*(X1%*%psi1 > 0)
#  mu1_1 = (A1opt - 1)*X1%*%psi1  
#   mu1_0 = (A1opt - 0)*X1%*%psi1  
# Stage 2
# X21_1 = X11 + 1 + rnorm(n, mean = 0, sd = 0.1)
# X22_1 = rbinom(n, 1, p = expit(-0.1 + 0.3 * X11 + 1))
# X23_1 = X13 + 1 + rnorm(n, mean = 0, sd = 0.2)
# X24_1 = rbinom(n, 1, p = expit(-0.4 + X22_1 + 1))
# X25_1 = X15 + 1 + runif(n, min = -1, max = 1)
# X26_1 = X16 + 1 + sample(0:2, n, replace = TRUE)
# X27_1 = as.integer(cut(runif(n, 0, 1), breaks = 3))
# X28_1 = abs(X21_1 - X24_1) + sin(X23_1 + 1 )+ runif(n, min = 0, max = 1)                             
# X29_1 = cos(X25_1 + X15 + 1)+ runif(n, min = 0, max = 1)                                       
# X210_1 = 0.5*X23_1 + 0.5*X21_1 + 1 + runif(n, min = 0, max = 1)
# X21_0 = X11 + 0 + rnorm(n, mean = 0, sd = 0.1)
# X22_0 = rbinom(n, 1, p = expit(-0.1 + 0.3 * X11 + 0))
# X23_0 = X13 + 0 + rnorm(n, mean = 0, sd = 0.2)
# X24_0 = rbinom(n, 1, p = expit(-0.4 + X22_0 + 0))
# X25_0 = X15 + 0 + runif(n, min = -1, max = 1)
# X26_0 = X16 + 0 + sample(0:2, n, replace = TRUE)
# X27_0 = as.integer(cut(runif(n, 0, 1), breaks = 3))
# X28_0 = abs(X21_0 - X24_0) + sin(X23_0 + 0 ) + runif(n, min = 0, max = 1)                            
# X29_0 = cos(X25_0 + X15 + 0)  + runif(n, min = 0, max = 1)                                     
# X210_0 = 0.5*X23_0 + 0.5*X21_0 + 0 + runif(n, min = 0, max = 1)

# Simple
# Y_1 = rnorm(n, mean =  X11+ X12+ X13+ X14+X15+X16+X17+X18+X19+X110 + X21_1+ X22_1+ X23_1+ X24_1+ X25_1+ X26_1+ X27_1+ X28_1+ X29_1+ X210_1 - mu1_1, 1) #Y^{1,opt}
# Y_0 = rnorm(n, mean = X11+ X12+ X13+ X14+X15+X16+X17+X18+X19+X110 + X21_0+ X22_0+ X23_0+ X24_0+ X25_0+ X26_0+ X27_0+ X28_0+ X29_0+ X210_0 - mu1_0, 1) #Y^{0,opt}
# true.psi = c(lm(Y_1 - Y_0 ~ X11+ X12+ X13+ X14+X15+X16+X17+X18+X19+X110)$coef, psi2)
# true.psi
# true.psi = c( 2.4149562 ,  1.9525503 , -0.2016648 , -0.9292295   ,0.9987304  , 1.5924654 ,  1.0022201,  -0.2004970 ,  0.3013375,0.9590737   ,0.4964222 , -0.5000000 ,  1.0000000,  -0.2000000 ,  0.3000000  , 1.0000000,   0.5000000 ,  1.0000000,-0.2000000 ,  0.3000000,   1.0000000,   0.5000000)
  
# Medium
#   Y_1 = rnorm(n, mean =  X11 + X13 - X14 + X15 - X16 + X17 - X18 - 0.5*X19 - X110 
#               + X13*X13 + 0.5*X15*X15 - 0.5*X16*X16 + 0.5*X17*X17 - 0.5*X18*X18 + 0.25*X19*X19 - 0.25*X110*X110 
#               - X21_1 - X23_1 -1.5*X24_1 + X25_1 - X26_1 + X27_1 - X28_1 - 0.5*X29_1 - X210_1 
#               + 0.5*X21_1*X21_1 + 0.25*X23_1*X23_1 + 0.25*X25_1*X25_1 - 0.5*X26_1*X26_1 + 0.5*X27_1*X27_1 - 0.5*X28_1*X28_1
#               + 0.5*X29_1*X29_1 - 0.25*X210_1*X210_1 - mu1_1, 1)
#   Y_0 = rnorm(n, mean =  X11 + X13 - X14 + X15 - X16 + X17 - X18 - 0.5*X19 - X110 
#               + X13*X13 + 0.5*X15*X15 - 0.5*X16*X16 + 0.5*X17*X17 - 0.5*X18*X18 + 0.25*X19*X19 - 0.25*X110*X110 
#               - X21_0 - X23_0 -1.5*X24_0 + X25_0 - X26_0 + X27_0 - X28_0 - 0.5*X29_0 - X210_0 
#               + 0.5*X21_0*X21_0 + 0.25*X23_0*X23_0 + 0.25*X25_0*X25_0 - 0.5*X26_0*X26_0 + 0.5*X27_0*X27_0 - 0.5*X28_0*X28_0
#               + 0.5*X29_0*X29_0 - 0.25*X210_0*X210_0 - mu1_0, 1)
# true.psi = c(lm(Y_1 - Y_0 ~ X11+ X12+ X13+ X14+X15+X16+X17+X18+X19+X110)$coef, psi2)
# true.psi
#true.psi = c( -10.607848830,  -1.420540003,  -0.194972803  , 3.088690147 ,  1.005106175 ,  1.346672115  , 0.005283755  ,-0.194350423,0.296906346 ,  1.013564867 ,  0.505205223, -0.5000000   ,1.0000000 , -0.2000000  , 0.3000000 ,  1.0000000 ,  0.5000000  , 1.0000000,-0.2000000  , 0.3000000 ,  1.0000000 ,  0.5000000)
 
# Complex
# 
# Y_1 = rnorm(n, mean = 1.5 + 1.5*X11 + sin(X13) - 1.5*X14 + cos(X15) + 1.5*sin(cos(X13*X13)) + abs(sin(X15*X15))
#             - 0.25*X16*X16 + cos(X17*X17) - 0.5*log(X18*X18 + 1) + 0.5*X19*X19 -cos(X110*X110) 
#             - X21_1 + sin(X23_1) - X24_1 + cos(X25_1) + 0.5*X21_1*X21_1 + 1.5*sin(cos(X23_1*X23_1)) - abs(sin(X25_1*X25_1))
#             - 0.25*X26_1*X26_1 + cos(X27_1*X27_1) - 0.25*log(X28_1*X28_1 + 1) + 0.5*X29_1*X29_1 - 1.5*cos(X210_1*X210_1) - mu1_1, 1)
# Y_0 = rnorm(n, mean = 1.5 + 1.5*X11 + sin(X13) - 1.5*X14 + cos(X15) + 1.5*sin(cos(X13*X13)) + abs(sin(X15*X15))
#             - 0.1*X16*X16 + cos(X17*X17) - 0.5*log(X18*X18 + 1) + 0.5*X19*X19 -cos(X110*X110) 
#             - X21_0 + sin(X23_0) - X24_0 + cos(X25_0) + 0.5*X21_0*X21_0 + 1.5*sin(cos(X23_0*X23_0)) - abs(sin(X25_0*X25_0))
#             - 0.1*X26_0*X26_0 + cos(X27_0*X27_0) - 0.25*log(X28_0*X28_0 + 1) + 0.5*X29_0*X29_0 - 1.5*cos(X210_0*X210_0) - mu1_0, 1)
# 
# true.psi = c(lm(Y_1 - Y_0 ~ X11+ X12+ X13+ X14+X15+X16+X17+X18+X19+X110)$coef, psi2)
# true.psi
# true.psi = c( -4.3516984   ,1.2135523 , -0.1977484 , -1.9028925 ,  0.9999270 ,  0.9770289  ,-0.9991191,  -0.2004283  , 0.2984942 , 0.9898369  , 0.4974904 ,-0.5000000 ,  1.0000000 , -0.2000000   ,0.3000000  , 1.0000000  , 0.5000000 ,  1.0000000 , -0.2000000 ,  0.3000000  , 1.0000000  , 0.5000000 )

  
# Function to run  for each scenarios for each size
run_scenario = function(n, scenario) {
  # Initialize objects
  results.logit = results.random = results.bayes = results.neural =
    results.SL = results.SVM =  matrix(NA, nrow = nrep, ncol = 22)
  
  for(i in 1:nrep){
    # Generate data based on the chosen scenario
    if(scenario == "simple"){
      # covariates at stage 1 
      X11 = rbinom(n, 1, p = 0.5)                                 
      X12 = rbinom(n, 1, p = 0.05 + 0.5 * X11)                    
      X13 = rnorm(n, mean = 0, sd = 0.5)                            
      X14 = rbinom(n, 1, p = expit(-0.4 + X12))                 
      X15 = runif(n, min = 0, max = 1)
      X16 = as.integer(cut(runif(n, 0, 1), breaks = 3))          
      X17 = as.integer(cut(runif(n, 0, 1), breaks = 3))
      X18 = X11*X14 + rnorm(n, 0, 0.5)                          
      X19 = log(X15 + 1) + runif(n, min = 0, max = 1)                                          
      X110 = -X13 + X12*X15 + runif(n, min = 0, max = 1)   
      X1= cbind(1, X11, X12, X13, X14, X15, X16, X17, X18, X19, X110)
      # treatment at stage 1
      A1 = rbinom(n, 1, p = expit(-4 - 1.5*X11 + 5.5* X13-5.5*X14 - 1.5*X15 
                                + 2.5*X16 + 2.5*X17 + X18 - 0.5*X19 + X110))
      # Optimal treatment assignment stage 1 
      A1opt = 1*(X1%*%psi1 > 0)
      mu1 = (A1opt - A1)*X1%*%psi1
      # covariates at stage 2
      X21 = X11 + A1 + rnorm(n, mean = 0, sd = 0.1)
      X22 = rbinom(n, 1, p = expit(-0.1 + 0.3 * X11 + A1))
      X23 = X13 + A1 + rnorm(n, mean = 0, sd = 0.2)
      X24 = rbinom(n, 1, p = expit(-0.4 + X22 + A1))
      X25 = X15 + A1 + runif(n, min = -1, max = 1)
      X26 = X16 + A1 + sample(0:2, n, replace = TRUE)
      X27 = as.integer(cut(runif(n, 0, 1), breaks = 3))
      X28 = abs(X21 - X24) + sin(X23 + A1 ) + runif(n, min = 0, max = 1)                            
      X29 = cos(X25 + X15 + A1)   + runif(n, min = 0, max = 1 )                               
      X210 = 0.5*X23 + 0.5*X21 + A1 + runif(n, min = 0, max = 1)
      X2= cbind(1, X21, X22, X23, X24, X25, X26, X27, X28, X29, X210)
      
      # treatment at stage 2 
      A2 = rbinom(n, 1, p = expit(15 - X21 - X23 - 3.5*X24 + X25 
                                  - 2.5*X26 - X27 - X28 - 0.5*X29 - X210))
      # Optimal treatment assignment stage 2
      A2opt = 1*(X2%*%psi2 > 0)
      mu2 = (A2opt - A2)*X2%*%psi2
      #Outcome generation
      Y = rnorm(n, mean = X11 + X12 + X13 + X14 + X15 + X16 + X17+ X18 + X19 
         + X110 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + X29 + X210 - mu1 - mu2, 1)
    } else if (scenario == "medium"){
      # covariates at stage 1
      X11 = rbinom(n, 1, p = 0.5)                               
      X12 = rbinom(n, 1, p = 0.05 + 0.5 * X11)                    
      X13 = rnorm(n, mean = 0, sd = 0.5)                            
      X14 = rbinom(n, 1, p = expit(-0.4 + X12))                 
      X15 = runif(n, min = 0, max = 1)
      X16 = as.integer(cut(runif(n, 0, 1), breaks = 3))          
      X17 = as.integer(cut(runif(n, 0, 1), breaks = 3))
      X18 = X11*X14 + rnorm(n, 0, 0.5)                          
      X19 = log(X15 + 1) + runif(n, min = 0, max = 1)                                          
      X110 = -X13 + X12*X15 + runif(n, min = 0, max = 1)  
      X1= cbind(1, X11, X12, X13, X14, X15, X16, X17, X18, X19, X110)
      # treatment at stage 1
      A1 = rbinom(n, 1, p = expit(5 - 1.5*X11 + 0.5*X12 - 0.5*X14 + 0.5*X13 -1.5*X15 - X16 - X17 + X18
           -0.5*X19 + X110  + 0.5*X13*X13 + X15*X15  -0.5*X18*X18 + X19*X19 - 0.5*X110*X110))
      # Optimal treatment assignment stage 1 
      A1opt = 1*(X1%*%psi1 > 0)
      mu1 = (A1opt - A1)*X1%*%psi1
      #Stage 2
      # covariates at stage 2
      X21 = X11 + A1 + rnorm(n, mean = 0, sd = 0.1)
      X22 = rbinom(n, 1, p = expit(-0.1 + 0.3 * X11 + A1))
      X23 = X13 + A1 + rnorm(n, mean = 0, sd = 0.2)
      X24 = rbinom(n, 1, p = expit(-0.4 + X22 + A1))
      X25 = X15 + A1 + runif(n, min = -1, max = 1)
      X26 = X16 + A1 + sample(0:2, n, replace = TRUE)
      X27 = as.integer(cut(runif(n, 0, 1), breaks = 3))
      X28 = abs(X21 - X24) + sin(X23 + A1 ) + runif(n, min = 0, max = 1)                             
      X29 = cos(X25 + X15 + A1)  + runif(n, min = 0, max = 1)                                      
      X210 = 0.5*X23 + 0.5*X21 + A1+ runif(n, min = 0, max = 1) 
      X2= cbind(1, X21, X22, X23, X24, X25, X26, X27, X28, X29, X210)
      
      # treatment at stage 2
      A2 = rbinom(n, 1, p = expit( 5- X21 - X23 -1*X24 + X25 - 0.5*X26 - X27 
                         - X28 - 0.5*X29 - X210 + 0.5*X21*X21 + 0.5*X23*X23 
             + 0.5*X25*X25 - 0.1*X26*X26 - 0.5*X28*X28 + X29*X29 - 0.5*X210*X210))
      # Optimal treatment assignment stage 2
      A2opt = 1*(X2%*%psi2 > 0)
      mu2 = (A2opt - A2)*X2%*%psi2
      #Outcome generation
      Y = rnorm(n, mean =  X11 + X13 - X14 + X15 - X16 + X17 - X18 - 0.5*X19 - X110 
                + X13*X13 + 0.5*X15*X15 - 0.5*X16*X16 + 0.5*X17*X17 - 0.5*X18*X18 + 0.25*X19*X19 - 0.25*X110*X110 
                - X21 - X23 -1.5*X24 + X25 - X26 + X27 - X28 - 0.5*X29 - X210 
                + 0.5*X21*X21 + 0.25*X23*X23 + 0.25*X25*X25 - 0.5*X26*X26 + 0.5*X27*X27 - 0.5*X28*X28
                + 0.5*X29*X29 - 0.25*X210*X210 - mu1-mu2, 1)
      
    } else if (scenario == "complex"){
      
      # covariates at stage 1
      X11 = rbinom(n, 1, p = 0.5)                                
      X12 = rbinom(n, 1, p = 0.05 + 0.5 * X11)                    
      X13 = rnorm(n, mean = 0, sd = 0.5)                            
      X14 = rbinom(n, 1, p = expit(-0.4 + X12))                 
      X15 = runif(n, min = 0, max = 1)
      X16 = as.integer(cut(runif(n, 0, 1), breaks = 3))          
      X17 = as.integer(cut(runif(n, 0, 1), breaks = 3))
      X18 = X11*X14 + rnorm(n, 0, 0.5)                          
      X19 = log(X15 + 1) + runif(n, min = 0, max = 1)                                          
      X110 = -X13 + X12*X15 + runif(n, min = 0, max = 1) 
      X1= cbind(1, X11, X12, X13, X14, X15, X16, X17, X18, X19, X110)
      # treatment at stage 1
      A1 = rbinom(n, 1, p =  expit(4 - 1.5*X11 + 0.5*X12 + sin(X13) - X14 + cos(X15) - X16 
                          + sin(cos(X13*X13)) + abs(sin(X15*X15))
                    - cos(X17*X17) - log(X18*X18) + X19*X19 - exp(tan(X110*X110))))
      # Optimal treatment assignment stage 1 
      A1opt = 1*(X1%*%psi1 > 0)
      mu1 = (A1opt - A1)*X1%*%psi1
      # covariates at stage 2
      X21 = X11 + A1 + rnorm(n, mean = 0, sd = 0.1)
      X22 = rbinom(n, 1, p = expit(-0.1 + 0.3 * X11 + A1))
      X23 = X13 + A1 + rnorm(n, mean = 0, sd = 0.2)
      X24 = rbinom(n, 1, p = expit(-0.4 + X22 + A1))
      X25 = X15 + A1 + runif(n, min = -1, max = 1)
      X26 = X16 + A1 + sample(0:2, n, replace = TRUE)
      X27 = as.integer(cut(runif(n, 0, 1), breaks = 3))
      X28 = abs(X21 - X24) + sin(X23 + A1 )+ runif(n, min = 0, max = 1)                              
      X29 = cos(X25 + X15 + A1)+ runif(n, min = 0, max = 1)                                        
      X210 = 0.5*X23 + 0.5*X21 + A1+ runif(n, min = 0, max = 1) 
      X2 = cbind(1, X21, X22, X23, X24, X25, X26, X27, X28, X29, X210)
      # treatment at stage 2
      A2 = rbinom(n, 1, p = expit(- 0.5*X21 + sin(X23) - X24 + cos(X25) + X21*X21 
                  + sin(cos(X23*X23)) + abs(sin(X25*X25)) - 0.1*X26*X26 
                  - cos(X27*X27) - log(X28*X28) + X29*X29 - exp(tan(X210*X210))))
      # Optimal treatment assignment stage 2
      A2opt = 1*(X2%*%psi2 > 0)
      mu2 = (A2opt - A2)*X2%*%psi2
      #Outcome generation
      Y = rnorm(n, mean = 1.5 + 1.5*X11 + sin(X13) - 1.5*X14 + cos(X15) + 1.5*sin(cos(X13*X13))
                + abs(sin(X15*X15))- 0.1*X16*X16 + cos(X17*X17) - 0.5*log(X18*X18 + 1) 
                + 0.5*X19*X19 -cos(X110*X110) - X21 + sin(X23) - X24 + cos(X25)
                + 0.5*X21*X21 + 1.5*sin(cos(X23*X23)) - abs(sin(X25*X25))
                - 0.1*X26*X26 + cos(X27*X27) - 0.25*log(X28*X28 + 1) 
                + 0.5*X29*X29 - 1.5*cos(X210*X210) - mu1-mu2, 1)
      
    }
    
    #Complete data  
    dat = data.frame(X11, X12, X13, X14, X15, X16, X17, X18, X19, X110,
                     X21, X22, X23, X24, X25, X26, X27, X28, X29, X210,A1,A2,Y)
    
    #Data sets
    # Create a data frame H1 
    H1 = data.frame(X11, X12, X13, X14, X15, X16, X17, X18, X19, X110)
    # Create a data frame H2 
    H2 = data.frame(A1, X11, X12, X13, X14, X15, X16, X17, X18, X19, X110,
                    X21, X22, X23, X24, X25, X26, X27, X28, X29, X210) 
    # Create a matrix L2 with two columns: a column of ones and X2
    L2 = cbind(1,X21, X22, X23, X24, X25, X26, X27, X28, X29, X210)
    # Generate a random sample of indices s, representing the indices of the selected rows
    # The sample is obtained by randomly selecting n/2 indices from the range 1 to n and sorting them
    s = sort(sample(1:n, n/2))
    
    ### Logit
    tryCatch({
    ##Stage 2
    model.logit.2 = glm(A2~ X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 
      + X19 + X110 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + X29 
      + X210 + A1, family = "binomial", data = dat, maxit = 1000) 
    pred.logit.2 = predict(model.logit.2,type="res",dat)
    w.logit.2 = abs(dat$A2-pred.logit.2) # Calculate absolute differences between A2 and predicted values
    model.res.logit.2 = summary(glm(Y~X11+ X12 + X13 + X14 + X15 + X16 + X17+ X18 + X19 + X110 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + X29 + X210
          +A1*(1 + X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X110 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + X29 + X210)
          +A2*(1 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + X29 + X210), weights = w.logit.2, family = gaussian, data = dat, maxit = 1000))# Fit weighted least squares model for Y 
    psi.hat.logit.2 = model.res.logit.2$coef[c("A2", "X21:A2", "X22:A2", "X23:A2", "X24:A2", "X25:A2", "X26:A2", "X27:A2", "X28:A2", "X29:A2", "X210:A2"),1]# Extract coefficients for A2 and interaction term from the model results
    ## Stage 1
    model.logit.1 = glm(A1 ~ X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X110, family = "binomial", data = dat, maxit = 1000)
    # Insert predictions for the test set and training set
    pred.logit.1 = predict(model.logit.1, type = "res", dat)
    w.logit.1 = abs(dat$A1 - pred.logit.1)# Calculate absolute differences between A1 and predicted values
    A2opt.hat.logit = 1*(L2%*%psi.hat.logit.2 > 0) # Estimate optimal assignment for A2 based on the second stage's results
    mu2.hat.logit = (A2opt.hat.logit - dat$A2)*L2%*%psi.hat.logit.2 # Calculate the treatment effect for A1
    Yopt.hat.logit.2 = dat$Y + mu2.hat.logit# Calculate the outcome with the estimated treatment effect for A1
    model.res.logit.1 = summary(glm( Yopt.hat.logit.2  ~ X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X110
       + A1*(1 + X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X110 ), weights = w.logit.1, dat, family = gaussian, maxit = 1000))  # Fit weighted least squares model for Yopt using predictors X1, A1*(1 + X1)
    psi.hat.logit.1 = model.res.logit.1$coef[c("A1", "X11:A1", "X12:A1", "X13:A1", "X14:A1", "X15:A1", "X16:A1", "X17:A1", "X18:A1", "X19:A1", "X110:A1"),1]# Extract coefficients for A1 and interaction term from the model results
    results.logit[i,] = c(psi.hat.logit.1, psi.hat.logit.2)
    }, error = function(e){})
    
    ### Random forest
    tryCatch({
    #Stage 2
    model.random.2.train = randomForest(factor(A2)~ X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X110 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + X29 + X210 + A1, importance = TRUE, data = dat[s,])
    model.random.2.test = randomForest(factor(A2)~ X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X110 + X21 + X22 + X23+X24+ X25+X26+X27+X28+ X29+X210+A1,  importance = TRUE,data=dat[-s,])
    # Make predictions for A2 using the test and train models
    pred.random.2 = dat$A2
    pred.random.2[s] = predict(model.random.2.test, type = "prob", dat[s,])[,2]
    pred.random.2[-s] = predict(model.random.2.train, type = "prob", dat[-s,])[,2]
    w.random.2 = abs(dat$A2 - pred.random.2)# Calculate weights based on the absolute difference between true and predicted A2
    model.res.random.2 = summary(glm(Y~X11+ X12+ X13+X14+ X15+ X16+ X17+ X18+X19+ X110 + X21+ X22+ X23+X24+ X25+X26+X27+X28+ X29+X210
                                    +A1*(1+X11+ X12+ X13+X14+ X15+ X16+ X17+ X18+X19+ X110 + X21+ X22+ X23+X24+ X25+X26+X27+X28+ X29+X210)
                                    +A2*(1+ X21+ X22+ X23+X24+ X25+X26+X27+X28+ X29+X210), weights = w.random.2, family = gaussian, data = dat)) # Fit a weighted linear regression model for Y using A2 predictions and other features
    psi.hat.random.2 = model.res.random.2$coef[c("A2",  "X21:A2", "X22:A2", "X23:A2", "X24:A2", "X25:A2", "X26:A2", "X27:A2", "X28:A2", "X29:A2", "X210:A2"),1]# Extract coefficients related to A2 and interaction term 
    #Stage1
    model.random.1.train = randomForest(factor(A1) ~X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X110, importance = TRUE, data = dat[s,])# Train a random forest model for predicting A1
    model.random.1.test = randomForest(factor(A1) ~ X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X110, importance = TRUE, data = dat[-s,])
    # Make predictions for A1 using the test and train models
    pred.random.1 = dat$A1
    pred.random.1[s] = predict(model.random.1.test, type = "prob", dat[s,])[,2]
    pred.random.1[-s] = predict(model.random.1.train, type = "prob", dat[-s,])[,2]
    w.random.1 = abs(dat$A1 - pred.random.1)# Calculate weights based on the absolute difference between true and predicted A1
    A2opt.hat.random = 1*(L2%*%psi.hat.random.2 > 0)# Determine the optimal A2 values based on learned coefficients from Stage 2
    mu2.hat.random = ( A2opt.hat.random - dat$A2)*L2%*%psi.hat.random.2 # Calculate the adjusted predictions for Y using A1 predictions and optimal A2 values
    Yopt.hat.random.2 = dat$Y + mu2.hat.random
    model.res.random.1 = summary(glm(Yopt.hat.random.2 ~ X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X110 
              + A1*(1 + X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X110), weights = w.random.1, family = gaussian, data = dat))# Fit a weighted linear regression model for adjusted Y using A1 predictions and other features
    psi.hat.random.1 = model.res.random.1$coef[c("A1", "X11:A1", "X12:A1", "X13:A1", "X14:A1", "X15:A1", "X16:A1", "X17:A1", "X18:A1", "X19:A1", "X110:A1"),1]# Extract coefficients related to A1 and interaction term 
    results.random[i,] = c(psi.hat.random.1, psi.hat.random.2)
    }, error = function(e){})
    
    ### Naive bayes model 
    tryCatch({
    ###Stage 2
    model.bayes.2.train = naiveBayes(A2~ X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X110 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + X29 + X210 + A1, data = dat[s,])# Train Naive Bayes model for A2 using  the training set
    model.bayes.2.test = naiveBayes(A2~ X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X110 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + X29 + X210 + A1, data = dat[-s,])# Train Naive Bayes model for A2 using the test set
    # Make predictions for A2 on the entire dataset using the test and train models
    pred.bayes.2 = dat$A2
    pred.bayes.2[s] = predict(model.bayes.2.test, dat[s,], "raw")[,2]
    pred.bayes.2[-s] = predict(model.bayes.2.train, dat[-s,], "raw")[,2]
    # Calculate weights based on the absolute difference between true A2 values and predicted values
    w.bayes.2 = abs(dat$A2 - pred.bayes.2)
    # Fit a Gaussian model for Y using X1, X2, A1, and predicted A2 values, with weights
    model.res.bayes.2 = summary(glm(Y~X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X110 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + X29 + X210
            + A1*(1 + X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X110 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + X29 + X210)
            + A2*(1 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + X29 + X210), weights = w.bayes.2, family = gaussian, data = dat))
    psi.hat.bayes.2 =  model.res.bayes.2$coef[c("A2", "X21:A2", "X22:A2", "X23:A2", "X24:A2", "X25:A2", "X26:A2", "X27:A2", "X28:A2", "X29:A2", "X210:A2"),1]# Extract coefficients for A2 and interaction term 
    ###Stage 1
    model.bayes.1.train = naiveBayes(A1~ X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X110, data = dat[s,])# Train Naive Bayes model for A1 using  the training set
    model.bayes.1.test = naiveBayes(A1~ X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X110, data = dat[-s,])# Train Naive Bayes model for A1 using the test set
    # Make predictions for A1 on the entire dataset using the test and train models
    pred.bayes.1 = dat$A1
    pred.bayes.1[s] = predict(model.bayes.1.test, dat[s,], type = "raw")[,2]
    pred.bayes.1[-s] = predict(model.bayes.1.train, dat[-s,], type = "raw")[,2]
    # Calculate weights based on the absolute difference between true A1 values and predicted values
    w.bayes.1=abs(dat$A1 - pred.bayes.1)
    A2opt.hat.bayes = 1*(L2%*%psi.hat.bayes.2 > 0)# Determine the optimal A2 using the learned coefficients for A2 and L2:A2
    mu2.hat.bayes = (A2opt.hat.bayes - dat$A2)*L2%*%psi.hat.bayes.2 # Calculate the optimal mu2 using the optimal A2 and coefficients for L2:A2
    Yopt.hat.bayes.2 = dat$Y + mu2.hat.bayes # Calculate Yopt by adding mu2 to the original Y
    model.res.bayes.1 = summary(glm(Yopt.hat.bayes.2 ~ X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X110 + A1*(1 + X11+ X12+ X13+X14+ X15+ X16+ X17+ X18+X19+ X110), weights = w.bayes.1, family = gaussian, data = dat))# Fit a Gaussian model for Yopt  with weights
    psi.hat.bayes.1 = model.res.bayes.1$coef[c("A1", "X11:A1", "X12:A1", "X13:A1", "X14:A1", "X15:A1", "X16:A1", "X17:A1", "X18:A1", "X19:A1", "X110:A1"),1]# Extract coefficients for A1 and interaction term 
    results.bayes[i,] = c(psi.hat.bayes.1, psi.hat.bayes.2)
    }, error = function(e){})
    
    ### Neural networks
    tryCatch({
    #Stage 2
    model.neurone.2.train = nnet(A2~X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X110 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + X29+X210 + A1, size = 3, decay = 0.1, data = dat[s,])# Train Neural Network model for A2 using the training set
    model.neurone.2.test = nnet(A2~X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X110 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + X29 + X210 + A1, size = 3, decay = 0.1, data = dat[-s,])# Train Neural Network model for A2 using the test set
    # Make predictions for A2 on the entire dataset using the test and train models
    pred.neurone.2 = dat$A2
    pred.neurone.2[s] = predict(model.neurone.2.test,type = "raw", dat[s,] )
    pred.neurone.2[-s] = predict(model.neurone.2.train,type = "raw", dat[-s,] )
    # Calculate weights based on the absolute difference between true A2 values and predicted values
    w.neurone.2 = abs(dat$A2-pred.neurone.2)
    model.res.neurone.2 = summary(glm(Y~X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X110 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + X29 + X210 
                                      + A1*(1 + X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X110 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + X29 + X210) 
                                      + A2*(1+ X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + X29 + X210), weights =  w.neurone.2, family = gaussian, data = dat))# Fit a Gaussian model for Y using X1, X2, A1, and predicted A2 values, with weights
    psi.hat.neurone.2 = model.res.neurone.2$coef[c("A2", "X21:A2", "X22:A2", "X23:A2", "X24:A2", "X25:A2", "X26:A2", "X27:A2", "X28:A2", "X29:A2", "X210:A2"),1]# Extract coefficients for A2 and interaction term "X21:A2", "X22:A2", "X23:A2", "X24:A2", "X25:A2"
    ### Stage 1
    model.neurone.1.train = nnet(A1~ X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X110, size = 3, decay = 0.1, data = dat[s,])# Train Neural Network model for A1 using the training set
    model.neurone.1.test = nnet(A1~ X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X110, size = 3, decay = 0.1, data = dat[-s,])# Train Neural Network model for A1 using the test set
    # Make predictions for A1 on the entire dataset using the test and train model
    pred.neurone.1 = dat$A1
    pred.neurone.1[s] = predict(model.neurone.1.test, type = "raw", dat[s,] )
    pred.neurone.1[-s] = predict(model.neurone.1.train, type = "raw", dat[-s,] )
    # Calculate weights based on the absolute difference between true A1 values and predicted values
    w.neurone.1=abs(dat$A1-pred.neurone.1)
    A2opt.hat.neurone = 1*(L2%*%psi.hat.neurone.2 > 0)# Determine the optimal A2 using the learned coefficients for A2 and X2:A2
    mu2.hat.neurone = (A2opt.hat.neurone - dat$A2)*L2%*%psi.hat.neurone.2 # Calculate the optimal mu2 using the optimal A2 and coefficients for X2:A2
    Yopt.hat.neurone.2 = dat$Y + mu2.hat.neurone # Calculate Yopt by adding mu2 to the original Y
    model.res.neurone.1 = summary(glm(Yopt.hat.neurone.2  ~ X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X110 + A1*(1 + X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X110), weights =  w.neurone.1, family = gaussian, data = dat))# Fit a Gaussian model for Yopt using X1 and A1, with weights
    psi.hat.neurone.1 = model.res.neurone.1$coef[c("A1", "X11:A1", "X12:A1", "X13:A1", "X14:A1", "X15:A1", "X16:A1", "X17:A1", "X18:A1", "X19:A1", "X110:A1"),1] # Extract coefficients for A1 and interaction term 
    results.neural[i,] = c(psi.hat.neurone.1, psi.hat.neurone.2)
    }, error = function(e){})
    
    ### SVM
    tryCatch({
    ### Stage 2
    model.SVM.2.train = svm(A2~X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X110 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + X29 + X210 + A1, data = dat[s,], kernel = "linear")# Train SVM model for A2 
    model.SVM.2.test = svm(A2~ X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X110 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + X29 + X210 + A1, data = dat[-s,], kernel = "linear")# Train SVM model for A2 
    # Make predictions for A2 on the entire dataset using the test and train models
    pred.SVM.2 = dat$A2
    pred.SVM.2[s] = predict(model.SVM.2.test, probability = TRUE,  newdata = dat[s,])
    pred.SVM.2[-s] = predict(model.SVM.2.train, probability = TRUE,  newdata = dat[-s,])
    w.SVM.2 = abs(dat$A2 - pred.SVM.2)# Calculate weights based on the absolute difference between true A2 values and predicted values
    model.res.SVM.2 = summary(glm(Y~X11+ X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X110 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + X29 + X210 
                                  + A1*(1 + X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X110 + X21+ X22+ X23 + X24 + X25 + X26 + X27 + X28 + X29 + X210) 
                                  + A2*(1 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + X29 + X210), weights=  w.SVM.2, family = gaussian, data = dat))# Fit a Gaussian model for Y with weights
    psi.hat.SVM.2 = model.res.SVM.2$coef[c("A2",  "X21:A2", "X22:A2", "X23:A2", "X24:A2", "X25:A2", "X26:A2", "X27:A2", "X28:A2", "X29:A2", "X210:A2"),1]# Extract coefficients for A2 and interaction term
    #Stage 1
    model.SVM.1.train = svm(A1~ X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X110, data = dat[s,], kernel = "linear")# Train SVM model for A1 using the training set
    model.SVM.1.test = svm(A1~ X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X110 , data = dat[-s,], kernel = "linear")# Train SVM model for A1 using  the test set
    # Make predictions for A1 on the entire dataset using the test and train models
    pred.SVM.1 = dat$A1
    pred.SVM.1[s] = predict(model.SVM.1.test, probability=TRUE,  newdata = dat[s,])
    pred.SVM.1[-s] = predict(model.SVM.1.train, probability=TRUE,  newdata = dat[-s,])
    w.SVM.1 = abs(dat$A1-pred.SVM.1)# Calculate weights based on the absolute difference between true A1 values and predicted values
    A2opt.hat.SVM = 1*(L2%*%psi.hat.SVM.2 > 0)# Determine the optimal A2 using the learned coefficients for A2 and L2:A2
    mu2.hat.SVM = (A2opt.hat.SVM - dat$A2)*L2%*%psi.hat.SVM.2# Calculate the optimal mu2 using the optimal A2 and coefficients for L2:A2
    Yopt.hat.SVM.2 = dat$Y + mu2.hat.SVM# Calculate Yopt by adding mu2 to the original Y
    model.res.SVM.1 = summary(glm(Yopt.hat.SVM.2~ X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X110+ A1*(1 + X11+ X12+ X13+X14+ X15+ X16+ X17+ X18+X19+ X110), weights =  w.SVM.1, family = gaussian, data = dat))# Fit a Gaussian model for Yopt with weights
    psi.hat.SVM.1 = model.res.SVM.1$coef[c("A1", "X11:A1", "X12:A1", "X13:A1", "X14:A1", "X15:A1", "X16:A1", "X17:A1", "X18:A1", "X19:A1", "X110:A1"),1] # Extract coefficients for A1 and interaction term 
    results.SVM[i,] = c(psi.hat.SVM.1,psi.hat.SVM.2)
    }, error = function(e){})
    
    ### dWOLS_SL
    tryCatch({
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
    lm.dwols2 = lm(Y ~X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X110 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + X29 + X210 
              + A1*(1 + X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X110 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + X29 + X210) 
              + A2*(1 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + X29 + X210), weights = w2, data = dat)
    psi2.hat.dwols = lm.dwols2$coef[c("A2", "X21:A2", "X22:A2", "X23:A2", "X24:A2", "X25:A2", "X26:A2", "X27:A2", "X28:A2", "X29:A2", "X210:A2")]
    
    ### Stage 1
    # Estimate treatment effect for A1 using Super Learner
    mu.t.hat1 = SuperLearner_function(Y = A1[s], X = H1[s,, drop = FALSE], family = "binomial")
    mu.t.hat2 = SuperLearner_function(Y = A1[-s], X = H1[-s,, drop = FALSE], family = "binomial")
    mu.t.h = dat$A1
    mu.t.h[s] = predict(mu.t.hat2, type = "raw",
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
    lm.dwols1 = lm(Yopt.hat ~ X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X110 + A1*(1 + X11 + X12 + X13 + X14 + X15+ X16 + X17 + X18 + X19 + X110), weights = w1, data = dat)
    psi1.hat.dwols = lm.dwols1$coef[c("A1", "X11:A1", "X12:A1", "X13:A1", "X14:A1", "X15:A1", "X16:A1", "X17:A1", "X18:A1", "X19:A1", "X110:A1")]
    results.SL[i,] = c(psi1.hat.dwols, psi2.hat.dwols)
    }, error = function(e){})
    
    #results.RQL[i,]=c(psi1.RQL, psi2.RQL)
    print(data.frame(n, i, time = Sys.time()))
    
    # Save results based on the scenario
    file.name = paste("LargeDimensionality_TwoTimeStudy5_V2_scenario_", scenario, "_", n, ".Rdata", sep="")
    save(results.logit, results.random, results.bayes, results.neural, results.SVM,
         results.SL, file = file.name)
    
  }
  
}

# Number of repetitions  
nrep = 1000

# List of scenarios
scenarios = c("simple", "medium", "complex")


# Loop through scenarios and sample sizes
for (n in c(300, 1000)){
  for (scenario in scenarios)  {
    # Set a specific seed for random number generation
    set.seed(43578578)
    run_scenario(n, scenario)
  }
}

