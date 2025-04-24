
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
psi1 = c(-3, 1, -0.2, 0.3, 1,0.5, 1, -0.2, 0.3, 1, 0.5)
psi2 = c(-0.5, 1, -0.2, 0.3, 1, 0.5, 1, -0.2, 0.3, 1, 0.5)

# Number of repetitions  
nrep = 1000

# Function to run  for each scenarios for each size
run_scenario = function(n, scenario) {

  for(i in 1:nrep){
#     if(i < ...) next;
    seeds[[i]] = .Random.seed;
    
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
      A1 = rbinom(n, 1, p = expit(-4 - 1.5*X11 + 5.5 * X13 - 5.5*X14 - 1.5*X15 + 2.5*X16 + 2.5*X17 + X18 - 0.5*X19 + X110))
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
      X210 = 0.5*X23 + 0.5*X21 + A1+ runif(n, min = 0, max = 1)
      X2= cbind(1, X21, X22, X23, X24, X25, X26, X27, X28, X29, X210)
      
      # treatment at stage 2 
      A2 = rbinom(n, 1, p = expit(15 - X21 - X23 - 3.5*X24 + X25 - 2.5*X26 - X27 - X28 - 0.5*X29 - X210))
      # Optimal treatment assignment stage 2
      A2opt = 1*(X2%*%psi2 > 0)
      mu2 = (A2opt - A2)*X2%*%psi2
      #Outcome generation
      Y = rnorm(n, mean = X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X110 + X21
                + X22 + X23 + X24 + X25 + X26 + X27 + X28 + X29 + X210 - mu1 - mu2, 1)
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
      A1 = rbinom(n, 1, p = expit(5 - 1.5*X11+0.5*X12-0.5*X14 + 0.5*X13 - 1.5*X15 - X16 - X17 + X18
                                  -0.5*X19 + X110  + 0.5*X13*X13 + X15*X15 
                                  -0.5*X18*X18 + X19*X19 - 0.5*X110*X110))
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
      A2 = rbinom(n, 1, p = expit( 5- X21 - X23 - 1*X24 + X25 -0.5*X26 - X27 
                                   - X28 - 0.5*X29 - X210 + 0.5*X21*X21 + 0.5*X23*X23 
                                   + 0.5*X25*X25 -0.1*X26*X26  
                                   - 0.5*X28*X28 + X29*X29 - 0.5*X210*X210))
      # Optimal treatment assignment stage 2
      A2opt = 1*(X2%*%psi2 > 0)
      mu2 = (A2opt - A2)*X2%*%psi2
      #Outcome generation
      Y = rnorm(n, mean =  X11 + X13 - X14 + X15 - X16 + X17 - X18 - 0.5*X19 - X110 
                + X13*X13 + 0.5*X15*X15 - 0.5*X16*X16 + 0.5*X17*X17 - 0.5*X18*X18 + 0.25*X19*X19 - 0.25*X110*X110 
                - X21 - X23 -1.5*X24 + X25 - X26 + X27 - X28 - 0.5*X29 - X210 
                + 0.5*X21*X21 + 0.25*X23*X23 + 0.25*X25*X25 - 0.5*X26*X26 + 0.5*X27*X27 - 0.5*X28*X28
                + 0.5*X29*X29 - 0.25*X210*X210 - mu1 - mu2, 1)
      
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
      A1 = rbinom(n, 1, p =  expit(4 - 1.5*X11+0.5*X12 + sin(X13)-X14+cos(X15)-X16 
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
      X2= cbind(1, X21, X22, X23, X24, X25, X26, X27, X28, X29, X210)
      # treatment at stage 2
      A2 = rbinom(n, 1, p = expit(- 0.5*X21 + sin(X23) -X24 + cos(X25) + X21*X21 
                                  + sin(cos(X23*X23)) + abs(sin(X25*X25)) -0.1*X26*X26 
                                  -cos(X27*X27) -log(X28*X28) + X29*X29 - exp(tan(X210*X210))))
      # Optimal treatment assignment stage 2
      A2opt = 1*(X2%*%psi2 > 0)
      mu2 = (A2opt - A2)*X2%*%psi2
      #Outcome generation
      Y = rnorm(n, mean = 1.5 + 1.5*X11 + sin(X13) - 1.5*X14 + cos(X15) + 1.5*sin(cos(X13*X13))
                + abs(sin(X15*X15))- 0.1*X16*X16 + cos(X17*X17) - 0.5*log(X18*X18 + 1) 
                + 0.5*X19*X19 - cos(X110*X110) - X21 + sin(X23) - X24 + cos(X25)
                + 0.5*X21*X21 + 1.5*sin(cos(X23*X23)) - abs(sin(X25*X25))
                - 0.1*X26*X26 + cos(X27*X27) - 0.25*log(X28*X28 + 1) 
                + 0.5*X29*X29 - 1.5*cos(X210*X210) - mu1-mu2, 1)
      
    }    
    #Complete data  
    dat = data.frame(X11, X12, X13, X14, X15, X16, X17, X18, X19, X110, X21, X22, X23, X24, X25, X26, X27, X28, X29, X210, A1, A2, Y)
    
    #Data sets
    # Create a data frame H1 
    H1 = data.frame(X11, X12, X13, X14, X15, X16, X17, X18, X19, X110)
    # Create a data frame H2 
    H2 = data.frame(A1, X11, X12, X13, X14, X15, X16, X17, X18, X19, X110, X21, X22, X23, X24, X25, X26, X27, X28, X29, X210) 
    # Create a matrix L2 with two columns: a column of ones and X2
    L2=cbind(1,X21, X22, X23, X24, X25, X26, X27, X28, X29, X210)
    # Generate a random sample of indices s, representing the indices of the selected rows
    # The sample is obtained by randomly selecting n/2 indices from the range 1 to n and sorting them
    s=sort(sample(1:n, n/2))
    
    #RQL
    ## Stage 2
     mu.t.hat1 = SuperLearner_function(Y = A2[s], X = H2[s,, drop = FALSE], family = "binomial")
     mu.t.hat2 = SuperLearner_function(Y = A2[-s], X = H2[-s,, drop = FALSE], family = "binomial")
     mu.t.h = dat$A2
     mu.t.h[s] = predict(mu.t.hat2, type = "raw", newdata = data.frame(H2[s,, drop = FALSE]))$pred
     mu.t.h[-s] = predict(mu.t.hat1, type = "raw", newdata = data.frame(H2[-s,, drop = FALSE]))$pred
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
     lm.Qp2 = lm(Y.tilde ~ A.tilde + X.tilde[,12:21])
     psi2.RQL =lm.Qp2$coef[-1]
     
    # # Stage 1
     blip = apply(cbind(as.matrix(cbind(1, 0 - mu.t.h, H2[,12:21]*(0 - mu.t.h)))%*%coef(lm.Qp2) -
                          cbind(1, A.tilde, X.tilde[,12:21])%*%coef(lm.Qp2),
                        as.matrix(cbind(1, 1 - mu.t.h, H2[,12:21]*(1 - mu.t.h)))%*%coef(lm.Qp2) -
                         cbind(1, A.tilde, X.tilde[,12:21])%*%coef(lm.Qp2)),1, max)
     s2.yopt<-as.numeric(Y) + blip
     mu.t.hat1 = SuperLearner_function(Y = A1[s], X = H1[s,, drop = FALSE], family = "binomial")
     mu.t.hat2 = SuperLearner_function(Y = A1[-s], X = H1[-s,, drop = FALSE], family = "binomial")
     mu.t.h = dat$A1
     mu.t.h[s] = predict(mu.t.hat2, type = "raw", newdata = data.frame(H1[s,, drop = FALSE]))$pred
     mu.t.h[-s] = predict(mu.t.hat1, type = "raw", newdata = data.frame(H1[-s,, drop = FALSE]))$pred
     mu.y.hat1 =SuperLearner_function(Y = s2.yopt[s], X = H1[s,, drop = FALSE],family = "gaussian")
     mu.y.hat2 =SuperLearner_function(Y = s2.yopt[-s], X = H1[-s,, drop = FALSE],family = "gaussian")
     mu.y.h =dat$Y
     mu.y.h[s] =predict(mu.y.hat2, type = "raw", newdata = data.frame(H1[s,, drop = FALSE]))$pred
     mu.y.h[-s] =predict(mu.y.hat1, type = "raw", newdata = data.frame(H1[-s,, drop = FALSE]))$pred
     Y.tilde =as.numeric(s2.yopt) - as.numeric(mu.y.h)
     A.tilde =dat$A1 - mu.t.h
     X.tilde<-scale(H1, scale = FALSE, center = FALSE)*A.tilde
     lm.Qp1 =lm(Y.tilde ~ A.tilde + X.tilde)
     psi1.RQL=lm.Qp1$coef[-1]
    
    
    #Result matrix 
    results.RQL[i,]=c(psi1.RQL, psi2.RQL)
    print(data.frame(n, i, time = Sys.time()))
    
    # Save results based on the scenario
    file.name = paste("RQL_TwoTimeStudy5_scenario_", scenario, "_", n, ".Rdata", sep="")
    save(results.RQL, seeds, file = file.name)
    
  }
  
}

# Number of repetitions
nrep = 1000
# Set a specific seed for random number generation
seeds = list()
results.RQL =  matrix(NA, nrow = nrep, ncol = 22)
# load("...")

# List of scenarios
# For each execution, the scenario needs to be changed. "simple", "medium", "complex"
scenarios = c("simple", "medium", "complex")

for (n in c(1000)){
  for (scenario in "complex")  {
  set.seed(45631566)
#    .Random.seed = seeds[[...]]
    run_scenario(n, scenario)
  }
}



