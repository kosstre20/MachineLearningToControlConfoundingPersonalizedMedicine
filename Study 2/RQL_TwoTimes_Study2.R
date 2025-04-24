
# Set working directory
setwd("...")

# Define logistic function
expit = plogis

# SuperLearner function
SuperLearner_function = function(Y, X, family) {
  return(SuperLearner(Y = Y, X = X, family = family, 
                      SL.library = c("SL.glm", "SL.randomForest", "SL.nnet",
                                     "SL.glm.interaction", "SL.svm")))}

# Psi parameters
psi1 = c(-0.5,1)
psi2 = c(-0.5,1)


# Function to run  for each scenarios for each size
run_scenario = function(n, scenario) {
  # Initialize objects

  for(i in 1:nrep){
#  if(i < ...) next
    seeds[[i]] = .Random.seed;
    
    # Generate data based on the chosen scenario
    if(scenario == "simple"){
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
      Y = rnorm(n, mean = X1 + X2 + X1*X1 + X2*X2 - mu1 - mu2, 1)
    } else if (scenario == "complex"){
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
    tryCatch({
      # RQL
      ## Stage 2
      mu.t.hat1 =  SuperLearner_function(Y = A2[s], X = H2[s,, drop = FALSE], family = "binomial")
      mu.t.hat2 =  SuperLearner_function(Y = A2[-s], X = H2[-s,, drop = FALSE], family = "binomial")
      mu.t.h =  dat$A2
      mu.t.h[s] =  predict(mu.t.hat2, type = "raw",newdata = data.frame(H2[s,, drop = FALSE]))$pred
      mu.t.h[-s] =  predict(mu.t.hat1, type = "raw",newdata = data.frame(H2[-s,, drop = FALSE]))$pred
      mu.y.hat1 =  SuperLearner_function(Y = Y[s], X = H2[s,], family = "gaussian")
      mu.y.hat2 =  SuperLearner_function(Y = Y[-s], X = H2[-s,], family = "gaussian")
      mu.y.h = dat$Y
      mu.y.h[s] = predict(mu.y.hat2, type = "raw", newdata = data.frame(H2[s,, drop = FALSE]))$pred
      mu.y.h[-s] =  predict(mu.y.hat1, type = "raw", newdata = data.frame(H2[-s,, drop = FALSE]))$pred
      Y.tilde= dat$Y - mu.y.h
      A.tilde = dat$A2 - mu.t.h
      X.tilde = scale(H2, scale = FALSE, center = FALSE)*A.tilde
      X.tilde1 = scale(H2, scale = FALSE)*(1 - mean(A2))
      X.tilde0 = scale(H2, scale = FALSE)*(0 - mean(A2))
      X.tilde.scale = apply(X.tilde, 2, sd)
      lm.Qp2 =  lm(Y.tilde ~ A.tilde + X.tilde[,3])
      psi2.RQL = lm.Qp2$coef[-1]
      
      # Stage 1
      blip = apply(cbind(cbind(1, 0 - mu.t.h, H2[,3]*(0 - mu.t.h))%*%coef(lm.Qp2) -
                           cbind(1, A.tilde, X.tilde[,3])%*%coef(lm.Qp2),
                         cbind(1, 1 - mu.t.h, H2[,3]*(1 - mu.t.h))%*%coef(lm.Qp2) -
                           cbind(1, A.tilde, X.tilde[,3])%*%coef(lm.Qp2)),1, max)
      s2.yopt=as.numeric(Y) + blip
      mu.t.hat1 =  SuperLearner_function(Y = A1[s], X = H1[s,, drop = FALSE], family = "binomial")
      mu.t.hat2 =  SuperLearner_function(Y = A1[-s], X = H1[-s,, drop = FALSE], family = "binomial")
      mu.t.h =  dat$A1
      mu.t.h[s] =  predict(mu.t.hat2, type = "raw",newdata = data.frame(H1[s,, drop = FALSE]))$pred
      mu.t.h[-s] =  predict(mu.t.hat1, type = "raw",newdata = data.frame(H1[-s,, drop = FALSE]))$pred
      mu.y.hat1 = SuperLearner_function(Y = s2.yopt[s], X = H1[s,, drop = FALSE],family = "gaussian")
      mu.y.hat2 = SuperLearner_function(Y = s2.yopt[-s], X = H1[-s,, drop = FALSE],family = "gaussian")
      mu.y.h = dat$Y
      mu.y.h[s] = predict(mu.y.hat2, type = "raw",newdata = data.frame(H1[s,, drop = FALSE]))$pred
      mu.y.h[-s] = predict(mu.y.hat1, type = "raw", newdata = data.frame(H1[-s,, drop = FALSE]))$pred
      Y.tilde = as.numeric(s2.yopt) - as.numeric(mu.y.h)
      A.tilde = dat$A1 - mu.t.h
      X.tilde=scale(H1, scale = FALSE, center = FALSE)*A.tilde
      lm.Qp1 = lm(Y.tilde ~ A.tilde + X.tilde)
      psi1.RQL= lm.Qp1$coef[-1]
      #Result matrix 
      results.RQL[i,]=c(psi1.RQL, psi2.RQL)}, error = function(e){});
    print(data.frame(n, i, time = Sys.time()))
    
    # Save results based on the scenario
    file.name = paste("RQL_TwoTimeStudy2_scenario_", scenario, "_", n, ".Rdata", sep="")
    save(results.RQL, seeds, file = file.name)
  }
  
}

# Number of repetitions  
nrep = 1000
results.logit = results.RQL = matrix(NA, nrow = nrep, ncol = 4)
seeds = list();
# load("...")

# List of scenarios
scenarios = c("simple", "medium", "complex")
# For each execution, the scenario needs to be changed. "simple", "medium", "complex"
# Loop through scenarios and sample sizes

for (n in 1000){
  for (scenario in "complex")  {
    set.seed(43578578)
#    .Random.seed = seeds[[...]];
    run_scenario(n, scenario)
  }
}

