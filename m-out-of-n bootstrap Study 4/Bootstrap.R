# Set working directory

setwd("...")

require(SuperLearner)

# SuperLearner function
SuperLearner_function = function(Y, X, family) {
  return(SuperLearner(Y = Y, X = X, family = family, 
                      SL.library = c("SL.glm", "SL.randomForest", "SL.nnet",
                                     "SL.glm.interaction")))
}

n = 300
nrep = 200
B = 200 # Number of bootstrap replicates
q = 0.95 # Choice of q
K = ceiling(log(0.5) / log(q)) # K such that q^K >= 0.5 
expit = plogis


results.logit = matrix(NA, nrow = nrep, ncol = 2)
seeds = list();


# Psi parameters
psi1 = c(-0.5, 1)
psi2 = c(-0.5, 1)
true.psi = c(0.504513694, 0.001588589, -0.5, 1.0)
res.m = matrix(NA, nrow = nrep, ncol = 4)
res.psi = matrix(NA, nrow = nrep, ncol = 4)
res.ll = matrix(NA, nrow = nrep, ncol = 4)
res.ul = matrix(NA, nrow = nrep, ncol = 4)
res.beta = matrix(NA, nrow = nrep, ncol = 4)
set.seed(43578578)

for (i in 1:nrep) {
  seeds[[i]] = .Random.seed
  
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
  
  dat = data.frame(X1, X2, A1, A2, Y)
  
  L =cbind(1, dat$X2)
  
  # Estimate from the original sample 
  # Create a data frame H1 with a single column X1
  H1 = data.frame(X1)
  # Create a data frame H2 with columns A1, X1, and X2
  H2 = data.frame(A1, X1, X2) 
  # Create a matrix L2 with two columns: a column of ones and X2
  L2 = cbind(1, X2)
  # Generate a random sample of indices s, representing the indices of the selected rows
  # The sample is obtained by randomly selecting n/2 indices from the range 1 to n and sorting them
  s = sort(sample(1:n,n/2))
  
  ## Stage 2
  # Estimate treatment effect for A2 using Super Learner
  mu.t.hat1 = SuperLearner_function(Y = A2[s], X = H2[s,, drop = FALSE], family = "binomial")
  mu.t.hat2 = SuperLearner_function(Y = A2[-s], X = H2[-s,, drop = FALSE], family = "binomial")
  mu.t.h = dat$A2
  mu.t.h[s] = predict(mu.t.hat2, type = "raw",newdata = data.frame(H2[s,, drop = FALSE]))$pred
  mu.t.h[-s] = predict(mu.t.hat1, type = "raw",newdata = data.frame(H2[-s,, drop = FALSE]))$pred
  # Calculate weights
  w2 = abs(dat$A2 - mu.t.h)
  # Weighted least squares regression for Stage 2
  lm.dwols2 = lm(Y ~ X1 + X2 + A1*(1 + X1 + X2) + A2*(1 + X2), weights = w2, data = dat)
  psi2.hat.dwols = lm.dwols2$coef[c("A2", "X2:A2")]
  
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
  lm.dwols1 =lm(Yopt.hat ~ X1 + A1*(1 + X1), weights = w1, data = dat)
  psi1.hat.dwols = lm.dwols1$coef[c("A1", "X1:A1")]
  
  maxR = numeric(4)
  R_j = matrix(NA, nrow = 100, ncol = 4)
  m_j = numeric(K+1)
  x_min = rep(Inf, 4)
  x_max = rep(-Inf, 4)
  psi.jb = list()
  psi.n = c(psi1.hat.dwols, psi2.hat.dwols)
  m = numeric(4)
  jopt = numeric(4)
  V = matrix(NA, nrow = K + 1, ncol = 4)
  Vopt = numeric(4)
  
  for (j in 0:K) { 
    
    m_j[j+1] = floor(q^j * n)
    psi2.jb = matrix(NA, nrow = B, ncol = 2)
    psi1.jb = matrix(NA, nrow = B, ncol = 2)

    
    for (b in 1:B) {
      
      # Bootstrap sampling
      indices =sample(1:n, m_j[j+1], replace = TRUE)
      X1b = X1[indices]
      X2b = X2[indices]
      A1b = A1[indices]
      A2b = A2[indices]
      Yb = Y[indices]
      
      # Ajustement croisé
      s = sort(sample(1:m_j[j+1], m_j[j+1] / 2))  

      # Estimate from the original sample 
      # Create a data frame H1 with a single column X1
      H1b = data.frame(X1b)
      # Create a data frame H2 with columns A1, X1, and X2
      H2b = data.frame(A1b, X1b, X2b) 
      # Create a matrix L2 with two columns: a column of ones and X2
      L2b = cbind(1, X2b)

      
      ## Stage 2
      # Estimate treatment effect for A2 using Super Learner
      mu.t.hat1 = SuperLearner_function(Y = A2b[s], X = H2b[s,, drop = FALSE], family = "binomial")
      mu.t.hat2 = SuperLearner_function(Y = A2b[-s], X = H2b[-s,, drop = FALSE], family = "binomial")
      mu.t.h = A2b
      mu.t.h[s] = predict(mu.t.hat2, type = "raw",newdata = data.frame(H2b[s,, drop = FALSE]))$pred
      mu.t.h[-s] = predict(mu.t.hat1, type = "raw",newdata = data.frame(H2b[-s,, drop = FALSE]))$pred
      # Calculate weights
      w2 = abs(A2b - mu.t.h)
      # Weighted least squares regression for Stage 2
      lm.dwols2 = lm(Yb ~ X1b + X2b + A1b*(1 + X1b + X2b) + A2b*(1 + X2b), weights = w2)
      psi2.hat.dwols =lm.dwols2$coef[c("A2b", "X2b:A2b")]
      
      ### Stage 1
      # Estimate treatment effect for A1 using Super Learner
      mu.t.hat1 = SuperLearner_function(Y = A1b[s], X = H1b[s,, drop = FALSE], family = "binomial")
      mu.t.hat2 = SuperLearner_function(Y = A1b[-s], X = H1b[-s,, drop = FALSE], family = "binomial")
      mu.t.h = A1b
      mu.t.h[s]= predict(mu.t.hat2, type = "raw",
                         newdata = data.frame(H1b[s,, drop = FALSE]))$pred
      mu.t.h[-s] = predict(mu.t.hat1, type = "raw",
                           newdata = data.frame(H1b[-s,, drop = FALSE]))$pred
      # Calculate weights
      w1 = abs(A1b - mu.t.h)
      # Optimal treatment assignment for Stage 1
      A2opt.hat = 1*(L2b%*%psi2.hat.dwols > 0)
      mu2.hat.dwols = (A2opt.hat - A2b)*L2b%*%psi2.hat.dwols
      Yopt.hat = Yb + mu2.hat.dwols
      # Weighted least squares regression for Stage 1
      lm.dwols1 =lm(Yopt.hat ~ X1b + A1b*(1 + X1b), weights = w1)
      psi1.hat.dwols = lm.dwols1$coef[c("A1b", "X1b:A1b")]
      psi2.jb[b,] = psi2.hat.dwols
      psi1.jb[b,] = psi1.hat.dwols
    }
    V[j+1, ] = apply(cbind(psi1.jb, psi2.jb), 2, var);
    
    # Calcul de la fonction de répartition empirique
    diff1 =sqrt(m_j[j+1]) * (psi1.jb - psi.n[1:2])
    diff2 =sqrt(m_j[j+1]) * (psi2.jb - psi.n[3:4])

    # Test avec ue grille
    x_min =pmin(x_min, apply(cbind(diff1, diff2), 2, min, na.rm = TRUE))
    x_max =pmax(x_max, apply(cbind(diff1, diff2), 2, max, na.rm = TRUE))
    psi.jb[[j+1]] =cbind(psi1.jb, psi2.jb)
    
  }
  supR = matrix(NA, nrow = K, ncol = 4);
  ci_list = vector("list", ncol(supR))
  for(p in 1:4){
    x_grid = seq(x_min[p], x_max[p], length.out = 100)
    for(j in 1:K){
      for(x in 1:length(x_grid)){
        R_j[x,p] = mean((sqrt(m_j[j])*(psi.jb[[j]][,p] - psi.n[p]))<=x_grid[x], na.rm = TRUE) -
          mean((sqrt(m_j[j+1])*(psi.jb[[j+1]][,p] - psi.n[p]))<=x_grid[x], na.rm = TRUE)
      }
      supR[j,p] = max(abs(R_j[,p]))
    }
    jopt[p] = which.min(supR[,p]);
    m[p] = m_j[which.min(supR[,p])];
    Vopt[p] = V[which.min(supR[,p]),p];
  }
  
  res.m[i,] = m;
  res.psi[i,] = psi.n;
  beta = numeric(4);
  for(p in 1:4){
    logVm = log(V[, p]);
    logm = -2*log(m_j);
    beta[p] = coef(lm(logVm ~ logm))[2];
  }
  res.ll[i,] = psi.n - 1.96*(m/n)**beta*sqrt(Vopt);
  res.ul[i,] = psi.n + 1.96*(m/n)**beta*sqrt(Vopt);
  res.beta[i,] = beta;

  # Save results based on the scenario
  file.name = paste("m_out_of_n_bootstrap_Study4_SL",".Rdata", sep="")
  save(res.m, res.psi, res.ll, res.ul, res.beta, seeds, file = file.name)
  print(data.frame(perc = i/nrep*100, beta = res.beta[i,], time = Sys.time()));
}




