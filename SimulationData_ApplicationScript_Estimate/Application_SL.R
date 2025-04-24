
# To use the simulated dataset
# Set the working directory 
setwd("...")

# Load the simulated dataset from a CSV file
dat = read.csv("Simulation_data_Application.csv") 

cov_names = c("cage", "ctabac", "cbmi", "meno", "grade", "staging_rvalue","HR" ,
              "type_chx", "histfm_1degre", "prise_hts", "ind_chi", "ind_rad", "ind_herceptin",
              "year_rec")

dat[cov_names] = lapply(dat[cov_names], factor)

# The sample is obtained by randomly selecting n/2 indices from the range 1 to n and sorting them
set.seed(43578578)
n = nrow(dat)
s = sort(sample(1:n, n/2))

SuperLearner_function = function(Y, X, family) {
  return(SuperLearner(Y = Y, X = X, family = family,
                      SL.library = c("SL.glm", "SL.randomForest", "SL.nnet",
                                     "SL.glm.interaction")))}

H = dat[,c("cage" , "cbmi" , "ctabac" , "meno","grade" , "staging_rvalue" ,
           "HR" , "type_chx" , "histfm_1degre" , "prise_hts" ,
           "ind_chi" , "ind_rad","ind_herceptin" , "year_rec")]

tryCatch({
mu.t.hat1 = SuperLearner_function(Y = dat$A[s], X = H[s,, drop = FALSE], family = "binomial")
mu.t.hat2 = SuperLearner_function(Y = dat$A[-s], X = H[-s,, drop = FALSE], family = "binomial")
mu.t.h = dat$A
mu.t.h[s] = predict(mu.t.hat2, type = "raw", newdata = H[s,, drop = FALSE])$pred
mu.t.h[-s] = predict(mu.t.hat1, type = "raw", newdata = H[-s,, drop = FALSE])$pred
w.SL = abs(dat$A - mu.t.h)
lm.dwols.gauss = survreg(Surv(Y,1 - censor)~ cage + HR_bmi + ctabac + meno + grade + staging_rvalue +
                           type_chx + histfm_1degre + prise_hts +
                           ind_chi + ind_rad + ind_herceptin + year_rec+A*(1 + HR_bmi), weights = w.SL, dist="gaussian", data = dat)
lm.dwols.weibull = survreg(Surv(Y,1-censor)~ cage + HR_bmi + ctabac + meno + grade + staging_rvalue +
                             type_chx + histfm_1degre + prise_hts +
                             ind_chi + ind_rad + ind_herceptin + year_rec+A*(1 + HR_bmi), weights = w.SL, dist= "weibull", data = dat)
lm.dwols.exponential = survreg(Surv(Y,1-censor)~ cage + HR_bmi + ctabac + meno + grade + staging_rvalue +
                                 type_chx + histfm_1degre + prise_hts +
                                 ind_chi + ind_rad + ind_herceptin + year_rec+A*(1 + HR_bmi), weights = w.SL, dist= "exponential", data = dat)
lm.dwols.lognormal = survreg(Surv(Y,1 - censor)~ cage + HR_bmi + ctabac + meno + grade + staging_rvalue +
                               type_chx + histfm_1degre + prise_hts +
                               ind_chi + ind_rad + ind_herceptin + year_rec + A*(1 + HR_bmi), weights = w.SL, dist= "lognormal", data = dat)
lm.dwols.loglogistic = survreg(Surv(Y,1 - censor)~ cage + HR_bmi + ctabac + meno + grade + staging_rvalue +
                                type_chx + histfm_1degre + prise_hts +
                                ind_chi + ind_rad + ind_herceptin + year_rec+A*(1 + HR_bmi), weights = w.SL, dist= "loglogistic", data = dat)
},error=function(e){})

AIC(lm.dwols.gauss, k = log(nrow(dat)))
AIC(lm.dwols.weibull, k = log(nrow(dat)))
AIC(lm.dwols.exponential, k = log(nrow(dat)))
AIC(lm.dwols.lognormal, k = log(nrow(dat)))
AIC(lm.dwols.loglogistic, k = log(nrow(dat)))


# BIC - real data
## Gauss = 3540.90
## weibull= 3321.38
## exponential= 3336.65
## lognormal= 3321.27
## loglogistic= 3308.14 ok


### m out of n boostrap 

output_dir=setwd("...")
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

set.seed(43578578)
B = 200
q = 0.95
K = ceiling(log(0.5) / log(q)) 


start_time_point = Sys.time()
n = nrow(dat)
s = sort(sample(1:n, n/2))

H = dat[, c("cage", "cbmi", "ctabac", "meno", "grade", "staging_rvalue",
            "HR", "type_chx", "histfm_1degre", "prise_hts",
            "ind_chi", "ind_rad", "ind_herceptin", "year_rec")]
tryCatch({
mu.t.hat1 = SuperLearner_function(Y = dat$A[s], X = H[s,, drop = FALSE], family = "binomial")
mu.t.hat2 = SuperLearner_function(Y = dat$A[-s], X = H[-s,, drop = FALSE], family = "binomial")
mu.t.h = dat$A
mu.t.h[s] = predict(mu.t.hat2, type = "raw", newdata = H[s,, drop = FALSE])$pred
mu.t.h[-s] = predict(mu.t.hat1, type = "raw", newdata = H[-s,, drop = FALSE])$pred
w.SL = abs(dat$A - mu.t.h)
model.SL = survreg(Surv(Y, 1 - censor) ~ cage + HR_bmi + ctabac + meno + grade + staging_rvalue +
                     type_chx + histfm_1degre + prise_hts +
                     ind_chi + ind_rad + ind_herceptin + year_rec + A * (1 + HR_bmi),
                   weights = w.SL, dist = "loglogistic", data = dat)

psi.hat.SL = coef(model.SL)[30] + c(0, coef(model.SL)[31:33])
},error=function(e){})
end_time_point = Sys.time()
time_point_estimation = as.numeric(difftime(end_time_point, start_time_point, units = "secs"))

#Boot
start_time_bootstrap = Sys.time()

R_j.SL = matrix(NA, nrow = 100, ncol = 4)
m_j = numeric(K+1)
x_min.SL = rep(Inf, 4)
x_max.SL = rep(-Inf, 4)
psi.jb.SL = list()
m.SL = numeric(4)
jopt.SL = numeric(4)
V.SL = matrix(NA, nrow = K + 1, ncol = 4)
Vopt.SL = numeric(4)

for (j in 0:K) { 
  
  m_j[j+1] = floor(q^j * n)
  psi.hat.SL.jb = matrix(NA, nrow = B, ncol = 4)
  
  for (b in 1:B) {
    indices = sample(1:n, m_j[j+1], replace = TRUE)
    datboot = dat[indices,]
    s =sort(sample(1:m_j[j+1], m_j[j+1] / 2))
    
    H = datboot[, c("cage", "cbmi", "ctabac", "meno", "grade", "staging_rvalue",
                    "HR", "type_chx", "histfm_1degre", "prise_hts",
                    "ind_chi", "ind_rad", "ind_herceptin", "year_rec")]
    tryCatch({
    mu.t.hat1 = SuperLearner_function(Y = datboot$A[s], X = H[s,, drop = FALSE], family = "binomial")
    mu.t.hat2 = SuperLearner_function(Y = datboot$A[-s], X = H[-s,, drop = FALSE], family = "binomial")
    mu.t.h=datboot$A
    mu.t.h[s] = predict(mu.t.hat2, type = "raw", newdata = H[s,, drop = FALSE])$pred
    mu.t.h[-s] = predict(mu.t.hat1, type = "raw", newdata = H[-s,, drop = FALSE])$pred
    datboot$w.SL = abs(datboot$A - mu.t.h)
    dat_subset = datboot[datboot$w.SL > 0, ]
    model.SL.boot = survreg(Surv(Y, 1 - censor) ~ cage + HR_bmi + ctabac + meno + grade + staging_rvalue +
                              type_chx + histfm_1degre + prise_hts +
                              ind_chi + ind_rad + ind_herceptin + year_rec + A * (1 + HR_bmi),
                            weights = w.SL, dist = "loglogistic", data = dat_subset)
    psi.hat.SL.jb[b,] = coef(model.SL.boot)[30] + c(0, coef(model.SL.boot)[31:33])
    },error=function(e){})
    #Save boostrap
    save(psi.hat.SL.jb, file = "bootstrap_estimate_SL.Rdata")
    print(data.frame(b = b, j = j, time = Sys.time()))  
    
  }
  
  V.SL[j+1, ] = colVars(psi.hat.SL.jb)
  
  diff.SL = sqrt(m_j[j+1]) * (psi.hat.SL.jb - psi.hat.SL)

  x_min.SL = pmin(x_min.SL, apply(diff.SL, 2, min, na.rm = TRUE))  
  x_max.SL = pmax(x_max.SL, apply(diff.SL, 2, max, na.rm = TRUE))
   psi.jb.SL[[j+1]] =psi.hat.SL.jb
  
}

supR.SL = matrix(NA, nrow = K, ncol = 4)
ci_list.SL = vector("list", ncol(supR.SL))

for(p in 1:4){
  x_grid.SL = seq(x_min.SL[p], x_max.SL[p], length.out = 100)
  
  for(j in 1:K){
    for(x in 1:length(x_grid.SL)){
      R_j.SL[x,p] = mean((sqrt(m_j[j])*(psi.jb.SL[[j]][,p] - psi.hat.SL[p]))<=x_grid.SL[x], na.rm = TRUE) -
        mean((sqrt(m_j[j+1])*(psi.jb.SL[[j+1]][,p] - psi.hat.SL[p]))<=x_grid.SL[x], na.rm = TRUE)
    }
    supR.SL[j,p] = max(abs(R_j.SL[,p]))
  }
  jopt.SL[p] = which.min(supR.SL[,p])
  m.SL[p] = m_j[which.min(supR.SL[,p])]
  Vopt.SL[p] = V.SL[which.min(supR.SL[,p]),p]
}

beta.SL = numeric(4)
for(p in 1:4){
  logVm.SL = log(V.SL[, p])
  logm = -2*log(m_j)
  beta.SL[p] = coef(lm(logVm.SL ~ logm))[2]
  
}

res.ll.SL = psi.hat.SL - 1.96*(m.SL/n)**beta.SL*sqrt(Vopt.SL)
res.ul.SL = psi.hat.SL + 1.96*(m.SL/n)**beta.SL*sqrt(Vopt.SL)
end_time_bootstrap = Sys.time()  

time_bootstrap = as.numeric(difftime(end_time_bootstrap, start_time_bootstrap, units = "secs"))

application_estimate_path = file.path(output_dir, "Estimate_SL.Rdata")
execution_times_path = file.path(output_dir, "Execution_times_SL.Rdata")
save(m_j,beta.SL, m.SL,Vopt.SL, psi.hat.SL, res.ll.SL, res.ul.SL, file = application_estimate_path)
save(time_point_estimation, time_bootstrap, file = execution_times_path)
print(data.frame(time = end_time_bootstrap))

# Result
load("bootstrap_estimate_SL.Rdata")
load("Estimate_SL.Rdata")
load("Execution_times_SL.Rdata")



