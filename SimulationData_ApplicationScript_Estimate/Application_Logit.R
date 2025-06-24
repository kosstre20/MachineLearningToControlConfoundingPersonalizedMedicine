
#------ To use the simulated dataset
# Set the working directory 
setwd("...")

# Load the simulated dataset from a CSV file
dat = read.csv("Simulation_data_Application.csv")

cov_names = c("cage", "ctabac", "cbmi", "meno", "grade", "staging_rvalue","HR" ,
              "type_chx", "histfm_1degre", "prise_hts", "ind_chi", "ind_rad", "ind_herceptin",
              "year_rec")

dat[cov_names] = lapply(dat[cov_names], factor)

set.seed(43578578)

#-------Distribution

model.logit = glm(A ~ cage + cbmi + ctabac + meno + grade + staging_rvalue +
                        HR + type_chx + histfm_1degre + prise_hts +
                        ind_chi + ind_rad + ind_herceptin + year_rec, family = "binomial", data = dat ) 
pred.logit = predict(model.logit, type= "response", dat)
w.logit = abs(dat$A - pred.logit)

model.res.logit.gauss = survreg(Surv(Y,1 - censor)~ cage + HR_bmi + ctabac + meno + grade + staging_rvalue +
                                type_chx + histfm_1degre + prise_hts +
                                ind_chi + ind_rad + ind_herceptin + year_rec+A*(1 + HR_bmi), weights = w.logit,dist = "gaussian", data = dat)

model.res.logit.weibull = survreg(Surv(Y,1 - censor)~ cage + HR_bmi + ctabac + meno + grade + staging_rvalue +
                                  type_chx + histfm_1degre + prise_hts +
                                  ind_chi + ind_rad + ind_herceptin + year_rec+A*(1 + HR_bmi), weights = w.logit,dist = "weibull", data = dat)


model.res.logit.exponential = survreg(Surv(Y, 1 - censor)~ cage + HR_bmi + ctabac + meno + grade + staging_rvalue +
                                      type_chx + histfm_1degre + prise_hts +
                                      ind_chi + ind_rad + ind_herceptin + year_rec + A*(1 + HR_bmi), weights = w.logit,dist = "exponential", data = dat)


model.res.logit.lognormal = survreg(Surv(Y, 1 - censor)~ cage + HR_bmi + ctabac + meno + grade + staging_rvalue +
                                    type_chx + histfm_1degre + prise_hts +
                                    ind_chi + ind_rad + ind_herceptin + year_rec + A*(1 + HR_bmi), weights = w.logit,dist = "lognormal", data = dat)


model.res.logit.loglogistic = survreg(Surv(Y, 1 - censor)~ cage + HR_bmi + ctabac + meno + grade + staging_rvalue +
                                      type_chx + histfm_1degre + prise_hts +
                                      ind_chi + ind_rad + ind_herceptin + year_rec + A*(1 + HR_bmi), weights = w.logit,dist = "loglogistic", data = dat)

AIC(model.res.logit.gauss, k = log(nrow(dat)))
AIC(model.res.logit.weibull, k = log(nrow(dat)))
AIC(model.res.logit.exponential, k = log(nrow(dat)))
AIC(model.res.logit.lognormal, k = log(nrow(dat)))
AIC(model.res.logit.loglogistic, k = log(nrow(dat)))

#  BIC - real data
## Gauss = 3630.609
## weibull= 3411.736
## exponential= 3430.399
## lognormal= 3412.403
## loglogistic= 3399.614 ok



#----------Estimate---

bootstrap_function = function(dat, repetitions, save_path= "boostrap_estimate_logit") {
  n = nrow(dat)
  t = matrix(NA, nrow = repetitions, ncol = 4)  #  bootstrap (t)
  t0 = numeric(4)  #  original data (t0)
  
  start_time_point <- Sys.time()
  
  ### Logit
  model.logit = glm(A ~ cage + cbmi + ctabac + meno + grade + staging_rvalue +
                           HR + type_chx + histfm_1degre + prise_hts +
                           ind_chi + ind_rad + ind_herceptin +year_rec, family = "binomial", data = dat ) 

  pred.logit = predict(model.logit,type = "response", dat)
  w.logit = abs(dat$A - pred.logit)
  
  model.res.logit.loglogistic = survreg(Surv(Y,1 - censor)~ cage + HR_bmi + ctabac + meno
        + grade + staging_rvalue + type_chx + histfm_1degre + prise_hts + ind_chi + ind_rad
        + ind_herceptin + year_rec + A*(1+HR_bmi), weights = w.logit,dist = "loglogistic", data = dat)
  psi.hat.logit = coef(model.res.logit.loglogistic)[30:33]
  t0[1:4] = psi.hat.logit
  
  end_time_point <- Sys.time()
  time_point_estimation <- as.numeric(difftime(end_time_point, start_time_point, units = "secs"))
  
  #Boost
  start_time_bootstrap <- Sys.time()
  for (i in 1:repetitions) {
    sr = sample(1:n, replace = TRUE)
    datboot = dat[sr,]

    # Modèle logit
    model.logit.boot = glm(A ~ cage + cbmi + ctabac + meno + grade + staging_rvalue +
                               HR + type_chx + histfm_1degre + prise_hts +
                               ind_chi + ind_rad + ind_herceptin + year_rec, 
                             family = "binomial", data = datboot)
    pred.logit.boot = predict(model.logit.boot, type = "response", datboot)
    w.logit = abs(datboot$A - pred.logit.boot)
    model.res.logit.loglogistic = survreg(Surv(Y, 1 - censor) ~ cage + HR_bmi + ctabac + meno + grade + staging_rvalue +
                                             type_chx + histfm_1degre + prise_hts +
                                             ind_chi + ind_rad + ind_herceptin + year_rec + A * (1 + HR_bmi), 
                                           weights = w.logit, dist = "loglogistic", data = datboot)
    psi.hat.logit.boot = coef(model.res.logit.loglogistic)[30:33]
    t[i, 1:4] = psi.hat.logit.boot

  }
  end_time_bootstrap <- Sys.time()  
  time_bootstrap <- as.numeric(difftime(end_time_bootstrap, start_time_bootstrap, units = "secs"))
  
  save(t0, t,time_point_estimation,time_bootstrap, file = save_path)
  return(list(t0 = t0,t = t,time_point_estimation=time_point_estimation,time_bootstrap=time_bootstrap))
}
set.seed(43578578)
boost = bootstrap_function(dat, 1000, "boostrap_estimate_logit.Rdata")

#Result

load("boostrap_estimate_logit.Rdata")

#Estimate
Estimate_logit = cbind(boost$t0[1], boost$t0[1] + boost$t0[2],  boost$t0[1] + boost$t0[3],  boost$t0[1] + boost$t0[4])
Estimate_logit

#- real data : 0.1674197 0.1552848 -0.01623169 -0.1358993

#IC
result = cbind(boost$t[,1], boost$t[,1] + boost$t[,2],  boost$t[,1] + boost$t[,3],  boost$t[,1] + boost$t[,4])
apply(result, 2, quantile, c(0.025, 0.975))

# - real data : 2.5%  -0.004487878 -0.0493057 -0.6302328 -0.6031604
# - real data:  97.5%  0.332090292  0.3350279  0.6653885  0.3405084


