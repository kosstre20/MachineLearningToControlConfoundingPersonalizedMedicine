#### Loading required libraries
# install.packages("randomForest")
# install.packages("SuperLearner")
# install.packages("survey")
# install.packages("boot");
# install.packages("rms");
# install.packages("tableone")
# install.packages("dplyr")
#Library
library(randomForest)
library(SuperLearner)
library(survey)
library(boot)
library(rms)
library(tableone)
library(dplyr)

#rep
setwd("C:\\Users\\kossi\\OneDrive - Université Laval\\Application")

#### Read dataset
ds = read.csv("cms_er_bmi.csv", sep = ";", na.strings = c("."))
head(ds)
summary(ds)

#function
SuperLearner_function = function(Y, X, family) {
  return(SuperLearner(Y = Y, X = X, family = family,
                      SL.library = c("SL.glm", "SL.randomForest", "SL.nnet",
                                     "SL.glm.interaction")))}

#### Change dates to date format
# Converting date columns in the dataset to date format
ds$d_diag = as.Date(ds$d_diag, "%d-%m-%Y");
ds$date_bx_sein = as.Date(ds$date_bx_sein, "%d-%m-%Y");
ds$date_chx_sein = as.Date(ds$date_chx_sein, "%d-%m-%Y");
ds$date_recep = as.Date(ds$date_recep, "%d-%m-%Y");
ds$dfin_2011 = as.Date(ds$dfin_2011, "%d-%m-%Y");

### Restrict attention to certain participants
ds2 = ds[ds$d_diag >= as.Date("01-01-1987", "%d-%m-%Y") & ds$d_diag < as.Date("01-01-2010", "%d-%m-%Y"),];

## Data manipulation
# Defining outcome variable Y and censoring indicator
ds2$Y = ds2$suivi_2011a
ds2$censor = (ds2$statut_2011 == 1)*1

## Defining treatment variable A
ds2$A = ds2$ind_hor;

## Potential confounders
ds2$cbmi = ifelse(ds2$bmi >= 25, 1, 0);
ds2$meno = ds2$meno_statut; 
ds2$meno[ds2$meno_statut == 9] = ifelse(ds2$cage[ds2$meno_statut == 9] >= 3, 2, 1);
ds2$staging_rvalue[ds2$staging_rvalue == 9] = NA;
ds2$ctabac[ds2$ctabac %in% c(2,3,4)] = 2;
ds2$ctabac = factor(ds2$ctabac);
ds2$histfm_1degre[ds2$histfm_1degre == 9] = NA;
ds2$prise_hts[ds2$prise_hts %in% c(2,3,4)] = 2;
ds2$prise_hts[ds2$prise_hts == 9] = NA;
ds2$ind_chi[ds2$ind_chi == 9] = 0;
ds2$ind_rad[ds2$ind_rad == 9] = 1;
ds2$ind_herceptin[ds2$ind_herceptin == 9] = NA;
ds2$year = as.numeric(format(ds2$d_diag, '%Y'));
ds2$cage = factor(ds2$cage);
ds2$cbmi = factor(ds2$cbmi);
ds2$meno = factor(ds2$meno);
ds2$grade = factor(ds2$grade);
ds2$staging_rvalue = factor(ds2$staging_rvalue);
ds2$recod_oest = factor(ds2$recod_oest);
ds2$recod_prog = factor(ds2$recod_prog);
ds2$type_chx = factor(ds2$type_chx);
ds2$histfm_1degre = factor(ds2$histfm_1degre);
ds2$prise_hts = factor(ds2$prise_hts);
ds2$ind_chi = factor(ds2$ind_chi);
ds2$ind_rad = factor(ds2$ind_rad);
ds2$ind_herceptin = factor(ds2$ind_herceptin);

ds2$yearcat = ifelse(ds2$d_diag >= as.Date("01-01-1985", "%d-%m-%Y") & ds2$d_diag <= as.Date("31-12-1989", "%d-%m-%Y"), "85-89",
               ifelse(ds2$d_diag >= as.Date("01-01-1990", "%d-%m-%Y") & ds2$d_diag <= as.Date("31-12-1994", "%d-%m-%Y"), "90-94", 
                ifelse(ds2$d_diag >= as.Date("01-01-1995", "%d-%m-%Y") & ds2$d_diag <= as.Date("31-12-1999", "%d-%m-%Y"), "95-99",
                 ifelse(ds2$d_diag >= as.Date("01-01-2000", "%d-%m-%Y") & ds2$d_diag <= as.Date("31-12-2004", "%d-%m-%Y"), "00-04", "05-09"))));

ds2 = ds2 %>% 
  mutate(year_rec = recode(yearcat,
                               "85-89" = 1,
                               "90-94" = 2,
                               "95-99" = 3,
                               "00-04" = 4,
                               "05-09" = 5))

ds2$year_rec=factor(ds2$year_rec)
#Hormonal statut
ds2$HR=rep(NA, nrow(ds2))
ds2$HR=ifelse(ds2$recod_oest ==1 | ds2$recod_prog == 1, 1, 
                 ifelse(ds2$recod_oest == 2 & ds2$recod_prog == 2, 2, 9))
ds2$HR=factor(ds2$HR)
#### Remove obs w/ missing data
cov_names = c("cage", "ctabac", "cbmi", "meno", "grade", "staging_rvalue","HR" ,
              "type_chx", "HR","histfm_1degre", "prise_hts", "ind_chi", "ind_rad", "ind_herceptin",
               "yearcat")
ds3 = ds2[, c("A", "Y",  "censor",cov_names, "year_rec", "year")]

#Données sans NA
dat = ds3[complete.cases(ds3[,-2]),]

#### Descriptive statistics
#Descriptive statistics for covariate variables
print(CreateTableOne(vars = cov_names, data = dat, includeNA = TRUE), test = FALSE, smd = TRUE);

#Descriptive statistics for covariate variables stratified by treatment 'A'
print(CreateTableOne(vars = cov_names, strata = "A", data = dat), test = FALSE, smd = TRUE);

#Start here if you use the dat base available on github
#dat=  read.csv("data_Application.csv", sep = ",", na.strings = c("."))

# Combining 'HR' and 'cbmi' variables
dat$HR_bmi = paste0("HR=",dat$HR,",cbmi=",dat$cbmi)
dat$HR_bmi = factor(dat$HR_bmi)

# The sample is obtained by randomly selecting n/2 indices from the range 1 to n and sorting them
set.seed(43578578)
n=nrow(dat)
s=sort(sample(1:n,n/2))
### Logit
model.logit.train=glm(A ~ cage + cbmi + ctabac + meno + grade + staging_rvalue +
                           HR + type_chx + histfm_1degre + prise_hts +
                           ind_chi + ind_rad + ind_herceptin +year_rec, family = "binomial",data=dat[s,] ) 
model.logit.test=glm(A ~ cage + cbmi + ctabac + meno + grade + staging_rvalue +
                          HR + type_chx + histfm_1degre + prise_hts +
                          ind_chi + ind_rad + ind_herceptin + year_rec, family = "binomial",data=dat[-s,] ) 
pred.logit=dat$A 
pred.logit[s]=predict(model.logit.test,type="res",dat[s,])
pred.logit[-s]=predict(model.logit.train,type="res", dat[-s,])
w.logit=abs(dat$A-pred.logit)

model.res.logit.gauss=survreg(Surv(Y,1-censor)~ cage + HR_bmi + ctabac + meno + grade + staging_rvalue +
                                 type_chx + histfm_1degre + prise_hts +
                                 ind_chi + ind_rad + ind_herceptin + year_rec+A*(1+HR_bmi), weights= w.logit,dist = "gaussian",data=dat)

model.res.logit.weibull=survreg(Surv(Y,1-censor)~ cage + HR_bmi + ctabac + meno + grade + staging_rvalue +
                                 type_chx + histfm_1degre + prise_hts +
                                 ind_chi + ind_rad + ind_herceptin + year_rec+A*(1+HR_bmi), weights= w.logit,dist = "weibull",data=dat)


model.res.logit.exponential=survreg(Surv(Y,1-censor)~ cage + HR_bmi + ctabac + meno + grade + staging_rvalue +
                                 type_chx + histfm_1degre + prise_hts +
                                 ind_chi + ind_rad + ind_herceptin + year_rec+A*(1+HR_bmi), weights= w.logit,dist = "exponential",data=dat)


model.res.logit.lognormal=survreg(Surv(Y,1-censor)~ cage + HR_bmi + ctabac + meno + grade + staging_rvalue +
                                 type_chx + histfm_1degre + prise_hts +
                                 ind_chi + ind_rad + ind_herceptin + year_rec+A*(1+HR_bmi), weights= w.logit,dist = "lognormal",data=dat)


model.res.logit.loglogistic=survreg(Surv(Y,1-censor)~ cage + HR_bmi + ctabac + meno + grade + staging_rvalue +
                                 type_chx + histfm_1degre + prise_hts +
                                 ind_chi + ind_rad + ind_herceptin + year_rec+A*(1+HR_bmi), weights= w.logit,dist = "loglogistic",data=dat)

AIC(model.res.logit.gauss, k = log(nrow(dat)))
AIC(model.res.logit.weibull, k = log(nrow(dat)))
AIC(model.res.logit.exponential, k = log(nrow(dat)))
AIC(model.res.logit.lognormal, k = log(nrow(dat)))
AIC(model.res.logit.loglogistic, k = log(nrow(dat)))

#BIC
## Gauss = 3658.61
## weibull= 3435.64
## exponential= 3453.99
## lognormal= 3435.06
## loglogistic= 3423.16 ok

### dWOLS_SL
SuperLearner_function = function(Y, X, family) {
  return(SuperLearner(Y = Y, X = X, family = family,
                      SL.library = c("SL.glm", "SL.randomForest", "SL.nnet",
                                     "SL.glm.interaction")))}
H = dat[,c("cage" , "cbmi" , "ctabac" , "meno","grade" , "staging_rvalue" ,
                  "HR" , "type_chx" , "histfm_1degre" , "prise_hts" ,
                  "ind_chi" , "ind_rad","ind_herceptin" , "year_rec")]
mu.t.hat1=SuperLearner_function(Y = dat$A[s], X = H[s,, drop = FALSE], family = "binomial")
mu.t.hat2=SuperLearner_function(Y = dat$A[-s], X = H[-s,, drop = FALSE], family = "binomial")
mu.t.h = dat$A
mu.t.h[s] = predict(mu.t.hat2, type = "raw",newdata = H[s,, drop = FALSE])$pred
mu.t.h[-s] = predict(mu.t.hat1, type = "raw",newdata = H[-s,, drop = FALSE])$pred
w.SL = abs(dat$A - mu.t.h)
lm.dwols.gauss = survreg(Surv(Y,1-censor)~ cage + HR_bmi + ctabac + meno + grade + staging_rvalue +
                  type_chx + histfm_1degre + prise_hts +
                  ind_chi + ind_rad + ind_herceptin + year_rec+A*(1+HR_bmi), weights = w.SL, dist="gaussian", data=dat)
lm.dwols.weibull = survreg(Surv(Y,1-censor)~ cage + HR_bmi + ctabac + meno + grade + staging_rvalue +
                            type_chx + histfm_1degre + prise_hts +
                            ind_chi + ind_rad + ind_herceptin + year_rec+A*(1+HR_bmi), weights = w.SL, dist="weibull", data=dat)
lm.dwols.exponential = survreg(Surv(Y,1-censor)~ cage + HR_bmi + ctabac + meno + grade + staging_rvalue +
                            type_chx + histfm_1degre + prise_hts +
                            ind_chi + ind_rad + ind_herceptin + year_rec+A*(1+HR_bmi), weights = w.SL, dist="exponential", data=dat)
lm.dwols.lognormal = survreg(Surv(Y,1-censor)~ cage + HR_bmi + ctabac + meno + grade + staging_rvalue +
                            type_chx + histfm_1degre + prise_hts +
                            ind_chi + ind_rad + ind_herceptin + year_rec+A*(1+HR_bmi), weights = w.SL, dist="lognormal", data=dat)
lm.dwols.loglogistic= survreg(Surv(Y,1-censor)~ cage + HR_bmi + ctabac + meno + grade + staging_rvalue +
                            type_chx + histfm_1degre + prise_hts +
                            ind_chi + ind_rad + ind_herceptin + year_rec+A*(1+HR_bmi), weights = w.SL, dist="loglogistic", data=dat)

AIC(lm.dwols.gauss, k = log(nrow(dat)))
AIC(lm.dwols.weibull, k = log(nrow(dat)))
AIC(lm.dwols.exponential, k = log(nrow(dat)))
AIC(lm.dwols.lognormal, k = log(nrow(dat)))
AIC(lm.dwols.loglogistic, k = log(nrow(dat)))


#BIC
## Gauss = 3540.90
## weibull= 3321.38
## exponential= 3336.65
## lognormal= 3321.27
## loglogistic= 3308.14 ok


bootstrap_function = function(dat, repetitions) {
  n = nrow(dat)
  t = matrix(NA, nrow = repetitions, ncol = 8)  # Pour stocker les résultats des échantillons bootstrap (t)
  t0 = numeric(8)  # Pour stocker les résultats de l'échantillon original (t0)
  
  s=sort(sample(1:n,n/2))
  ### Logit
  model.logit.train=glm(A ~ cage + cbmi + ctabac + meno + grade + staging_rvalue +
                           HR + type_chx + histfm_1degre + prise_hts +
                           ind_chi + ind_rad + ind_herceptin +year_rec, family = "binomial",data=dat[s,] ) 
  model.logit.test=glm(A ~ cage + cbmi + ctabac + meno + grade + staging_rvalue +
                          HR + type_chx + histfm_1degre + prise_hts +
                          ind_chi + ind_rad + ind_herceptin + year_rec, family = "binomial",data=dat[-s,] ) 
  pred.logit=dat$A 
  pred.logit[s]=predict(model.logit.test,type="res",dat[s,])
  pred.logit[-s]=predict(model.logit.train,type="res", dat[-s,])
  w.logit=abs(dat$A-pred.logit)
  model.res.logit.loglogistic=survreg(Surv(Y,1-censor)~ cage + HR_bmi + ctabac + meno + grade + staging_rvalue +
                                         type_chx + histfm_1degre + prise_hts +
                                         ind_chi + ind_rad + ind_herceptin + year_rec+A*(1+HR_bmi), weights= w.logit,dist = "loglogistic",data=dat)
  psi.hat.logit = coef(model.res.logit.loglogistic)[30:33]
  t0[1:4] = psi.hat.logit
  
  H = dat[, c("cage", "cbmi", "ctabac", "meno", "grade", "staging_rvalue",
               "HR", "type_chx", "histfm_1degre", "prise_hts",
               "ind_chi", "ind_rad", "ind_herceptin", "year_rec")]
  mu.t.hat1=SuperLearner_function(Y = dat$A[s], X = H[s,, drop = FALSE], family = "binomial")
  mu.t.hat2=SuperLearner_function(Y = dat$A[-s], X = H[-s,, drop = FALSE], family = "binomial")
  mu.t.h = dat$A
  mu.t.h[s] = predict(mu.t.hat2, type = "raw",newdata = H[s,, drop = FALSE])$pred
  mu.t.h[-s] = predict(mu.t.hat1, type = "raw",newdata = H[-s,, drop = FALSE])$pred
  w.SL = abs(dat$A - mu.t.h)
  model.SL = survreg(Surv(Y, 1 - censor) ~ cage + HR_bmi + ctabac + meno + grade + staging_rvalue +
                        type_chx + histfm_1degre + prise_hts +
                        ind_chi + ind_rad + ind_herceptin + year_rec + A * (1 + HR_bmi),
                      weights = w.SL, dist = "loglogistic", data = dat)
  psi2.hat.dwols = coef(model.SL)[30:33]
  t0[5:8] = psi2.hat.dwols
  
  for (i in 1:repetitions) {
    sr = sample(1:n, replace = TRUE)
    datboot = dat[sr,]
    s = sort(sample(1:n, n/2))
    # Modèle logit
    model.logit.train = glm(A ~ cage + cbmi + ctabac + meno + grade + staging_rvalue +
                               HR + type_chx + histfm_1degre + prise_hts +
                               ind_chi + ind_rad + ind_herceptin + year_rec, 
                             family = "binomial", data = datboot[s,])
    model.logit.test = glm(A ~ cage + cbmi + ctabac + meno + grade + staging_rvalue +
                              HR + type_chx + histfm_1degre + prise_hts +
                              ind_chi + ind_rad + ind_herceptin + year_rec, 
                            family = "binomial", data = datboot[-s,])
    pred.logit = datboot$A 
    pred.logit[s] = predict(model.logit.test, type = "response", datboot[s,])
    pred.logit[-s] = predict(model.logit.train, type = "response", datboot[-s,])
    w.logit = abs(datboot$A - pred.logit)
    model.res.logit.loglogistic = survreg(Surv(Y, 1 - censor) ~ cage + HR_bmi + ctabac + meno + grade + staging_rvalue +
                                             type_chx + histfm_1degre + prise_hts +
                                             ind_chi + ind_rad + ind_herceptin + year_rec + A * (1 + HR_bmi), 
                                           weights = w.logit, dist = "loglogistic", data = datboot)
    psi.hat.logit.boot = coef(model.res.logit.loglogistic)[30:33]
    
    # SuperLearner
    mu.t.hat1 = SuperLearner_function(Y = datboot$A[s], X = H[s,, drop = FALSE], family = "binomial")
    mu.t.hat2 = SuperLearner_function(Y = datboot$A[-s], X = H[-s,, drop = FALSE], family = "binomial")
    mu.t.h[s] = predict(mu.t.hat2, type = "raw", newdata = H[s,, drop = FALSE])$pred
    mu.t.h[-s] = predict(mu.t.hat1, type = "raw", newdata = H[-s,, drop = FALSE])$pred
    datboot$w.SL = abs(datboot$A - mu.t.h)
    dat_subset = datboot[datboot$w.SL > 0, ]
    model.SL.boot = survreg(Surv(Y, 1 - censor) ~ cage + HR_bmi + ctabac + meno + grade + staging_rvalue +
                               type_chx + histfm_1degre + prise_hts +
                               ind_chi + ind_rad + ind_herceptin + year_rec + A * (1 + HR_bmi),
                             weights = w.SL, dist = "loglogistic", data = dat_subset)
    psi2.hat.dwols.boot = coef(model.SL.boot)[30:33]
    
    t[i, 1:4] = psi.hat.logit.boot
    t[i, 5:8] = psi2.hat.dwols.boot
  }
  return(list(t0 = t0,t = t))
}
set.seed(43578578)
boost= bootstrap_function(dat, 1000)

#Estimate
cbind(boost$t0[1], boost$t0[1] + boost$t0[2],  boost$t0[1] + boost$t0[3],  boost$t0[1] + boost$t0[4],
      boost$t0[5], boost$t0[5] + boost$t0[6],  boost$t0[5] + boost$t0[7],  boost$t0[5] + boost$t0[8])

# [,1]      [,2]        [,3]       [,4]
# [1,] 0.1273602 0.1829231 0.006184629 -0.1369743
# [,5]      [,6]        [,7]        [,8]
# [1,] 0.1549335 0.1593787 -0.00302827 -0.08851415

#IC | On joue avec t
result= cbind(boost$t[,1], boost$t[,1] + boost$t[,2],  boost$t[,1] + boost$t[,3],  boost$t[,1] + boost$t[,4],
           boost$t[,5], boost$t[,5] + boost$t[,6],  boost$t[,5] + boost$t[,7],  boost$t[,5] + boost$t[,8])

apply(result, 2, quantile, c(0.025, 0.975))


# [,1]        [,2]       [,3]       [,4]
# 2.5%  -0.04117879 0.002605579 -0.6314604 -0.5711675
# 97.5%  0.31411631 0.403932925  0.6260311  0.3513442
# [,5]       [,6]       [,7]       [,8]
# 2.5%  -0.003743371 0.05606054 -0.4996039 -0.5672808
# 97.5%  0.334097449 0.46793749  0.7397402  0.3451360
