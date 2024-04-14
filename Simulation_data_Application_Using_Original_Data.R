# #install.packages("dplyr")
# # Load the library
# library(dplyr)
# # Set the seed for reproducibility
# set.seed(43578578)
# # Define the sample size
# n = 5444 
# # Define the inverse logit function
# expit = plogis
# # Proportions for each category
# proportions = list(
#   cage = c(0.056, 0.206, 0.306, 0.25, 0.182),
#   ctabac = c(0.552, 0.415, 0.034),
#   cbmi = c(0.462, 0.538),
#   HR=c(0.822, 0.162, 0.016),
#   meno = c(0.281, 0.719),
#   grade = c(0.235, 0.321, 0.337, 0.107),
#   staging_rvalue = c(0.456, 0.425, 0.119),
#   type_chx = c(0.248, 0.738, 0.014),
#   histfm_1degre = c(0.253, 0.747),
#   prise_hts = c(0.438, 0.562),
#   ind_chi = c(0.506, 0.494),
#   ind_rad = c(0.232, 0.768),
#   ind_herceptin = c(0.966, 0.034),
#   year_rec = c(0.071, 0.131, 0.168, 0.274, 0.356)
# )
# # Generate random variables based on specified proportions
# variables = lapply(proportions, function(prop) {
#   if (length(prop) == 2) {
#     rbinom(n, size = 1, prob = prop)
#   } else {
#     sample(x = seq_along(prop), size = n, replace = TRUE, prob = prop)
#   }
# })
# 
# # Using original data (dat)
# dt=dat
# # Convert variables to numeric
# dt = dt %>%
#   mutate(across(c(cage, cbmi, ctabac, meno, grade, staging_rvalue,
#                   HS, type_chx, histfm_1degre, prise_hts,
#                   ind_chi, ind_rad, ind_herceptin, year_rec), as.numeric))
# # Logistic regression for A
# model=glm(A ~ cage + cbmi + ctabac + meno + grade + staging_rvalue +
#             HS + type_chx + histfm_1degre + prise_hts +
#             ind_chi + ind_rad + ind_herceptin +year_rec, family = "binomial",data=dt )
# # Extract coefficients from the linear regression model
# coefficients = coef(model)
# # Calculate the probability p for each observation
# p_A = expit(coefficients[1] +coefficients[2]*dt$cage + coefficients[3]*dt$cbmi 
#             +coefficients[4]*dt$ctabac +coefficients[5]*dt$meno+coefficients[6]*dt$grade + 
#               coefficients[7]*dt$staging_rvalue+coefficients[8]*dt$HS+coefficients[9]*dt$type_chx+
#               coefficients[10]*dt$histfm_1degre+coefficients[11]*dt$prise_hts+coefficients[12]*dt$ind_chi+
#               coefficients[13]*dt$ind_rad +coefficients[14]*dt$ind_herceptin +coefficients[15]*dt$year_rec)
# # Simulate A based on the calculated probabilities
# dt$A_sim = rbinom(n, size = 1, prob = p_A)
# # Check the simulation for A
# table(dt$A_sim) 
# 
# # Linear regression for variable Y
# # Create the log-logistic model
# model_Y <- lm(log(dt$Y)  ~ A + cage + cbmi + ctabac + meno + grade + staging_rvalue +
#                 HS + type_chx + histfm_1degre + prise_hts +
#                 ind_chi + ind_rad + ind_herceptin + year_rec, data = dt)
# # Extract coefficients from the linear regression model
# coefficients_Y = coef(model_Y)
# # # Extract the sigma
# sigma_Y = summary(model_Y)$sigma
# # Calculate the probability p for each observation
# p_Y =with( dt,coefficients_Y[1] + coefficients_Y[2]*A+coefficients_Y[3] * dt$cage+coefficients_Y[4] * dt$cbmi+coefficients_Y[5]* dt$ctabac +
#              coefficients_Y[6] * dt$meno +coefficients_Y[7] * dt$grade +coefficients_Y[8]* dt$staging_rvalue+coefficients_Y[9] * dt$HS+coefficients_Y[10]* dt$type_chx+
#              coefficients_Y[11] * dt$histfm_1degre+ coefficients_Y[12]* dt$prise_hts +
#              coefficients_Y[13]* dt$ind_chi+coefficients_Y[14] * dt$ind_rad +coefficients_Y[15] * dt$ind_herceptin +coefficients_Y[16] * dt$year_rec)
# # Simulate Y based on the calculated probabilities
# dt$Y_sim = exp(rlogis(n, location = p_Y, scale = pi/(sqrt(3)*sigma_Y )))
# 
# ##Censor : Temps_censor
# dt = dt %>%
#   group_by(year_rec) %>%
#   mutate(min_Y = min(Y),
#          max_Y = max(Y)) %>%
#   rowwise() %>%
#   mutate(Temps_censor = runif(1, min_Y, max_Y)) %>%
#   ungroup() %>%
#   select(-min_Y, -max_Y)
# 
# #Actual survival time 
# dt$Yobs = pmin(dt$Y_sim , dt$Temps_censor)
# dt$Censor_T = ifelse( dt$Y_sim  > dt$Temps_censor ,1,0)
# table(dt$Censor_T) 
# 
# #Database creation
# base <- data.frame(
#   A = dt$A_sim,
#   Y = dt$Yobs,
#   censor = dt$Censor_T,
#   variables
# )
# 
# setwd("C:\\Users\\kossi\\OneDrive - Université Laval\\Application\\simul")
# write.csv(base, file = "data_Application.csv", row.names = TRUE)
# 
# 
