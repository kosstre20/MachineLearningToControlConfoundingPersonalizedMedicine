
# Set the working directory where the intermediate simulation result files (.Rdata) 
 
setwd("Study 1/Intermediate_results_study1")

# Source the scrip that defines the core analysis functions

#source("../../run_simulation_analysis_all.R")


# True parameter values 
true.psi.list = c(0.5007186, 0.9998634, -0.5, 1.0)

# The sample sizes (n = 300 and n = 1000)
sizes = c(300, 1000)

# Initialize an empty list to store results for each sample size
all_results_by_n = list()

# Number of simulation replicates
nrep = 1000

# Loop through each sample size to compute summary metrics and Monte Carlo errors
for (n in sizes) {
  res=run_simulation_analysis_all(
    true.psi   = true.psi.list,
    scenarios  = c("simple", "medium", "complex"),  # Scenarios as defined in the study
    data_prefix = "TwoTimeStudy1_V2_scenario_",     # Prefix for .Rdata 
    rql_prefix  = "RQL_TwoTimeStudy1_scenario_",     # Prefix for RQL-specific 
    suffix      = paste0("_", n, ".Rdata"),          # Suffix indicating sample size
    nrep        = nrep
  )
  
  # Store results using sample size 
  all_results_by_n[[paste0("n", n)]]=res
}

# The list of methods used in the analysis
simulation_methods = c("Logit", "RF", "Bayes", "Neural", "SVM", "SL", "RQL")
colnames.psi = c("psi_10", "psi_11", "psi_20", "psi_21")

# Extract and format all evaluation metrics for n = 300 (Table 1 of main manuscript)
res300 = extract_all_metrics_for_n(all_results_by_n, 300, methods = simulation_methods, colnames.psi = colnames.psi)
cat("===== Table 1 of main manuscript, n = 300 =====\n")
print(res300) 

# Extract and format all evaluation metrics for n = 1000 (Table 1 of supplementary material)
res1000 = extract_all_metrics_for_n(all_results_by_n, 1000, methods = simulation_methods, colnames.psi = colnames.psi)
cat("===== Table 1 of supplementary material, n = 1000 =====\n")
print(res1000)  

# Extract Monte Carlo standard errors for n = 300 (Table 5 of supplementary material)
mc_errors_300 = cbind(
  all_results_by_n[["n300"]]$MCerror$simple,
  all_results_by_n[["n300"]]$MCerror$medium,
  all_results_by_n[["n300"]]$MCerror$complex
)

# Extract Monte Carlo standard errors for n = 1000 (Table 5 of supplementary material)
mc_errors_1000 = cbind(
  all_results_by_n[["n1000"]]$MCerror$simple,
  all_results_by_n[["n1000"]]$MCerror$medium,
  all_results_by_n[["n1000"]]$MCerror$complex
)

# Table 5 of supplementary material- Study 1
cat("===== Table 5 of supplementary material- Study 1 =====\n")
print(cbind( all_results_by_n[["n300"]]$MCerror$simple[,"MC_biases"], 
       all_results_by_n[["n1000"]]$MCerror$simple[,"MC_biases"],
       all_results_by_n[["n300"]]$MCerror$simple[,"MC_SD"],
       all_results_by_n[["n1000"]]$MCerror$simple[,"MC_SD"],
       all_results_by_n[["n300"]]$MCerror$medium[,"MC_biases"],
       all_results_by_n[["n1000"]]$MCerror$medium[,"MC_biases"],
       all_results_by_n[["n300"]]$MCerror$medium[,"MC_SD"], 
       all_results_by_n[["n1000"]]$MCerror$medium[,"MC_SD"],
       all_results_by_n[["n300"]]$MCerror$complex[,"MC_biases"],
       all_results_by_n[["n1000"]]$MCerror$complex[,"MC_biases"],
       all_results_by_n[["n300"]]$MCerror$complex[,"MC_SD"], 
       all_results_by_n[["n1000"]]$MCerror$complex[,"MC_SD"]))
