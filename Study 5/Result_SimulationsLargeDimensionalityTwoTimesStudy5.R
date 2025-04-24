
# Set the working directory where the intermediate simulation result files (.Rdata) 
setwd("...")


# Source the scrip that defines the core analysis functions
source("../../run_simulation_analysis_all.R")


# True parameter values 
true.psi.list = list(
  simple  = c(2.4149562, 1.9525503 ,-0.2016648, -0.9292295, 0.9987304, 1.5924654, 1.0022201,
              -0.2004970, 0.3013375, 0.9590737, 0.4964222, -0.5000000, 1.0000000, -0.2000000,
              0.3000000, 1.0000000, 0.5000000, 1.0000000, -0.2000000, 0.3000000, 1.0000000, 0.5000000),
  medium  = c(-10.607848830, -1.420540003, -0.194972803, 3.088690147, 1.005106175, 1.346672115,
              0.005283755, -0.194350423, 0.296906346, 1.013564867, 0.505205223, -0.5000000, 1.0000000,
              -0.2000000, 0.3000000, 1.0000000, 0.5000000, 1.0000000, -0.2000000, 0.3000000,
              1.0000000, 0.5000000),
  complex = c(-4.3516984, 1.2135523, -0.1977484, -1.9028925, 0.9999270, 0.9770289, -0.9991191,
              -0.2004283, 0.2984942, 0.9898369, 0.4974904, -0.5000000, 1.0000000, -0.2000000,
              0.3000000, 1.0000000, 0.5000000, 1.0000000, -0.2000000, 0.3000000, 1.0000000, 0.5000000)
)

# The sample sizes (n = 300 and n = 1000)
sizes = c(300, 1000)

# Initialize an empty list to store results for each sample size
all_results_by_n = list()

# Number of simulation replicates
nrep = 1000

# Loop through each sample size to compute summary metrics and Monte Carlo errors
for (n in sizes) {
  res = run_simulation_analysis_all(
    true.psi   = true.psi.list,
    scenarios  = c("simple", "medium", "complex"),  # Scenarios as defined in the study
    data_prefix = "LargeDimensionality_TwoTimeStudy5_V2_scenario_",     # Prefix for .Rdata 
    rql_prefix  = "RQL_TwoTimeStudy5_scenario_",     # Prefix for RQL-specific 
    suffix      = paste0("_", n, ".Rdata"),          # Suffix indicating sample size
    nrep        = nrep
  )
  
  # Store results using sample size 
  all_results_by_n[[paste0("n", n)]] = res
}

# The list of methods used in the analysis
simulation_methods = c("Logit", "RF", "Bayes", "Neural", "SVM", "SL", "RQL")
colnames.psi = c("psi_10", "psi_11", "psi_12", "psi_13", "psi_14", "psi_15",
                  "psi_16", "psi_17", "psi_18", "psi_19", "psi_110", "psi_20",
                  "psi_21", "psi_22", "psi_23", "psi_24", "psi_25", "psi_26",
                  "psi_27", "psi_28", "psi_29", "psi_210")

# Extract and format all evaluation metrics for n = 300 (Tables 6 to 8, supplementary material)
res300 = extract_all_metrics_Study5_for_n(all_results_by_n, 300, methods = simulation_methods, colnames.psi = colnames.psi)

res300$simple #(Table 6)

res300$medium #(Table 7)

res300$complex #(Table 8)

# Extract and format all evaluation metrics for n = 1000 (Tables 9 to 11, supplementary material)
res1000 = extract_all_metrics_Study5_for_n(all_results_by_n, 1000, methods = simulation_methods, colnames.psi = colnames.psi)

res1000$simple #(Table 9)  

res1000$medium #(Table 10)

res1000$complex #(Table 11)

# Extract Monte Carlo standard errors for n = 300 (Table 12 of supplementary material)
mc_errors_300 = cbind(
  all_results_by_n[["n300"]]$MCerror$simple,
  all_results_by_n[["n300"]]$MCerror$medium,
  all_results_by_n[["n300"]]$MCerror$complex
)
mc_errors_300

# Extract Monte Carlo standard errors for n = 1000 (Table 12 of supplementary material)
mc_errors_1000 = cbind(
  all_results_by_n[["n1000"]]$MCerror$simple,
  all_results_by_n[["n1000"]]$MCerror$medium,
  all_results_by_n[["n1000"]]$MCerror$complex
)
mc_errors_1000

# Table 12 of supplementary material- Study 5
cbind( all_results_by_n[["n300"]]$MCerror$simple[,"MC_biases"], 
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
       all_results_by_n[["n1000"]]$MCerror$complex[,"MC_SD"])
