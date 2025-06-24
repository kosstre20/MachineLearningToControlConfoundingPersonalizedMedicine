
# Set working directory
setwd("Two_to_Five_Folds_Study_4/Intermediate_results_Two_to_Five_Folds_Study_4")

# True parameter values for each scenario
true_psi_list = list(
  simple = c(0.4999415, 0.9988612, -0.5, 1.0),
  medium = c(1.497140, 2.996765, -0.5, 1.0),
  complex = c(0.504513694, 0.001588589, -0.5, 1.0)
)

# scenario names, sample sizes, and number of folds to evaluate
scenarios = c("simple", "medium", "complex")
samples = c(300, 1000)
folds = 1:5

# column names for the psi parameters
colnames.psi = c("psi_10", "psi_11", "psi_20", "psi_21")

# -------- Analysis Function --------
# This function computes bias, standard deviation (SD), and root mean squared error (RMSE)
# for each number of folds in a given scenario and sample size.
analyze_scenario = function(scenario, sample_size, true_psi) {
  Bias = SD = RMSE = list()
  
  for (k in folds) {
    # Load the corresponding .RData file
    file = sprintf("TwoTimeStudy4_V2_scenario_%s_%s_%dfolds.Rdata", scenario, sample_size, k)
    load(file)
    
    # Compute bias as the difference between estimated and true psi
    Bias[[k]] = colMeans(results.SL, na.rm = TRUE) - true_psi
    
    # Compute standard deviation of the estimates
    SD[[k]] = apply(results.SL, 2, sd, na.rm = TRUE)
    
    # Compute RMSE combining bias and SD
    RMSE[[k]] = sqrt(Bias[[k]]^2 + SD[[k]]^2)
  }
  
  # Stack the results into a single matrix
  table = rbind(
    do.call(rbind, Bias),
    do.call(rbind, SD),
    do.call(rbind, RMSE)
  )
  
  colnames(table) = colnames.psi
  return(table)
}

results_tables = list()

# Loop over each sample size and scenario
for (s in samples) {
  for (scenario in scenarios) {
    key = paste0(scenario, s)  
    results_tables[[key]] = analyze_scenario(scenario, s, true_psi_list[[scenario]])
  }
}

# -------- Format Output --------

format_results_table = function(table_matrix) {
  rownames_all = c(
    paste0("Bias_", 1:5, "folds"),
    paste0("SD_", 1:5, "folds"),
    paste0("RMSE_", 1:5, "folds")
  )
  rownames(table_matrix) = rownames_all
  round(table_matrix, 2)
}

# Combine results by sample size for the three scenarios
res_combined_300 = cbind(
  results_tables$simple300,
  results_tables$medium300,
  results_tables$complex300
)
res_combined_1000 = cbind(
  results_tables$simple1000,
  results_tables$medium1000,
  results_tables$complex1000
)

# Format the final results for reporting  (Table 13 and 14 of the supplementary materials, 1folds= no cross-fitting)
formatted_300 = format_results_table(res_combined_300)

cat("===== Table 13 of supplementary material, Performance evaluation across scenarios and number of folds for the SuperLearner, n = 300 =====\n")
print(formatted_300)

formatted_1000 = format_results_table(res_combined_1000)
cat("===== Table 14 of supplementary material, Performance evaluation across scenarios and number of folds for the SuperLearner, n = 1000 =====\n")
print(formatted_1000)
