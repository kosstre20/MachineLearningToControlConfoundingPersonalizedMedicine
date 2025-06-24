# =====  Set the working directory to the project root =====  
setwd("...")


# ===== Loading the source code for the required packages ===== 
source("load_packages.R")


# =====  Loading the source code for the function of simulations results ===== 
source("run_simulation_analysis_all.R")


# ===== Function ===== 
run_study_script=function(script_path, nrep = 1000, scenarios = c("simple", "medium", "complex"), sizes = c(300, 1000)) {
  original_dir=getwd()
  tryCatch({
    if (!file.exists(script_path)) stop(paste("The following file was not found:", script_path))
    cat("\n===== Running:", script_path, "=====\n")
    
    assign("nrep", nrep, envir = .GlobalEnv)
    assign("scenarios", scenarios, envir = .GlobalEnv)
    assign("sizes", sizes, envir = .GlobalEnv)
    
    source(script_path, local = TRUE)
    
  }, error = function(e) {
    message("Error while running ", script_path, ": ", e$message)
  }, finally = {
    setwd(original_dir)
  })
}


# ===== Part 1 : Part 1: to reproduce the results of the paper including
#       all tables and figures by using the intermediate results ===== 

# Table 1 of main manuscript, n = 300
# Table 1 of supplementary material, n = 1000
# Table 5 of supplementary material- Study 1
run_study_script("Study 1/Results_SimulationsTwoTimesStudy1.R")

# Table 2 of main manuscript, n = 300
# Table 2 of supplementary material, n = 1000
# Table 5 of supplementary material- Study 2

run_study_script("Study 2/Results_SimulationsTwoTimesStudy2.R")

# Table 3 of main manuscript, n = 300
# Table 3 of supplementary material, n = 1000
# Table 5 of supplementary material- Study 3

run_study_script("Study 3/Results_SimulationsTwoTimesStudy3.R")

# Table 4 of main manuscript, n = 300
# Table 4 of supplementary material, n = 1000
# Table 5 of supplementary material- Study 4

run_study_script("Study 4/Results_SimulationsTwoTimesStudy4.R")

# Table 6 of supplementary material, Simple scenario, n = 300
# Table 7 of supplementary material, Medium scenario, n = 300
# Table 8 of supplementary material, Complex scenario, n = 300
# Table 9 of supplementary material, Simple scenario, n = 1000
# Table 10 of supplementary material,  Medium scenario, n = 1000
# Table 11 of supplementary material, Complex scenario, n = 1000
# Table 12 of supplementary material- Study 5

run_study_script("Study 5/Result_SimulationsLargeDimensionalityTwoTimesStudy5.R")

# Table 13 of supplementary material, Performance evaluation across scenarios and number of folds for the SuperLearner, n = 300 
# Table 14 of supplementary material, Performance evaluation across scenarios and number of folds for the SuperLearner, n = 1000
run_study_script("Two_to_Five_Folds_Study_4/Result_Two_to_Five_Folds_Study_4.R")

# Results of the m-out-of-n bootstrap procedure in the
# complex scenario of Study 4, section of the main manuscript
run_study_script("m-out-of-n bootstrap Study 4/Results_Boostrap.R")

# Figure 1:  Distribution of the treatment probabilities (A) Studies 1-3
# Figure 2: Study 1-3, Stage 1: Distribution of Covariates X1 for all scenarios and Covariate by A1 for each scenario
# The results are generated in the Figures subdirectory of the Plot_Positivity_Studies_1_4 directory.
source("Plot_Positivity_Studies_1_4/Boxplot_Posivity_Distribution_Studies_1_3.R")

# Figure 1: Distribution of the treatment probabilities (B) Study 4.
# Figure 3: Study 4, Stage 1: Distribution of Covariates X1 for all scenarios and Covariate by A1 for each scenario
# Figure 4: Distribution of Covariates X2 and Covariate X2 by A2, simple scenario
# Figure 5: Study 4, Stage 2: Distribution of Covariates X2 and Covariate X2 by A2, medium scenario
# Figure 6: Study 4, Stage 2: Distribution of Covariates X2 and Covariate X2 by A2, complex scenario
# The results are generated in the Figures subdirectory of the Plot_Positivity_Studies_1_4 directory.

source("Plot_Positivity_Studies_1_4/Boxplot_Posivity_Distribution_Study_4.R")

# Figure 7: Distribution of the treatment probabilities: Study 5, (A) Stage 1, (B) Stage 2
# Figure 8: Distribution of Covariates: Study 5, Stage 1, for all scenarios
# Figure 9: Study 5, Stage 1, Distribution of Covariates by A1, scenario simple
# Figure 10: Study 5, Stage 1, Distribution of Covariates by A1, scenario medium
# Figure 11: Study 5, Stage 1, Distribution of Covariates by A1, scenario complex
# Figure 12: Distribution of Covariates: Study 5, Stage 2, scenario simple
# Figure 13: Distribution of Covariates: Study 5, Stage 2, scenario medium
# Figure 14: Distribution of Covariates: Study 5, Stage 2, scenario complex
# The results are generated in the Figures subdirectory of the Study 5 directory.

source("Study 5/PlotStudy5.R")


# ===== Part 2 : to reproduce the intermediate results (i.e. ALL RData files) ===== 
# Note that running the RQL code may require manual interventions, as described in ReadMe.


# Study 1
run_study_script("Study 1/SimulationsTwoTimesStudy1.R")
#source("Study 1/RQL_TwoTimes_Study1.R")

# Study 2
run_study_script("Study 2/SimulationsTwoTimesStudy2.R")
#source("Study 2/RQL_TwoTimes_Study2.R")

# Study 3
run_study_script("Study 3/SimulationsTwoTimesStudy3.R")
#source("Study 3/RQL_TwoTimes_Study3.R")

# Study 4
run_study_script("Study 4/SimulationsTwoTimesStudy4.R")
#source("Study 4/RQL_TwoTimes_Study4.R")

# Study 5
run_study_script("Study 5/SimulationsLargeDimensionalityTwoTimesStudy5.R")
#source("Study 5/RQLStudy5.R")

# Two_to_Five_Folds_Study_4
run_study_script("Two_to_Five_Folds_Study_4/Two_to_Five_Folds_Study_4.R")

# m-out-of-n bootstrap Study 4
source("m-out-of-n bootstrap Study 4/Bootstrap.R")

# ===== Part 3 : to reproduce some scenarios with reduced settings ===== 
# Note that running the RQL code may require manual interventions, as described in ReadMe.


# Run Study 2 with 300 repetitions
run_study_script("Study 2/SimulationsTwoTimesStudy2.R", nrep = 500)
run_study_script("Study 2/Results_SimulationsTwoTimesStudy2.R")

# Run Study 4 with selected scenarios
run_study_script("Study 4/SimulationsTwoTimesStudy4.R", scenarios = "simple")


# Lightweight test for Study 5
run_study_script("Study 5/SimulationsLargeDimensionalityTwoTimesStudy5.R", nrep = 10, scenarios = c("simple", "medium","complex"))
run_study_script("Study 5/Result_SimulationsLargeDimensionalityTwoTimesStudy5.R")