# List of required packages
required_packages <- c(
  "randomForest",   # For random forest modeling
  "SuperLearner",   # For SuperLearner modeling
  "e1071",          # For SVM modeling
  "nnet",           # For neural network modeling
  "MASS",           # For Naive Bayes modeling
  "earth",          # For MARS (Multivariate Adaptive Regression Splines) modeling
  "resample" ,      # For resampling methods
  "tableone",       # For creating summary tables
  "survey",         # For data manipulation
  "haven",          # For reading and writing various data formats, including SAS files
  "dplyr",          # For data manipulation, provides functions for filtering, selecting, and transforming data
  "labelled",       # For assigning variable labels
  "survival",       # For survival analysis
  "openxlsx"        # For reading and writing Excel files, especially for exporting results in Excel format
)


# Function to install and load packages
load_or_install <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg)
      library(pkg, character.only = TRUE)
    }
  }
}

# Apply the function to the list of packages
load_or_install(required_packages)
