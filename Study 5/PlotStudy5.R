
# Set the directory where figures will be saved
figure_dir= "..."

n = 100000
expit = plogis

# Define the vector of true parameter values
psi1 = c(-3, 1, -0.2, 0.3, 1, 0.5, 1, -0.2, 0.3, 1, 0.5)
psi2 = c(-0.5, 1, -0.2, 0.3, 1, 0.5, 1, -0.2, 0.3, 1, 0.5)

# List of binary variables for stage 1
binary_vars_1 = c('X11', 'X12', 'X14', 'X16', 'X17')


# List of binary variables for stage 2
binary_vars_2 = c('X22', 'X24', 'X27')

set.seed(43578578)

# Functions to generate covariates 

generate_stage1_covariates= function() {
  set.seed(43578578)
  X11 = rbinom(n, 1, 0.5)
  X12 = rbinom(n, 1, 0.05 + 0.5 * X11)
  X13 = rnorm(n, 0, 0.5)
  X14 = rbinom(n, 1, expit(-0.4 + X12))
  X15 = runif(n)
  X16 = as.integer(cut(runif(n), 3))
  X17 = as.integer(cut(runif(n), 3))
  X18 = X11 * X14 + rnorm(n, 0, 0.5)
  X19 = log(X15 + 1) + runif(n)
  X110 = -X13 + X12 * X15 + runif(n)
  
  data.frame(X11, X12, X13, X14, X15, X16, X17, X18, X19, X110)
}

generate_stage2_covariates= function(X11, X13, X15, X16, A1) {
  set.seed(43578578)
  X21 = X11 + A1 + rnorm(n, 0, 0.1)
  X22 = rbinom(n, 1, expit(-0.1 + 0.3 * X11 + A1))
  X23 = X13 + A1 + rnorm(n, 0, 0.2)
  X24 = rbinom(n, 1, expit(-0.4 + X22 + A1))
  X25 = X15 + A1 + runif(n, -1, 1)
  X26 = X16 + A1 + sample(0:2, n, replace = TRUE)
  X27 = as.integer(cut(runif(n), 3))
  X28 = abs(X21 - X24) + sin(X23 + A1) + runif(n)
  X29 = cos(X25 + X15 + A1) + runif(n)
  X210 = 0.5 * X23 + 0.5 * X21 + A1 + runif(n)
  
  data.frame(X21, X22, X23, X24, X25, X26, X27, X28, X29, X210)
}


plot_stage1_covariates = function(data, binary_vars, filename) {
  png(filename = file.path(figure_dir, filename), width = 1200, height = 800)
  par(mfrow = c(2, 5), mar = c(4, 4, 2, 1))
  
  for (var in names(data)) {
    if (var %in% binary_vars) {
      freq = prop.table(table(data[[var]]))
      barplot(freq,
              main = var,
              col = gray.colors(length(freq)),
              names.arg = names(freq),
              ylim = c(0, 1),
              ylab = "Frequency")
      legend("topright", legend = names(freq), fill = gray.colors(length(freq)), bty = "n")
    } else {
      boxplot(data[[var]], main = var, ylab = "Values", col = "darkgray")
    }
  }
  dev.off()
}


plot_distribution = function(data, binary_vars, file_name, by_treatment = FALSE, A = NULL) {
  png(filename = file.path(figure_dir, file_name), width = 1200, height = 800)
  par(mfrow = c(2, 5), mar = c(4, 4, 2, 1))
  
  for (var in names(data)) {
    if (var %in% binary_vars) {
      if (by_treatment && !is.null(A)) {
        freq = prop.table(table(data[[var]], A), margin = 2)
        bp = barplot(freq, beside = TRUE, main = var,
                      col = gray.colors(nrow(freq)), 
                      ylim = c(0, 1), ylab = "Frequency",
                      xlab = "A1", names.arg = colnames(freq))
        legend("topright", legend = rownames(freq), fill = gray.colors(nrow(freq)), bty = "n")
      } else {
        freq = prop.table(table(data[[var]]))
        bp = barplot(freq, main = var, col = gray.colors(length(freq)),
                      names.arg = names(freq), ylim = c(0, 1), ylab = "Frequency")
        legend("topright", legend = names(freq), fill = gray.colors(length(freq)), bty = "n")
      }
    } else {
      if (by_treatment && !is.null(A)) {
        boxplot(data[[var]] ~ A, main = var, ylab = "Values", col = c("lightgray", "gray"))
      } else {
        boxplot(data[[var]], main = var, ylab = "Values", col = "darkgray")
      }
    }
  }
  dev.off()
}


# Scenario Runner plot

run_scenario = function(scenario) {
  set.seed(43578578)
  dt1 = generate_stage1_covariates()
  attach(dt1)
  
  if (scenario == "simple") {
    A1_prob = expit(-4 - 1.5 * X11 + 5.5 * X13 - 5.5 * X14 - 1.5 * X15 + 2.5 * X16 + 2.5 * X17 + X18 - 0.5 * X19 + X110)
    A1 = rbinom(n, 1, A1_prob)
    A2_prob = expit(15 - X11 - X13 - 3.5 * X14 + X15 - 2.5 * X16 - X17 - X18 - 0.5 * X19 - X110)
    suffix = "simple"
  } else if (scenario == "medium") {
    A1_prob = expit(5 - 1.5 * X11 + 0.5 * X12 - 0.5 * X14 + 0.5 * X13 - 1.5 * X15 - X16 - X17 + X18
                     - 0.5 * X19 + X110 + 0.5 * X13^2 + X15^2 - 0.5 * X18^2 + X19^2 - 0.5 * X110^2)
    A1 = rbinom(n, 1, A1_prob)
    A2_prob = expit(5 - X11 - X13 - X14 + X15 - 0.5 * X16 - X17 - X18 - 0.5 * X19 - X110
                     + 0.5 * X11^2 + 0.5 * X13^2 + 0.5 * X15^2 - 0.1 * X16^2 - 0.5 * X18^2 + X19^2 - 0.5 * X110^2)
    suffix = "medium"
  } else if (scenario == "complex") {
    A1_prob = expit(4 - 1.5 * X11 + 0.5 * X12 + sin(X13) - X14 + cos(X15) - X16 
                     + sin(cos(X13^2)) + abs(sin(X15^2)) - cos(X17^2) - log(X18^2) + X19^2 - exp(tan(X110^2)))
    A1 = rbinom(n, 1, A1_prob)
    A2_prob = expit(-0.5 * X11 + sin(X13) - X14 + cos(X15) + X11^2 + sin(cos(X13^2)) +
                       abs(sin(X15^2)) - 0.1 * X16^2 - cos(X17^2) - log(X18^2) + X19^2 - exp(tan(X110^2)))
    suffix = "complex"
  }
  
  A1opt = 1 * ((as.matrix(cbind(1, dt1))) %*% psi1 > 0)
  mu1 = (A1opt - A1) * (as.matrix(cbind(1, dt1)) %*% psi1)
  
  dt2 = generate_stage2_covariates(X11, X13, X15, X16, A1)
  
  # Plot dt1, dt2
  #Figures 9 to 11, supplementary material
  plot_distribution(dt1, binary_vars_1, sprintf("Distribution_X11_X10_by_A1_%s.png", suffix), TRUE, A1)
  #Figures 12 to 14, supplementary material
  plot_distribution(dt2, binary_vars_2, sprintf("Distribution_X21_X210_%s.png", suffix), FALSE)
  
  return(list(A1_prob = A1_prob, A2_prob = A2_prob, suffix = suffix))
}

# Run All

results_simple = run_scenario("simple")
results_medium = run_scenario("medium")
results_complex = run_scenario("complex")

# All variable stage 1 (Figure 8 of supplementary material)

dt1 = generate_stage1_covariates()
plot_stage1_covariates(dt1, binary_vars_1, "Distribution_X11_X10_all_scenarios.png")


# plot probability (Figure 7 of supplementary material)
png(filename = file.path(figure_dir, "Distribution_A1_stage1.png"), width = 800, height = 600)
boxplot(cbind(Simple = results_simple$A1_prob,
              Medium = results_medium$A1_prob,
              Complex = results_complex$A1_prob),
        ylim = c(0, 1), main = "(A) Stage 1", col = c("lightgray", "gray", "darkgray"))
dev.off()

png(filename = file.path(figure_dir, "Distribution_A2_stage2.png"), width = 800, height = 600)
boxplot(cbind(Simple = results_simple$A2_prob,
              Medium = results_medium$A2_prob,
              Complex = results_complex$A2_prob),
        ylim = c(0, 1), main = "(B) Stage 2", col = c("lightgray", "gray", "darkgray"))
dev.off()
