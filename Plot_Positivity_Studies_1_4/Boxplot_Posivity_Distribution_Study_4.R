####### Study 4  ######

# General path to save all figures for Study 4

figure_dir = "..."
dir.create(figure_dir, showWarnings = FALSE, recursive = TRUE)

set.seed(43578578)
# Sample size
n = 100000

# Logistic function
expit = plogis

# Stage 1
X1 = rnorm(n)

# Probability distributions for Study 4
simple_4 = expit(0.5 - X1)
moins_4   = expit(- X1 + 0.5 * X1^2)
complex_4 = expit(1.2*X1 - 0.25*X1^2 - abs(X1) + 0.5 * abs(sin(X1)))

# Treatment assignment
A1.simple.4 = rbinom(n, 1, p = simple_4)
A1.moins.4   = rbinom(n, 1, p = moins_4)
A1.complex.4 = rbinom(n, 1, p = complex_4)

# Dataset
dt1 = data.frame(
  X1 = X1,
  simple_4 = simple_4,
  moins_4 = moins_4,
  complex_4 = complex_4,
  A1.simple.4 = A1.simple.4,
  A1.moins.4 = A1.moins.4,
  A1.complex.4 = A1.complex.4
)

# ---- Plot 1: Probability distributions (Figure 1 B of supplementary material)
png(file.path(figure_dir, "Boxplot_probabilities_Study_4.png"), width = 800, height = 600)
boxplot(cbind(Simple = simple_4, Medium = moins_4, Complex = complex_4),
        ylim = c(0, 1), main = "(B) Study 4", col = c("lightgray", "gray", "darkgray"))
dev.off()

# ---- Plot 2: X1 by A for Study 4 (Figure 3 of supplementary material)
png(file.path(figure_dir, "Distribution_X1_study_4_by_A.png"), width = 1200, height = 800)
par(mfrow = c(1, 4))
boxplot(dt1$X1, ylab = "Values", col = "darkgray", xlab = "X1")
boxplot(dt1$X1 ~ dt1$A1.simple.4, main = "X1 by A1", ylab = "Values", xlab = "Simple", col = c("lightgray", "gray"))
boxplot(dt1$X1 ~ dt1$A1.moins.4,   main = "X1 by A1", ylab = "Values", xlab = "Medium", col = c("lightgray", "gray"))
boxplot(dt1$X1 ~ dt1$A1.complex.4, main = "X1 by A1", ylab = "Values", xlab = "Complex", col = c("lightgray", "gray"))
dev.off()

# ----- Stage 2 plots by scenario -----

psi1 = c(-0.5, 1)

# --- Simple --- (Figure 4 of supplementary material)
X1 = rnorm(n)
A1.simple.4 = rbinom(n, 1, p = expit(0.5 - X1))
A1opt = 1 * (cbind(1, X1) %*% psi1 > 0)
mu1 = (A1opt - A1.simple.4) * cbind(1, X1) %*% psi1
X2.simple = X1 + A1.simple.4 + rnorm(n)
A2.simple = rbinom(n, 1, p = expit(0.5 - X2.simple))

png(file.path(figure_dir, "Distribution_X2_study_4_by_A2_simple.png"), width = 1200, height = 800)
par(mfrow = c(1, 2))
boxplot(X2.simple, ylab = "Values", col = "darkgray", xlab = "X2")
boxplot(X2.simple ~ A2.simple, main = "X2 by A2", ylab = "Values", xlab = "Simple", col = c("lightgray", "gray"))
dev.off()

# --- Medium --- (Figure 5 of supplementary material)
X1 = rnorm(n)
A1.moins.4 = rbinom(n, 1, p = expit(- X1 + 0.5 * X1^2))
A1opt = 1 * (cbind(1, X1) %*% psi1 > 0)
mu1 = (A1opt - A1.moins.4) * cbind(1, X1) %*% psi1
X2.medium = X1 + A1.moins.4 + rnorm(n)
A2.medium = rbinom(n, 1, p = expit(-X2.medium + 0.5 * X2.medium^2))

png(file.path(figure_dir, "Distribution_X2_study_4_by_A2_medium.png"), width = 1200, height = 800)
par(mfrow = c(1, 2))
boxplot(X2.medium, ylab = "Values", col = "darkgray", xlab = "X2")
boxplot(X2.medium ~ A2.medium, main = "X2 by A2", ylab = "Values", xlab = "Medium", col = c("lightgray", "gray"))
dev.off()

# --- Complex --- (Figure 6 of supplementary material)
X1 = rnorm(n)
A1.complex.4 = rbinom(n, 1, p = expit(1.2*X1 - 0.25*X1^2 - abs(X1) + 0.5 * abs(sin(X1))))
A1opt = 1 * (cbind(1, X1) %*% psi1 > 0)
mu1 = (A1opt - A1.complex.4) * cbind(1, X1) %*% psi1
X2.complex = X1 + A1.complex.4 + rnorm(n)
A2.complex = rbinom(n, 1, p = expit(1.2*X2.complex - 0.25*X2.complex^2 - abs(X2.complex) + 0.5 * abs(sin(X2.complex))))

png(file.path(figure_dir, "Distribution_X2_study_4_by_A2_complex.png"), width = 1200, height = 800)
par(mfrow = c(1, 2))
boxplot(X2.complex, ylab = "Values", col = "darkgray", xlab = "X2")
boxplot(X2.complex ~ A2.complex, main = "X2 by A2", ylab = "Values", xlab = "Complex", col = c("lightgray", "gray"))
dev.off()

