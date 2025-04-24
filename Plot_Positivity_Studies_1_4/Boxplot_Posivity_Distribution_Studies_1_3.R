
set.seed(43578578)

# Define the sample size
n = 100000

# Define the expit function as the logistic function
expit = plogis

####### Study 1 - Study 3 ######
X1 = runif(n, - 1, 1)# Generate covariate X1 from a uniform distribution between -1 and 1

# Define the probability functions for three different scenarios
simple = expit(0.5 - X1)  # Simple scenario
medium = expit( - X1 + 0.5 * X1 * X1) # Medium  scenario
complex = expit(1.2 * X1 - 0.25 * X1 * X1 - abs(X1) + 0.5 * abs(sin(X1)))# Complex scenario

# Generate treatment assignment based on the probabilities defined above
A1.simple = rbinom(n, 1, p=expit(0.5 - X1))
A1.medium = rbinom(n, 1, p=expit( - X1 + 0.5 * X1 * X1))
A1.complex = rbinom(n, 1, p=expit(1.2 * X1 - 0.25 * X1 * X1 - abs(X1) + 0.5 * abs(sin(X1))))

# Create a data frame containing all the generated variables
dt1 <- data.frame(
  X1 = X1,
  simple = simple,
  medium = medium,
  complex = complex,
  A1.simple = A1.simple,
  A1.medium = A1.medium,
  A1.complex = A1.complex
)



figure_dir = "..."
dir.create(figure_dir, showWarnings = FALSE, recursive = TRUE)

# Generate boxplots comparing the probability distributions for Study 1 - Study 3 
# (Figure 1A of supplementary material)
png(filename = file.path(figure_dir, "Boxplot_probabilities_Study1_3.png"), width = 800, height = 600)
boxplot(cbind(Simple = simple, Medium = medium, Complex = complex),
        ylim = c(0, 1),
        main = "(A) Study 1, Study 2 and Study 3",
        col = c("lightgray", "gray", "darkgray"))

dev.off() 

# Generate boxplots for covariate X1 distribution by treatment groups in Study 1 - Study 3
# (Figure 2 of supplementary material)
png(filename = file.path(figure_dir, "Distribution_X1_studies1_3_by_A.png"), width = 1200, height = 800)
par(mfrow = c(1, 4)) 
boxplot(dt1$X1, ylab = "Values", col = "darkgray", xlab = "X1")
boxplot(dt1$X1 ~ dt1$A1.simple, main = "X1 by A1", ylab = "Values", xlab = "Simple", col = c("lightgray", "gray"))
boxplot(dt1$X1 ~ dt1$A1.medium,   main = "X1 by A1", ylab = "Values", xlab = "Medium", col = c("lightgray", "gray"))
boxplot(dt1$X1 ~ dt1$A1.complex, main = "X1 by A1", ylab = "Values", xlab = "Complex", col = c("lightgray", "gray"))
dev.off()


  
