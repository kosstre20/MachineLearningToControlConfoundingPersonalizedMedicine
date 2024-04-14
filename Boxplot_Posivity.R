n = 100000
expit = plogis

# Study 1 - Study 3
X1 = runif(n, -1, 1)
parfait = expit(0.5 - X1)
moins = expit(-X1+0.5*X1*X1)
complexe = expit(1.2 * X1 - 0.25 * X1 * X1 - abs(X1) + 0.5 * abs(sin(X1)))

# Study 4
X1 = rnorm(n)
parfait_4 = expit(0.5 - X1)
moins_4 = expit(-X1+0.5*X1*X1)
complexe_4 = expit(1.2*X1-0.25*X1*X1-abs(X1)+0.5*abs(sin(X1)))
# Boxplot des deux jeux de données
dev.new()
par(mfrow = c(1, 2), mar = c(5, 4, 2, 1))  # Définir une disposition 1x2
boxplot(cbind(Simple=parfait, Medium=moins, Complex=complexe), ylim = c(0, 1), main = "(A)Study 1, Study 1 and Study 3", col = c("lightgray", "gray", "darkgray"))
#abline(h = 0.5, col = "black", lty = 2)

boxplot(cbind(Simple=parfait_4, Medium=moins_4, Complex=complexe_4), ylim = c(0, 1), main = " (B)Study 4", col = c("lightgray", "gray", "darkgray"))
#abline(h = 0.45, col = "black", lty = 2)

# Réinitialiser la disposition par défaut
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)

