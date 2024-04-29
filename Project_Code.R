attach(machine)
# Plot erp vs prp with colored points
plot(machine$prp, machine$erp, 
     xlab = "prp", ylab = "erp",
     main = "erp vs prp")

# Create a formula for the erp regression model
erp_formula <- formula(erp ~ . - model - vendor)
# Fit the erp multiple linear regression model
erp_model <- lm(erp_formula, data = machine)
summary(erp_model)

# Create a formula for the prp regression model
prp_formula <- formula(prp ~ . - model - vendor)
# Fit the prp multiple linear regression model
prp_model <- lm(prp_formula, data = machine)
summary(prp_model)

# Create a formula for the reduced prp regression model
reduced_prp_formula <- formula(prp ~ chmax)
# Fit the reduced prp multiple linear regression model
reduced_prp_model <- lm(reduced_prp_formula, data = machine)
# Display the summary of the reduced prp model
summary(reduced_prp_model)

# Perform ANOVA for reduced erp model
reduced_erp_anova <- anova(reduced_erp_model)
print(reduced_erp_anova)

# Perform ANOVA for reduced prp model
reduced_prp_anova <- anova(reduced_prp_model)
print(reduced_prp_anova)

# Create a scatter plot matrix for the predictors and erp
plot_data <- cbind(machine$erp, machine[, c("myct", "mmin", "mmax", "chmax")])
colnames(plot_data) <- c("erp", "myct", "mmin", "mmax", "chmax")
pairs(plot_data)

# Create scatter plot for reduced prp model
plot(machine$chmax, machine$prp,
     xlab = "chmax", ylab = "prp",
     main = "Scatter Plot - Reduced prp Model")
abline(reduced_prp_model, col = "blue")

# Check assumptions for the reduced erp model
# 1. Linearity of the Relationship
plot(reduced_erp_model$fitted.values, reduced_erp_model$residuals,
     xlab = "Predicted erp", ylab = "Residuals",
     main = "Residuals vs. Fitted Values")

# 4. Normality of Residuals
hist(reduced_erp_model$residuals, main = "Histogram of Residuals")
qqnorm(reduced_erp_model$residuals, main = "Q-Q Plot of Residuals")
qqline(reduced_erp_model$residuals)

# Check assumptions for the reduced prp model
# 1. Linearity of the Relationship
plot(reduced_prp_model$fitted.values, reduced_prp_model$residuals,
     xlab = "Predicted prp", ylab = "Residuals",
     main = "Residuals vs. Fitted Values")

# 4. Normality of Residuals
hist(reduced_prp_model$residuals, main = "Histogram of Residuals")
qqnorm(reduced_prp_model$residuals, main = "Q-Q Plot of Residuals")
qqline(reduced_prp_model$residuals)
