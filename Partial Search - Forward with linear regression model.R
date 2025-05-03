library(leaps) 
library(forecast) 

data <- read.csv("clean_data.csv")

# View(data)
dependent_var <- "average_wait_time"  
independent_vars <- setdiff(names(data), dependent_var)


# Split the dataset into training and validation sets
set.seed(123) # For reproducibility
train_indices <- sample(1:nrow(data), size = 0.6 * nrow(data), replace = FALSE)
train_data <- data[train_indices, ]
validation_data <- data[-train_indices, ]

#forward selection
regfit.fwd <- regsubsets(as.formula(paste(dependent_var, "~", paste(independent_vars, collapse = " + "))),
                         data = train_data,
                         nvmax = length(independent_vars),
                         method = "forward")
regsum.fwd <- summary(regfit.fwd)

# Identify the best model based on Adjusted R-squared
best_fwd <- which.max(regsum.fwd$adjr2)
cat("Number of variables in the best forward selection model:", best_fwd, "\n")

coef(regfit.fwd,best_fwd)

# Extract the names of the selected variables
selected_vars <- names(coef(regfit.fwd, best_fwd))[-1]  # Exclude intercept

# Filter out factor levels and keep only base variable names
selected_vars_cleaned <- unique(gsub("\\:.*$", "", gsub("store_location.*", "store_location", selected_vars)))

# Construct the formula for the linear regression model
fwd_formula <- as.formula(paste(dependent_var, "~", paste(selected_vars_cleaned, collapse = " + ")))
cat("Formula used for linear regression:\n")
print(fwd_formula)

# Fit the linear regression model with the selected variables
fwd_model <- lm(fwd_formula, data = train_data)

# Make predictions on the validation set
predicted_fwd <- predict(fwd_model, newdata = validation_data)

# Evaluate the predictive performance of the model
performance_metrics <- forecast::accuracy(predicted_fwd, validation_data[[dependent_var]])
print("Performance Metrics (Validation Set):")
print(performance_metrics)

# Plot Adjusted R-squared for visualization
plot(regsum.fwd$adjr2, xlab = "Number of Variables", ylab = "Adjusted R-squared", type = "l")
points(best_fwd, regsum.fwd$adjr2[best_fwd], col = "red", cex = 2, pch = 20)
