library(caret)
set.seed(1) #set a seed for reproducability
data <- read.csv("Clean_data.csv")
myIndex <- sample (nrow(data),
                   size= nrow(data)*0.6)
trainvalidationSet <- data[myIndex,]
testSet <- data[-myIndex,]

# 10-fold cross validation
ctrl <- trainControl(method = "cv",number=10) 
myGrid <- expand.grid(k=c(1:15))
knn.mod <- train(average_wait_time~num_cashiers + arrival_rate + 
                   service_rate_per_cashier +queue_length + avg_items_per_customer + 
                   self_checkout_percentage +evening + experienced + new + snow +
                   mobile_payment,
                 data = trainvalidationSet,
                 method = "knn",
                 trControl = ctrl,
                 tuneGrid=myGrid,
                 preProc=c("center","scale")
)
knn.mod# standardize predictors
# check preprocessing
knn.mod$preProcess$std
knn.mod$preProcess$mean

predicted_time <- predict(knn.mod,
                          newdata = testSet)
# Actual and predicted values
actual <- testSet$average_wait_time
predicted <- predicted_time

# Mean Absolute Error
mae <- mean(abs(actual - predicted))
print(paste("Mean Absolute Error:", mae))

# Root Mean Squared Error
rmse <- sqrt(mean((actual - predicted)^2))
print(paste("Root Mean Squared Error:", rmse))

# R-squared
ss_total <- sum((actual - mean(actual))^2)
ss_residual <- sum((actual - predicted)^2)
r_squared <- 1 - (ss_residual / ss_total)
print(paste("R-squared:", r_squared))
plot(actual,predicted)

#full_model
knn.mod <- train(average_wait_time~num_cashiers + arrival_rate + 
                   service_rate_per_cashier +queue_length + avg_items_per_customer + 
                   self_checkout_percentage +evening + experienced + new + snow +
                   mobile_payment,
                 data = data,
                 method = "knn",
                 trControl = ctrl,
                 tuneGrid=myGrid,
                 preProc=c("center","scale")
)
knn.mod# standardize predictors
# check preprocessing
knn.mod$preProcess$std
knn.mod$preProcess$mean

predicted_time <- predict(knn.mod,
                          newdata = data)
# Actual and predicted values
actual <- data$average_wait_time
predicted <- predicted_time

# Mean Absolute Error
mae <- mean(abs(actual - predicted))
print(paste("Mean Absolute Error:", mae))

# Root Mean Squared Error
rmse <- sqrt(mean((actual - predicted)^2))
print(paste("Root Mean Squared Error:", rmse))

# R-squared
ss_total <- sum((actual - mean(actual))^2)
ss_residual <- sum((actual - predicted)^2)
r_squared <- 1 - (ss_residual / ss_total)
print(paste("R-squared:", r_squared))
plot(actual,predicted)

