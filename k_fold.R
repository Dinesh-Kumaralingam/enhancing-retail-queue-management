#K-fold no selection
clean_data<- read.csv("clean_data.csv")
library(caret)
train_control <- trainControl(method = "cv",number = 10)
model <- train(average_wait_time~.,data=clean_data,
                method = "lm",
                trControl = train_control)

predicted.data <- predict(model,clean_data)
forecast::accuracy(predicted.data,clean_data$average_wait_time)
ggplot(data.frame(Actual = clean_data$average_wait_time, Predicted = predicted.data),
       aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted Values", x = "Actual", y = "Predicted") +
  theme_minimal()
#Forward_Variable_selection

train_control <- trainControl(method = "cv",number = 10)
model1 <- train(average_wait_time~num_cashiers + arrival_rate + 
                 service_rate_per_cashier +queue_length + avg_items_per_customer + 
                 self_checkout_percentage +evening + experienced + new + snow +
                 mobile_payment,data = clean_data,
                 method = "lm",
                 trControl = train_control)

predicted.data1 <- predict(model1,clean_data)
forecast::accuracy(predicted.data1,clean_data$average_wait_time)

ggplot(data.frame(Actual = clean_data$average_wait_time, Predicted = predicted.data1),
       aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted Values", x = "Actual", y = "Predicted") +
  theme_minimal()

 
#Best-Fit_Variable_selection

model2 <- train(average_wait_time~arrival_rate+queue_length+avg_items_per_customer
               +self_checkout_percentage+weekend+evening+experienced+
                 snow+clear+holiday+sale+product_launch,data=clean_data,
               method = "lm",
               trControl = train_control)

predicted.data2 <- predict(model2,clean_data)
forecast::accuracy(predicted.data2,clean_data$average_wait_time)
ggplot(data.frame(Actual = clean_data$average_wait_time, Predicted = predicted.data2),
       aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted Values", x = "Actual", y = "Predicted") +
  theme_minimal()


#Exhaustive-Variable_selection
model3 <- train(average_wait_time~arrival_rate+queue_length+avg_items_per_customer
               +self_checkout_percentage+weekend+evening+experienced+
                 snow+clear+holiday+sale+product_launch,
               data=clean_data,
               method = "lm",
               trControl = train_control)
predicted.data3 <- predict(model3,clean_data)
forecast::accuracy(predicted.data3,clean_data$average_wait_time)
ggplot(data.frame(Actual = clean_data$average_wait_time, Predicted = predicted.data2),
       aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted Values", x = "Actual", y = "Predicted") +
  theme_minimal()
#backward
model4 <- train(average_wait_time~arrival_rate+queue_length+avg_items_per_customer
               +self_checkout_percentage+ weekend +evening+experienced+snow+clear
               + holiday+sale+product_launch,
               data=clean_data,
               method = "lm",
               trControl = train_control)

predicted.data4 <- predict(model4,clean_data)
#Backward-Variable_selection
forecast::accuracy(predicted.data4,clean_data$average_wait_time)
ggplot(data.frame(Actual = clean_data$average_wait_time, Predicted = predicted.data4),
       aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted Values", x = "Actual", y = "Predicted") +
  theme_minimal()
