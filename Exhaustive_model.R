clean_data<- read.csv("clean_data.csv")
set.seed(1) 
train_indices <- sample(1:nrow(clean_data),
                        size=nrow(clean_data)*0.6,
                        replace=F)
training_set <- clean_data[train_indices,]
validation_set <- clean_data[-train_indices,]
library(leaps)
#exhaustive search
regfit.exhaustive <- regsubsets(average_wait_time ~ .,
                          data = training_set,
                          nvmax = 25,
                          method = 'exhaustive') 
reg.summary <- summary(regfit.exhaustive)
names(reg.summary)
reg.summary$adjr2
plot(reg.summary$adjr2, xlab = "Number of Variables",
     ylab = "Adjusted RSq", type = "l")

#reg.summary$cp
which.max(reg.summary$adjr2)
plot(reg.summary$adjr2, xlab = "Number of Variables",
     ylab = "Adjusted RSq", type = "l")
points(12, reg.summary$adjr2[12], col = "red", cex = 2,
       pch = 20)
coef(regfit.exhaustive,12)

#model
#Let's estimate the model chosen by the best subset on training dataset
exhaustive.mod <- lm(average_wait_time~arrival_rate+queue_length+avg_items_per_customer
                     +self_checkout_percentage+weekend+evening+experienced+
                       snow+clear+holiday+sale+product_launch,
                     data=training_set)
# predict Salaries on validation sample and evaluate predictive performance
predicted.exhaustive <- predict(exhaustive.mod,validation_set)

forecast::accuracy(predicted.exhaustive,validation_set$average_wait_time)
#summary(regfit.exhaustive)

