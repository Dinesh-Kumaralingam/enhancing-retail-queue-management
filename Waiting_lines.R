library(dplyr)
#install.packages('tidyverse')
library(tidyverse)
library(ggplot2)
library(corrplot)
#Step 1: Reading the data set
MyData <- read.csv('waiting_lines.csv')
dim(MyData)
#Counting the total no of missing values in each column
sapply(MyData,function(x) sum(is.na(x)))
#Counting the total no of missing values in whole data set
sum(is.na(MyData))
#Removing first column
MyData<-MyData[,-1]
#Getting the percentage of missing data
missing_data_perc <- sapply(MyData, function(x) mean(is.na(x)) * 100)
missing_data_perc_df <- data.frame(Variables = names(missing_data_perc), 
                              Missing_Percentage = missing_data_perc)
#reordering for values in descending order
missing_data_perc_df$Variables <- factor(missing_data_perc_df$Variables,
  levels = missing_data_perc_df$Variables[order(-missing_data_perc_df$Missing_Percentage)])

#Plotting graph for missing data percentage
ggplot(data=missing_data_perc_df ,mapping= aes(x= Variables,
                                                  y= Missing_Percentage))+
  geom_bar(stat = "identity", fill = "orange")+coord_flip()
#omit the data where the value is NA
C_Data<-na.omit(MyData)   
View(C_Data)
sapply(C_Data,function(x) sum(is.na(x)))
#Counting the total no.of missing values
sum(is.na(C_Data))
dim(C_Data)
C_Data <- subset(C_Data,select = -date)
#dim(C_Data)
#View(C_Data)
#creating correlation
numeric_data <- select_if(C_Data, is.numeric)
correlation_matrix <- cor(numeric_data, use = "complete.obs")
corrplot(correlation_matrix, method = "shade", type = "full", tl.col = "black", tl.srt = 45)
#1.Converting day of week to weekend and weekday and created a dummy variables and  numerical variable
C_Data$weekend <- ifelse( C_Data$day_of_week=="Saturday" |C_Data$day_of_week=="Sunday",1,0)
#2.Creating dummy variables for time of day  and assigned a numerical value
C_Data$morning <- ifelse( C_Data$time_of_day=="Morning",1,0)
C_Data$evening <- ifelse( C_Data$time_of_day=="Evening",1,0)
#3.Creating dummy variables for payment_method  and assigned a numerical value
C_Data$cash <- ifelse( C_Data$payment_method=="Cash",1,0)
C_Data$card <- ifelse( C_Data$payment_method=="Card",1,0)
C_Data$mobile_payment <- ifelse( C_Data$payment_method=="Mobile Payment",1,0)
#4.Creating dummy variables for cashier_experience  and assigned a numerical value
C_Data$experienced <- ifelse( C_Data$cashier_experience=="Experienced",1,0)
C_Data$new <- ifelse( C_Data$cashier_experience=="New",1,0)
#5. Creating dummy variables for weather and assigned a numerical value
C_Data$snow <- ifelse( C_Data$weather=="Snow",1,0)
C_Data$clear <- ifelse( C_Data$weather=="Clear",1,0)
C_Data$rain <- ifelse( C_Data$weather=="Rain",1,0)
View(C_Data)
#6. Creating dummy variables for special_event and assigned a numerical value
C_Data$holiday <- ifelse( C_Data$special_event=="Holiday",1,0)
C_Data$sale <- ifelse( C_Data$special_event=="Sale",1,0)
C_Data$product_launch <- ifelse( C_Data$special_event=="Product Launch",1,0)
View(C_Data)
#Dropping the categorical variables and only having numerical data's
N_Data <-  subset(C_Data, select = -c(weather,day_of_week,special_event
                                      ,payment_method,time_of_day,cashier_experience))
View(N_Data)
#correlation plot for all numeric data
numeric_data1 <- select_if(N_Data, is.numeric)
correlation_matrix1 <- cor(numeric_data1)
corrplot(correlation_matrix1, method = "shade", type = "full", tl.col = "black", tl.srt = 45)


#remove negative values in average wait time
negative_rows <- apply(N_Data < 0, 1, any)

negative_rows
#Filter out rows with any negative values
clean_data <- N_Data[!negative_rows, ]
#View(clean_data)
#detecting outlier
# Calculate Q1, Q3, and IQR
Q1 <- quantile(clean_data$average_wait_time, 0.25)
Q3 <- quantile(clean_data$average_wait_time, 0.75)
IQR <- Q3 - Q1

# Define outlier bounds
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

mean_value <- round(mean(clean_data$arrival_rate))

limit= 120
#inserting the mean value in arrival rate
clean_data$arrival_rate[clean_data$arrival_rate > limit] <- mean_value

#exporting to csv file
write.csv(clean_data, "clean_data.csv", row.names = FALSE)


