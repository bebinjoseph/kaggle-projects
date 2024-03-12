# Load necessary libraries
#library(caret) # for data splitting
rm(list = ls())

# Load your own training and test datasets
train_data <- read.csv("train.csv")
test_data <- read.csv("test.csv")

# Fit OLS model on the training data
#init_ols_model <- lm(SalePrice ~ LotArea, data = train_data)

# Print the summary of the model
#summary(init_ols_model)

newtrain<-train_data%>%select(where(is.numeric))
num_ols_model<- lm(SalePrice ~ .-Id, data = newtrain)
# Predict on the test data using the trained model
#init_test_predictions <- predict(init_ols_model, newdata = test_data)
#init_predictions_df <- data.frame(SalePrice = init_test_predictions)
num_test_pred<- predict(num_ols_model, newdata = test_data)
num_test_pred_df<- data.frame(SalePrice=num_test_pred)
#merged_data<- cbind(test_data,predictions_df)
submission<-cbind(test_data$Id,num_test_pred_df)
names(submission)[names(submission) == "test_data$Id"] <- "Id"
write.csv(newtrain, "newtraindata.csv", row.names = FALSE)
#submission %>% rename(Id = test_data$Id)
#test_data1<-test_data.merge(test_predictions)
# Calculate metrics (if needed)
# For example, Mean Squared Error (MSE)
#mse <- mean((test_data$SalePrice - test_predictions)^2)
#print(paste("Mean Squared Error:", mse))

