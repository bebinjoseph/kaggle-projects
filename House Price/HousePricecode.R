# Load necessary libraries
library(caret) # for data splitting
library(leaps)
rm(list = ls())
#sum(is.na())
# Load your own training and test datasets
train_data <- read.csv("train.csv")
test_data <- read.csv("test.csv")

# Fit OLS model on the training data
#init_ols_model <- lm(SalePrice ~ LotArea, data = train_data)
tic()
fwsel = regsubsets(SalePrice~.-Id, data = newtrain, nvmax = 15, method = "forward")
toc()


####
tic()
# Compute correlation matrix with NA handling
correlation_matrix <- cor(newtrain[, -which(names(newtrain) == "Id")], use = "pairwise.complete.obs")

# Find highly correlated variables
highly_correlated <- findCorrelation(correlation_matrix, cutoff = 0.8, verbose = TRUE)

# Print highly correlated variables
cat("Highly correlated variables:\n")
print(names(newtrain)[highly_correlated])

# Remove highly correlated variables from the dataset
newtrain_clean <- newtrain[, -highly_correlated]

# Re-run subset selection
fwsel <- regsubsets(SalePrice ~ . - Id, data = newtrain_clean, nvmax = 15, method = "forward")

toc()
############

sumbfs = summary(fwsel)

#print results
sumbfs

kbicf=which.min(sumbfs$bic) #BIC choice
kaicf=which.min(sumbfs$cp)  #AIC choice (AIC proportional to Cp and so ranking is the same)
which.max(sumbfs$adjr2) #for kicks, see the model with the highest adjusted R-squared - does it overfit?

plot((sumbfs$bic-mean(sumbfs$bic))/sd(sumbfs$bic), main = "AIC and BIC (Standardized)", xlab = "k",
     ylab = "IC", type = "l", col = "blue")
points((sumbfs$cp-mean(sumbfs$cp))/sd(sumbfs$cp), type = "l", col = "red")
legend("topright", c("BIC","AIC"),lty=c(1,1) ,col=c("blue","red"))

## Calculate OOS MSE using AIC and BIC Minimizing models:
#Get the X-matrix for the test set:
#test.mat = model.matrix(Sa~.-ID, data = test)

#extract coefficients from the best model on BIC
temp.coef = coef(fwsel, id = kbicf)
# Get the selected variables from the forward selection result
selected_vars <- names(coef(fwsel, id = 15))

# Fit the regression model using the selected variables
# Fit the regression model using the selected variables
final_model <- lm(SalePrice ~ MSSubClass + LotArea + OverallQual + YearBuilt + YearRemodAdd + MasVnrArea + BsmtFinSF1 + GrLivArea + BsmtFullBath + BedroomAbvGr + TotRmsAbvGrd + GarageCars + WoodDeckSF + ScreenPorch + PoolArea, data = newtrain_clean)


# Summarize the final model
summary(final_model)

# Define the variable names
selva <- c("MSSubClass", "LotArea", "OverallQual", "YearBuilt", "YearRemodAdd", 
           "MasVnrArea", "BsmtFinSF1", "GrLivArea", "BsmtFullBath", "BedroomAbvGr", 
           "TotRmsAbvGrd", "GarageCars", "WoodDeckSF", "ScreenPorch", "PoolArea")

# Subset test_dat using the selected variables
test_subset <- test_data[, selva]


# Calculate the mean SalePrice
mean_saleprice <- mean(submission3$SalePrice, na.rm = TRUE)

# Impute missing SalePrice values with the mean
submission3$SalePrice[is.na(submission3$SalePrice)] <- mean_saleprice


fssel_test_pred<- predict(final_model, newdata = test_data)
fssel_test_pred_df<- data.frame(SalePrice=fssel_test_pred)
submission3<-cbind(test_data$Id,fssel_test_pred_df)
names(submission3)[names(submission3) == "test_data$Id"] <- "Id"
write.csv(submission3, "submission3forwardselection.csv", row.names = FALSE)
#MSEBIC = mean((test$Balance-test.mat[,names(temp.coef)]%*%temp.coef)^2)

# Print the summary of the model
#summary(init_ols_model)

#newtrain<-train_data%>%select(where(is.numeric))
#num_ols_model<- lm(SalePrice ~ .-Id, data = newtrain)
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

