library(ggplot2)

data1 = read.csv("C:/Users/baysq/Desktop/income_data.csv")
data1

# untrained model
model <- lm(income ~ happiness, data = data1)
summary(model)



# Using Machine Learning package called "caret"
library(caret)
index_1 <- createDataPartition(data1$income, p = 0.7, list = FALSE)
train_data <- data1[index_1,]
test_data <- data1[-index_1,]


lm_model <- train(
  form = income ~ ., 
  data = train_data, 
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)

print(lm_model)
summary(lm_model)

# Obtain predictions on training dataset
predictions <- predict(lm_model, newdata = train_data)
print(predictions)
summary(predictions)
# Evaluate model performance (e.g., RMSE)
RMSE <- sqrt(mean((predictions - train_data$income)^2))
print(paste("Root Mean Squared Error:", RMSE))
# Calculate R-squared
rsquared <- cor(predictions, train_data)^2
print(paste("R-squared:", rsquared))


# Obtain predictions on test dataset
predictions <- predict(lm_model, newdata = test_data)
summary(predictions)
# Evaluate model performance (e.g., RMSE)
RMSE <- sqrt(mean((predictions - test_data$income)^2))
print(paste("Root Mean Squared Error:", RMSE))
# Calculate R-squared
rsquared <- cor(predictions, test_data)^2
print(paste("R-squared:", rsquared))






model_1 <- train(income ~ .,data = data1,method = "lm")
model_1

summary(model_1)

# fit training data

data2 = read.csv("C:/Users/baysq/Desktop/heart_data.csv")
data2
model2 <- train(
  heartdiseases ~ .,
  data = data2,
  method = 'lm'
)