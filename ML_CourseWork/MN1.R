#TASK 1

install.packages("readxl")
install.packages("neuralnet")
install.packages("ggplot2")
install.packages("dplyr")

library(dplyr)
library(neuralnet)
library(ggplot2)
library(readxl)
setwd("D:/2nd Year/ML_CourseWork")
UOW_data <- read_excel("uow_consumption.xlsx")

# Rename columns
colnames(UOW_data) <- c("date", "time_eighteen", "time_nineteen", "time_twenty")
head(UOW_data)

#apply lag method
UOW_data$lag_1 <- lag(UOW_data$time_twenty, 1)
UOW_data$lag_2 <- lag(UOW_data$time_twenty, 2)
UOW_data$lag_3 <- lag(UOW_data$time_twenty, 3)
UOW_data$lag_4 <- lag(UOW_data$time_twenty, 4)
UOW_data$lag_7 <- lag(UOW_data$time_twenty, 7)
UOW_data <- na.omit(UOW_data)

#TASK 3
#dividing data to testing and training
UOW_train <- UOW_data[1:380,]
UOW_test <- UOW_data[381:nrow(UOW_data),]

#TASK 4
#normalization

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Exclude the date column from normalization
UOW_train_normalized <- as.data.frame(lapply(UOW_train[-1], normalize))
UOW_test_normalized <- as.data.frame(lapply(UOW_test[-1], normalize))

# Set the column names of the test_normalized data frame
colnames(UOW_test_normalized) <- colnames(UOW_train_normalized)

#TASK 5

input_vectors <- list(
  c("lag_1"),
  c("lag_1", "lag_2"),
  c("lag_1", "lag_2", "lag_3"),
  c("lag_1", "lag_2", "lag_3", "lag_4"),
  c("lag_1", "lag_7"),
  c("lag_1", "lag_2", "lag_7"),
  c("lag_1", "lag_2", "lag_3", "lag_7"),
  c("lag_1", "lag_2", "lag_3", "lag_4", "lag_7")
)

build_mlp_model <- function(train_data, test_data, input_vars, hidden_structure) {
  formula <- paste("time_twenty ~", paste(input_vars, collapse = " + "))
  nn <- neuralnet(as.formula(formula), train_data, hidden = hidden_structure)
  test_matrix <- as.matrix(test_data[, input_vars, drop = FALSE])
  colnames(test_matrix) <- colnames(train_data[, input_vars, drop = FALSE])
  predictions <- predict(nn, test_matrix)
  return(list(model = nn, predictions = predictions))
}

models <- list()
for (i in 1:length(input_vectors)) {
  models[[i]] <- build_mlp_model(UOW_train_normalized, UOW_test_normalized, input_vectors[[i]], c(5))
}


#TASK 6
#calculated using the standard statistical indices (RMSE, MAE, MAPE and sMAPE – symmetric MAPE)
calculate_metrics <- function(actual, predicted) {
  rmse <- sqrt(mean((actual - predicted)^2))
  mae <- mean(abs(actual - predicted))
  mape <- mean(abs(actual - predicted) / predicted)
  smape <- mean(abs(actual - predicted) / (abs(actual) + abs(predicted)) * 2) * 100
  return(list(RMSE = rmse, MAE = mae, MAPE = mape, sMAPE = smape))
} 

evaluation_metrics <- list()
for (i in 1:length(models)) {
  evaluation_metrics[[i]] <- calculate_metrics(UOW_test_normalized$time_twenty, models[[i]]$predictions)
}


#TASK 7
#Create a comparison table of their testing performances
comparison_table <- data.frame(
  Model_Description = c("AR(1)", "AR(2)", "AR(3)", "AR(4)", "AR(1,7)", "AR(2,7)", "AR(3,7)", "AR(4,7)"),
  RMSE = sapply(evaluation_metrics, function(x) x$RMSE),
  MAE = sapply(evaluation_metrics, function(x) x$MAE),
  MAPE = sapply(evaluation_metrics, function(x) x$MAPE),
  sMAPE = sapply(evaluation_metrics, function(x) x$sMAPE)
)

print(comparison_table)

# Add more models with different hidden layer structures and input vectors to create 12-15 models in total

# Efficiency comparison between one-hidden layer and two-hidden layer networks

model_1_hidden <- build_mlp_model(UOW_train_normalized, UOW_test_normalized, c("lag_1", "lag_2", "lag_3", "lag_7"), c(5))
model_2_hidden <- build_mlp_model(UOW_train_normalized, UOW_test_normalized, c("lag_1", "lag_2", "lag_3", "lag_7"), c(3, 2))

# Check the total number of weight parameters per network
num_weights_1_hidden <- sum(sapply(model_1_hidden$model$weights, length))
num_weights_2_hidden <- sum(sapply(model_2_hidden$model$weights, length))

cat("Total number of weight parameters for the one-hidden layer network:", num_weights_1_hidden, "\n")
cat("Total number of weight parameters for the two-hidden layer network:", num_weights_2_hidden, "\n")



#Part2

#Task1 
# Add the 18th and 19th hour attributes to the input vectors
narx_input_vectors <- list(
  c("lag_1", "time_eighteen", "time_nineteen"),
  c("lag_1", "lag_2", "time_eighteen", "time_nineteen"),
  c("lag_1", "lag_2", "lag_3", "time_eighteen", "time_nineteen"),
  c("lag_1", "lag_2", "lag_3", "lag_7", "time_eighteen", "time_nineteen"),
  c("lag_1", "lag_2", "lag_3", "lag_4", "lag_7", "time_eighteen", "time_nineteen")
)

# Build NARX models
narx_models <- list()
for (i in 1:length(narx_input_vectors)) {
  narx_models[[i]] <- build_mlp_model(UOW_train_normalized, UOW_test_normalized, narx_input_vectors[[i]], c(5))
}

# Evaluate NARX models
narx_evaluation_metrics <- list()
for (i in 1:length(narx_models)) {
  narx_evaluation_metrics[[i]] <- calculate_metrics(UOW_test_normalized$time_twenty, narx_models[[i]]$predictions)
}

# Create a comparison table for NARX models
narx_comparison_table <- data.frame(
  Model_Description = c("NARX(1,18,19)", "NARX(2,18,19)", "NARX(3,18,19)", "NARX(3,7,18,19)", "NARX(4,7,18,19)"),
  RMSE = sapply(narx_evaluation_metrics, function(x) x$RMSE),
  MAE = sapply(narx_evaluation_metrics, function(x) x$MAE),
  MAPE = sapply(narx_evaluation_metrics, function(x) x$MAPE),
  sMAPE = sapply(narx_evaluation_metrics, function(x) x$sMAPE)
)

print(narx_comparison_table)

#Task 2 

# Denormalize the predictions
denormalize <- function(x, min_value, max_value) {
  return(x * (max_value - min_value) + min_value)
}

best_model_index <- which.min(sapply(evaluation_metrics, function(x) x$RMSE))
best_model <- models[[best_model_index]]
best_model_predictions <- best_model$predictions

min_value <- min(UOW_train$time_twenty)
max_value <- max(UOW_train$time_twenty)

denormalized_predictions <- denormalize(best_model_predictions, min_value, max_value)

# Plot the predicted output vs. desired output using a line chart
plot(UOW_test$time_twenty, type = "l", col = "blue", xlab = "Time", ylab = "Hour 20 Consumption", main = "Line Chart of Desired vs. Predicted Output")
lines(denormalized_predictions, col = "red")
legend("topleft", legend = c("Desired Output", "Predicted Output"), col = c("blue", "red"), lty = 1, cex = 0.8)




