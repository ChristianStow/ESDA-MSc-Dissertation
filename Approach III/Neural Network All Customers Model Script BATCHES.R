#install packages
library(tidyverse)
library(lubridate)
library(maditr)
library(forcats)
library(dbscan)
library(factoextra)
library(broom)
library(ehaGoF)
library(keras)



# Part 1: Build models ----------------------------------------------------

# Load main dataset with all customers
train <- read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/training.csv")
#filter to keep first customer only
train<-train%>%
  na.omit()

customers_847<-unique(train$customer)
customers_847<-customers_847[601:847]

train <- train[train$customer %in% customers_847, ]
  


# Define a function to build and train a model for a given customer
build_and_train_model <- function(customer_data) {
  # Prepare data for the neural network
  x_train <- as.matrix(customer_data[, 4:22])
  y_train <- customer_data$consumption
  
  # Normalize the input variables to improve model training
  x_train <- scale(x_train)
  
  # Build and compile the neural network model
  model <- keras_model_sequential() %>%
    layer_dense(units = 12, activation = "tanh", input_shape = c(19)) %>%
    layer_dense(units = 1, activation = "linear")
  
  model %>% compile(
    loss = "mean_squared_error",  # Use mean squared error for regression
    optimizer = optimizer_adam(),
    metrics = c("mean_absolute_error")  # Optional: you can monitor mean absolute error during training
  )
  
  # Train the model
  history <- model %>% fit(
    x_train, y_train,
    epochs = 20,
    batch_size = 20,
    validation_split = 0.1  # Use 10% of the data for validation
  )
  
  # Return the trained model
  return(model)
}

# Split the data by customer and build models for each customer
customer_data_list <- split(train, train$customer)
trained_models <- lapply(customer_data_list, build_and_train_model)




# Part 2: Make predictions on test dataset --------------------------------

##################################################Predict on the test dataset
# Create a list of test data subsets for each customer
test <- read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/test.csv")

#filter to keep first customer only
test<-test%>%
  na.omit()

test <- test[test$customer %in% customers_847, ]

test_data_list <- split(test, test$customer)

compute_scaling_params <- function(data) {
  center <- colMeans(data)
  scale <- apply(data, 2, sd)
  scaling_params <- list(center = center, scale = scale)
  return(scaling_params)
}

# Define a function to make predictions for a given customer using their trained model and scaling parameters
predict_customer_consumption <- function(customer_data, model, scaling_params) {
  # Prepare data for prediction
  x_test <- as.matrix(customer_data[, 4:22])
  
  # Normalize the input variables using the scaling parameters computed for the customer's training data
  x_test_scaled <- scale(x_test, center = scaling_params$center, scale = scaling_params$scale)
  
  # Make predictions using the trained model
  predictions <- predict(model, x_test_scaled)
  
  return(predictions)
}

# Create a list to store predictions for each customer
customer_predictions <- list()

# Iterate through each customer's test data and use the corresponding trained model for prediction
customer_predictions_df <- data.frame(customer = character(),
                                      original_consumption = numeric(),
                                      GMT = numeric(),
                                      predictions = numeric(),
                                      stringsAsFactors = FALSE)

# Iterate through each customer's test data and use the corresponding trained model for prediction
for (customer in names(test_data_list)) {
  customer_data <- test_data_list[[customer]]
  model <- trained_models[[customer]]
  
  # Extract original consumption and GMT variable for the current customer
  original_consumption <- customer_data$consumption
  GMT <- customer_data$GMT
  
  # Compute scaling parameters for the current customer's training data
  customer_train_data <- customer_data_list[[customer]]
  scaling_params <- compute_scaling_params(customer_train_data[, 4:22])
  
  # Predict consumption for the current customer
  predictions <- predict_customer_consumption(customer_data, model, scaling_params)
  
  # Store customer predictions and additional columns in the data frame
  customer_predictions_df <- rbind(customer_predictions_df,
                                   data.frame(customer = rep(customer, length(predictions)),
                                              original_consumption = original_consumption,
                                              GMT = GMT,
                                              predictions = predictions,
                                              stringsAsFactors = FALSE))
}

write.csv(customer_predictions_df, "C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/Neural Network Single Customer Model/test_predictions_847.csv", row.names=FALSE)

# Part 3: Make predictions on event data set -------------------------------

##################################################Predict on the test dataset
# Create a list of test data subsets for each customer
event <- read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/event.csv")

#filter to keep first customer only
event<-event%>%
  na.omit()

event <- event[event$customer %in% customers_847, ]

event_data_list <- split(event, event$customer)

compute_scaling_params <- function(data) {
  center <- colMeans(data)
  scale <- apply(data, 2, sd)
  scaling_params <- list(center = center, scale = scale)
  return(scaling_params)
}

# Define a function to make predictions for a given customer using their trained model and scaling parameters
predict_customer_consumption <- function(customer_data, model, scaling_params) {
  # Prepare data for prediction
  x_test <- as.matrix(customer_data[, 4:22])
  
  # Normalize the input variables using the scaling parameters computed for the customer's training data
  x_test_scaled <- scale(x_test, center = scaling_params$center, scale = scaling_params$scale)
  
  # Make predictions using the trained model
  predictions <- predict(model, x_test_scaled)
  
  return(predictions)
}

# Create a list to store predictions for each customer
customer_predictions <- list()

# Iterate through each customer's test data and use the corresponding trained model for prediction
customer_predictions_df <- data.frame(customer = character(),
                                      original_consumption = numeric(),
                                      GMT = numeric(),
                                      predictions = numeric(),
                                      stringsAsFactors = FALSE)

# Iterate through each customer's test data and use the corresponding trained model for prediction
for (customer in names(event_data_list)) {
  customer_data <- event_data_list[[customer]]
  model <- trained_models[[customer]]
  
  # Extract original consumption and GMT variable for the current customer
  original_consumption <- customer_data$consumption
  GMT <- customer_data$GMT
  
  # Compute scaling parameters for the current customer's training data
  customer_train_data <- customer_data_list[[customer]]
  scaling_params <- compute_scaling_params(customer_train_data[, 4:22])
  
  # Predict consumption for the current customer
  predictions <- predict_customer_consumption(customer_data, model, scaling_params)
  
  # Store customer predictions and additional columns in the data frame
  customer_predictions_df <- rbind(customer_predictions_df,
                                   data.frame(customer = rep(customer, length(predictions)),
                                              original_consumption = original_consumption,
                                              GMT = GMT,
                                              predictions = predictions,
                                              stringsAsFactors = FALSE))
}

write.csv(customer_predictions_df, "C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/Neural Network Single Customer Model/event_predictions_847.csv", row.names=FALSE)



