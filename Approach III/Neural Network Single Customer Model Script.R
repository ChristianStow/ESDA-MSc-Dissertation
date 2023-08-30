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

#load main dataset with all customers
train<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/training.csv")

#filter to keep first customer only
train_first_customer<-train%>%
  filter(customer=="D0000")%>%
  na.omit()

####Prepare data for the neural network
# Extract the input variables and target variable
x_train <- as.matrix(train_first_customer[c(4:22)])
y_train <- train_first_customer$consumption

# Normalize the input variables to improve model training
x_train <- scale(x_train)


#Build and compile the neural network model with the updated target variable
model2 <- keras_model_sequential() %>%
  layer_dense(units = 12, activation = "tanh", input_shape = c(19)) %>%
  layer_dense(units = 1, activation = "linear")  # Output layer with 'linear' activation for regression

model2 %>% compile(
  loss = "mean_squared_error",  # Use mean squared error for regression
  optimizer = optimizer_adam(),
  metrics = c("mean_absolute_error")  # Optional: you can monitor mean absolute error during training
)

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = 20,
  batch_size = 20,
  validation_split = 0.1  # Use 10% of the data for validation
)


