tensorflow::install_tensorflow()
library(keras)

#install packages
library(tidyverse)
library(lubridate)
library(maditr)
library(forcats)
library(dbscan)
library(factoextra)
library(broom)
library(ehaGoF)


#Tuning

############BASIC MODEL -NO TUNING
#Build and compile the neural network model with the updated target variable
model2 <- keras_model_sequential() %>%
  layer_dense(units = 8, activation = "relu", input_shape = c(18)) %>%
  layer_dense(units = 5, activation = "relu")%>%
  layer_dense(units = 1, activation = "linear")  # Output layer with 'linear' activation for regression

model2 %>% compile(
  loss = "mean_squared_error",  # Use mean squared error for regression
  optimizer = optimizer_adam(),
  metrics = c("mean_absolute_error")  # Optional: you can monitor mean absolute error during training
)


#train the model
epochs <- 1
batch_size <- 32

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)



#INCREASE FIRST LAYER FIRST - FIRST LAYER FIXED AT 8
#FIRST LAYER =16


model2 <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape = c(18)) %>%
  layer_dense(units = 8, activation = "relu")%>%
  layer_dense(units = 1, activation = "linear")  # Output layer with 'linear' activation for regression

model2 %>% compile(
  loss = "mean_squared_error",  # Use mean squared error for regression
  optimizer = optimizer_adam(),
  metrics = c("mean_absolute_error")  # Optional: you can monitor mean absolute error during training
)


#train the model
epochs <- 5
batch_size <- 32

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)

F16S4<-history2$metrics$val_mean_absolute_error








#FIRST LAYER =32
model2 <- keras_model_sequential() %>%
  layer_dense(units = 32, activation = "relu", input_shape = c(18)) %>%
  layer_dense(units = 8, activation = "relu")%>%
  layer_dense(units = 1, activation = "linear")  # Output layer with 'linear' activation for regression

model2 %>% compile(
  loss = "mean_squared_error",  # Use mean squared error for regression
  optimizer = optimizer_adam(),
  metrics = c("mean_absolute_error")  # Optional: you can monitor mean absolute error during training
)


#train the model
epochs <- 5
batch_size <- 32

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)

F32S4<-history2$metrics$val_mean_absolute_error





#FIRST LAYER =48
model2 <- keras_model_sequential() %>%
  layer_dense(units = 48, activation = "relu", input_shape = c(18)) %>%
  layer_dense(units = 8, activation = "relu")%>%
  layer_dense(units = 1, activation = "linear")  # Output layer with 'linear' activation for regression

model2 %>% compile(
  loss = "mean_squared_error",  # Use mean squared error for regression
  optimizer = optimizer_adam(),
  metrics = c("mean_absolute_error")  # Optional: you can monitor mean absolute error during training
)


#train the model
epochs <- 5
batch_size <- 32

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)


F48S4<-history2$metrics$val_mean_absolute_error





#FIRST LAYER =64
model2 <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu", input_shape = c(18)) %>%
  layer_dense(units = 8, activation = "relu")%>%
  layer_dense(units = 1, activation = "linear")  # Output layer with 'linear' activation for regression

model2 %>% compile(
  loss = "mean_squared_error",  # Use mean squared error for regression
  optimizer = optimizer_adam(),
  metrics = c("mean_absolute_error")  # Optional: you can monitor mean absolute error during training
)


#train the model
epochs <- 5
batch_size <- 32

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)

F64S4<-history2$metrics$val_mean_absolute_error





#INCREASE SECOND LAYER  TO 16
model2 <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape = c(18)) %>%
  layer_dense(units = 16, activation = "relu")%>%
  layer_dense(units = 1, activation = "linear")  # Output layer with 'linear' activation for regression

model2 %>% compile(
  loss = "mean_squared_error",  # Use mean squared error for regression
  optimizer = optimizer_adam(),
  metrics = c("mean_absolute_error")  # Optional: you can monitor mean absolute error during training
)


#train the model
epochs <- 5
batch_size <- 32

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)

F16S16<-history2$metrics$val_mean_absolute_error








#FIRST LAYER =32
model2 <- keras_model_sequential() %>%
  layer_dense(units = 32, activation = "relu", input_shape = c(18)) %>%
  layer_dense(units = 16, activation = "relu")%>%
  layer_dense(units = 1, activation = "linear")  # Output layer with 'linear' activation for regression

model2 %>% compile(
  loss = "mean_squared_error",  # Use mean squared error for regression
  optimizer = optimizer_adam(),
  metrics = c("mean_absolute_error")  # Optional: you can monitor mean absolute error during training
)


#train the model
epochs <- 5
batch_size <- 32

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)

F32S16<-history2$metrics$val_mean_absolute_error





#FIRST LAYER =48
model2 <- keras_model_sequential() %>%
  layer_dense(units = 48, activation = "relu", input_shape = c(18)) %>%
  layer_dense(units = 16, activation = "relu")%>%
  layer_dense(units = 1, activation = "linear")  # Output layer with 'linear' activation for regression

model2 %>% compile(
  loss = "mean_squared_error",  # Use mean squared error for regression
  optimizer = optimizer_adam(),
  metrics = c("mean_absolute_error")  # Optional: you can monitor mean absolute error during training
)


#train the model
epochs <- 5
batch_size <- 32

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)


F48S16<-history2$metrics$val_mean_absolute_error





#FIRST LAYER =64
model2 <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu", input_shape = c(18)) %>%
  layer_dense(units = 16, activation = "relu")%>%
  layer_dense(units = 1, activation = "linear")  # Output layer with 'linear' activation for regression

model2 %>% compile(
  loss = "mean_squared_error",  # Use mean squared error for regression
  optimizer = optimizer_adam(),
  metrics = c("mean_absolute_error")  # Optional: you can monitor mean absolute error during training
)


#train the model
epochs <- 5
batch_size <- 32

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)

F64S16<-history2$metrics$val_mean_absolute_error






#INCREASE SECOND LAYER  TO 32
model2 <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape = c(18)) %>%
  layer_dense(units = 32, activation = "relu")%>%
  layer_dense(units = 1, activation = "linear")  # Output layer with 'linear' activation for regression

model2 %>% compile(
  loss = "mean_squared_error",  # Use mean squared error for regression
  optimizer = optimizer_adam(),
  metrics = c("mean_absolute_error")  # Optional: you can monitor mean absolute error during training
)


#train the model
epochs <- 5
batch_size <- 32

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)

F16S32<-history2$metrics$val_mean_absolute_error








#FIRST LAYER =32
model2 <- keras_model_sequential() %>%
  layer_dense(units = 32, activation = "relu", input_shape = c(18)) %>%
  layer_dense(units = 32, activation = "relu")%>%
  layer_dense(units = 1, activation = "linear")  # Output layer with 'linear' activation for regression

model2 %>% compile(
  loss = "mean_squared_error",  # Use mean squared error for regression
  optimizer = optimizer_adam(),
  metrics = c("mean_absolute_error")  # Optional: you can monitor mean absolute error during training
)


#train the model
epochs <- 5
batch_size <- 32

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)

F32S32<-history2$metrics$val_mean_absolute_error





#FIRST LAYER =48
model2 <- keras_model_sequential() %>%
  layer_dense(units = 48, activation = "relu", input_shape = c(18)) %>%
  layer_dense(units = 32, activation = "relu")%>%
  layer_dense(units = 1, activation = "linear")  # Output layer with 'linear' activation for regression

model2 %>% compile(
  loss = "mean_squared_error",  # Use mean squared error for regression
  optimizer = optimizer_adam(),
  metrics = c("mean_absolute_error")  # Optional: you can monitor mean absolute error during training
)


#train the model
epochs <- 5
batch_size <- 32

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)


F48S32<-history2$metrics$val_mean_absolute_error





#FIRST LAYER =64
model2 <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu", input_shape = c(18)) %>%
  layer_dense(units = 32, activation = "relu")%>%
  layer_dense(units = 1, activation = "linear")  # Output layer with 'linear' activation for regression

model2 %>% compile(
  loss = "mean_squared_error",  # Use mean squared error for regression
  optimizer = optimizer_adam(),
  metrics = c("mean_absolute_error")  # Optional: you can monitor mean absolute error during training
)


#train the model
epochs <- 5
batch_size <- 32

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)

F64S32<-history2$metrics$val_mean_absolute_error






#INCREASE SECOND LAYER  TO 48
model2 <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape = c(18)) %>%
  layer_dense(units = 48, activation = "relu")%>%
  layer_dense(units = 1, activation = "linear")  # Output layer with 'linear' activation for regression

model2 %>% compile(
  loss = "mean_squared_error",  # Use mean squared error for regression
  optimizer = optimizer_adam(),
  metrics = c("mean_absolute_error")  # Optional: you can monitor mean absolute error during training
)


#train the model
epochs <- 5
batch_size <- 32

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)

F16S48<-history2$metrics$val_mean_absolute_error








#FIRST LAYER =32
model2 <- keras_model_sequential() %>%
  layer_dense(units = 32, activation = "relu", input_shape = c(18)) %>%
  layer_dense(units = 48, activation = "relu")%>%
  layer_dense(units = 1, activation = "linear")  # Output layer with 'linear' activation for regression

model2 %>% compile(
  loss = "mean_squared_error",  # Use mean squared error for regression
  optimizer = optimizer_adam(),
  metrics = c("mean_absolute_error")  # Optional: you can monitor mean absolute error during training
)


#train the model
epochs <- 5
batch_size <- 32

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)

F32S48<-history2$metrics$val_mean_absolute_error





#FIRST LAYER =48
model2 <- keras_model_sequential() %>%
  layer_dense(units = 48, activation = "relu", input_shape = c(18)) %>%
  layer_dense(units = 48, activation = "relu")%>%
  layer_dense(units = 1, activation = "linear")  # Output layer with 'linear' activation for regression

model2 %>% compile(
  loss = "mean_squared_error",  # Use mean squared error for regression
  optimizer = optimizer_adam(),
  metrics = c("mean_absolute_error")  # Optional: you can monitor mean absolute error during training
)


#train the model
epochs <- 5
batch_size <- 32

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)


F48S48<-history2$metrics$val_mean_absolute_error





#FIRST LAYER =64
model2 <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu", input_shape = c(18)) %>%
  layer_dense(units = 48, activation = "relu")%>%
  layer_dense(units = 1, activation = "linear")  # Output layer with 'linear' activation for regression

model2 %>% compile(
  loss = "mean_squared_error",  # Use mean squared error for regression
  optimizer = optimizer_adam(),
  metrics = c("mean_absolute_error")  # Optional: you can monitor mean absolute error during training
)


#train the model
epochs <- 5
batch_size <- 32

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)

F64S48<-history2$metrics$val_mean_absolute_error






#INCREASE SECOND LAYER  TO 64
model2 <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape = c(18)) %>%
  layer_dense(units = 64, activation = "relu")%>%
  layer_dense(units = 1, activation = "linear")  # Output layer with 'linear' activation for regression

model2 %>% compile(
  loss = "mean_squared_error",  # Use mean squared error for regression
  optimizer = optimizer_adam(),
  metrics = c("mean_absolute_error")  # Optional: you can monitor mean absolute error during training
)


#train the model
epochs <- 5
batch_size <- 32

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)

F16S64<-history2$metrics$val_mean_absolute_error








#FIRST LAYER =32
model2 <- keras_model_sequential() %>%
  layer_dense(units = 32, activation = "relu", input_shape = c(18)) %>%
  layer_dense(units = 64, activation = "relu")%>%
  layer_dense(units = 1, activation = "linear")  # Output layer with 'linear' activation for regression

model2 %>% compile(
  loss = "mean_squared_error",  # Use mean squared error for regression
  optimizer = optimizer_adam(),
  metrics = c("mean_absolute_error")  # Optional: you can monitor mean absolute error during training
)


#train the model
epochs <- 5
batch_size <- 32

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)

F32S64<-history2$metrics$val_mean_absolute_error





#FIRST LAYER =48
model2 <- keras_model_sequential() %>%
  layer_dense(units = 48, activation = "relu", input_shape = c(18)) %>%
  layer_dense(units = 64, activation = "relu")%>%
  layer_dense(units = 1, activation = "linear")  # Output layer with 'linear' activation for regression

model2 %>% compile(
  loss = "mean_squared_error",  # Use mean squared error for regression
  optimizer = optimizer_adam(),
  metrics = c("mean_absolute_error")  # Optional: you can monitor mean absolute error during training
)


#train the model
epochs <- 5
batch_size <- 32

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)


F48S64<-history2$metrics$val_mean_absolute_error





#FIRST LAYER =64
model2 <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu", input_shape = c(18)) %>%
  layer_dense(units = 64, activation = "relu")%>%
  layer_dense(units = 1, activation = "linear")  # Output layer with 'linear' activation for regression

model2 %>% compile(
  loss = "mean_squared_error",  # Use mean squared error for regression
  optimizer = optimizer_adam(),
  metrics = c("mean_absolute_error")  # Optional: you can monitor mean absolute error during training
)


#train the model
epochs <- 5
batch_size <- 32

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)

F64S64<-history2$metrics$val_mean_absolute_error