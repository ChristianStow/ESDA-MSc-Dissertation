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


#INCREASE FIRST LAYER FIRST - SECOND LAYER FIXED AT 8
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
batch_size <- 128

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)

F16S8<-history2$metrics$val_mean_absolute_error
F16S82 <- history2$metrics$val_loss








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
batch_size <- 128

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)

F32S8<-history2$metrics$val_mean_absolute_error
F32S82<-history2$metrics$val_loss




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
batch_size <- 128

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)


F48S8<-history2$metrics$val_mean_absolute_error
F48S82<-history2$metrics$val_loss




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
batch_size <- 128

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)

F64S8<-history2$metrics$val_mean_absolute_error
F64S82<-history2$metrics$val_loss








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
batch_size <- 128

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)

F16S16<-history2$metrics$val_mean_absolute_error
F16S162<-history2$metrics$val_loss








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
batch_size <- 128

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)

F32S16<-history2$metrics$val_mean_absolute_error
F32S162<-history2$metrics$val_loss





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
batch_size <- 128

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)


F48S16<-history2$metrics$val_mean_absolute_error
F48S162<-history2$metrics$val_loss





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
batch_size <- 128

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)

F64S16<-history2$metrics$val_mean_absolute_error
F64S162<-history2$metrics$val_loss






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
batch_size <- 128

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)

F16S32<-history2$metrics$val_mean_absolute_error
F16S322<-history2$metrics$val_loss








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
batch_size <- 128

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)

F32S32<-history2$metrics$val_mean_absolute_error
F32S322<-history2$metrics$val_loss





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
batch_size <- 128

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)


F48S32<-history2$metrics$val_mean_absolute_error
F48S322<-history2$metrics$val_loss





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
batch_size <- 128

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)

F64S32<-history2$metrics$val_mean_absolute_error
F64S322<-history2$metrics$val_loss






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
batch_size <- 128

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)

F16S48<-history2$metrics$val_mean_absolute_error
F16S482<-history2$metrics$val_loss








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
batch_size <- 128

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)

F32S48<-history2$metrics$val_mean_absolute_error
F32S482<-history2$metrics$val_loss





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
batch_size <- 128

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)


F48S48<-history2$metrics$val_mean_absolute_error
F48S482<-history2$metrics$val_loss





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
batch_size <- 128

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)

F64S48<-history2$metrics$val_mean_absolute_error
F64S482<-history2$metrics$val_loss






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
batch_size <- 128

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)

F16S64<-history2$metrics$val_mean_absolute_error
F16S642<-history2$metrics$val_loss








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
batch_size <- 128

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)

F32S64<-history2$metrics$val_mean_absolute_error
F32S642<-history2$metrics$val_loss





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
batch_size <- 128

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)


F48S64<-history2$metrics$val_mean_absolute_error
F48S642<-history2$metrics$val_loss





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
batch_size <- 128

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)

F64S64<-history2$metrics$val_mean_absolute_error
F64S642<-history2$metrics$val_loss








#Pick the best model
MAE<-rbind(F16S8, F16S16, F16S32, F16S64, F32S8, F32S16, F32S32, F32S48, F32S64, F48S8, F48S16, F48S32,
           F48S48, F48S64, F64S8, F64S16, F64S32, F64S48, F64S64)

#looks ike F64S64 is the best one in terms of validation MAE



# Evaluate the optimal model on the test set
#create test dataset
event_days<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Neural network analysis/Neural network analysis/Neural network data prep/event_days.csv")

test_days<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Neural network analysis/Neural network analysis/Neural network data prep/test_days.csv")

test_days<-test_days%>%
  mutate(Date=ymd(Date))

df_long3<-df_long3%>%
  mutate(Date = as.Date(GMT))

test<-df_long3%>%
  filter(Date %in% test_days$Date)



#Create training dataset
event_days<-event_days%>%
  mutate(Date=ymd(Date))

train<-anti_join(df_long3,test, by="Date")
train2<-anti_join(train, event_days, by="Date")





####Prepare data for the neural network
# Extract the input variables and target variable
test<-test[c(4:9, 11:21)]
train2<-train2[c(4:9, 11:21)]

x_test <- as.matrix(test)
x_train<-as.matrix(train2)
x_train <- scale(x_train)
x_test <- scale(x_test)



###
test<-df_long3%>%
  filter(Date %in% test_days$Date)

#Create training dataset
event_days<-event_days%>%
  mutate(Date=ymd(Date))

train<-anti_join(df_long3,test, by="Date")
train2<-anti_join(train, event_days, by="Date")

y_train <- train2$consumption
y_test <- test$consumption



#train model
model2 <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu", input_shape = c(17)) %>%
  layer_dense(units = 64, activation = "relu")%>%
  layer_dense(units = 1, activation = "linear")  # Output layer with 'linear' activation for regression

model2 %>% compile(
  loss = "mean_squared_error",  # Use mean squared error for regression
  optimizer = optimizer_adam(),
  metrics = c("mean_absolute_error")  # Optional: you can monitor mean absolute error during training
)


#train the model
epochs <- 10
batch_size <- 128

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)


#evaluate performance

evaluation <- model2 %>% evaluate(x_test, y_test)
mse <- evaluation$mean_squared_error
mae <- evaluation$mean_absolute_error


#Get predictions for the test data
predictions <- predict(model2, x_test)


# Compare predictions with the actual target values (y_test)
comparison <- data.frame(Actual = y_test, Predicted = predictions)

y_test_df<-as.data.frame(y_test)
predictions_df<-as.data.frame(predictions)

mse <- mean((y_test_df$y_test - predictions_df$V1)^2)

rmse <- sqrt(mse)

mean_observed <- mean(y_test_df$y_test)

rrmse <- rmse / mean_observed ##0.985


###final model performance
test<-comparison%>%
  na.omit()
test<-test%>%
  mutate(DSR=Actual-Predicted)

test<-test%>%
  mutate(percent=DSR/Actual)

RRMSE = sqrt(sum(test$DSR^2))/sqrt(sum(test$Actual^2))

RER=test$percent
RER=RER[is.finite(RER)]
meanRER <- mean(RER)

####################create line charts

test <- test %>%
  mutate(hour = lubridate::hour(Date),
         minute = lubridate::minute(Date))

test<-test%>%
  mutate(halfhour=ifelse(test$minute>1, (2*test$hour)+1, 2*test$hour))

test<-test%>%
  mutate(Date=as.Date(Date))

test2<-test%>%
  filter(Date=="2013-01-15"| Date=="2013-03-20"|Date=="2013-05-06"| Date=="2013-07-17"| Date=="2013-09-27"|Date=="2013-11-20")

test3 <- test2 %>%
  pivot_longer(cols=c('Predicted'),
               names_to='Consumption',
               values_to='kw')

test4<-test3%>%
  group_by(Consumption, Date, halfhour)%>% 
  summarise(mean_power = mean(kw))


test4 %>% 
  ggplot(aes(x = halfhour, 
             y = mean_power, 
             fill = Consumption,
             colour = Consumption)) + 
  geom_line() + 
  facet_wrap(~Date)

write.csv(test4, "C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/DR participation/NNchart.csv", row.names=FALSE)


write.csv(test, "C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Model performance/approach2test.csv", row.names=FALSE)

























#RER
abs_diff_observed_predicted <- abs(y_test_df$y_test - predictions_df$V1)
abs_diff_observed_mean <- abs(y_test_df$y_test - mean(y_test_df$y_test))

# Calculate Relative Error Ratio (RER)
rer <- abs_diff_observed_predicted / abs_diff_observed_mean
median(rer)


event_days<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Neural network analysis/Neural network analysis/Neural network data prep/event_days.csv")

test_days<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Neural network analysis/Neural network analysis/Neural network data prep/test_days.csv")

test_days<-test_days%>%
  mutate(Date=ymd(Date))

df_long3<-df_long3%>%
  mutate(Date = as.Date(GMT))

test<-df_long3%>%
  filter(Date %in% test_days$Date)

comparison$Date<-test$GMT
comparison$ID<-test$customer_id

comparison$pos10prct<-comparison$Actual*1.1
comparison$neg10prct<-comparison$Actual*0.9

comparison$inrange<-ifelse(comparison$Predicted<=comparison$pos10prct 
                           & comparison$Predicted>=comparison$neg10prct, 1,0)

table(comparison$inrange)

RER<-255344/1533520

