install.packages("tensorflow")
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


lcl<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Data processing and cleansing/Data exploration/FINALDATASET.csv")
lcl<-lcl%>%
  mutate(Date = as.Date(GMT))
lcl<-lcl[c(3,3203:4049)]


# Convert 'timestamp' column to a proper date-time format
lcl<-lcl%>%
  mutate(GMT=ymd_hms(GMT))

# Melt the dataframe to long format
df_long <- lcl %>%
  pivot_longer(cols = starts_with("D"),
               names_to = "customer_id",
               values_to = "consumption") %>%
  arrange(customer_id, GMT)

df_long2<-df_long
# Add columns for each day's consumption (5 days ago to 1 day ago)
for (i in 1:5) {
  df_long2 <- df_long2 %>%
    group_by(customer_id) %>%
    mutate(!!paste0("consumption_", i, "_days_ago") := lag(consumption, i * 48)) %>%
    ungroup()
}

# Drop rows with NA values (rows corresponding to the earliest dates in the dataset)
df_long2 <- df_long2[complete.cases(df_long2), ]



#Extract value from timestamp into new variables
df_long3<-df_long2%>%
  mutate(hour=hour(GMT))%>%
  mutate(year=year(GMT))%>%
  mutate(month=month(GMT))%>%
  mutate(day=day(GMT))%>%
  mutate(week=week(GMT))

df_long3<-df_long3%>%
  mutate(sinmonth = sin(2 * pi * month / 12),
         cosmonth = cos(2 * pi * month / 12),
         sinday = sin(2 * pi * day / 7),
         cosday = cos(2 * pi * day / 7),
         cosweek = cos(2* pi * week / 52),
         sinweek = sin(2 * pi * week / 52),
         sinhour = sin(2 * pi * hour / 24),
         coshour = cos(2 * pi * hour / 24))



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
x_train <- as.matrix(train2[c(4:21)])
y_train <- train2$consumption

x_test <- as.matrix(test[c(4:21)])
y_test <- test$consumption

# Normalize the input variables to improve model training
x_train <- scale(x_train)
x_test <- scale(x_test)







############BASIC MODEL -NO TUNING
#Build and compile the neural network model with the updated target variable
model2 <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu", input_shape = c(18)) %>%
  layer_dense(units = 64, activation = "relu")%>%
  layer_dense(units = 1, activation = "linear")  # Output layer with 'linear' activation for regression

model2 %>% compile(
  loss = "mean_squared_error",  # Use mean squared error for regression
  optimizer = optimizer_adam(),
  metrics = c("mean_absolute_error")  # Optional: you can monitor mean absolute error during training
)




model2 <- keras_model_sequential() %>%
  layer_dense(units = 48, activation = "tanh", input_shape = c(18)) %>%
  layer_dense(units = 1, activation = "linear")  # Output layer with 'linear' activation for regression

model2 %>% compile(
  loss = "mean_squared_error",  # Use mean squared error for regression
  optimizer = optimizer_rmsprop(learning_rate = 0.002),
  metrics = c("mean_absolute_error")  # Optional: you can monitor mean absolute error during training
)










model2 <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu", input_shape = c(18)) %>%
  layer_dense(units = 64, activation = "relu")%>%
  layer_dense(units = 1, activation = "linear")  # Output layer with 'linear' activation for regression

model2 %>% compile(
  loss = "mean_squared_error",  # Use mean squared error for regression
  optimizer = optimizer_rmsprop(learning_rate = 0.0025),
  metrics = c("mean_absolute_error")  # Optional: you can monitor mean absolute error during training
)




#train the model
epochs <- 7
batch_size <- 128

history2 <- model2 %>% fit(
  x_train, y_train,
  epochs = epochs,
  batch_size = batch_size,
  validation_split = 0.1  # Use 10% of the data for validation
)
















######Now time to tune the model

library(mlr3)
library(mlr3tuning)
remotes::install_github("mlr-org/mlr3keras")
library(mlr3keras)

model <- keras_model_sequential() %>%
  layer_dense(units = 8, activation = "relu", input_shape = c(18)) %>%
  layer_dense(units = 5, activation = "relu")%>%
  layer_dense(units = 1, activation = "linear")  # Output layer with 'linear' activation for regression

y_train_df <- data.frame(target = y_train)
task <- TaskRegr$new("my_task", backend = data.frame(x = x_train, y = y_train_df),target = "target")

hyperparam_space <- ParamSet$new(params=list(
  ParamDbl$new("units_layer1", lower = 4, upper = 64),
  ParamDbl$new("units_layer2", lower = 4, upper = 64)
))


learner <- lrn("regr.keras", model = model)

# Define the tuning instance for random search
tuning_instance <- TuningInstanceSingleCrit$new(
  task = task,
  learner = learner,
  resampling = rsmp("cv", folds = 3),  # Cross-validation with 3 folds
  measure = msr("regr.mse"),
  terminator = trm("evals", n_evals = 2),  # Number of evaluations/trials
  search_space = hyperparam_space
)


###not working
tuner <- tnr(tuner = "random_search", learner = learner, task = task, resampling = rsmp("cv", folds = 3), measure = msr("regr.mae"), terminator = trm("evals", n_evals = 2), search_space = hyperparam_space)

tuner$optimize(tuning_instance)

tuner <- tnr(tuner = tuning_instance)
###not working




#option 2
learner = lrn("regr.keras", model = model)

get_keras_model = function(arch = "arch1", lr = 3*10^-4) {
  if (arch == "arch1") {
    model = keras_model_sequential() %>%
      layer_dense(units = 16L, input_shape = 10L, activation = "relu") %>%
      layer_dense(units = 16L, activation = "relu") %>%
      layer_dense(units = 1L, activation = "linear")
  } else if (arch == "arch2") {
    model = keras_model_sequential() %>%
      layer_dense(units = 64L, input_shape = 10L, activation = "relu") %>%
      layer_dense(units = 32L, activation = "relu") %>%
      layer_dense(units = 1L, activation = "linear")
  }
  model %>%
    compile(optimizer = optimizer_adam(lr),
            loss = "binary_crossentropy",
            metrics = "accuracy")
}

hyperparam_space$trafo = function(x, param_set) {
  x$units_layer2  = x$units_layer1  = NULL # delete temp. params
  return(x)
}


#task = mlr_tasks$get("mtcars")
resampling = rsmp("holdout")
measure = msr("regr.mse")
tuner = tnr("grid_search")
terminator = trm("evals", n_evals = 4)
instance = TuningInstanceSingleCrit$new(
  task = task,
  learner = learner,
  resampling = resampling,
  measure = measure,
  search_space = hyperparam_space,
  terminator = terminator
)
tuner$optimize(tuning_instance)




































###############################################################kerastuneR OPTION-BACK UP
#install kerastuner
install.packages('kerastuneR')
kerastuneR::install_kerastuner()

#Test model using online
library(keras)
library(tensorflow)
library(kerastuneR)
library(dplyr)


build_model = function(hp) {
  
  model = keras_model_sequential()
  model %>% layer_dense(units=hp$Int('units',
                                     min_value=4,
                                     max_value=64,
                                     step=4),
                        input_shape = ncol(x_train),
                        activation='relu') %>% 
    layer_dense(units=1, activation='linear') %>% 
    compile(
      optimizer= tf$keras$optimizers$Adam(
        hp$Choice('learning_rate',
                  values=c(1e-2, 1e-3, 1e-4))),
      loss='binary_crossentropy',
      metrics='accuracy') 
  return(model)
}


tuner = RandomSearch(
  build_model,
  objective = 'val_accuracy',
  max_trials = 5,
  executions_per_trial = 3,
  directory = 'my_dir',
  project_name = 'helloworld')

tuner %>% search_summary()


tuner %>% fit_tuner(x_train,y_train,validation_split = 0.2, epochs=5)



























# Evaluate the model on the test set
evaluation <- model2 %>% evaluate(x_test, y_test)
mse <- evaluation$mean_squared_error
mae <- evaluation$mean_absolute_error


#Get predictions for the test data
predictions <- predict(model2, x_test)


# Compare predictions with the actual target values (y_test)
comparison <- data.frame(Actual = y_test, Predicted = predictions)






















