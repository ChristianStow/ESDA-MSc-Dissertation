library(tidyverse)
library(lubridate)

batch1<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/Neural Network Single Customer Model/test_predictions_150.csv")
batch2<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/Neural Network Single Customer Model/test_predictions_300.csv")
batch3<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/Neural Network Single Customer Model/test_predictions_400.csv")
batch4<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/Neural Network Single Customer Model/test_predictions_600.csv")
batch5<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/Neural Network Single Customer Model/test_predictions_847.csv")

test<-rbind(batch1, batch2, batch3, batch4, batch5)



test<-test%>%
  na.omit()
test<-test%>%
  mutate(DSR=original_consumption-predictions)

test<-test%>%
  mutate(percent=DSR/original_consumption)

RRMSE = sqrt(sum(test$DSR^2))/sqrt(sum(test$original_consumption^2))

RER=test$percent
RER=RER[is.finite(RER)]
meanRER <- mean(RER)