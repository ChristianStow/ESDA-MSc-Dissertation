library(tidyverse)
library(lubridate)
library(anytime)
library(rpart)
library(rsample)
library(dials)
library(tidymodels)
library(rpart.plot)
library(vip) 
library(dplyr)
library(Metrics)
library(mlr)
library(ggplot2)
library(plotly)
library(randomForest)
library(openair)
library(e1071)
library(corrplot)
library(relaimpo)
library(fpc)
library(glmnet)



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

# Part 4 Data exploration -------------------------------------------------



#Explore correlations of shortlisted variables

df_long4<-df_long3[c(3:21)]

mydata.cor = cor(df_long4, method = c("pearson"))
corrplot(mydata.cor, type='upper', method='ellipse', col = COL2('BrBG', 10),tl.col = 'black', tl.srt = 45, tl.cex=0.75)


#Use linear regression to identify variables with statistical significance

linear_model1 <- lm(consumption ~ ., df_long4)
summary(linear_model1)


