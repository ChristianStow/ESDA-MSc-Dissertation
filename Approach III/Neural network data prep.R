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
library(zoo)
library(ncdf4) 
library(chron)




##########Create dataset with five lagged periods and the time, minute, day and month variables########

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

df_long3$group <- ifelse(grepl("D", df_long3$customer_id), "treatment",
                                ifelse(grepl("N", df_long3$customer_id), "control", NA))
df_long3<-df_long3%>%
  filter(group=="treatment")


#######Clean environment
remove(df_long)
remove(df_long2)
remove(lat)
remove(lon)
remove(t2m_array)
remove(tday)
remove(time)
remove(time2)
remove(tmonth)
remove(tyear)
remove(lcl)
remove(t2m_df2)
remove(t2m_df3)
gc()


#############Load the mean control group consumption datasets and merge with the rest of the data - 2013
names(df_long3)[names(df_long3) == "customer_id"] <- "customer"

merged_2013<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/merged_2013.csv")

merged_2013<-merged_2013%>%
  filter(group=="treatment")

df_long4<-df_long3%>%
  filter(GMT >= "2013-01-01 00:00:00")


merged_2013$GMT<- as.POSIXct(merged_2013$GMT, format = "%Y-%m-%d %H:%M:%S")

overall_2013<-merge(df_long4, merged_2013, by=c("customer", "GMT"))

overall_2013<-overall_2013[c(1:20,23)]

write.csv(overall_2013,"C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/overall_2013.csv", row.names=FALSE)


#############Load the mean control group consumption datasets and merge with the rest of the data - 2012

merged_2012<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/merged_2012.csv")

merged_2012<-merged_2012%>%
  filter(group=="treatment")


df_long5<-df_long3%>%
  filter(GMT < "2013-01-01 00:00:00")


merged_2012$GMT<- as.POSIXct(merged_2012$GMT, format = "%Y-%m-%d %H:%M:%S")

overall_2012<-merge(df_long5, merged_2012, by=c("customer", "GMT"))

overall_2012<-overall_2012[c(1:20,23)]

write.csv(overall_2012,"C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/overall_2012.csv", row.names=FALSE)



# Part 2 ------------------------------------------------------------------

####Start new session and load data back in

overall_2013<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/overall_2013.csv")
overall_2012<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/overall_2012.csv")



#######Load temperature data################

##############2013 batch
temp <- nc_open("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Temperature/adaptor.mars.internal-1691690118.3221095-25199-10-83a2ee62-b885-4a0a-832c-59f52744e4c1.nc" )

lon <- ncvar_get(temp, "longitude")
lat <- ncvar_get(temp, "latitude")
time <- ncvar_get(temp, "time")
tunits <- ncatt_get(temp,"time","units")

tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tyear <- as.integer(unlist(tdstr)[1]) 
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])

time2<-chron(time/24, origin=c(tmonth, tday, tyear) )


t2m_array <- ncvar_get(temp,"t2m")

t2m_df2 <- data.frame( cbind( time,t2m_array  ))
t2m_df2<-t2m_df2%>%
  mutate(GMT=chron(time/24, origin=c(tmonth, tday, tyear) ))

t2m_df2$celcius<-t2m_df2$t2m_array-273.15

t2m_df2$GMT<-as.POSIXct(t2m_df2$GMT, format = "(%m/%d/%y %H:%M:%S)")
t2m_df2$GMT <- as.POSIXct(t2m_df2$GMT, format = "%Y-%m-%d %H:%M:%S")

t2m_df2_halfhourly <- t2m_df2 %>%
  complete(GMT = seq(min(GMT), max(GMT), by = "30 min")) %>%
  fill(celcius)

##############2012 batch
temp <- nc_open("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Temperature/adaptor.mars.internal-1691691136.3910246-21287-2-f499c740-91b2-44d4-b108-11a3b1c62ac2.nc" )

lon <- ncvar_get(temp, "longitude")
lat <- ncvar_get(temp, "latitude")
time <- ncvar_get(temp, "time")
tunits <- ncatt_get(temp,"time","units")

tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tyear <- as.integer(unlist(tdstr)[1]) 
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])

time2<-chron(time/24, origin=c(tmonth, tday, tyear) )


t2m_array <- ncvar_get(temp,"t2m")

t2m_df3 <- data.frame( cbind( time,t2m_array  ))
t2m_df3<-t2m_df3%>%
  mutate(GMT=chron(time/24, origin=c(tmonth, tday, tyear) ))

t2m_df3$celcius<-t2m_df3$t2m_array-273.15

t2m_df3$GMT<-as.POSIXct(t2m_df3$GMT, format = "(%m/%d/%y %H:%M:%S)")
t2m_df3$GMT <- as.POSIXct(t2m_df3$GMT, format = "%Y-%m-%d %H:%M:%S")

t2m_df3_halfhourly <- t2m_df3 %>%
  complete(GMT = seq(min(GMT), max(GMT), by = "30 min")) %>%
  fill(celcius)

###merge temperature with main datasets
overall_2013$GMT <- as.POSIXct(overall_2013$GMT, format = "%Y-%m-%d %H:%M:%S")
overall_2013_temp<-merge(overall_2013,t2m_df2_halfhourly, by="GMT", all = TRUE)
overall_2013_temp<-overall_2013_temp[c(1:21,24)]

overall_2012$GMT <- as.POSIXct(overall_2012$GMT, format = "%Y-%m-%d %H:%M:%S")
overall_2012_temp<-merge(overall_2012,t2m_df3_halfhourly, by="GMT", all = TRUE)
overall_2012_temp<-overall_2012_temp[c(1:21,24)]


overall_2012_temp<-overall_2012_temp%>%
  na.omit()


##############Bind them both together

overall_2012_2013<-rbind(overall_2012_temp,overall_2013_temp)

write.csv(overall_2012_2013,"C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/overall_2012_2013.csv", row.names=FALSE)



################################################CREATE YOUR DATASETS#######################
final<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/overall_2012_2013.csv")
final$GMT <- as.POSIXct(final$GMT, format = "%Y-%m-%d %H:%M:%S")

############Create event dataset
tariff<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/03 Data and methodology/Data/tariff_d.csv")

tariff<-tariff%>%
  mutate(GMT=ymd_hms(GMT))

tariff <- tariff %>%
  filter(Event_tags != "")

tariff <- tariff %>%
  filter(Event_tags != "CM")

event<-overall_2012_2013%>%
  filter(GMT %in% tariff$GMT)

write.csv(event,"C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/event.csv", row.names=FALSE)



############Create test dataset
#create test dataset
test_days<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Neural network analysis/Neural network analysis/Neural network data prep/test_days.csv")

test_days<-test_days%>%
  mutate(Date = as.Date(Date))

overall_2012_2013<-overall_2012_2013%>%
  mutate(Date = as.Date(GMT))

test<-overall_2012_2013%>%
  filter(Date %in% test_days$Date)

write.csv(test,"C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/test.csv", row.names=FALSE)


############Create training dataset

training<-anti_join(overall_2012_2013, test, by = "GMT")
training<-anti_join(training, event, by = "GMT")

write.csv(training,"C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/training.csv", row.names=FALSE)


