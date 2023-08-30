library(tidyverse)
library(lubridate)


# Part 1: Load data -------------------------------------------------------


batch1<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/Neural Network Single Customer Model/event_predictions_150.csv")
batch2<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/Neural Network Single Customer Model/event_predictions_300.csv")
batch3<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/Neural Network Single Customer Model/event_predictions_400.csv")
batch4<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/Neural Network Single Customer Model/event_predictions_600.csv")
batch5<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/Neural Network Single Customer Model/event_predictions_847.csv")

merged<-rbind(batch1, batch2, batch3, batch4, batch5)
merged<-merged%>%
  mutate(GMT=ymd_hms(GMT))

tariff<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/03 Data and methodology/Data/tariff_d.csv")
tariff<-tariff%>%
  mutate(GMT=ymd_hms(GMT))


DSR<-merge(merged, tariff, by="GMT")

#add in hour and month
DSR<-DSR%>%
  mutate(hour=hour(GMT))%>%
  mutate(day=day(GMT))%>%
  mutate(minute=minute(GMT))

#add in half hour
DSR<-DSR%>%
  mutate(halfhour=ifelse(DSR$minute>1, (2*DSR$hour)+1, 2*DSR$hour))

#add in new price event tag
DSR<-DSR%>%
  mutate(price=ifelse(grepl("L", DSR$Event_tags), "low",
                      ifelse(grepl("H", DSR$Event_tags), "high", NA)))

#add in new duration event tag
DSR<-DSR%>%
  mutate(duration=ifelse(grepl("3", DSR$Event_tags), "3",
                         ifelse(grepl("6", DSR$Event_tags), "6",
                                ifelse(grepl("9", DSR$Event_tags), "9",
                                       ifelse(grepl("12", DSR$Event_tags), "12",
                                              ifelse(grepl("24", DSR$Event_tags), "24",NA))))))


# Part 6: DSR analysis overall - t tests and percentage changes - low --------------------------------------------

DSRoverall<-DSR
DSRoverall<-DSRoverall%>%
  na.omit()
DSRoverall<-DSRoverall%>%
  mutate(DSR=original_consumption-predictions)

DSRoveralllow_summary<- DSRoverall %>%
  filter(price=="low")%>%
  group_by(customer)%>%
  summarize(mean_DSR = mean(DSR))

#T-test
t_test_result_low_overall <- t.test(DSRoveralllow_summary$mean_DSR, mu = 0)
print(t_test_result_low_overall)

#% difference
DSRoveralllow_summary<- DSRoverall %>%
  filter(price=="low")%>%
  group_by(customer)%>%
  summarize(mean_consumption = mean(predictions))

t_test_result_low_overall$estimate/mean(DSRoveralllow_summary$mean_consumption)


# Part 7: DSR analysis overall - t tests and percentage changes - high --------------------------------------------

DSRoverall<-DSR
DSRoverall<-DSRoverall%>%
  na.omit()
DSRoverall<-DSRoverall%>%
  mutate(DSR=original_consumption-predictions)

DSRoverallhigh_summary<- DSRoverall %>%
  filter(price=="high")%>%
  group_by(customer)%>%
  summarize(mean_DSR = mean(DSR))

#T-test
t_test_result_high_overall <- t.test(DSRoverallhigh_summary$mean_DSR, mu = 0)
print(t_test_result_high_overall)

#% difference
DSRoverallhigh_summary<- DSRoverall %>%
  filter(price=="high")%>%
  group_by(customer)%>%
  summarize(mean_consumption = mean(predictions))

t_test_result_high_overall$estimate/mean(DSRoverallhigh_summary$mean_consumption)



# Part 8: duration of event t test and percent changes - low --------------
#3 hours
DSRdurationlow_summary<- DSRoverall %>%
  filter(price=="low")%>%
  group_by(customer,duration)%>%
  summarize(mean_DSR = mean(DSR))

DSRdurationlow_summary_3hours<- DSRdurationlow_summary %>%
  filter(duration=="3")

#T-test
t_test_result_low_3hours <- t.test(DSRdurationlow_summary_3hours$mean_DSR, mu = 0)
print(t_test_result_low_3hours)

#% difference
DSRdurationlow_summary_3hours<- DSRoverall %>%
  filter(price=="low")%>%
  group_by(customer,duration)%>%
  summarize(mean_consumption = mean(predictions))

DSRdurationlow_summary_3hours<- DSRdurationlow_summary_3hours %>%
  filter(duration=="3")

t_test_result_low_3hours$estimate/mean(DSRdurationlow_summary_3hours$mean_consumption)


#24 hours
DSRdurationlow_summary<- DSRoverall %>%
  filter(price=="low")%>%
  group_by(customer,duration)%>%
  summarize(mean_DSR = mean(DSR))

DSRdurationlow_summary_24hours<- DSRdurationlow_summary %>%
  filter(duration=="24")

#T-test
t_test_result_low_24hours <- t.test(DSRdurationlow_summary_24hours$mean_DSR, mu = 0)
print(t_test_result_low_24hours)

#% difference
DSRdurationlow_summary_24hours<- DSRoverall %>%
  filter(price=="low")%>%
  group_by(customer,duration)%>%
  summarize(mean_consumption = mean(predictions))

DSRdurationlow_summary_24hours<- DSRdurationlow_summary_24hours %>%
  filter(duration=="24")

t_test_result_low_24hours$estimate/mean(DSRdurationlow_summary_24hours$mean_consumption)

# Part 9: duration of event t test and percent changes - high --------------
#3 hours
DSRdurationhigh_summary<- DSRoverall %>%
  filter(price=="high")%>%
  group_by(customer,duration)%>%
  summarize(mean_DSR = mean(DSR))

DSRdurationhigh_summary_3hours<- DSRdurationhigh_summary %>%
  filter(duration=="3")

#T-test
t_test_result_high_3hours <- t.test(DSRdurationhigh_summary_3hours$mean_DSR, mu = 0)
print(t_test_result_high_3hours)

#% difference
DSRdurationhigh_summary_3hours<- DSRoverall %>%
  filter(price=="high")%>%
  group_by(customer,duration)%>%
  summarize(mean_consumption = mean(predictions))

DSRdurationhigh_summary_3hours<- DSRdurationhigh_summary_3hours %>%
  filter(duration=="3")

t_test_result_high_3hours$estimate/mean(DSRdurationhigh_summary_3hours$mean_consumption)



#6 hours
DSRdurationhigh_summary<- DSRoverall %>%
  filter(price=="high")%>%
  group_by(customer,duration)%>%
  summarize(mean_DSR = mean(DSR))

DSRdurationhigh_summary_6hours<- DSRdurationhigh_summary %>%
  filter(duration=="6")

#T-test
t_test_result_high_6hours <- t.test(DSRdurationhigh_summary_6hours$mean_DSR, mu = 0)
print(t_test_result_high_6hours)

#% difference
DSRdurationhigh_summary_6hours<- DSRoverall %>%
  filter(price=="high")%>%
  group_by(customer,duration)%>%
  summarize(mean_consumption = mean(predictions))

DSRdurationhigh_summary_6hours<- DSRdurationhigh_summary_6hours %>%
  filter(duration=="6")

t_test_result_high_6hours$estimate/mean(DSRdurationhigh_summary_6hours$mean_consumption)





#12 hours
DSRdurationhigh_summary<- DSRoverall %>%
  filter(price=="high")%>%
  group_by(customer,duration)%>%
  summarize(mean_DSR = mean(DSR))

DSRdurationhigh_summary_12hours<- DSRdurationhigh_summary %>%
  filter(duration=="12")

#T-test
t_test_result_high_12hours <- t.test(DSRdurationhigh_summary_12hours$mean_DSR, mu = 0)
print(t_test_result_high_12hours)

#% difference
DSRdurationhigh_summary_12hours<- DSRoverall %>%
  filter(price=="high")%>%
  group_by(customer,duration)%>%
  summarize(mean_consumption = mean(predictions))

DSRdurationhigh_summary_12hours<- DSRdurationhigh_summary_12hours %>%
  filter(duration=="12")

t_test_result_high_12hours$estimate/mean(DSRdurationhigh_summary_12hours$mean_consumption)





# Part 10: Day of week t test and percent changes - low --------------------
##Sunday
DSRoverall<-DSRoverall%>%
  mutate(weekday=weekdays(GMT))

DSRoverall$weekday <- factor(DSRoverall$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

#Sunday
DSRdayofweeklow_summary<- DSRoverall %>%
  filter(price=="low")%>%
  group_by(customer, weekday)%>%
  summarize(mean_DSR = mean(DSR))

DSRdayofweeklow_summary<- DSRdayofweeklow_summary %>%
  filter(weekday=="Sunday")

#T-test
t_test_result_low_sunday <- t.test(DSRdayofweeklow_summary$mean_DSR, mu = 0)
print(t_test_result_low_sunday)


# Part 11: Day of week t test and percent changes - high --------------------
##Sunday
DSRoverall<-DSRoverall%>%
  mutate(weekday=weekdays(GMT))

DSRoverall$weekday <- factor(DSRoverall$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

#Sunday
DSRdayofweekhigh_summary<- DSRoverall %>%
  filter(price=="high")%>%
  group_by(customer, weekday)%>%
  summarize(mean_DSR = mean(DSR))

DSRdayofweekhigh_summary<- DSRdayofweekhigh_summary %>%
  filter(weekday=="Sunday")

#T-test
t_test_result_high_sunday <- t.test(DSRdayofweekhigh_summary$mean_DSR, mu = 0)
print(t_test_result_high_sunday)

##Tuesday
DSRoverall<-DSRoverall%>%
  mutate(weekday=weekdays(GMT))

DSRoverall$weekday <- factor(DSRoverall$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

#Tuesday
DSRdayofweekhigh_summary<- DSRoverall %>%
  filter(price=="high")%>%
  group_by(customer, weekday)%>%
  summarize(mean_DSR = mean(DSR))

DSRdayofweekhigh_summary<- DSRdayofweekhigh_summary %>%
  filter(weekday=="Tuesday")

#T-test
t_test_result_high_tuesday <- t.test(DSRdayofweekhigh_summary$mean_DSR, mu = 0)
print(t_test_result_high_tuesday)

# Part 12: Time of day t test and percent change - low --------------------
###0 hours
DSRtimeofdaylow_summary<- DSRoverall %>%
  filter(price=="low")%>%
  group_by(customer,halfhour)%>%
  summarize(mean_DSR = mean(DSR))

DSRtimeofdaylow_summary<-DSRtimeofdaylow_summary%>%
  filter(halfhour==47)

#T-test
t_test_result_low_timeofday <- t.test(DSRtimeofdaylow_summary$mean_DSR, mu = 0)
print(t_test_result_low_timeofday)

#% difference
DSRtimeofdaylow_summary<- DSRoverall %>%
  filter(price=="low")%>%
  group_by(customer, halfhour)%>%
  summarize(mean_consumption = mean(predictions))

DSRtimeofdaylow_summary<-DSRtimeofdaylow_summary%>%
  filter(halfhour==47)

t_test_result_low_timeofday$estimate/mean(DSRtimeofdaylow_summary$mean_consumption)