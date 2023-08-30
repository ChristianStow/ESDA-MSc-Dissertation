library(tidyverse)
library(lubridate)

# Part 1 load data --------------------------------------------------------


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


# Part 6: DSR analysis overall --------------------------------------------

DSRoverall<-DSR
DSRoverall<-DSRoverall%>%
  na.omit()
DSRoverall<-DSRoverall%>%
  mutate(DSR=original_consumption-predictions)


library(extrafont)
walbaum_font <- theme(text = element_text(family = "Times New Roman", size=14))
custom_colors <- c("#00bfff","#FA8072")


DSRoveralllow_summary<- DSRoverall %>%
  filter(price=="low")%>%
  group_by(customer)%>%
  summarize(mean_DSR = mean(DSR))
DSRoveralllow_summary$price<-"Low"
DSRoveralllow_summary<-DSRoveralllow_summary[c(2:3)]

DSRoverallhigh_summary<- DSRoverall %>%
  filter(price=="high")%>%
  group_by(customer)%>%
  summarize(mean_DSR = mean(DSR))
DSRoverallhigh_summary$price<-"High"
DSRoverallhigh_summary<-DSRoverallhigh_summary[c(2:3)]

DSRoverall_customer<-rbind(DSRoveralllow_summary,DSRoverallhigh_summary)


p <- ggplot(DSRoverall_customer, aes(factor(price), mean_DSR))
p + geom_violin(draw_quantiles = c(0.5), aes(fill=factor(price)))+
  scale_fill_manual(values = c("Low" = "#00bfff", "High" = "#FA8072")) +
  walbaum_font+
  labs(x = "Price Categories", y = "Mean Customer DSR", fill = "Price Level")+
  ylim(-0.3, 0.4)





# Part 7: DSR analysis by time of day -------------------------------------
library(extrafont)
walbaum_font <- theme(text = element_text(family = "Times New Roman", size=14))



DSRtimeofdaylow_summary<- DSRoverall %>%
  filter(price=="low")%>%
  group_by(halfhour)%>%
  summarize(mean_DSR = mean(DSR))
DSRtimeofdaylow_summary$price<-"Low"

DSRtimeofdayhigh_summary<- DSRoverall %>%
  filter(price=="high")%>%
  group_by(halfhour)%>%
  summarize(mean_DSR = mean(DSR))
DSRtimeofdayhigh_summary$price<-"High"

DSRtimeofday<-rbind(DSRtimeofdaylow_summary,DSRtimeofdayhigh_summary)


q <- ggplot(DSRtimeofday, aes(x = halfhour, y = mean_DSR, fill = price)) +
  geom_bar(stat = "identity", position = "stack", color="black") +
  labs(x = "Half hour", y = "Mean DSR", fill = "Price") +
  theme_minimal()+
  scale_fill_manual(values = c("Low" = "#00bfff", "High" = "#FA8072")) +
  walbaum_font+
  labs(x = "Half Hour", y = "Mean DSR", fill = "Price Level")+
  ylim(-0.04, 0.04)
  




# Part 8: Day of week -----------------------------------------------------
DSRoverall<-DSRoverall%>%
  mutate(weekday=weekdays(GMT))

DSRoverall$weekday <- factor(DSRoverall$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

DSRdayofweeklow_summary<- DSRoverall %>%
  filter(price=="low")%>%
  group_by(weekday)%>%
  summarize(mean_DSR = mean(DSR))
DSRdayofweeklow_summary$price<-"Low"

DSRdayofweekhigh_summary<- DSRoverall %>%
  filter(price=="high")%>%
  group_by(weekday)%>%
  summarize(mean_DSR = mean(DSR))
DSRdayofweekhigh_summary$price<-"High"

DSRdayofweek<-rbind(DSRdayofweeklow_summary,DSRdayofweekhigh_summary)

DSRdayofweek$weekday<- factor(DSRdayofweek$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

q <- ggplot(DSRdayofweek, aes(x = weekday, y = mean_DSR, fill = price)) +
  geom_bar(stat = "identity", position = "stack", color="black") +
  labs(x = "Half hour", y = "Mean DSR", fill = "Price") +
  theme_minimal()+
  scale_fill_manual(values = c("Low" = "#00bfff", "High" = "#FA8072")) +
  walbaum_font+
  labs(x = "Day of the week", y = "Mean DSR", fill = "Price Level")+
  ylim(-0.04, 0.04)






# Part 9: duration of event -----------------------------------------------

library(extrafont)
walbaum_font <- theme(text = element_text(family = "Times New Roman", size=14))
custom_colors <- c("#00bfff","#FA8072")

#####VIOLIN PLOTS
DSRdurationlow_summary<- DSRoverall %>%
  filter(price=="low")%>%
  group_by(customer,duration)%>%
  summarize(mean_DSR = mean(DSR))
DSRdurationlow_summary$price<-"Low"
DSRdurationlow_summary<-DSRdurationlow_summary[c(2:4)]


DSRdurationhigh_summary<- DSRoverall %>%
  filter(price=="high")%>%
  group_by(customer,duration)%>%
  summarize(mean_DSR = mean(DSR))
DSRdurationhigh_summary$price<-"High"
DSRdurationhigh_summary<-DSRdurationhigh_summary[c(2:4)]

DSRduration_customer<-rbind(DSRdurationlow_summary,DSRdurationhigh_summary)
DSRduration_customer$duration<- factor(DSRduration_customer$duration, levels = c("3", "6", "12", "24"))


p <- ggplot(DSRduration_customer, aes(factor(price), mean_DSR))
p + geom_violin(draw_quantiles = c(0.5), aes(fill=factor(price)))+
  scale_fill_manual(values = c("Low" = "#00bfff", "High" = "#FA8072")) +
  walbaum_font+
  labs(x = "Price Categories by Event Duration (Hours)", y = "Mean Customer DSR", fill = "Price Level")+
  ylim(-0.3, 0.4)+
  facet_wrap(vars(duration))



######REGULAR BAR CHARTS
DSRdurationlow_summary<- DSRoverall %>%
  filter(price=="low")%>%
  group_by(duration)%>%
  summarize(mean_DSR = mean(DSR))
DSRdurationlow_summary$price<-"Low"


DSRdurationhigh_summary<- DSRoverall %>%
  filter(price=="high")%>%
  group_by(duration)%>%
  summarize(mean_DSR = mean(DSR))
DSRdurationhigh_summary$price<-"High"

DSRduration_customer<-rbind(DSRdurationlow_summary,DSRdurationhigh_summary)
DSRduration_customer$duration<- factor(DSRduration_customer$duration, levels = c("3", "6", "12", "24"))


q <- ggplot(DSRduration_customer, aes(x = duration, y = mean_DSR, fill = price)) +
  geom_bar(stat = "identity", position = "stack", color="black") +
  labs(x = "Half hour", y = "Mean DSR", fill = "Price") +
  theme_minimal()+
  scale_fill_manual(values = c("Low" = "#00bfff", "High" = "#FA8072")) +
  walbaum_font+
  labs(x = "Duration of event", y = "Mean DSR", fill = "Price Level")+
  ylim(-0.04, 0.04)





# Part 10: DSR by season --------------------------------------------------
library(ggridges)

DSR <- DSR %>%
  mutate(month = lubridate::month(GMT)) %>%
  mutate(season = ifelse(month %in% c(12, 1, 2), "Winter",
                         ifelse(month %in% c(3, 4, 5), "Spring",
                                ifelse(month %in% c(6, 7, 8), "Summer",
                                       ifelse(month %in% c(9, 10, 11), "Autumn", NA)))))


DSRlow<-DSR%>%
  filter(price=="low")

DSRlow<-DSRlow%>%
  na.omit()
DSRlow<-DSRlow%>%
  mutate(DSR=original_consumption-predictions)



DSRlowseasonsummary<- DSRlow %>%
  group_by(season,halfhour)%>%
  summarize(mean_DSR = mean(DSR))

#doesn't work
ggplot(DSRoverall, aes(x=DSR, y=season, fill=DSR) ) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")
####doesn't look good



ggplot(DSRlow, aes(x=halfhour, y=DSR) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white")+
  facet_wrap(vars(season))



ggplot(DSRlowseasonsummary, aes(x = halfhour, y = mean_DSR)) +
  geom_bar(stat = "identity",fill = "#00bfff") +
  coord_polar(start = 0)+
  facet_wrap(vars(season))+
  walbaum_font+
  labs(x = "Halfhour", y = "Mean DSR")







DSRhigh<-DSR%>%
  filter(price=="high")

DSRhigh<-DSRhigh%>%
  na.omit()
DSRhigh<-DSRhigh%>%
  mutate(DSR=original_consumption-predictions)



DSRhighseasonsummary<- DSRhigh %>%
  group_by(season,halfhour)%>%
  summarize(mean_DSR = mean(DSR))


ggplot(DSRhighseasonsummary, aes(x = halfhour, y = mean_DSR)) +
  geom_bar(stat = "identity",fill = "#FA8072") +
  coord_polar(start = 0)+
  facet_wrap(vars(season))+
  walbaum_font+
  labs(x = "Halfhour", y = "Mean DSR")

