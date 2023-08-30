library(tidyverse)
library(lubridate)


# Part 1: Get data loaded -------------------------------------------------


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



DSRoverall<-DSR
DSRoverall<-DSRoverall%>%
  na.omit()
DSRoverall<-DSRoverall%>%
  mutate(DSR=original_consumption-predictions)


#add in appliance data
survey<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/03 Data and methodology/Data/survey_answers.csv")




# Part 6: DSR analysis on HH member - low prices --------------------------------------------

#Keep relevant variables
HHmembers<-survey[c("Household_id","Q213")]
#rename variable
names(HHmembers)[names(HHmembers) == "Household_id"] <- "customer"
#keep only low price events
DSRoveralllow_summary<- DSRoverall %>%
  filter(price=="low")%>%
  group_by(customer)%>%
  summarize(mean_DSR = mean(DSR))

#add in DSR
HHmembers<-merge(HHmembers,DSRoveralllow_summary, by="customer")
#omimt na values
HHmembers<-HHmembers%>%
  na.omit()

#run regression
numberHHmember <- lm(mean_DSR ~ Q213, data=HHmembers)
summary(numberHHmember)

#now do it by grouping number of HH members
#create new occupancy variable
HHmembersdummy<-HHmembers
HHmembersdummy$single<-ifelse(HHmembersdummy$Q213==1,1,0)

#regressoin
numberHHmemberdummy <- lm(mean_DSR ~ single, data=HHmembersdummy)
summary(numberHHmemberdummy)

#create bar plot with confidence intervals

library(extrafont)
walbaum_font <- theme(text = element_text(family = "Times New Roman", size=14))


mean_data <- aggregate(mean_DSR ~ single, HHmembersdummy, mean)
se_data <- aggregate(mean_DSR ~ single, HHmembersdummy, function(x) sd(x) / sqrt(length(x)))

barplot_heights <- mean_data$mean_DSR
barplot_err <- se_data$mean_DSR

custom_colors <- c("#FA8072", "#FF3333")

combined_data <- data.frame(
  Group = factor(mean_data$single, levels = c(0, 1), 
                 labels = c("Multiple Occupancy", "Single Occupancy")),
  Mean = barplot_heights,
  SE = barplot_err
)

# Create the ggplot
ggplot(combined_data, aes(x = Group, y = Mean, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color="black") +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
                position = position_dodge(width = 0.5), width = 0.1, color="black") +
  labs(x = "Occupancy Status", y = "Mean DSR (kWh)") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal()+
  walbaum_font+
  theme(legend.position = "none")+
  ylim(0, 0.025)



# Part 7: DSR analysis on number of kids --------------------------------------------

kids<-survey[c("Household_id","Q222","Q223","Q224","Q225","Q226","Q227","Q228","Q229")]

names(kids)[names(kids) == "Household_id"] <- "customer"

kids$first <- ifelse(kids$Q222 == "0-4", "1",
                     ifelse(kids$Q222 == "5-11", "1",
                            ifelse(kids$Q222 == "12-15", "1",
                                   ifelse(kids$Q222 == "16-17", "1", "0"))))

kids$second <- ifelse(kids$Q223 == "0-4", "1",
                     ifelse(kids$Q223 == "5-11", "1",
                            ifelse(kids$Q223 == "12-15", "1",
                                   ifelse(kids$Q223 == "16-17", "1", "0"))))

kids$third <- ifelse(kids$Q224 == "0-4", "1",
                      ifelse(kids$Q224 == "5-11", "1",
                             ifelse(kids$Q224 == "12-15", "1",
                                    ifelse(kids$Q224 == "16-17", "1", "0"))))

kids$fourth <- ifelse(kids$Q225 == "0-4", "1",
                      ifelse(kids$Q225 == "5-11", "1",
                             ifelse(kids$Q225 == "12-15", "1",
                                    ifelse(kids$Q225 == "16-17", "1", "0"))))

kids$fifth <- ifelse(kids$Q226 == "0-4", "1",
                      ifelse(kids$Q226 == "5-11", "1",
                             ifelse(kids$Q226 == "12-15", "1",
                                    ifelse(kids$Q226 == "16-17", "1", "0"))))

kids$sixth <- ifelse(kids$Q227 == "0-4", "1",
                      ifelse(kids$Q227 == "5-11", "1",
                             ifelse(kids$Q227 == "12-15", "1",
                                    ifelse(kids$Q227 == "16-17", "1", "0"))))

kids$seventh <- ifelse(kids$Q228 == "0-4", "1",
                      ifelse(kids$Q228 == "5-11", "1",
                             ifelse(kids$Q228 == "12-15", "1",
                                    ifelse(kids$Q228 == "16-17", "1", "0"))))

kids$eighth <- ifelse(kids$Q229 == "0-4", "1",
                      ifelse(kids$Q229 == "5-11", "1",
                             ifelse(kids$Q229 == "12-15", "1",
                                    ifelse(kids$Q229 == "16-17", "1", "0"))))
kids[, 10:17] <- lapply(kids[, 10:17], as.numeric)
kids$kids <- rowSums(kids[, 10:17])


#keep only low price events

DSRoveralllow_summary<- DSRoverall %>%
  filter(price=="low")%>%
  group_by(customer)%>%
  summarize(mean_DSR = mean(DSR))



kids<-merge(kids,DSRoveralllow_summary, by="customer")
kids<-kids%>%
  na.omit()

kids$kids<-ifelse(kids$kids>=1,1,0)

numberkids <- lm(mean_DSR ~ kids, data=kids)
summary(numberkids)


#create bar plot with confidence intervals
"#FA8072"
"#FF3333"
"#CC0000"

library(extrafont)
walbaum_font <- theme(text = element_text(family = "Times New Roman", size=14))


mean_data <- aggregate(mean_DSR ~ kids, kids, mean)
se_data <- aggregate(mean_DSR ~ kids, kids, function(x) sd(x) / sqrt(length(x)))

barplot_heights <- mean_data$mean_DSR
barplot_err <- se_data$mean_DSR

custom_colors <- c("#FA8072", "#FF3333")

combined_data <- data.frame(
  Group = factor(mean_data$kids, levels = c(0, 1), 
                 labels = c("No children", "Has children")),
  Mean = barplot_heights,
  SE = barplot_err
)

# Create the ggplot
ggplot(combined_data, aes(x = Group, y = Mean, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color="black") +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
                position = position_dodge(width = 0.5), width = 0.1, color="black") +
  labs(x = "Child Status", y = "Mean DSR (kWh)") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal()+
  walbaum_font+
  ylim(0,0.03)+
  theme(legend.position = "none") 


# Part 8: DSR analysis on number of rooms --------------------------------------------

rooms<-survey[c("Household_id","Q238")]

names(rooms)[names(rooms) == "Household_id"] <- "customer"


#keep only low price events

DSRoveralllow_summary<- DSRoverall %>%
  filter(price=="low")%>%
  group_by(customer)%>%
  summarize(mean_DSR = mean(DSR))



rooms<-merge(rooms,DSRoveralllow_summary, by="customer")
rooms<-rooms%>%
  na.omit()

numberrooms <- lm(mean_DSR ~ Q238, data=rooms)
summary(numberrooms)

rooms2<-rooms
rooms2$small<-ifelse(rooms2$Q238<=3,1,0)
rooms2$medium <- ifelse(rooms2$Q238 > 3 & rooms2$Q238 <= 5, 1, 0)
rooms2$large<-ifelse(rooms2$Q238>5,1,0)
rooms2$size <- ifelse(rooms2$small == "1", "small",
                       ifelse(rooms2$medium == "1", "medium",
                                    ifelse(rooms2$large == "1", "large", "0")))

numberrooms2 <- lm(mean_DSR ~ size, data=rooms2)
summary(numberrooms2)


#create bar plot with confidence intervals
"#FA8072"
"#FF3333"
"#CC0000"

library(extrafont)
walbaum_font <- theme(text = element_text(family = "Times New Roman", size=14))


mean_data <- aggregate(mean_DSR ~ size, rooms2, mean)
se_data <- aggregate(mean_DSR ~ size, rooms2, function(x) sd(x) / sqrt(length(x)))

barplot_heights <- mean_data$mean_DSR
barplot_err <- se_data$mean_DSR

custom_colors <- c("#FA8072", "#FF3333","#CC0000")

combined_data <- data.frame(
  Group = mean_data$size,
  Mean = barplot_heights,
  SE = barplot_err
)

# Create the ggplot
ggplot(combined_data, aes(x = Group, y = Mean, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color="black") +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
                position = position_dodge(width = 0.5), width = 0.1, color="black") +
  labs(x = "Dwelling size", y = "Mean DSR (kWh)") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal()+
  walbaum_font+
  theme(legend.position = "none")+
  ylim(0,0.03)



