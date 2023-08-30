#install packages
library(tidyverse)
library(lubridate)
library(maditr)



# SECTION 1 DATA EXPLORATION OVER WHOLE PERIOD AND REMOVE MISSING  --------

# Par 1: Control group processing ------------------------------------------------
#Load dataset
LCL_c<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/03 Data and methodology/Data/consumption_n.csv")

#Convert GMT to date/time
LCL_c<-LCL_c%>%
  mutate(GMT=ymd_hms(GMT))

#Filter to get trial year data
LCL_c_trainingperiod<-LCL_c%>%
  filter(GMT < "2014-01-01 00:00:00",
         GMT >= "2012-07-01 00:00:00")

#Export trial dataset
write.csv(LCL_c_trainingperiod, "C:/Users/chris/OneDrive/Documents/University/Dissertation/03 Data and methodology/Data/LCL_c_trainingperiod.csv", row.names=FALSE)

#clean environment to free space





# Part 2: Treatment group processing ------------------------------------------------
#Load dataset
LCL_d<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/03 Data and methodology/Data/consumption_d.csv")

#Convert GMT to date/time
LCL_d<-LCL_d%>%
  mutate(GMT=ymd_hms(GMT))

#Filter to get trial year data
LCL_d_trainingperiod<-LCL_d%>%
  filter(GMT < "2014-01-01 00:00:00",
         GMT >= "2012-07-01 00:00:00")

#Export trial dataset
write.csv(LCL_d_trainingperiod, "C:/Users/chris/OneDrive/Documents/University/Dissertation/03 Data and methodology/Data/LCL_d_trainingperiod.csv", row.names=FALSE)

#clean environment to free space





# Part 3: Combine treatment and control trial data ------------------------------------------------
LCL_c_trainingperiod<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/03 Data and methodology/Data/LCL_c_trainingperiod.csv")
LCL_d_trainingperiod<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/03 Data and methodology/Data/LCL_d_trainingperiod.csv")

#Join datasets
LCL_all<-left_join(LCL_c_trainingperiod, LCL_d_trainingperiod, by=c("GMT"))



# Part 4: Generate descriptive statistics on missing data during training period ----------------------------------------------------------------
count_na_func <- function(x) sum(is.na(x)) 

LCL_all2<-as.data.frame(t(LCL_all))

LCL_all3 <-LCL_all2%>%
  mutate(missing = apply(.[1:26354], 1, count_na_func))


#Create historgram and descriptive statistics of missing data pct by half hour
hist(LCL_all3$missing)

ggplot(LCL_all3) +
  aes(x = missing) +
  geom_histogram()



mean(LCL_all3$missing)
var(LCL_all3$missing)
sd(LCL_all3$missing)

#Generate frequencies of households (by group) missing data by % of missingness
LCL_all3$missingpct<-(LCL_all3$missing/26354)*100


library(extrafont)

walbaum_font <- theme(text = element_text(family = "Times New Roman", size=14))
#############################################################################CHART FOR THE REPORT######################
plot1<-ggplot(LCL_all3) +
  aes(x = missingpct) +
  geom_histogram(fill = "#FA8072", color = "black") +  
  xlab("Percentage of Missing Values") +                         
  ylab("Number of Households") +                         
  coord_cartesian(xlim = c(0, 75))+
  walbaum_font
#############################################################################CHART FOR THE REPORT######################



plot(ecdf(LCL_all3[,"missingpct"]))

ggplot(LCL_all3, aes(missingpct)) +
  stat_ecdf(geom="step")


ggplot(LCL_all3, aes(missingpct)) +                     
  geom_histogram(aes(y = cumsum(..count..)))

#############################################################################CHART FOR THE REPORT######################
plot2<-ggplot(LCL_all3, aes(missingpct)) +                     
  geom_histogram(aes(y = cumsum(..count..)), fill = "#FA8072") +
  xlab("Percentage of Missing Values") +  # Change x-axis title
  ylab("Cumulative Number of Households") +  # Change y-axis title
  scale_x_continuous(limits = c(0, 75)) +  # Set x-axis limits
  walbaum_font 
#############################################################################CHART FOR THE REPORT######################
library(gridExtra)
grid.arrange(plot1, plot2, ncol = 2)
#############################################################################CHART FOR THE REPORT######################





mean(LCL_all3$missingpct)
var(LCL_all3$missingpct)
sd(LCL_all3$missingpct)

LCL_all4<-LCL_all3%>%
  filter(missingpct<5)

write.csv(LCL_all4, "C:/Users/chris/OneDrive/Documents/University/Dissertation/03 Data and methodology/Data/LCLwholeperiod.csv", row.names=FALSE)

LCL_all4<-as.data.frame(t(LCL_all4))






# SECTION 2 DATA EXPLORATION OVER TREATMENT YEAR AND REMOVE MISSING -------


# Par 1: Control group processing ------------------------------------------------
#Load dataset
LCL_c<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/03 Data and methodology/Data/consumption_n.csv")

#Convert GMT to date/time
LCL_c<-LCL_c%>%
  mutate(GMT=ymd_hms(GMT))

#Filter to get trial year data
LCL_c_trial<-LCL_c%>%
  filter(GMT < "2014-01-01 00:00:00",
         GMT >= "2013-01-01 00:00:00")

#Export trial dataset
write.csv(LCL_c_trial, "C:/Users/chris/OneDrive/Documents/University/Dissertation/03 Data and methodology/Data/LCL_c_trial.csv", row.names=FALSE)

#clean environment to free space





# Part 2: Treatment group processing ------------------------------------------------
#Load dataset
LCL_d<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/03 Data and methodology/Data/consumption_d.csv")

#Convert GMT to date/time
LCL_d<-LCL_d%>%
  mutate(GMT=ymd_hms(GMT))

#Filter to get trial year data
LCL_d_trial<-LCL_d%>%
  filter(GMT < "2014-01-01 00:00:00",
         GMT >= "2013-01-01 00:00:00")

#Export trial dataset
write.csv(LCL_d_trial, "C:/Users/chris/OneDrive/Documents/University/Dissertation/03 Data and methodology/Data/LCL_d_trial.csv", row.names=FALSE)

#clean environment to free space





# Part 3: Combine treatment and control trial data ------------------------------------------------
LCL_c_trial<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/03 Data and methodology/Data/LCL_c_trial.csv")
LCL_d_trial<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/03 Data and methodology/Data/LCL_d_trial.csv")

#Join datasets
LCL_trial<-left_join(LCL_c_trial, LCL_d_trial, by=c("GMT"))


#Part 4: Remove households with more than 50 missing observations
#LCL_all4<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/03 Data and methodology/Data/LCLwholeperiod.csv")

#LCL_all4<-as.data.frame(t(LCL_all4))


LCL_trial3<-as.data.frame(t(LCL_trial))

count_na_func <- function(x) sum(is.na(x)) 

LCL_trial3<-LCL_trial3 %>%
  mutate(missing = apply(.[1:17520], 1, count_na_func))

LCL_trial4<-LCL_trial3 %>%
  filter(missing>100)

LCL_trial4<-as.data.frame(t(LCL_trial4))


drop <- colnames(LCL_trial4) # I think this bit needs to go up at the front and then everything else should run smoothly. 
LCL_all5 = LCL_all4[,!(names(LCL_all4) %in% drop)]

write.csv(LCL_all5, "C:/Users/chris/OneDrive/Documents/University/Dissertation/03 Data and methodology/Data/LCLwholeperiod.csv", row.names=TRUE)


# Part 4: Examine missing data --------------------------------------------

LCL_trial2<-LCL_trial

drop <- colnames(LCL_all5) # I think this bit needs to go up at the front and then everything else should run smoothly. 
LCL_trial2 = LCL_trial[,(names(LCL_trial) %in% drop)]




LCL_trial2 <- LCL_trial2 %>%
  mutate(month = lubridate::month(GMT),
         hour = lubridate::hour(GMT),
         day = lubridate::day(GMT),
         minute = lubridate::minute(GMT))

LCL_trial2<-LCL_trial2%>%
  mutate(halfhour=ifelse(LCL_trial2$minute>1, (2*LCL_trial2$hour)+1, 2*LCL_trial2$hour))


LCL_trial2<-LCL_trial2 %>%
  mutate(missing = apply(.[2:4054], 1, count_na_func))



LCL_trial2<-LCL_trial2%>%
  mutate(month=as.factor(month),
         halfhour=as.factor(halfhour))

missingdatatable<-as.data.frame(dcast(LCL_trial2, halfhour ~ month, value.var="missing", fun.aggregate=sum))

longer <- missingdatatable %>% 
  pivot_longer(2:13,
               names_to = 'month',
               values_to = 'missing') 

longer<-longer%>%
  mutate(month=as.numeric(month))

##longer<-longer%>%
##  mutate(month=month.name[month])


##longer<-longer%>%
##  mutate(month=as.factor(month))


ggplot(longer, aes(x = halfhour, y = month, fill = missing)) +
  geom_tile() +
  labs(x = "Hour",
       y = "Month")

#########################################################################################CHART FOR THE REPORT ###########################################
custom_palette <- colorRampPalette(c("#FAB7A0", "#FA8072", "#E9967A"))

ggplot(longer, aes(x = halfhour, y = month, fill = missing)) +
  geom_tile() +
  scale_fill_gradientn(colors = custom_palette(100)) +  # Use the custom color palette with 100 colors
  labs(x = "Hour",
       y = "Month",
       fill = "Count") +  # Change the legend title
  scale_y_continuous(breaks = seq(1, 12, 1)) +  # Set y-axis scale from 1 to 12 in intervals of 1
  walbaum_font
#########################################################################################CHART FOR THE REPORT ###########################################




#Create historgram and descriptive statistics of missing data pct by half hour
hist(LCL_trial2$missing)

ggplot(LCL_trial2) +
  aes(x = missing) +
  geom_histogram()


mean(LCL_trial2$missing)
var(LCL_trial2$missing)
sd(LCL_trial2$missing)






# SECTION 3: DATA IMPUTATION ----------------------------------------------

LCL_all6<-LCL_all5

#LCL_all6<-LCL_all6[c(1,9,16,17)]
#Get day of the week in number format
#Get hour in number format
#Get minute in number format
#Create time index
LCL_all6<-LCL_all6%>%
  mutate(GMT=ymd_hms(GMT))


LCL_all6<-LCL_all6%>%
  mutate(timestamp=as.POSIXct(GMT))%>%
  mutate(weekday=wday(timestamp, label = TRUE, week_start = 1))%>%
  mutate(weekday=as.numeric(weekday))


LCL_all6 <- LCL_all6 %>%
  mutate(hour = lubridate::hour(GMT),
         day = lubridate::day(GMT),
         minute = lubridate::minute(GMT))

LCL_all6 <- cbind(ID = 1:nrow(LCL_all6), LCL_all6) 

#Create WN
LCL_all6<-LCL_all6%>%
  mutate(WN=weekday+(hour/24)+(minute/(24*60)))



LCL_all6 <- LCL_all6 %>%
  mutate_if(is.character, as.numeric)

#Create for loop
household_columns <- colnames(LCL_all6[3:4049])


for (col in household_columns) {
  household_data <- LCL_all6[, c(col, "WN", "ID")]
  missing_obs <- subset(household_data, is.na(household_data[[col]]))
  
  for (i in 1:nrow(missing_obs)) {
    wn_value <- missing_obs[i, "WN"]
    id_value <- missing_obs[i, "ID"]
    
    non_missing_obs <- subset(household_data,
                              !is.na(household_data[[col]]) &
                                (WN >= wn_value - 0.04236111) &
                                (WN <= wn_value + 0.04236111) &
                                (ID >= id_value - 384) &
                                (ID <= id_value + 384))
    
    if (nrow(non_missing_obs) > 0) {
      average_value <- mean(non_missing_obs[[col]])
      missing_obs[i, col] <- average_value
    }
  }
  
  LCL_all6[which(is.na(LCL_all6[[col]])), col] <- missing_obs[[col]]
}


LCL_all8<-LCL_all6%>%
  filter(GMT >= "2012-07-20 00:00:00")

LCL_all7 <- LCL_all8 %>% select_if(~!anyNA(.))



write.csv(LCL_all7, "C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Data processing and cleansing/Data exploration/FINALDATASET.csv", row.names=TRUE)


















