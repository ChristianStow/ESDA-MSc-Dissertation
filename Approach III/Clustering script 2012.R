#install packages
library(tidyverse)
library(lubridate)
library(maditr)
library(forcats)
library(dbscan)
library(factoextra)
library(broom)
library(ehaGoF)


# Part 1 :PREP THE DATA ---------------------------------------------------

#Load the main data
lcl<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Data processing and cleansing/Data exploration/FINALDATASET.csv")
lcl<-lcl%>%
  mutate(Date = as.Date(GMT))






# Part 2: JULY 2012 ----------------------------------------------------
###########################################################################July 2012

###################cut data to keep only July observations
lcljuly<-lcl%>%
  filter(GMT >= "2012-07-01 00:00:00",
         GMT <= "2012-07-31 23:30:00")

#add a halfhour variable and month variable
july<-lcljuly%>%
  mutate(halfhour=ifelse(lcljuly$minute>1, (2*lcljuly$hour)+1, 2*lcljuly$hour))%>%
  mutate(month = lubridate::month(GMT))

#keep only relevant columns, i.e., household observations and halfhour and month variables
july<-july[c(2:4049, 4057:4058)]


########create typical load profiles by averaging loads for each customer
july_transform <- july %>% 
  pivot_longer(-c(GMT, ID, halfhour,month),
               names_to = 'customer',
               values_to = 'kW') %>% 
  group_by(customer, halfhour) %>%
  summarise(mean_kW = mean(kW)) %>%
  pivot_wider(names_from = halfhour, 
              values_from = mean_kW) %>%
  ungroup()


#########################Cluster analysis
set.seed(123)

k<-6

### Run the cluster analysis
julyclusters <- july_transform %>%
  select(-customer)%>%
  kmeans(k)


july_transform2 <- july_transform %>%
  mutate(cluster = as.factor(julyclusters$cluster))


##################################CREATE AVERAGES OF CONTROL GROUP CLUSTERS PER MONTH#############
july2<-lcljuly%>%
  mutate(halfhour=ifelse(lcljuly$minute>1, (2*lcljuly$hour)+1, 2*lcljuly$hour))%>%
  mutate(month = lubridate::month(GMT))

july2<-july2[c(2:4049, 4057)]

july_longer<-july2%>%
  pivot_longer(-c(GMT, ID, halfhour),
               names_to = 'customer',
               values_to = 'kW')

july_longer<-merge(july_longer,july_transform2, by="customer")
july_longer<-july_longer[c(1,3,4,5,54)]

july_longer$group <- ifelse(grepl("D", july_longer$customer), "treatment",
                            ifelse(grepl("N", july_longer$customer), "control", NA))
july_longer2<-july_longer%>%
  filter(group=="control")

july_longer2 <- july_longer2 %>% 
  group_by(cluster, GMT) %>%
  summarise(mean_kW = mean(kW)) %>%
  ungroup()

july_longer3<-merge(july_longer2,july_longer,by=c("GMT", "cluster"))


# Part 2: AUGUST 2012 ----------------------------------------------------
###########################################################################August 2012

###################cut data to keep only August observations
lclaugust<-lcl%>%
  filter(GMT >= "2012-08-01 00:00:00",
         GMT <= "2012-08-31 23:30:00")

#add a halfhour variable and month variable
august<-lclaugust%>%
  mutate(halfhour=ifelse(lclaugust$minute>1, (2*lclaugust$hour)+1, 2*lclaugust$hour))%>%
  mutate(month = lubridate::month(GMT))

#keep only relevant columns, i.e., household observations and halfhour and month variables
august<-august[c(2:4049, 4057:4058)]


########create typical load profiles by averaging loads for each customer
august_transform <- august %>% 
  pivot_longer(-c(GMT, ID, halfhour,month),
               names_to = 'customer',
               values_to = 'kW') %>% 
  group_by(customer, halfhour) %>%
  summarise(mean_kW = mean(kW)) %>%
  pivot_wider(names_from = halfhour, 
              values_from = mean_kW) %>%
  ungroup()


#########################Cluster analysis

set.seed(123)

k<-6

### Run the cluster analysis
augustclusters <- august_transform %>%
  select(-customer)%>%
  kmeans(k)


august_transform2 <- august_transform %>%
  mutate(cluster = as.factor(augustclusters$cluster))


##################################CREATE AVERAGES OF CONTROL GROUP CLUSTERS PER MONTH#############
august2<-lclaugust%>%
  mutate(halfhour=ifelse(lclaugust$minute>1, (2*lclaugust$hour)+1, 2*lclaugust$hour))%>%
  mutate(month = lubridate::month(GMT))

august2<-august2[c(2:4049, 4057)]

august_longer<-august2%>%
  pivot_longer(-c(GMT, ID, halfhour),
               names_to = 'customer',
               values_to = 'kW')

august_longer<-merge(august_longer,august_transform2, by="customer")
august_longer<-august_longer[c(1,3,4,5,54)]

august_longer$group <- ifelse(grepl("D", august_longer$customer), "treatment",
                              ifelse(grepl("N", august_longer$customer), "control", NA))
august_longer2<-august_longer%>%
  filter(group=="control")

august_longer2 <- august_longer2 %>% 
  group_by(cluster, GMT) %>%
  summarise(mean_kW = mean(kW)) %>%
  ungroup()

august_longer3<-merge(august_longer2,august_longer,by=c("GMT", "cluster"))


# Part 2: SEPTEMBER 2012 ----------------------------------------------------
###########################################################################September 2012

###################cut data to keep only September observations
lclseptember<-lcl%>%
  filter(GMT >= "2012-09-01 00:00:00",
         GMT <= "2012-09-30 23:30:00")

#add a halfhour variable and month variable
september<-lclseptember%>%
  mutate(halfhour=ifelse(lclseptember$minute>1, (2*lclseptember$hour)+1, 2*lclseptember$hour))%>%
  mutate(month = lubridate::month(GMT))

#keep only relevant columns, i.e., household observations and halfhour and month variables
september<-september[c(2:4049, 4057:4058)]


########create typical load profiles by averaging loads for each customer
september_transform <- september %>% 
  pivot_longer(-c(GMT, ID, halfhour,month),
               names_to = 'customer',
               values_to = 'kW') %>% 
  group_by(customer, halfhour) %>%
  summarise(mean_kW = mean(kW)) %>%
  pivot_wider(names_from = halfhour, 
              values_from = mean_kW) %>%
  ungroup()


#########################Cluster analysis

set.seed(123)

k<-7

### Run the cluster analysis
septemberclusters <- september_transform %>%
  select(-customer)%>%
  kmeans(k)


september_transform2 <- september_transform %>%
  mutate(cluster = as.factor(septemberclusters$cluster))



##################################CREATE AVERAGES OF CONTROL GROUP CLUSTERS PER MONTH#############
september2<-lclseptember%>%
  mutate(halfhour=ifelse(lclseptember$minute>1, (2*lclseptember$hour)+1, 2*lclseptember$hour))%>%
  mutate(month = lubridate::month(GMT))

september2<-september2[c(2:4049, 4057)]

september_longer<-september2%>%
  pivot_longer(-c(GMT, ID, halfhour),
               names_to = 'customer',
               values_to = 'kW')

september_longer<-merge(september_longer,september_transform2, by="customer")
september_longer<-september_longer[c(1,3,4,5,54)]

september_longer$group <- ifelse(grepl("D", september_longer$customer), "treatment",
                                 ifelse(grepl("N", september_longer$customer), "control", NA))
september_longer2<-september_longer%>%
  filter(group=="control")

september_longer2 <- september_longer2 %>% 
  group_by(cluster, GMT) %>%
  summarise(mean_kW = mean(kW)) %>%
  ungroup()

september_longer3<-merge(september_longer2,september_longer,by=c("GMT", "cluster"))


# Part 2: OCTOBER 2012 ----------------------------------------------------
###########################################################################October 2012

###################cut data to keep only October observations
lcloctober<-lcl%>%
  filter(GMT >= "2012-10-01 00:00:00",
         GMT <= "2012-10-31 23:30:00")

#add a halfhour variable and month variable
october<-lcloctober%>%
  mutate(halfhour=ifelse(lcloctober$minute>1, (2*lcloctober$hour)+1, 2*lcloctober$hour))%>%
  mutate(month = lubridate::month(GMT))

#keep only relevant columns, i.e., household observations and halfhour and month variables
october<-october[c(2:4049, 4057:4058)]


########create typical load profiles by averaging loads for each customer
october_transform <- october %>% 
  pivot_longer(-c(GMT, ID, halfhour,month),
               names_to = 'customer',
               values_to = 'kW') %>% 
  group_by(customer, halfhour) %>%
  summarise(mean_kW = mean(kW)) %>%
  pivot_wider(names_from = halfhour, 
              values_from = mean_kW) %>%
  ungroup()


#########################Cluster analysis

set.seed(123)

k<-6

### Run the cluster analysis
octoberclusters <- october_transform %>%
  select(-customer)%>%
  kmeans(k)


october_transform2 <- october_transform %>%
  mutate(cluster = as.factor(octoberclusters$cluster))


##################################CREATE AVERAGES OF CONTROL GROUP CLUSTERS PER MONTH#############
october2<-lcloctober%>%
  mutate(halfhour=ifelse(lcloctober$minute>1, (2*lcloctober$hour)+1, 2*lcloctober$hour))%>%
  mutate(month = lubridate::month(GMT))

october2<-october2[c(2:4049, 4057)]

october_longer<-october2%>%
  pivot_longer(-c(GMT, ID, halfhour),
               names_to = 'customer',
               values_to = 'kW')

october_longer<-merge(october_longer,october_transform2, by="customer")
october_longer<-october_longer[c(1,3,4,5,54)]

october_longer$group <- ifelse(grepl("D", october_longer$customer), "treatment",
                               ifelse(grepl("N", october_longer$customer), "control", NA))
october_longer2<-october_longer%>%
  filter(group=="control")

october_longer2 <- october_longer2 %>% 
  group_by(cluster, GMT) %>%
  summarise(mean_kW = mean(kW)) %>%
  ungroup()

october_longer3<-merge(october_longer2,october_longer,by=c("GMT", "cluster"))


# Part 2: NOVEMBER 2012 ----------------------------------------------------
###########################################################################November 2012

###################cut data to keep only November observations
lclnovember<-lcl%>%
  filter(GMT >= "2012-11-01 00:00:00",
         GMT <= "2012-11-30 23:30:00")

#add a halfhour variable and month variable
november<-lclnovember%>%
  mutate(halfhour=ifelse(lclnovember$minute>1, (2*lclnovember$hour)+1, 2*lclnovember$hour))%>%
  mutate(month = lubridate::month(GMT))

#keep only relevant columns, i.e., household observations and halfhour and month variables
november<-november[c(2:4049, 4057:4058)]


########create typical load profiles by averaging loads for each customer
november_transform <- november %>% 
  pivot_longer(-c(GMT, ID, halfhour,month),
               names_to = 'customer',
               values_to = 'kW') %>% 
  group_by(customer, halfhour) %>%
  summarise(mean_kW = mean(kW)) %>%
  pivot_wider(names_from = halfhour, 
              values_from = mean_kW) %>%
  ungroup()


#########################Cluster analysis

set.seed(123)

k<-6

### Run the cluster analysis
novemberclusters <- november_transform %>%
  select(-customer)%>%
  kmeans(k)


november_transform2 <- november_transform %>%
  mutate(cluster = as.factor(novemberclusters$cluster))

##################################CREATE AVERAGES OF CONTROL GROUP CLUSTERS PER MONTH#############
november2<-lclnovember%>%
  mutate(halfhour=ifelse(lclnovember$minute>1, (2*lclnovember$hour)+1, 2*lclnovember$hour))%>%
  mutate(month = lubridate::month(GMT))

november2<-november2[c(2:4049, 4057)]

november_longer<-november2%>%
  pivot_longer(-c(GMT, ID, halfhour),
               names_to = 'customer',
               values_to = 'kW')

november_longer<-merge(november_longer,november_transform2, by="customer")
november_longer<-november_longer[c(1,3,4,5,54)]

november_longer$group <- ifelse(grepl("D", november_longer$customer), "treatment",
                                ifelse(grepl("N", november_longer$customer), "control", NA))
november_longer2<-november_longer%>%
  filter(group=="control")

november_longer2 <- november_longer2 %>% 
  group_by(cluster, GMT) %>%
  summarise(mean_kW = mean(kW)) %>%
  ungroup()

november_longer3<-merge(november_longer2,november_longer,by=c("GMT", "cluster"))


# Part 2: DECEMBER 2012 ----------------------------------------------------
###########################################################################December 2012

###################cut data to keep only December observations
lcldecember<-lcl%>%
  filter(GMT >= "2012-12-01 00:00:00",
         GMT <= "2012-12-31 23:30:00")

#add a halfhour variable and month variable
december<-lcldecember%>%
  mutate(halfhour=ifelse(lcldecember$minute>1, (2*lcldecember$hour)+1, 2*lcldecember$hour))%>%
  mutate(month = lubridate::month(GMT))

#keep only relevant columns, i.e., household observations and halfhour and month variables
december<-december[c(2:4049, 4057:4058)]


########create typical load profiles by averaging loads for each customer
december_transform <- december %>% 
  pivot_longer(-c(GMT, ID, halfhour,month),
               names_to = 'customer',
               values_to = 'kW') %>% 
  group_by(customer, halfhour) %>%
  summarise(mean_kW = mean(kW)) %>%
  pivot_wider(names_from = halfhour, 
              values_from = mean_kW) %>%
  ungroup()


#########################Cluster analysis

set.seed(123)

k<-6

### Run the cluster analysis
decemberclusters <- december_transform %>%
  select(-customer)%>%
  kmeans(k)


december_transform2 <- december_transform %>%
  mutate(cluster = as.factor(decemberclusters$cluster))



##################################CREATE AVERAGES OF CONTROL GROUP CLUSTERS PER MONTH#############
december2<-lcldecember%>%
  mutate(halfhour=ifelse(lcldecember$minute>1, (2*lcldecember$hour)+1, 2*lcldecember$hour))%>%
  mutate(month = lubridate::month(GMT))

december2<-december2[c(2:4049, 4057)]

december_longer<-december2%>%
  pivot_longer(-c(GMT, ID, halfhour),
               names_to = 'customer',
               values_to = 'kW')

december_longer<-merge(december_longer,december_transform2, by="customer")
december_longer<-december_longer[c(1,3,4,5,54)]

december_longer$group <- ifelse(grepl("D", december_longer$customer), "treatment",
                                ifelse(grepl("N", december_longer$customer), "control", NA))
december_longer2<-december_longer%>%
  filter(group=="control")

december_longer2 <- december_longer2 %>% 
  group_by(cluster, GMT) %>%
  summarise(mean_kW = mean(kW)) %>%
  ungroup()

december_longer3<-merge(december_longer2,december_longer,by=c("GMT", "cluster"))





#####Create merged dataset and save down
merged_2012<-rbind(july_longer3,august_longer3,september_longer3,october_longer3,
                   november_longer3,december_longer3)

write.csv(merged_2012, "C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/merged_2012.csv", row.names = FALSE)
