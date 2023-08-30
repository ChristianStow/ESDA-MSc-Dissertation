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



#create test dataset
tariff<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/03 Data and methodology/Data/tariff_d.csv")

tariff<-tariff%>%
  mutate(GMT=ymd_hms(GMT))

days_with_events <- tariff %>%
  filter(Event_tags != "") %>%
  distinct(Date = as.Date(GMT))

all_days <- tariff %>%
  filter(Event_tags == "") %>%
  distinct(Date = as.Date(GMT))

days_without_events<- anti_join(all_days, days_with_events, by = "Date")

lcl<-lcl%>%
  filter(Date %in% days_without_events$Date)



lcl2<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Data processing and cleansing/Data exploration/FINALDATASET.csv")
lcl2<-lcl2%>%
  mutate(Date = as.Date(GMT))





# Part 2: JANUARY 2013 ----------------------------------------------------
###########################################################################January 2013

###################cut data to keep only January observations
lcljanuary<-lcl%>%
  filter(GMT >= "2013-01-01 00:00:00",
         GMT <= "2013-01-31 23:30:00")

#add a halfhour variable and month variable
january<-lcljanuary%>%
  mutate(halfhour=ifelse(lcljanuary$minute>1, (2*lcljanuary$hour)+1, 2*lcljanuary$hour))%>%
  mutate(month = lubridate::month(GMT))

#keep only relevant columns, i.e., household observations and halfhour and month variables
january<-january[c(2:4049, 4057:4058)]


########create typical load profiles by averaging loads for each customer
jan_transform <- january %>% 
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
januaryclusters <- jan_transform %>%
  select(-customer)%>%
  kmeans(k)


jan_transform2 <- jan_transform %>%
  mutate(cluster = as.factor(januaryclusters$cluster))

##################################CREATE AVERAGES OF CONTROL GROUP CLUSTERS PER MONTH#############
lcljanuary2<-lcl2%>%
  filter(GMT >= "2013-01-01 00:00:00",
         GMT <= "2013-01-31 23:30:00")

january2<-lcljanuary2%>%
  mutate(halfhour=ifelse(lcljanuary2$minute>1, (2*lcljanuary2$hour)+1, 2*lcljanuary2$hour))%>%
  mutate(month = lubridate::month(GMT))

january2<-january2[c(2:4049, 4057)]

january_longer<-january2%>%
  pivot_longer(-c(GMT, ID, halfhour),
               names_to = 'customer',
               values_to = 'kW')

january_longer<-merge(january_longer,jan_transform2, by="customer")
january_longer<-january_longer[c(1,3,4,5,54)]

january_longer$group <- ifelse(grepl("D", january_longer$customer), "treatment",
                                 ifelse(grepl("N", january_longer$customer), "control", NA))
january_longer2<-january_longer%>%
  filter(group=="control")

january_longer2 <- january_longer2 %>% 
  group_by(cluster, GMT) %>%
  summarise(mean_kW = mean(kW)) %>%
  ungroup()

january_longer3<-merge(january_longer2,january_longer,by=c("GMT", "cluster"))

# Part 2: FEBRUARY 2013 ----------------------------------------------------
###########################################################################February 2013

###################cut data to keep only February observations
lclfebruary<-lcl%>%
  filter(GMT >= "2013-02-01 00:00:00",
         GMT <= "2013-02-28 23:30:00")

#add a halfhour variable and month variable
february<-lclfebruary%>%
  mutate(halfhour=ifelse(lclfebruary$minute>1, (2*lclfebruary$hour)+1, 2*lclfebruary$hour))%>%
  mutate(month = lubridate::month(GMT))

#keep only relevant columns, i.e., household observations and halfhour and month variables
february<-february[c(2:4049, 4057:4058)]


########create typical load profiles by averaging loads for each customer
feb_transform <- february %>% 
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
februaryclusters <- feb_transform %>%
  select(-customer)%>%
  kmeans(k)


feb_transform2 <- feb_transform %>%
  mutate(cluster = as.factor(februaryclusters$cluster))


##################################CREATE AVERAGES OF CONTROL GROUP CLUSTERS PER MONTH#############
lclfebruary2<-lcl2%>%
  filter(GMT >= "2013-02-01 00:00:00",
         GMT <= "2013-02-28 23:30:00")

february2<-lclfebruary2%>%
  mutate(halfhour=ifelse(lclfebruary2$minute>1, (2*lclfebruary2$hour)+1, 2*lclfebruary2$hour))%>%
  mutate(month = lubridate::month(GMT))

february2<-february2[c(2:4049, 4057)]

february_longer<-february2%>%
  pivot_longer(-c(GMT, ID, halfhour),
               names_to = 'customer',
               values_to = 'kW')

february_longer<-merge(february_longer,feb_transform2, by="customer")
february_longer<-february_longer[c(1,3,4,5,54)]

february_longer$group <- ifelse(grepl("D", february_longer$customer), "treatment",
                                ifelse(grepl("N", february_longer$customer), "control", NA))
february_longer2<-february_longer%>%
  filter(group=="control")

february_longer2 <- february_longer2 %>% 
  group_by(cluster, GMT) %>%
  summarise(mean_kW = mean(kW)) %>%
  ungroup()

february_longer3<-merge(february_longer2,february_longer,by=c("GMT", "cluster"))


# Part 2: MARCH 2013 ----------------------------------------------------
###########################################################################March 2013

###################cut data to keep only March observations
lclmarch<-lcl%>%
  filter(GMT >= "2013-03-01 00:00:00",
         GMT <= "2013-03-31 23:30:00")

#add a halfhour variable and month variable
march<-lclmarch%>%
  mutate(halfhour=ifelse(lclmarch$minute>1, (2*lclmarch$hour)+1, 2*lclmarch$hour))%>%
  mutate(month = lubridate::month(GMT))

#keep only relevant columns, i.e., household observations and halfhour and month variables
march<-march[c(2:4049, 4057:4058)]


########create typical load profiles by averaging loads for each customer
mar_transform <- march %>% 
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
marchclusters <- mar_transform %>%
  select(-customer)%>%
  kmeans(k)


mar_transform2 <- mar_transform %>%
  mutate(cluster = as.factor(marchclusters$cluster))



##################################CREATE AVERAGES OF CONTROL GROUP CLUSTERS PER MONTH#############
lclmarch2<-lcl2%>%
  filter(GMT >= "2013-03-01 00:00:00",
         GMT <= "2013-03-31 23:30:00")

march2<-lclmarch2%>%
  mutate(halfhour=ifelse(lclmarch2$minute>1, (2*lclmarch2$hour)+1, 2*lclmarch2$hour))%>%
  mutate(month = lubridate::month(GMT))

march2<-march2[c(2:4049, 4057)]

march_longer<-march2%>%
  pivot_longer(-c(GMT, ID, halfhour),
               names_to = 'customer',
               values_to = 'kW')

march_longer<-merge(march_longer,mar_transform2, by="customer")
march_longer<-march_longer[c(1,3,4,5,54)]

march_longer$group <- ifelse(grepl("D", march_longer$customer), "treatment",
                             ifelse(grepl("N", march_longer$customer), "control", NA))
march_longer2<-march_longer%>%
  filter(group=="control")

march_longer2 <- march_longer2 %>% 
  group_by(cluster, GMT) %>%
  summarise(mean_kW = mean(kW)) %>%
  ungroup()

march_longer3<-merge(march_longer2,march_longer,by=c("GMT", "cluster"))


# Part 2: APRIL 2013 ----------------------------------------------------
###########################################################################April 2013

###################cut data to keep only April observations
lclapril<-lcl%>%
  filter(GMT >= "2013-04-01 00:00:00",
         GMT <= "2013-04-30 23:30:00")

#add a halfhour variable and month variable
april<-lclapril%>%
  mutate(halfhour=ifelse(lclapril$minute>1, (2*lclapril$hour)+1, 2*lclapril$hour))%>%
  mutate(month = lubridate::month(GMT))

#keep only relevant columns, i.e., household observations and halfhour and month variables
april<-april[c(2:4049, 4057:4058)]


########create typical load profiles by averaging loads for each customer
apr_transform <- april %>% 
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
aprilclusters <- apr_transform %>%
  select(-customer)%>%
  kmeans(k)


apr_transform2 <- apr_transform %>%
  mutate(cluster = as.factor(aprilclusters$cluster))


##################################CREATE AVERAGES OF CONTROL GROUP CLUSTERS PER MONTH#############
lclapril2<-lcl2%>%
  filter(GMT >= "2013-04-01 00:00:00",
         GMT <= "2013-04-30 23:30:00")

april2<-lclapril2%>%
  mutate(halfhour=ifelse(lclapril2$minute>1, (2*lclapril2$hour)+1, 2*lclapril2$hour))%>%
  mutate(month = lubridate::month(GMT))

april2<-april2[c(2:4049, 4057)]

april_longer<-april2%>%
  pivot_longer(-c(GMT, ID, halfhour),
               names_to = 'customer',
               values_to = 'kW')

april_longer<-merge(april_longer,apr_transform2, by="customer")
april_longer<-april_longer[c(1,3,4,5,54)]

april_longer$group <- ifelse(grepl("D", april_longer$customer), "treatment",
                             ifelse(grepl("N", april_longer$customer), "control", NA))
april_longer2<-april_longer%>%
  filter(group=="control")

april_longer2 <- april_longer2 %>% 
  group_by(cluster, GMT) %>%
  summarise(mean_kW = mean(kW)) %>%
  ungroup()

april_longer3<-merge(april_longer2,april_longer,by=c("GMT", "cluster"))


# Part 2: MAY 2013 ----------------------------------------------------
###########################################################################May 2013

###################cut data to keep only May observations
lclmay<-lcl%>%
  filter(GMT >= "2013-05-01 00:00:00",
         GMT <= "2013-05-31 23:30:00")

#add a halfhour variable and month variable
may<-lclmay%>%
  mutate(halfhour=ifelse(lclmay$minute>1, (2*lclmay$hour)+1, 2*lclmay$hour))%>%
  mutate(month = lubridate::month(GMT))

#keep only relevant columns, i.e., household observations and halfhour and month variables
may<-may[c(2:4049, 4057:4058)]


########create typical load profiles by averaging loads for each customer
may_transform <- may %>% 
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
mayclusters <- may_transform %>%
  select(-customer)%>%
  kmeans(k)


may_transform2 <- may_transform %>%
  mutate(cluster = as.factor(mayclusters$cluster))


##################################CREATE AVERAGES OF CONTROL GROUP CLUSTERS PER MONTH#############
lclmay2<-lcl2%>%
  filter(GMT >= "2013-05-01 00:00:00",
         GMT <= "2013-05-31 23:30:00")

may2<-lclmay2%>%
  mutate(halfhour=ifelse(lclmay2$minute>1, (2*lclmay2$hour)+1, 2*lclmay2$hour))%>%
  mutate(month = lubridate::month(GMT))

may2<-may2[c(2:4049, 4057)]

may_longer<-may2%>%
  pivot_longer(-c(GMT, ID, halfhour),
               names_to = 'customer',
               values_to = 'kW')

may_longer<-merge(may_longer,may_transform2, by="customer")
may_longer<-may_longer[c(1,3,4,5,54)]

may_longer$group <- ifelse(grepl("D", may_longer$customer), "treatment",
                           ifelse(grepl("N", may_longer$customer), "control", NA))
may_longer2<-may_longer%>%
  filter(group=="control")

may_longer2 <- may_longer2 %>% 
  group_by(cluster, GMT) %>%
  summarise(mean_kW = mean(kW)) %>%
  ungroup()

may_longer3<-merge(may_longer2,may_longer,by=c("GMT", "cluster"))

# Part 2: JUNE 2013 ----------------------------------------------------
###########################################################################June 2013

###################cut data to keep only June observations
lcljune<-lcl%>%
  filter(GMT >= "2013-06-01 00:00:00",
         GMT <= "2013-06-30 23:30:00")

#add a halfhour variable and month variable
june<-lcljune%>%
  mutate(halfhour=ifelse(lcljune$minute>1, (2*lcljune$hour)+1, 2*lcljune$hour))%>%
  mutate(month = lubridate::month(GMT))

#keep only relevant columns, i.e., household observations and halfhour and month variables
june<-june[c(2:4049, 4057:4058)]


########create typical load profiles by averaging loads for each customer
june_transform <- june %>% 
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
juneclusters <- june_transform %>%
  select(-customer)%>%
  kmeans(k)


june_transform2 <- june_transform %>%
  mutate(cluster = as.factor(juneclusters$cluster))


##################################CREATE AVERAGES OF CONTROL GROUP CLUSTERS PER MONTH#############
lcljune2<-lcl2%>%
  filter(GMT >= "2013-06-01 00:00:00",
         GMT <= "2013-06-30 23:30:00")

june2<-lcljune2%>%
  mutate(halfhour=ifelse(lcljune2$minute>1, (2*lcljune2$hour)+1, 2*lcljune2$hour))%>%
  mutate(month = lubridate::month(GMT))

june2<-june2[c(2:4049, 4057)]

june_longer<-june2%>%
  pivot_longer(-c(GMT, ID, halfhour),
               names_to = 'customer',
               values_to = 'kW')

june_longer<-merge(june_longer,june_transform2, by="customer")
june_longer<-june_longer[c(1,3,4,5,54)]

june_longer$group <- ifelse(grepl("D", june_longer$customer), "treatment",
                            ifelse(grepl("N", june_longer$customer), "control", NA))
june_longer2<-june_longer%>%
  filter(group=="control")

june_longer2 <- june_longer2 %>% 
  group_by(cluster, GMT) %>%
  summarise(mean_kW = mean(kW)) %>%
  ungroup()

june_longer3<-merge(june_longer2,june_longer,by=c("GMT", "cluster"))


# Part 2: JULY 2013 ----------------------------------------------------
###########################################################################July 2013

###################cut data to keep only July observations
lcljuly<-lcl%>%
  filter(GMT >= "2013-07-01 00:00:00",
         GMT <= "2013-07-31 23:30:00")

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
lcljuly2<-lcl2%>%
  filter(GMT >= "2013-07-01 00:00:00",
         GMT <= "2013-07-31 23:30:00")

july2<-lcljuly2%>%
  mutate(halfhour=ifelse(lcljuly2$minute>1, (2*lcljuly2$hour)+1, 2*lcljuly2$hour))%>%
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


# Part 2: AUGUST 2013 ----------------------------------------------------
###########################################################################August 2013

###################cut data to keep only August observations
lclaugust<-lcl%>%
  filter(GMT >= "2013-08-01 00:00:00",
         GMT <= "2013-08-31 23:30:00")

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
lclaugust2<-lcl2%>%
  filter(GMT >= "2013-08-01 00:00:00",
         GMT <= "2013-08-31 23:30:00")

august2<-lclaugust2%>%
  mutate(halfhour=ifelse(lclaugust2$minute>1, (2*lclaugust2$hour)+1, 2*lclaugust2$hour))%>%
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


# Part 2: SEPTEMBER 2013 ----------------------------------------------------
###########################################################################September 2013

###################cut data to keep only September observations
lclseptember<-lcl%>%
  filter(GMT >= "2013-09-01 00:00:00",
         GMT <= "2013-09-30 23:30:00")

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
lclseptember2<-lcl2%>%
  filter(GMT >= "2013-09-01 00:00:00",
         GMT <= "2013-09-30 23:30:00")

september2<-lclseptember2%>%
  mutate(halfhour=ifelse(lclseptember2$minute>1, (2*lclseptember2$hour)+1, 2*lclseptember2$hour))%>%
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


# Part 2: OCTOBER 2013 ----------------------------------------------------
###########################################################################October 2013

###################cut data to keep only October observations
lcloctober<-lcl%>%
  filter(GMT >= "2013-10-01 00:00:00",
         GMT <= "2013-10-31 23:30:00")

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
lcloctober2<-lcl2%>%
  filter(GMT >= "2013-10-01 00:00:00",
         GMT <= "2013-10-31 23:30:00")

october2<-lcloctober2%>%
  mutate(halfhour=ifelse(lcloctober2$minute>1, (2*lcloctober2$hour)+1, 2*lcloctober2$hour))%>%
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


# Part 2: NOVEMBER 2013 ----------------------------------------------------
###########################################################################November 2013

###################cut data to keep only November observations
lclnovember<-lcl%>%
  filter(GMT >= "2013-11-01 00:00:00",
         GMT <= "2013-11-30 23:30:00")

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
lclnovember2<-lcl2%>%
  filter(GMT >= "2013-11-01 00:00:00",
         GMT <= "2013-11-30 23:30:00")

november2<-lclnovember2%>%
  mutate(halfhour=ifelse(lclnovember2$minute>1, (2*lclnovember2$hour)+1, 2*lclnovember2$hour))%>%
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


# Part 2: DECEMBER 2013 ----------------------------------------------------
###########################################################################December 2013

###################cut data to keep only December observations
lcldecember<-lcl%>%
  filter(GMT >= "2013-12-01 00:00:00",
         GMT <= "2013-12-31 23:30:00")

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
lcldecember2<-lcl2%>%
  filter(GMT >= "2013-12-01 00:00:00",
         GMT <= "2013-12-31 23:30:00")

december2<-lcldecember2%>%
  mutate(halfhour=ifelse(lcldecember2$minute>1, (2*lcldecember2$hour)+1, 2*lcldecember2$hour))%>%
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
merged_2013<-rbind(january_longer3,february_longer3,march_longer3,april_longer3,may_longer3,
                   june_longer3,july_longer3,august_longer3,september_longer3,october_longer3,
                   november_longer3,december_longer3)

write.csv(merged_2013, "C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/merged_2013.csv", row.names = FALSE)
