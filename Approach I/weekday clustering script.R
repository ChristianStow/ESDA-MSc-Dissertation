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

test<-days_without_events %>% sample_frac(0.2, replace = FALSE)
test<-test%>%
  mutate(Date=as.Date(Date))

testdata<-lcl%>%
  filter(Date %in% test$Date)




####create training dataset
#remove the trial days from the overall dataset
train<-anti_join(all_days,days_with_events, by="Date")
train2<-anti_join(train, test, by="Date")

lcl<-lcl%>%
  filter(Date %in% train2$Date)








# Part 2: JANUARY 2013 ----------------------------------------------------
###########################################################################January 2013

###################cut data to keep only January observations
lcljanuary<-lcl%>%
  filter(GMT >= "2013-01-01 00:00:00",
         GMT < "2013-01-31 00:00:00")

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
fviz_nbclust(jan_transform [,2:49], kmeans, method = "wss", k=20)#this points towards 9

set.seed(123)

k<-6

### Run the cluster analysis
januaryclusters <- jan_transform %>%
  select(-customer)%>%
  kmeans(k)


jan_transform2 <- jan_transform %>%
  mutate(cluster = as.factor(januaryclusters$cluster))





# Part 2: FEBRUARY 2013 ----------------------------------------------------
###########################################################################February 2013

###################cut data to keep only February observations
lclfebruary<-lcl%>%
  filter(GMT >= "2013-02-01 00:00:00",
         GMT < "2013-02-28 00:00:00")

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
fviz_nbclust(feb_transform [,2:49], kmeans, method = "wss", k=20)#this points towards 9

set.seed(123)

k<-6

### Run the cluster analysis
februaryclusters <- feb_transform %>%
  select(-customer)%>%
  kmeans(k)


feb_transform2 <- feb_transform %>%
  mutate(cluster = as.factor(februaryclusters$cluster))




# Part 2: MARCH 2013 ----------------------------------------------------
###########################################################################March 2013

###################cut data to keep only March observations
lclmarch<-lcl%>%
  filter(GMT >= "2013-03-01 00:00:00",
         GMT < "2013-03-31 00:00:00")

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
fviz_nbclust(mar_transform [,2:49], kmeans, method = "wss", k=20)#this points towards 9

set.seed(123)

k<-6

### Run the cluster analysis
marchclusters <- mar_transform %>%
  select(-customer)%>%
  kmeans(k)


mar_transform2 <- mar_transform %>%
  mutate(cluster = as.factor(marchclusters$cluster))





# Part 2: APRIL 2013 ----------------------------------------------------
###########################################################################April 2013

###################cut data to keep only April observations
lclapril<-lcl%>%
  filter(GMT >= "2013-04-01 00:00:00",
         GMT < "2013-04-30 00:00:00")

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
fviz_nbclust(apr_transform [,2:49], kmeans, method = "wss", k=20)#this points towards 9

set.seed(123)

k<-6

### Run the cluster analysis
aprilclusters <- apr_transform %>%
  select(-customer)%>%
  kmeans(k)


apr_transform2 <- apr_transform %>%
  mutate(cluster = as.factor(aprilclusters$cluster))



# Part 2: MAY 2013 ----------------------------------------------------
###########################################################################May 2013

###################cut data to keep only May observations
lclmay<-lcl%>%
  filter(GMT >= "2013-05-01 00:00:00",
         GMT < "2013-05-31 00:00:00")

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
fviz_nbclust(may_transform [,2:49], kmeans, method = "wss", k=20)#this points towards 9

set.seed(123)

k<-6

### Run the cluster analysis
mayclusters <- may_transform %>%
  select(-customer)%>%
  kmeans(k)


may_transform2 <- may_transform %>%
  mutate(cluster = as.factor(mayclusters$cluster))



# Part 2: JUNE 2013 ----------------------------------------------------
###########################################################################June 2013

###################cut data to keep only June observations
lcljune<-lcl%>%
  filter(GMT >= "2013-06-01 00:00:00",
         GMT < "2013-06-30 00:00:00")

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
fviz_nbclust(june_transform [,2:49], kmeans, method = "wss", k=20)#this points towards 9

set.seed(123)

k<-6

### Run the cluster analysis
juneclusters <- june_transform %>%
  select(-customer)%>%
  kmeans(k)


june_transform2 <- june_transform %>%
  mutate(cluster = as.factor(juneclusters$cluster))




# Part 2: JULY 2013 ----------------------------------------------------
###########################################################################July 2013

###################cut data to keep only July observations
lcljuly<-lcl%>%
  filter(GMT >= "2013-07-01 00:00:00",
         GMT < "2013-07-31 00:00:00")

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
fviz_nbclust(july_transform [,2:49], kmeans, method = "wss", k=20)#this points towards 9

set.seed(123)

k<-6

### Run the cluster analysis
julyclusters <- july_transform %>%
  select(-customer)%>%
  kmeans(k)


july_transform2 <- july_transform %>%
  mutate(cluster = as.factor(julyclusters$cluster))



# Part 2: AUGUST 2013 ----------------------------------------------------
###########################################################################August 2013

###################cut data to keep only August observations
lclaugust<-lcl%>%
  filter(GMT >= "2013-08-01 00:00:00",
         GMT < "2013-08-31 00:00:00")

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
fviz_nbclust(august_transform [,2:49], kmeans, method = "wss", k=20)#this points towards 9

set.seed(123)

k<-6

### Run the cluster analysis
augustclusters <- august_transform %>%
  select(-customer)%>%
  kmeans(k)


august_transform2 <- august_transform %>%
  mutate(cluster = as.factor(augustclusters$cluster))




# Part 2: SEPTEMBER 2013 ----------------------------------------------------
###########################################################################September 2013

###################cut data to keep only September observations
lclseptember<-lcl%>%
  filter(GMT >= "2013-09-01 00:00:00",
         GMT < "2013-09-30 00:00:00")

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
fviz_nbclust(september_transform [,2:49], kmeans, method = "wss", k=20)#this points towards 9

set.seed(123)

k<-7

### Run the cluster analysis
septemberclusters <- september_transform %>%
  select(-customer)%>%
  kmeans(k)


september_transform2 <- september_transform %>%
  mutate(cluster = as.factor(septemberclusters$cluster))





# Part 2: OCTOBER 2013 ----------------------------------------------------
###########################################################################October 2013

###################cut data to keep only October observations
lcloctober<-lcl%>%
  filter(GMT >= "2013-10-01 00:00:00",
         GMT < "2013-10-31 00:00:00")

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
fviz_nbclust(october_transform [,2:49], kmeans, method = "wss", k=20)#this points towards 9

set.seed(123)

k<-6

### Run the cluster analysis
octoberclusters <- october_transform %>%
  select(-customer)%>%
  kmeans(k)


october_transform2 <- october_transform %>%
  mutate(cluster = as.factor(octoberclusters$cluster))




# Part 2: NOVEMBER 2013 ----------------------------------------------------
###########################################################################November 2013

###################cut data to keep only November observations
lclnovember<-lcl%>%
  filter(GMT >= "2013-11-01 00:00:00",
         GMT < "2013-11-30 00:00:00")

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
fviz_nbclust(november_transform [,2:49], kmeans, method = "wss", k=20)#this points towards 9

set.seed(123)

k<-6

### Run the cluster analysis
novemberclusters <- november_transform %>%
  select(-customer)%>%
  kmeans(k)


november_transform2 <- november_transform %>%
  mutate(cluster = as.factor(novemberclusters$cluster))



# Part 2: DECEMBER 2013 ----------------------------------------------------
###########################################################################December 2013

###################cut data to keep only December observations
lcldecember<-lcl%>%
  filter(GMT >= "2013-12-01 00:00:00",
         GMT < "2013-12-31 00:00:00")

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
fviz_nbclust(december_transform [,2:49], kmeans, method = "wss", k=20)#this points towards 9

set.seed(123)

k<-6

### Run the cluster analysis
decemberclusters <- december_transform %>%
  select(-customer)%>%
  kmeans(k)


december_transform2 <- december_transform %>%
  mutate(cluster = as.factor(decemberclusters$cluster))








# Part 3: calculate consumption of treatment group homes based on control group average -----------------------------------


###create single month cluster assignments

jan_clusters<-jan_transform2[c(1, 50)]
feb_clusters<-feb_transform2[c(1, 50)]
mar_clusters<-mar_transform2[c(1, 50)]
apr_clusters<-apr_transform2[c(1, 50)]
may_clusters<-may_transform2[c(1, 50)]
june_clusters<-june_transform2[c(1, 50)]
july_clusters<-july_transform2[c(1, 50)]
aug_clusters<-august_transform2[c(1, 50)]
sep_clusters<-september_transform2[c(1, 50)]
oct_clusters<-october_transform2[c(1, 50)]
nov_clusters<-november_transform2[c(1, 50)]
dec_clusters<-december_transform2[c(1, 50)]




###################create new test dataset for JANUARY#################################
januarytestdata<-testdata%>%
  filter(GMT >= "2013-01-01 00:00:00",
         GMT < "2013-01-31 00:00:00")

januarytestdata2<-januarytestdata[c(3:4049)]
januarytestdata2<-t(januarytestdata2)
januarytestdata2 <- data.frame(row_names = row.names(januarytestdata2), januarytestdata2)
row.names(januarytestdata2) <- NULL
colnames(januarytestdata2) <- januarytestdata2[1, ]
januarytestdata2 <- januarytestdata2[-1, ]
colnames(januarytestdata2)[1] <- "customer"


#bring in the cluster and group IDs into the 
january_merged <- merge(januarytestdata2, jan_clusters, by = "customer", all.x = TRUE)

january_merged[, 2:194] <- apply(january_merged[, 2:194], 2, as.numeric)

january_merged$group <- ifelse(grepl("D", january_merged$customer), "treatment",
                                    ifelse(grepl("N", january_merged$customer), "control", NA))

january_treatment_df <- subset(january_merged, group == "treatment")

# Step 2: Create a new data frame with the same structure as the subsetted data frame
january_averaged_df <- january_treatment_df

# Step 3: Iterate through each month column and calculate the average value from the control group customers in the same cluster

for (i in 2:(ncol(january_averaged_df) - 2)) {
  for (j in unique(january_averaged_df$cluster)) {
    cluster_values <- january_merged[january_merged$cluster == j & january_merged$group == "control", i]
    january_averaged_df[january_averaged_df$cluster == j, i] <- mean(cluster_values, na.rm = TRUE)
  }
}




###################create new test dataset for FEBRUARY#################################
februarytestdata<-testdata%>%
  filter(GMT >= "2013-02-01 00:00:00",
         GMT < "2013-02-28 00:00:00")

februarytestdata2<-februarytestdata[c(3:4049)]
februarytestdata2<-t(februarytestdata2)
februarytestdata2 <- data.frame(row_names = row.names(februarytestdata2), februarytestdata2)
row.names(februarytestdata2) <- NULL
colnames(februarytestdata2) <- februarytestdata2[1, ]
februarytestdata2 <- februarytestdata2[-1, ]
colnames(februarytestdata2)[1] <- "customer"


#bring in the cluster and group IDs into the 
february_merged <- merge(februarytestdata2, feb_clusters, by = "customer", all.x = TRUE)

february_merged[, 2:146] <- apply(february_merged[, 2:146], 2, as.numeric)

february_merged$group <- ifelse(grepl("D", february_merged$customer), "treatment",
                                ifelse(grepl("N", february_merged$customer), "control", NA))

february_treatment_df <- subset(february_merged, group == "treatment")

# Step 2: Create a new data frame with the same structure as the subsetted data frame
february_averaged_df <- february_treatment_df

# Step 3: Iterate through each month column and calculate the average value from the control group customers in the same cluster

for (i in 2:(ncol(february_averaged_df) - 2)) {
  for (j in unique(february_averaged_df$cluster)) {
    cluster_values <- february_merged[february_merged$cluster == j & february_merged$group == "control", i]
    february_averaged_df[february_averaged_df$cluster == j, i] <- mean(cluster_values, na.rm = TRUE)
  }
} 



###################create new test dataset for MARCH#################################
marchtestdata<-testdata%>%
  filter(GMT >= "2013-03-01 00:00:00",
         GMT < "2013-03-31 00:00:00")

marchtestdata2<-marchtestdata[c(3:4049)]
marchtestdata2<-t(marchtestdata2)
marchtestdata2 <- data.frame(row_names = row.names(marchtestdata2), marchtestdata2)
row.names(marchtestdata2) <- NULL
colnames(marchtestdata2) <- marchtestdata2[1, ]
marchtestdata2 <- marchtestdata2[-1, ]
colnames(marchtestdata2)[1] <- "customer"


#bring in the cluster and group IDs into the 
march_merged <- merge(marchtestdata2, mar_clusters, by = "customer", all.x = TRUE)

march_merged[, 2:146] <- apply(march_merged[, 2:146], 2, as.numeric)

march_merged$group <- ifelse(grepl("D", march_merged$customer), "treatment",
                             ifelse(grepl("N", march_merged$customer), "control", NA))

march_treatment_df <- subset(march_merged, group == "treatment")

# Step 2: Create a new data frame with the same structure as the subsetted data frame
march_averaged_df <- march_treatment_df

# Step 3: Iterate through each month column and calculate the average value from the control group customers in the same cluster

for (i in 2:(ncol(march_averaged_df) - 2)) {
  for (j in unique(march_averaged_df$cluster)) {
    cluster_values <- march_merged[march_merged$cluster == j & march_merged$group == "control", i]
    march_averaged_df[march_averaged_df$cluster == j, i] <- mean(cluster_values, na.rm = TRUE)
  }
} 



###################create new test dataset for APRIL#################################
apriltestdata<-testdata%>%
  filter(GMT >= "2013-04-01 00:00:00",
         GMT < "2013-04-30 00:00:00")

apriltestdata2<-apriltestdata[c(3:4049)]
apriltestdata2<-t(apriltestdata2)
apriltestdata2 <- data.frame(row_names = row.names(apriltestdata2), apriltestdata2)
row.names(apriltestdata2) <- NULL
colnames(apriltestdata2) <- apriltestdata2[1, ]
apriltestdata2 <- apriltestdata2[-1, ]
colnames(apriltestdata2)[1] <- "customer"


#bring in the cluster and group IDs into the 
april_merged <- merge(apriltestdata2, apr_clusters, by = "customer", all.x = TRUE)

april_merged[, 2:242] <- apply(april_merged[, 2:242], 2, as.numeric)

april_merged$group <- ifelse(grepl("D", april_merged$customer), "treatment",
                             ifelse(grepl("N", april_merged$customer), "control", NA))

april_treatment_df <- subset(april_merged, group == "treatment")

# Step 2: Create a new data frame with the same structure as the subsetted data frame
april_averaged_df <- april_treatment_df

# Step 3: Iterate through each month column and calculate the average value from the control group customers in the same cluster

for (i in 2:(ncol(april_averaged_df) - 2)) {
  for (j in unique(april_averaged_df$cluster)) {
    cluster_values <- april_merged[april_merged$cluster == j & april_merged$group == "control", i]
    april_averaged_df[april_averaged_df$cluster == j, i] <- mean(cluster_values, na.rm = TRUE)
  }
} 




###################create new test dataset for MAY#################################
maytestdata<-testdata%>%
  filter(GMT >= "2013-05-01 00:00:00",
         GMT < "2013-05-31 00:00:00")

maytestdata2<-maytestdata[c(3:4049)]
maytestdata2<-t(maytestdata2)
maytestdata2 <- data.frame(row_names = row.names(maytestdata2), maytestdata2)
row.names(maytestdata2) <- NULL
colnames(maytestdata2) <- maytestdata2[1, ]
maytestdata2 <- maytestdata2[-1, ]
colnames(maytestdata2)[1] <- "customer"


#bring in the cluster and group IDs into the 
may_merged <- merge(maytestdata2, may_clusters, by = "customer", all.x = TRUE)

may_merged[, 2:194] <- apply(may_merged[, 2:194], 2, as.numeric)

may_merged$group <- ifelse(grepl("D", may_merged$customer), "treatment",
                           ifelse(grepl("N", may_merged$customer), "control", NA))

may_treatment_df <- subset(may_merged, group == "treatment")

# Step 2: Create a new data frame with the same structure as the subsetted data frame
may_averaged_df <- may_treatment_df

# Step 3: Iterate through each month column and calculate the average value from the control group customers in the same cluster

for (i in 2:(ncol(may_averaged_df) - 2)) {
  for (j in unique(may_averaged_df$cluster)) {
    cluster_values <- may_merged[may_merged$cluster == j & may_merged$group == "control", i]
    may_averaged_df[may_averaged_df$cluster == j, i] <- mean(cluster_values, na.rm = TRUE)
  }
} 




###################create new test dataset for JUNE#################################
junetestdata<-testdata%>%
  filter(GMT >= "2013-06-01 00:00:00",
         GMT < "2013-06-31 00:00:00")

junetestdata2<-junetestdata[c(3:4049)]
junetestdata2<-t(junetestdata2)
junetestdata2 <- data.frame(row_names = row.names(junetestdata2), junetestdata2)
row.names(junetestdata2) <- NULL
colnames(junetestdata2) <- junetestdata2[1, ]
junetestdata2 <- junetestdata2[-1, ]
colnames(junetestdata2)[1] <- "customer"


#bring in the cluster and group IDs into the 
june_merged <- merge(junetestdata2, june_clusters, by = "customer", all.x = TRUE)

june_merged[, 2:146] <- apply(june_merged[, 2:146], 2, as.numeric)

june_merged$group <- ifelse(grepl("D", june_merged$customer), "treatment",
                            ifelse(grepl("N", june_merged$customer), "control", NA))

june_treatment_df <- subset(june_merged, group == "treatment")

# Step 2: Create a new data frame with the same structure as the subsetted data frame
june_averaged_df <- june_treatment_df

# Step 3: Iterate through each month column and calculate the average value from the control group customers in the same cluster

for (i in 2:(ncol(june_averaged_df) - 2)) {
  for (j in unique(june_averaged_df$cluster)) {
    cluster_values <- june_merged[june_merged$cluster == j & june_merged$group == "control", i]
    june_averaged_df[june_averaged_df$cluster == j, i] <- mean(cluster_values, na.rm = TRUE)
  }
} 




###################create new test dataset for JULY#################################
julytestdata<-testdata%>%
  filter(GMT >= "2013-07-01 00:00:00",
         GMT < "2013-07-31 00:00:00")

julytestdata2<-julytestdata[c(3:4049)]
julytestdata2<-t(julytestdata2)
julytestdata2 <- data.frame(row_names = row.names(julytestdata2), julytestdata2)
row.names(julytestdata2) <- NULL
colnames(julytestdata2) <- julytestdata2[1, ]
julytestdata2 <- julytestdata2[-1, ]
colnames(julytestdata2)[1] <- "customer"


#bring in the cluster and group IDs into the 
july_merged <- merge(julytestdata2, july_clusters, by = "customer", all.x = TRUE)

july_merged[, 2:194] <- apply(july_merged[, 2:194], 2, as.numeric)

july_merged$group <- ifelse(grepl("D", july_merged$customer), "treatment",
                            ifelse(grepl("N", july_merged$customer), "control", NA))

july_treatment_df <- subset(july_merged, group == "treatment")

# Step 2: Create a new data frame with the same structure as the subsetted data frame
july_averaged_df <- july_treatment_df

# Step 3: Iterate through each month column and calculate the average value from the control group customers in the same cluster

for (i in 2:(ncol(july_averaged_df) - 2)) {
  for (j in unique(july_averaged_df$cluster)) {
    cluster_values <- july_merged[july_merged$cluster == j & july_merged$group == "control", i]
    july_averaged_df[july_averaged_df$cluster == j, i] <- mean(cluster_values, na.rm = TRUE)
  }
} 




###################create new test dataset for AUGUST#################################
augusttestdata<-testdata%>%
  filter(GMT >= "2013-08-01 00:00:00",
         GMT < "2013-08-31 00:00:00")

augusttestdata2<-augusttestdata[c(3:4049)]
augusttestdata2<-t(augusttestdata2)
augusttestdata2 <- data.frame(row_names = row.names(augusttestdata2), augusttestdata2)
row.names(augusttestdata2) <- NULL
colnames(augusttestdata2) <- augusttestdata2[1, ]
augusttestdata2 <- augusttestdata2[-1, ]
colnames(augusttestdata2)[1] <- "customer"


#bring in the cluster and group IDs into the 
august_merged <- merge(augusttestdata2, aug_clusters, by = "customer", all.x = TRUE)

august_merged[, 2:290] <- apply(august_merged[, 2:290], 2, as.numeric)

august_merged$group <- ifelse(grepl("D", august_merged$customer), "treatment",
                              ifelse(grepl("N", august_merged$customer), "control", NA))

august_treatment_df <- subset(august_merged, group == "treatment")

# Step 2: Create a new data frame with the same structure as the subsetted data frame
august_averaged_df <- august_treatment_df

# Step 3: Iterate through each month column and calculate the average value from the control group customers in the same cluster

for (i in 2:(ncol(august_averaged_df) - 2)) {
  for (j in unique(august_averaged_df$cluster)) {
    cluster_values <- august_merged[august_merged$cluster == j & august_merged$group == "control", i]
    august_averaged_df[august_averaged_df$cluster == j, i] <- mean(cluster_values, na.rm = TRUE)
  }
} 




###################create new test dataset for SEPTEMBER#################################
septembertestdata<-testdata%>%
  filter(GMT >= "2013-09-01 00:00:00",
         GMT < "2013-09-30 00:00:00")

septembertestdata2<-septembertestdata[c(3:4049)]
septembertestdata2<-t(septembertestdata2)
septembertestdata2 <- data.frame(row_names = row.names(septembertestdata2), septembertestdata2)
row.names(septembertestdata2) <- NULL
colnames(septembertestdata2) <- septembertestdata2[1, ]
septembertestdata2 <- septembertestdata2[-1, ]
colnames(septembertestdata2)[1] <- "customer"


#bring in the cluster and group IDs into the 
september_merged <- merge(septembertestdata2, sep_clusters, by = "customer", all.x = TRUE)

september_merged[, 2:146] <- apply(september_merged[, 2:146], 2, as.numeric)

september_merged$group <- ifelse(grepl("D", september_merged$customer), "treatment",
                                 ifelse(grepl("N", september_merged$customer), "control", NA))

september_treatment_df <- subset(september_merged, group == "treatment")

# Step 2: Create a new data frame with the same structure as the subsetted data frame
september_averaged_df <- september_treatment_df

# Step 3: Iterate through each month column and calculate the average value from the control group customers in the same cluster

for (i in 2:(ncol(september_averaged_df) - 2)) {
  for (j in unique(september_averaged_df$cluster)) {
    cluster_values <- september_merged[september_merged$cluster == j & september_merged$group == "control", i]
    september_averaged_df[september_averaged_df$cluster == j, i] <- mean(cluster_values, na.rm = TRUE)
  }
} 




###################create new test dataset for OCTOBER#################################
octobertestdata<-testdata%>%
  filter(GMT >= "2013-10-01 00:00:00",
         GMT < "2013-10-31 00:00:00")

octobertestdata2<-octobertestdata[c(3:4049)]
octobertestdata2<-t(octobertestdata2)
octobertestdata2 <- data.frame(row_names = row.names(octobertestdata2), octobertestdata2)
row.names(octobertestdata2) <- NULL
colnames(octobertestdata2) <- octobertestdata2[1, ]
octobertestdata2 <- octobertestdata2[-1, ]
colnames(octobertestdata2)[1] <- "customer"


#bring in the cluster and group IDs into the 
october_merged <- merge(octobertestdata2, oct_clusters, by = "customer", all.x = TRUE)

october_merged[, 2:146] <- apply(october_merged[, 2:146], 2, as.numeric)

october_merged$group <- ifelse(grepl("D", october_merged$customer), "treatment",
                               ifelse(grepl("N", october_merged$customer), "control", NA))

october_treatment_df <- subset(october_merged, group == "treatment")

# Step 2: Create a new data frame with the same structure as the subsetted data frame
october_averaged_df <- october_treatment_df

# Step 3: Iterate through each month column and calculate the average value from the control group customers in the same cluster

for (i in 2:(ncol(october_averaged_df) - 2)) {
  for (j in unique(october_averaged_df$cluster)) {
    cluster_values <- october_merged[october_merged$cluster == j & october_merged$group == "control", i]
    october_averaged_df[october_averaged_df$cluster == j, i] <- mean(cluster_values, na.rm = TRUE)
  }
} 




###################create new test dataset for NOVEMBER#################################
novembertestdata<-testdata%>%
  filter(GMT >= "2013-11-01 00:00:00",
         GMT < "2013-11-30 00:00:00")

novembertestdata2<-novembertestdata[c(3:4049)]
novembertestdata2<-t(novembertestdata2)
novembertestdata2 <- data.frame(row_names = row.names(novembertestdata2), novembertestdata2)
row.names(novembertestdata2) <- NULL
colnames(novembertestdata2) <- novembertestdata2[1, ]
novembertestdata2 <- novembertestdata2[-1, ]
colnames(novembertestdata2)[1] <- "customer"


#bring in the cluster and group IDs into the 
november_merged <- merge(novembertestdata2, nov_clusters, by = "customer", all.x = TRUE)

november_merged[, 2:146] <- apply(november_merged[, 2:146], 2, as.numeric)

november_merged$group <- ifelse(grepl("D", november_merged$customer), "treatment",
                                ifelse(grepl("N", november_merged$customer), "control", NA))

november_treatment_df <- subset(november_merged, group == "treatment")

# Step 2: Create a new data frame with the same structure as the subsetted data frame
november_averaged_df <- november_treatment_df

# Step 3: Iterate through each month column and calculate the average value from the control group customers in the same cluster

for (i in 2:(ncol(november_averaged_df) - 2)) {
  for (j in unique(november_averaged_df$cluster)) {
    cluster_values <- november_merged[november_merged$cluster == j & november_merged$group == "control", i]
    november_averaged_df[november_averaged_df$cluster == j, i] <- mean(cluster_values, na.rm = TRUE)
  }
} 




###################create new test dataset for DECEMBER#################################
decembertestdata<-testdata%>%
  filter(GMT >= "2013-12-01 00:00:00",
         GMT < "2013-12-31 00:00:00")

decembertestdata2<-decembertestdata[c(3:4049)]
decembertestdata2<-t(decembertestdata2)
decembertestdata2 <- data.frame(row_names = row.names(decembertestdata2), decembertestdata2)
row.names(decembertestdata2) <- NULL
colnames(decembertestdata2) <- decembertestdata2[1, ]
decembertestdata2 <- decembertestdata2[-1, ]
colnames(decembertestdata2)[1] <- "customer"


#bring in the cluster and group IDs into the 
december_merged <- merge(decembertestdata2, dec_clusters, by = "customer", all.x = TRUE)

december_merged[, 2:98] <- apply(december_merged[, 2:98], 2, as.numeric)

december_merged$group <- ifelse(grepl("D", december_merged$customer), "treatment",
                                ifelse(grepl("N", december_merged$customer), "control", NA))

december_treatment_df <- subset(december_merged, group == "treatment")

# Step 2: Create a new data frame with the same structure as the subsetted data frame
december_averaged_df <- december_treatment_df

# Step 3: Iterate through each month column and calculate the average value from the control group customers in the same cluster

for (i in 2:(ncol(december_averaged_df) - 2)) {
  for (j in unique(december_averaged_df$cluster)) {
    cluster_values <- december_merged[december_merged$cluster == j & december_merged$group == "control", i]
    december_averaged_df[december_averaged_df$cluster == j, i] <- mean(cluster_values, na.rm = TRUE)
  }
} 





##########APPEND ALL DATASETS TOGETHER####################
january_averaged_df<-january_averaged_df[, -c(ncol(january_averaged_df)-1, ncol(january_averaged_df))]
february_averaged_df<-february_averaged_df[, -c(ncol(february_averaged_df)-1, ncol(february_averaged_df))]
march_averaged_df<-march_averaged_df[, -c(ncol(march_averaged_df)-1, ncol(march_averaged_df))]
april_averaged_df<-april_averaged_df[, -c(ncol(april_averaged_df)-1, ncol(april_averaged_df))]
may_averaged_df<-may_averaged_df[, -c(ncol(may_averaged_df)-1, ncol(may_averaged_df))]
june_averaged_df<-june_averaged_df[, -c(ncol(june_averaged_df)-1, ncol(june_averaged_df))]
july_averaged_df<-july_averaged_df[, -c(ncol(july_averaged_df)-1, ncol(july_averaged_df))]
august_averaged_df<-august_averaged_df[, -c(ncol(august_averaged_df)-1, ncol(august_averaged_df))]
september_averaged_df<-september_averaged_df[, -c(ncol(september_averaged_df)-1, ncol(september_averaged_df))]
october_averaged_df<-october_averaged_df[, -c(ncol(october_averaged_df)-1, ncol(october_averaged_df))]
november_averaged_df<-november_averaged_df[, -c(ncol(november_averaged_df)-1, ncol(november_averaged_df))]
december_averaged_df<-december_averaged_df[, -c(ncol(december_averaged_df)-1, ncol(december_averaged_df))]


datasets_to_merge <- list(january_averaged_df, february_averaged_df, march_averaged_df, april_averaged_df,
                          may_averaged_df, june_averaged_df, july_averaged_df, august_averaged_df,
                          september_averaged_df, october_averaged_df, november_averaged_df, december_averaged_df)

averaged_combined<-Reduce(function(x, y) merge(x, y, by = "customer", all = TRUE), datasets_to_merge)








january_treatment_df<-january_treatment_df[, -c(ncol(january_treatment_df)-1, ncol(january_treatment_df))]
february_treatment_df<-february_treatment_df[, -c(ncol(february_treatment_df)-1, ncol(february_treatment_df))]
march_treatment_df<-march_treatment_df[, -c(ncol(march_treatment_df)-1, ncol(march_treatment_df))]
april_treatment_df<-april_treatment_df[, -c(ncol(april_treatment_df)-1, ncol(april_treatment_df))]
may_treatment_df<-may_treatment_df[, -c(ncol(may_treatment_df)-1, ncol(may_treatment_df))]
june_treatment_df<-june_treatment_df[, -c(ncol(june_treatment_df)-1, ncol(june_treatment_df))]
july_treatment_df<-july_treatment_df[, -c(ncol(july_treatment_df)-1, ncol(july_treatment_df))]
august_treatment_df<-august_treatment_df[, -c(ncol(august_treatment_df)-1, ncol(august_treatment_df))]
september_treatment_df<-september_treatment_df[, -c(ncol(september_treatment_df)-1, ncol(september_treatment_df))]
october_treatment_df<-october_treatment_df[, -c(ncol(october_treatment_df)-1, ncol(october_treatment_df))]
november_treatment_df<-november_treatment_df[, -c(ncol(november_treatment_df)-1, ncol(november_treatment_df))]
december_treatment_df<-december_treatment_df[, -c(ncol(december_treatment_df)-1, ncol(december_treatment_df))]


datasets_to_merge2 <- list(january_treatment_df, february_treatment_df, march_treatment_df, april_treatment_df,
                          may_treatment_df, june_treatment_df, july_treatment_df, august_treatment_df,
                          september_treatment_df, october_treatment_df, november_treatment_df, december_treatment_df)

treatment_combined<-Reduce(function(x, y) merge(x, y, by = "customer", all = TRUE), datasets_to_merge2) 









# Part 4:  Calculate performance metrics----------------------------------------------------------------


treatment_combined2<-t(treatment_combined)
treatment_combined2 <- data.frame(row_names = row.names(treatment_combined2), treatment_combined2)
row.names(treatment_combined2) <- NULL
colnames(treatment_combined2) <- treatment_combined2[1, ]
treatment_combined2 <- treatment_combined2[-1, ]
colnames(treatment_combined2)[1] <- "GMT"
treatment_combined2$Reading<-"actual"





averaged_combined2<-t(averaged_combined)
averaged_combined2 <- data.frame(row_names = row.names(averaged_combined2), averaged_combined2)
row.names(averaged_combined2) <- NULL
colnames(averaged_combined2) <- averaged_combined2[1, ]
averaged_combined2 <- averaged_combined2[-1, ]
colnames(averaged_combined2)[1] <- "GMT"
averaged_combined2$Reading<-"predicted"


averaged_combined3 <- averaged_combined2 %>%
  pivot_longer(-c(GMT, Reading),
               names_to='Customer',
               values_to='Consumption')




treatment_combined3 <- treatment_combined2 %>%
  pivot_longer(-c(GMT, Reading),
               names_to='Customer',
               values_to='Consumption')


treatment_combined3$Predicted<-averaged_combined3$Consumption


# using library ehaGoF for goodness of fit

target<-as.numeric(treatment_combined3$Consumption)
predicted<-as.numeric(treatment_combined3$Predicted)
# Goodness of fit - relative root mean square error (RRMSE)
gofRRMSE(target, predicted)


relative_rmse <- function(observed_values, predicted_values) {
  n <- length(observed_values)
  mse <- sum((observed_values - predicted_values)^2) / n
  rmse <- sqrt(mse)
  max_value <- max(observed_values)
  min_value <- min(observed_values)
  rrmse <- rmse / (max_value - min_value)
  return(rrmse)
}

rrmse_value <- relative_rmse(target, predicted)

relative_error_ratio <- function(observed_value, true_value) {
  error <- observed_value - true_value
  relative_error <- abs(error / true_value)
  return(relative_error)
}

ratio <- as.data.frame(relative_error_ratio(predicted, target))

ratio2<-ratio%>%
  filter(`relative_error_ratio(predicted, target)`<0.1)




####################create line charts

overallmerged2<-treatment_combined3


overallmerged2 <- overallmerged2 %>%
  mutate(hour = lubridate::hour(GMT),
         minute = lubridate::minute(GMT))

overallmerged2<-overallmerged2%>%
  mutate(halfhour=ifelse(overallmerged2$minute>1, (2*overallmerged2$hour)+1, 2*overallmerged2$hour))

#32-43
#14-23
overallmerged2<-overallmerged2%>%
  mutate(Predicted2=ifelse(halfhour==14|halfhour==15|halfhour==16|halfhour==17|halfhour==18|halfhour==19|
                             halfhour==20|halfhour==21|halfhour==22|halfhour==23|halfhour==32|halfhour==33|
                             halfhour==34|halfhour==35|halfhour==36|halfhour==37|halfhour==38|halfhour==39|
                             halfhour==40|halfhour==41|halfhour==42|halfhour==43, Predicted, NA))

overallmerged2<-overallmerged2%>%
  mutate(Predicted2=ifelse(halfhour==13|halfhour==24|halfhour==31|halfhour==44, Consumption, Predicted2))


overallmerged2<-overallmerged2%>%
  mutate(combined_date=substr(GMT, 1, 10))

overallmerged2$group <- ifelse(grepl("D", overallmerged2$Customer), "treatment", "control")

overallmerged3<-overallmerged2[c(10, 11, 4, 5, 8)]



overallmerged2_longer <- overallmerged3 %>%
  pivot_longer(cols=c('Consumption','Predicted'),
               names_to='Consumption',
               values_to='kw')

overallmerged2_longer<-overallmerged2_longer%>%
  mutate(kw=as.numeric(kw))


overallmerged2_longer2<-overallmerged2_longer%>%
  filter(combined_date>"2013-03-01",
         combined_date<"2013-08-03")

overallmerged2_longer2<-overallmerged2_longer2%>%
  group_by(Consumption, combined_date, halfhour)%>% 
  summarise(mean_power = mean(kw))



overallmerged2_longer2 %>% 
  ggplot(aes(x = halfhour, 
             y = mean_power, 
             fill = Consumption,
             colour = Consumption)) + 
  geom_line() + 
  facet_wrap(~combined_date)








###final model performance
test<-treatment_combined3%>%
  na.omit()

test<-test%>%
  mutate(Consumption=as.numeric(Consumption))%>%
  mutate(Predicted=as.numeric(Predicted))

test<-test%>%
  mutate(DSR=Consumption-Predicted)

test<-test%>%
  mutate(percent=DSR/Consumption)

RRMSE = sqrt(sum(test$DSR^2))/sqrt(sum(test$Consumption^2))

RER=test$percent
RER=RER[is.finite(RER)]
meanRER <- mean(RER)

####################create line charts

test <- test %>%
  mutate(hour = lubridate::hour(GMT),
         minute = lubridate::minute(GMT))

test<-test%>%
  mutate(halfhour=ifelse(test$minute>1, (2*test$hour)+1, 2*test$hour))

test<-test%>%
  mutate(Date=as.Date(GMT))

test2<-test%>%
  filter(Date=="2013-01-15"| Date=="2013-03-20"|Date=="2013-05-06"| Date=="2013-07-17"| Date=="2013-09-27"|Date=="2013-11-20")

test3 <- test2 %>%
  pivot_longer(cols=c('Predicted'),
               names_to='Value',
               values_to='kw')

test4<-test3%>%
  group_by(Value, Date, halfhour)%>% 
  summarise(mean_power = mean(kw))


test4 %>% 
  ggplot(aes(x = halfhour, 
             y = mean_power, 
             fill = Value,
             colour = Value)) + 
  geom_line() + 
  facet_wrap(~Date)

write.csv(test4, "C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/DR participation/clusterchart.csv", row.names=FALSE)
