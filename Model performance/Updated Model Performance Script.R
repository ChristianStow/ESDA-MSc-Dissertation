library(tidyverse)
library(lubridate)


# Part 1: Approach 1 ------------------------------------------------------

  
approach1test<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Model performance/approach1test.csv")

approach1test<-approach1test%>%
  na.omit()
approach1test<-approach1test%>%
  mutate(DSR=Consumption-Predicted)

approach1test<-approach1test%>%
  mutate(percent=DSR/Consumption)

RRMSE = sqrt(sum(approach1test$DSR^2))/sqrt(sum(approach1test$Consumption^2))

approach1_summary_data <- approach1test %>%
  group_by(Customer) %>%
  summarise(sum_dsr_squared = sum(DSR^2),
            sum_consumption_squared = sum(Consumption^2))

# Calculate RRMSE for each customer
approach1_summary_data <- approach1_summary_data %>%
  mutate(RRMSE = sqrt(sum_dsr_squared) / sqrt(sum_consumption_squared))



# Part 2: Approach 2 ------------------------------------------------------

approach2test<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Model performance/approach2test.csv")

approach2test<-approach2test%>%
  na.omit()
approach2test<-approach2test%>%
  mutate(DSR=Actual-Predicted)

approach2test<-approach2test%>%
  mutate(percent=DSR/Actual)

approach2_summary_data <- approach2test %>%
  group_by(ID) %>%
  summarise(sum_dsr_squared = sum(DSR^2),
            sum_consumption_squared = sum(Actual^2))

# Calculate RRMSE for each customer
approach2_summary_data <- approach2_summary_data %>%
  mutate(RRMSE = sqrt(sum_dsr_squared) / sqrt(sum_consumption_squared))

# Part 3: Approach 3 ------------------------------------------------------
batch1<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/Neural Network Single Customer Model/test_predictions_150.csv")
batch2<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/Neural Network Single Customer Model/test_predictions_300.csv")
batch3<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/Neural Network Single Customer Model/test_predictions_400.csv")
batch4<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/Neural Network Single Customer Model/test_predictions_600.csv")
batch5<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/Neural Network Single Customer Model/test_predictions_847.csv")

approach3test<-rbind(batch1, batch2, batch3, batch4, batch5)

approach3test<-approach3test%>%
  na.omit()
approach3test<-approach3test%>%
  mutate(DSR=original_consumption-predictions)

approach3test<-approach3test%>%
  mutate(percent=DSR/original_consumption)

approach3_summary_data <- approach3test %>%
  group_by(customer) %>%
  summarise(sum_dsr_squared = sum(DSR^2),
            sum_consumption_squared = sum(original_consumption^2))

# Calculate RRMSE for each customer
approach3_summary_data <- approach3_summary_data %>%
  mutate(RRMSE = sqrt(sum_dsr_squared) / sqrt(sum_consumption_squared))




# Part 4: Distribution chart ----------------------------------------------
approach1_summary_data$approach<-"Cluster"
approach2_summary_data$approach<-"FFNN"
approach3_summary_data$approach<-"Cluster+FFNN"

names(approach1_summary_data)[names(approach1_summary_data) == "Customer"] <- "customer"
names(approach2_summary_data)[names(approach2_summary_data) == "ID"] <- "customer"

merged<-rbind(approach1_summary_data, approach2_summary_data, approach3_summary_data)
merged<-merged[c(1,4,5)]


ggplot(merged, aes(x = RRMSE, fill = approach)) +
  geom_density(alpha = 0.5) +
  labs(title = "Kernel Density Plots") +
  theme_minimal()

ggplot(merged, aes(x = RRMSE)) +
  geom_density(alpha = 0.5) +
  labs(title = "Kernel Density Plots") +
  theme_minimal()+
  facet_wrap(vars(approach))


library(extrafont)
walbaum_font <- theme(text = element_text(family = "Times New Roman", size=14))

p <- ggplot(merged, aes(factor(approach), RRMSE))
p + geom_violin(draw_quantiles = c(0.5), aes(fill=factor(approach)))+
  scale_fill_manual(values = c("Cluster" = "#00bfff", "Cluster+FFNN" = "#FA8072", "FFNN"="purple")) +
  walbaum_font+
  labs(x = "Approach", y = "RRMSE", fill = "Approach")+
  ylim(0, 6)+
  guides(fill = "none") 





# Part 5: Scatter plot ----------------------------------------------------
approach1test<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Model performance/approach1test.csv")
approach1test<-approach1test%>%
  na.omit()
approach2test<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Model performance/approach2test.csv")
approach2test<-approach2test%>%
  na.omit()
batch1<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/Neural Network Single Customer Model/test_predictions_150.csv")
batch2<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/Neural Network Single Customer Model/test_predictions_300.csv")
batch3<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/Neural Network Single Customer Model/test_predictions_400.csv")
batch4<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/Neural Network Single Customer Model/test_predictions_600.csv")
batch5<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/Neural Network Single Customer Model/test_predictions_847.csv")
approach3test<-rbind(batch1, batch2, batch3, batch4, batch5)
approach3test<-approach3test%>%
  na.omit()


approach1test$approach<-"Cluster"
approach2test$approach<-"FFNN"
approach3test$approach<-"Cluster+FFNN"

approach1test<-approach1test[c(4,5,6)]
approach2test<-approach2test[c(1,2,14)]
approach3test<-approach3test[c(2,4,5)]

names(approach2test)[names(approach2test) == "Actual"] <- "Consumption"
names(approach3test)[names(approach3test) == "original_consumption"] <- "Consumption"
names(approach3test)[names(approach3test) == "predictions"] <- "Predicted"


merged<-rbind(approach1test,approach2test,approach3test)



ggplot(merged, aes(x=Predicted, y=Consumption) ) +
  geom_hex(bins = 750) +
  scale_fill_continuous(type = "gradient") +
    theme_bw()+
  facet_wrap(vars(approach),nrow=1)+
  geom_smooth(method = "lm", se = FALSE,color = "black", linewidth = 0.1)+
  walbaum_font+
  labs(x = "Predicted Consumption (kWh)", y = "Actual Consumption (kWh)", fill = "Count")




# Part 6: Mean and 90 percentile comparisons on a single random day -------

approach1test<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Model performance/approach1test.csv")
approach1test<-approach1test%>%
  na.omit()
approach2test<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Model performance/approach2test.csv")
approach2test<-approach2test%>%
  na.omit()
batch1<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/Neural Network Single Customer Model/test_predictions_150.csv")
batch2<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/Neural Network Single Customer Model/test_predictions_300.csv")
batch3<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/Neural Network Single Customer Model/test_predictions_400.csv")
batch4<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/Neural Network Single Customer Model/test_predictions_600.csv")
batch5<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/Neural Network Single Customer Model/test_predictions_847.csv")
approach3test<-rbind(batch1, batch2, batch3, batch4, batch5)
approach3test<-approach3test%>%
  na.omit()


approach1test$approach<-"Cluster"
approach2test$approach<-"FFNN"
approach3test$approach<-"Cluster+FFNN"

approach1test<-approach1test[c(4,5,6,1)]
approach2test<-approach2test[c(1,2,14)]
approach2test$GMT<-approach3test$GMT
approach3test<-approach3test[c(2,4,5,3)]

names(approach2test)[names(approach2test) == "Actual"] <- "Consumption"
names(approach3test)[names(approach3test) == "original_consumption"] <- "Consumption"
names(approach3test)[names(approach3test) == "predictions"] <- "Predicted"



merged<-rbind(approach1test,approach2test,approach3test)

merged<-merged%>%
  mutate(GMT=ymd_hms(GMT))%>%
  mutate(Date=as.Date(GMT))%>%
  filter(Date=="2013-01-02")


mean_power <- merged %>% 
  group_by(approach, GMT) %>% 
  summarise(mean_power = mean(Predicted),
            upper = quantile(Predicted, 0.9),
            lower = quantile(Predicted, 0.1))

mean_power<-mean_power%>%
  mutate(hour=lubridate::hour(GMT))%>%
  mutate(minute=lubridate::minute(GMT))%>%
  mutate(halfhour=ifelse(minute>1, (2*hour)+1, 2*hour))




mean_power_plot <-mean_power %>% 
  ggplot(aes(x = halfhour, 
             y = mean_power, 
             ymin = lower,
             ymax = upper,
             fill=approach,
             colour=approach)) + 
  geom_line(show.legend = FALSE) + 
  geom_ribbon(alpha = 0.2, color = "black") +
  facet_wrap(vars(approach),nrow=1)+
  xlab("Half Hour") +                
  ylab("Mean Power (kWh)") +        
  walbaum_font+
  guides(fill = "none")


custom_colors <- c("Cluster" = "#00bfff", "Cluster+FFNN" = "#FA8072", "FFNN" = "purple")

# Apply the custom color palette using scale_color_manual()
mean_power_plot2<-mean_power_plot +
  scale_color_manual(values = custom_colors)+
  scale_fill_manual(values = custom_colors)+
  guides(fill = "none")



##pull them altogether

mean_power_plot3 <-mean_power %>% 
  ggplot(aes(x = halfhour, 
             y = mean_power, 
             ymin = lower,
             ymax = upper,
             fill=approach,
             colour=approach)) + 
  geom_line(show.legend = FALSE) + 
  geom_ribbon(alpha = 0.2, color = "black") +
  xlab("Half Hour") +                
  ylab("Mean Power (kWh)") +        
  walbaum_font

custom_colors <- c("Cluster" = "#00bfff", "Cluster+FFNN" = "#FA8072", "FFNN" = "purple")

# Apply the custom color palette using scale_color_manual()
mean_power_plot4 <-mean_power_plot3 +
  scale_color_manual(values = custom_colors)+
  scale_fill_manual(values = custom_colors)


library(gridExtra)
grid.arrange(mean_power_plot2, mean_power_plot4, ncol = 2,widths = c(2, 1.33))


# Part 5: Calculate bias ----------------------------------------------------
approach1test<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Model performance/approach1test.csv")
approach1test<-approach1test%>%
  na.omit()
approach2test<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Model performance/approach2test.csv")
approach2test<-approach2test%>%
  na.omit()
batch1<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/Neural Network Single Customer Model/test_predictions_150.csv")
batch2<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/Neural Network Single Customer Model/test_predictions_300.csv")
batch3<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/Neural Network Single Customer Model/test_predictions_400.csv")
batch4<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/Neural Network Single Customer Model/test_predictions_600.csv")
batch5<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Clustering and neural network/Neural Network Single Customer Model/test_predictions_847.csv")
approach3test<-rbind(batch1, batch2, batch3, batch4, batch5)
approach3test<-approach3test%>%
  na.omit()


approach1test$approach<-"Cluster"
approach2test$approach<-"FFNN"
approach3test$approach<-"Cluster+FFNN"

approach1test<-approach1test[c(4,5,6)]
approach2test<-approach2test[c(1,2,14)]
approach3test<-approach3test[c(2,4,5)]


mean_true1 <- mean(approach1test$Consumption)
mean_estimated1 <- mean(approach1test$Predicted)
bias1 <- mean_estimated1 - mean_true1
bias1/mean_true1*100
mean_true2 <- mean(approach2test$Actual)
mean_estimated2 <- mean(approach2test$Predicted)
bias2 <- mean_estimated2 - mean_true2
bias2/mean_true2*100
mean_true3 <- mean(approach3test$original_consumption)
mean_estimated3 <- mean(approach3test$predictions)
bias3 <- mean_estimated3 - mean_true3
bias3/mean_true3*100



# Part X Model performance ------------------------------------------------


approach3test<-approach3test%>%
  mutate(DSR=original_consumption-predictions)

approach3test<-approach3test%>%
  mutate(percent=DSR/original_consumption)

RRMSE = sqrt(sum(approach3test$DSR^2))/sqrt(sum(approach3test$original_consumption^2))

RER=approach3test$percent
RER=RER[is.finite(RER)]
meanRER <- mean(RER)


