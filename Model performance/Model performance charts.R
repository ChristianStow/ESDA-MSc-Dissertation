library(tidyverse)
library(lubridate)

LSTM<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/DR participation/LSTMchart.csv")
NN<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/DR participation/NNchart.csv")
cluster<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/DR participation/Clusterchart.csv")


LSTMwider<-LSTM%>%
  pivot_wider(names_from = Consumption, 
              values_from = mean_power)

Combined<-LSTMwider
Combined$FFNN<-NN$mean_power
Combined$Cluster<-cluster$mean_power

names(Combined)[4] <- "LSTM"
names(Combined)[3] <- "Actual"


Combined_longer <- Combined %>%
  pivot_longer(cols=c('Actual','LSTM', 'FFNN', 'Cluster'),
               names_to='Consumption',
               values_to='mean_kw')

library(extrafont)
walbaum_font <- theme(text = element_text(family = "Times New Roman", size=14))

Combined_longer %>% 
  ggplot(aes(x = halfhour, 
             y = mean_kw, 
             fill = Consumption,
             colour = Consumption)) + 
  geom_line() + 
  facet_wrap(~Date)+
  xlab("Half Hour") +                
  ylab("Mean Power (kWh)") +        
  walbaum_font







