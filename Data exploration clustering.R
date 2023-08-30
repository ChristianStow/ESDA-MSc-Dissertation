#install packages
library(tidyverse)
library(lubridate)
library(maditr)
library(forcats)

lcl<-read.csv("C:/Users/chris/OneDrive/Documents/University/Dissertation/04 Analysis and results/Data processing and cleansing/Data exploration/FINALDATASET.csv")


lcltrial<-lcl%>%
  filter(GMT < "2014-01-01 00:00:00",
         GMT >= "2013-01-01 00:00:00")

lcltrial<-lcltrial%>%
  mutate(halfhour=ifelse(lcltrial$minute>1, (2*lcltrial$hour)+1, 2*lcltrial$hour))%>%
  mutate(month = lubridate::month(GMT))


#Transform: row for each customer, column for each month/hourhour
df_transform <- lcltrial %>% 
  pivot_longer(-c(GMT, ID, X, timestamp, weekday, month, day, halfhour, hour, minute, WN),
               names_to = 'customer',
               values_to = 'kW') %>% 
  group_by(customer, month, halfhour) %>%
  summarise(mean_kW = mean(kW)) %>%
  pivot_wider(names_from = c(month, halfhour), 
              values_from = mean_kW) %>%
  ungroup()


#Normalise rows

for (i in 1:nrow(df_transform)){
  max_value <- max(df_transform[i,-1])
  min_value <- min(df_transform[i,-1])
  df_transform[i,-1] <- (df_transform[i,-1] - min(df_transform[i,-1])) / (max_value - min_value)
}



# Add treatment/control group to original time series
df_transform$group <- ifelse(grepl("D", df_transform$customer), "treatment", "control")



# Pivot the data
df_transform_pivot <- df_transform %>%
  pivot_longer(contains('_')) %>% 
  group_by(customer) %>%
  mutate(index = row_number())



# Adding weekday and hour columns in order to facet_wrap on weekday
month_halfhour <- str_split(df_transform_pivot$name, '_', simplify=TRUE)
df_transform_pivot$month <- as.factor(month_halfhour[,1])
df_transform_pivot$halfhour <- as.integer(month_halfhour[,2])



#Plot the mean consumption profiles for each group
mean_power <- df_transform_pivot %>% 
  group_by(group, month, halfhour) %>% 
  summarise(mean_power = mean(value,na.rm=TRUE),
            upper = quantile(value, 0.75,na.rm=TRUE),
            lower = quantile(value, 0.25, na.rm=TRUE))

mean_power<-mean_power%>%
  mutate(month=month.name[month])

month_order <- c("January", "February", "March", "April", "May", "June", 
                 "July", "August", "September", "October", "November", "December")

# Convert month to an ordered factor with the specified levels
mean_power$month <- factor(mean_power$month, levels = month_order, ordered = TRUE)


mean_power %>% 
  ggplot(aes(x = halfhour, 
             y = mean_power, 
             ymin = lower,
             ymax = upper,
             fill = group,
             colour = group)) + 
  geom_line() + 
  geom_ribbon(alpha = 0.2, color = NA) + 
  facet_wrap(~month) +
  labs(subtitle = 'Line shows mean power (kW)\nRibbon shows inter-quartile range')

#########################################################################################CHART FOR THE REPORT####################
library(extrafont)

walbaum_font <- theme(text = element_text(family = "Times New Roman", size=14))


# Custom colors for the "treatment" and "control" groups
custom_colors <- c("#00bfff","#FA8072")

mean_power %>%
  ggplot(aes(x = halfhour, 
             y = mean_power, 
             ymin = lower,
             ymax = upper,
             fill = group,
             colour = group)) + 
  geom_line() + 
  geom_ribbon(alpha = 0.2, color = NA) + 
  facet_wrap(~month) +
  xlab("Half Hour") +                # Change the x-axis label
  ylab("Mean Power (kWh)") +        # Change the y-axis label
  scale_fill_manual(values = custom_colors) +     # Set custom fill colors
  scale_color_manual(values = custom_colors) +    # Set custom line colors
  walbaum_font 
#########################################################################################CHART FOR THE REPORT####################





###without the quartiles
mean_power %>% 
  ggplot(aes(x = halfhour, 
             y = mean_power, 
             fill = group,
             colour = group)) + 
  geom_line() + 
  facet_wrap(~month) +
  labs(subtitle = 'Line shows mean power (kW)\nRibbon shows inter-quartile range')



