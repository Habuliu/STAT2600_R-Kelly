library(tidyverse)
library(dplyr)
milk <- read.csv('milk_data.txt')
milk <- milk %>%
  mutate(milk_million = milk_produced/1000000)

milksub <- milk %>%
  filter(state %in% c('New York','California','Oregon','Colorado','Texas')) %>%
  select(state, year, milk_million)
avgline <- milk %>%
  group_by(year) %>%
  summarise(
    n = n(),
    mean = mean(milk_million, na.rm = TRUE)
  ) 
milkperyear <- milk %>% 
  group_by(year) %>%
  summarise(
    n = n(),
    sum = sum(milk_million, na.rm = TRUE)
  ) 

c <- ggplot(data = milksub, aes(x = year, y = milk_million, color = state)) +
  geom_point() + 
  geom_point(data = avgline, aes(avgline$year, avgline$mean), color = 'black')+

  ggtitle('Pounds of milk over time by state') +
  xlab('Year') +
  ylab('Milk Produced (Million lb)')



print(c)