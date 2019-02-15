---
  title: "Lab5"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
```{r}
library(tidyverse)
```

```{r, include=FALSE}
milk <- read.csv('state_milk_production.csv')
head(milk)
```

##Team Section

```{r, include=FALSE}
library(tidyverse)
library(dplyr)
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
```
```{r}
the_most <- milk %>% group_by(year, milk_million) %>% arrange(desc(milk_million))
the_least <- milk %>% group_by(year, milk_million) %>% arrange(milk_million)
```
So, we can see the year when the most milk was produced in the United States was in 2014. And the year when the least milk was produced in the United States was 2013 and 2017.

```{r}
mostIn2017 <- milk %>% filter(year == 2017) %>% arrange(desc(milk_million))
leastIn2017 <- milk %>% filter(year == 2017) %>% arrange(milk_million)
```
So, we can see that in 2017, the 5 states that produced the most milk are California, Wisconsin, New York, Idaho and Texas. 
So, we can see that in 2017, the 5 states that produced the least milk are Alaska, Rhode Island, Hawaii, Arkansas, Alabama.


##Individual

####Lauren: 1998

```{r}
milk <- milk %>%
  mutate(milk_million = milk_produced/1000000)
milk_lauren <- milk %>% filter(year==1998)

ggplot(data = milk_lauren, aes(x = milk_million)) +
  geom_density() + 
  ggtitle('Distribution estimate of milk produced in 1998 by state') +
  labs(x="Milk Produced in Millions",y="Density" )

lauren_stats <- milk_lauren %>% summarise(average = mean(milk_million), median = median(milk_million))

most_milk_lauren <- milk_lauren %>% filter(year==1998) %>% arrange(desc(milk_produced)) 
```

Mean (Millions) | Median (Millions) | Maximum | Minimum
---- | ------ | ------- | -------
  3145 | 1412   | California | Alaska 

####Ping: 1996

```{r}
milk1996 <- milk %>% filter(year == 1996)

ggplot(data = milk1996, aes(x = milk_million)) +
  geom_density() + 
  ggtitle('Distribution estimate of milk produced in 1996 by state')

mean <- milk1996 %>% summarise(Mean = mean(milk_million))
mean

median <- milk1996 %>% summarise(median = median(milk_million))
median

milk_most <- milk1996 %>% arrange(desc(milk_million)) 
milk_most 

milk_least <- milk1996 %>% arrange(milk_million)
milk_least 

```

Mean (Millions) | Median (Millions) | Maximum | Minimum
---- | ------ | ------- | -------
  3080 | 1480   | California | Alaska 

####Gregor: 1999


my_milk <- milk %>%
  filter(year == "1999") 

ggplot(my_milk, aes(x = milk_million)) + geom_histogram(bins = 30) + 
  ggtitle("Distribution of milk production in the year of 1999") + 
  xlab("Production of milk in millions") + ylab("Number of states that have produced x milk amount")


my_milk_summary <- my_milk %>%
  group_by(year) %>%
  summarise(Average_Milk_Produced = sum(milk_million) / 50, Median_Milk_Produced = median(milk_million))


top_state <- my_milk %>%
  arrange(desc(milk_million)) %>%
  as.tibble() %>%
  top_n(1) %>%
  select(state, milk_million)

bottom_state <- my_milk %>%
  arrange(desc(milk_million)) %>%
  as.tibble() %>%
  top_n(-1) %>%
  select(state, milk_million)

####Peter: 1999

```{r}
milk <- milk %>%
  mutate(milk_million = milk_produced/1000000)
milk_peter <- milk %>% filter(year==1999)

ggplot(data = milk_peter, aes(x = milk_million)) +
  geom_density() + 
  ggtitle('Distribution estimate of milk produced in 1999 by state') +
  labs(x="Milk Produced in Millions",y="Density" )

peter_stats <- milk_peter %>% summarise(average = mean(milk_million), median = median(milk_million))

most_milk_peter <- milk_peter %>% filter(year==1999) %>% arrange(desc(milk_produced)) 
```

Mean (Millions) | Median (Millions) | Maximum | Minimum
---- | ------ | ------- | -------
  3252 | 1418   | California | Alaska 

####Sasha: 2000
```{r}
milk <- milk %>%
  mutate(milk_million = milk_produced/1000000)
milk_sasha <- milk %>% filter(year==2000)

ggplot(data = milk_sasha, aes(x = milk_million)) +
  geom_density(fill= 'lightpink', color= 'thistle1') + 
  ggtitle('Distribution estimate of milk produced in 2000 by state') +
  labs(x="Milk Produced in Millions",y="Density" )

sasha_stats <- milk_sasha %>% summarise(average = mean(milk_million), median = median(milk_million))

most_milk_sasha <- milk_sasha %>% filter(year==2000) %>% arrange(desc(milk_produced)) 
```

Mean (Millions) | Median (Millions) | Maximum | Minimum
---- | ------ | ------- | -------
  3348 | 1454   | California | Alaska 

###Team Conclusion
I think plotting the graph with time series and with the states that we want to compare is a very good way
to learn about this data set. We can clearly see how different states have different amount of milk produced. 
Also, we can see how the production of milk changed every year. With the graphing of milk production versus time,
one could start making predictions about how a state's economy is changing over time. If a certain state is
known for it's milk production, and it plummits during a certain year, then that may lead to more insights on what
is going on in that state. 



