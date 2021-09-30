# MakeoverMondayViz

A few visualizations I created using ggplot in R for Data.World's Makeover Mondays. 


**3/29/2021 UK Exports to the EU**


library(tidyverse)
library(httr)
library(readxl)
library(lubridate)
library(ggthemes)

#import data 
GET("https://query.data.world/s/5x54n3of3j55xzsno6y3biyc3plzn3", write_disk(tf <- tempfile(fileext = ".xlsx")))
df <- read_excel(tf)

#explore data
summary(df)
lapply(df, class)


#transform data 

# Separate year from datetime:
df$Year <- format(df$Month, format="%Y")

dfg <- df %>%
  group_by(Month) %>%
  summarise(sum_imports_bil = sum(`Imports (£m)`) / 1000, sum_exports_bil = sum(`Exports (£m)`) / 1000) %>%
  mutate(pct_change_export = (sum_exports_bil/lag(sum_exports_bil) - 1) * 100)

b <- dfg %>%
  filter(dfg$Month >= as.Date("2020-12-01"))

# set event
d <- dfg %>%
  select(Month, pct_change_export) %>%
  filter(Month == as.Date("2020-12-01")) %>%
  mutate(event = "Brexit Formally Takes Place")


#visualize data 

ggplot(dfg, aes(x = Month, y = sum_exports_bil)) +
  geom_line(size = 1, color = "blue") +
  geom_line(data = b, aes(x = Month, y = sum_exports_bil), color = "red", size = 1) +
  labs(title = "Exports to the EU Have Plunged 43% Since Brexit", 
       subtitle = NULL, 
       x = NULL, 
       y = "£B")  +
  geom_vline(data=d, aes(xintercept=Month), color="black", linetype="dotted", size = 1) +
  geom_text(data=d, mapping=aes(x=Month, y=8, label=event), size=4, angle=90, vjust=-0.9, hjust=-1.2) +
  theme_fivethirtyeight() +
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  labs(caption="Data source: ONS") 

#visualize data (pct change)


ggplot(dfg, aes(x = Month, y = pct_change_export)) +
  geom_line(size = 1, color = "blue") +
geom_line(data = b, aes(x = Month, y = pct_change_export), color = "red", size = 1) +
  labs(title = "Exports to the EU Have Plunged 43% Since Brexit", 
       subtitle = NULL, 
       x = NULL, 
       y = "£B")  +
  geom_vline(data=d, aes(xintercept=Month), color="black", linetype="dotted", size = 1) +
  geom_text(data=d, mapping=aes(x=Month, y=8, label=event), size=4, angle=90, vjust=-0.9, hjust=2) +
  theme_fivethirtyeight() +
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  labs(caption="Data source: ONS") 
  

![euexports](https://github.com/JonFain90/MakeoverMondayViz/tree/master/3.29.2021/viz/3_39_21_viz_2.pdf)


5/3/2021
