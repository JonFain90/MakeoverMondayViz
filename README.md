# MakeoverMondayViz

A few visualizations I created using ggplot in R for Data.World's Makeover Mondays. 


**3/29/2021 UK Exports to the EU**

**packages**

```{r }

library(tidyverse)
library(httr)
library(readxl)
library(lubridate)
library(ggthemes)

```

**import data** 

```{r }

GET("https://query.data.world/s/5x54n3of3j55xzsno6y3biyc3plzn3", write_disk(tf <- tempfile(fileext = ".xlsx")))
df <- read_excel(tf)

```

**explore data**

```{r }


summary(df)
lapply(df, class)

```

**transform data**

```{r }

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

```

**visualize data**

```{r }

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

```

**visualize data (pct change)**

```{r }

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

```

![eu exports](https://github.com/JonFain90/MakeoverMondayViz/blob/master/3.29.2021/viz/3_39_21_viz_21024_1.jpg)



**5/3/2021 Compensation Gap**

**packages**

```{r }

library(tidyverse)
library(ggthemes)
library("httr")
library("readxl")

```

**import data**

```{r }

GET("https://query.data.world/s/aciilwosbzladpgbskoo7brkebaazg", write_disk(tf <- tempfile(fileext = ".xlsx")))
df <- read_excel(tf)

```


**transform and prepare data**

```{r }

df <- df %>%
  rename("Realized" = "Realized CEO compensation", 
         "Granted" = "Granted CEO compensation")

#set year for geom_text
y <- df %>%
  filter(Year %in% c("1965", "2019"))
  

# set event 1
d1 <- df %>%
  filter(Year == "1965") %>% 
  mutate(event = "CEOs earned 21 times as much \n as the average worker in 1965")

#set event 2
d2 <- df %>%
  filter(Year == "2019") %>% 
  mutate(event = "CEOs earned 320 times as much \n as the average worker in 2019")


#set narrative 
d3 <- df %>%
  filter(Year == "1965") %>% 
  mutate(event = 
  " The ratio between CEO and worker compensation 
  has increased 1400% since 1965. Increases in 
  'Realized' compensation represent the valuation 
  of stock, as opposed to 'Granted', which represents 
  the value of stock options when they were granted.")

```

**visualize data**

```{r }

ggplot() +
  geom_line(data = df, aes(x = Year, y = Realized, color = "blue"), size = 1) +
  geom_line(data = df, aes(x = Year, y = Granted, color = "red"), size = 1) +
  ylim(0, 450) +
  scale_x_continuous(breaks = seq(min(df$Year), max(df$Year), by = 10))+
  labs(title = "The Compensation Gap Between CEOs and Workers Has Grown Exponentially", 
       x = NULL, 
       y = "CEO to Worker Compensation Ratio")  +
  geom_text(data = y, aes(x = Year, y = Realized, label = Realized),fontface = "bold",  position = position_dodge(0.9),
            vjust = 0) +
  geom_label(data=d1, mapping=aes(x=Year, y= 50, label= event) ,label.size = NA,  size=4, angle=0, vjust=0, hjust=.1) +
  geom_label(data=d2, mapping=aes(x=Year, y= 380, label= event) , label.size = NA, fontface= "bold", size=4, angle=0, vjust=0, hjust=.9) +
  geom_label(data=d3, mapping=aes(x=Year, y= 275, label= event) , label.size = NA, size=4, angle=0, vjust=0, hjust=.1) +
  scale_color_manual(values=c("blue", "grey"),name = "Compensation", labels = c("Realized", "Granted") ) +
    labs(caption="Data source: Economic Policy Institute") +
  theme_classic() +
  theme(legend.position = c(0.8, 0.2)) 

```

![comp gap](https://github.com/JonFain90/MakeoverMondayViz/blob/master/5.3.2021/viz/5.s%20mm%20viz.jpg)
