library(tidyverse)
library(ggthemes)
library("httr")
library("readxl")
GET("https://query.data.world/s/aciilwosbzladpgbskoo7brkebaazg", write_disk(tf <- tempfile(fileext = ".xlsx")))
df <- read_excel(tf)

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
