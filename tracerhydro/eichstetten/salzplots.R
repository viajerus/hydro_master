library(ggplot2)
library(readxl)
library(tidyverse)
library(lubridate)
library(anytime)  


multi3420_1 <- read_excel("/Users/dabanto/Downloads/3420_1.xlsx")

multi3420_2 <- read_excel("/Users/dabanto/Downloads/3420_2.xlsx")


multi3420_1 <- multi3420_1 %>%
  mutate(output = 0.0005031685*(`Cond [µS/cm]`) - 0.4512743753) %>%
  mutate(Time = format(Time,"%H:%M:%S")) %>% 
  mutate(Date = anydate(Date)) %>% 
  mutate(date_time = as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M:%S"))


multi3420_2 <- multi3420_2 %>%
  mutate(output = 0.0005011327*(`Cond [µS/cm]`) - 0.4593158407) %>% 
  mutate(Time = format(Time,"%H:%M:%S")) %>% 
  mutate(Date = anydate(Date)) %>% 
  mutate(date_time = as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M:%S"))



p1 <- ggplot(multi3420_1, aes(x=date_time, y=output)) +
  geom_line() + 
  ylab("Salzkonzentation") +
  ggtitle("Durchgangskurve 3420_1")

p1

ggsave("/Users/dabanto/Desktop/master/hydro_master/tracerhydro/eichstetten/p1.png", p1,  dpi = 600)

p2 <- ggplot(multi3420_2, aes(x=date_time, y=output)) +
  geom_line() + 
  ylab("Salzkonzentation") +
  ggtitle("Durchgangskurve 3420_2")
ggsave("/Users/dabanto/Desktop/master/hydro_master/tracerhydro/eichstetten/p2.png", p2,  dpi = 600)

p2