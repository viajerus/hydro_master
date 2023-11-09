library(dplyr)
library(lubridate)
library(ggplot2)
library(hydroEvents)

nis <- read.table("/home/dabanto/Downloads/N_07234.txt", skip=2, header = T)

nis <- nis %>%
  mutate(date = make_date(Jahr, Monat, Tag))


nis_sum <-
  nis %>% 
  tidyr::drop_na(date) %>%             
  mutate(day = floor_date(   
    date,
    unit = "day")) %>% 
  group_by(day)%>%
  summarize(n_sum = sum(N))  


ggplot(nis_sum, aes(x= day, y= n_sum)) +
  geom_bar() +
  xlab("") +
  ylab("# tweets") +
  labs(title = "Sentiments for covid-19 related tweets",
       subtitle = "in Baden-Württenberg")

ggp <- ggplot(nis_sum)  + 
  geom_bar(aes(x=day, y=n_sum),stat="identity", fill="cyan",colour="#006000")+
  scale_x_date(date_breaks = "24 weeks",
               date_labels = "%b-%y") + 
    labs(title= "Aggregierter Niederschlag (mm)",
       x="Jahr-Monat",y="Niederschlagssumme")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size=12, hjust = 0.5),
        axis.title.y = element_text(color = "orange", size=10, hjust = 0.5, vjust = 0.6))
ggp


events = eventPOT(nis$N, threshold = 1, min.diff = 2)

plotEvents(dataLoch, dates = NULL, events = events, type = "hyet", ylab = "Rainfall [mm]", main = "Rainfall Events (threshold = 1, min.diff = 2)")


load("/home/dabanto/Downloads/dataLoch.rda")


srt = as.Date(min(nis$date))
end = as.Date(max(nis$date))
dat = dataCatchment$`105105A`[which(dataCatchment$`105105A`$Date >= srt & dataCatchment$`105105A`$Date <= end),]

events.P = eventPOT(nis$N, threshold = 15, min.diff = 1)

plotEvents(nis$N, dates = nis$date, events = events.P, type = "hyet", colline = "#377EB8", colpnt = "#E41A1C",ylab = "Rainfall (mm)", xlab = 2015, main = "")


newfun <- function(vec, val, n) {
  rows <- sapply(which(vec==val), function(x) seq(x-n, x+n, 1))
  rows <- unique(sort(rows[rows>0 & rows<length(vec)]))
  return(vec[rows])
}



