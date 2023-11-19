library(dplyr)
library(lubridate)
library(ggplot2)
library(hydroEvents)
library(readr)


nis <- read.table("/Users/dabanto/Downloads/N_07234.txt", skip=2, header = T)


abfluss <- read_csv("/Users/dabanto/Downloads/landespegel.csv")


abfluss <- abfluss %>% 
  mutate(date = as.POSIXct(`Datum / Uhrzeit`, format= "%d/%m/%Y %H:%M"))


workdat<-merge(abfluss,nis,by = "date")

workdat$Stunde <- NULL


nis <- nis %>%
  mutate(date = make_datetime(Jahr, Monat, Tag, Stunde))


nis_sum <-
  nis %>% 
  tidyr::drop_na(date) %>%             
  mutate(day = floor_date(   
    date,
    unit = "day")) %>% 
  group_by(day)%>%
  summarize(n_sum = sum(N))  



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

events.P = eventPOT(workdat$N, threshold = 15, min.diff = 1)

plotEvents(workdat$N, dates = workdat$date, events = events.P, type = "hyet", colline = "#377EB8", colpnt = "#E41A1C",ylab = "Rainfall (mm)", xlab = 2015, main = "")

workdat <- tibble::rowid_to_column(workdat, "ID")


events <- dplyr::pull(events.P, which.max)

rowRanges <- lapply(which(rownames(workdat) %in% events), function(x) x + c(-20:20))

filt <- lapply(rowRanges, function(x) workdat[x, ])

out <- do.call(rbind, filt)

new <-  do.call(rbind, Map(cbind, Name = seq_along(filt), filt))


p1 <- ggplot() + geom_line(aes(y = N, x = date, colour = Name),
                           data = new, stat="identity")
p1

str(new)

new$Wert <- gsub(",", ".", new$Wert)

new$Wert <- as.numeric(new$Wert)

new$Wert <- new$Wert*1000*3600/62805000  #mm/hour

new %>% 
  group_by(Name) %>% 
  ggplot(aes(x = date, y = Wert, group = Name, color = Name)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Name, ncol = 2, scales = "free")



ggplot(df, aes(x = x, y = y, color = variable2, fill = variable2)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_line(aes(group = variable2), size = 1.5, color = "red") +
  facet_wrap(~variable1, scales = "free_y") +
  labs(title = "Facet Wrap Plot with Bar Plot and Line")