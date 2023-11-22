library(dplyr)
library(lubridate)
library(ggplot2)
library(hydroEvents)
library(readr)
library(lfstat)
library(lmom)
library(lattice)
library(lfstat)
library(tidyr)
library(reshape)

nis <- read.table("/Users/dabanto/Downloads/N_07234.txt", skip=2, header = T)


abfluss <- read_csv("/Users/dabanto/Downloads/landespegel.csv")


abfluss <- abfluss %>% 
  mutate(date = as.POSIXct(`Datum / Uhrzeit`, format= "%d/%m/%Y %H:%M"))

nis <- nis %>%
  mutate(date = make_datetime(Jahr, Monat, Tag, Stunde))

workdat<-merge(abfluss,nis,by = "date")

workdat$Stunde <- NULL

workdat$Wert <- gsub(",", ".", workdat$Wert)

workdat$Wert <- as.numeric(workdat$Wert)

workdat <- tibble::rowid_to_column(workdat, "ID")


save(workdat, file = "/Users/dabanto/Downloads/workdat.RData")

#aggreggierte tageswerte


#plot niederschlag und 
par(mfrow=c(2,1),mar=c(0,4,4,2))
plot(workdat$date,workdat$Wert,type="l",ylab="Abfluss [m3/s]",xlab="Datum",main="Stundenmittelwerte (Kinzig Schenkenzell)")
par(mar=c(4,4,0,2))
plot(workdat$date,workdat$N,type="l",col="darkblue",ylab="Niederschlag [mm]",xlab="Datum")


#nach events suchen
events.P = eventPOT(workdat$N, threshold = 15, min.diff = 1)

#plot events
plotEvents(workdat$N, dates = workdat$date, events = events.P, type = "hyet", colline = "#377EB8", colpnt = "#E41A1C",ylab = "Rainfall (mm)", xlab = 2015, main = "")



#rows extrahieren

events <- dplyr::pull(events.P, which.max)

#
rowRanges <- lapply(which(rownames(workdat) %in% events), function(x) x + c(-15:15))

filt <- lapply(rowRanges, function(x) workdat[x, ])

out <- do.call(rbind, filt)

#dataset for loop

new <-  do.call(rbind, Map(cbind, Name = seq_along(filt), filt))


p1 <- ggplot() + geom_line(aes(y = N, x = date, colour = Name),
                           data = new, stat="identity")
p1

str(new)


new$Wert <- new$Wert*1000*3600/62805000  #mm/hour


new_test <- new %>% 
  filter(Name == 4) %>% 
  select(date, Wert, N)

BFI_res <- eventBaseflow(new_test$Wert)


d = seq(
  from=min(new_test$date),
  to=max(new_test$date),
  by="hour"
)  

new_test$date <- NULL

limbs(data = new_test$Wert, dates = d, events = BFI_res)

## ausprobieren
df <- limbs(data = new_test$Wert, dates = d, events = BFI_res)



#iterate over every event to calculate baseflow

result_data <- data.frame()


for (i in 1:length(events)) {
  #i <- 1
  new_test <- new %>% 
    filter(Name == i) %>% 
    select(date, Wert, N)
  event <- xts(x = new_test, order.by = new_test$date)
  event$date <- NULL
  event$baseflow <- baseflow(event$Wert)
  df = data.frame(date=index(event), coredata(event), event_id = rep(i))
  result_data <- bind_rows(result_data, df)
  
  }
  
result_data <- result_data %>% 
  drop_na()

  
agg <- result_data %>%
  group_by(event_id) %>%
  summarise(across(c(Wert, N, baseflow), sum))

agg <- agg %>% 
  dplyr::rename(Qges = Wert) %>% 
  dplyr::rename(Qb = baseflow)


agg$Qf <- (agg$Qges-agg$Qb)

agg$RFC <- agg$Qf/agg$N
  

for (i in 1:nrow(agg)) {
  new_t <- new %>% 
    filter(Name == i) %>% 
    select(Wert, N) %>% 
    mutate(id = row_number())
  
  BFI_res <- tryCatch(eventBaseflow(new_t$Wert), error = function(e) return(NA))
  print(rep(i))
  ifelse(!is.na(BFI_res), df <- limbs(data = new_test$Wert, 
                                      dates = NULL, events = BFI_res), NA)
  
  agg$Q_event[i] <- ifelse(is.na(df), NA, df[1, "sum"])
  agg$srt_event[i] <- ifelse(is.na(df), NA, df[1, "srt"])
  agg$end_event[i] <- ifelse(is.na(df), NA, df[1, "end"])
  
  agg$sum_n[i] <- new %>%
    filter(Name == i) %>% 
    slice(agg$srt_event[i]:agg$end_event[i]) %>%
    summarise(sum_value = sum(N)) %>% 
    pull(sum_value)
  
}

agg$srt_event[3]




result <- new %>%
  filter(Name == 1) %>% 
  slice(agg$srt_event[1]:agg$end_event[1]) %>%
  summarise(sum_value = sum(N)) %>% 
  pull(sum_value)






agg$RFC_2 <- agg$Q_event/agg$sum_n


plot_q <- agg %>% 
  select(event_id, RFC, RFC_2)

plot_q <- as.data.frame(plot_q)

plot_q <- melt(plot_q, id=c("event_id"))

plot_q %>% 
  group_by(variable) %>% 
  ggplot(aes(x = event_id, y = value, group = variable, color = variable)) +
  geom_line() 




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








ggp_2 <- ggplot(plot_q)  + 
  geom_line(aes(x=event_id, y=RFC),stat="identity", fill="cyan",colour="#006000")+
  geom_line(aes(x=event_id, y=1000*RFC_2),stat="identity",color="red",size=0.5)+
  scale_x_date(date_breaks = "8 weeks",
               date_labels = "%b-%y") + 
  
  labs(title= "Geolocated COVID-19 related tweets for all of BW and mean \n  average sentiment score trend (weekly)",
       x="Year-Month",y="Geolocated COVID-19 tweets")+
  scale_y_continuous(sec.axis=sec_axis(~.*0.001,name="Average sentiment score")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size=11, hjust = 0.5),
        axis.title.y = element_text(color = temperatureColor, size=10, hjust = 0.5, vjust = 0.6),
        axis.title.y.right = element_text(color = priceColor, size=10, hjust = 0.5, vjust = 0.6))
ggp_2








