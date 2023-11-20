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

save(workdat, file = "/Users/dabanto/Downloads/workdat.RData")

#aggreggierte tageswerte

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


#nach events suchen
events.P = eventPOT(workdat$N, threshold = 15, min.diff = 1)

plotEvents(workdat$N, dates = workdat$date, events = events.P, type = "hyet", colline = "#377EB8", colpnt = "#E41A1C",ylab = "Rainfall (mm)", xlab = 2015, main = "")

workdat <- tibble::rowid_to_column(workdat, "ID")

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

new$Wert <- gsub(",", ".", new$Wert)

new$Wert <- as.numeric(new$Wert)

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
  rename(Qges = Wert) %>% 
  rename(Qb = baseflow)


agg$Qf <- (agg$Qges-agg$Qb)

agg$RFC <- agg$Qf/agg$N
  

for (i in 1:nrow(agg)) {
  new_t <- new %>% 
    filter(Name == i) %>% 
    select(Wert, N) %>% 
    mutate(id = row_number())
  
  BFI_res <- tryCatch(eventBaseflow(new_t$Wert), error = function(e) NA)
  print(rep(i))
  ifelse(!is.na(BFI_res), df <- limbs(data = new_test$Wert, dates = NULL, events = BFI_res), NA)
  
  agg$Q_event[i] <- ifelse(is.na(df), NA, df[1, "sum"])
  agg$srt_event[i] <- ifelse(is.na(df), NA, df[1, "srt"])
  agg$end_event[i] <- ifelse(is.na(df), NA, df[1, "end"])
  
}


agg$RFC_2 <- agg$Q_event/agg$N


plot_q <- agg %>% 
  select(event_id, RFC, RFC_2)

plot_q <- as.data.frame(plot_q)

plot_q <- melt(plot_q, id=c("event_id"))


plot_q %>% 
  group_by(variable) %>% 
  ggplot(aes(x = event_id, y = value, group = variable, color = variable)) +
  geom_point() +
  geom_line() +
  facet_wrap(~variable, nrow = 2, scales = "free")




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