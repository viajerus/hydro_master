library(dplyr)
library(ggplot2)
library(tidyr)
library(mapview)
library(readr)
library(lubridate)


df_neckar <- read.table("/Users/dabanto/Downloads/2023-11-13_12-55/6335600_Q_Day.Cmd.txt", header = T)

diclofenac <- read_csv("/Users/dabanto/Downloads/Daten_der_Chemie_Messstellen_2023_11_13.csv")

calcium <- read_csv("/Users/dabanto/Downloads/Daten_der_Chemie_Messstellen_2023_11_13(1).csv")

#set as date

df_neckar <- df_neckar %>% 
  rename(Datum = YYYY.MM.DD.hh.mm.) %>%
  mutate(date = ymd(Datum)) %>% 
  select(-Datum)


diclofenac <- diclofenac %>% 
  mutate(date = as.Date(Datum, format="%d.%m.%Y %H:%M"))

calcium <- calcium %>% 
  mutate(date = as.Date(Datum, format="%d.%m.%Y %H:%M"))

#extract data
df_neckar <- df_neckar %>%
  filter(between(date, as.Date('2010-01-01'), as.Date('2022-12-31')))

diclofenac$Messwert <- gsub(",", ".", diclofenac$Messwert)

diclofenac$Messwert <- as.numeric(diclofenac$Messwert)

#plot fluss

ggp <- ggplot(df_neckar)  + 
  
  geom_line(aes(x=date, y=Value),stat="identity", size=0.2)+
  scale_x_date(date_breaks = "32 weeks",
               date_labels = "%b-%y") + 
    labs(title= "Mittlerer Tagesabfluss Q",
       x="Datum",y="Q")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size=12, hjust = 0.5),
        legend.position = "none")
ggp

#water quality

coeff <- 10000



ggplot() +
   
  geom_point(data = calcium, aes(x = date, y = Messwert), color = "#56B4E9", size=0.8) + # Divide by 10 to get the same range than the temperature
  geom_point(data = diclofenac, aes(x = date, y = 1000*Messwert), color = "#E69F00", size=0.8) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "First Axis",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*0.001 , name="Second Axis")
  )



ggplot(calcium) +
  geom_point(aes(x = date, y = Messwert), color = "blue") 

  
  
  
  
  