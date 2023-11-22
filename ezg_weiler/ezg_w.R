library(dplyr)
library(ggplot2)
library(tidyr)
library(mapview)
library(readr)
library(lubridate)
library(smplot2)
library(ggstatsplot)
library("ggpubr")
library(gtsummary)
library(cowplot)
library(grid)
library(gridExtra)
library(reshape)
library(patchwork)
library(gt)

df_neckar <- read.table("/Users/dabanto/Downloads/2023-11-13_12-55/6335600_Q_Day.Cmd.txt", header = T)

diclofenac <- read_csv("/Users/dabanto/Downloads/Daten_der_Chemie_Messstellen_2023_11_13.csv")

calcium <- read_csv("/Users/dabanto/Downloads/Daten_der_Chemie_Messstellen_2023_11_13(1).csv")

#set as date

df_neckar <- df_neckar %>% 
  dplyr::rename(Datum = YYYY.MM.DD.hh.mm.) %>%
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



werte <- ggplot() +
   
  geom_point(data = calcium, aes(x = date, y = Messwert), color = "#56B4E9", size=0.8) + # Divide by 10 to get the same range than the temperature
  geom_point(data = diclofenac, aes(x = date, y = 1000*Messwert), color = "#E69F00", size=0.8) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "First Axis",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*0.001 , name="Second Axis")
  )



big_ds <- merge(x = df_neckar, y = diclofenac[ , c("date", "Messwert")], by = "date", all.x = TRUE)

big_ds <- big_ds %>% 
  dplyr::rename(Messwert_diclofenac = Messwert)

big_ds$Value <- NULL

big_ds <- merge(x = big_ds, y = calcium[ , c("date", "Messwert")], by = "date", all.x = TRUE)

big_ds <- big_ds %>% 
  dplyr::rename(Messwert_calcium = Messwert)


mdata <- melt(big_ds, id=c("date"))


panel_labels <- c("Messwert_diclofenac" = "Diclofenac µg/l", "Messwert_calcium" = "Calcium mg/l")

p1 <- ggplot(mdata, aes(date, value)) + 
  geom_point() + 
  ylab ("Messwerte") +
  facet_wrap(~variable, scales = "free_y", ncol = 1, labeller = labeller(variable = panel_labels)) +
  theme(axis.title.x=element_blank())
p1

p2 <- ggplot(df_neckar, aes(date, Value)) + 
  xlab("Datum") + ylab ("Mittlerer Tagesabfluss (Q)") +
  geom_line() 

combined_plot <- p1 / p2
combined_plot


neck_calc <- merge(x = df_neckar, y = calcium[ , c("date", "Messwert")], by = "date", all.x = TRUE)

neck_calc <- neck_calc %>% 
  rename(Calcium_w = Messwert)

neck_calc$log_q <- log(neck_calc$Value)


ggscatter(neck_calc, x = "log_q", y = "Calcium_w",
          color = "blue", cor.coef = TRUE,
          cor.method = "pearson",
          xlab = "Abfluss", ylab = "Konzentration")


model <- lm(log_q~Calcium_w, data=neck_calc)
summary(model)  

x <- tbl_regression( 
  model, 
  pvalue_fun = function(x) {
    if_else(
      is.na(x), 
      NA_character_,
      if_else(x < 0.001, format(x, digits = 3, scientific = TRUE), format(round(x, 3), scientific = T))
    )
  },
  estimate_fun = function(x) {
    if_else(
    is.na(x), 
    NA_character_,
    if_else(x < 0.001, format(x, digits = 3, scientific = TRUE), format(round(x, 3), scientific = T)),
    )
  } 
)

add_glance_ex1 <-
  x %>%
  add_glance_table(include = c(nobs, r.squared))

add_glance_ex1

### now for diclfenac


neck_dic <- merge(x = df_neckar, y = diclofenac[ , c("date", "Messwert")], by = "date", all.x = TRUE)

neck_dic <- neck_dic %>% 
  rename(Diclofenac_w = Messwert)

neck_dic$log_q <- log(neck_calc$Value)


ggscatter(neck_dic, x = "log_q", y = "Diclofenac_w",
          color = "blue", cor.coef = TRUE,
          cor.method = "pearson",
          xlab = "Abfluss", ylab = "Konzentration")


model <- lm(log_q~Diclofenac_w, data=neck_dic)
summary(model)  

x <- tbl_regression( 
  model, 
  pvalue_fun = function(x) {
    if_else(
      is.na(x), 
      NA_character_,
      if_else(x < 0.001, format(x, digits = 3, scientific = TRUE), format(round(x, 3), scientific = T))
    )
  },
  estimate_fun = function(x) {
    if_else(
      is.na(x), 
      NA_character_,
      if_else(x < 0.001, format(x, digits = 3, scientific = TRUE), format(round(x, 3), scientific = T)),
    )
  } 
)

add_glance_ex1 <-
  x %>%
  add_glance_table(include = c(nobs, r.squared))

add_glance_ex1




  
  