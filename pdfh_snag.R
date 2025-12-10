#install.packages("RODBC")
library(tidyverse)
library(RODBC)
library(Cairo)
library(plotrix)
library(rstatix)
library(chisq.posthoc.test)
library(gridExtra)
library(dunn.test)
library(MuMIn)
library(effects)
library(janitor)
library(rcompanion)
library(ggpubr)

## BRING IN DATAFRAME ####
data <- read.csv("2024_pdfh_snag.csv")

#data$LOCATIONBOX1 <- data$LocationBox1
#data$LOCATIONBOX2 <- data$LocationBox2
#data$LOCATIONBOX3 <- data$LocationBox3

## !!! DEFINE NUMBER OF PERMITS !!! THIS MUST BE UPDATED ####
totperm <- 1600

#Use dplyr to create the LOCATION column based on the conditions
df <- data %>%
  mutate(LOCATION = case_when(
    LOCATIONBOX1 == 1 & LOCATIONBOX2 == 0 & LOCATIONBOX3 == 0 ~ 1,
    LOCATIONBOX1 == 0 & LOCATIONBOX2 == 1 & LOCATIONBOX3 == 0 ~ 2,
    LOCATIONBOX1 == 1 & LOCATIONBOX2 == 1 & LOCATIONBOX3 == 0 ~ 3,
    LOCATIONBOX1 == 0 & LOCATIONBOX2 == 0 & LOCATIONBOX3 == 1 ~ 4,
    LOCATIONBOX1 == 0 & LOCATIONBOX2 == 1 & LOCATIONBOX3 == 1 ~ 5,
    LOCATIONBOX1 == 1 & LOCATIONBOX2 == 0 & LOCATIONBOX3 == 1 ~ 6,
    LOCATIONBOX1 == 1 & LOCATIONBOX2 == 1 & LOCATIONBOX3 == 1 ~ 7,
    LOCATIONBOX1 == 0 & LOCATIONBOX2 == 0 & LOCATIONBOX3 == 0 ~ 8
  ))

## FORMAT VARIABLES BY CONVERTING TO FACTORS, MANUALLY DEFINING LEVELS, AND CREATING LEVEL LABELS ####
df$HARVEST <- factor(df$HARVEST, levels = c(1,2,4,5), labels = c("NO_HARVEST","HARVEST_BELOW","HARVEST_ABOVE","DID_NOT_FISH"))
df$RETURN <- factor(df$RETURN, levels = c(1,2), labels = c("BEFORE_REMINDER","AFTER_REMINDER"))
df$EffortFAC <- factor(df$EFFORT, levels = c(1,2,3,4,5), labels = c("1-5 HOURS","6-10 HOURS","11-15 HOURS","16-20 HOURS","21-25 HOURS"))

#?case_when

#Bottom S4 Table
# Assuming you have a data frame `df` with a column `LOCATION`
df <- df %>%
  mutate(LOCATION_2 = case_when(
    LOCATION %in% c(1, 2, 3) ~ 1,
    LOCATION == 4 ~ 2,
    LOCATION %in% c(5, 6, 7) ~ 3,
    LOCATION == 8 ~ 8,
    TRUE ~ NA_real_ # Keep existing value if no condition is met
  ))
df$LOCATION_2 <- factor(df$LOCATION_2, levels = c(1,2,3,8), labels = c("TAILWATER","DOWNRIVER","BOTH","NO_RESPONSE"))
df$LOCATION <- factor(df$LOCATION, levels = c(1,2,3,4,5,6,7,8), labels = c("TAILWATER_BANK","TAILWATER_BOAT","TAILWATER_BANK_AND_BOAT","DOWNRIVER_BOAT","TAILWATER_BOAT_AND_DOWNRIVER_BOAT","TAILWATER_BANK_AND_DOWNRIVER_BOAT","ALL","NO_RESPONSE"))
## Creating released categories
df$BELOW_CAT <- ifelse(df$BELOW == 0, 0,
                       ifelse(df$BELOW == 1, 1,
                              ifelse(df$BELOW == 2, 2,
                                     ifelse(df$BELOW == 3, 3,
                                            ifelse(df$BELOW == 4, 4,
                                                   ifelse(df$BELOW >= 5 & df$BELOW <= 9, 5,
                                                          ifelse(df$BELOW >= 10 & df$BELOW <= 14, 6,
                                                                 ifelse(df$BELOW >= 15 & df$BELOW <= 19, 7,
                                                                        ifelse(df$BELOW > 20, 8, NA)))))))))
df <- df %>%
  mutate(IN_CAT = case_when(
    IN == 0 ~ 0,
    IN == 1 ~ 1,
    IN == 2 ~ 2,
    IN == 3 ~ 3,
    IN == 4 ~ 4,
    IN %in% 5:9 ~ 5,
    IN %in% 10:14 ~ 6,
    IN %in% 15:19 ~ 7,
    IN > 20 ~ 8
  ))

df$ABOVE_CAT <- ifelse(df$ABOVE == 0, 0,
                       ifelse(df$ABOVE == 1, 1,
                              ifelse(df$ABOVE == 2, 2,
                                     ifelse(df$ABOVE == 3, 3,
                                            ifelse(df$ABOVE == 4, 4,
                                                   ifelse(df$ABOVE >= 5 & df$ABOVE <= 9, 5,
                                                          ifelse(df$ABOVE >= 10 & df$ABOVE <= 14, 6,
                                                                 ifelse(df$ABOVE >= 15 & df$ABOVE <= 19, 7,
                                                                        ifelse(df$ABOVE > 20, 8, NA)))))))))

df$BELOW_CAT <- factor(df$BELOW_CAT, levels = c(1,2,3,4,5,6,7,8), labels = c("1", "2", "3", "4", "5", "6", "7", "8"))
df$ABOVE_CAT <- factor(df$ABOVE_CAT, levels = c(1,2,3,4,5,6,7,8), labels = c("1", "2", "3", "4", "5", "6", "7", "8"))
df$IN_CAT <- factor(df$IN_CAT, levels = c(1,2,3,4,5,6,7,8), labels = c("1", "2", "3", "4", "5", "6", "7", "8"))

## Creating effort categories
df$EFFORT_HOURS_CAT <- case_when(
  df$EFFORT >= 1 & df$EFFORT <= 5 ~ 1,
  df$EFFORT >= 6 & df$EFFORT <= 10 ~ 2,
  df$EFFORT >= 11 & df$EFFORT <= 15 ~ 3,
  df$EFFORT >= 16 & df$EFFORT <= 20 ~ 4,
  df$EFFORT >= 21 & df$EFFORT <= 25 ~ 5,
  df$EFFORT >= 26 & df$EFFORT <= 30 ~ 6,
  df$EFFORT >= 31 & df$EFFORT <= 40 ~ 7,
  df$EFFORT >= 41 & df$EFFORT <= 50 ~ 8,
  df$EFFORT >= 51 & df$EFFORT <= 100 ~ 9
)

## Create subset dataframe and then append new column to original dataframe containing sum of total days fished ####
daydf <- df[13:43]
df$EFFORT_DAYS = rowSums(daydf, na.rm = TRUE)

## !!!DATES MUST BE ADJUSTED FOR THE CURRENT CALENDAR YEAR!!! Create subset dataframe and then append new column to original dataframe containing sum of total weekend days fished
weekenddf <- df %>% 
  select(OCTOBER5, OCTOBER6, OCTOBER12, OCTOBER13, OCTOBER19, OCTOBER20, OCTOBER26, OCTOBER27)
df$WEEKEND_DAYS = rowSums(weekenddf, na.rm = TRUE)

## !!!DATES MUST BE ADJUSTED FOR THE CURRENT CALENDAR YEAR!!! Create subset dataframe and then append new column to original dataframe containing sum of total week days fished
weekdf <- df %>% 
  select(starts_with("OCTOBER")) %>% 
  select(-OCTOBER5, -OCTOBER6, -OCTOBER12, -OCTOBER13, -OCTOBER19, -OCTOBER20, -OCTOBER26, -OCTOBER27) 
  
df$WEEK_DAYS = rowSums(weekdf, na.rm = TRUE)

## !!!DATES MUST BE ADJUSTED FOR THE CURRENT CALENDAR YEAR!!! Use ifelse() statement to create a new column indicating whether or not effort occurred on the opening weekend
df$OPEN_WEEKEND <- ifelse(df$OCTOBER5, 1, 0)
df$OPEN_WEEKEND <- factor(df$OPEN_WEEKEND, levels = c(0,1), labels = c("DIDNTFISH","FISHED"))

df$openday <- ifelse(df$OCTOBER1, 1, 0)
df$openday <- factor(df$openday, levels = c(0,1), labels = c("DIDNTFISH","FISHED"))

## Create new dataframes for each tag return category (before and after reminder) ####
beforedf <- df %>% 
  filter(RETURN == "BEFORE_REMINDER")

afterdf <- df %>% 
  filter(RETURN == "AFTER_REMINDER")

## Calculate counts for "before" dataframe ####
before_counts <- table(beforedf$HARVEST)
print(before_counts)
before <- as.list(before_counts)

after_counts <- table(afterdf$HARVEST)
print(after_counts)
after <- as.list(after_counts)

## DEFINING VALUES FOR FINAL ANALYSES AND OUTPUT ####
numbefore <- nrow(beforedf)
numafter <- nrow(afterdf)

notrespond <- totperm - numbefore
perafterremind <- numafter/notrespond
afterdidntfish <- after$DID_NOT_FISH * notrespond / numafter
afternofish <- notrespond * after$NO_HARVEST / numafter
afterbelow <- notrespond * after$HARVEST_BELOW / numafter
afterabove <- notrespond * after$HARVEST_ABOVE / numafter
totdidntfish <- before$DID_NOT_FISH + afterdidntfish
totfished <- totperm - totdidntfish
totnofish <- before$NO_HARVEST + afternofish
totbelow <- before$HARVEST_BELOW + afterbelow
totabove <- before$HARVEST_ABOVE + afterabove
totharvested <- totbelow + totabove
percentdidntfish <- totdidntfish/totfished*100
percentnofish <- totnofish/totperm*100
percentbelow <- totbelow/totperm*100
percentabove <- totabove/totperm*100
percentresponded <- (numbefore + numafter) / totperm * 100

totdidntfish <- round(totdidntfish, digits = 0)
totnofish <- round(totnofish, digits = 0)
totbelow <- round(totbelow, digits = 0)
totabove <- round(totabove, digits = 0)
afterdidntfish <- round(afterdidntfish, digits = 0)
afternofish <- round(afternofish, digits = 0)
afterbelow <- round(afterbelow, digits = 0)
afterabove <- round(afterabove, digits = 0)
percentdidntfish <- round(percentdidntfish, digits = 1)
percentnofish <- round(percentnofish, digits = 1)
percentbelow <- round(percentbelow, digits = 1)
percentabove <- round(percentabove, digits = 1)
percentresponded <- round(percentresponded, digits = 0)
## GENERATE DATAFRAME CONTAINING FINAL VALUES ####
output_a1 <- data.frame(totdidntfish, percentdidntfish, totnofish, percentnofish, totbelow, percentbelow, totabove, percentabove, totharvested, percentresponded)
print(output_a1)

## QUESTION 2 BIG ASS TABLE ####
data2 <- df
data2 <- data2 %>%
  mutate(Harvest = as.factor(HARVEST)) %>% 
  filter(Harvest != "DID_NOT_FISH") %>% 
  mutate(BELOW_CAT = ifelse(is.na(BELOW_CAT), 0, BELOW_CAT)) %>% 
  mutate(IN_CAT = ifelse(is.na(IN_CAT), 0, IN_CAT)) %>% 
  mutate(ABOVE_CAT = ifelse(is.na(ABOVE_CAT), 0, ABOVE_CAT))

relbelow <- table(factor(data2$BELOW_CAT, levels = 0:8))
print(relbelow)
relin <- table(factor(data2$IN_CAT, levels = 0:8))
print(relin)
relabove <- table(factor(data2$ABOVE_CAT, levels = 0:8))
print(relabove)

bel <- as.list(relbelow)
ins <- as.list(relin)
abo <- as.list(relabove)
numres <- nrow(data2)

relbelow2 <- table(data2$BELOW)
print(relbelow2)
relbelow2 <- as.data.frame(relbelow2) %>% 
  mutate(Var1 = as.character(Var1)) %>% 
  filter(Var1 != "0") %>% 
  mutate(Var1 = as.numeric(Var1))

relin2 <- table(data2$IN)
print(relin2)
relin2 <- as.data.frame(relin2) %>% 
  mutate(Var1 = as.character(Var1)) %>% 
  filter(Var1 != "0") %>% 
  mutate(Var1 = as.numeric(Var1))

relabove2 <- table(data2$ABOVE)
print(relabove2)
relabove2 <- as.data.frame(relabove2) %>% 
  mutate(Var1 = as.character(Var1)) %>% 
  filter(Var1 != "0") %>% 
  mutate(Var1 = as.numeric(Var1))

relbelow2$val <- relbelow2$Var1 * relbelow2$Freq
beltotal <- sum(relbelow2$val)

relin2$val <- relin2$Var1 * relin2$Freq
intotal <- sum(relin2$val)

relabove2$val <- relabove2$Var1 * relabove2$Freq
abovetotal <- sum(relabove2$val)

below_expanded <- beltotal * totfished / numres
in_expanded <- intotal * totfished / numres
above_expanded <- abovetotal * totfished / numres
annual_total <- below_expanded + in_expanded + above_expanded

below_expanded <- round(below_expanded, digits = 0)
in_expanded <- round(in_expanded, digits = 0)
above_expanded <- round(above_expanded, digits = 0)
annual_total <- round(annual_total, digits = 0)

print(relbelow)
print(relin)
print(relabove)
output_a2 <- data.frame(below_expanded, in_expanded, above_expanded, annual_total)
print(output_a2)
## QUESTION 3 EFFORT HOURS
## Calculate values of Effort (hours)

totalhoursreported <- sum(data2$EFFORT, na.rm = TRUE)

sumansweredq2 <- (length(data2$EFFORT)) - (sum(is.na(data2$EFFORT)))
meanhours <- totalhoursreported / sumansweredq2

estimatedtotalhours <- meanhours * totfished
meanhours <- round(meanhours, digits = 1)
totalhoursreported <- round(totalhoursreported, digits = 0)
#dataframe
output_a3 <- data.frame(totalhoursreported, meanhours, estimatedtotalhours, sumansweredq2)
print(output_a3)

## Question 4 LOCATION ####
location_freq <- table(data2$LOCATION_2)
print(location_freq)
location_freq <- as.list(location_freq)

loc_fr <- table(data2$LOCATION)
print(loc_fr)
loc_fr <- as.list(loc_fr)
# Calculate number of respondents
numberrespondents <- location_freq$TAILWATER + location_freq$DOWNRIVER + location_freq$BOTH
perctailwater <- location_freq$TAILWATER / numberrespondents * 100
perctailwater <- round(perctailwater, digits = 1)
percdownriver <- location_freq$DOWNRIVER / numberrespondents * 100
percdownriver <- round(percdownriver, digits = 1)
percboth <- location_freq$BOTH / numberrespondents * 100
percboth <- round(percboth, digits = 1)
percresp <- numberrespondents/totperm*100
percresp <- round(percresp, digits = 1)

twbank <- loc_fr$TAILWATER_BANK / numberrespondents * 100
twbank <- round(twbank, digits = 1)
twboat <- loc_fr$TAILWATER_BOAT / numberrespondents * 100
twboat <- round(twboat, digits = 1)
twboth <- loc_fr$TAILWATER_BANK_AND_BOAT / numberrespondents * 100
twboth <- round(twboth, digits = 1)
drboat <- loc_fr$DOWNRIVER_BOAT / numberrespondents * 100
drboat <- round(drboat, digits = 1)
twbodrbo <- loc_fr$TAILWATER_BOAT_AND_DOWNRIVER_BOAT / numberrespondents * 100
twbodrbo <- round(twbodrbo, digits = 1)
twbadrbo <- loc_fr$TAILWATER_BANK_AND_DOWNRIVER_BOAT / numberrespondents * 100
twbadrbo <- round(twbadrbo, digits = 1)
all <- loc_fr$ALL / numberrespondents * 100
all <- round(all, digits = 1)

#Question 4 output 1
output_a41 <- data.frame(perctailwater,percdownriver,percboth,percresp)
print(output_a41)
#Question 4 output 2
output_a42 <- data.frame(twbank, twboat, twboth, drboat, twbodrbo, twbadrbo, all)
print(output_a42)

## QUESTION A5 - EFFORT DAYS ####
#data4 <- df %>% 
  #mutate(Harvest = as.numeric(Harvest))
#data4 <- data4 %>% 
  #filter(data4$Harvest < 5,
         #data4$EFFORT_DAYS != 0)
data4 <- data2

sumansweredq4 <- (length(data4$EFFORT_DAYS)) - (sum(is.na(data4$EFFORT_DAYS)))
sumdays <- sum(data4$EFFORT_DAYS)
meandays <- sumdays / sumansweredq4
estimdays <- meandays * totfished
estimdays <- round(estimdays, digits = 0)
meandays <- round(meandays, digits = 1)

output_a5 <- data.frame(sumdays,meandays,estimdays,sumansweredq4)
print(output_a5)

## QUESTION 6 ####
sumweekenddays <- sum(data4$WEEKEND_DAYS)
meanwkdperangler <- sumweekenddays / sumansweredq4
estimtotwkdday <- meanwkdperangler * totfished
estimtotwkdday <- round(estimtotwkdday, digits = 0)
meanwkdperangler <- round(meanwkdperangler, digits = 2)

output_a6 <- data.frame(sumweekenddays, meanwkdperangler, estimtotwkdday, sumansweredq4)
print(output_a6)

## Question 7 ####
sumweekdays <- sum(data4$WEEK_DAYS)
meanweek <- sumweekdays / sumansweredq4
estimweek <- meanweek * totfished
estimweek <- round(estimweek, digits = 0)
meanweek <- round(meanweek, digits = 2)
output_a7 <- data.frame(sumweekdays, meanweek, estimweek, sumansweredq4)
print(output_a7)

## Questions 8 & 9 ####

freqopenday <- table(data4$openday)
print(freqopenday)
freqopenday <- as.list(freqopenday)

percfishedopenday <- (freqopenday$FISHED) / sumansweredq4 * 100
percfishedopenday <- round(percfishedopenday, digits = 1)

percdidntfishopenday <- (freqopenday$DIDNTFISH) / sumansweredq4 * 100
percdidntfishopenday <- round(percdidntfishopenday, digits = 1)

freqopenweek <- table(data4$OPEN_WEEKEND)
print(freqopenweek)
freqopenweek <- as.list(freqopenweek)

percfishedopenweek <- (freqopenweek$FISHED) / sumansweredq4 * 100
percfishedopenweek <- round(percfishedopenweek, digits = 1)

percdidntfishopenweek <- (freqopenweek$DIDNTFISH) / sumansweredq4 * 100
percdidntfishopenweek <- round(percdidntfishopenweek, digits = 1)

percresponding <- sumansweredq4/totperm*100
percresponding <- round(percresponding, digits = 1)

output_a8 <- data.frame(percfishedopenday,percdidntfishopenday,percresponding)
print(output_a8)

output_a9 <- data.frame(percfishedopenweek,percdidntfishopenweek,percresponding)
print(output_a9)

## Plotting ####

data2 <- data2 %>% 
  mutate(released = BELOW_CAT + IN_CAT + ABOVE_CAT)

# Num Paddlefish Released by effort hours 
ggplot(data = data2, aes(x = EFFORT, y = released)) +
  geom_point(size = 3.51) +
  theme_pubr() +
  scale_y_continuous(limits = c(0,15), breaks = seq(0,15,1)) +
  scale_x_continuous(limits = c(0,50), breaks = seq(0,50,10)) +
  labs(x = "Snagging Effort (hours)", y = "Number of Paddlefish Released") +
  theme(axis.title = element_text(size = 14),  # Increase font size of axis titles
        axis.text = element_text(size = 12))


