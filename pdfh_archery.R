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
data <- read.csv("2024_pdfh_archery.csv")

## !!! DEFINE NUMBER OF PERMITS !!! THIS MUST BE UPDATED ####
data$PERMIT_NUMBER <- 275
totperm <- 275

## Use ifelse() statement to create effort categories based on ranges of effort
data$effort_cat <- ifelse(data$Effort >= 0 & data$Effort <= 5, "1",
                          ifelse(data$Effort >= 6 & data$Effort <= 10, "2",
                                 ifelse(data$Effort >= 11 & data$Effort <= 15, "3",
                                        ifelse(data$Effort >= 16 & data$Effort <= 20, "4",
                                               ifelse(data$Effort >= 21 & data$Effort <= 25, "5",
                                                      ifelse(data$Effort >= 26 & data$Effort <= 30, "6",
                                                             ifelse(data$Effort >= 31 & data$Effort <= 35, "7",
                                                                    ifelse(data$Effort >= 36 & data$Effort <= 40, "1", "8"))))))))

## FORMAT VARIABLES BY CONVERTING TO FACTORS, MANUALLY DEFINING LEVELS, AND CREATING LEVEL LABELS ####
data$Harvest <- factor(data$Harvest, levels = c(1,2,3,4,5), labels = c("NO_HARVEST","HARVEST_BELOW","HARVEST_SLOT","HARVEST_ABOVE","DID_NOT_FISH"))
data$Location <- factor(data$Location, levels = c(1,2,3,4), labels = c("TAILWATER","DOWNRIVER","BOTH","NO_RESPONSE"))
data$Return <- factor(data$Return, levels = c(1,2), labels = c("BEFORE_REMINDER","AFTER_REMINDER"))
data$EffortFAC <- factor(data$Effort, levels = c(1,2,3,4,5), labels = c("1-5 HOURS","6-10 HOURS","11-15 HOURS","16-20 HOURS","21-25 HOURS"))

## Create subset dataframe and then append new column to original dataframe containing sum of total days fished ####
daydf <- data[8:37]
data$EFFORT_DAYS = rowSums(daydf, na.rm = TRUE)

## !!!DATES MUST BE ADJUSTED FOR THE CURRENT CALENDAR YEAR!!! Create subset dataframe and then append new column to original dataframe containing sum of total weekend days fished
weekenddf <- data %>% 
  select(Day1,Day2,Day8,Day9,Day15,Day16,Day22,Day23,Day29,Day30)
data$WEEKEND_DAYS = rowSums(weekenddf, na.rm = TRUE)
  
## !!!DATES MUST BE ADJUSTED FOR THE CURRENT CALENDAR YEAR!!! Create subset dataframe and then append new column to original dataframe containing sum of total week days fished
weekdf <- data %>% 
  select(-Day1,-Day2,-Day8,-Day9,-Day15,-Day16,-Day22,-Day23,-Day29,-Day30) %>% 
  select(starts_with("Day"))
data$WEEK_DAYS = rowSums(weekdf, na.rm = TRUE)

## !!!DATES MUST BE ADJUSTED FOR THE CURRENT CALENDAR YEAR!!! Use ifelse() statement to create a new column indicating whether or not effort occurred on the opening weekend
data$OPEN_WEEKEND <- ifelse(data$Day1 | data$Day2, 1, 0)
data$OPEN_WEEKEND <- factor(data$OPEN_WEEKEND, levels = c(0,1), labels = c("DIDNTFISH","FISHED"))

data$openday <- ifelse(data$Day1, 1, 0)
data$openday <- factor(data$openday, levels = c(0,1), labels = c("DIDNTFISH","FISHED"))

## Create new dataframes for each tag return category (before and after reminder) ####
beforedf <- data %>% 
  filter(Return == "BEFORE_REMINDER")

afterdf <- data %>% 
  filter(Return == "AFTER_REMINDER")

## Calculate counts for "before" dataframe ####
before_counts <- table(beforedf$Harvest)
print(before_counts)
before <- as.list(before_counts)

after_counts <- table(afterdf$Harvest)
print(after_counts)
after <- as.list(after_counts)

## DEFINING VALUES FOR FINAL ANALYSES AND OUTPUT ####
numbefore <- nrow(beforedf)
numafter <- nrow(afterdf)

notrespond <- totperm - numbefore
perafterremind <- numafter/notrespond
afterdidntfish <- after$DID_NOT_FISH * notrespond / numafter
afterdidntfish <- round(afterdidntfish, digits = 0)
afternofish <- notrespond * after$NO_HARVEST / numafter
afternofish <- round(afternofish, digits = 0)
afterbelow <- notrespond * after$HARVEST_BELOW / numafter
afterbelow <- round(afterbelow, digits = 0)
afterslot <- notrespond * after$HARVEST_SLOT / numafter
afterslot <- round(afterslot, digits = 0)
afterabove <- notrespond * after$HARVEST_ABOVE / numafter
afterabove <- round(afterabove, digits = 0)
totdidntfish <- before$DID_NOT_FISH + afterdidntfish
totfished <- totperm - totdidntfish
totnofish <- before$NO_HARVEST + afternofish
totbelow <- before$HARVEST_BELOW + afterbelow
totslot <- before$HARVEST_SLOT + afterslot
totabove <- before$HARVEST_ABOVE + afterabove
totharvested <- totbelow + totslot + totabove
percentdidntfish <- totdidntfish/totfished*100
percentdidntfish <- round(percentdidntfish, digits = 1)
percentnofish <- totnofish/totperm*100
percentnofish <- round(percentnofish, digits = 1)
percentbelow <- totbelow/totperm*100
percentbelow <- round(percentbelow, digits = 1)
percentslot <- totslot/totperm*100
percentslot <- round(percentslot, digits = 1)
percentabove <- totabove/totperm*100
percentabove <- round(percentabove, digits = 1)
percentresponded <- (numbefore + numafter) / totperm * 100
percentresponded <- round(percentresponded, digits = 0)

## GENERATE DATAFRAME CONTAINING FINAL VALUES ####
output_a1 <- data.frame(totdidntfish, percentdidntfish, totnofish, percentnofish, totbelow, percentbelow, totslot, percentslot, totabove, percentabove, totharvested, percentresponded)
print(output_a1)

## !!! ATTENTION !!! MUST UPDATE FILE NAME TO CURRENT YEAR ####
#write.csv(file = "2023_pdfh_archery_a1.csv", result_a1, row.names = FALSE)

## QUESTION 2 - EFFORT ####

## Calculate values of Effort (hours)
data2 <- data
data2 <- data2 %>%
  mutate(Harvest = as.numeric(Harvest)) %>% 
  filter(Harvest < 5)

totalhoursreported <- sum(data2$Effort, na.rm = TRUE)

sumansweredq2 <- (length(data2$Effort)) - (sum(is.na(data2$Effort)))
meanhours <- totalhoursreported / sumansweredq2

estimatedtotalhours <- meanhours * totfished
meanhours <- round(meanhours, digits = 1)
totalhoursreported <- round(totalhoursreported, digits = 0)
#dataframe
output_a2 <- data.frame(totalhoursreported, meanhours, estimatedtotalhours, sumansweredq2)
print(output_a2)

## !!! ATTENTION !!! MUST UPDATE FILE NAME TO CURRENT YEAR ####
#write.csv(file = "2023_pdfh_archery_a2.csv", result_a2, row.names = FALSE)

## QUESTION A3 - LOCATION ####
location_freq <- table(data2$Location)
print(location_freq)
location_freq <- as.list(location_freq)

# Calculate number of respondents
numberrespondents <- location_freq$TAILWATER + location_freq$DOWNRIVER + location_freq$BOTH
perctailwater <- location_freq$TAILWATER / numberrespondents * 100
perctailwater <- round(perctailwater, digits = 1)
percdownriver <- location_freq$DOWNRIVER / numberrespondents * 100
percdownriver <- round(percdownriver, digits = 1)
percboth <- location_freq$BOTH / numberrespondents * 100
percboth <- round(percboth, digits = 1)

#Question 3 output
output_a3 <- data.frame(perctailwater,percdownriver,percboth,numberrespondents)
print(output_a3)

## QUESTION A4 - EFFORT DAYS ####
data4 <- data %>% 
  mutate(Harvest = as.numeric(Harvest))
data4 <- data4 %>% 
  filter(data4$Harvest < 5,
         data4$EFFORT_DAYS != 0)

sumansweredq4 <- (length(data4$EFFORT_DAYS)) - (sum(is.na(data4$EFFORT_DAYS)))
sumdays <- sum(data4$EFFORT_DAYS)
meandays <- sumdays / sumansweredq4
estimdays <- meandays * totfished
estimdays <- round(estimdays, digits = 0)
meandays <- round(meandays, digits = 1)

output_a4 <- data.frame(sumdays,meandays,estimdays,sumansweredq4)
print(output_a4)

## QUESTION 5 ####
sumweekenddays <- sum(data4$WEEKEND_DAYS)
meanwkdperangler <- sumweekenddays / sumansweredq4
meanwkdperangler <- round(meanwkdperangler, digits = 1)
estimtotwkdday <- meanwkdperangler * totfished
estimtotwkdday <- round(estimtotwkdday, digits = 0)

output_a5 <- data.frame(sumweekenddays, meanwkdperangler, estimtotwkdday, sumansweredq4)
print(output_a5)

## Question 6 ####
sumweekdays <- sum(data4$WEEK_DAYS)
meanweek <- sumweekdays / sumansweredq4
estimweek <- meanweek * totfished
estimweek <- round(estimweek, digits = 0)
meanweek <- round(meanweek, digits = 1)
output_a6 <- data.frame(sumweekdays, meanweek, estimweek, sumansweredq4)
print(output_a6)

268*0.8

## Question 7 ####
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

output_a7 <- data.frame(percfishedopenday,percdidntfishopenday,percfishedopenweek,percdidntfishopenweek, sumansweredq4)
print(output_a7)

