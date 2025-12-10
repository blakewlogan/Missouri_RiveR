library(Cairo)
library(plotrix)
library(rstatix)
library(tidyverse)
library(ggplot2)
library(chisq.posthoc.test)
library(gridExtra)
library(dunn.test)
library(AICcmodavg)
library(MuMIn)
library(effects)
library(janitor)
library(rcompanion)
library(ggpubr)

data <- read.csv("gavins_pdfh.csv")



data$bin <- ifelse(data$spillway == 0, 0,
                       ifelse(data$spillway >= 0, 1))
                              
data$total <- data$powerhouse + data$spillway

ggplot(data = data, aes(x = year, y = harvest, fill = total))+
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Estimated Harvest") +
  theme(legend.title = element_blank()) +
  coord_cartesian(expand = FALSE) +
  scale_y_continuous(limits = c(0,1500), breaks = seq(0,1500,250)) +
  scale_x_continuous(limits = c(1999,2025), breaks = seq(2000,2024,1)) +
  theme_pubr()
