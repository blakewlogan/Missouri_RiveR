#install.packages("chisq.posthoc.test")
#install.packages("gridExtra")
#install.packages("dunn.test")
#install.packages("AICcmodavg")
#install.packages("MuMIn")
#install.packages("effects")
#install.packages("janitor")
#install.packages("rcompanion")
#install.packages("ggpubr")
#install.packages("plotrix")
#install.packages("Cairo")
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

## CREATE DATAFRAME ############################################################
data <- read.csv("hoop_study.csv") %>%
  filter(Method != "512",
         Method != "525",
         Method != "534",
         Method != "562",
         Year > 1998,
         ID < 100000) %>% 
  mutate(Method = as.factor(Method),
         UID = as.character(UID))

data.all <- read.csv("hoop.net.unl.csv") %>%
  mutate(Method = as.factor(Method),
         Species = as.factor(Species))

gear_table <- data %>% 
  group_by(UID) %>% 
  summarise(count = n())


##Subset dataframes to calculate mean + sd
## 4 ft 1.5 in ####
data527 <- data %>%
  filter(Method != "514",
         Method != "516",
         Method != "522") %>% 
  mutate(Method = as.factor(Method))

mean(data527$Length)
sd(data527$Length)
median(data527$Length)
std.error(data527$Length)
shapiro.test(data527$Length)

values(data527$Year)

## 4 ft 1 in ####
data516 <- data %>%
  filter(Method != "514",
         Method != "527",
         Method != "522") %>% 
  mutate(Method = as.factor(Method))

mean(data516$Length)
sd(data516$Length)
std.error(data516$Length)
median(data516$Length)

levels(data516$Year)

## 2 ft 1.5 in ####
data522 <- data %>%
  filter(Method != "514",
         Method != "516",
         Method != "527") %>% 
  mutate(Method = as.factor(Method))

mean(data522$Length)
sd(data522$Length)
std.error(data522$Length)
median(data522$Length)
levels(data522$Year)

## 2 ft 1 in ####
data514 <- data %>%
  filter(Method != "527",
         Method != "516",
         Method != "522") %>% 
  mutate(Method = as.factor(Method))

levels(data514$Year)

mean(data514$Length)
sd(data514$Length)
median(data514$Length)
std.error(data514$Length)
shapiro_test(data514$Length)

## 3.5 ft 2 in ####
data534 <- data.all %>% 
  filter(Method == "534")

## mutate + ifelse statement ####
data.all <- bind_rows(data1,data2) %>% 
  janitor::clean_names() %>% 
  filter(season != "n/a") %>% 
  mutate(basin = as.factor(ifelse(site %in% c("Missouri","Plattsmouth"), "Platte-Missouri Confluence", "Lower Platte Basin")),
        season = as.factor(season),
        river = as.factor(river),
        reach = as.factor(reach),
        sex = as.factor(sex),
        year = as.factor(year))

write.csv(data.all, "sic.lw.compiled.csv")

## BRING IN DATAFRAME ####

data.all <- read.csv("updated.lw.csv")

data.all <- data.all %>% 
  filter(length != "n/a",
         species == "SIC") %>% 
  mutate(basin = as.factor(ifelse(site %in% c("Missouri","Plattsmouth"), "Platte-Missouri confluence", "Platte River basin"))) %>% 
  mutate(basin = as.factor(basin),
         reach = as.factor(reach),
         season = as.factor(season),
         sex = as.factor(sex),
         length = as.numeric(length))
data.all2 <- data.all %>% 
  filter(basin == "Platte River Basin")


databhc <- data.all %>% 
  filter(species == "BHC",
         length != "n/a") %>% 
  mutate(length = as.numeric(length))

#mean lengths for reach
summary_sic_length_reach <- data.all %>% 
  group_by(reach) %>% 
  summarise(mean = mean(length),
            lower_ci = mean - 1.96 * sd(length) / sqrt(length(length)),
            upper_ci = mean + 1.96 * sd(length) / sqrt(length(length)))

#mean lengths for basin
summary_sic_length_basin <- data.all %>% 
  group_by(basin) %>% 
  summarise(mean = mean(length),
            lower_ci = mean - 1.96 * sd(length) / sqrt(length(length)),
            upper_ci = mean + 1.96 * sd(length) / sqrt(length(length)))



mean(data.all$length)
sd(data.all$length)*1.96

mean(databhc$length)
(sd(databhc$length))*1.96

levels(data.all$reach)

##Cumulative distribution between basin ####
#compute ecdf for large hoop
ecdf_large <- ecdf(data$Length[data$Method == "527"])

x_values <- seq(min(data$Length), max(data$Length), length.out = 1000)

# Compute ECDF for small hoop
ecdf_small <- ecdf(data$Length[data$Method == "514"])
x_valuesb <- seq(min(data$Length), max(data$Length), length.out = 1000)

# Plot the cumulative distribution functions
Cairo::Cairo(
  10, #width
  10, #height
  file = paste("ccf.ecdf.hoop",".png", sep = ""),
  type = "png",
  bg = "white", #or transparent
  dpi = 400,
  units = "cm"
)



plot(x_values, ecdf_large(x_values), xlim = c(0,700), type = "s", col = "black", main = "",
     xlab = "Total Length (mm)", ylab = "Cumulative Distribution", lty = 1, lwd = 2)
lines(x_values, ecdf_small(x_values), type = "s", col = "black", lty = 2, lwd = 2)
legend("bottomright", legend = c("1.2 m, 38 mm mesh", "0.6 m, 25 mm mesh"), col = c("black", "black"), lty = c(1,2), lwd = 1, bty = "n")
dev.off()
##Cumulative distribution between season ####

#compute ecdf for spring
ecdf_spr <- ecdf(data.all$length[data.all$season == "Spring"])
x_values3 <- seq(min(data.all$length), max(data.all$length), length.out = 1000)

# Compute ECDF for summer
ecdf_sum <- ecdf(data.all$length[data.all$season == "Summer"])
x_values4 <- seq(min(data.all$length), max(data.all$length), length.out = 1000)

# Compute ECDF for fall
ecdf_fal <- ecdf(data.all$length[data.all$season == "Fall"])
x_values5 <- seq(min(data.all$length), max(data.all$length), length.out = 1000)

# Plot the cumulative distribution functions
plot(x_values, ecdf_spr(x_values), type = "s", col = "black", main = "",
     xlab = "Total Length (mm)", ylab = "Cumulative Distribution", lty = 7, lwd = 2)
lines(x_values, ecdf_sum(x_values), type = "s", col = "black", lty = 2,lwd = 2)
lines(x_values, ecdf_fal(x_values), type = "l", col = "black", lty = 3,lwd = 2)
legend("bottomright", legend = c("Spring", "Summer","Fall"), col = c("black", "black","black"), lty = c(1,2,3), lwd = 2, bty = "n")


####BRAXTON'S WONDERFUL IDEA ####
hoop <- ggplot(data = data, aes(x = Length, fill = Method, group = Method))+
  geom_density(alpha = 0.5)+
  labs(x = "Total Length (mm)", y = "Density") +
  theme(legend.title = element_blank()) +
  scale_y_continuous(limits = c(0,0.01)) +
  scale_x_continuous(limits = c(0,850), breaks = seq(0,800,100)) +
  coord_cartesian(expand = FALSE) +
  scale_fill_discrete(labels = c("0.6 m, 25 mm","1.2 m, 25 mm","0.6 m, 38 mm", "1.2 m, 38 mm")) +
  labs(fill = "Hoop Net Dimensions") +
  theme_pubr()

ggsave(hoop, file = "density_dist_hoop.png", dpi = 500)

ggplot(data = data.all, aes(x = Length, fill = Method, group = Method))+
  geom_density(alpha = 0.5)+
  labs(x = "Total Length (mm)", y = "Density") +
  theme_bw() +
  theme(legend.title = element_blank()) +
  coord_cartesian(expand = FALSE)

## SVCP by Basin
svcp.basin.density <- ggplot(data = data.all, aes(x = length, fill = basin, group = basin))+
  geom_density(alpha = 0.5)+
  labs(x = "Total Length (mm)", y = "Density") +
  theme(legend.title = element_blank()) +
  scale_y_continuous(limits = c(0,0.01)) +
  scale_x_continuous(limits = c(400,1000), breaks = seq(400,1000,100)) +
  scale_fill_manual(values = c("grey","grey5")) +
  coord_cartesian(expand = FALSE) +
  labs(fill = "") +
  theme_pubr()

ggsave(svcp.basin.density, file = "svcp.basin.density.png", height = 5, width = 7, dpi = 500)



#and again with facet wrap by river reach
sic_density <- ggplot(data = data.all, aes(x = length, fill = basin, group = basin))+
  geom_density(alpha = 0.5)+
  labs(x = "Total Length (mm)", y = "Density") +
  scale_x_continuous(limits = c(400, 1000), breaks = seq(400,1000,100)) +
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = c("grey","grey10")) +
  theme_pubr()+
  theme(legend.title = element_blank(), legend.position = c(0.9, 0.1), legend.justification = c(1, 0))
ggsave(sic_density, file = "sic.density.updated.png", dpi = 500)

ecdf_mo <- ecdf(data.all$length[data.all$river == "Missouri"])
ecdf_pl <- ecdf(data.all$length[data.all$river == "Platte"])
ecdf_sc <- ecdf(data.all$length[data.all$river == "Salt Creek"])
ecdf_eh <- ecdf(data.all$length[data.all$river == "Elkhorn"])
ecdf_lo <- ecdf(data.all$length[data.all$river == "Loup"])

plot(x_values, ecdf_pl(x_values), type = "l", col = "red", main = "",
     xlab = "Total Length (mm)", ylab = "Cumulative Distribution", lty = 1, lwd = 2)
lines(x_values, ecdf_mo(x_values), type = "l", col = "blue", lwd = 2)
lines(x_values, ecdf_sc(x_values), type = "l", col = "green", lwd = 2)
lines(x_values, ecdf_eh(x_values), type = "l", col = "purple", lwd = 2)
lines(x_values, ecdf_lo(x_values), type = "l", col = "orange", lwd = 2)
legend("bottomright", legend = c("Missouri","Platte","Salt Creek","Elkhorn","Loup"), col = c("blue", "red","green","purple","orange"), lwd = 2, bty = "n")

## Kruskal Wallis tests and Effect Sizes####

kruskal.test(Length ~ Method, data = data) #for river reach

kruskal.test(length ~ basin, data = data.all) #confluence vs upstream

kruskal.test(length ~ season, data = data.all) #for season

kruskal.test(length ~ sex, data = data.all)

data.all %>% 
  kruskal_effsize(length ~ reach)

data.all %>% 
  kruskal_effsize(length ~ basin)

data.all %>% 
  kruskal_effsize(length ~ season)

## Dunn's test for pairwise comparisons ####

d <- as.data.frame(dunn_test(data.all, length ~ reach, p.adjust.method = "bonferroni"))

write.csv(d, file = "dunn.length.hoop.csv", row.names = FALSE)

d1 <- d %>% 
  select(group1,group2,statistic, p.adj,p.adj.signif)
dunn_test(data2, Length ~ Method, p.adjust.method = "bonferroni")

dunn_test(data, Length ~ Method, p.adjust.method = "bonferroni")

## Effect size using Vargha and delaneys A ####

multiVDA(Length ~ Method, data=data)

multiVDA(length ~ basin, data=data.all)

multiVDA(length ~ season, data=data.all)




## KS Test for Catfish distributions ####

ks.test(data527$Length, data522$Length)

?ks.test()

0.0001*6
