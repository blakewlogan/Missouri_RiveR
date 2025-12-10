library(tidyverse)
library(dplyr)
library(ggplot2)
library(chisq.posthoc.test)
library(gridExtra)
library(dunn.test)
library(AICcmodavg)
library(MuMIn)
library(effects)
library(ggpmisc)
library(rcompanion)
library(officer)
library(flextable)
library(ggpubr)
?ggpmisc
#install.packages("officer")
#install.packages("flextable")
#install.packages("ggpmisc")
#install.packages("effects")
#install.packages("MuMIn")
#install.packages("AICcmodavg")
#install.packages("dunn.test")
#install.packages("gridExtra")
#install.packages("devtools")
#devtools::install_github("ebbertd/chisq.posthoc.test")

data.age <- read.csv("final.age.data.csv") %>% 
  mutate(reach = as.factor(reach)) %>% 
  filter(final_age != "x",
         species == "SIC") %>% 
  mutate(final_age = as.numeric(final_age))

summary_sic_age_basin <- data.age %>% 
  group_by(basin) %>% 
  summarise(mean = mean(length),
            lower_ci = mean - 1.96 * sd(length) / sqrt(length(length)),
            upper_ci = mean + 1.96 * sd(length) / sqrt(length(length)))

write.csv(data.age,file = "data.age.csv", row.names = FALSE)

## BRING IN DATAFRAME ##########################################################

data.all <- read.csv("updated.lw.csv") %>% 
  filter(season != "n/a",
         sex != "n/a",
         sex != "f/m",
         !is.na(reach),
         reach != "Upper Platte",
         length != "n/a",
         weight != "n/a") %>% 
  mutate(basin = as.factor(ifelse(site %in% c("Missouri","Plattsmouth"), "Platte-Missouri confluence", "Platte River basin")),
         season = as.factor(season),
         reach = as.factor(reach),
         sex = as.factor(sex),
         length = as.numeric(length),
         weight = as.numeric(weight),
         log_length = log10(length),
         log_weight = log10(weight)) %>% 
  drop_na(reach)



# Random stuff ####
mean(data.a$Length)
mean(data.mo$Length)
mean(data.a$Weight)

max(data.a$Age)

#Create data frames by river
data.pl <- data.a |>
  filter(River == 'Platte')

data.sc <- data.a |>
  filter(River == 'Salt Creek')

data.eh <- data.a |>
  filter(River == 'Elkhorn')

data.mo <- data.a |>
  filter(River == 'Missouri')

data.lo <- data.a |>
  filter(River == 'Loup')

data.pl.spr <- data.pl |>
  filter(Season == 'Spring')

data.pl.smr <- data.pl |>
  filter(Season != 'Spring')

#Calculate mean by river reach

mean(data.pl$Length)
mean(data.sc$Age)
mean(data.eh$Age)
mean(data.lo$Age)
mean(data.mo$Length)

#test for normality
data.pl_scaled <- scale(data.pl$Length)
shapiro.test(data.pl$Age)
shapiro.test(data.sc$Age)
shapiro.test(data.eh$Age)
shapiro.test(data.lo$Length)
shapiro.test(data.mo$Age)

#Kolmogorov Smirnov test to compare age distributions between river reaches
ks_result_age <- ks.test(data.pl$Age, data.eh$Age)
print(ks_result_age)

#plot age by reach
data.a$facet = factor(data.a$Season, levels = c("Spring","Summer","Fall"))

mo <- ggplot(data.mo, aes(x = Length))+
  geom_histogram(breaks =seq(400, 1000, by = 15))+
  scale_y_continuous(limits = c(0,20))+
  labs(x="", y="",title = "Missouri")+
  coord_cartesian(expand = FALSE)+
  theme_classic() +
  theme(axis.title = element_text(size = 14),  # Increase font size of axis titles
        axis.text = element_text(size = 12)) +
  theme(plot.title = element_text(hjust = 0.25)) +
  theme(plot.title = element_text(size = 16)) +
  scale_x_continuous(breaks = seq(400,900,100), limits = c(400,900))

pl <- ggplot(data.a, aes(x = Length))+
  geom_histogram(breaks =seq(400, 1000, by = 15))+
  scale_y_continuous(limits = c(0,150))+
  labs(x="Total Length (mm)", y="Count",title = "Platte")+
  coord_cartesian(expand = FALSE)+
  theme_classic() +
  theme(axis.title = element_text(size = 14),  # Increase font size of axis titles
        axis.text = element_text(size = 12)) +
  theme(plot.title = element_text(hjust = 0.25)) +
  theme(plot.title = element_text(size = 16)) +
  scale_x_continuous(breaks = seq(400,900,100), limits = c(400,900))

gridExtra::grid.arrange(pl, mo, ncol = 2, nrow = 1) +
  theme_classic()
  



##Sex Ratios ###################################################################

#create two dataframes (m + f) to count # of males/females
data.male <- data.all |>
  filter(sex == 'm')
data.female <- data.all |>
  filter(sex == 'f')

mean(data.male$length)
mean(data.female$length)


prop.test(x = 984, n = 1582, alternative = 'two.sided')


Cramers_V(chi = 75.435, n = sum(reach.sex), df = 1)

# male count = 902
# female count = 568
902/568
#sex ratio (m:f) of 1.59:1

r <- c("Missouri","Lower Platte","Salt Creek","Middle Platte","Elkhorn","Loup","Central Platte","Missouri","Lower Platte","Salt Creek","Middle Platte","Elkhorn","Loup","Central Platte")
pr <- c(0.4327,0.4197,0.3867,0.3916,0.4014,0.1023,0.0526,0.5673,0.5803,0.6133,0.6084,0.5986,0.8977,0.9474)
Sex <- c("f","f","f","f","f","f","f","m","m","m","m","m","m","m")
l_ci <- c(0.5075,0.5207,0.5715,0.6778,0.5178,0.8331,0.8470,0,0,0,0,0,0,0)
u_ci <- c(0.6271,0.6399,0.6551,0.5194,0.6794,0.9623,1,0,0,0,0,0,0,0)



sexdf <- data.frame(r,Sex,pr,l_ci,u_ci) %>% 
  mutate(lfin = (1-pr) - ((pr-l_ci)*2),
         ufin = (1-pr) + ((pr-l_ci)*2)) 


riv <- c("Missouri","Lower Platte","Salt Creek","Middle Platte","Elkhorn","Loup","Central Platte")

sexdf$reach <- factor(sexdf$r, levels = riv)

sex.r.reach <- ggplot(sexdf, aes(x = reach, y = pr, fill = Sex)) +
  geom_col(color = "black", size = 0.6, position = "fill") +
  scale_y_continuous(expand = c(0,0)) +
  geom_errorbar(aes(ymin = l_ci, ymax = u_ci), width = 0.2, color = "black", size = 1) +
  labs(x = "River Reach", y = "Proportion") +
  scale_fill_manual(values = c("white","grey")) +
  theme_classic() +
  theme(axis.title = element_text(size = 14),  # Increase font size of axis titles
        axis.text = element_text(size = 12)) 

ggsave("sex.r.reach.png",sex.r.reach,dpi=500)

?pairwise.prop.test
#perform test by river reach
reach.sex <- table(data.all$reach, data.all$sex)
reach.sex

prop.test(reach.sex)
pairwise.prop.test(reach.sex, p.adjust.method = "bonferroni")

Cramers_V <- function(chi, n, df) sqrt((chi)/(n * df))
Cramers_V(chi = 43.405, n = sum(reach.sex), df = 6)

#and for basin
basin.sex <- table(data.all$basin, data.all$sex)
basin.sex

prop.test(basin.sex)
pairwise.prop.test(basin.sex, p.adjust.method = "bonferroni")

Cramers_V <- function(chi, n, df) sqrt((chi)/(n * df))

df <- min(dim(basin.sex)) - 1

Cramers_V(chi = 8.350, n = sum(reach.sex), df = df)

#perform test by season
data.season <-table(data.all$season, data.all$sex)
data.season
prop.test(data.season)
#not significant, so no post hoc

#KS test between sex and length distribution
ks_result_sex_length <- ks.test(data.male$Length, data.female$Length)
print(ks_result_sex_length)

median(data.male$Length)
median(data.female$Length)

ks_result_sex_weight <- ks.test(data.male$Weight, data.female$Weight)
print(ks_result_sex_weight)
mean(data.male$Weight)
mean(data.female$Weight)

#take a look at the data

ggplot(data.c, aes(x = Length))+
  geom_histogram(breaks =seq(400, 1000, by = 15))+
  scale_y_continuous(limits = c(0,125))+
  facet_wrap(vars(Sex), ncol = 1, nrow = 2, scales = "free",
             labeller = as_labeller(c(f = "Female", m = "Male")), 
             strip.position = "left") +
  labs(x="Total Length (mm)", y="Count")+
  coord_cartesian(expand = FALSE)+
  scale_x_continuous(breaks = seq(400,900,100), limits = c(400,900)) +
  labs(y = NULL) +
  theme_classic() +
  theme(strip.background = element_blank(), strip.placement = "outside")
  
#Kruskal wallis test to compare mean lengths between sexes

mwu_sex <- wilcox.test(Length ~ Sex, data = data.c, alternative = "greater")
print(mwu_sex)
dunn.test(kw_sex)

#generating length-weight relationships
data.all$log_length <- log10(data.all$length)
data.all$log_weight <- log10(data.all$weight)

# Fit linear regression model
model <- lm(log(log_weight) ~ log(log_length), data = data.c)

# Print summary of the regression
summary(model)

# Plot the original data and the regression line
plot(data.c$log_length, data.c$log_weight, main = "", 
     xlab = "Log10 Length", ylab = "Log10 Weight",
     title = NULL,
     abline(model, col = "red"))

# Fit linear regression model
model_b <- lm(log_weight ~ log_length * River * Sex * Season, data = data.c)

# Perform ANCOVA
ancova_result <- aov(model_b)

# Print summary of ANCOVA
summary(ancova_result)



#Fit linear regression models for each sex
platte_model <- lm(log_weight ~ log_length, data = subset(fish_data, River == "Platte"))
elkhorn_model <- lm(log_weight ~ log_length, data = subset(fish_data, River == "Elkhorn"))
salt_model <- lm(log_weight ~ log_length, data = subset(fish_data, River == "Salt Creek"))
loup_model <- lm(log_weight ~ log_length, data = subset(fish_data, River == "Loup"))
missouri_model <- lm(log_weight ~ log_length, data = subset(fish_data, River == "Missouri"))

# Create a scatter plot for Loup with regression line
loup_plot <- ggplot(subset(fish_data, River == "Loup"), aes(x = log_length, y = log_weight)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, color = "red") +
  scale_y_continuous(limits = c(3.0,3.9, by = 0.25)) +
  scale_x_continuous(limits = c(2.68,2.95, by = 0.15)) +
  labs(title = "Loup", x = "Log10 Length", y = "Log10 Weight") +
  theme_classic() +
  coord_cartesian(expand = FALSE)

# Create a scatter plot for Missouri with regression line
missouri_plot <- ggplot(subset(fish_data, River == "Missouri"), aes(x = log_length, y = log_weight)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, color = "red") +
  labs(title = "Missouri", x = "Log10 Length", y = "Log10 Weight")

# Create a scatter plot for Platte with regression line
platte_plot <- ggplot(subset(fish_data, River == "Platte"), aes(x = log_length, y = log_weight)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, color = "red") +
  scale_y_continuous(limits = c(3.0,3.9, by = 0.25)) +
  scale_x_continuous(limits = c(2.68,2.95, by = 0.15)) +
  labs(title = "Platte", x = "Log10 Length", y = "Log10 Weight") +
  theme_classic() +
  coord_cartesian(expand = FALSE)

# Create a scatter plot for Salt with regression line
salt_plot <- ggplot(subset(fish_data, River == "Salt Creek"), aes(x = log_length, y = log_weight)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, color = "red") +
  scale_y_continuous(limits = c(3.0,3.9, by = 0.25)) +
  scale_x_continuous(limits = c(2.68,2.95, by = 0.15)) +
  labs(title = "Salt Creek", x = "Log10 Length", y = "Log10 Weight") +
  theme_classic() +
  coord_cartesian(expand = FALSE)

# Create a scatter plot for Elkhorn with regression line
elk_plot <- ggplot(subset(fish_data, River == "Elkhorn"), aes(x = log_length, y = log_weight)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, color = "red") +
  scale_y_continuous(limits = c(3.0,3.9, by = 0.25)) +
  scale_x_continuous(limits = c(2.68,2.95, by = 0.15)) +
  labs(title = "Elkhorn", x = "Log10 Length", y = "Log10 Weight") +
  theme_classic() +
  coord_cartesian(expand = FALSE)

# Display both plots side by side
gridExtra::grid.arrange(platte_plot, salt_plot, elk_plot, loup_plot, ncol = 2, nrow = 2) +
  theme_classic()

# Fit linear regression model
model <- lm(log_weight ~ log_length * River, data = fish_data)

# Extract residuals from the ANCOVA model
residuals <- resid(model)

# Create a data frame with residuals and River
residuals_df <- data.frame(residuals, River = fish_data$River)
residuals_df$riverfactor <- as.factor(residuals_df$River)
# Perform Dunn's test for pairwise comparisons
dunn_result <- dunn.test(residuals_df$residuals ~ residuals_df$riverfactor, method = "bonferroni")

# Print the results
print(dunn_result)

?chisq.posthoc.test

#build and rank models for each variable
river_model <- lm(log_weight ~ log_length * River, data = fish_data)
sex_model <- lm(log_weight ~ log_length * Sex, data = fish_data)
season_model <- lm(log_weight ~ log_length * Season, data = fish_data)
riv_season_model <- lm(log_weight ~ log_length * Season * River, data = fish_data)
sex_season_model <- lm(log_weight ~ log_length * Season * Sex, data = fish_data)
riv_sex_model <- lm(log_weight ~ log_length * Sex * River, data = fish_data)

aic_river <- AIC(river_model)
aic_sex <- AIC(sex_model)
aic_season <- AIC(season_model)

aic_df <- data.frame(
  Model = c("River", "Sex", "Season"),
  AIC = c(aic_river, aic_sex, aic_season))

ranked_models <- aic_df[order(aic_df$AIC), ]

print("Ranked models based on AIC:")
print(ranked_models)

aicc_river <- AICc(river_model)
aicc_sex <- AICc(sex_model)
aicc_season <- AICc(season_model)

aicc_df <- data.frame(
  Model = c("River Model", "Sex Model", "Season Model"),
  AIC = c(aic_river, aic_sex, aic_season),
  AICc = c(aicc_river, aicc_sex, aicc_season))


print("Models with AIC, AICc, and AICc weights:")
print(aicc_df)

modlist_body <- list(river = river_model,sex = sex_model,season = season_model,rivsea = riv_season_model,sexsea = sex_season_model,rivsex = riv_sex_model)
model.sel(modlist_body)

summary(riv_season_model)

plot(allEffects(riv_season_model))
plot(Effect("River",riv_season_model))


ancov_river <- aov(log_weight ~ log_length * River, data = fish_data)
summary(ancov_river)
plot(allEffects(river_model))

#using Ogle R Book
fish_data <- fish_data %>% 
  mutate(Sex = as.factor(Sex))


summary(fish_ancov)
adjusted_p_values <- p.adjust(p_values, method = "BH")
print(adjusted_p_values)

cbind(coef=coef(river_model),confint(river_model))

symbs <- c(1,2,3,4)

#all this shit doesn't work
plot(Weight~Length, data = fish_data, pch=symbs[fish_data$River],
     ylab = "Weight (g)", xlab = "Total Length (mm)",cex=0.6)

tmp <- fish_data %>% 
  group_by(River) %>% 
  summarize(min = min(Length, na.rm = TRUE),
            max = max(Length, na.rm = TRUE))
tmpx <- seq(tmp$min[1],tmp$max[1],length.out = 99)
tmpy <- 10^(predict(river_model,
                    data.frame(log_length = log10(tmpx), RiverPlatte = factor(Platte))))

fitPlot(river_model,xlab="log Fork Length (mm)",ylab="log Weight (g)",legend="topleft",main="")

#let's give ggplot a try
ggplot(fish_data,aes(x=log_weight,y=log_length,color=River,fill=River)) +
  labs(x = Log)
  geom_smooth(method="lm",alpha=0.2) +
  geom_point() +
  theme_classic()


fish_data <- data.c 
  
?cov

#sex model
TukeyHSD(m)
m <- aov(new_sex_model)
new_sex_model <- lm(log_weight ~ log_length * reach, data = data.all)
confint(new_sex_model)  

ggplot(data.all,aes(x=log_length,y=log_weight,color=year,fill=year)) +
  labs(x = "Log10 Length",y = "Log10 Weight") +
  stat_poly_line(method="lm",alpha=0.2) +
  stat_poly_eq(use_label(c("R2","P","eq")),formula = y ~ x) +
  geom_point() +
  theme_classic()

#and again without log scale
ggplot(data.all,aes(x=length,y=weight,color=year,fill=year,shape = year)) +
  labs(x = "Total Length (mm)",y = "Weight (g)") +
  stat_poly_line(formula = x ~ log10(y) , alpha=0.2) +
  stat_poly_eq(formula = x ~ log10(y) , use_label(c('R2', 'P', 'eq'))) +
  geom_point() +
  theme_classic()

#BODY CONDITION ### LENGTH WEIGHT REGRESSIONS ##################################

#LW Regression by year ####
year_model <- lm(log_weight ~ log_length * year, data = data.all)
anova(year_model)
cbind(coef = coef(year_model),confint(year_model))
confint(year_model)

lw.year <- ggplot(data.all,aes(x=log_length,y=log_weight)) +
  labs(x = "Log10 Length",y = "Log10 Weight") +
  geom_smooth(method = "lm", color = "black", alpha = 0.2) +
  geom_point() +
  facet_wrap(~year, ncol = 1) +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  scale_color_manual(values = c("black", "black")) +
  labs(color = NULL, linetype = NULL, shape = NULL) +
  theme_pubr(12) +
  theme(strip.text = element_text(face = "bold", size = 11))

ggsave("lw.year.png",lw.year,dpi = 350)

#LW Regression by basin ####
basin_model <- lm(log_weight ~ log_length * basin, data = data.all)
anova(basin_model)
cbind(coef = coef(basin_model),confint(basin_model))

lwbasin <- 
  ggplot(data.all,aes(x=log_length,y=log_weight)) +
  labs(x = "Log10 Length",y = "Log10 Weight") +
  geom_smooth(method="lm", color = "black", alpha=0.2) +
  facet_wrap(~basin, ncol = 1) +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point() +
  scale_color_manual(values = c("black", "black")) +
  labs(color = NULL, linetype = NULL, shape = NULL) +
  theme_pubr(12) +
  theme(strip.text = element_text(face = "bold", size = 11))

ggsave("lw.basin.2.png",lwbasin,dpi = 500)

#LW Regression by river reach ####
river_model <- lm(log_weight ~ log_length * reach, data = data.all)
anova(river_model)
cbind(coef = coef(river_model),confint(river_model))

lw.reach 

linetypes <- c("Missouri" = "dashed", "lower Platte" = "dashed", "middle Platte" = "dashed", "Loup" = "dashed", "Elkhorn" = "dashed", "central Platte" = "dotted", "Salt Creek" = "solid")
 
lw.reach <- ggplot(data.all,aes(x=log_length,y=log_weight, fill = reach, color = reach, linetype = reach)) +
  labs(x = "Log10 Length",y = "Log10 Weight") +
  geom_smooth(method="lm",alpha=0.2) +
  #geom_point(shape = 21, size = 1.8) +
  #stat_poly_eq(use_label(c("eq", "R2"))) +
  scale_color_manual(values = c("black", "grey","grey","grey","grey","grey","black")) +
  scale_fill_manual(values = c("black", "grey","grey","grey","grey","grey","black")) +
  scale_linetype_manual(values = linetypes) +
  #facet_wrap(~reach, ncol = 2) +
  scale_x_continuous(limits = c(2.65, 3.01), breaks = seq(2.65,3,by = 0.05)) +
  ylim(2.9,4.1) +
  labs(color = NULL, linetype = NULL, shape = NULL) +
  theme_pubr(12) +
  theme(strip.text = element_text(face = "bold", size = 11)) +
  theme(legend.position = "bottom") +
  guides(fill = "none") +
  coord_cartesian(expand = FALSE)
ggsave('lw.reach.png', height = 4.5, width = 7.5, lw.reach, dpi = 500)

levels(data.all$reach)

#LW Regression by season ####
season_model <- lm(log_weight ~ log_length * season, data = data.all)
anova(season_model)
cbind(coef = coef(season_model),confint(season_model))

ggplot(data.all,aes(x=log_length,y=log_weight, shape = season, color = season, linetype = season)) +
  labs(x = "Log10 Length",y = "Log10 Weight") +
  geom_smooth(method="lm",alpha=0.2) +
  geom_point() +
  scale_color_manual(values = c("black", "black","black")) +
  labs(color = NULL, linetype = NULL, shape = NULL) +
  theme_classic()


#LW Regression by sex ####
sex_model <- lm(log_weight ~ log_length * sex, data = data.all)
anova(sex_model)
cbind(coef = coef(sex_model),confint(sex_model))

sic.sex.lw <- ggplot(data.all,aes(x=log_length,y=log_weight,color=sex,fill=sex,shape=sex)) +
  labs(x = "Log10 Length",y = "Log10 Weight") +
  scale_color_manual(values = c("black","grey50")) +
  scale_fill_manual(values = c("black","grey50")) +
  scale_x_continuous(limits = c(2.65,2.95),breaks = seq(2.65,2.95,0.05)) +
  scale_y_continuous(limits = c(3,4),breaks = seq(3,4,0.1)) +
  coord_cartesian(expand = F) +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_smooth(method="lm",alpha=0.2) +
  geom_point() +
  theme_classic()
ggsave("sic.sex.lw.jpg",sic.sex.lw,dpi=500)

plot(sex_model)

#### AGE ANALYSIS ##############################################################

data.age <- read.csv("final.age.data.csv") %>% 
  filter(age != "x",
         age >1,
         sex != "f/m",
         sex != "n/a",
         species == "SIC",
         season != "n/a") %>%
  mutate(age = as.numeric(age),
         reach = as.factor(reach),
         season = as.factor(season),
         sex = as.factor(sex),
         basin = as.factor(ifelse(site %in% c("Missouri","Plattsmouth"), "Platte-Missouri Confluence", "Platte River Basin")))
  
levels(data.age$reach)
levels(data.age$sex)
levels(data.age$season)

##MEANS
summary_sic_age_basin <- data.age %>% 
  summarise(mean = mean(age),
            lower_ci = mean - 1.96 * sd(age) / sqrt(length(age)),
            upper_ci = mean + 1.96 * sd(age) / sqrt(length(age)))

## Kruskal-Wallis between basin, reach, and sex ####
kruskal_test(age ~ basin, data = data.age)
kruskal_test(age ~ reach, data = data.age)
kruskal_test(age ~ sex, data = data.age)
kruskal_test(age ~ season, data = data.age)

## KW Effect Size (eta squared [H]) ####
data.age %>%
  kruskal_effsize(age ~ basin)
data.age %>%
  kruskal_effsize(age ~ reach)
data.age %>%
  kruskal_effsize(age ~ sex)

?kruskal_effsize

## Dunn's test % Vargha and Delaney's A ####
dunn_test(data.age, age ~ basin, p.adjust.method = "bonferroni")
d <- dunn_test(data.age, age ~ reach, p.adjust.method = "bonferroni")
dunn_test(data.age, age ~ sex, p.adjust.method = "bonferroni")
write.csv(d, "dunn.reach.age.csv")
print(d, n = 30)
multiVDA(x = data.age$age,
         g = data.age$basin)
multiVDA(x = data.age$age,
         g = data.age$reach)
multiVDA(x = data.age$age,
         g = data.age$sex)

newd <- as.data.frame(d) %>%
  select(group1,group2,statistic,p.adj)

?multiVDA

print(d, n = 30)
agetable <- flextable(d)
doc <- read_docx() %>%
  body_add_flextable(agetable) %>%
  print(target = "agetable.docx")

data.age$reach <- factor(data.age$reach, levels = c("Missouri","lower Platte","Salt Creek","Elkhorn","middle Platte","Loup","central Platte"))

sic.age.vio <- 
ggplot(data = data.age, aes(x = reach, y = age)) +
  geom_violin(fill = "grey40") +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = widths,
               colour = "black") +
  labs(x = "River Reach",y="Age (years)") +
  scale_y_continuous(limits = c(0,13), breaks = seq(0,13,1), expand = c(0,0)) +
  theme_pubr() +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12))
ggsave("sic.age.vio.jpg",sic.age.vio, width = 7, height = 5, dpi=500)

##  ggarrange

combinedage <- ggarrange(sic.age.vio, sic.vonb.typical,
                         nrow = 2, ncol = 1)
ggsave(combinedage, file = "svcp.age.vonb.png", width = 7, height = 9, dpi = 500)


sic.age.box <- ggplot(data = data.age, aes(x = reach, y = final_age)) +
  geom_boxplot(fill = "grey40") +
  labs(x = "River Reach",y="Age (years)") +
  scale_y_continuous(limits = c(0,14), breaks = seq(0,13,1), expand = c(0,0)) +
  theme_pubr()
ggsave("sic.age.box.jpg",sic.age.box,dpi=500)


mean_ages <- aggregate(age ~ reach, data = data.age, FUN = mean)
widths <- c(0.67,0.72,0.72,0.722,0.72,0.75,0.52)


ggplot(data = mo, aes(x = final_age, fill = reach)) +
  geom_density(alpha = 0.5) +
  labs(x = "Age (years)", y = "Kernel Density", fill = "Reach") +
  scale_color_manual(values = c("black","grey70")) +
  scale_fill_manual(values = c("black","grey70")) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(limits = c(0,13), breaks = seq(0,13,1), expand = c(0,0)) +
  theme(legend.title = element_blank(),
        legend.position = c(1,0),
        legend.justification = c(1,0)) +
  theme_pubr()

ggsave("sic.molo.age.jpg",sic.molo.age, dpi = 500)

ggplot(data = data.age, aes(x = age, color = basin)) +
  geom_density() +
  labs(x = "Age (years)", y = "Cumulative Distribution", linetype = "Basin") +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0)) +
  theme_pubr()

mo <- data.age %>% 
  filter(reach != "Lower Platte",
         reach != "Salt Creek",
         reach != "Middle Platte",
         reach != "Elkhorn",
         reach != "Loup")
mean(mo$age)

lo <- data.age %>% 
  filter(reach == "Loup")
mean(lo$age)

## Plot mean length at age for all groups ####

age_reach <- c("Missouri","Lower Platte","Salt Creek","Middle Platte","Elkhorn","Loup","Central Platte")

data.age$reach <- factor(data.age$reach, levels = age_reach)

summary_age1 <- data.age %>%
  filter(sex != "n/a") %>% 
  group_by(age, basin) %>% 
  summarise(mean = mean(length),
            lower_ci = mean - 1.96 * sd(length) / sqrt(length(length)),
            upper_ci = mean + 1.96 * sd(length) / sqrt(length(length)))  
  

sic.mlage.basin <- ggplot(data = summary_age1, aes(x = age, y = mean, shape = basin)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_errorbar(data = summary_age1, aes(ymin = lower_ci, ymax = upper_ci),
                position = position_dodge(width = 0.3), width = 0.2, color = "black", size = 1) +
  scale_y_continuous(limits = c(0,1000),
                     expand = c(0,0)) +
  scale_x_continuous(limits = c(1,13), breaks = seq(1,13,1)) +
  theme_classic(16) +
  labs(x = "Age (years)",y = "Mean Total Length (mm)") +
  theme(axis.title = element_text(size = 14),  # Increase font size of axis titles
        axis.text = element_text(size = 12)) +
  theme(legend.title = element_blank(), legend.position = c(0.9, 0.1), legend.justification = c(1, 0))
ggsave("sic.mlage.basin.jpg",sic.mlage.basin,dpi=600)

## GLM Approach to compare length at age ####
glm_basin <- glm(length ~ age * basin, data = data.age)
summary(glm_basin)

glm_reach <- glm(length ~ age * reach, data = data.age)
summary(glm_reach)

predicted_length <- predict(glm_basin, type = "response")
print(predicted_length)




## Plotting Length Distributions with ggplot ####
ggplot(data.age, aes(x = age))+
  geom_histogram(binwidth = 1)+
  scale_x_continuous(limits = c(1,13), breaks = seq(1,13,1)) +
  scale_y_continuous(limits = c(0,100))+
  labs(x="Age (years)", y="Count")+
  coord_cartesian(expand = FALSE)+
  theme_classic() +
  theme(axis.title = element_text(size = 14),  # Increase font size of axis titles
        axis.text = element_text(size = 12))


sc <- data.all %>% 
  filter(!is.na(reach),
         reach == "Salt Creek")
sc$reach <- factor(sc$reach, levels = "Salt Creek")

sic.condition.reach <- ggplot(data.all,aes(x=log_length,y=log_weight,color = reach,fill = reach, na.rm = T)) +
  labs(x = "Log10 Length",y = "Log10 Weight") +
  geom_smooth(method="lm", alpha=0.2, linetype = 2) +
  scale_color_manual(values = c("black","gray50","gray50","gray50","gray50","gray50")) +
  scale_fill_manual(values = c("black","gray50","gray50","gray50","gray50","gray50")) +
  geom_smooth(data = sc, method="lm", alpha=0.2, size = 1.5, na.rm = TRUE) +
  scale_x_continuous(limits = c(2.7, 3), breaks = seq (2.7,3,0.05)) +
  ylim(3,4) +
  coord_cartesian(expand = F) +
  labs(color = NULL, fill = NULL, shape = NULL) +
  theme_pubr(12) +
  theme(strip.text = element_text(face = "bold", size = 11))

ggsave("sic.condition.reach.jpg", sic.condition.reach, dpi = 500)

data.all$reach <- factor(data.all$reach, levels = c("Salt Creek","Missouri","Lower Platte","Middle Platte","Elkhorn","Loup"))



geom_point(shape = 21, size = 1.8, na.rm = TRUE) +
  geom_point(data = sc, shape = 21, size = 1.8, na.rm = TRUE) +

  