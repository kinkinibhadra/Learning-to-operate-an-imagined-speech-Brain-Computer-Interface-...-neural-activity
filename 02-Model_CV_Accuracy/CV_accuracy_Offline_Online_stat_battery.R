# CV accuracy Offline

rm(list=ls(all=TRUE)); graphics.off() # clear out workspace and close opened figures
#-----------------------------------------------------------------------

# Load the packages needed for this script

my_packages = c("splitstackshape", "emmeans", "dplyr", "afex", "gdata", "stringr","readr",
                "tidyr", "openxlsx", "ggplot2", "lme4", "lmerTest" ,"hrbrthemes","viridis","nlme","ggrepel")
lapply(my_packages, require, character.only = TRUE)

# Global settings for figures 
theme_set(theme_bw(base_size = 12))
theme_SM <- theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(), 
                  panel.background = element_blank(), 
                  axis.title.x = element_text(size = 16),
                  # axis.text.x = element_text(size = 14),
                  # axis.text.y = element_text(size = 14),
                  axis.title.y = element_text(size = 16),
                  axis.text=element_text(size=12, family = "Arial", colour = 'black')) #  panel.border = element_blank()


# Custom Function, SE .....
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


indir =  "C:/Users/bhadra9/OneDrive - unige.ch (1)/Project_EEG-BCI/Second_paper_data+code/Combined CVacc_colors+batt/"
setwd(indir)

Battery_online <- read.csv("Online_battery_CVacc.csv")
Battery_offline <- read.csv("Offline_battery_CVacc.csv")

# Convert Day to numeric
Battery_online$Day <- as.numeric(Battery_online$Day)
Battery_offline$Day <- as.numeric(Battery_offline$Day)


AVGSE_online <- summarySE(Battery_online, measurevar = "Accuracy", groupvars = c("Day"))
AVGSE_offline <- summarySE(Battery_offline, measurevar = "Accuracy", groupvars = c("Day"))


pd <- position_dodge(0.1) # move them .05 to the left and right

# FOR ONLINE ----

avgp <- ggplot() + 
  coord_cartesian(ylim = c(50, 74)) +
  geom_point(data = AVGSE_online, aes(x = Day, y = Accuracy), color = "black", show.legend = FALSE,
             shape = 18, size = 1, alpha = .0) +
  geom_errorbar(data = AVGSE_online, aes(x = Day, y = Accuracy, ymin = Accuracy - se, ymax = Accuracy + se), 
                width = .0, position = pd, size = 1, show.legend = FALSE, color = rgb(59, 179, 113, maxColorValue = 255)) +
  geom_line(data = AVGSE_online, aes(x = Day, y = Accuracy), position = pd, size = 2, color = rgb(59, 179, 113, maxColorValue = 255)) + 
  geom_point(data = AVGSE_online, aes(x = Day, y = Accuracy), position = pd, size = 5) + 
  theme_SM +
  labs(x = "Day", y = "Online CV Accuracy [%]")

avgp + theme(axis.line = element_line(size = 1), panel.border = element_blank(),
             axis.text = element_text(face = "bold")) + 
  annotate(geom = "text", x = 4, y = 52, label = "n = 15", size = 6, color = "black") +
  scale_y_continuous(breaks = seq(50, 74, by = 2)) + # Set the tick interval to 10
  theme_SM



# Statistics, planned contrast --------


# ---- Link contrasts to the predictors: Trial averaged data RAW ----
Battery_online$time_contrast <- ifelse(Battery_online$Day == '1', '-2',
                                      ifelse(Battery_online$Day == '2','-1',
                                             ifelse(Battery_online$Day == '3','0',
                                                    ifelse(Battery_online$Day == '4','1',
                                                           ifelse(Battery_online$Day == '5','2','bug')))))
Battery_online$time_contrast = as.numeric(Battery_online$time_contrast)
levels(Battery_online$time_contrast) # Should be NULL

AVG_stat <-lmer(Accuracy ~ time_contrast + (1|Subject), data = Battery_online, 
                na.action = na.exclude) 

anova(AVG_stat)
summary(AVG_stat)

# Extract the variance components
residual_variance <- attr(VarCorr(AVG_stat), "sc")^2
fixed_effect_variance <- fixef(AVG_stat)["time_contrast"]^2

# Calculate the total variance
total_variance <- residual_variance + fixed_effect_variance

# Calculate eta-squared
eta_squared <- fixed_effect_variance / total_variance
print(c('AVG_stat ETASQ -- ',eta_squared))



# FOR OFFLINE ----
avgp <- ggplot() + 
  coord_cartesian(ylim = c(46, 60)) +
  geom_point(data = AVGSE_offline, aes(x = Day, y = Accuracy), color = "black", show.legend = FALSE,
             shape = 18, size = 1, alpha = .0) +
  geom_errorbar(data = AVGSE_offline, aes(x = Day, y = Accuracy, ymin = Accuracy - se, ymax = Accuracy + se), 
                width = .0, position = pd, size = 1, show.legend = FALSE, color = rgb(59, 179, 113, maxColorValue = 255)) +
  geom_line(data = AVGSE_offline, aes(x = Day, y = Accuracy), position = pd, size = 2, color = rgb(59, 179, 113, maxColorValue = 255)) + 
  geom_point(data = AVGSE_offline, aes(x = Day, y = Accuracy), position = pd, size = 5) + 
  theme_SM +
  labs(x = "Day", y = "Offline CV Accuracy [%]")

avgp + theme(axis.line = element_line(size = 1), panel.border = element_blank(),
             axis.text = element_text(face = "bold")) + 
  annotate(geom = "text", x = 4, y = 48, label = "n = 15", size = 6, color = "black") +
  scale_y_continuous(breaks = seq(46, 60, by = 2)) + # Set the tick interval to 10
  theme_SM



# Statistics, planned contrast --------


# ---- Link contrasts to the predictors: Trial averaged data RAW ----
Battery_offline$time_contrast <- ifelse(Battery_offline$Day == '1', '-2',
                                       ifelse(Battery_offline$Day == '2','-1',
                                              ifelse(Battery_offline$Day == '3','0',
                                                     ifelse(Battery_offline$Day == '4','1',
                                                            ifelse(Battery_offline$Day == '5','2','bug')))))
Battery_offline$time_contrast = as.numeric(Battery_offline$time_contrast)
levels(Battery_offline$time_contrast) # Should be NULL

AVG_stat <-lmer(Accuracy ~ time_contrast + (1|Subject), data = Battery_offline, 
                na.action = na.exclude) 

anova(AVG_stat)
summary(AVG_stat)

# Extract the variance components
residual_variance <- attr(VarCorr(AVG_stat), "sc")^2
fixed_effect_variance <- fixef(AVG_stat)["time_contrast"]^2

# Calculate the total variance
total_variance <- residual_variance + fixed_effect_variance

# Calculate eta-squared
eta_squared <- fixed_effect_variance / total_variance
print(c('AVG_stat ETASQ -- ',eta_squared))





