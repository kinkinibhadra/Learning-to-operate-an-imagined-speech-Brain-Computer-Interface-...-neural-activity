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

# Set your directory path
indir <- "C:/Users/bhadra9/OneDrive - unige.ch (1)/Project_EEG-BCI/Second_paper_data+code/05_BCI_offline_CV_acc/CV_colors_offline/Data"

# List all CSV files in the directory
csv_files <- list.files(indir, pattern = ".csv", full.names = TRUE)

# Read and combine CSV files into one data frame
combined_data <- lapply(csv_files, read_csv) %>%
  bind_rows()

# file_path <- "C:/Users/bhadra9/OneDrive - unige.ch (1)/Project_EEG-BCI/Second_paper_data+code/Combined_CV_accuracy"
# write.csv(combined_data, file.path(file_path, "Offline_colors_CVacc.csv"))


# Convert Day to numeric
combined_data$Day <- as.numeric(combined_data$Day)


AVGSE <- summarySE(combined_data, measurevar = "Accuracy", groupvars = c("Day"))

pd <- position_dodge(0.1) # move them .05 to the left and right
avgPerc_RAW$Day = as.numeric(avgPerc_RAW$Day) # The geom_smooth does not work as a factor
summary_avg_Perf_corr_RAW$Day = as.numeric(summary_avg_Perf_corr_RAW$Day)

avgp <- ggplot() + 
  coord_cartesian(ylim = c(48, 60)) +
  geom_point(data = AVGSE, aes(x = Day, y = Accuracy), color = "black", show.legend = FALSE,
             shape = 18, size = 1, alpha = .0) +
  geom_errorbar(data = AVGSE, aes(x = Day, y = Accuracy, ymin = Accuracy - se, ymax = Accuracy + se), 
                width = .0, position = pd, size = 1.5, show.legend = FALSE, color = rgb(170, 21, 70, maxColorValue = 255)) +
  geom_line(data = AVGSE, aes(x = Day, y = Accuracy), position = pd, size = 2, color = rgb(170, 21, 70, maxColorValue = 255)) + 
  geom_point(data = AVGSE, aes(x = Day, y = Accuracy), position = pd, size = 5) + 
  theme_SM +
  labs(x = "Day", y = "Offline CV Accuracy [%]")

avgp + theme(axis.line = element_line(size = 1), panel.border = element_blank(),
             axis.text = element_text(face = "bold")) + 
  annotate(geom = "text", x = 4, y = 51, label = "n = 10", size = 6, color = "black") +
  scale_y_continuous(breaks = seq(48, 60, by = 2)) + # Set the tick interval to 10
  theme_SM



# Statistics --------


# ---- Link contrasts to the predictors: Trial averaged data RAW ----
combined_data$time_contrast <- ifelse(combined_data$Day == '1', '-2',
                                    ifelse(combined_data$Day == '2','-1',
                                           ifelse(combined_data$Day == '3','0',
                                                  ifelse(combined_data$Day == '4','1',
                                                         ifelse(combined_data$Day == '5','2','bug')))))
combined_data$time_contrast = as.numeric(combined_data$time_contrast)
levels(combined_data$time_contrast) # Should be NULL

AVG_stat <-lmer(Accuracy ~ time_contrast + (1|Subject), data = combined_data, 
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





