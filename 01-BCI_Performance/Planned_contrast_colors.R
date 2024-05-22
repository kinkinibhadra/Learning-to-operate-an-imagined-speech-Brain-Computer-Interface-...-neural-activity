# Author Kinkini Bhadra
# Created on 10th August, 2022 
#-----------------------------------------------------------------------

## examples for contrast analysis
# https://towardsdatascience.com/doing-and-reporting-your-first-planned-contrasts-in-r-ee77ff277088
# https://github.com/Statistics4Doctors
# https://rstudio-pubs-static.s3.amazonaws.com/65059_586f394d8eb84f84b1baaf56ffb6b47f.html
#-----------------------------------------------------------------------


rm(list=ls(all=TRUE)); graphics.off() # clear out workspace and close opened figures
#-----------------------------------------------------------------------

# Load the packages needed for this script

my_packages = c("splitstackshape", "emmeans", "dplyr", "afex", "gdata", "stringr",
                "tidyr", "openxlsx", "ggplot2", "lme4", "lmerTest" ,"hrbrthemes","viridis","nlme", "ggrepel")
lapply(my_packages, require, character.only = TRUE)

# ..............................................................
#    *********** FUNCTION TO SUMMARIZE THE DATA ***********
# ..............................................................


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

# ------------------------End of the function ------------------------------

# Data Directory
# personal path
KB_path = 'C:/Users/bhadra9/OneDrive - unige.ch (1)/BCI_Group_shared'
#SM_path = ''

# Absolute path
dir = paste(KB_path, '/Code/KB/00_Reproducable_abstract_plots/Fig1_BCI_Behaviour/Data/', sep = '')

dir = 'C:/Users/bhadra9/OneDrive - unige.ch (1)/Project_EEG-BCI/Second_paper_data+code/03_BCI_performance_Time/Colors/Data/' # its the same data
setwd(dir) # setting the current directory

# Data Organisation and create data frame
FilesList  <- list.files(dir,glob2rx('*.csv'))
subj       <- length(FilesList)
All_Data   <- c()

for (i_sub in 1: subj) {
  
  tmp = as.data.frame(read.table(FilesList[i_sub],header=T,sep=",")) 
        names(tmp)=c("Subject",'Trial','Day','Block','Truth', 'Perf_corr_RAW',
        'Last_cursor_pos', 'Max_cursor_pos', 'Trial_duration_SEC')
        
  All_Data = rbind(All_Data, tmp) # combine tables
  
}


# Also replace the R and L with 'Ghi' and 'fo' (Its worth it to do that)
# R = 'ghi' ; L = 'fo'
All_Data$Truth <- str_replace(All_Data$Truth, "L", "/fo/")
All_Data$Truth <- str_replace(All_Data$Truth, "R", "/gi/")


# transform numerical variables into factors (otherwise they count as 1 DoF)
All_Data$Subject  = as.factor(All_Data$Subject)
All_Data$Day      = as.factor(All_Data$Day)
All_Data$Block    = as.factor(All_Data$Block)
All_Data$Truth    = as.factor(All_Data$Truth)


# Removing all the variables apart from our main data frame (I like clutter free workspace)
keep(All_Data, summarySE, sure = TRUE)

All_Data <- na.omit(All_Data)



# ---------------- AVERAGE PERFORMANCE -------------

# compute the average Perc correct --> by DAY
avgPerc_RAW          = aggregate(Perf_corr_RAW    ~ Subject + Day,All_Data, mean)
file_path <- "C:/Users/bhadra9/OneDrive - unige.ch (1)/Project_EEG-BCI/Second_paper_data+code/03_BCI_performance_Time/Combined"
# write.csv(avgPerc_RAW, file.path(file_path, "Colors_avgPerf.csv"))

avgPerc_RAW_norm     = aggregate(Perf_corr_RAW_norm    ~ Subject + Day,All_Data, mean)



# Copy-paste from Silvia for Plot Settings
# -------------------------------------------------------------------------------
# Global settings for figures 
theme_set(theme_bw(base_size = 12))
theme_SM <- theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(), 
                  panel.background = element_blank(), 
                  axis.title.x = element_text(size = 18),
                  # axis.text.x = element_text(size = 14),
                  # axis.text.y = element_text(size = 14),
                  axis.title.y = element_text(size = 18),
                  axis.text=element_text(size=14, family = "Arial", colour = 'black')) #  panel.border = element_blank()




# ------------------------- BASIC DATA TREND CHECKING PLOTS --------------------------

# ......................... 1. Performance density plots ........................... 

# histogram of PERCENTAGE CORRECT_ RAW based on both class
ggplot(All_Data, aes(x = Perf_corr_RAW, color = Subject)) + 
  geom_histogram(aes(color = Subject, y=..density..,), fill="white", alpha = 0.4, position = "identity") +
  geom_density(data = All_Data, alpha=.8, aes(fill = Subject))+
  facet_wrap(~Subject)


# histogram of PERCENTAGE CORRECT_ RAW based on TRUE LABEL
ggplot(All_Data, aes(x=Perf_corr_RAW,color=Truth)) + 
  geom_histogram(aes(color=Truth,y=..density.., fill=Truth), alpha = 0.1,position="identity") +
  geom_density(data=All_Data,alpha=.2, aes(fill=Truth)) + facet_wrap(~Subject)



# ......................... 2. Trial Average Performance per Subject ........................... 

# Dot Plot for average trial performance of each subject computed in  % [0:100]
# --> Based on RAW performance
ggplot(avgPerc_RAW)+
  geom_point(aes(x = Day, y = Perf_corr_RAW,color = Subject),shape=16, size = 3) +
  geom_line(aes(x = Day, y = Perf_corr_RAW, color = Subject,group=Subject)) +
  facet_grid(.~Subject,scales="free") + theme_SM + ylim(38, 80) +
  # geom_smooth(method = lm, formula = Perf_corr_RAW ~ Day) +
  labs(x = "Day", y = "Percentage correct [%]") + ggtitle ('Individual BCI-control performance_RAW (online)') 
# NOTES: Picture size used is 1300,300


# Indv plots ------ Latest on 10/09/2023 -------
se_ALL_DATA <- summarySE(All_Data, measurevar = "Perf_corr_RAW", groupvars = c("Day", "Subject"))




All_Data$Day = as.numeric(All_Data$Day)

# Fit separate linear regression models for each subject and calculate slopes
slope_data <- All_Data %>%
  group_by(Subject) %>%
  do({
    model <- lm(Perf_corr_RAW ~ Day, data = .)
    data.frame(Subject = unique(.$Subject), slope = coef(model)[2])
  })

# Sort the subjects based on the calculated slopes
sorted_subjects <- slope_data %>%
  arrange(slope) %>%
  pull(Subject)

# Create a factor variable for Subject based on the sorted order
All_Data$Subject <- factor(All_Data$Subject, levels = sorted_subjects)
se_ALL_DATA$Subject =  factor(se_ALL_DATA$Subject, levels = sorted_subjects)

facet_labels <- data.frame(Subject = sorted_subjects, CustomLabel = 1:10)
facet_labels$CustomLabel = as.factor(facet_labels$CustomLabel)

# Create the plot
final_plot <- ggplot(data = se_ALL_DATA, aes(x = Day, y = Perf_corr_RAW, color = Subject, group = Subject)) +
  coord_cartesian(ylim = c(35, 80)) +
  geom_point(shape = 19, size = 3, show.legend = FALSE) +
  facet_grid(. ~ Subject, scales = "free") +
  theme(strip.background = element_rect(fill = "grey60"),
        strip.text = element_text(color = "black", size = 14)) +
  theme_SM +
  labs(x = "Day", y = "BCI Performance [%]") +
  ggtitle('Performance across individuals throughout 5 days of training') +
  geom_smooth(data = All_Data, aes(x = Day, y = Perf_corr_RAW, group = Subject), method = "lm", show.legend = FALSE,
              fill = "grey60", size = 1, se = TRUE, linetype = "solid") +
  scale_y_continuous(breaks = seq(35, 80, by = 10)) # Specify the tick intervals here

final_plot = final_plot + theme(axis.text = element_text(size = 14)) +
  theme(axis.line = element_line(size = 1),
        axis.text = element_text(face = "bold")) + theme_SM

final_plot + labs(facet = "CustomLabel")

# Pic size used: 1150,320 for the same font size

# Without facet labels ---------------------

final_plot <- ggplot(data = se_ALL_DATA, aes(x = Day, y = Perf_corr_RAW, color = Subject, group = Subject)) +
  coord_cartesian(ylim = c(35, 80)) +
  geom_point(shape = 19, size = 3, show.legend = FALSE) +
  facet_grid(. ~ Subject, scales = "free") +
  theme(strip.background = element_rect(fill = "grey60"),
        strip.text = element_blank()) +  # Removed strip text
  theme_SM +
  labs(x = "Day", y = "BCI Performance [%]") +
  ggtitle('Performance across individuals throughout 5 days of training') +
  geom_smooth(data = All_Data, aes(x = Day, y = Perf_corr_RAW, group = Subject), method = "lm", show.legend = FALSE,
              fill = "grey60", size = 1, se = TRUE, linetype = "solid") +
  scale_y_continuous(breaks = seq(35, 80, by = 10)) + # Specify the tick intervals here
  theme(axis.text = element_text(size = 14)) +
  theme(axis.line = element_line(size = 1),
        axis.text = element_text(face = "bold")) + 
  theme_SM

final_plot



# Average Plot ------------

# install.packages("ggrepel")
summary_avg_Perf_corr_RAW <- summarySE(avgPerc_RAW, measurevar = "Perf_corr_RAW", groupvars = c("Day"))

pd <- position_dodge(0.1) # move them .05 to the left and right
avgPerc_RAW$Day = as.numeric(avgPerc_RAW$Day) # The geom_smooth does not work as a factor
summary_avg_Perf_corr_RAW$Day = as.numeric(summary_avg_Perf_corr_RAW$Day)

avgp <- ggplot() + 
  coord_cartesian(ylim = c(45, 58)) +
  geom_point(data = avgPerc_RAW, aes(x = Day, y = Perf_corr_RAW), color = "black", show.legend = FALSE,
             shape = 18, size = 1, alpha = .0) +
  geom_smooth(data = avgPerc_RAW, aes(x = Day, y = Perf_corr_RAW, color= Day), method = "lm", show.legend = FALSE, 
              linetype = "dashed", size = 1, alpha = 0.3, se = FALSE, color = "grey60") +
  geom_errorbar(data = summary_avg_Perf_corr_RAW, aes(x = Day, y = Perf_corr_RAW, ymin = Perf_corr_RAW - se, ymax = Perf_corr_RAW + se), 
                width = .0, position = pd, size = 1, show.legend = FALSE, color = rgb(170, 21, 70, maxColorValue = 255)) +
  geom_line(data = summary_avg_Perf_corr_RAW, aes(x = Day, y = Perf_corr_RAW), position = pd, size = 2, color = rgb(170, 21, 70, maxColorValue = 255)) + 
  geom_point(data = summary_avg_Perf_corr_RAW, aes(x = Day, y = Perf_corr_RAW), position = pd, size = 5) + 
  theme_SM +
  labs(x = "Day", y = "BCI Performance [%]")

# Add scale_y_continuous to set the tick interval to 10
avgp + theme(axis.line = element_line(size = 1), panel.border = element_blank(),
             axis.text = element_text(face = "bold")) + 
  scale_color_gradient(low = "#F22738", high = "#73020C") +
  annotate(geom = "text", x = 4, y = 48, label = "n = 10", size = 6, color = "black") +
  scale_y_continuous(breaks = seq(44, 60, by = 2)) + # Set the tick interval to 10
  theme_SM

# Extracted file size is 350, 400




# ..............................................................................
# ***************************  STATISTICS  **************************
# ..............................................................................

# Code Motivation: 
# https://github.com/Statistics4Doctors/R-Tutorial-All-Files/blob/main/R%20Tutorial%20-%20%2310%20-%20Planned%20contrasts.R
# Youtube Andy Field: https://www.youtube.com/watch?v=fXDNBeY2qp0
# Silvia's Code


# Link contrasts to the predictors: # ----> All trial data RAW
All_Data$time_contrast <- ifelse(All_Data$Day == '1', '-2',
                          ifelse(All_Data$Day == '2','-1',
                                 ifelse(All_Data$Day == '3','0',
                                        ifelse(All_Data$Day == '4','1',
                                               ifelse(All_Data$Day == '5','2','bug')))))
All_Data$time_contrast = as.numeric(All_Data$time_contrast)
levels(All_Data$time_contrast) # Should be NULL

ALLTRAIL_stat <-lmer(Perf_corr_RAW ~ time_contrast + (1|Subject), data = All_Data, 
                na.action = na.exclude) 

anova(ALLTRAIL_stat)
summary(ALLTRAIL_stat)

# Extract the variance components
residual_variance <- attr(VarCorr(ALLTRAIL_stat), "sc")^2
fixed_effect_variance <- fixef(ALLTRAIL_stat)["time_contrast"]^2

# Calculate the total variance
total_variance <- residual_variance + fixed_effect_variance

# Calculate eta-squared
eta_squared <- fixed_effect_variance / total_variance
print(c('MC ISO ETASQ -- ',eta_squared))



# ---- Link contrasts to the predictors: Trial averaged data RAW ----
avgPerc_RAW$time_contrast <- ifelse(avgPerc_RAW$Day == '1', '-2',
                                 ifelse(avgPerc_RAW$Day == '2','-1',
                                        ifelse(avgPerc_RAW$Day == '3','0',
                                               ifelse(avgPerc_RAW$Day == '4','1',
                                                      ifelse(avgPerc_RAW$Day == '5','2','bug')))))
avgPerc_RAW$time_contrast = as.numeric(avgPerc_RAW$time_contrast)
levels(avgPerc_RAW$time_contrast) # Should be NULL

AVG_stat <-lmer(Perf_corr_RAW ~ time_contrast + (1|Subject), data = avgPerc_RAW, 
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


