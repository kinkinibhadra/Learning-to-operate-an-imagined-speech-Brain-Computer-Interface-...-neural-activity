
# Clear the workspace ---------------
rm(list=ls(all=TRUE)); graphics.off() # clear out workspace and close opened figures

# Load the packages needed for this script ----

my_packages = c("splitstackshape", "emmeans", "dplyr", "afex", "gdata", "stringr", "ggrepel",
                "tidyr", "openxlsx", "ggplot2", "lme4", "lmerTest" ,"hrbrthemes","viridis","nlme")
lapply(my_packages, require, character.only = TRUE)

# Function to summarize the date -----
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


# Global settings for figures  ----
theme_set(theme_bw(base_size = 12))
theme_SM <- theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(), 
                  panel.background = element_blank(), 
                  axis.title.x = element_text(size = 18),
                  # axis.text.x = element_text(size = 14),
                  # axis.text.y = element_text(size = 14),
                  axis.title.y = element_text(size = 18),
                  axis.text=element_text(size=14, family = "Arial", colour = 'black')) #  panel.border = element_blank()




# Load the four datasets from the indir 

indir =  "C:/Users/bhadra9/OneDrive - unige.ch (1)/Project_EEG-BCI/Second_paper_data+code/Combined CVacc_colors+batt/"
setwd(indir)

Colors_online <- read.csv("Online_colors_CVacc.csv")
Colors_offline <- read.csv("Offline_colors_CVacc.csv")

Battery_online <- read.csv("Online_battery_CVacc.csv")
Battery_offline <- read.csv("Offline_battery_CVacc.csv")


# Plotting Online ----

AVGSE_online_colors <- summarySE(Colors_online, measurevar = "Accuracy", groupvars = c("Day"))
AVGSE_online_battery <- summarySE(Battery_online, measurevar = "Accuracy", groupvars = c("Day"))

#Plotting the overlap together (paper) ---------
# Combine the two data frames with a new 'group' column
combined_df <- bind_rows(
  mutate(AVGSE_online_battery, Feedback = "Battery"),
  mutate(AVGSE_online_colors, Feedback = "Colors")
)

# Define custom RGB colors
colors <- c("Battery" = rgb(59, 179, 113, maxColorValue = 255), 
            "Colors" = rgb(170, 21, 70, maxColorValue = 255))

# Plot
avgp <- ggplot(combined_df, aes(x = Day, y = Accuracy, color = Feedback)) +
  geom_line(aes(group = Feedback), position = position_dodge(width = 0.1), size = 2) +  # Line before points and error bars
  geom_errorbar(aes(group = Feedback, ymin = Accuracy - se, ymax = Accuracy + se), 
                width = 0, size = 1, position = position_dodge(width = 0.1)) +
  geom_point(aes(group = Feedback), size = 5, position = position_dodge(width = 0.1), colour= "black") +  # Dodge points according to group
  labs(x = "Day",
       y = "Online CV Accuracy [%]") +
  # annotate(geom = "text", x = 4, y = 60, label = "n = 15", size = 6, color = colors["Battery"]) +  # Annotation for n = 15 in green
  # annotate(geom = "text", x = 4, y = 49, label = "n = 10", size = 6, color = colors["Colors"]) +  # Annotation for n = 15 in green
  theme(axis.line = element_line(size = 1), panel.border = element_blank(),
        axis.text = element_text(face = "bold"),
        axis.line.x = element_line(color = "black"), 
        axis.line.y = element_line(color = "black"),  
        axis.line.x.top = element_blank(), 
        axis.line.y.right = element_blank()) +  
  scale_color_manual(values = colors) +
  scale_y_continuous(breaks = seq(52, 74, by = 2)) + 
  
  theme_SM

avgp
# Exported in 500 by 400 res

# Plotting the overlap separately -----
pd <- position_dodge(0.1) # move them .05 to the left and right
# avgPerc_RAW$Day = as.numeric(avgPerc_RAW$Day) # The geom_smooth does not work as a factor
# summary_avg_Perf_corr_RAW$Day = as.numeric(summary_avg_Perf_corr_RAW$Day)

avgp <- ggplot() + 
  coord_cartesian(ylim = c(52, 74)) +
  geom_point(data = AVGSE_online_battery, aes(x = Day, y = Accuracy), color = "black", show.legend = FALSE,
             shape = 18, size = 1, alpha = .0) +
  geom_errorbar(data = AVGSE_online_battery, aes(x = Day, y = Accuracy, ymin = Accuracy - se, ymax = Accuracy + se), 
                width = 0, position = pd, size = 1, show.legend = FALSE, color = rgb(59,179,113, maxColorValue = 255), alpha = 1) +
  geom_line(data = AVGSE_online_battery, aes(x = Day, y = Accuracy), position = pd, size = 2, color = rgb(59,179,113, maxColorValue = 255)) + 
  geom_point(data = AVGSE_online_battery, aes(x = Day, y = Accuracy), position = pd, size = 5)
  
avgp = avgp + theme(axis.line = element_line(size = 1), panel.border = element_blank(),
                    axis.text = element_text(face = "bold"))

avgp = avgp +
  geom_point(data = AVGSE_online_colors, aes(x = Day, y = Accuracy), color = "black", show.legend = FALSE,
             shape = 18, size = 1, alpha = .0) +
  geom_errorbar(data = AVGSE_online_colors, aes(x = Day, y = Accuracy, ymin = Accuracy - se, ymax = Accuracy + se), 
                width = 0, position = pd, size = 1, show.legend = FALSE, color = rgb(170, 21, 70, maxColorValue = 255), alpha = 1) +
  geom_line(data = AVGSE_online_colors, aes(x = Day, y = Accuracy), position = pd, size = 2, color = rgb(170, 21, 70, maxColorValue = 255)) + 
  geom_point(data = AVGSE_online_colors, aes(x = Day, y = Accuracy), position = pd, size = 5) + 
  theme_SM +
  labs(x = "Day", y = "Online CV Accuracy [%]")

avgp + theme(axis.line = element_line(size = 1), panel.border = element_blank(),
             axis.text = element_text(face = "bold")) + 
  # annotate(geom = "text", x = 4, y = 53, label = "n = 10", size = 6, color = "black") +
  scale_y_continuous(breaks = seq(52, 74, by = 2)) + # Set the tick interval to 10
  theme_SM

# Extracted file size is 350, 400



# Plotting Offline ----

AVGSE_offline_colors <- summarySE(Colors_offline, measurevar = "Accuracy", groupvars = c("Day"))
AVGSE_offline_battery <- summarySE(Battery_offline, measurevar = "Accuracy", groupvars = c("Day"))

#Plotting the overlap together (paper) ---------
# Combine the two data frames with a new 'group' column
combined_df <- bind_rows(
  mutate(AVGSE_offline_battery, Feedback = "Battery"),
  mutate(AVGSE_offline_colors, Feedback = "Colors")
)

# Define custom RGB colors
colors <- c("Battery" = rgb(59, 179, 113, maxColorValue = 255), 
            "Colors" = rgb(170, 21, 70, maxColorValue = 255))

# Plot
avgp <- ggplot(combined_df, aes(x = Day, y = Accuracy, color = Feedback)) +
  geom_line(aes(group = Feedback), position = position_dodge(width = 0.1), size = 2) +  # Line before points and error bars
  geom_errorbar(aes(group = Feedback, ymin = Accuracy - se, ymax = Accuracy + se), 
                width = 0, size = 1, position = position_dodge(width = 0.1)) +
  geom_point(aes(group = Feedback), size = 5, position = position_dodge(width = 0.1), colour= "black") +  # Dodge points according to group
  labs(x = "Day",
       y = "Offline CV Accuracy [%]") +
  # annotate(geom = "text", x = 4, y = 60, label = "n = 15", size = 6, color = colors["Battery"]) +  # Annotation for n = 15 in green
  # annotate(geom = "text", x = 4, y = 49, label = "n = 10", size = 6, color = colors["Colors"]) +  # Annotation for n = 15 in green
  theme(axis.line = element_line(size = 1), panel.border = element_blank(),
        axis.text = element_text(face = "bold"),
        axis.line.x = element_line(color = "black"), 
        axis.line.y = element_line(color = "black"),  
        axis.line.x.top = element_blank(), 
        axis.line.y.right = element_blank()) +  
  scale_color_manual(values = colors) +
  scale_y_continuous(breaks = seq(46, 62, by = 2)) + 
  
  theme_SM

avgp
# Exported in 500 by 400 res

# Plotting online separately 
pd <- position_dodge(0.1) # move them .05 to the left and right
# avgPerc_RAW$Day = as.numeric(avgPerc_RAW$Day) # The geom_smooth does not work as a factor
# summary_avg_Perf_corr_RAW$Day = as.numeric(summary_avg_Perf_corr_RAW$Day)

avgp <- ggplot() + 
  coord_cartesian(ylim = c(46, 62)) +
  geom_point(data = AVGSE_offline_battery, aes(x = Day, y = Accuracy), color = "black", show.legend = FALSE,
             shape = 18, size = 1, alpha = .0) +
  geom_errorbar(data = AVGSE_offline_battery, aes(x = Day, y = Accuracy, ymin = Accuracy - se, ymax = Accuracy + se), 
                width = 0, position = pd, size = 1, show.legend = FALSE, color = rgb(59,179,113,maxColorValue = 255), alpha = 1) +
  geom_line(data = AVGSE_offline_battery, aes(x = Day, y = Accuracy), position = pd, size = 2, color = rgb(59,179,113,maxColorValue = 255)) + 
  geom_point(data = AVGSE_offline_battery, aes(x = Day, y = Accuracy), position = pd, size = 5)

avgp = avgp + theme(axis.line = element_line(size = 1), panel.border = element_blank(),
             axis.text = element_text(face = "bold"))

avgp = avgp +
  geom_point(data = AVGSE_offline_colors, aes(x = Day, y = Accuracy), color = "black", show.legend = FALSE,
             shape = 18, size = 1, alpha = .0) +
  geom_errorbar(data = AVGSE_offline_colors, aes(x = Day, y = Accuracy, ymin = Accuracy - se, ymax = Accuracy + se), 
                width = 0, position = pd, size = 1, show.legend = FALSE, color = rgb(170, 21, 70, maxColorValue = 255), alpha = 1) +
  geom_line(data = AVGSE_offline_colors, aes(x = Day, y = Accuracy), position = pd, size = 2, color = rgb(170, 21, 70, maxColorValue = 255)) + 
  geom_point(data = AVGSE_offline_colors, aes(x = Day, y = Accuracy), position = pd, size = 5) + 
  theme_SM +
  labs(x = "Day", y = "Offline CV Accuracy [%]")

avgp + theme(axis.line = element_line(size = 1), panel.border = element_blank(),
             axis.text = element_text(face = "bold")) + 
  # annotate(geom = "text", x = 4, y = 50, label = "n = 10", size = 6, color = "black") +
  scale_y_continuous(breaks = seq(46, 62, by = 2)) + # Set the tick interval to 10
  theme_SM




# Statistics OFFLINE -----------
Battery_aod_off <- aggregate(Accuracy ~ Subject, data = Battery_offline, FUN = mean)
Colors_aod_off <- aggregate(Accuracy ~ Subject, data = Colors_offline, FUN = mean)

test_result <- t.test(Battery_aod_off$Accuracy, Colors_aod_off$Accuracy, var.equal = FALSE)

mean1 <- mean(Battery_aod_off$Accuracy)
mean2 <- mean(Colors_aod_off$Accuracy)
sd1 <- sd(Battery_aod_off$Accuracy)
sd2 <- sd(Colors_aod_off$Accuracy)
n1 <- length(Battery_aod_off$Accuracy)
n2 <- length(Colors_aod_off$Accuracy)
pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
cohen_d <- abs(mean1 - mean2) / pooled_sd
cat("Cohen's d:", cohen_d, "\n")
# Also make bar plots ----

combined_data <- rbind(
  transform(Battery_aod_off, Dataset = "Battery_aod_off"),
  transform(Colors_aod_off, Dataset = "Colors_aod_off")
)

# Create the box and whisker plot 

# Calculate standard errors and average for each group
# se_data <- combined_data %>%
#   group_by(Dataset) %>%
#   summarize(
#     se = sd(Perf_corr_RAW) / sqrt(n()),
#     avg = mean(Perf_corr_RAW)
#   )

# Create the box and whisker plot with centered x-axis labels, SE bars, and average
plot <- ggplot(combined_data, aes(x = Dataset, y = Accuracy, fill = Dataset)) +
  geom_boxplot() +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5) +
  labs(
    x = "",
    y = "Offline CV Accuracy [%]"
  ) +
  scale_fill_manual(
    values = c("Battery_aod_off" = rgb(59,179,113, maxColorValue = 255), "Colors_aod_off" = rgb(170, 21, 70, maxColorValue = 255))
  ) +
  scale_x_discrete(labels = c("Battery_aod_off" = "Battery", "Colors_aod_off" = "Colors"))

# Center x-axis labels and make SE bars bold and thick
plot + theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  theme(axis.line = element_line(size = 1), panel.border = element_blank(),
        axis.text = element_text(face = "bold")) +
  guides(fill = FALSE) +
  # geom_errorbar(data = se_data, aes(x = Dataset, ymin = avg - se, ymax = avg + se), width = 0.2, size = 1) +
  # geom_point(data = se_data, aes(x = Dataset, y = avg), color = "red", size = 3) +
  theme_SM

# picture extracted for 250 by 400 




# Statistics ONLINE -----------
Battery_aod_on <- aggregate(Accuracy ~ Subject, data = Battery_online, FUN = mean)
Colors_aod_on <- aggregate(Accuracy ~ Subject, data = Colors_online, FUN = mean)

test_result <- t.test(Battery_aod_on$Accuracy, Colors_aod_on$Accuracy, var.equal = FALSE)

mean1 <- mean(Battery_aod_on$Accuracy)
mean2 <- mean(Colors_aod_on$Accuracy)
sd1 <- sd(Battery_aod_on$Accuracy)
sd2 <- sd(Colors_aod_on$Accuracy)
n1 <- length(Battery_aod_on$Accuracy)
n2 <- length(Colors_aod_on$Accuracy)
pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
cohen_d <- abs(mean1 - mean2) / pooled_sd
cat("Cohen's d:", cohen_d, "\n")

# Also make bar plots ----

combined_data <- rbind(
  transform(Battery_aod_on, Dataset = "Battery_aod_on"),
  transform(Colors_aod_on, Dataset = "Colors_aod_on")
)

# Create the box and whisker plot 

# Calculate standard errors and average for each group
# se_data <- combined_data %>%
#   group_by(Dataset) %>%
#   summarize(
#     se = sd(Perf_corr_RAW) / sqrt(n()),
#     avg = mean(Perf_corr_RAW)
#   )

# Create the box and whisker plot with centered x-axis labels, SE bars, and average
plot <- ggplot(combined_data, aes(x = Dataset, y = Accuracy, fill = Dataset)) +
  geom_boxplot() +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5) +
  labs(
    x = "",
    y = "Online CV Accuracy [%]"
  ) +
  scale_fill_manual(
    values = c("Battery_aod_on" = rgb(59, 179, 113, maxColorValue = 255), "Colors_aod_on" = rgb(170, 21, 70, maxColorValue = 255))
  ) +
  scale_x_discrete(labels = c("Battery_aod_on" = "Battery", "Colors_aod_on" = "Colors"))

# Center x-axis labels and make SE bars bold and thick
plot + theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  theme(axis.line = element_line(size = 1), panel.border = element_blank(),
        axis.text = element_text(face = "bold")) +
  guides(fill = FALSE) +
  # geom_errorbar(data = se_data, aes(x = Dataset, ymin = avg - se, ymax = avg + se), width = 0.2, size = 1) +
  # geom_point(data = se_data, aes(x = Dataset, y = avg), color = "red", size = 3) +
  theme_SM

# picture extracted for 250 by 400 








