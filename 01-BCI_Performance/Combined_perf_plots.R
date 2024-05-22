
# Clear the workspace ---------------
rm(list=ls(all=TRUE)); graphics.off() # clear out workspace and close opened figures

# Load the packages needed for this script ----

my_packages = c("splitstackshape", "emmeans", "dplyr", "afex", "gdata", "stringr", "ggrepel",
                "tidyr", "openxlsx", "ggplot2", "lme4", "lmerTest" ,"hrbrthemes","viridis","nlme", "broom")
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




# Load the two datasets from the indir 

indir =  "C:/Users/bhadra9/OneDrive - unige.ch (1)/Project_EEG-BCI/Second_paper_data+code/03_BCI_performance_Time/Combined/"
setwd(indir)

Battery_avgPerf <- read.csv("Battery_avgPerf.csv")
Colors_avgPerf <- read.csv("Colors_avgPerf.csv")

summary_avg_Perf_Battery <- summarySE(Battery_avgPerf, measurevar = "Perf_corr_RAW", groupvars = c("Day"))
summary_avg_Perf_Colors <- summarySE(Colors_avgPerf, measurevar = "Perf_corr_RAW", groupvars = c("Day"))

#Plotting the overlap together (paper) ---------
# Combine the two data frames with a new 'group' column
combined_df <- bind_rows(
  mutate(summary_avg_Perf_Battery, Feedback = "Battery"),
  mutate(summary_avg_Perf_Colors, Feedback = "Colors")
)

# Define custom RGB colors
colors <- c("Battery" = rgb(59, 179, 113, maxColorValue = 255), 
            "Colors" = rgb(170, 21, 70, maxColorValue = 255))

# Plot
avgp <- ggplot(combined_df, aes(x = Day, y = Perf_corr_RAW, color = Feedback)) +
  geom_smooth(data = Battery_avgPerf, aes(x = Day, y = Perf_corr_RAW, color= Day), method = "lm", show.legend = FALSE, 
              linetype = "dashed", size = 1, alpha = 0.3, se = FALSE, color = "grey60") +
  geom_smooth(data = Colors_avgPerf, aes(x = Day, y = Perf_corr_RAW, color= Day), method = "lm", show.legend = FALSE, 
              linetype = "dashed", size = 1, alpha = 0.3, se = FALSE, color = "grey60") +
  geom_line(aes(group = Feedback), position = position_dodge(width = 0.1), size = 2) +  
  geom_errorbar(aes(group = Feedback, ymin = Perf_corr_RAW - se, ymax = Perf_corr_RAW + se), 
                width = 0, size = 1, position = position_dodge(width = 0.1)) +
  geom_point(aes(group = Feedback), size = 5, position = position_dodge(width = 0.1), colour= "black") +  # Dodge points according to group
  labs(x = "Day",
       y = "BCI Performance [%]") +
  annotate(geom = "text", x = 4, y = 60, label = "n = 15", size = 6, color = colors["Battery"]) +  # Annotation for n = 15 in green
  annotate(geom = "text", x = 4, y = 49, label = "n = 10", size = 6, color = colors["Colors"]) +  
  theme(axis.line = element_line(size = 1), panel.border = element_blank(),
        axis.text = element_text(face = "bold"),
        axis.line.x = element_line(color = "black"), 
        axis.line.y = element_line(color = "black"),  
        axis.line.x.top = element_blank(), 
        axis.line.y.right = element_blank()) +  
  scale_color_manual(values = colors) +
  scale_y_continuous(breaks = seq(44, 64, by = 2)) + 
  
  theme_SM

avgp

# Exported in 500 by 400 in SVG format and then converted to PNG

# Plotting the overlap separately ----
# Plot for the Battery ----

pd <- position_dodge(0.1) # move them .05 to the left and right


avgp <- ggplot() + 
  coord_cartesian(ylim = c(45, 64)) +
  geom_point(data = Battery_avgPerf, aes(x = Day, y = Perf_corr_RAW), color = "black", show.legend = FALSE,
             shape = 18, size = 1, alpha = .0) +
  geom_smooth(data = Battery_avgPerf, aes(x = Day, y = Perf_corr_RAW, color= Day), method = "lm", show.legend = FALSE, 
              linetype = "dashed", size = 1, alpha = 0.3, se = FALSE, color = "grey60") +
  geom_errorbar(data = summary_avg_Perf_Battery, aes(x = Day, y = Perf_corr_RAW, ymin = Perf_corr_RAW - se, ymax = Perf_corr_RAW + se), 
                width = 0, position = pd, size = 1, show.legend = FALSE, color = rgb(59,179,113, maxColorValue = 255), alpha = 1) +
  geom_line(data = summary_avg_Perf_Battery, aes(x = Day, y = Perf_corr_RAW), position = pd, size = 2, color = rgb(59,179,113, maxColorValue = 255)) + 
  geom_point(data = summary_avg_Perf_Battery, aes(x = Day, y = Perf_corr_RAW), position = pd, size = 5) + 
  theme_SM +
  labs(x = "Day", y = "BCI Performance [%]")

avgp = avgp + theme(axis.line = element_line(size = 1), panel.border = element_blank(),
             axis.text = element_text(face = "bold")) + 
  scale_color_gradient(low = "#F22738", high = "#73020C") +
  annotate(geom = "text", x = 4, y = 60, label = "n = 15", size = 6, color = rgb(59,179,113, maxColorValue = 255))


# Plot for the Colors ----

avgp = avgp + 
  geom_point(data = Colors_avgPerf, aes(x = Day, y = Perf_corr_RAW), color = "black", show.legend = FALSE,
             shape = 18, size = 1, alpha = .0) +
  geom_smooth(data = Colors_avgPerf, aes(x = Day, y = Perf_corr_RAW, color= Day), method = "lm", show.legend = FALSE, 
              linetype = "dashed", size = 1, alpha = 0.3, se = FALSE, color = "grey60") +
  geom_errorbar(data = summary_avg_Perf_Colors, aes(x = Day, y = Perf_corr_RAW, ymin = Perf_corr_RAW - se, ymax = Perf_corr_RAW + se), 
                width = 0, position = pd, size = 1, show.legend = FALSE, color = rgb(170, 21, 70, maxColorValue = 255), alpha = 1) +
  geom_line(data = summary_avg_Perf_Colors, aes(x = Day, y = Perf_corr_RAW), position = pd, size = 2, color = rgb(170, 21, 70, maxColorValue = 255)) + 
  geom_point(data = summary_avg_Perf_Colors, aes(x = Day, y = Perf_corr_RAW), position = pd, size = 5) 

avgp + theme(axis.line = element_line(size = 1), panel.border = element_blank(),
             axis.text = element_text(face = "bold")) + 
  scale_color_gradient(low = "#F22738", high = "#73020C") +
  annotate(geom = "text", x = 4, y = 49, label = "n = 10", size = 6, color = rgb(170, 21, 70, maxColorValue = 255)) + 
  scale_y_continuous(breaks = seq(44, 64, by = 2)) + # Set the tick interval to 10
  theme_SM

# Extracted file size is 350, 400



# Statistics (By average)-----------
Battery_aod <- aggregate(Perf_corr_RAW ~ Subject, data = Battery_avgPerf, FUN = mean)
Colors_aod <- aggregate(Perf_corr_RAW ~ Subject, data = Colors_avgPerf, FUN = mean)

test_result <- t.test(Battery_aod$Perf_corr_RAW, Colors_aod$Perf_corr_RAW, var.equal = FALSE)

mean1 <- mean(Battery_aod$Perf_corr_RAW)
mean2 <- mean(Colors_aod$Perf_corr_RAW)
sd1 <- sd(Battery_aod$Perf_corr_RAW)
sd2 <- sd(Colors_aod$Perf_corr_RAW)
n1 <- length(Battery_aod$Perf_corr_RAW)
n2 <- length(Colors_aod$Perf_corr_RAW)
pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
cohen_d <- abs(mean1 - mean2) / pooled_sd
cat("Cohen's d:", cohen_d, "\n")


# Also make bar plots ----

combined_data <- rbind(
  transform(Battery_aod, Dataset = "Battery_aod"),
  transform(Colors_aod, Dataset = "Colors_aod")
)

# Create the box and whisker plot with centered x-axis labels, SE bars, and average
plot <- ggplot(combined_data, aes(x = Dataset, y = Perf_corr_RAW, fill = Dataset)) +
  geom_boxplot() +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5) +
  labs(
    x = "",
    y = "BCI Performance [%]"
  ) +
  scale_fill_manual(
    values = c("Battery_aod" = rgb(59,179,113, maxColorValue = 255), "Colors_aod" = rgb(170, 21, 70, maxColorValue = 255))
  ) +
  scale_x_discrete(labels = c("Battery_aod" = "Battery", "Colors_aod" = "Colors"))

# Center x-axis labels and make SE bars bold and thick
plot + theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  theme(axis.line = element_line(size = 1), panel.border = element_blank(),
        axis.text = element_text(face = "bold")) +
  guides(fill = FALSE) +
  # geom_errorbar(data = se_data, aes(x = Dataset, ymin = avg - se, ymax = avg + se), width = 0.2, size = 1) +
  # geom_point(data = se_data, aes(x = Dataset, y = avg), color = "red", size = 3) +
  theme_SM

# picture extracted for 250 by 400 


# Statistics (By Slope)-----------
Battery_aod <- aggregate(Perf_corr_RAW ~ Subject, data = Battery_avgPerf, FUN = mean)
Colors_aod <- aggregate(Perf_corr_RAW ~ Subject, data = Colors_avgPerf, FUN = mean)

grouped_data_bat <- Battery_avgPerf %>%
  group_by(Subject)
grouped_data_col <- Colors_avgPerf %>%
  group_by(Subject)

# Fit a linear regression model for each subject and extract the coefficients
Battery_aod <- grouped_data_bat %>%
  do(tidy(lm(Perf_corr_RAW ~ Day, data = .))) %>%
  filter(term != "(Intercept)") 
Colors_aod <- grouped_data_col %>%
  do(tidy(lm(Perf_corr_RAW ~ Day, data = .))) %>%
  filter(term != "(Intercept)") 

test_result <- t.test(Battery_aod$estimate, Colors_aod$estimate, var.equal = FALSE)
# test_result <- wilcox.test(Battery_aod$estimate, Colors_aod$estimate)



mean1 <- mean(Battery_aod$estimate)
mean2 <- mean(Colors_aod$estimate)
sd1 <- sd(Battery_aod$estimate)
sd2 <- sd(Colors_aod$estimate)
n1 <- length(Battery_aod$estimate)
n2 <- length(Colors_aod$estimate)
pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
cohen_d <- abs(mean1 - mean2) / pooled_sd
cat("Cohen's d:", cohen_d, "\n")


# Also make bar plots ----

combined_data <- rbind(
  transform(Battery_aod, Dataset = "Battery_aod"),
  transform(Colors_aod, Dataset = "Colors_aod")
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
plot <- ggplot(combined_data, aes(x = Dataset, y = estimate, fill = Dataset)) +
  geom_boxplot() +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5) +
  labs(
    x = "",
    y = "BCI Performance [%]"
  ) +
  scale_fill_manual(
    values = c("Battery_aod" = rgb(59,179,113, maxColorValue = 255), "Colors_aod" = rgb(170, 21, 70, maxColorValue = 255))
  ) +
  scale_x_discrete(labels = c("Battery_aod" = "Battery", "Colors_aod" = "Colors"))

# Center x-axis labels and make SE bars bold and thick
plot + theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  theme(axis.line = element_line(size = 1), panel.border = element_blank(),
        axis.text = element_text(face = "bold")) +
  guides(fill = FALSE) +
  # geom_errorbar(data = se_data, aes(x = Dataset, ymin = avg - se, ymax = avg + se), width = 0.2, size = 1) +
  # geom_point(data = se_data, aes(x = Dataset, y = avg), color = "red", size = 3) +
  theme_SM

# picture extracted for 250 by 400 

