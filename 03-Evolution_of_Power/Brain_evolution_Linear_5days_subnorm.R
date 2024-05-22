
# Created on : 07-11-2022 
# Author: Kinkini Bhadra for the 'first version' of paper analysis

# ----------
# Context
# ----------
# The general hypothesis of the paper is that there is a training effect over the 5 days.
# So in this code we would like to use a general linear model or a linear mixed model on the
# Time- frequency data to see the effect/ changes in power over 5 days of training. 

# ----------------------------
# General details of the data
# ----------------------------
# The Time-frequency data extracted are baseline corrected and trial-averaged and converted into .csv
# for easy readability in R. The code used can be found in the path: 
# C:\Users\bhadra9\OneDrive - unige.ch (1)\Project_EEG-BCI\Scripts\scripts_battery_protocol\2022_11\Data_organization_4_R

# This is the version 2 of the previous analysis script. The only change that we do is in normalizing the data.
# Here we will normalize the data for each subject throughout the days with an underlying assumption that the nature
# of the data for each subject contains some similarity even if there is change over the days.
# So lets do it!


# Clear the workspace
rm(list = ls(all = TRUE))
graphics.off()



# Import all the necessary libraries in one line
my_packages = c("nlme", "stats", "ggplot2", "arm", "readr", "tidyverse",
                "rsq", "arm", "dplyr", "reshape", "scales")
lapply(my_packages, require, character.only = TRUE)



# Input and output file directories
#indir = 'C:/Users/bhadra9/OneDrive - unige.ch (1)/BCI_Group_shared/data/KB/Neural_Data_for_R_avg/' # OLD ONE
indir = 'C:/Users/bhadra9/OneDrive - unige.ch (1)/BCI_Group_shared/Code/KB/00_Reproducable_abstract_plots/Fig2_Power_Evolution/Data/'
setwd(indir) # used to change the directory

#outdir_data = 'C:/Users/bhadra9/OneDrive - unige.ch (1)/2022_Data_battery_protocol_outputs/2022_11/Data_4paper_v1/power_evo_over5days/'
outdir_data = 'C:/Users/bhadra9/OneDrive - unige.ch (1)/BCI_Group_shared/Code/KB/00_Reproducable_abstract_plots/Fig2_Power_Evolution/Output_data/Observed_data_v3/'

# outdir_plots = 'C:/Users/bhadra9/OneDrive - unige.ch (1)/2022_Data_battery_protocol_outputs/2022_11/Supporting_plots/power_evo_over5days/Version_two/'
outdir_plots = 'C:/Users/bhadra9/OneDrive - unige.ch (1)/BCI_Group_shared/Code/KB/00_Reproducable_abstract_plots/Fig2_Power_Evolution/Plot/control_plots/'


# Loading the files and gather data
data_files_list <- list.files(indir, glob2rx('*.csv'))

# Initializing variables
f_stat = t_stat = p_val = Chan2save = Freq2save = c()


# Loading the data
for (i_data in 1:length(data_files_list)) {
  all_data = read.csv(file = data_files_list[i_data])
  
  # Change the name NA to SNA: Silvia's comment yet to be applied, but for now, I manually chceked and its fine!
  which(is.na(all_data$Subject_Name))
  all_data$Subject_Name <- all_data$Subject_Name %>% replace_na('SNA')
  
  
  # Convert into factors
  all_data$Subject_Name = as.factor(all_data$Subject_Name)
  all_data$Day = as.factor(all_data$Day)
  all_data$chanel = as.factor(all_data$chanel)
  
  
  # useful information from the data to be used later
  unq_sub = unique(all_data$Subject_Name)
  unq_day = unique(all_data$Day)
  unq_chan = unique(all_data$chanel)
  freq_name = unique(all_data$Freq_Band)
  
  
  # Plot to see the raw data Power: Per Subject
  # ?????????????????????????????????????????????????????????????????????????????????????????????????????????????
  # Comment: participant 'YL' has very high alpha in the occipital electrodes. After looking at the clean data, it appears that the participant might have 
  # closed the eyes during the trial. Should we treat this participant as an outlier?
  ggplot(all_data, aes(x = Day , y = chanel, fill = Power)) +
    geom_tile(color = "grey") +
    facet_grid(. ~ Subject_Name) +
    scale_fill_gradient2(low = "blue",mid = 'white',  high = "red", limits = c(-100, 200)) +
    theme(axis.text.x = element_text(size=rel(0.8))) +
    ggtitle(freq_name, 'Raw Power')
  
  # Save the plot
  filename = as.character(paste(freq_name, "Raw Power.png"))
  # ggsave(path = outdir_plots, filename = filename, width = 16, height = 8)
  
  
  # Plot to see the raw data Power: Average
  # ?????????????????????????????????????????????????????????????????????????????????????????????????????????????
  # NOTE1: The geom_tile somehow gives weird color scales that are not mapped to its original values 
  # Therefore, in order to see the colour that is mapped to the tile values, therefore summarizing the 
  # values beforehand is a good idea. bUt check!
  # NOTE2: The data has outliers produced by the subject 'YL'; may be using median value instead of mean will be a good idea
  
  # Mean 
  all_data %>% group_by(Day, chanel) %>% summarize(Power_mean = mean(Power)) %>%
    ungroup() %>% 
    ggplot(aes(x=Day, y=chanel)) + geom_tile(aes(fill=Power_mean), color = "grey") +
    stat_summary_2d(aes(z = Power_mean, 
                        label = after_stat(number(value, accuracy = 0.01))),
                    fun = mean,geom = "text") +
    scale_fill_gradient2(low = "blue",mid = 'white',  high = "red", limits = c(-60, 60)) +
    theme(axis.text.x = element_text(size=rel(0.8))) +
    ggtitle(freq_name, 'Raw Power: mean')
  
  # Save the plot
  filename = as.character(paste(freq_name, "Raw Power_mean.png"))
  # ggsave(path = outdir_plots, filename = filename, width = 8, height = 10)
  
  
  # Median
  all_data %>% group_by(Day, chanel) %>% summarize(Power_mean = median(Power)) %>%
    ungroup() %>% 
    ggplot(aes(x=Day, y=chanel)) + geom_tile(aes(fill=Power_mean), color = "grey") +
    stat_summary_2d(aes(z = Power_mean, 
                        label = after_stat(number(value, accuracy = 0.01))),
                    fun = mean,geom = "text") +
    scale_fill_gradient2(low = "blue",mid = 'white',  high = "red", limits = c(-60, 1)) +
    theme(axis.text.x = element_text(size=rel(0.8))) +
    ggtitle(freq_name, 'Raw Power: median')
  
  # Save the plot
  filename = as.character(paste(freq_name, "Raw Power_median.png"))
  # ggsave(path = outdir_plots, filename = filename, width = 8, height = 10)
  
  
  # MANUAL TESTING OF THE AVERAGE VALUES: FOR SANITY CHECK only!
  # test_mean = test_median = c()
  # count = 0
  # for (i_data in 1:5) {  # ! WARNING: hardcoded here
  #   for (i_chan in 1:62) { # ! WARNING: hardcoded here
  #     chan_data <- filter(all_data, chanel == unq_chan[i_chan] , Day == i_data)
  #     count = count + 1
  #     test_mean[count] = mean(chan_data$Power)
  #     test_median[count] = median(chan_data$Power)
  #   }
  # }
  # # Find the min and max
  # min(test_mean)
  # max(test_mean)
  
  # Normalizing the data
  # -----------------------------------------------------------------------------
  # Z-score: The function used for this is 'scale' and is default function in R 
  # -----------------------------------------------------------------------------
  
  # Per Subject and per Day Normalization
  all_z_score_norm = all_min_max_norm = all_min_max_minus_norm = all_z_score_norm_out_exc = c()
  
  for (i_sub in 1: length(unq_sub)) {

      # Filter the data and normalize
      sub_data <- filter(all_data, Subject_Name == unq_sub[i_sub] ) # 62 values from 62 channels
      
      z_score_norm = scale(sub_data$Power)
      all_z_score_norm = c(all_z_score_norm, z_score_norm)
      
      # We do the removing of outliers here
      lower_bound <- quantile(z_score_norm, 0.025)
      lower_bound
      
      upper_bound <- quantile(z_score_norm, 0.975)
      upper_bound
      
      # Assign the normalized values to another variable and replace with NA based on threshold 
      z_score_norm_out_excl = z_score_norm
      z_score_norm_out_excl[z_score_norm_out_excl > upper_bound ] <- NA
      z_score_norm_out_excl[z_score_norm_out_excl < lower_bound ] <- NA
      all_z_score_norm_out_exc = c(all_z_score_norm_out_exc, z_score_norm_out_excl)
      
  }
  
  # Having all the data in the same data frame
  all_data$z_score_norm = all_z_score_norm
  all_data$min_max_norm = all_min_max_norm  
  all_data$min_max_minus_norm = all_min_max_minus_norm
  all_data$z_score_norm_out_exc = all_z_score_norm_out_exc
  
  
  # # Check the distribution
  # for (i_sub in 1: length(unq_sub)) {
  #   for (i_day in 1: length(unq_day)) {
  #     
  #     # Filter the data and normalize
  #     sub_data <- filter(all_data, Subject_Name == unq_sub[i_sub] & Day == unq_day[i_day] )
  #     
  #     hist(sub_data$z_score_norm, breaks=12, col="red")
  #     
  #     d = density(sub_data$z_score_norm)
  #     plot(d)
  #     
  #   }
  # }
  
  
  # Some plots to check the normalized data: Per subject
  ggplot(all_data, aes(x = Day , y = chanel, fill = z_score_norm)) +
    geom_tile(color = "grey") +
    facet_grid(. ~ Subject_Name) +
    scale_fill_gradient2(low = "blue",mid = 'white',  high = "red") +
    theme(axis.text.x = element_text(size=rel(0.8))) +
    ggtitle(freq_name, 'Norm: z-scored')
  
  # Save the plot
  filename = as.character(paste(freq_name, "Norm_z-scored.png"))
  ggsave(path = outdir_plots, filename = filename, width = 16, height = 8)
  
  
  # Some plots to check the normalized data: Average
  all_data %>% group_by(Day, chanel) %>% summarize(Power_mean = mean(z_score_norm)) %>%
    ungroup() %>% 
    ggplot(aes(x=Day, y=chanel)) + geom_tile(aes(fill=Power_mean), color = "grey") +
    stat_summary_2d(aes(z = Power_mean, 
                        label = after_stat(number(value, accuracy = 0.01))),
                    fun = mean,geom = "text") +
    scale_fill_gradient2(low = "blue",mid = 'white',  high = "red") +
    theme(axis.text.x = element_text(size=rel(0.8))) +
    ggtitle(freq_name, 'Norm: z-scored')
  
  # Save the plot
  filename = as.character(paste(freq_name, "Norm_z-scored_Mean.png"))
  ggsave(path = outdir_plots, filename = filename, width = 8, height = 10)
  
  
  
  # ???????????????????????????????????????????????????????????????????????????????????????????????????????????????????
  # COMMENT : All types of normalization makes the data look very similar, which is great!!
  # But I think as the power values could be positive and negative, using either of the z-scored data or the 
  # or minmax -1 to +1 might be a better idea. Again, the -1 to +1 normalization follows a linear scale and therefore not all 
  # negative raw power values correspond to a negative normalized values, in this case, using z-scored normalized value might 
  # be the best as (a) Its more statistically relevant, (b) standard practive in most of the papers including Tim's 
  
  
  # Removing the outliers i.e. z_score above +3 or -3
  # all_data$z_score_norm[all_data$z_score_norm >  3 ] <- NA
  # all_data$z_score_norm[all_data$z_score_norm < -3 ] <- NA
  # 
  # # Plot to check after removing the outliers: Per Subject
  # ggplot(all_data, aes(x = Day , y = chanel, fill = z_score_norm)) +
  #   geom_tile(color = "grey") +
  #   facet_grid(. ~ Subject_Name) +
  #   scale_fill_gradient2(low = "blue",mid = 'white',  high = "red") +
  #   theme(axis.text.x = element_text(size=rel(0.8))) +
  #   ggtitle(freq_name, 'Norm: z-scored_outlier_Removed')
  # 
  # filename = as.character(paste(freq_name, "Norm_z-scored_outlier_Removed.png"))
  # ggsave(path = outdir_plots, filename = filename, width = 16, height = 8)
  # 
  # 
  # # Plot to check after removing the outliers: Average
  # all_data %>% group_by(Day, chanel) %>% summarize(Power_mean = mean(z_score_norm)) %>%
  #   ungroup() %>% 
  #   ggplot(aes(x=Day, y=chanel)) + geom_tile(aes(fill=Power_mean), color = "grey") +
  #   stat_summary_2d(aes(z = Power_mean, 
  #                       label = after_stat(number(value, accuracy = 0.01))),
  #                   fun = mean,geom = "text") +
  #   scale_fill_gradient2(low = "blue",mid = 'white',  high = "red") +
  #   theme(axis.text.x = element_text(size=rel(0.8))) +
  #   ggtitle(freq_name, 'Norm: z-scored_Outlier_Removed')
  # 
  # # Save the plot
  # filename = as.character(paste(freq_name, "Norm_z-scored_Mean_outlier_Removed.png"))
  # ggsave(path = outdir_plots, filename = filename, width = 8, height = 10)
  
  
  # But the data is a bit skewed and therefore we cannot consider the +/-3 which is applicable only for a
  # Gaussian Distribution. So, I will find the lower and upper bound based on the distribution I have and 
  # remove the data points outside these values. Since this is subjected to individual days and subjects
  # we need to do it inside the subject and day loop.
  
  
  # For distribution checking
  for (i_chan in 1:length(unq_chan)) {
    
    chan_name = as.character(unq_chan[i_chan])
    
    chan_data <- filter(all_data, chanel == chan_name)
    
    # hist(chan_data$z_score_norm_out_exc, breaks=12, col="red")
    
    ggplot(chan_data, aes(z_score_norm_out_exc)) + 
      geom_histogram() + 
      facet_wrap(~Day)
    
    chan_data$Day = as.factor(chan_data$Day)
    ggplot(chan_data, aes(Day, z_score_norm_out_exc)) + 
      geom_boxplot()
    
    ggplot(chan_data, aes(z_score_norm_out_exc)) + 
      geom_histogram()
    
  }
  
  
  # For each channel there are 15 values per day and we can model them over the days
  for (i_chan in 1:length(unq_chan)) {
    
    chan_name = as.character(unq_chan[i_chan])
    
    chan_data <- filter(all_data, chanel == chan_name)
    
    
    # Some more plotting
    chan_data$time_contrast <- ifelse(chan_data$Day == '1', '-2',
                                      ifelse(chan_data$Day == '2','-1',
                                             ifelse(chan_data$Day == '3','0',
                                                    ifelse(chan_data$Day == '4','1',
                                                           ifelse(chan_data$Day == '5','2','bug')))))
    chan_data$time_contrast = as.numeric(chan_data$time_contrast)
    # levels(chan_data$time_contrast)
    
    test_model2 = lmerTest::lmer(z_score_norm_out_exc ~ time_contrast + (1|Subject_Name), data = chan_data, REML = FALSE, na.action = na.exclude)
    
    # Trying with LME
    # M3_avg_data_lme <-lme(z_score_norm_out_exc ~ time_contrast, data = chan_data, 
    #                       random = ~ time_contrast|Subject_Name, 
    #                       na.action = na.exclude, control = list(opt="optim"), method = "ML") 
    # 
    # summary(M3_avg_data_lme)
    
    # Getting the p-values 
    # Code: https://www.r-bloggers.com/2014/02/three-ways-to-get-parameter-specific-p-values-from-lmer/
    coefs <- data.frame(coef(summary(test_model2)))
    # use normal distribution to approximate p-value
    coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
    coefs
    
    
    test = summary(test_model2)
    sum = anova(test_model2)
    
    
    # Trying the GAM model -----> NOT WORKING WELL YET!
    # GAM1 <- gam(Power ~ Day, method = "REML", data = chan_data)
    # summary(GAM1)
    # plot_smooth(model1, view = "Power", rug = F, plot_all = "Day", main = "")
    
    
    # making list of the needed variables
    # Save the outputs in a list  (FOR LMER model)
    f_stat[i_chan] = sum$`F value`
    p_val[i_chan] = coefs$p.z[2]
    Chan2save[i_chan] = chan_name
    Freq2save[i_chan] = freq_name
    t_stat[i_chan] = coefs$t.value[2]
    
    
    evol_neural_dat = data.frame(Freq2save, Chan2save, f_stat, t_stat, p_val)
    
    
    filename = paste(outdir_data, paste('evol_neural_dat_z_scored_removed_outliers_IQR_normsub', all_data$Freq_Band[1], '.csv', sep = '_'), sep = '/')
    write_csv(evol_neural_dat, filename)
    
  } # i_chan
} # i_data

