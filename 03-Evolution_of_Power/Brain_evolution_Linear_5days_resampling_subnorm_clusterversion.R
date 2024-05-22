
# Created on : 08-11-2022 
# Author: Kinkini Bhadra for the 'first version' of paper analysis

# ----------
# Context
# ----------
# This is the version2 of the previous script where we will use permutation between different groups and attempt the analysis such that we 
# find a threshold for the multiple comparison correction!
# The general hypothesis of the paper is that there is a training effect over the 5 days.
# So in this code we would like to use a general linear model or a linear mixed model on the
# Time- frequency data to see the effect/ changes in power over 5 days of training. 

# ***** A LOT OF DATA RELATED IMPORTANT INFORMATION AND SANITY CHECK PLOTS HAVA BEEN REMOVED FROM THIS CODE
# IN ORDER TO KEEP THE CODE CLEAN. PLEASE REFER TO THE V1 VERSION OF THIS CODE TO CHCEK THEM! *********

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
                "rsq", "arm", "dplyr", "tictoc", "lmerTest")
lapply(my_packages, require, character.only = TRUE)



# Input and output file directories
indir = 'C:/Users/bhadra9/OneDrive - unige.ch (1)/BCI_Group_shared/data/KB/Neural_Data_for_R_avg/'
setwd(indir) # used to change the directory

outdir_data = 'C:/Users/bhadra9/OneDrive - unige.ch (1)/2022_Data_battery_protocol_outputs/2022_11/Data_4paper_v1/power_evo_over5days/Resampled_data_v2/'
# outdir_plots = 'C:/Users/bhadra9/OneDrive - unige.ch (1)/2022_Data_battery_protocol_outputs/2022_11/Supporting_plots/power_evo_over5days/'


# Loading the files and gather data
data_files_list <- list.files(indir, glob2rx('*.csv'))



# Loading the data
for (i_data in 1) { # :length(data_files_list
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
  
  
  # Normalizing the data IN Z-SCORE
  
  # -----------------------------------------------------------------------------
  # 3. Z-score: The function used for this is 'scale' and is default function in R 
  # -----------------------------------------------------------------------------
  
  # Per Subject and per Day Normalization
  all_z_score_norm = all_z_score_norm_out_exc = c()
  
  for (i_sub in 1: length(unq_sub)) {
    
    # Filter the data and normalize
    sub_data <- filter(all_data, Subject_Name == unq_sub[i_sub])
    
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
  all_data$z_score_norm_out_exc = all_z_score_norm_out_exc
  
  
  # ?????????????????????????????????????????????????????????????????????????????????????
  # NOTE: The statobs is already present from the v1 of the script. Now we need to create 
  # an output called statrand that is of dimension (62 by 1000; nrand= 1000)
  
  
  # -------------------------------- RESAMPLING ---------------------------------------
  # Resampling parameters
  
  numrandomization = 1000
  numsub = 15 # 15 subjects
  numfact = 5 # 5 days
  
  # We will permute the data for each subject over the days, i.e. we will shuffle the day 
  # information in a within subject data. We do this for each channel and therefore this code must 
  # be included inside the loop.
  
  # Initializing variables
  evol_neural_dat_pval = evol_neural_dat_df = evol_neural_dat_tstat = Chan2save = Freq2save = c()
  f_stat = t_stat = p_val = Chan2save = Freq2save = df = c()
  
  Freq2save = replicate(62, freq_name)
  evol_neural_dat_tstat = data.frame(Freq2save, unq_chan)
  evol_neural_dat_pval = data.frame(Freq2save, unq_chan)
  evol_neural_dat_df = data.frame(Freq2save, unq_chan)
  
  
  
  # For each channel there are 15 values per day and we can model them over the days
  for (i_chan in 1: length(unq_chan)) { # 
    chan_name = as.character(unq_chan[i_chan])
    
    chan_data <- filter(all_data, chanel == chan_name)
    
    # contrast
    chan_data$time_contrast <- ifelse(chan_data$Day == '1', '-2',
                                      ifelse(chan_data$Day == '2','-1',
                                             ifelse(chan_data$Day == '3','0',
                                                    ifelse(chan_data$Day == '4','1',
                                                           ifelse(chan_data$Day == '5','2','bug')))))
    chan_data$time_contrast = as.numeric(chan_data$time_contrast)
    # levels(chan_data$time_contrast)
    
    tic("Random loop start") 
    for (i_rand in 1: numrandomization) { # 
      
      # Channel data to be reshuffled and then we model the data accordingly for each iteration
      all_shuffled_data = c()
      for (i_sub in 1:length(unq_sub)) {
        sub_name = unq_sub[i_sub]
        sub_data = filter(chan_data, Subject_Name == sub_name)
        shuffled_data= sample(sub_data$z_score_norm_out_exc) # Shuffling the data
        
        all_shuffled_data = c(all_shuffled_data, shuffled_data)
      }
      
      chan_data$perm_dat = all_shuffled_data
      
      
      test_model2 = lmerTest::lmer(perm_dat ~ time_contrast + (1|Subject_Name), data = chan_data, REML = FALSE, na.action = na.exclude)
      
      
      # Getting the p-values 
      # Code: https://www.r-bloggers.com/2014/02/three-ways-to-get-parameter-specific-p-values-from-lmer/
      coefs <- data.frame(coef(summary(test_model2)))
      # use normal distribution to approximate p-value
      coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
      coefs
      
      summary(test_model2)
      
      test = summary(test_model2)
      sum = anova(test_model2)
      
      
      # making list of the needed variables
      # Save the outputs in a list  (FOR LMER model)
      f_stat[i_rand] = sum$`F value`
      p_val = coefs$p.z[2]
      t_stat = coefs$t.value[2]
      df = sum$DenDF
      
      
      # data frame for the t-stat
      evol_neural_dat_tstat[i_chan, 2+ i_rand] <- t_stat                  # Append new column
      colnames(evol_neural_dat_tstat)[ncol(evol_neural_dat_tstat)] <- paste0("tstat", i_rand)  # Rename column name
      
      # Data frame for the pvalues
      evol_neural_dat_pval[i_chan, 2+ i_rand] <- p_val                  # Append new column
      colnames(evol_neural_dat_pval)[ncol(evol_neural_dat_pval)] <- paste0("p_val", i_rand)  # Rename column name
      
      # Data frame for the Degress of freedom
      evol_neural_dat_df[i_chan, 2+ i_rand] <- df                  # Append new column
      colnames(evol_neural_dat_df)[ncol(evol_neural_dat_df)] <- paste0("df", i_rand)  # Rename column name
      
      
    } # i_chan
    toc() 
  } # i_rand
  
  # Save the t-stats
  filename = paste(outdir_data, paste('evol_neural_dat_z_scored_removed_outliers_IQR_resampled_tstat', all_data$Freq_Band[1], '.csv', sep = '_'), sep = '/')
  write_csv(evol_neural_dat_tstat, filename)
  
  # Save the p-values
  filename = paste(outdir_data, paste('evol_neural_dat_z_scored_removed_outliers_IQR_resampled_pval', all_data$Freq_Band[1], '.csv', sep = '_'), sep = '/')
  write_csv(evol_neural_dat_pval, filename)
  
  # Save the p-values
  filename = paste(outdir_data, paste('evol_neural_dat_z_scored_removed_outliers_IQR_resampled_df', all_data$Freq_Band[1], '.csv', sep = '_'), sep = '/')
  write_csv(evol_neural_dat_df, filename)
  
} # i_data

