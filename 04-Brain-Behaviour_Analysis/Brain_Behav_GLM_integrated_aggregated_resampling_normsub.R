# Context: This code is to carry out the GAM analysis at one stage for better interpretation
# Created on : 21-10-2022 by Kinkini Bhadra
# Trying on averaged data

# Created on : 08-11-2022 
# Author: Kinkini Bhadra for the 'first version' of paper analysis

# ----------
# Context
# ----------
# The general hypothesis of the paper is that there is a training effect over the 5 days.
# So in this code we would like to use a general linear model or a linear mixed model on the
# Time- frequency data and the BCI Behaviour (i.e. BCI performance) to see  if there is any
# correlation between the two over 5 days of training. 

# ***** AS I HAD USED ZSCORE NORMALIZATION ON THE POWER DATA BEFORE, I WILL USE THE SAME METHOD OF 
# NORMALIZATION IN THIS ANALYSIS TOO *********

# ----------------------------
# General details of the data
# ----------------------------
# The Time-frequency data extracted are baseline corrected and trial-averaged and converted into .csv
# for easy readability in R. The code used can be found in the path: 
# C:\Users\bhadra9\OneDrive - unige.ch (1)\Project_EEG-BCI\Scripts\scripts_battery_protocol\2022_11\Data_organization_4_R



# Some codes to clear the workspace
rm(list = ls(all = TRUE))
graphics.off()

# Import all the necessary libraries in one line
my_packages = c("nlme", "stats", "ggplot2", "arm", "readr", "tidyverse",
                "rsq", "arm", "dplyr", "tictoc", "itsadug", "ggpubr")
lapply(my_packages, require, character.only = TRUE)


# Input and output file directories
# indir = 'C:/Users/bhadra9/OneDrive - unige.ch (1)/2022_Data_battery_protocol_outputs/2022_11/Data_4paper_v1/Brain_Behaviour/Data_v2/'
indir = 'C:/Users/bhadra9/OneDrive - unige.ch (1)/2022_Data_battery_protocol_outputs/2022_11/Data_4paper_v1/Brain_Behaviour/Data_v2/'

setwd(indir) # used to change the directory

# outdir_data = 'C:/Users/bhadra9/OneDrive - unige.ch (1)/2022_Data_battery_protocol_outputs/2022_11/Data_4paper_v1/Brain_Behaviour/Resampled_Data_v2/'
outdir_data = ''

# Loading the files and gather data from the folder. Ideally this needs to be done in a loop like in MATLAB
# Listing the subject folder
Freq_folder <- list.files(path = ".",
                          full.names = TRUE,
                          recursive = FALSE) # recursive 'false' lists down only the subject folders and not the extras

# Getting into the subject folder
for (i_freq in 1: length(Freq_folder)) { #length(Freq_folder)
  
  all_data = read.csv(file = Freq_folder[i_freq])
  
  # useful information from the data to be used later
  unq_sub = unique(all_data$Subject)
  unq_day = unique(all_data$Day)
  unq_chan = unique(all_data$Channel)
  freq_name = unique(all_data$Freq_band)
  
  # Convert to factors
  all_data$Subject = as.factor(all_data$Subject)
  all_data$Day = as.numeric(all_data$Day)
  
  
  
  # -------------------------------- RESAMPLING ---------------------------------------
  # Resampling parameters
  
  numrandomization = 1000
  numsub = 15 # 15 subjects
  numfact = 5 # 5 days
  
  # We will permute the data for each subject over the days, i.e. we will shuffle the day 
  # information in a within subject data. We do this for each channel and therefore this code must 
  # be included inside the loop.
  
  
  # -------------------------------------------------------------------
  # Initializing the variables to be saved later
  Subject = Freq = p_val_time = t_val_time = f_value_time = chan = df_val_time =  
    p_val_interctn = t_val_interctn = f_value_interctn = df_val_interctn =  Est_corr = p_val_corr = c()
  
  
  # variables that are to be saved need pre-allocation of freq name and channel name 
  Freq = replicate(62, freq_name)
  df_val_time = data.frame(Freq, unq_chan)
  t_val_time = data.frame(Freq, unq_chan)
  p_val_time = data.frame(Freq, unq_chan)
  f_value_time = data.frame(Freq, unq_chan)
  Est_corr = data.frame(Freq, unq_chan)
  p_val_corr = data.frame(Freq, unq_chan)
  p_val_interctn = data.frame(Freq, unq_chan)
  t_val_interctn = data.frame(Freq, unq_chan)
  df_val_interctn = data.frame(Freq, unq_chan)
  f_value_interctn = data.frame(Freq, unq_chan)
  p_val_pow = data.frame(Freq, unq_chan)
  t_val_pow = data.frame(Freq, unq_chan)
  
  
  
  # Modelling of the data will be done for each channel
  for (i_chan in 1:length(unq_chan)) { # 
    
    chan_name = as.character(unq_chan[i_chan])
    
    # Filter the data based on each channel
    chan_data <- filter(all_data, Channel == chan_name)
    
    # Some plotting to check the power over performance
    # NOTE: The scaled performance data does not effect the slope, so we use the raw performance data
    
    chan_data$time_contrast<- ifelse(chan_data$Day == '1', '-2',
                                     ifelse(chan_data$Day == '2','-1',
                                            ifelse(chan_data$Day == '3','0',
                                                   ifelse(chan_data$Day == '4','1',
                                                          ifelse(chan_data$Day == '5','2','bug')))))
    chan_data$time_contrast = as.numeric(chan_data$time_contrast)
    
    
    tic("Random loop start")
    for (i_rand in 1:numrandomization) { # 
      
      # Channel data to be reshuffled and then we model the data accordingly for each iteration
      all_shuffled_data = c()
      for (i_sub in 1:length(unq_sub)) {# 
        sub_name = unq_sub[i_sub]
        sub_data = filter(chan_data, Subject == sub_name)
        shuffled_data= sample(sub_data$norm_pow_exc_out) # Shuffling the data
        
        all_shuffled_data = c(all_shuffled_data, shuffled_data)
      }
      
      chan_data$perm_dat = all_shuffled_data
      
      # GLM modelling
      test_model2 = lmerTest::lmer(scaled_perf ~ perm_dat + time_contrast + perm_dat:time_contrast 
                                   + (1|Subject), data = chan_data, REML = FALSE, na.action = na.exclude)
      
      # Correlation for the power and performance
      corr = cor.test(chan_data$perm_dat, chan_data$scaled_perf, method = c("pearson"))
      
      
      # Start working from here: 
      t = anova(test_model2)
      t1 = summary(test_model2)
      
      
      # making list of the needed variables
      
      p_pow    = t$`Pr(>F)`[1]
      t_pow    = t1$coefficients[14]
      
      p_time = t$`Pr(>F)`[2]
      df_time  = t$DenDF[2]
      t_time   = t1$coefficients[15]
      f_time = t$`F value`[2]
      chan    = chan_name
      
      p_corr   = corr$p.value             
      Betacorr    = corr$estimate[["cor"]] 
      
      t_interctn  = t1$coefficients[16]
      f_interctn = t$`F value`[3]
      df_interctn  =  t$DenDF[3]
      p_interctn  = t$`Pr(>F)`[3]
      
      # data frame for the p_val_pow
      p_val_pow[i_chan, 2+ i_rand]   <- p_pow
      colnames(p_val_pow)[ncol(p_val_pow)] <- paste0("pval", i_rand)  # Rename column name
      
      # data frame for the t_val_pow
      t_val_pow[i_chan, 2+ i_rand]   <- t_pow
      colnames(t_val_pow)[ncol(t_val_pow)] <- paste0("tstat", i_rand)  # Rename column name
      
      
      # data frame for the t_val_time
      t_val_time[i_chan, 2+ i_rand]  <- t_time                  # Append new column
      colnames(t_val_time)[ncol(t_val_time)] <- paste0("tstat", i_rand)  # Rename column name
      
      # Data frame for the pvalues
      p_val_time[i_chan, 2+ i_rand]  <- p_time                  # Append new column
      colnames(p_val_time)[ncol(p_val_time)] <- paste0("pval", i_rand)  # Rename column name
      
      # Data frame for the fvalues
      f_value_time[i_chan, 2+ i_rand] <- f_time                  # Append new column
      colnames(f_value_time)[ncol(f_value_time)] <- paste0("fstat", i_rand)  # Rename column name
      
      
      # Data frame for the beta correlates
      Est_corr[i_chan, 2+ i_rand] <- Betacorr                  # Append new column
      colnames(Est_corr)[ncol(Est_corr)] <- paste0("Bcorr", i_rand)  # Rename column name
      
      # Data frame for the pval corre
      p_val_corr[i_chan, 2+ i_rand] <- p_corr                 # Append new column
      colnames(p_val_corr)[ncol(p_val_corr)] <- paste0("pcorr", i_rand)  # Rename column name
      
      
      # Data frame for the tstat
      t_val_interctn[i_chan, 2+ i_rand] <- t_interctn                  # Append new column
      colnames(t_val_interctn)[ncol(t_val_interctn)] <- paste0("tstat", i_rand)  # Rename column name
      
      # Data frame for the fstat
      f_value_interctn[i_chan, 2+ i_rand] <- f_interctn                  # Append new column
      colnames(f_value_interctn)[ncol(f_value_interctn)] <- paste0("fstat", i_rand)  # Rename column name
      
      # Data frame for the Degress of freedom
      p_val_interctn[i_chan, 2+ i_rand] <- p_interctn                  # Append new column
      colnames(p_val_interctn)[ncol(p_val_interctn)] <- paste0("pval", i_rand)  # Rename column name
      
      
    } # i_chan
    toc() 
  } # i_rand
  
  # Save the t-stats
  filename = paste(outdir_data, paste('p_val_pow_resampled', freq_name, '.csv', sep = '_'), sep = '/')
  write_csv(p_val_pow, filename)
  
  # Save the p-values
  filename = paste(outdir_data, paste('t_val_pow_resampled', freq_name, '.csv', sep = '_'), sep = '/')
  write_csv(t_val_pow, filename)
  
  # Save the t-stats
  filename = paste(outdir_data, paste('t_val_time_resampled', freq_name, '.csv', sep = '_'), sep = '/')
  write_csv(t_val_time, filename)
  
  # Save the p-values
  filename = paste(outdir_data, paste('p_val_time_resampled', freq_name, '.csv', sep = '_'), sep = '/')
  write_csv(p_val_time, filename)
  
  # Save the p-values
  filename = paste(outdir_data, paste('f_value_time_resampled', freq_name, '.csv', sep = '_'), sep = '/')
  write_csv(f_value_time, filename)
  
  # Save the t-stats
  filename = paste(outdir_data, paste('Est_corr_resampled', freq_name, '.csv', sep = '_'), sep = '/')
  write_csv(Est_corr, filename)
  
  # Save the p-values
  filename = paste(outdir_data, paste('p_val_corr_resampled', freq_name, '.csv', sep = '_'), sep = '/')
  write_csv(p_val_corr, filename)
  
  # Save the p-values
  filename = paste(outdir_data, paste('t_val_interctn_resampled', freq_name, '.csv', sep = '_'), sep = '/')
  write_csv(t_val_interctn, filename)
  
  # Save the p-values
  filename = paste(outdir_data, paste('f_value_interctn_resampled', freq_name, '.csv', sep = '_'), sep = '/')
  write_csv(f_value_interctn, filename)
  
  # Save the p-values
  filename = paste(outdir_data, paste('p_val_interctn_resampled', freq_name, '.csv', sep = '_'), sep = '/')
  write_csv(p_val_interctn, filename)
  
  
  
} # i_freq








