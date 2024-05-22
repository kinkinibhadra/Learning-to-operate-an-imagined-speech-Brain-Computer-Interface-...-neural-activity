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
                "rsq", "arm", "dplyr", "tictoc", "itsadug")
lapply(my_packages, require, character.only = TRUE)


# Input and output file directories
indir = 'C:/Users/bhadra9/OneDrive - unige.ch (1)/BCI_Group_shared/Code/KB/00_Reproducable_abstract_plots/Fig3_Brain-Behaviour/Data_v3/'
# indir = 'C:/Users/bhadra9/OneDrive - unige.ch (1)/BCI_Group_shared/data/KB/Neural_Data_for_R_withtrials/'
# indir = '/home/users/b/bhadra/01_MyData/Rdata/Brain_Behav/'
setwd(indir) # used to change the directory

#outdir_data = 'C:/Users/bhadra9/OneDrive - unige.ch (1)/#2022_Data_battery_protocol_outputs/2022_11/Data_4paper_v1/#Brain_Behaviour/Data_v2/'
outdir_data = 'C:/Users/bhadra9/OneDrive - unige.ch (1)/BCI_Group_shared/Code/KB/00_Reproducable_abstract_plots/Fig3_Brain-Behaviour/Data_v32/'

#obv_data    = 'C:/Users/bhadra9/OneDrive - unige.ch (1)/#2022_Data_battery_protocol_outputs/2022_11/Data_4paper_v1/#Brain_Behaviour/Observed_Datav2/'
obv_data = 'C:/Users/bhadra9/OneDrive - unige.ch (1)/BCI_Group_shared/Code/KB/00_Reproducable_abstract_plots/Fig3_Brain-Behaviour/Output_data/Observed_Datav3/'

# Loading the files and gather data from the folder. Ideally this needs to be done in a loop like in MATLAB
# Listing the subject folder
Freq_folder <- list.dirs(path = ".",
                         full.names = TRUE,
                         recursive = FALSE) # recursive 'false' lists down only the subject folders and not the extras

# Getting into the subject folder
for (i_freq in 1: length(Freq_folder)) { #length(Freq_folder)
  
  setwd(file.path(indir, Freq_folder[i_freq]))
  
  tmp_dir1 <- getwd()
  
  # List the files
  data_files_list <- list.files(tmp_dir1, glob2rx('*.csv'))
  
  all_data = c()
  for (i_data in 1: length(data_files_list) ) { # length(data_list)
    data = read.csv(file = data_files_list[i_data])
    all_data = rbind(all_data, data)
    
  } # i_data
  
  # Still a bad way of doing it, but change it later
  which(is.na(all_data$Subject_Name))
  all_data$Subject_Name <- all_data$Subject_Name %>% replace_na('SNA')
  
  
  # useful information from the data to be used later
  unq_sub = unique(all_data$Subject_Name)
  unq_day = unique(all_data$Day)
  unq_chan = unique(all_data$Channel)
  freq_name = unique(all_data$Freq_Band)
  
  
  # Aggregating the data    
  # Aggregate the data over all the trials separately for each days. Filter the trial data in a loop and then average and rbind.
  #---------------------------------------------------------------------------------------
  
  agg_data = c()
  
  for (i_sub in 1: length(unq_sub)) {
    sub_name = unq_sub[i_sub]
    for (i_day in 1: length(unq_day)) {
      day_name = unq_day[i_day]
      for (i_chan in 1:length(unq_chan)) {
        chan_name = unq_chan[i_chan]
        
        sub_data <- filter(all_data, Subject_Name == sub_name & Day == day_name & Channel ==  chan_name) 
        
        power_agg   = mean(sub_data$Power)
        perf_agg    = mean(sub_data$Raw_Perf)
        Subject     = sub_data$Subject_Name[1]
        Day         = sub_data$Day[1]
        Channel     = sub_data$Channel[1]
        Freq_band   = freq_name
        
        temp_df = data.frame(Subject, Day, Channel, Freq_band, power_agg, perf_agg)
        
        agg_data = rbind(agg_data, temp_df) # 75 observations i.e. 15 sub * 5 days
        # NOTE: This aggregated data is for each channel 
      }
    }
  }
  
  # Saving variable space
  all_data = agg_data
  
  all_data$Day = as.factor(all_data$Day)
  
  # testing plots
  ggplot(all_data, aes(x=Day, y=perf_agg)) +
    geom_boxplot(alpha=0.7) +
    stat_summary(fun=mean, geom="point", shape=20, size=8, color="red", fill="red") +
    theme(legend.position="none") +
    scale_fill_brewer(palette="Set1")
  
  # Save this plot for all subjects and frequencies
  
  
  # **********************************************************************
  # NOTE: the plots are interesting because, there are bad trials which are removed from the neural data and the so are the 
  # corresponding performance values. The graph changes a lot as compared to the main plot of performance mainly because 
  # some noisy trials are removed. 
  
  
  # Normalizing the data
  # ------------------------------------------------------------------------------------------------------
  # Z-score: The function used for this is 'scale' and is default function in R: To normalize the power
  # ------------------------------------------------------------------------------------------------------
  
  # Per Subject Normalization
  norm_power= norm_perf = norm_pow_exc_out =  c()
  
  for (i_sub in 1 : length(unq_sub)) { # : length(unq_sub
    
    # Filter the data and normalize
    sub_data <- filter(all_data, Subject == unq_sub[i_sub] ) # 62 values from 62 channels
    
    # z-score normalization of power
    z_score_norm = scale(sub_data$power_agg)
    norm_power = c(norm_power, z_score_norm)
    
    # We do the removing of outliers here
    lower_bound <- quantile(z_score_norm, 0.025)
    lower_bound
    
    upper_bound <- quantile(z_score_norm, 0.975)
    upper_bound
    
    # Assign the normalized values to another variable and replace with NA based on threshold 
    z_score_norm_out_excl = z_score_norm
    z_score_norm_out_excl[z_score_norm_out_excl > upper_bound ] <- NA
    z_score_norm_out_excl[z_score_norm_out_excl < lower_bound ] <- NA
    norm_pow_exc_out = c(norm_pow_exc_out, z_score_norm_out_excl)
  }

  
  # Having all the data in the same data frame
  all_data$z_score_norm = norm_power
  all_data$norm_pow_exc_out = norm_pow_exc_out
  
  # We scale the performance data across the participants because it is just scaling the data to fit a line through it.
  # min max normalization of power to scale from 0 to 1; and I do not think it effects the meaning of the data.
  # min_max = minMax(all_data$perf_agg)
  # norm_perf = c(norm_perf, min_max)
  # all_data$norm_perf = norm_perf
  # But may be instead of min_max scaling, we could use the direct mapping of the data by dividing things by 100
  all_data$scaled_perf = all_data$perf_agg / 100
  
  # Save the file for the freq band
  filename = paste(outdir_data, paste('all_data', Freq_band, '.csv', sep = '_'), sep = '/')
  write_csv(all_data, filename)
  
}









#---------------------------------------------------------------------------------------------
# The data from above takes a long time to analyse, therefore, I have saved it before and now we just call them


# Some codes to clear the workspace
rm(list = ls(all = TRUE))
graphics.off()

# Import all the necessary libraries in one line
my_packages = c("nlme", "stats", "ggplot2", "arm", "readr", "tidyverse",
                "rsq", "arm", "dplyr", "tictoc", "itsadug", "ggpubr")
lapply(my_packages, require, character.only = TRUE)


# Input and output file directories
# indir = 'C:/Users/bhadra9/OneDrive - unige.ch (1)/2022_Data_battery_protocol_outputs/2022_11/Data_4paper_v1/Brain_Behaviour/Data_v2/'
indir = 'C:/Users/bhadra9/OneDrive - unige.ch (1)/BCI_Group_shared/Code/KB/00_Reproducable_abstract_plots/Fig3_Brain-Behaviour/Data_v32/'
setwd(indir) # used to change the directory

obv_data = 'C:/Users/bhadra9/OneDrive - unige.ch (1)/BCI_Group_shared/Code/KB/00_Reproducable_abstract_plots/Fig3_Brain-Behaviour/Output_data/Observed_Datav3/'

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
  freq_name = unique(all_data$Freq_Band)
  
  
  # -------------------------------------------------------------------
  # Initializing the variables to be saved later
  Subject = Freq = p_val_time = t_val_time = f_value_time = chan = df_val_time =  
    p_val_interctn = t_val_interctn = f_value_interctn = df_val_interctn =  Est_corr = p_val_corr = 
    p_val_pow = t_val_pow = c()
  
  
  # Modelling of the data will be done for each channel
  for (i_chan in 1:length(unq_chan)) {
    
    chan_name = all_data$Channel[i_chan]
    Day_name = all_data$Day[i_chan]
    Subject = all_data$Subject_Name[i_chan]
    Freq = all_data$Freq_band[i_chan]
    
    # Filter the data based on each channel
    sub_data <- filter(all_data, Channel == chan_name)
    
    # Some plotting to check the power over performance
    # NOTE: The scaled performance data does not effect the slope, so we use the raw performance data
    plot(sub_data$scaled_perf , sub_data$z_score_norm)
    abline(lm(sub_data$z_score_norm ~ sub_data$scaled_perf, data = sub_data), col = "blue")
    
    
    # Convert to factors
    sub_data$Subject = as.factor(sub_data$Subject)
    sub_data$Day = as.numeric(sub_data$Day)
    
    sub_data$time_contrast<- ifelse(sub_data$Day == '1', '-2',
                                    ifelse(sub_data$Day == '2','-1',
                                           ifelse(sub_data$Day == '3','0',
                                                  ifelse(sub_data$Day == '4','1',
                                                         ifelse(sub_data$Day == '5','2','bug')))))
    sub_data$time_contrast = as.numeric(sub_data$time_contrast)
    
    # GLM modelling
    # model3 <- lme(norm_perf ~ z_score_norm + time_contrast + power_agg:time_contrast, random= ~1|Subject,  data = sub_data)
    # model3 <- lme(norm_perf ~ z_score_norm + time_contrast , random= ~1|Subject,  data = sub_data, na.action=na.omit)
    test_model2 = lmerTest::lmer(scaled_perf ~ z_score_norm + time_contrast + z_score_norm:time_contrast 
                                 + (1|Subject), data = sub_data, REML = FALSE, na.action = na.exclude)
    
    
    # Start working from here: 
    t = anova(test_model2)
    t1 = summary(test_model2)
    # plot(test_model3)
    # anova(test_model3,test_model2)
    
    
    ggscatter(sub_data, x = "z_score_norm", y = "scaled_perf",
              add = "reg.line", conf.int = TRUE,
              cor.coef = TRUE, cor.method = "pearson",
              xlab = "Z-score Power", ylab = "Performance")
    
    # Correlation for the power and performance
    corr = cor.test(sub_data$z_score_norm, sub_data$scaled_perf, method = c("pearson"))
    
    # making list of the needed variables
    
    p_val_pow[i_chan]    = t$`Pr(>F)`[1]
    t_val_pow[i_chan]    = t1$coefficients[14]
    
    p_val_time[i_chan]   = t$`Pr(>F)`[2]
    df_val_time[i_chan]  = t$DenDF[2]
    t_val_time[i_chan]   = t1$coefficients[15]
    f_value_time[i_chan] = t$`F value`[2]
    chan[i_chan]         = chan_name
    
    p_val_corr[i_chan]   = corr$p.value             
    Est_corr[i_chan]     = corr$estimate[["cor"]] 
    
    t_val_interctn[i_chan]   = t1$coefficients[16]
    f_value_interctn[i_chan] = t$`F value`[3]
    df_val_interctn[i_chan]  =  t$DenDF[3]
    p_val_interctn[i_chan]   = t$`Pr(>F)`[3]
    
    Agg_data_est_GLM_z_scored_ALL = data.frame(Freq, chan, p_val_time, df_val_time, t_val_time, f_value_time, 
                                               p_val_pow, t_val_pow,
                                               Est_corr, p_val_corr,
                                               p_val_interctn, t_val_interctn, df_val_interctn, f_value_interctn)
    
    
    # Save the file for the freq band
    filename = paste(obv_data, paste('Agg_data_est_GLM_z_scored_ALL',Freq, '.csv', sep = '_'), sep = '/')
    write_csv(Agg_data_est_GLM_z_scored_ALL, filename)
    
  } # i_chan
} # i_freq







