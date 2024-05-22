%% Power Modulation : Time Frequency conversion
% Author: Kinkini Bhadra: 09/03/2023

%% clear the MATLAB workspace
clear
close all
clc

%% Setting the directory and MATLAB path
INDIR_data = 'D:\2022_Battery_protocol_data\Pre_processed_Data\';
cd(INDIR_data)

OUTDIR_data = 'D:\2022_Battery_protocol_data\Analysis\Power_Modulation_withtrials\';
% OUTDIR_plot = 'D:\2022_Battery_protocol_data\Analysis\ERDS_wavelet\plots\';


%% Inputs
% 1- time series of interest:
toi_low = -3.5; % in seconds
toi_high = 5.5; % in seconds
toi_base_low = -3;
toi_base_high = -2;

% 2- Important files to load
% Channel File for topoplots and multiplots
load 'C:\Users\bhadra9\Documents\MATLAB\fieldtrip-20200128\ANT_NEURO Chan loc\Antnew_without_with_ref.mat'
% Load colormap file
load 'C:\Users\bhadra9\Documents\MATLAB\new_pos_neg_map'

%% load the clean data
% NOTES: need to loop through the subjects and days

% Getting into each subject folder
subj_info = folders_inDir(INDIR_data);

for i_sub = 1 :length(subj_info)
    
    cd([INDIR_data subj_info(i_sub).name]) % changing to the subject directory
    subject_name = subj_info(i_sub).name;
    
    % Getting into day folder and load data
    day_info = folders_inDir(cd);
    
    for i_day = 1 :length(day_info)
        
        cd([INDIR_data subj_info(i_sub).name '\' day_info(i_day).name]) % changing to the subject directory
        day_num = extractAfter(day_info(i_day).name,3);
        
        data_name = ['clean_data' subject_name '_Day-' day_num '.mat']; % the string for the data name
        
        % loading the data
        load(data_name);
        
        % Method 1:  Doing the basic time frequency analysis to be able to compare the results
        cfg = [];
        cfg.method     = 'wavelet';
        cfg.output     = 'pow';
        cfg.taper      = 'hanning';
        cfg.foi        = 1:1:70; % setting the frequency window
        cfg.toi        = toi_low :0.02: toi_high; % setting the time window of interest
        cfg.keeptrials = 'yes';
        Power    = ft_freqanalysis(cfg, clean_data);
        
        % saving the data
        savefast([OUTDIR_data 'Power_withtrials' subject_name '_Day_',num2str(i_day) '.mat'],'Power');
        
    end % i_day
end % i_subj

