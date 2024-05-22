% This script is to convert the with trials TF data into different
% frequency bands and baseline correct it as well
% Author : Kinkini Bhadra Date: 15/03/2023

clear all
close all

% Indir
INDIR  = 'D:\2022_Battery_protocol_data\Analysis\Power_Modulation_withtrials\';
OUTDIR = 'D:\2022_Battery_protocol_data\Analysis\Power_Modulation_withtrials_freqseg\';
cd(INDIR)

% Use ft_select data to convert into different frequency bands i.e. cut the
% data
toi_base_low = -3;
toi_base_high = -2;

% Important files to load
% Channel File for topoplots and multiplots
load 'C:\Users\bhadra9\Documents\MATLAB\fieldtrip-20200128\ANT_NEURO Chan loc\Antnew_without_with_ref.mat'


% Frequency band sof interest
freq_chart = struct('name',{'Theta(4-7 Hz)', 'Alpha(8-13 Hz)', 'Beta(14-26 Hz)', 'L_Gamma(27-40 Hz)', 'H_Gamma(41-70 Hz)'}, 'lowval', {4,8,14,27,41},...
    'highval',{7,13,26,40,70});

%% load the clean data
% Loading all data in a loop
data_info = dir('*.mat');

for i_sub = 1 :length(data_info)
    
    file_name = data_info(i_sub).name;
    new_filename = [extractBefore(file_name,'.mat') '_freqbase'];
    
    % load data
    load(file_name)
    
    % Freqband cutting in a loop to save time
    for i_freq = 1: length(freq_chart)
        Freq_name = freq_chart(i_freq).name;
        
        % freq segmentation
        cfg = [];
        cfg.frequency = [freq_chart(i_freq).lowval freq_chart(i_freq).highval];
        freqseg = ft_selectdata(cfg, Power);
        
        % baseline correction
        cfg = [];
        cfg.baseline = [toi_base_low toi_base_high];
        cfg.baselinetype = 'relative';
        freqsegbase = ft_freqbaseline(cfg, freqseg);
        
        % Converting the relative change into percentage increase or decrease
        freqsegbase.powspctrm = freqsegbase.powspctrm .*100; % to convert in percentage
        
        % saving the file in each freq folder
        mkdir([OUTDIR Freq_name])
        savefast([OUTDIR Freq_name '\', Freq_name new_filename '_powerfreqsegbase.mat'], 'freqsegbase')
        
        clearvars freqsegbase freqseg
    end % ifreq
end % isub
