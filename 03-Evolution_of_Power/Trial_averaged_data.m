%% New GLM Pipeline:
% - Data prep for R
% - create .csv files for each participant, each day and each frequency band

clear
close all

indir = 'D:\2022_Battery_protocol_data\Analysis\ERDS_wavelet\Power_base_avg_v3\';
cd(indir)

n_freq = folders_inDir(indir);

outdir = 'C:\Users\bhadra9\OneDrive - unige.ch (1)\BCI_Group_shared\Code\KB\00_Reproducable_abstract_plots\Fig2_Power_Evolution\Data\';


% Subject folder
for ifreq = 1: length(n_freq)
    cd([indir, n_freq(ifreq).name]);
    n_data = dir('*.mat');
    
%     Data_factor_for_R = array2table(NaN(1,6), 'VariableNames',{'key','Subject_Name','Day','Freq_Band','chanel', 'Power'});
    Data_factor_for_R = [];
    
    % Debugging code: to check for the participants who do not have the
    % data for all the days
    if size(n_data) < 75  % because we have data from 5 days for 15 participants
        error('Data inconsistency: The participant does-not have data from all the days');
        return
    end
    
    
    % Loading data from each particular day
    for idata = 1: length(n_data)
        data = load(n_data(idata).name);
        
        freq_name = char(extractBefore(n_data(idata).name,'_base'));
        subj_name = char(extractBetween(n_data(idata).name,'Power_','_Day'));
        Day_name = char(extractBetween(n_data(idata).name,'ay_','.mat'));
        % assigning power, channel, performance and events to a separate variable for easy
        % recognition and for later use
        
        % Imagery period only
        time_slice =  find(data.Power_base.time >= 0 & data.Power_base.time <= 5);
        time_low = min(time_slice);
        time_high = max(time_slice);
        powerval = data.Power_base.powspctrm(:,:,time_low: time_high);
        chan = data.Power_base.label';
        
        % average over frequency and time
        avg_ov_freq = squeeze(nanmean(powerval,2)); % 3rd dimention is frequency
        avg_ov_time = squeeze(nanmean(avg_ov_freq,2));
        
        power = avg_ov_time;
        
        % Arranging the output data for R: Add separate columns with
        % subject name, Day, channel, freq band
               
        % For other information
        key = (1: length(chan))';
        Subject_Name = repmat(subj_name, length(chan),1); % the information is repeated based on the number of trials
        Day = repmat(Day_name, length(chan),1);
        Freq_Band = repmat(freq_name, length(chan),1);
        
        % converting the above varibles to a table
        tmp_data_slice = table(key,Subject_Name, Day(:), Freq_Band, char(string(chan)'), power, ...
            'VariableNames',{'key','Subject_Name','Day','Freq_Band','chanel', 'Power'});
        
        % joining the tables
        Data_factor_for_R = [Data_factor_for_R ; tmp_data_slice];
        
        
        clearvars data subj_name Day_name powerval table_title Freq_Band...
            subj_name key tmp_power_tab power avg_ov_time avg_ov_freq Subject_Name tmp_data_slice
        
    end % idata
    
    % saving the datafile in .csv
    table_title = [outdir 'All_sub_' freq_name '.csv'];
    writetable(Data_factor_for_R, table_title)
    
    clearvars freq_name Data_factor_for_R
    
end %ifreq

cd(outdir)
