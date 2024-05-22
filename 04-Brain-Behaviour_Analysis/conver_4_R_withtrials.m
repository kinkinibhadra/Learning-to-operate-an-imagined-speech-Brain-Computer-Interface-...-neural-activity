%% New GLM Pipeline:
% - Data prep for R
% - create .csv files for each participant, each day and each frequency band

clear
close all

indir = 'D:\2022_Battery_protocol_data\Analysis\Power_Modulation_withtrials_freqseg\';
cd(indir)
n_freq = folders_inDir(indir);

outdir = 'C:\Users\bhadra9\OneDrive - unige.ch (1)\BCI_Group_shared\Code\KB\00_Reproducable_abstract_plots\Fig3_Brain-Behaviour\Data_v3\';

% Subject folder
for i_freq = 1 : length(n_freq)
    cd([indir, n_freq(i_freq).name]);
    n_day = dir('*.mat');
    
    % Debugging code: to check for the participants who do not have the
    % data for all the days
    if size(n_day) < 75  % because we have data from 5 days for each participant
        error('Data inconsistency: The participant does-not have data from all the days');
        return
    end
    
    freq_name = string(extractBefore(n_freq(i_freq).name,'('));
    
    % Loading each dataset
    for i_data = 1: length(n_day)
%         cd([indir, n_freq(i_freq).name]);
        
        data = load(n_day(i_data).name);
        
        sub_name = string(extractBetween(n_day(i_data).name,'trials','_Day'));
        day_name = char(extractBetween(n_day(i_data).name,'Day_','_freqbase'));
        % assigning power, channel, performance and events to a separate variable for easy
        % recognition and for later use
        % Imagery period only
        time_slice =  find(data.freqsegbase.time >= 0 & data.freqsegbase.time <= 5);
        time_low = min(time_slice);
        time_high = max(time_slice);
        powerval = data.freqsegbase.powspctrm(:,:,:,time_low: time_high);
        
        
        chan = string(data.freqsegbase.label');
        event = data.freqsegbase.trialinfo(:,1);
        Raw_performance = data.freqsegbase.trialinfo(:,2);
        smooth_performance = data.freqsegbase.trialinfo(:,4);
        biased_performance = data.freqsegbase.trialinfo(:,3);
        time_taken = data.freqsegbase.trialinfo(:,5);
        last_bar = data.freqsegbase.trialinfo(:,6);
        max_bar = data.freqsegbase.trialinfo(:,7);
        n_trials = length(data.freqsegbase.trialinfo);
        trial_no = 1: n_trials;
        
        
        % average over frequency and time
        avg_ov_freq = squeeze(nanmean(powerval,3)); % 3rd dimention is frequency
        avg_ov_time = squeeze(nanmean(avg_ov_freq,3));
        power  = avg_ov_time;
                
        % For other information
        key = (1: n_trials*62)';
        Subject_Name = repmat(sub_name, n_trials*62,1); % the information is repeated based on the number of trials
        Day = repmat(day_name, n_trials*62,1);
        Freq_Band = repmat(freq_name, n_trials*62,1);
        
        [Events, Raw_Perf, Smooth_Perf, biased_Perf, time_BCI,...
            last_bar_pos, max_bar_pos, Trial, Channel, Power] = deal([]);
            
        for i_trials = 1: n_trials
            Events = [Events; repmat(event(i_trials), 62,1)];
            Raw_Perf = [Raw_Perf; repmat(Raw_performance(i_trials), 62,1)];
            Smooth_Perf = [Smooth_Perf; repmat(smooth_performance(i_trials), 62,1)];
            biased_Perf = [biased_Perf; repmat(biased_performance(i_trials), 62,1)];
            time_BCI = [time_BCI; repmat(time_taken(i_trials), 62,1)];
            last_bar_pos = [last_bar_pos; repmat(last_bar(i_trials), 62,1)];
            max_bar_pos = [max_bar_pos; repmat(max_bar(i_trials), 62,1)];
            Trial = [Trial; repmat(trial_no(i_trials), 62,1)];
            Channel = [Channel; chan'];
            Power = [Power; power(i_trials, :)'];
        end
        
        % converting the above varibles to a table
        tmp_data_slice = table(key,Subject_Name, Day, Freq_Band, Trial, Channel, Events, Power,...
            Raw_Perf, Smooth_Perf,biased_Perf, time_BCI, last_bar_pos, max_bar_pos, ...
            'VariableNames',{'key','Subject_Name','Day','Freq_Band','Trial','Channel','Events',...
            'Power', 'Raw_Perf', 'Smooth_Perf','biased_Perf','time_BCI','last_bar_pos','max_bar_pos'});

        % Making freq directory
        mkdir([outdir char(freq_name)])
        
        % saving the datafile in .csv
        table_title = [outdir, char(freq_name) '\' char(sub_name) '_' char(freq_name) '_Day' num2str(day_name) '.csv'];

        writetable(tmp_data_slice, table_title)
        
        clearvars data sub_name day_name powerval event Raw_performance smooth_performance biased_performance...
            time_taken last_bar max_bar n_trials trial_no avg_ov_freq avg_ov_time power key Subject_Name...
            Day Freq_Band Events Raw_Perf Smooth_Perf biased_Perf time_BCI last_bar_pos max_bar_pos...
            Trial Channel tmp_data_slice table_title
                
    end % idata
    
    clearvars n_day freq_name
    
end %iSubj

cd(outdir)
