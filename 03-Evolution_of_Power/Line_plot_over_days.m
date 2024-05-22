%% This code is to make plots of the average power from the trials and plot
% for each day, but only from the significant channels that were found
% after the cluster correction

% Author: Kinkini Bhadra, University of Geneva
% Date created : 22/02/2023

% Clear workspace
clear all
close all

% Directories
indir   = 'C:\Users\bhadra9\OneDrive - unige.ch (1)\BCI_Group_shared\Code\KB\00_Reproducable_abstract_plots\Fig2_Power_Evolution\Data\';
plot_dir = 'C:\Users\bhadra9\OneDrive - unige.ch (1)\BCI_Group_shared\Code\KB\00_Reproducable_abstract_plots\Fig2_Power_Evolution\Plot\';

% change to the data directory
cd(indir)

% Details about the files in the current directory
datlist = dir('*.csv');
nfreq = length(datlist);

% Plotting palette
grey_color = [71 71 71] ./255; % Dark one for the axis
grey_color_light = [.7 .7 .7]; % [128 128 128]/255; % light grey color for the non-significant channels
red_color = [0.6350 0.0780 0.1840]; % Red color used for the significant channels

% Loop over different frequencies
for ifreq = 4:5% nfreq
    
    % Details of the loop information
    figure
    freqname = string(extractBetween(datlist(ifreq).name, 'sub_','.csv'));
    
    % Read the data only when its theta and low gamma
    if freqname == "Theta" || freqname == "L_Gamma"
        data = readtable(datlist(ifreq).name);
        allchan = unique(data.chanel); % all chan list
    else
        data = [];
    end
    
    
    if freqname == "Theta" % 37 channels
        title_name = 'Theta';
        % Significant channels
        sigchanlist = {'FP1', 'FPZ', 'FP2', 'F3 ','FZ ','F8 ','F8 ','FC5','FC1','T8 ',...
            'CP6','PZ ','P4 ','P8 ','POZ','O2 ','AF7','AF3','AF4','AF8','F5 ','PO8','OZ ',...
            'F1 ','F2 ','F6 ','FCZ','CP4','P1 ','P2 ','P6 ', 'PO4', 'PO6', 'FT8', 'TP8'}';
        % Non significant channels
        nonsigchanlist = setdiff(allchan, sigchanlist);
        
    elseif freqname == "L_Gamma" % 43 channels
        title_name = 'Low Gamma';
        sigchanlist = {'FP1','FPZ','FP2','F7 ','F3 ','F8 ','FC5','FC6','T7 ','C3 ','C4 ',...
            'T8 ','CP5','CP1','CP2','CP6','P3 ','PZ ','P4 ','P8 ','POZ','AF3',...
            'AF4','AF8','F5 ','F6 ','FC3','FC4','C5 ','C1 ','C6 ','CP3','CP4',...
            'P5 ','P2 ','P6 ','PO3','FT8','TP7','OZ ','CPZ'};
        % Non significant channels
        nonsigchanlist = setdiff(allchan, sigchanlist);
    else
        sigchanlist = {};
        nonsigchanlist = {};
        title_name = '';
    end
    
    % Loop over non-significant channels
    %     for insig = 1: length(nonsigchanlist)
    %         % Current Channel
    %         nsigchan = string(nonsigchanlist(insig));
    %
    %         % Day 1
    %         day1dat = data.Power(data.Day == 1 & data.chanel == nsigchan); % Should be 15 values from 15 subjects
    %         day1mean = mean(day1dat);
    %
    %         % Day 2
    %         day1dat = data.Power(data.Day == 2 & data.chanel == nsigchan); % Should be 15 values from 15 subjects
    %         day2mean = mean(day1dat);
    %
    %         % Day 3
    %         day1dat = data.Power(data.Day == 3 & data.chanel == nsigchan); % Should be 15 values from 15 subjects
    %         day3mean = mean(day1dat);
    %
    %         % Day 4
    %         day1dat = data.Power(data.Day == 4 & data.chanel == nsigchan); % Should be 15 values from 15 subjects
    %         day4mean = mean(day1dat);
    %
    %         % Day 5
    %         day1dat = data.Power(data.Day == 5 & data.chanel == nsigchan); % Should be 15 values from 15 subjects
    %         day5mean = mean(day1dat);
    %
    %         X = [1 2 3 4 5];
    %         Y = [day1mean day2mean day3mean day4mean day5mean];
    %
    %         % Plotting
    %         plot(X,Y,'Color',grey_color_light ,'MarkerSize',1, 'LineWidth', 1.5)
    %         hold on
    %
    %     end % non significant channels
    
    % Loop over significant channels
    for isig = 1: length(sigchanlist)
        
        % Current Channel
        sigchan = string(sigchanlist(isig));
        
        % Day 1
        day1dat = data.Power(data.Day == 1 & data.chanel == sigchan); % Should be 15 values from 15 subjects
        day1mean = mean(day1dat);
        
        % Day 2
        day1dat = data.Power(data.Day == 2 & data.chanel == sigchan); % Should be 15 values from 15 subjects
        day2mean = mean(day1dat);
        
        % Day 3
        day1dat = data.Power(data.Day == 3 & data.chanel == sigchan); % Should be 15 values from 15 subjects
        day3mean = mean(day1dat);
        
        % Day 4
        day1dat = data.Power(data.Day == 4 & data.chanel == sigchan); % Should be 15 values from 15 subjects
        day4mean = mean(day1dat);
        
        % Day 5
        day1dat = data.Power(data.Day == 5 & data.chanel == sigchan); % Should be 15 values from 15 subjects
        day5mean = mean(day1dat);
        
        X = [1 2 3 4 5];
        Y = [day1mean day2mean day3mean day4mean day5mean];
        
        % Plotting
        p = plot(X,Y,'Color', red_color ,'MarkerSize',1, 'LineWidth', 1.5);
        hold on
        
    end % Significant chans
    
    % Plotting Cosmetic changes:
    xlabel('Day'); ylabel(['Average Power: ' newline char(title_name)]);
    xlim([0.8, 5.2])
    xticks([1 2 3 4 5]); xticklabels({'1','2','3','4','5'})
    set(gca,'linewidth',3, 'fontsize',25, 'fontname', 'Arial', 'XColor', grey_color, 'YColor', grey_color)
    box off
    
    % Select the figure and change the y-label manually
    if freqname == "Theta" % 37 channels
        ylim([-27, 25])
    elseif freqname == "L_Gamma" % 43 channels
        ylim([-13, 14])
    else
    end
    
    % Save the files
%     saveas(gca,fullfile([plot_dir,['line_sig', char(freqname), '.png']]));
%     saveas(gca,fullfile([plot_dir,['line_sig', char(freqname), '.fig']]));
%     saveas(gca,fullfile([plot_dir,['line_sig', char(freqname), '.eps']]));
%     saveas(gca,fullfile([plot_dir,['line_sig', char(freqname), '.svg']]));
    
end % ifreq



% saveas(gca,fullfile([plot_dir,['line_', char(freqname), '.png']]));
% saveas(gca,fullfile([plot_dir,['line_', char(freqname), '.fig']]));
% saveas(gca,fullfile([plot_dir,['line_', char(freqname), '.eps']]));
% saveas(gca,fullfile([plot_dir,['line_', char(freqname), '.svg']]));


