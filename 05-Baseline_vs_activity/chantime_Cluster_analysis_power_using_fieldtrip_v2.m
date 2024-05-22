%% ------------ Cluster based correction using fieldtrip code on the ERD/ERS power data --------------
% Average the data for all the subjects for each day and then use stat
clear
close all
clc


%% ----------------------------------------------------------------------------------------------------
% Setting the directory
%----------------------------------------------------------------------------------------------------

Data_Indir = 'D:\2022_Battery_protocol_data\Analysis\ERDS_wavelet\Data\Power_Modulation_v2\';
cd(Data_Indir)

base_data_dir = 'D:\2022_Battery_protocol_data\Analysis\ERDS_wavelet\Power_base_avg_v3\';
Cluster_data = 'D:\2022_Battery_protocol_data\Analysis\ERDS_wavelet\Data\Power_stat_v3\';
Cluster_plot = 'D:\2022_Battery_protocol_data\Analysis\ERDS_wavelet\Plots_power_sub_avg_v3\';
avg_data_dir = 'D:\2022_Battery_protocol_data\Analysis\ERDS_wavelet\Power_sub_avg_v3\';

% 2- Important files to load
% Channel File for topoplots and multiplots
load 'C:\Users\bhadra9\Documents\MATLAB\fieldtrip-20200128\ANT_NEURO Chan loc\Antnew_without_with_ref.mat'
% Load colormap file
load 'C:\Users\bhadra9\Documents\MATLAB\new_pos_neg_map'
% Load neighbours
load('easycap64ch-avg_neighb_withCPZ');

freq_chart = struct('name',{'Theta(4-7 Hz)', 'Alpha(8-13 Hz)', 'Beta(14-26 Hz)', 'L_Gamma(27-40 Hz)', 'H_Gamma(41-70 Hz)'}, 'lowval', {4,8,14,27,41},...
    'highval',{7,13,26,40,70});

%% --------------------------------
% Loading the data and breaking it into different frequency bands
% i.e. average over frequency bands
% ---------------------------------


list_all_sub = dir([pwd, '\Power' '*.mat']);

for i = 1: length(list_all_sub) % all subject,all day: total should be 75
    
    % load data
    filename = list_all_sub(i).name;
    load(filename)
    
    for i_freq = 3:4%1%: length(freq_chart)
        freq_name = extractBefore(freq_chart(i_freq).name, '(');
        
        cfg_select = [];
        cfg_select.frequency   = [freq_chart(i_freq).lowval freq_chart(i_freq).highval];
        cfg_select.avgoverfreq = 'no';
        cfg_select.nanmean     = 'no';
        freqseg = ft_selectdata(cfg_select, Power);
        
        % Baseline Correction per frequency band
        cfg = [];
        cfg.baseline = [-3 -2];
        cfg.baselinetype = 'relchange';
        Power_base = ft_freqbaseline(cfg, freqseg);
        
        % Converting the relative change into percentage increase or decrease
        Power_base.powspctrm = Power_base.powspctrm .*100; % to convert in percentage
        
%         % average over freq
%         cfg_select.avgoverfreq = 'yes';
%         cfg_select.nanmean     = 'yes';
%         Power_base_avg = ft_selectdata(cfg_select, Power_base);
        
        % save for each subject
        mkdir([base_data_dir freq_name])
        save([base_data_dir freq_name '\', freq_name '_base_' filename], 'Power_base')
    end
    
end % i

%% Enter in each frequency folder and then average all days for each subject
% Average for each subjects all days, so that we have 1 .mat file per subject
cd(base_data_dir)

for i_freq = 3:4%: length(freq_chart)
    
    Freq_name = extractBefore(freq_chart(i_freq).name, '(');
    
    cd([base_data_dir Freq_name])
    
    list_all_sub = dir([pwd, '\' Freq_name '*.mat']);
    
    counter = -4;
    for i = 1: length(list_all_sub)/5
        counter = counter + 5;
        sublist = list_all_sub(counter : counter+4);
        
        sub_name = char(extractBetween(sublist(1).name, 'Power_','_Day'));
        
        for j = 1: length(sublist)
            load(sublist(j).name)
            sub_data{j} = Power_base;
        end % j
        
        cfg_grand = [];
        cfg_grand.foilim         = 'all';
        cfg_grand.toilim         = 'all';
        cfg_grand.channel        = 'all';
        cfg_grand.parameter      = 'powspctrm';
        grand_avg_sub            = ft_freqgrandaverage(cfg_grand, sub_data{:}) ;
        
        % save for each subject
        mkdir([avg_data_dir Freq_name])
        save([avg_data_dir Freq_name '\', Freq_name '_' sub_name '_grand_avg_sub.mat'], 'grand_avg_sub')
        
    end % i
    
    % For averaging over the frequency domain and putting all subject data
    % in one structure
    cd([avg_data_dir Freq_name '\'])
    
    List_data = dir([pwd, '\' Freq_name '*.mat']);

    allsub_ERDS_TF = [];
    for i_dataset = 1: length(List_data)
        load(List_data(i_dataset).name)
        
        cfg_aof.frequency   = 'all';
        cfg_aof.avgoverfreq = 'yes';
        cfg_aof.nanmean     = 'yes';
        ERDS_TF_avgfreq     = ft_selectdata(cfg_aof, grand_avg_sub);
        
        ERDS_TF_avgfreq.freq = 1; % arbitrary because for some reason the decimaly are different
        grandavg_aof{i_dataset} = ERDS_TF_avgfreq;
        
        % Creating a duplicate data with zeros
        %- >> follow this: https://mailman.science.ru.nl/pipermail/fieldtrip/2019-May/038962.html
        duplicate_data = ERDS_TF_avgfreq;
        duplicate_data.powspctrm = zeros(size(ERDS_TF_avgfreq.powspctrm));
        
        dup_data{i_dataset} = duplicate_data;
        
    end
    
    % Grand average the data for plotting
    cfg_grand = [];
    cfg_grand.foilim         = 'all';
    cfg_grand.toilim         = 'all';
    cfg_grand.channel        = 'all';
    cfg_grand.parameter      = 'powspctrm';
    grand_avg_power = ft_freqgrandaverage(cfg_grand, grandavg_aof{:}) ;
    
    % save the grand average to plot
%     save([Cluster_data, Freq_name '_avg.mat'], 'grand_avg_power')

    
    % For each frequency band we do this cluster analysis
    
    cfg=[];
    cfg.latency = 'all';
    cfg.frequency = 'all';
    cfg.dim         = grandavg_aof{1}.dimord;
    cfg.method      = 'montecarlo';
    cfg.statistic   = 'ft_statfun_depsamplesT';
    cfg.parameter   = 'powspctrm';
    cfg.correctm    = 'cluster';
    cfg.computecritval = 'yes';
    cfg.neighbours  = neighbours;
    cfg.numrandomization = 1000;
    cfg.alpha       = 0.025; % Set alpha level
    cfg.tail        = 0;    % Two sided testing
    
    % Design Matrix
    nsubj = numel(grandavg_aof);
    cfg.design(1,:) = [1:nsubj 1:nsubj]; %[covariate'  covariate'];%
    cfg.design(2,:) = [ones(1,nsubj) ones(1,nsubj)*2];
    cfg.uvar        = 1; % row of design matrix that contains unit variable (in this case: subjects)
    cfg.ivar        = 2; % row of design matrix that contains independent variable (the conditions)
    
    stat = ft_freqstatistics(cfg, grandavg_aof{:}, dup_data{:});
    
    % Save this to save time
    save([Cluster_data, Freq_name '_stat.mat'], 'stat')
    
    %% Plotting
    
    load('C:\Users\bhadra9\OneDrive - unige.ch (1)\Project_EEG-BCI\Scripts\Neural Data Analysis\November_2021\pos_neg_colormap.mat')
    % Plot the power values first and then mask with outline on it
    
    % Power data
    im = figure;
    im(i_freq) = imagesc(grand_avg_power.time, 1: length(grand_avg_power.label),...
        squeeze(grand_avg_power.powspctrm)); % nice image plot showing significant time points for each channel
    im(i_freq).AlphaData = .7;
    colormap(pos_neg_colormap)
    
    % Plot only the significant data
    im = figure;
    im(i_freq) = imagesc(grand_avg_power.time, 1: length(grand_avg_power.label),...
        squeeze(grand_avg_power.powspctrm)); % nice image plot showing significant time points for each channel
    im(i_freq).AlphaData = .7;
    
    % im = imagesc(grand_avg_erds.time, 1: length(grand_avg_erds.label),...
    %     squeeze(grand_avg_erds.powspctrm)); % nice image plot showing significant time points for each channel
    % im.AlphaData = .7;
    
    colormap(pos_neg_colormap)
    hold on
    contour(grand_avg_power.time, 1: length(grand_avg_power.label), ...
        squeeze(stat.mask).* squeeze(grand_avg_power.powspctrm) ,1,'linecolor','k')
    
    yticks = grand_avg_power.label;
    set(gca, 'YTick', 1:length(grand_avg_power.label), 'YTickLabel', yticks);
    
    % Change the hard coding here
    xline(-3,'--m','Baseline','LabelHorizontalAlignment','left', 'LineWidth', 1.5) % For a straight line on the x- axis whos y value always remain same
    xline(-2,'--m','Cue','LabelHorizontalAlignment','left', 'LineWidth', 1.5) % For a straight line on the x- axis whos y value always remain same
    xline(0,'--m','Trial Begin','LabelHorizontalAlignment','left', 'LineWidth', 1.5) % For a straight line on the x- axis whos y value always remain same
    xline(5,'-m','Trial End','LabelHorizontalAlignment','left', 'LineWidth', 1.5) % For a straight line on the x- axis whos y value always remain same
    xline(7,'--m','Feedback','LabelHorizontalAlignment','left', 'LineWidth', 1.5) % For a straight line on the x- axis whos y value always remain same
    
    title(Freq_name,'fontsize',16 )
    % title('H_Gamma(40-70 Hz)','fontsize',16 )
    
    colorbar
    
    % Save the plot
    saveas(gcf,fullfile([Cluster_plot, Freq_name, '.fig']));
    
    cd('..')
end


%% %% -------------------ADDED ON 10/03/2023
% ONLY FOR PLOTTING: Next time just use this part of the script to create
% the plots. NOTE that it takes while to run it because the data is a bit
% heavy

clear
close all
clc

% Cool trick to set all font for plots to change into a desired font type 
set(0, 'DefaultAxesFontName', 'Arial');
set(0, 'DefaultTextFontName', 'Arial');

suffix = '_minmax';

% Cluster_data = 'C:\Users\bhadra9\OneDrive - unige.ch (1)\BCI_Group_shared\Code\KB\00_Reproducable_abstract_plots\Fig6_Baseline_vs_activity\Output_data\chantime\';
Cluster_data = 'D:\2022_Battery_protocol_data\Analysis\ERDS_wavelet\Data\Power_stat_v3\';
Cluster_plot = 'C:\Users\bhadra9\OneDrive - unige.ch (1)\BCI_Group_shared\Code\KB\00_Reproducable_abstract_plots\Fig6_Baseline_vs_activity\Plots\chantime\';

% Channel File for topoplots and multiplots
load 'C:\Users\bhadra9\Documents\MATLAB\fieldtrip-20200128\ANT_NEURO Chan loc\Antnew_without_with_ref.mat'

% Load colormap file
tmpcolormap = ft_colormap('RdBu');
tmpcolormap = flip(tmpcolormap,1) ;

% Load neighbours
load('easycap64ch-avg_neighb_withCPZ');

% freq_chart = struct('name',{'Theta(4-8 Hz)', 'Alpha(8-12 Hz)', 'Beta(12-25 Hz)', 'L_Gamma(25-40 Hz)', 'H_Gamma(40-70 Hz)'}, 'lowval', {4,8,12,25,40},...
%     'highval',{8,12,25,40,70});
freq_chart = struct('name',{'Theta(4-7 Hz)', 'Alpha(8-13 Hz)', 'Beta(14-25 Hz)', 'L_Gamma(26-40 Hz)', 'H_Gamma(41-70 Hz)'}, 'lowval', {4,8,14,26,41},...
    'highval',{7,13,25,40,70});

% Pre state the min max values
vmin = -50;
vmax = 50;


for i_freq = 3:4%1: length(freq_chart)
    Freq_name = extractBefore(freq_chart(i_freq).name, '('); %freq_chart(i_freq).name;
    cd(Cluster_data)
    
    load([Freq_name, '_stat.mat'])
    load([Freq_name, '_avg.mat'])
    
    % Plotting
    % Power data
    im = figure;
    im(i_freq) = imagesc(grand_avg_power.time, 1: length(grand_avg_power.label),...
        squeeze(grand_avg_power.powspctrm)); % nice image plot showing significant time points for each channel
    im(i_freq).AlphaData = .7;
    colormap(tmpcolormap) %darkb2r(vmin, vmax)
%     colorbarpwn(vmin, vmax, 'full', 0)

    % Plot only the significant data
    im = figure;
    im(i_freq) = imagesc(grand_avg_power.time, 1: length(grand_avg_power.label),...
        squeeze(grand_avg_power.powspctrm)); % nice image plot showing significant time points for each channel
    im(i_freq).AlphaData = .7;
    
    colormap(tmpcolormap) %darkb2r(vmin, vmax)
%     colorbarpwn(vmin, vmax, 'full', 0)

    hold on
    [M,c] = contour(grand_avg_power.time, 1: length(grand_avg_power.label), ...
        squeeze(stat.mask).* squeeze(grand_avg_power.powspctrm) ,1,'linecolor','k');
    c.LineWidth = 1.5;
    
    yticks = grand_avg_power.label;
    set(gca, 'YTick', 1:length(grand_avg_power.label), 'YTickLabel', yticks, 'Fontsize', 16);
    
    % Change the hard coding here
    xline(-3,'--', 'Baseline','LabelHorizontalAlignment','center', 'LineWidth', 1.5, 'Color', [246/ 256 24/ 256 127/ 256], 'FontSize', 14) % For a straight line on the x- axis whos y value always remain same
    xline(-2,'--', 'Cue','LabelHorizontalAlignment','center', 'LineWidth', 1.5, 'Color', [246/ 256 24/ 256 127/ 256], 'FontSize', 14) % For a straight line on the x- axis whos y value always remain same
    xline(0, '--', 'Online Feedback START','LabelHorizontalAlignment','center', 'LineWidth', 1.5, 'Color', [246/ 256 24/ 256 127/ 256], 'FontSize', 14) % For a straight line on the x- axis whos y value always remain same
    xline(5, '--' , 'Online Feedback STOP','LabelHorizontalAlignment','center', 'LineWidth', 1.5, 'Color', [246/ 256 24/ 256 127/ 256], 'FontSize', 14) % For a straight line on the x- axis whos y value always remain same
    xline(7, '--', 'Rest','LabelHorizontalAlignment','center', 'LineWidth', 1.5, 'Color', [246/ 256 24/ 256 127/ 256], 'FontSize', 14) % For a straight line on the x- axis whos y value always remain same

    title([Freq_name  datestr(now, 'dd-mm-yyyy')],'fontsize',16 )
    
    set(gca, 'FontName', 'Arial');
    
    colorbar
%     caxis([vmin, vmax]) 
    
    c = colorbar;
    c.FontName = 'Arial';  c.FontSize = 18;
    c.Label.String   = 'Power [%]';
    c.Label.FontSize = 30;
    c.FontSize = 30;
    
    set(gcf, 'Position', [100, 500, 700, 1300])
    
    % Save the plot
    saveas(gcf,fullfile([Cluster_plot, datestr(now, 'dd-mm-yyyy'), Freq_name, suffix, '.fig']));
    % We dont save the .png directly because the figures need manual intervention
%     saveas(gcf,fullfile([Cluster_plot, datestr(now, 'dd-mm-yyyy'), Freq_name, suffix, '.png']));

    
end







