% Baseline vs_activity: Second way of doing it:
% - Average over the Days for each subjects
% - Cluster analysis at the level of topoplot

clear
close all

% Data directors
MAIN_DIR = 'D:\2022_Battery_protocol_data\Analysis\ERDS_wavelet\Power_base_avg_v3\';
DIR_DAY = folders_inDir(MAIN_DIR);

% Data directors
dir_data = 'D:\2022_Battery_protocol_data\Analysis\Baseline_vs_activity\Data_pow_stat_v3\';


%% Getting into each frequency folder
[Freq_Dir] = folders_inDir(MAIN_DIR);


for ifreq = 3:4%1: length(Freq_Dir)
    
    cd([MAIN_DIR Freq_Dir(ifreq).name])
    
    list_all_files = dir('*.mat');
    
    Freq_name = Freq_Dir(ifreq).name;
    
    counter = -4;
    for i_Sub = 1: length(list_all_files)/5 % because 5 days
        
        counter = counter + 5;
        
        list_sub = list_all_files(counter : counter+4);
        sub_name = char(extractBetween(list_sub(1).name, 'er_', '_Day_'));
        
        % GRAND AVERAGE Time-frequency plot: SINGLE CHANNEL
        cfg_grandAvg = [];
        cfg_grandAvg.parameter = 'powspctrm';
        cfg_grandAvg.inputfile = {list_sub.name};
        cfg.toilim = [0 5];
        SubAvg = ft_freqgrandaverage(cfg_grandAvg);
        
        % Saving the Subject averaged data
        save([dir_data, Freq_name '_' sub_name '_SubAvgpow_0to5.mat'], 'SubAvg')
        
    end
end


%% Stat and Plotting
clear all
close all

% Loading the grand averaged data
dir_data = 'D:\2022_Battery_protocol_data\Analysis\Baseline_vs_activity\Data_pow_stat_v3\';
cd(dir_data)

DIR_FIG = 'C:\Users\bhadra9\OneDrive - unige.ch (1)\BCI_Group_shared\Code\KB\00_Reproducable_abstract_plots\Fig6_Baseline_vs_activity\Plots\Topoplot\Topoplots_v3\';
dir_cluster_stat_data = 'D:\2022_Battery_protocol_data\Analysis\Baseline_vs_activity\Cluster_stat_data_power_v3\';

% Channel file
% load('Antnew_without_M1_M2_EOG.mat')
load('Antnew_without_with_ref.mat')
% Neighbours file
load('easycap64ch-avg_neighb_withCPZ');

%%
data = dir('*.mat');

counter = -14;
for ifreq = 1: length(data)/15 % Because 15 subjects
    
    counter = counter + 15;
    sub_data_list = data(counter: counter+14);
    
    Freq_name = extractBefore(sub_data_list(ifreq).name, '_');
    
    cfg_stat = [];
    cfg_stat.latency = [0 5];
    cfg_stat.frequency = 'all';
    cfg_stat.avgovertime = 'yes';
    cfg_stat.avgoverfreq = 'yes';
    cfg_stat.nanmean     = 'yes';
    cfg_stat.method = 'montecarlo';
    cfg_stat.statistic        = 'ft_statfun_depsamplesT';
    cfg_stat.correctm         = 'cluster';
    cfg_stat.clusteralpha     = 0.025;
    cfg_stat.clusterstatistic = 'maxsum';
    cfg_stat.minnbchan        = 2;
    cfg_stat.tail             = 0;
    cfg_stat.clustertail      = 0;
    cfg_stat.alpha            = 0.025;
    cfg_stat.numrandomization = 1000;
    cfg_stat.neighbours       = neighbours;
    
    % Creating duplicate data
    for i_sub = 1: numel(sub_data_list)
        load(sub_data_list(i_sub).name) % Load one dataset and create duplicate data from there
        sub_data{i_sub} = SubAvg;
        dup_temp = SubAvg;
        dup_temp.powspctrm = zeros(size(SubAvg.powspctrm));
        dup_data{i_sub} = dup_temp;
    end
    
    % Design Matrix
    nsubj = numel(sub_data_list);
    cfg_stat.design(1,:) = [1:nsubj 1:nsubj]; %[covariate'  covariate'];%
    cfg_stat.design(2,:) = [ones(1,nsubj) ones(1,nsubj)*2];
    cfg_stat.uvar        = 1; % row of design matrix that contains unit variable (in this case: subjects)
    cfg_stat.ivar        = 2; % row of design matrix that contains independent variable (the conditions)
    
    [stat] = ft_freqstatistics(cfg_stat, sub_data{:}, dup_data{:});
    
    % Save this stat for later
    savefast([dir_cluster_stat_data, Freq_name '_stat.mat'], 'stat')
    
end

%% Plotting
clear all
close all

% Channel file
% load('Antnew_without_M1_M2_EOG.mat')
load('Antnew_without_with_ref') % follows ant-neuro sequence % used for fieldtrip only

% Neighbours file
load('easycap64ch-avg_neighb_withCPZ');

dir_cluster_stat_data = 'D:\2022_Battery_protocol_data\Analysis\Baseline_vs_activity\Cluster_stat_data_power_v3\';
DIR_FIG = 'C:\Users\bhadra9\OneDrive - unige.ch (1)\BCI_Group_shared\Code\KB\00_Reproducable_abstract_plots\Fig6_Baseline_vs_activity\Plots\Topoplot\Topoplots_v3\';

cd(dir_cluster_stat_data)
data = dir('*.mat');

% n_colors = 251;
% tmpcolormap = ft_colormap('RdBu');
% tmpcolormap = flip(tmpcolormap,1) ;

% Mix max values from the data  ( I found it out before and then plug it here, NOT AUTOMATIZED)
vmin = -7;
vmax = 3.4;

% % Set the center of the colormap to white
% mid_idx = round(n_colors/2);
% tmpcolormap(mid_idx,:) = [1 1 1];

for i = 1: length(data) % 5 frequency bands
    
    load(data(i).name)
    Freq_name = extractBefore(data(i).name, '_stat' );
    
    if Freq_name == "Alpha"
        title_name1 = Freq_name;
        title_name2 = '(8-13 Hz)';
    elseif Freq_name == "Beta"
        title_name1 = Freq_name;
        title_name2 = '(14-26 Hz)';
    elseif Freq_name == "Theta"
        title_name1 = Freq_name;
        title_name2 = '(4-7 Hz)';
    elseif Freq_name == "H"
        title_name1 = 'High Gamma';
        title_name2 = '(41-70 Hz)';
    elseif Freq_name == "L"
        title_name1 = 'Low Gamma';
        title_name2 = '(27-40 Hz)';
    end
    
    chan_lay = lay.label;
    input_data = stat.stat;
    mask = stat.mask; % testing null hypothesis
    
    % Topoplots Configuration
    cfg_topoTFR                    = [];
    cfg_topoTFR.parameter          = 'input_data';
    cfg_topoTFR.layout             = lay;
    cfg_topoTFR.marker             = 'off'; %'labels';
    cfg_topoTFR.markersymbol       = '.';
    cfg_topoTFR.markersize         = 2;
    cfg_topoTFR.colorbar           = 'EastOutside';
    cfg_topoTFR.style              = 'both'; % set it to remove contour lines
    cfg_topoTFR.colormap           = darkb2r(vmin,vmax); %tmpcolormap;
    cfg_topoTFR.highlight          = '*'; % significant channels are named use 'labels'
    cfg_topoTFR.highlightcolor     = [0 0 0];
    cfg_topoTFR.highlightsize      = 16;
    cfg_topoTFR.highlightsymbol    = '.'; %'*';
    cfg_topoTFR.comment            = 'no';
    cfg_topoTFR.interactive        = 'no';
    cfg_topoTFR.colorbartext       = 't-stat';
    cfg_topoTFR.highlightchannel   = chan_lay(find(mask)); %{'AF3'};
    cfg_topoTFR.zlim = [vmin,vmax]; %'maxmin' %[-max(input_data) max(input_data)]; %, 'maxabs', 'zeromax', 'minzero', or [zmin zmax]
    
    fprintf('For the frequency %s, the min value is %d and the max value is %d', title_name1, min(input_data), max(input_data));
    
    
    % Data structure that fieldtrip likes
    struct2plot = struct;
    struct2plot.input_data = input_data;
    struct2plot.dimord    = 'chan';
    struct2plot.parameter = input_data;
    struct2plot.label     = chan_lay; % cause it's sorted according to this order
    
    f = figure;
    ft_topoplotTFR(cfg_topoTFR, struct2plot),
    title([title_name1 newline title_name2], 'FontSize', 18)
    f.Position = [440   540   365   258];
    
    % Customizing the colorbar
    c = colorbar;
    c.FontName = 'Arial';  c.FontSize = 18;
    c.Label.String   = 't-stat';
    c.Label.FontSize = 20;
    
    saveas(gcf,fullfile([DIR_FIG, Freq_name, 'FT0-5_v3.png']));
    saveas(gcf,fullfile([DIR_FIG, Freq_name, 'FT0-5_v3.svg']));
    saveas(gcf,fullfile([DIR_FIG, Freq_name, 'FT0-5_v3.fig']));
    
end

