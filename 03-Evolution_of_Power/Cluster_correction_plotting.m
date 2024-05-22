%% Cluster based correction
% This code is to do multiple comparison correction based on cluster
% correction using the neighbours channel information. Earlier we checked
% the fieldtrip based code and have created / converted the neighbours
% channel file into a connectivity matrix and that is supposed to give us a
% cluster correction.

clear all
close all

% Load neighbours
load('easycap64ch-avg_neighb_withCPZ');

indir_rand   = 'C:\Users\bhadra9\OneDrive - unige.ch (1)\BCI_Group_shared\Code\KB\00_Reproducable_abstract_plots\Fig2_Power_Evolution\Output_data\Resampled_data_v4\';
indir_obs    = 'C:\Users\bhadra9\OneDrive - unige.ch (1)\BCI_Group_shared\Code\KB\00_Reproducable_abstract_plots\Fig2_Power_Evolution\Output_data\Observed_data_v4\';
outdir_plots = 'C:\Users\bhadra9\OneDrive - unige.ch (1)\BCI_Group_shared\Code\KB\00_Reproducable_abstract_plots\Fig2_Power_Evolution\Plot\';


% List the csv files:
cd(indir_rand)
data_list_rand = dir('*tstat*.csv');% dir('*t_val_in*.csv'); ---> for the Brain-Behav analysis

cd(indir_obs)
data_list_obs = dir('*.csv'); % dir('*subnorm*.csv'); ---> for the Brain-Behav analysis

% The connectivity matrix that has been created from neighbours file
load('Ant_neuro_connectivity')
% load('ant_neurocap_sortedtolabel_wCPZ')
load('Antnew_without_with_ref') % follows ant-neuro sequence % used for fieldtrip only
load('C:\Users\bhadra9\Documents\MATLAB\Cyan_black_gradient.mat') % ora_vio_colormap
% tmpcolormap = ft_colormap('RdBu'); % PuOr % cool
cfg_topoTFR                    = [];
% cfg_topoTFR.colormap           = flip(tmpcolormap,1); % tmpcolormap; 
chan_lay = lay.label;

vmin = -1;
vmax = 4.5;

for i_freq = 1: length(data_list_rand)
    
    cd(indir_rand)
    datarand = readtable(data_list_rand(i_freq).name); % NOTE use the +10 for Brain-Behav analysis only
    
    cd(indir_obs)
    dataobs = readtable(data_list_obs(i_freq).name);
    
    % For the title:
    freq_name = char(table2cell(dataobs(1,1)));
    temp_name = strrep(freq_name ,'_','.') ;
    
    % Assigning the freq ranges
    if temp_name == "Alpha"
        title_name = [temp_name newline '(8-13 Hz)'];
    elseif temp_name == "Beta"
        title_name = [temp_name newline '(13-26 Hz)'];
    elseif temp_name == "H"
        title_name = ['High Gamma' newline '(41-70 Hz)'];
    elseif temp_name == "L"
        title_name = ['Low Gamma' newline '(26-40 Hz)'];
    elseif temp_name == "Theta"
        title_name = [temp_name newline '(4-7 Hz)'];
    end
    
    
    statrnd = table2array(datarand(:,3:end)); % the matrix should be 62chan by nrands
    statobs = table2array(dataobs(:,4))'; % the matrix should 1 by 62chan % table2array(dataobs(:,12))'; --> for BB analysis
    
    % The function we call from fieldtrip is in the private folder. SO we
    % need to change the directory
    cd('C:\Users\bhadra9\Documents\MATLAB\fieldtrip-20200128\fieldtrip-20200128\private\')
    cfg = [];
    cfg.dim = 62;
    %   cfg.inside (only for source data)
    cfg.tail = 0;
    cfg.label = dataobs(:,2);
    cfg.alpha = 0.025;
    cfg.multivariate = 'no';
    cfg.orderedstats = 'no';
    cfg.clusterstatistic = 'maxsum'; % maxsize : priority to maxsize of the cluster
    cfg.clusterthreshold = 'parametric';
    cfg.clusteralpha = 0.025;
    cfg.clustertail  = 0;
    
    % critical value!!
    df = 60; % max(dataobs.df_val_interctn);
    critval = [tinv(cfg.alpha/2,df),tinv(1-cfg.alpha/2,df)];
    
    cfg.clustercritval = critval; % !! Work on this
    cfg.connectivity = sparse(connectivity);
    %   cfg.wcm_weight
    cfg.feedback = "text";
    cfg.numrandomization = 1000;
    
    
    [stat, cfg] = clusterstat(cfg, statrnd, statobs);
    
    %     stat.mask = stat.prob<=cfg.alpha;
    %     stat.label = table2cell(cfg.label);
    %     sig_data(:, i_freq) = double(stat.mask)';
    %     find(sig_data==1)
    
    % ------------------------------------------
    Nrand = cfg.numrandomization;
    
    stat.prob = stat.prob .* 2;
    stat.prob(stat.prob>1) = 1; % clip at p=1
    % also correct the probabilities in the pos/negcluster fields
    if isfield(stat, 'posclusters')
        for i=1:length(stat.posclusters)
            stat.posclusters(i).prob = stat.posclusters(i).prob*2;
            if stat.posclusters(i).prob>1; stat.posclusters(i).prob = 1; end
        end
    end
    if isfield(stat, 'negclusters')
        for i=1:length(stat.negclusters)
            stat.negclusters(i).prob = stat.negclusters(i).prob*2;
            if stat.negclusters(i).prob>1; stat.negclusters(i).prob = 1; end
        end
    end
    
    % compute range of confidence interval p ? 1.96(sqrt(var(p))), with var(p)= var(x/n) = p*(1-p)/N
    stddev = sqrt(stat.prob.*(1-stat.prob)/Nrand);
    stat.cirange = 1.96*stddev;
    
    if isfield(stat, 'posclusters')
        for i=1:length(stat.posclusters)
            stat.posclusters(i).stddev  = sqrt(stat.posclusters(i).prob.*(1-stat.posclusters(i).prob)/Nrand);
            stat.posclusters(i).cirange =  1.96*stat.posclusters(i).stddev;
            if i==1 && stat.posclusters(i).prob<cfg.alpha && stat.posclusters(i).prob+stat.posclusters(i).cirange>=cfg.alpha
                ft_warning('FieldTrip:posCluster_exceeds_alpha', sprintf('The p-value confidence interval of positive cluster #%i includes %.3f - consider increasing the number of permutations!', i, cfg.alpha));
            end
        end
    end
    if isfield(stat, 'negclusters')
        for i=1:length(stat.negclusters)
            stat.negclusters(i).stddev  = sqrt(stat.negclusters(i).prob.*(1-stat.negclusters(i).prob)/Nrand);
            stat.negclusters(i).cirange =  1.96*stat.negclusters(i).stddev;
            if i==1 && stat.negclusters(i).prob<cfg.alpha && stat.negclusters(i).prob+stat.negclusters(i).cirange>=cfg.alpha
                ft_warning('FieldTrip:negCluster_exceeds_alpha', sprintf('The p-value confidence interval of negative cluster #%i includes %.3f - consider increasing the number of permutations!', i, cfg.alpha));
            end
        end
    end
    
    stat.mask = stat.prob<=cfg.alpha;
    
%     G = figure;
    %     addpath(genpath('C:\Users\bhadra9\Documents\MATLAB\eeglab14_1_0b')) % adding the folders and subfolders to the MATLAB path
    %     topoplot(statobs , sorted_chanfile, 'electrodes','on','maplimits', [1 6], 'style', 'map'), title(freq_name,'FontSize',16);
    %     topoplot(stat.mask, sorted_chanfile, 'electrodes','on', 'style', 'blank');
    %     colormap(Cyan_black_gradient) % change the colormap
    %     rmpath(genpath('C:\Users\bhadra9\Documents\MATLAB\eeglab14_1_0b'))
    
    %     colorbar
    %     cb = colorbar('FontSize',18);
    %     cb.Label.String = "T-stat";
    
    % Fieldtrip version of the topoplots
    load('Antnew_without_with_ref') % follows ant-neuro sequence % used for fieldtrip only
    % load('ant_neurocap_sortedtolabel_wCPZ_ascending')  % follows alphabetical order of the channels (limited usage)
    % Colorbars: Pink_gradient, Green_gradient, Cyan_gradient, ora_vio_colormap (yet to finalize the final one)
    % load('C:\Users\bhadra9\Documents\MATLAB\ora_vio_colormap.mat') %
%     tmpcolormap = ft_colormap('RdBu'); % PuOr
    chan_lay = lay.label;
    
    input_data = statobs;
    mask = stat.mask; % testing null hypothesis
    
    % Topoplots Configuration
%     cfg_topoTFR                    = [];
    cfg_topoTFR.parameter          = 'input_data';
    cfg_topoTFR.layout             = lay;
    cfg_topoTFR.marker             = 'off'; %'labels';
    cfg_topoTFR.markersymbol       = '.';
    cfg_topoTFR.markersize         = 2;
    cfg_topoTFR.colorbar           = 'EastOutside';
    cfg_topoTFR.style              = 'both'; % set it to remove contour lines
%     cfg_topoTFR.colormap           = flip(tmpcolormap,1); % tmpcolormap; 
    cfg_topoTFR.colormap           = darkb2r(vmin,vmax); %tmpcolormap;
    cfg_topoTFR.highlight          = '*'; % significant channels are named use 'labels'
    cfg_topoTFR.highlightcolor     = [0 0 0];
    cfg_topoTFR.highlightsize      = 16; %6;
    cfg_topoTFR.highlightsymbol    = '.';
    cfg_topoTFR.comment            = 'no';
    cfg_topoTFR.interactive        = 'no';
    cfg_topoTFR.colorbartext       = 't-stat';
    cfg_topoTFR.highlightchannel   = chan_lay(find(mask)); %{'AF3'};
    cfg_topoTFR.zlim = [vmin,vmax]; %'maxmin'; % %[-max(input_data) max(input_data)]; %'maxabs'; %'maxmin', 'maxabs', 'zeromax', 'minzero', or [zmin zmax]
    
    
    % Data structure that fieldtrip likes
    struct2plot = struct;
    struct2plot.input_data = input_data';
    struct2plot.dimord    = 'chan';
    struct2plot.parameter = input_data';
    struct2plot.label     = chan_lay; % cause it's sorted according to this order
    
    % to change the default background color of the figure dialogue
    %     f = figure('Color',[0 0 0],'InvertHardcopy','off');
    %     figure('Color',[1.0000 0.9059 0.2595],'InvertHardcopy','off');
    f = figure;
    ft_topoplotTFR(cfg_topoTFR, struct2plot),
    title(title_name, 'FontSize', 18)
    f.Position = [440   540   365   258];
    
    % Customizing the colorbar
    c = colorbar;
    c.FontName = 'Arial';  c.FontSize = 18;
    c.Label.String   = 't-stat';
    c.Label.FontSize = 20;
    
    % save the corrected
    saveas(f,fullfile([outdir_plots,['Brain-evo_new_countour_corr6', freq_name, '.png']]));
    saveas(f,fullfile([outdir_plots,['Brain-evo_new_countour_corr6', freq_name, '.eps']]));

    
end

