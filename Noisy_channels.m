
% code to create a topoplot of all the noisy channels
% Date: 25/04/2024
clear all
close all

indir = 'D:\2022_Battery_protocol_data\Pre_processed_Data\';
cd(indir)

All_bad = [];
subfiles = folders_inDir(indir);
for isub = 1: length(subfiles)
    newdir = [indir subfiles(isub).name];
    cd(newdir)
    
    dayfiles = folders_inDir(newdir);
    for iday = 1: length(dayfiles)
        newdir2 = [indir subfiles(isub).name '\' dayfiles(iday).name];
        cd(newdir2)
        
        % read the noisy channel files and store them
        % NOTE: there are two of them
        daynum = sscanf(dayfiles(iday).name,'Day%d');
        load(['bad_chan_2' subfiles(isub).name '_Day-' num2str(daynum)]);
        load(['bad_channel_ICA' subfiles(isub).name '_Day-' num2str(daynum)]);
        
        bad_comb = [bad_chan bad_chan_2];
        All_bad = [All_bad bad_comb];
        
        
    end
    
end


for i = 1:numel(All_bad)
    All_bad{i} = strrep(All_bad{i}, '-', '');
end

[unique_elements, ~, idx] = unique(All_bad);
frequency = histcounts(idx, 1:numel(unique_elements)+1);

allval = [];
for i_chan = 1: length(sorted_chanfile)
    
    label = sorted_chanfile(i_chan).labels;
    
    
    if any(strcmp(label, unique_elements))
        idx = find(strcmp(label, unique_elements));
        tmpval = frequency(idx);
    else
        tmpval = 0;
    end
    allval = [allval tmpval];
    
end

% Plot it
load('ant_neurocap_sortedtolabel_ascending') % loading chan_file
load('my_Red_Wh_colormap.mat'); % loading colormap

KB_DIR = 'C:\Users\bhadra9\OneDrive - unige.ch (1)\BCI_Group_shared';
chan_path = [KB_DIR '\Code\KB\00_Reproducable_abstract_plots\Offline_Classifier_features_Fig4\'];
load([chan_path 'Antnew_ascending_without_M1_M2_EOG.mat']) % used specifically here


data_to_plot = allval';


chan_lay = lay.label;

% Topoplots Configuration
cfg_topoTFR                    = [];
cfg_topoTFR.parameter          = 'input_data';
cfg_topoTFR.layout             = lay;
cfg_topoTFR.marker             = 'on'; %'on' %'labels';;
cfg_topoTFR.markersymbol       = '.';
cfg_topoTFR.markersize         = 2;
cfg_topoTFR.colorbar           = 'EastOutside';
cfg_topoTFR.style              = 'both'; % set it to remove contour lines
cfg_topoTFR.colormap           = my_Red_Wh_colormap; %flip(tmpcolormap,1) ; %eval(colormap_list{freq});
%     cfg_topoTFR.highlight          = '*'; % significant channels are named use 'labels'
cfg_topoTFR.highlightcolor     = [0 0 0];
cfg_topoTFR.highlightsize      = 6;
%     cfg_topoTFR.highlightsymbol    = '*';
cfg_topoTFR.comment            = 'no';
cfg_topoTFR.interactive        = 'no';
cfg_topoTFR.colorbartext       = 'Frequency';
%     cfg_topoTFR.highlightchannel   = chan_lay(find(mask)); %{'AF3'};
%         cfg_topoTFR.zlim = [min_scale max_scale]; %[-max(input_data) max(input_data)]; %'maxabs'; %'maxmin', 'maxabs', 'zeromax', 'minzero', or [zmin zmax]

input_data = data_to_plot;
% Data structure that fieldtrip likes
struct2plot = struct;
struct2plot.input_data = input_data;
struct2plot.dimord    = 'chan';
struct2plot.parameter = input_data;
struct2plot.label     = chan_lay; % cause it's sorted according to this order

f = figure;
ft_topoplotTFR(cfg_topoTFR, struct2plot),
title('Noisy Channels', 'FontSize', 18)
f.Position = [440   540   365   258];

% Customizing the colorbar
c = colorbar;
c.FontName = 'Arial';  c.FontSize = 18;
c.Label.String   = 'Frequency';
c.Label.FontSize = 25;

