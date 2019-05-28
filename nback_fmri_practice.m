function handles = nback_fmri_practice
%% INFORMATION:
% 
% *Scoring:  A' (Discrimination Index), similar to d'
%   H = Hit Rate (hits / # signal trials), 
%   F = False-positive Rate (false pos / # noise trials)
%   aPrime = .5 + sign(H - F) * ((H - F)^2 + abs(H - F)) / (4 * max(H, F) - 4 * H * F);
% 
% A' references:
%   Snodgrass, J. G., & Corwin, J. (1988). Pragmatics of measuring recognition
%       memory: applications to dementia and amnesia. Journal of Experimental
%       Psychology: General, 117(1), 34.
% 
%   Stanislaw, H., & Todorov, N. (1999). Calculation of signal detection theory
%       measures. Behavior research methods, instruments, & computers, 31(1),
%       137-149.
% 
%   *For mathematical formula, see Stanislaw & Todorov (1999, p. 142) 
%
% *Author: Elliot A. Layden (2017-18), 
% at The Environmental Neuroscience Laboratory, The University of Chicago
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Settings:
nback = 2;                  
sound_on = 1;               
position_on = 1;            
n_soundHits = 4; % control # of matches
n_positionHits = 4; % control # of matches  
nTrials = nback + 20;               
trial_time = 2; % seconds       
sound_type = 3; % Numbers-Female (1), Numbers-Male (2), Letters-Female1 (3), or Letters-Female2 (4)             
realtime_feedback = true; % Buttons flash green/red
show_remaining = true;  % Countdown trials
background_color_scheme = 1; % black (1), white (2)
volume_factor=1; % Default: 1 (no change); >1 = increase volume; <1 = decrease volume

% Hotkey Designations: 
startSessionKey = 'space'; % 'space'
begin_str = ['This is a practice ',sprintf('\n'),'session of 2-back. ',sprintf('\n'),'Press ',startSessionKey,' to begin.'];
positionMatchKey = '4';  % 'a'
positionMatchKey_str = 'Index';
soundMatchKey = '6'; % 'l'
soundMatchKey_str = 'Middle';
stopRunningKey = 'escape';

%% ------------------------ Begin ------------------------

% Initialize Global Variables:
practiceNum = 1;
timerVal = 0; curr_running = 0; stop_running = 0; ix = 1; canResize = false;
position_mem = []; sound_mem = []; txt_color = ones(1,3);
position_match_vec = []; sound_match_vec = []; 
user_position_match = []; user_sound_match = []; 

% Initialize object handles:
h_title = 10.384372;
square_width = .2;
positions = repmat([.05,.05,square_width,square_width],9,1);

% Get script path and subfolders:
script_fullpath = mfilename('fullpath');
[script_path,~,~] = fileparts(script_fullpath);
addpath(genpath(script_path))
audio_path = fullfile(script_path,'Audio');
audio_subfolders = {fullfile(audio_path,'numbers-female'),...
    fullfile(audio_path,'numbers-male'),...
    fullfile(audio_path,'letters-female1'),...
    fullfile(audio_path,'letters-female2')};

% Calculate Parameters
nTrials = nback + 20;

% Load Main Audio:
audio_dat = struct('sound',cell(1,4),'Fs',cell(1,4),'nSound',cell(1,4));
for i = 1:4
    listing = dir(audio_subfolders{i});
    nSound = length(listing)-2;
    audio_dat(i).sound = cell(1,nSound);
    audio_dat(i).Fs = cell(1,nSound);
    audio_dat(i).nSound = nSound;
    for j = 1:nSound
        [audio_dat(i).sound{j},audio_dat(i).Fs{j}] = audioread(fullfile(audio_subfolders{i},listing(j+2).name));
    end
end

% Initialize Figure:
handles.figure = figure('menubar','none','color',zeros(1,3),'numbertitle',...
    'off','name',['nback_fmri: (',num2str(nback),'-Back)'],'units','norm',...
    'Position',[.271,.109,.488,.802],'ResizeFcn',@resizeScreen); 
handles.axes1 = gca; set(handles.axes1,'Position',[0,0,1,1],'XLim',[0,1],...
    'YLim',[0,1],'Visible','off');
h_title = annotation('textbox','String','','FontName','Helvetica','FontSize',14,...
    'Position',[.4,.955,.2,.05],'Color',txt_color,'HorizontalAlignment','center',...
    'FontWeight','bold','EdgeColor','none');
   
%% Add other display elements (gridlines, rectangle, etc.):

% Gridlines:
minMeasure = .05; maxMeasure = .95; one_third = .3497; two_thirds = .6503; 
h_grid_lines = zeros(1,4);
h_grid_lines(1) = annotation('line','X',[one_third,one_third],'Y',[minMeasure,maxMeasure],'Color',txt_color,'LineWidth',1);
h_grid_lines(2) = annotation('line','X',[two_thirds,two_thirds],'Y',[minMeasure,maxMeasure],'Color',txt_color,'LineWidth',1);
h_grid_lines(3) = annotation('line','X',[minMeasure,maxMeasure],'Y',[two_thirds,two_thirds],'Color',txt_color,'LineWidth',1);
h_grid_lines(4) = annotation('line','X',[minMeasure,maxMeasure],'Y',[one_third,one_third],'Color',txt_color,'LineWidth',1);

% Initialize Match Button Text and Rectangle:
handles.h_rect = rectangle('Parent',handles.axes1,'Position',...
    [.05,.05,square_width,square_width],'Curvature',[.2,.2],'FaceColor',...
    'blue','Visible','off');
h_txt_begin = text('position',[.37,.48,1],'String', begin_str,...
    'Color',zeros(1,3),'HorizontalAlignment','center','VerticalAlignment',...
    'middle','FontSize',13,'FontWeight','bold','EdgeColor',repmat(.5020,[1,3]),...
    'BackgroundColor',[.9412,.9412,.9412]);
h_txt_pos = text('position',[.04,.01],'String',['Position:  ',positionMatchKey_str],...
    'Color',txt_color,'HorizontalAlignment','center','VerticalAlignment','top',...
    'FontSize',13,'FontWeight','normal','Visible','off','EdgeColor','none');
h_txt_sound = text('position',[.658,.01],'String',['Sound:  ',soundMatchKey_str],...
    'Color',txt_color,'HorizontalAlignment','center','VerticalAlignment','top',...
    'FontSize',13,'FontWeight','normal','Visible','off','EdgeColor','none');
if position_on; set(h_txt_pos,'Visible','on'); end
if sound_on; set(h_txt_sound,'Visible','on'); end

% Appearance Customization:
if background_color_scheme==1
    txt_color = ones(1,3);
    set(handles.figure,'Color',zeros(1,3))
elseif background_color_scheme==2
    txt_color = zeros(1,3);
    set(handles.figure,'Color',ones(1,3))
end
for ixxx = 1:4
    set(h_grid_lines(ixxx),'Color',txt_color)
end
set(h_title,'Color',txt_color); %set(h_txt_begin,'Color',txt_color)
set(h_txt_pos,'Color',txt_color); set(h_txt_sound,'Color',txt_color)

% Declare Figure Callbacks:
set(handles.figure,'WindowKeyPressFcn',@keypress_callback);

% Resize Figure function:
canResize = true;
resizeScreen;

%% Run Function:
function run_session
    set(handles.figure,'Resize','off')
    ix = 0; times = zeros(1,nTrials);
    while ix<nTrials && ~stop_running
        % Iteration and timers:
        ix = ix + 1; 
        if ix==1; times(ix) = toc(timerVal); 
        else times(ix) = times(ix-1) + toc(timerVal);
        end; timerVal = tic;
        
        % Feedback / Trial Countdown:
        if realtime_feedback
            if ishandle(h_txt_pos); set(h_txt_pos,'Color',txt_color); end
            if ishandle(h_txt_sound); set(h_txt_sound,'Color',txt_color); end
        end
        if show_remaining && ishandle(h_title); set(h_title,'String',num2str(nTrials-ix)); end
        
        % Adjust position / Play Audio:
        if position_on && ishandle(handles.h_rect); set(handles.h_rect,'Position',positions(position_mem(ix),:),'Visible','on'); end
        if sound_on && ishandle(handles.figure); sound(audio_dat(sound_type).sound{sound_mem(ix)}*volume_factor,audio_dat(sound_type).Fs{sound_mem(ix)}); end
        
        % Pause for trial time:
        if ishandle(handles.figure)
            pause(trial_time-toc(timerVal)-.0002)
        end
    end
    
    curr_running = 0; set(handles.h_rect,'Visible','off')
    set(h_txt_begin,'Visible','on'); set(h_title,'String','')
    set(h_txt_pos,'Color',txt_color); set(h_txt_sound,'Color',txt_color)
    % Calculate Score:
    if ~stop_running
        if practiceNum==1
            nback = 3;
            nTrials = nback+20;
            set(handles.figure,'name',['nback_fmri: (',num2str(nback),'-Back)'])
            begin_str = ['This is a practice ',sprintf('\n'),'session of 3-back. ',...
                sprintf('\n'),'Press ',startSessionKey,' to begin.'];
            set(h_txt_begin,'String',begin_str);
            practiceNum = 2;
        elseif practiceNum==2
            nback = 2;
            nTrials = nback+20;
            set(handles.figure,'name',['nback_fmri: (',num2str(nback),'-Back)'])
            begin_str = ['This is a practice ',sprintf('\n'),'session of 2-back. ',...
                sprintf('\n'),'Press ',startSessionKey,' to begin.'];
            set(h_txt_begin,'String',begin_str);
            practiceNum = 1;
        end
    else stop_running = 0;
    end
    set(handles.figure,'Resize','on')
end

% Key Press Callback:
function keypress_callback(~,which_key,~)
    if ishandle(handles.figure)
        switch which_key.Key
            case startSessionKey
                if ~curr_running
                    timerVal = tic;
                    curr_running = 1; set(h_txt_begin,'Visible','off')
                    position_mem_match = zeros(1,nTrials-nback); 
                    position_mem_match(1:n_positionHits) = 1;
                    sound_mem_match = zeros(1,nTrials-nback); 
                    sound_mem_match(1:n_soundHits) = 1;
                    % Shuffle Match Vectors:
                    position_mem_match = position_mem_match(randperm(length(position_mem_match)));
                    position_mem_match = [zeros(1,nback),position_mem_match]; % pad
                    sound_mem_match = sound_mem_match(randperm(length(sound_mem_match)));
                    sound_mem_match = [zeros(1,nback),sound_mem_match]; % pad
                    % Generate new mem's:
                    position_mem = zeros(1,nTrials); sound_mem = zeros(1,nTrials);
                    for ixx = 1:nTrials
                        % Position:
                        if position_mem_match(ixx) && ixx>nback
                            position_mem(ixx) = position_mem(ixx-nback);
                        elseif ixx<=nback
                            position_mem(ixx) = randi(9,1);
                        else
                            position_bank = setdiff(1:9,position_mem(ixx-nback));
                            position_mem(ixx) = position_bank(randperm(8,1));
                        end
                        % Sound:
                        if sound_mem_match(ixx) && ixx>nback
                            sound_mem(ixx) = sound_mem(ixx-nback);
                        elseif ixx<=nback
                            sound_mem = randi(audio_dat(sound_type).nSound,[1,nTrials]);
                            sound_mem(ixx) = randi(audio_dat(sound_type).nSound,1);
                        else
                            sound_bank = setdiff(1:audio_dat(sound_type).nSound,sound_mem(ixx-nback));
                            sound_mem(ixx) = sound_bank(randperm(audio_dat(sound_type).nSound-1,1));
                        end
                    end
                    position_match_vec = [position_mem,nan(1,nback)]==[zeros(1,nback),position_mem];
                    sound_match_vec = [sound_mem,nan(1,nback)]==[zeros(1,nback),sound_mem];
                    user_position_match = nan(1,nTrials+nback); user_sound_match = nan(1,nTrials+nback); %#ok
                    pause(trial_time-toc(timerVal)-0.0005)
                    run_session
                end
            case positionMatchKey
                if curr_running
                    user_position_match(ix) = 1;
                    if realtime_feedback
                        if position_match_vec(ix)
                            set(h_txt_pos,'Color',[0,1,0])
                        else
                            set(h_txt_pos,'Color',[1,0,0]) 
                        end
                    end
                end
            case soundMatchKey
                if curr_running
                    user_sound_match(ix) = 1;
                    if realtime_feedback
                        if sound_match_vec(ix)
                            set(h_txt_sound,'Color',[0,1,0])
                        else
                            set(h_txt_sound,'Color',[1,0,0]) 
                        end
                    end
                end
            case stopRunningKey
                stop_running = 1;
        end
    end
end

function resizeScreen(varargin)    
    if canResize 
        % Determine which dimension needs shrinking to become square:
        for ixx = 1:4; set(h_grid_lines(ixx),'units','norm'); end
        set(h_grid_lines(1),'Y',[minMeasure,maxMeasure])
        set(h_grid_lines(2),'Y',[minMeasure,maxMeasure])
        set(h_grid_lines(3),'X',[minMeasure,maxMeasure])
        set(h_grid_lines(4),'X',[minMeasure,maxMeasure])

        % Change units to points and shrink width or height to match:
        for ixx = 1:4; set(h_grid_lines(ixx),'units','points'); end
        ySpec = get(h_grid_lines(1),'Y'); xSpec = get(h_grid_lines(3),'X');
        height = diff(ySpec); width = diff(xSpec);
        axesRatio = height/width;
        diffPixels = abs(height - width);
        if height > width % shrink height to match
            set(h_grid_lines(1),'Y',[ySpec(1) + diffPixels/2, ySpec(2) - diffPixels/2])
            set(h_grid_lines(2),'Y',[ySpec(1) + diffPixels/2, ySpec(2) - diffPixels/2])
        elseif height < width % shrink width to match
            set(h_grid_lines(3),'X',[xSpec(1) + diffPixels/2, xSpec(2) - diffPixels/2])
            set(h_grid_lines(4),'X',[xSpec(1) + diffPixels/2, xSpec(2) - diffPixels/2])
        end
        
        % Change units back to normalized, and adjust grid spacing:
        for ixx = 1:4; set(h_grid_lines(ixx),'units','norm'); end
        xSpec = get(h_grid_lines(3),'X'); ySpec = get(h_grid_lines(1),'Y'); 
        oneThirdX = (diff(xSpec)/3) + xSpec(1);
        twoThirdsX = xSpec(2) - (diff(xSpec)/3);
        oneThirdY = (diff(ySpec)/3) + ySpec(1);
        twoThirdsY = ySpec(2) - (diff(ySpec)/3);
        set(h_grid_lines(1),'X',[oneThirdX, oneThirdX])
        set(h_grid_lines(2),'X',[twoThirdsX, twoThirdsX])
        set(h_grid_lines(3),'Y',[oneThirdY, oneThirdY])
        set(h_grid_lines(4),'Y',[twoThirdsY, twoThirdsY])

        % Square Positions:
        ylim(handles.axes1,[0,axesRatio]); % adjust Y-axes limits based on its ratio w/ X
        square_width = .95*(twoThirdsX-oneThirdX);
        useGapX = ((twoThirdsX-oneThirdX)-square_width)/2;
        oneThirdY = oneThirdY*axesRatio; twoThirdsY = twoThirdsY*axesRatio;
        useGapY = ((twoThirdsY-oneThirdY)-square_width)/2;
        positions = repmat([.05,.05,square_width,square_width],9,1);
        positions(1,1:2) = [xSpec(1)+useGapX,twoThirdsY+useGapY];
        positions(2,1:2) = [oneThirdX+useGapX,twoThirdsY+useGapY];
        positions(3,1:2) = [twoThirdsX+useGapX,twoThirdsY+useGapY];
        positions(4,1:2) = [xSpec(1)+useGapX,oneThirdY+useGapY];
        positions(5,1:2) = [oneThirdX+useGapX,oneThirdY+useGapY];
        positions(6,1:2) = [twoThirdsX+useGapX,oneThirdY+useGapY];
        positions(7,1:2) = [xSpec(1)+useGapX,ySpec(1)*axesRatio+useGapY];
        positions(8,1:2) = [oneThirdX+useGapX,ySpec(1)*axesRatio+useGapY];
        positions(9,1:2) = [twoThirdsX+useGapX,ySpec(1)*axesRatio+useGapY];
        
        % Adjust match button text positions:
        set(h_txt_pos,'position',[median([xSpec(1),oneThirdX]),ySpec(1)*axesRatio])
        set(h_txt_sound,'position',[median([twoThirdsX,xSpec(2)]),ySpec(1)*axesRatio])
        set(h_txt_begin,'position',[median([oneThirdX, twoThirdsX]),median(ySpec)*axesRatio])
        set(h_txt_begin,'BackgroundColor',[.9412,.9412,.9412])
        get(h_txt_begin)
    end
end

end