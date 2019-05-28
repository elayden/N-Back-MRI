function handles = nback_fmri

% Specify subject ID# and Session#:
subject_id = '1001';  
whichSession = 1; 

% Settings:
nMatches = 4;
trial_time = 3; % seconds  
roundIntermissionTime = 10; % seconds between rounds
percLure = .25; % proportion of non-match trials to become lures (n-back +/- 1) (default: .25, same as Android n-back, will 
    % yield 4 lures per round for either 2-back or 3-back)
    % nLures will be calculated as, nLures = round(percLure * (nTrials - nMatches - nback + 1));
sound_type = 1; % Numbers-Female (1), Numbers-Male (2), Letters-Female1 (3, DEFAULT), or Letters-Female2 (4) 
whichRound = 1; % 1:6 (don't change unless restarting collection midway through)

% Hotkey Designations: 
startSessionKey = 's'; % other: 'space'
positionMatchKey = '4';  % actual key
positionMatchKey_str = 'Index'; % key label
soundMatchKey = '6'; % actual key
soundMatchKey_str = 'Middle'; % key label
stopRunningKey = 'escape';

% Font Sizes:
messageFontSize = 24;
countdownFontSize = 64;
buttonLabelFontSize = 20;

% Debugging (leave these true unless testing)
playSilentSound = true; % if true, plays a silent sound during countdown to warm up system
adjustTimes = true; % if true, adjusts pause length to keep accurate time on each trial

%% INFO:
% *Data for each run will be saved in a separate .mat as:
%   subject_%02g_session_%g_run_%g.mat
%       summaryStats = a table of 4 rows containing columns A', hit_rate, 
%           and false-alarm_rate, for each round within a run
%       trialData = a 1x4 cell array containing one table of trial data per
%           cell/run
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
%  The Environmental Neuroscience Laboratory, The University of Chicago
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Settings:

% Don't need to change these:
nback = 2;                  
sound_on = 1;               
position_on = 1;   
nTrials = nback + 20;                            
nLures = round(percLure * (nTrials - nMatches - nback + 1));
realtime_feedback = true; % Buttons flash green/red
background_color_scheme = 1; % black (1), white (2)
volume_factor=1; % Default: 1 (no change); >1 = increase volume; <1 = decrease volume

% Strings:
initial_str = 'Please wait.'; 
% begin_str = [num2str(nback),'-back',sprintf('\n'),'starting in...'];

% begin_str = ['This will be a ',sprintf('\n'),'session of ',num2str(nback),...
%     '-back.',sprintf('\n'),'Please wait.']; % what is printed in middle

%% ------------------------ Begin ------------------------

% Initialize Global Variables:
timerVal = 0; firstTrial = true;
curr_running = 0; stop_running = 0; iter = 1; canResize = false;
position_mem = []; sound_mem = []; txt_color = ones(1,3);
position_match_vec = []; sound_match_vec = []; 
position_lures_vec = []; sound_lures_vec = []; 
user_position_match = []; user_sound_match = []; 
extraKeys = {};

% Determine target countdown & trial timer ticks:
targetTimes = zeros(roundIntermissionTime*6 + 22*4 + 23*2, 1);
ticks = 1; countTimes = 1;
for j = 1:6 % round
    for k = 1:roundIntermissionTime
        if countTimes > 1
            targetTimes(countTimes) =  targetTimes(countTimes-1) + 1;
        else targetTimes(countTimes) = 1;
        end
        countTimes = countTimes+1;
    end
    if j < 5; n = 22; else n = 23; end
    for k = 1:n
        targetTimes(countTimes) =  targetTimes(countTimes-1) + trial_time;
        countTimes = countTimes+1;
    end
end

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

% Create subject folder:
if ~isdir(fullfile(script_path,'Data')); mkdir(fullfile(script_path,'Data')); end
subject_folder = fullfile(script_path,'Data',subject_id);
if ~isdir(subject_folder); mkdir(subject_folder); end

% Calculate Parameters
nTrials = nback + 20;

% Load Main Audio:
audio_dat = struct('sound',cell(1,4),'Fs',cell(1,4),'nSound',cell(1,4));
listing = dir(fullfile(audio_subfolders{sound_type},'*.wav'));
nSound = length(listing);
audio_dat(sound_type).sound = cell(1,nSound);
audio_dat(sound_type).Fs = cell(1,nSound);
audio_dat(sound_type).nSound = nSound;
for j = 1:nSound
    [audio_dat(sound_type).sound{j},audio_dat(sound_type).Fs{j}] = audioread(fullfile(audio_subfolders{sound_type},listing(j).name));
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
h_txt_begin = annotation('textbox',[.37,.48,.26,.04],'String', initial_str,...
    'Color',txt_color,'HorizontalAlignment','center','VerticalAlignment',...
    'middle','FontSize',messageFontSize,'FontWeight','bold','EdgeColor','none');
h_txt_countdown = text('position',[.04,.01],'String',num2str(roundIntermissionTime),...
    'Color',txt_color,'HorizontalAlignment','center','VerticalAlignment','middle',...
    'FontSize',countdownFontSize,'FontWeight','normal','Visible','off','EdgeColor','none');
h_txt_pos = text('position',[.04,.01],'String',['Position:  ',positionMatchKey_str],...
    'Color',txt_color,'HorizontalAlignment','center','VerticalAlignment','top',...
    'FontSize',buttonLabelFontSize,'FontWeight','bold','Visible','off','EdgeColor','none');
h_txt_sound = text('position',[.658,.01],'String',['Sound:  ',soundMatchKey_str],...
    'Color',txt_color,'HorizontalAlignment','center','VerticalAlignment','top',...
    'FontSize',buttonLabelFontSize,'FontWeight','bold','Visible','off','EdgeColor','none');
if position_on; set(h_txt_pos,'Visible','on'); end
if sound_on;    set(h_txt_sound,'Visible','on'); end

% Appearance Customization:
if background_color_scheme==1
    txt_color = ones(1,3);
    set(handles.figure,'Color',zeros(1,3))
elseif background_color_scheme==2
    txt_color = zeros(1,3);
    set(handles.figure,'Color',ones(1,3))
end
for ix = 1:4
    set(h_grid_lines(ix),'Color',txt_color)
end
set(h_title,'Color',txt_color); set(h_txt_begin,'Color',txt_color)
set(h_txt_pos,'Color',txt_color); set(h_txt_sound,'Color',txt_color)
set(h_txt_countdown,'Color',txt_color);

% Declare Figure Callbacks:
set(handles.figure,'WindowKeyPressFcn',@keypress_callback);
set(handles.figure,'CloseRequestFcn',@close_nback_fmri)

% Resize Figure function:
canResize = true;
resizeScreen;

%% Run Function:
function run_session
    extraKeys = cell(nTrials,1);
    curr_running = 1;
    
    % Iterate through Trials:
    iter = 0; times = zeros(nTrials,1);
    while iter<nTrials && ~stop_running
        % Iteration and timers:
        iter = iter + 1; 
        times(iter) = toc(timerVal);
        
        % Feedback / Trial Countdown:
        if realtime_feedback
            if ishandle(h_txt_pos); set(h_txt_pos,'Color',txt_color); end
            if ishandle(h_txt_sound); set(h_txt_sound,'Color',txt_color); end
        end
        
        % Adjust position / Play Audio:
        if sound_on; sound(audio_dat(sound_type).sound{sound_mem(iter)}*volume_factor,audio_dat(sound_type).Fs{sound_mem(iter)}); end
        if position_on; set(handles.h_rect,'Position',positions(position_mem(iter),:),'Visible','on'); end

        % Pause for trial time minus execution delays:
%         [toc(timerVal),targetTimes(ticks)]
        if adjustTimes
            if abs(toc(timerVal)-targetTimes(ticks)) < .5
                pause(trial_time - (toc(timerVal)-targetTimes(ticks)));
            else
                pause(trial_time - sign(toc(timerVal)-targetTimes(ticks))*.5);
            end
        else
            pause(trial_time - .005);
        end
        ticks = ticks+1; 
%         disp(['pause trial (',num2str(iter),'):  ',num2str(trial_time - (toc(timerVal)-targetTimes(ticks)))])
    end
    
    % Adjust GUI elements:
    curr_running = 0; set(handles.h_rect,'Visible','off')
    set(h_txt_begin,'Visible','on'); set(h_title,'String','')
    set(h_txt_pos,'Color',txt_color); set(h_txt_sound,'Color',txt_color)
    set(h_txt_countdown,'Color',txt_color);
    
    % Calculate Scores:
    if ~stop_running
        
        % Position Stats:
        position_hits = sum((position_match_vec+user_position_match)==2);
        position_false = sum((position_match_vec-user_position_match)==-1);
        position_lure_errors = sum((position_lures_vec+user_position_match)==2)/nLures;
        H_pos = position_hits / nMatches;
        F_pos = position_false / (nTrials - nMatches);
        pos_score = .5 + sign(H_pos - F_pos) * ((H_pos - F_pos)^2 + abs(H_pos - F_pos)) / (4 * max(H_pos, F_pos) - 4 * H_pos * F_pos);
        
        % Sound Stats:
        sound_hits = sum((sound_match_vec+user_sound_match)==2);
        sound_false = sum((sound_match_vec-user_sound_match)==-1);
        sound_lure_errors = sum((sound_lures_vec+user_sound_match)==2)/nLures;
        H_sound = sound_hits / nMatches; F_sound = sound_false / (nTrials - nMatches);
        sound_score = .5 + sign(H_sound - F_sound) * ((H_sound - F_sound)^2 + abs(H_sound - F_sound)) / (4 * max(H_sound, F_sound) - 4 * H_sound * F_sound);
        
        user_position_match = user_position_match==1;
        user_sound_match = user_sound_match==1;
        
        % Write Data:
        writeData(pos_score, position_lure_errors, H_pos, F_pos, ...
            sound_score, sound_lure_errors, H_sound, F_sound, ...
            (1:nTrials)', times, position_match_vec', position_lures_vec', ...
            user_position_match', position_mem', sound_match_vec', sound_lures_vec', ...
            user_sound_match', sound_mem', extraKeys)
        
        % Iterate Session, Run, Round:   
        if whichRound==4
            whichRound = whichRound + 1; nback = 3; nTrials = nback + 20;
            countDownTimer(roundIntermissionTime);
        elseif whichRound==6
            set(h_txt_begin,'String',['Finished N-Back',sprintf('\n'),'Session ',num2str(whichSession)]);
            set(handles.figure,'WindowKeyPressFcn',[]);
        else
            whichRound = whichRound + 1;
            countDownTimer(roundIntermissionTime);
        end
     
    else
        set(h_txt_countdown,'Visible','off'); set(h_txt_begin,'String',initial_str);
        stop_running = 0; curr_running = 0;
    end
       
end

function countDownTimer(intermission) 
    stop_running = false;
    for i = 1:4; set(h_grid_lines(i),'Visible','off'); end
    set(h_txt_pos,'Visible','off'); set(h_txt_sound,'Visible','off')
    set(handles.figure,'name',['nback_fmri: (',num2str(nback),'-Back)'])
    set(h_txt_begin,'String',[num2str(nback),'-back',sprintf('\n'),'starting in...'],'Visible','on');
    set(h_txt_countdown,'Visible','on','String',num2str(intermission));
    for i = intermission-1:-1:0
        if ~stop_running
            if firstTrial
                pause(1); firstTrial = false; 
            elseif i==1
                if playSilentSound
                    sound(zeros(trial_time*1000,1),audio_dat(sound_type).Fs{1}); 
                end
                pause(1 - (toc(timerVal)-targetTimes(ticks)));
                ticks = ticks+1;
            else
%                 [toc(timerVal),targetTimes(ticks)]
                pause(1 - (toc(timerVal)-targetTimes(ticks)));
                ticks = ticks+1;
%                 ['pause countdown (',num2str(countticks),'): ',num2str(num2str(toc(timerVal))),num2str(targetTimes(ticks))]
            end
            set(h_txt_countdown,'String',num2str(i))
        else
            stop_running = 0; curr_running = 0;
            set(h_txt_countdown,'Visible','off'); set(h_txt_begin,'String',initial_str);
            return;
        end
    end
    set(h_txt_countdown,'Visible','off'); set(h_txt_begin,'Visible','off');
    
    % Generate Idx:
    wasSuccess = false;
    while ~wasSuccess
        [position_mem, position_match_vec, position_lures_vec, wasSuccess] = generateIdx(nTrials, nback, nMatches, percLure, 9);
    end
    
    wasSuccess = false;
    while ~wasSuccess
        [sound_mem, sound_match_vec, sound_lures_vec, wasSuccess] = generateIdx(nTrials, nback, nMatches, percLure, audio_dat(sound_type).nSound);
    end
    
    user_position_match = nan(1, nTrials); 
    user_sound_match = nan(1, nTrials); 
    
    % Initialize sound to prevent lag:
%     sound(zeros(trial_time*1000,1),audio_dat(sound_type).Fs{sound_mem(1)}); 
%     pause(trial_time-0.0005)
    for i = 1:4; set(h_grid_lines(i),'Visible','on'); end
    set(h_txt_pos,'Visible','on'); set(h_txt_sound,'Visible','on')
    
    % Run new session:
    run_session;
end

function writeData(pos_score, pos_lure_errors, H_pos, F_pos, ...
        sound_score, sound_lure_errors, H_sound, F_sound, trials, times, ...
        position_matches, pos_lures, position_user, position_stimuli,...
        sound_matches, sound_lures, sound_user, sound_stimuli, extraKeys)
    
%         fname = sprintf('subject_%03g_session_%g.mat',[subject_id, whichSession]);
        fname = ['subject_',subject_id,'_session_',num2str(whichSession),'.mat'];
        
        if exist(fullfile(subject_folder,fname),'file')
            load(fullfile(subject_folder,fname));
            summaryStats2 = table(whichRound, {char(datetime)}, nback, pos_score, ...
                pos_lure_errors, H_pos, F_pos, sound_score, ...
                sound_lure_errors, H_sound, F_sound,'VariableNames',...
                {'round','date_time','nback_level',...
                'A_pos','lure_errors_pos','hits_pos','false_alarms_pos',...
                'A_sound','lure_errors_sound','hits_sound','false_alarms_sound'});
            summaryStats = [summaryStats; summaryStats2]; %#ok
            trialData{whichRound} = table(trials,times,position_matches,...
                pos_lures, position_user, position_stimuli, ...
                sound_matches, sound_lures, sound_user, sound_stimuli, extraKeys,...
                'VariableNames',{'trial_num','seconds_from_start',...
                'position_matches','position_lures','user_position_matches','position_stimuli',...
                'sound_matches','sound_lures','user_sound_matches','sound_stimuli','extra_keys'}); %#ok        
        else
            summaryStats = table(whichRound, {char(datetime)}, nback, pos_score, ...
                pos_lure_errors, H_pos, F_pos, sound_score, ...
                sound_lure_errors, H_sound, F_sound,'VariableNames',...
                {'round','date_time','nback_level',...
                'A_pos','lure_errors_pos','hits_pos','false_alarms_pos',...
                'A_sound','lure_errors_sound','hits_sound','false_alarms_sound'}); %#ok
            trialData = cell(1,6);
            trialData{whichRound} = table(trials,times,position_matches,...
                pos_lures, position_user, position_stimuli, ...
                sound_matches, sound_lures, sound_user, sound_stimuli, extraKeys,...
                'VariableNames',{'trial_num','seconds_from_start',...
                'position_matches','position_lures','user_position_matches','position_stimuli',...
                'sound_matches','sound_lures','user_sound_matches','sound_stimuli','extra_keys'}); %#ok  
        end
        save(fullfile(subject_folder,fname),'summaryStats','trialData')
end

% Key Press Callback:
function keypress_callback(~,which_key,~)
    switch which_key.Key
        case startSessionKey
            if ~curr_running
                timerVal = tic; ticks = 1; firstTrial = true;
                countDownTimer(roundIntermissionTime);
            end
        case positionMatchKey
            if curr_running
                user_position_match(iter) = 1;
                if realtime_feedback
                    if position_match_vec(iter)
                        set(h_txt_pos,'Color',[0,1,0])
                    else
                        set(h_txt_pos,'Color',[1,0,0]) 
                    end
                end
            end
        case soundMatchKey
            if curr_running
                user_sound_match(iter) = 1;
                if realtime_feedback
                    if sound_match_vec(iter)
                        set(h_txt_sound,'Color',[0,1,0])
                    else
                        set(h_txt_sound,'Color',[1,0,0]) 
                    end
                end
            end
        case stopRunningKey
            stop_running = 1;
        otherwise
            if curr_running
                extraKeys{iter} = [extraKeys{iter}, ',', which_key.Key];
            end
    end
end

% Close Requests:
function close_nback_fmri(varargin)
    selection = questdlg('Are you sure you want to exit?','Exit',...
      'Yes','No','Yes');
    switch selection,
        case 'Yes'
            delete(handles.figure)
        case 'No'
          return
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
        set(h_txt_countdown,'position',[median([oneThirdX, twoThirdsX]),median([ySpec(1)*axesRatio, oneThirdY])])
    end
end

% Generate Indices of Matches and Lures Function:
function [idx, matches, lures, countMatches, countLures, wasSuccess] = generateIdx(nTrials, nback, nMatches, percLure, nStimuli)

    wasSuccess = true;
    
    nPassLimit = 30;
    if nback==1
        nLures = round(percLure * (nTrials - nMatches));
    else
        nLures = round(percLure * (nTrials - nMatches - nback + 1));
    end

    %% 1. Add matches:
    idx = zeros(1,nTrials); nMatchesReal = 0; nLuresReal = 0; nPasses = 0;
    while (nMatchesReal ~= nMatches || nLuresReal > nLures) && nPasses<nPassLimit
        nPasses = nPasses + 1;
        matchInd = find(idx==0); matchInd(matchInd<=nback) = [];
        matchInd = matchInd(randperm(length(matchInd),length(matchInd)));
        if nMatchesReal < nMatches
            if  idx(matchInd(1)-nback)>0 % match already present
                idx(matchInd(1)) = idx(matchInd(1)-nback);
            else % match not present
                idx(matchInd(1)) = randi(nStimuli);
                idx(matchInd(1)-nback) = idx(matchInd(1));
            end
        elseif ((nMatchesReal > nMatches) && (nLuresReal <= nLures))
            opts = find(idx>0); 
            if ~isempty(opts)
                idx(opts(randi(length(opts),1))) = 0;
                idx = clearNonMatches(idx, nback);
            end
        elseif (nLuresReal > nLures)
            [~, lureInd] = findLures(idx,nback);
            idx(lureInd) = 0;
        end
        nMatchesReal = findMatches(idx, nback); nLuresReal = findLures(idx, nback);
    end
    idx = clearNonMatches(idx, nback);
    
    %% 2. Add Lures:
    lurePoss = find(idx==0);
    if nback>1; 
        lurePoss(lurePoss<nback) = [];
    else lurePoss(lurePoss<nback+1) = [];
    end
    lurePossShuffled = lurePoss(randperm(length(lurePoss),length(lurePoss)));

    [~, whereMatches] = findMatches(idx, nback);
    whereMatches = find(whereMatches);
    whereMatches = [whereMatches, whereMatches-nback]; 
    
    nPasses = 0; ixx = 0; nLuresReal = findLures(idx, nback);
    while nLuresReal~=nLures && ixx<length(lurePossShuffled) && nPasses < nPassLimit
        nPasses = nPasses + 1;
        if nLuresReal < nLures
            % Iterate:
            ixx = ixx + 1;
            lureInd = lurePossShuffled(ixx);
            
            % Check if viable index:
            if idx(lureInd)~=0; continue; end
            
            % Choose Lure Type (1 or 2):
            onlyOneOption = false;
            if lureInd-nback-1 <= 0 || any(whereMatches == lureInd-nback-1)
                lureType = 2; onlyOneOption = true;
                if lureInd-nback+1<=0
                    continue; % can't add this lure at all 
                end
            elseif lureInd-nback+1 <= 0 || any(whereMatches == lureInd-nback+1)
                continue;
            else
                lureType = randi(2);
            end
            
            % Find stimuli to avoid at this index:
            toAvoid = whichToAvoid(lureInd, lureType, idx, nback);
            indOpts = setdiff(1:nStimuli, toAvoid);
            useStimulus = indOpts(randi(length(indOpts),1));
            
            if (lureType==1) 
                if (idx(lureInd-nback-1)==0) 
                    idx(lureInd) = useStimulus;
                    idx(lureInd-nback-1) = useStimulus;
                elseif (idx(lureInd-nback-1)~=idx(lureInd-nback)) && (lureInd+nback>nTrials || idx(lureInd-nback-1)~=idx(lureInd+nback)) % if -nBack-1 already has a lure, just use that stimulus for new lure:
                    idx(lureInd) = idx(lureInd-nback-1);
                elseif onlyOneOption
                    continue;
                else  % try adding -nBack+1 instead
                    toAvoid = whichToAvoid(lureInd, 2, idx, nback); 
                    indOpts = setdiff(1:nStimuli, toAvoid);
                    useStimulus = indOpts(randi(length(indOpts),1));
                    if (idx(lureInd-nback+1)==0) 
                        idx(lureInd) = useStimulus;
                        idx(lureInd - nback + 1) = useStimulus;
                    elseif ((lureInd-nback<=0 || idx(lureInd - nback + 1) ~= idx(lureInd - nback)) && (lureInd+nback>nTrials || idx(lureInd - nback + 1) ~= idx(lureInd + nback))) 
                            idx(lureInd) = idx(lureInd - nback + 1);
                    else 
                        continue;
                    end
                end
            elseif (lureType==2) 
                if (idx(lureInd-nback+1)==0) 
                    idx(lureInd) = useStimulus;
                    idx(lureInd - nback + 1) = useStimulus;
                elseif ((lureInd-nback<=0 || idx(lureInd - nback + 1) ~= idx(lureInd - nback)) && (lureInd+nback>nTrials || idx(lureInd - nback + 1) ~= idx(lureInd + nback)))
                    idx(lureInd) = idx(lureInd - nback + 1);
                elseif (onlyOneOption) 
                    continue;
                else  % Try adding -nBack-1 instead:
                    toAvoid = whichToAvoid(lureInd, 1, idx, nback); 
                    indOpts = setdiff(1:nStimuli, toAvoid);
                    useStimulus = indOpts(randi(length(indOpts),1));
                    if (idx(lureInd-nback-1)==0) 
                        idx(lureInd) = useStimulus;
                        idx(lureInd - nback - 1) = useStimulus;
                    elseif ((lureInd-nback-1<=0 || idx(lureInd - nback - 1) ~= idx(lureInd-nback)) && (lureInd+nback>nTrials || idx(lureInd - nback - 1) ~= idx(lureInd + nback))) 
                        idx(lureInd) = idx(lureInd - nback - 1);
                    else 
                        continue;
                    end
                end
            end
            
            % Count up the # of lures (new lures can be created unexpectedly):
            nLuresReal = findLures(idx, nback);
        
        elseif nLuresReal > nLures
            opts = lurePossShuffled(1:ixx);
            if ~isempty(opts)
                idx(opts(randi(length(opts),1))) = 0;
            end
            
            % Count up the # of lures (new lures can be created unexpectedly):
            nLuresReal = findLures(idx, nback);
        end
    end
    
    % If still too few lures, add more wherever gaps remain:
    remainingPossible = find(idx==0); nPasses = 0;
    while nLuresReal~=nLures && ~isempty(remainingPossible) && nPasses<nPassLimit 
        nPasses = nPasses + 1;
        if nLuresReal < nLures
            for ixx = remainingPossible
                possible = [];
                if (ixx+nback-1 <= nTrials) 
                    if ((ixx+nback>nTrials || idx(ixx+nback-1)~=idx(ixx+nback)) && (ixx-nback<=0 || idx(ixx+nback-1)~=idx(ixx-nback))) 
                        possible = [possible, idx(ixx+nback-1)]; %#ok
                    end
                    if (ixx+nback+1 <= nTrials) 
                        if (idx(ixx+nback+1)~=idx(ixx+nback) && (ixx-nback<=0 || idx(ixx+nback+1)~=idx(ixx-nback))) 
                            possible = [possible, idx(ixx+nback+1)]; %#ok
                        end
                    end
                end
                if (ixx-nback+1 > 0) 
                    if ((ixx-nback<=0 || idx(ixx-nback+1)~=idx(ixx-nback)) && (ixx+nback>nTrials || idx(ixx-nback+1)~=idx(ixx+nback))) 
                        possible = [possible, idx(ixx-nback+1)]; %#ok
                    end
                    if (ixx-nback-1 > 0) 
                        if (idx(ixx-nback-1)~=idx(ixx-nback) && (ixx+nback>nTrials || idx(ixx-nback-1)~=idx(ixx+nback))) 
                            possible = [possible, idx(ixx-nback-1)]; %#ok
                        end
                    end
                end
                possible(possible==0) = []; %#ok
                if ~isempty(possible)
                    idx(ixx) = possible(randi(length(possible),1));
                    nLuresReal = findLures(idx, nback);
                    remainingPossible = find(idx==0);
                    if (nLuresReal >= nLures) 
                        break;
                    end
                else % if is empty, cannot add lure
                    remainingPossible(remainingPossible==ixx) = [];
                end
            end
        elseif nLuresReal > nLures
            opts = lurePossShuffled(1:min(ixx,length(lurePossShuffled)));
            if ~isempty(opts)
                idx(opts(randi(length(opts),1))) = 0;
            end
            
            % Count up the # of lures (new lures can be created unexpectedly):
            nLuresReal = findLures(idx, nback);
            remainingPossible = find(idx==0);
        end
    end
    
    %% 3. Add non-lures & non-matches:
    for ixx = find(idx==0)
        toAvoid = [];
        if (ixx-nback+1 > 0) 
            if (idx(ixx-nback+1) ~= 0) 
                toAvoid = [toAvoid, idx(ixx-nback+1)]; %#ok
            end
            if (ixx-nback > 0) 
                if (idx(ixx-nback) ~= 0) 
                    toAvoid = [toAvoid, idx(ixx-nback)]; %#ok
                end
                if (ixx-nback-1 > 0) 
                    if (idx(ixx-nback-1) ~= 0) 
                        toAvoid = [toAvoid, idx(ixx-nback-1)]; %#ok
                    end
                end
            end
        end
        if (ixx+nback-1 <= nTrials) 
            if (idx(ixx+nback-1) ~= 0) 
                toAvoid = [toAvoid, idx(ixx+nback-1)]; %#ok
            end
            if (ixx+nback <= nTrials) 
                if (idx(ixx+nback) ~= 0) 
                    toAvoid = [toAvoid, idx(ixx+nback)]; %#ok
                end
                if (ixx+nback+1 <= nTrials) 
                    if (idx(ixx+nback+1) ~= 0) 
                        toAvoid = [toAvoid, idx(ixx+nback+1)]; %#ok
                    end
                end
            end
        end
        indOpts = setdiff(1:nStimuli, toAvoid);
        idx(ixx) = indOpts(randi(length(indOpts),1));
    end
    
    %% Check final counts of matches and lures:
    [countMatches, matches] = findMatches(idx, nback);
    if countMatches~=nMatches; 
        warning(['# matches, target : ',num2str(countMatches),', ',num2str(nMatches)]); 
        wasSuccess = false;
    end
    [countLures, lures] = findLures(idx, nback);
    if countLures~=nLures && ~isempty(remainingPossible) %&& nPasses < nPassLimit
        warning(['# lures, target : ',num2str(countLures),', ',num2str(nLures)]); 
        wasSuccess = false;
        return; 
    end
    
    %% Helper Functions:
    function [nMatches, matches] = findMatches(vec, nback)
        matches = false(size(vec)); nMatches = 0;
        for ixxx = 1:length(vec)
           if vec(ixxx)>0 && ixxx>nback && vec(ixxx)==vec(ixxx-nback)
               matches(ixxx) = true;
               nMatches = nMatches + 1;
           end
        end
    end

    function [nLures, lures] = findLures(vec,nback)
        nLures = 0; lures = false(size(vec));
        for ixxx = 1:length(vec)
            if vec(ixxx)~=0 
                if (ixxx-nback<=0 || vec(ixxx)~=vec(ixxx-nback)) % assure not a match
                    if (ixxx-nback+1>0 && vec(ixxx)==vec(ixxx-nback+1))
                        nLures = nLures+1;
                        lures(ixxx) = true;
                        continue;
                    end
                    if ixxx-nback-1>0 && vec(ixxx)==vec(ixxx-nback-1)
                        nLures = nLures+1;
                        lures(ixxx) = true;
                        continue;
                    end
                end
            end
        end
    end

    function avoid = whichToAvoid(lureInd, whichLureType, idx, nBackNum)
        avoid = [];
        if (lureInd+nBackNum <= length(idx)) && idx(lureInd+nBackNum)~=0 % Avoid new match forward
            avoid = [avoid, idx(lureInd+nBackNum)];
        end
        if (lureInd-nBackNum>0) && idx(lureInd-nBackNum)~=0  % Avoid new match backward
            avoid = [avoid, idx(lureInd-nBackNum)];
        end
        if (whichLureType==1) 
            if (lureInd-nBackNum-1-nBackNum>0) && idx(lureInd-nBackNum-1-nBackNum)~=0  % Avoid creating new match
                avoid = [avoid, idx(lureInd-nBackNum-1-nBackNum)];
            end
            if (idx(lureInd-nBackNum-1+nBackNum) ~= 0) % Avoid creating new match
                avoid = [avoid, idx(lureInd-nBackNum-1+nBackNum)]; 
            end
        elseif (whichLureType==2) 
            if (lureInd-nBackNum-nBackNum+1 > 0) && idx(lureInd-nBackNum-nBackNum+1)~=0  % Avoid creating new match
                avoid = [avoid, idx(lureInd-nBackNum-nBackNum+1)];
            end
            if (lureInd+1<=length(idx)) && idx(lureInd+1)~=0  % Avoid creating new match
                avoid = [avoid, idx(lureInd+1)];
            end
        end
    end

    function vec = clearNonMatches(vec, nback)
        for ixxx = 1:length(vec)
           if vec(ixxx)>0 && ~((ixxx>nback && vec(ixxx)==vec(ixxx-nback)) || (ixxx+nback<=length(vec) && vec(ixxx)==(vec(ixxx+nback))))
               vec(ixxx) = 0;
           end
        end
    end
end

end