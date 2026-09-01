clc
clear
close all

%% =========================================================
% Settings
% ==========================================================
data_loc = ...
    '/data1/youy/tropics-hurricane/figure/IBTrACS/';

% file_name = 'IBTrACS.ALL.v04r00.nc';
file_name = 'IBTrACS.ALL.v04r01.nc';

FN = fullfile(data_loc,file_name);

save_loc = ...
    '/data1/tiany/do_hurricanes_strengthen_before_landfall/step0/'; 
%    '/data1/youy/project/trend-landfall/data/step0/';

%% =========================================================
% Read IBTrACS
% ==========================================================
lon       = ncread(FN,'lon');
lat       = ncread(FN,'lat');

basin     = ncread(FN,'basin');
name      = ncread(FN,'name');
sid       = ncread(FN,'sid');

landfall  = ncread(FN,'landfall');
dist2land = ncread(FN,'dist2land');

usa_wd    = ncread(FN,'usa_wind');
wmo_wd    = ncread(FN,'wmo_wind');

nature    = ncread(FN,'nature');
usa_sshs  = ncread(FN,'usa_sshs');

%% =========================================================
% Time
% ==========================================================
time = ncread(FN,'time');

date1 = datenum( ...
    '1858-11-17-00:00:00', ...
    'yyyy-mm-dd-HH:MM:SS');

date2 = time + date1;

%% =========================================================
% Initialize
% ==========================================================
jsq = 0;
num = 0;

nstorm = size(time,2);

%% =========================================================
% Loop through storms
% ==========================================================
for i = 1:nstorm

    %% -----------------------------------------------------
    % Basic storm information
    % ------------------------------------------------------
    date3 = date2(:,i);

    [yr,mon,dy,hr,minu,sd] = datevec(date3(1));

    if yr < 1998
        continue
    end

    name1 = name(:,i);
    A = deblank(name1');

    SID = deblank(sid(:,i)');

    fprintf('%d  %s  %s\n',i,A,SID);

    %% -----------------------------------------------------
    % Storm variables
    % ------------------------------------------------------
    LF = landfall(:,i);
    DL = dist2land(:,i);

    WS = wmo_wd(:,i);

    NAT = nature(:,:,i);

    scale1 = usa_sshs(:,i);

    %% -----------------------------------------------------
    % Intensity criterion
    %
    % -1 = tropical storm
    %  0 = category 0
    %  1-5 = hurricane categories
    % ------------------------------------------------------
    ix_scale = ...
        scale1 >= -1 & ...
        scale1 <= 5;

    %% -----------------------------------------------------
    % Lifetime maximum wind criterion
    % >= 35 kt
    % ------------------------------------------------------
    max_ws = max(WS,[],'omitnan');

    if isempty(max_ws) || isnan(max_ws) || max_ws < 35
        continue
    end

    %% -----------------------------------------------------
    % Tropical-system criterion
    %
    % IBTrACS nature:
    % TS = Tropical System
    % ET = Extratropical System
    % SS = Subtropical System
    % ------------------------------------------------------
    ix_tropical = ...
        NAT(1,:)' == 'T' & ...
        NAT(2,:)' == 'S';

    %% -----------------------------------------------------
    % Candidate landfall points
    %
    % Zhong et al. criterion:
    % dist2land == 0
    % landfall  == 0
    %
    % Also require:
    % valid intensity category
    % tropical system
    % ------------------------------------------------------
    ix_landfall = ...
        DL == 0 & ...
        LF == 0 & ...
        ix_scale & ...
        ix_tropical;

    %% -----------------------------------------------------
    % Does storm have a valid landfall?
    % ------------------------------------------------------
    J = find(ix_landfall);

    if isempty(J)
        continue
    end

    %% -----------------------------------------------------
    % Use first qualifying landfall
    % ------------------------------------------------------
    J1 = J(1);

    start_time     = date3(1);
    start_landfall = date3(J1);

    %% -----------------------------------------------------
    % Require >60 hours before landfall
    % ------------------------------------------------------
    time_diff_hr = ...
        (start_landfall - start_time)*24;

    if time_diff_hr <= 60
        continue
    end

    %% =====================================================
    % Save selected storm
    % ======================================================
    hur.name = A;
    hur.sid  = SID;

    hur.lon  = lon(1:J1,i);
    hur.lat  = lat(1:J1,i);
    hur.time = date3(1:J1);

    hur.usa_wd = usa_wd(1:J1,i);
    hur.wmo_wd = wmo_wd(1:J1,i);

    hur.scale = ...
        usa_sshs(1:J1,i);

    hur.region = ...
        basin(:,1:J1,i);

    hur.nature = ...
        nature(:,1:J1,i);

    hur.location_in_ibtracs = i;

    hur.landfall_index = J1;
    hur.landfall_time  = start_landfall;

    hur.max_wind = max_ws;
    hur.pre_landfall_hours = time_diff_hr;

    %% -----------------------------------------------------
    % Create year folder
    % ------------------------------------------------------
    save_loc1 = ...
        fullfile(save_loc,num2str(yr));

    if ~exist(save_loc1,'dir')
        mkdir(save_loc1);
    end

    %% -----------------------------------------------------
    % Unique filename:
    % StormName-IBTrACSSID.mat
    % ------------------------------------------------------
    FN_save = ...
        fullfile(save_loc1,[A,'-',SID,'.mat']);

    %% -----------------------------------------------------
    % Warn if file somehow already exists
    % ------------------------------------------------------
    if exist(FN_save,'file')
        fprintf( ...
            'WARNING: file already exists: %s\n', ...
            FN_save);
    end

    %% -----------------------------------------------------
    % Save
    % ------------------------------------------------------
    save(FN_save,'hur');

    %% -----------------------------------------------------
    % Counters
    % ------------------------------------------------------
    jsq = jsq + 1;

    num = num + length(hur.lon);

    %% -----------------------------------------------------
    % Print selected storm
    % ------------------------------------------------------
    fprintf( ...
        ['SELECTED: %4d  %-15s  %-15s  ', ...
         'Landfall = %s  Max wind = %.0f kt  ', ...
         'Pre-LF = %.1f h\n'], ...
        jsq, ...
        A, ...
        SID, ...
        datestr(start_landfall,'yyyy-mm-dd HH:MM'), ...
        max_ws, ...
        time_diff_hr);

    clear hur

end

%% =========================================================
% Summary
% ==========================================================
fprintf('\n');
fprintf('=============================================\n');
fprintf('Total selected storms = %d\n',jsq);
fprintf('Total track points    = %d\n',num);
fprintf('=============================================\n');

%% =========================================================
% Count files actually written
% ==========================================================
files_saved = dir( ...
    fullfile(save_loc,'**','*.mat'));

fprintf('Total .mat files saved = %d\n', ...
    length(files_saved));

fprintf('Difference = %d\n', ...
    jsq - length(files_saved));

fprintf('=============================================\n');
