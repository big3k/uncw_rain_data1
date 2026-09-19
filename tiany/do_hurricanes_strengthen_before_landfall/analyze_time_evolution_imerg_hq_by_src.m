%seems & rr_src==7 (corresponds to MHS) has incrse trend

function analyze_time_evolution_imerg_hq_by_src(src_name) 

if nargin == 0
    src_name = 'ALL';
end

switch upper(src_name)

    case 'ALL'
        src_code = [];

    case 'TMI'
        src_code = 1;

    case 'AMSR2'
        src_code = 3;

    case 'SSMI'
        src_code = 4;

    case 'SSMIS'
        src_code = 5;

    case 'MHS'
        src_code = 7;

    case 'GMI'
        src_code = 9;

    case 'SSMIS'
        src_code = 5;

    case 'ATMS'
        src_code = 11;

    case 'SAPHIR'
        src_code = 20;

    otherwise
        error('Unknown source: %s', src_name) 

 end % switch 

%% =========================================================
% Settings
% ==========================================================
YR = 1998:2019;

%YDT data_loc = '/data1/tiany/do_hurricanes_strengthen_before_landfall/step2-data-3b42/'; 
%data_loc = '/data1/tiany/do_hurricanes_strengthen_before_landfall/step2-data-mswep/'; 
data_loc = '/data1/tiany/do_hurricanes_strengthen_before_landfall/step2-data-imerg/'; 

% Zhong et al. conditional rain-rate threshold
cv = 0.1;

% Exact hours relative to landfall
% -60, -57, ..., -3, 0
pt_intv = -60:3:0;

nyr = length(YR);
npt = length(pt_intv);

%% =========================================================
% Initialize
% ==========================================================
sum_rr = zeros(npt,nyr);
num_rr = zeros(npt,nyr);

%% =========================================================
% Loop through years
% ==========================================================
for i = 1:nyr

    yr = YR(i);

    fprintf('%d of %d   Year = %d\n', ...
        i,nyr,yr);

    %% -----------------------------------------------------
    % File names
    % ------------------------------------------------------
    file_rr = [data_loc, ...
        'imerg-rainrate-upto500km-', ...
        num2str(yr),'-hq.dat'];

    file_pt = [data_loc, ...
        'imerg-rainrate-upto500km-', ...
        num2str(yr),'-pt-hq.dat'];
       
    file_src = [data_loc, ...
         'imerg-rainrate-upto500km-', ...
         num2str(yr),'-src-hq.dat'];

    %% -----------------------------------------------------
    % Skip year if files do not exist
    % ------------------------------------------------------
    if ~exist(file_rr,'file') || ...
       ~exist(file_pt,'file') || ~exist(file_src,'file')

        fprintf('Missing files for %d\n',yr);
        continue
    end

    %% -----------------------------------------------------
    % Read rain rate
    % ------------------------------------------------------
    fid = fopen(file_rr,'r');
    rr = fread(fid,'float32');
    fclose(fid);

    %% -----------------------------------------------------
    % Read hours relative to landfall
    % ------------------------------------------------------
    fid = fopen(file_pt,'r');
    rr_pt = fread(fid,'float32');
    fclose(fid);
    
    %% -----------------------------------------------------
    % Read "MWprecipSource"
    % ------------------------------------------------------
    fid = fopen(file_src,'r');
    rr_src = fread(fid,'float32');
    fclose(fid);
    

    %% -----------------------------------------------------
    % Make sure arrays have same length
    % ------------------------------------------------------
    if length(rr) ~= length(rr_pt)

        error( ...
            'rr and rr_pt have different lengths in %d.', ...
            yr);

    end

     if length(rr) ~= length(rr_src)

        error( ...
            'rr and rr_src have different lengths in %d.', ...
            yr);

    end

    %% =====================================================
    % Conditional rain-rate criterion
    %
    % Zhong et al.:
    % use raining pixels > 0.1 mm h^-1
    % ======================================================

    ix_keep = rr > cv;

    if ~isempty(src_code)
       ix_keep = ix_keep & (rr_src == src_code);
    end

    rr     = rr(ix_keep);
    rr_pt  = rr_pt(ix_keep);
    rr_src = rr_src(ix_keep);

    %% =====================================================
    % Calculate statistics at exact 3-hourly times
    %
    % -60, -57, -54, ..., -3, 0
    % ======================================================
    for k = 1:npt

        ix = rr_pt == pt_intv(k);

        if any(ix)

            sum_rr(k,i) = ...
                sum(rr(ix),'omitnan');

            num_rr(k,i) = ...
                sum(ix);

        end

    end

end

%% =========================================================
% Combine all years
% ==========================================================
% Total rain-rate sum at each time relative to landfall
a = sum(sum_rr,2);

% Total number of raining pixels at each time
b = sum(num_rr,2);

% Mean conditional rain rate
c = a ./ b;

% Avoid divide-by-zero
c(b == 0) = NaN;

%% =========================================================
% Print results
% ==========================================================
fprintf('\n');
fprintf('Hours before landfall     Mean rain rate     N pixels\n');
fprintf('-------------------------------------------------------\n');

for k = 1:npt

    fprintf('%6.0f h                 %8.4f          %d\n', ...
        pt_intv(k), ...
        c(k), ...
        b(k));

end

%% =========================================================
% Plot Zhong-style curve
% ==========================================================
figure( ...
    'Color','w', ...
    'Position',[100 100 850 600]);

plot( ...
    pt_intv, ...
    c, ...
    '-ok', ...
    'LineWidth',1.5, ...
    'MarkerSize',5, ...
    'MarkerFaceColor','k');

xlabel( ...
    'Hours before landfall', ...
    'FontSize',14);

ylabel( ...
    'Mean conditional TC rain rate (mm h^{-1})', ...
    'FontSize',14);

xlim([-60 0]);
ylim([1.5 2]);

xticks(-60:12:0);

set(gca, ...
    'FontSize',13, ...
    'LineWidth',1.0, ...
    'TickDir','out', ...
    'Box','on');

grid on
set(gca,'GridAlpha',0.10);
png1 = sprintf('imerg_hq_mean_rr_%s.png', lower(src_name));
png2 = sprintf('imerg_hq_pixels_%s.png', lower(src_name));

exportgraphics(gcf,png1,'Resolution',120)


%stop

%% =========================================================
% Optional: sample-size figure
% ==========================================================
figure( ...
    'Color','w', ...
    'Position',[100 100 850 600]);

plot( ...
    pt_intv, ...
    b, ...
    '-ok', ...
    'LineWidth',1.5, ...
    'MarkerSize',5, ...
    'MarkerFaceColor','k');

xlabel( ...
    'Hours before landfall', ...
    'FontSize',14);

ylabel( ...
    'Number of raining pixels', ...
    'FontSize',14);

xlim([-60 0]);

xticks(-60:12:0);

set(gca, ...
    'FontSize',13, ...
    'LineWidth',1.0, ...
    'TickDir','out', ...
    'Box','on');

grid on
set(gca,'GridAlpha',0.10);
exportgraphics(gcf,png2,'Resolution',120)

end  % function 
