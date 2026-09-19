%seems & rr_src==7 (corresponds to MHS) has incrse trend

clc
clear
close all

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
        num2str(yr),'-ir.dat'];

    file_pt = [data_loc, ...
        'imerg-rainrate-upto500km-', ...
        num2str(yr),'-pt-ir.dat'];
      % 
      % file_src = [data_loc, ...
      %   'imerg-rainrate-upto500km-', ...
      %   num2str(yr),'-hq-src.dat'];

    %% -----------------------------------------------------
    % Skip year if files do not exist
    % ------------------------------------------------------
    if ~exist(file_rr,'file') || ...
       ~exist(file_pt,'file')

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
    
    % fid = fopen(file_src,'r');
    % rr_src = fread(fid,'float32');
    % fclose(fid);
    

    %% -----------------------------------------------------
    % Make sure arrays have same length
    % ------------------------------------------------------
    if length(rr) ~= length(rr_pt)

        error( ...
            'rr and rr_pt have different lengths in %d.', ...
            yr);

    end

    %% =====================================================
    % Conditional rain-rate criterion
    %
    % Zhong et al.:
    % use raining pixels > 0.1 mm h^-1
    % ======================================================
    ix_rain = rr > cv;

    rr    = rr(ix_rain);
    rr_pt = rr_pt(ix_rain);

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
exportgraphics(gcf,'imerg_ir-mean_rr.png','Resolution',120)


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
exportgraphics(gcf,'imerg_ir_pixels.png','Resolution',120)
