
function upscale_subset(case_loc, casef)

% 8/16/2024 
%  Upscale 0.01deg to 0.1-deg by averaging the 10x10 box 
%  Then if CONUS has any overlap with the (lon1, lon2, lat1, lat2) area, 
%  fill the CONUS values in the overlap, and save the (lon1, lon2, lat1, lat2) area
%  as the subset, together with the number of overlapped 0.1-deg pixels. 
%  
% Algorithm note: project both the CONUS and the (lon1, lon2, lat1, lat2) area onto 
%  a global 0.1-deg grid, then check overlap. 
%
% Input: 
%  case_loc: dir of the case file, to save the "mrms.mat" to 
%  casef: case file, i.e., "sate_select.mat" 
% Output
%  if any match, "mrms.mat" to the case_loc 

% overlap threshold: only the case overlap with MRMS with sufficient number of pixels
overlap_thresh=10000; 

% get the case's space/time
[lon1, lon2, lat1, lat2, yyyy, mm, dd, hr, mn]=get_time_boundary_of_case(casef);

fprintf("lon1=%8.2f lon2=%8.2f lat1=%8.2f lat2=%8.2f %d %d %d %d %d\n", lon1, lon2, lat1, lat2, yyyy, mm, dd, hr, mn)

% check if overlap with CONUS
% ---------------------------------------
% 0.1-deg grid, global, (1, 1) corrensponding to (lon=-180, lat=-90) 
gnr=3600
gnc=1800
glon0=-180.0
glat0=-90.0 

% to create masks 
global_case=zeros(gnr, gnc); 
global_mrms=zeros(gnr, gnc); 

nx1=round((lon1-glon0)/0.1)+1;
nx2=round((lon2-glon0)/0.1)+1;
ny1=round((lat1-glat0)/0.1)+1;
ny2=round((lat2-glat0)/0.1)+1;

% mask out case on the globe
global_case(nx1:nx2, ny1:ny2)=1; 

% mrms region on the globe 
mlon1=-130 
mlon2=-60.1 
mlat1=20 
mlat2=54.9
mx1=round((mlon1-glon0)/0.1)+1;
mx2=round((mlon2-glon0)/0.1)+1;
my1=round((mlat1-glat0)/0.1)+1;
my2=round((mlat2-glat0)/0.1)+1;
global_mrms(mx1:mx2, my1:my2)=1; 

mask=global_case.*global_mrms; 

n_overlap=sum(mask, 'all'); %global mask of overlap grids

if n_overlap < overlap_thresh
  fprintf("Too few overlap pixels: n_overlap=%d. Skip case\n", n_overlap); 
  return % do nothing 
end 

fprintf("Found overlap pixels: n_overlap=%d. Process case\n", n_overlap); 

% condtinue to fill the overlap area with mrms values and carve out the case region 
snr=700;
snc=350;

% ---------------------------------------
% if yes, then 

mn=round(mn/2)*2 % round to nearest 2-min
% construct MRMS file name
mrmsf=sprintf("/data1/tiany/MRMS/CONUS/PrecipRate_00.00/%4d%2.2d%2.2d/MRMS_PrecipRate_00.00_%4d%2.2d%2.2d-%2.2d%2.2d00.grib2.gz", yyyy, mm, dd, yyyy, mm, dd, hr, mn)

% if current filing is missing, look back (if mn is in 2nd half of the hour)
%  or forward (if in first half of the hour) 2 more minutes. 
good=0 % assumging file missing or bad unless proven 
if exist(mrmsf, 'file')  == 2  % file exist 
   finfo=dir(mrmsf);
   fsize=finfo.bytes;
   if fsize > 10000
      good=1
   end 
end 

if good == 0 
   fprintf("MRMS file missing or bad: %s\n", mrmsf) 
   if mn >= 30  % look back to avoid complex date/time computation 
     mn=mn-2 
   else 
     mn=mn+2 
   end 
   % new file to try 
   mrmsf=sprintf("/data1/tiany/MRMS/CONUS/PrecipRate_00.00/%4d%2.2d%2.2d/MRMS_PrecipRate_00.00_%4d%2.2d%2.2d-%2.2d%2.2d00.grib2.gz", yyyy, mm, dd, yyyy, mm, dd, hr, mn)
   if exist(mrmsf, 'file')  == 2 % backup file exists 
        finfo=dir(mrmsf);
        fsize=finfo.bytes;
        if fsize > 10000
          good=1
        end
   end
end

if good == 0 % still missing or bad, give up
     fprintf("Can't find alternative MRMS file: %s. Skip the case\n", mrmsf) 
     return 
else
     fprintf("Using good MRMS file: %s. \n", mrmsf) 
 
end

% file exists and big enough.

% temp unzip and de-grib'd filename
gribf=sprintf("tmp/%4d%2.2d%2.2d-%2.2d%2.2d00.grib2", yyyy, mm, dd, hr, mn)
binf=sprintf("tmp/%4d%2.2d%2.2d-%2.2d%2.2d00.bin", yyyy, mm, dd, hr, mn)
system("gunzip -c " + mrmsf + " > " + gribf); 
system("wgrib2 " +  gribf + " -no_header -bin " + binf)

% 0.01-deg grid
nr=7000;
nc=3500;

fp=fopen(binf,'rb');
rain=fread(fp,[nr,nc],'float32');  %  rain rate
fclose(fp);

% rain(rain==-3)=NaN; % -3 is undef 

orain = zeros(snr, snc); 
nvalid = zeros(snr, snc); 

for si=1:snr
  for sj=1:snc
     nvalid(si, sj)=0;  % non-undef values 
     for i=1:10 
       ix=(si-1)*10+i;
       for j=1:10
         iy=(sj-1)*10+j; 
         
         if rain(ix, iy) >= 0  
           nvalid(si, sj)=nvalid(si, sj)+1; 
           orain(si, sj)=orain(si, sj)+rain(ix, iy); 
         end 
       end % j
     end %i 
     if nvalid(si, sj) >= 50 % more than half are valid 0.01-deg pixels
        orain(si, sj) = orain(si, sj)/nvalid(si, sj); % average 
     else 
        orain(si, sj) = -3; % undef values 
     end  % if
   end % sj
end %si

% verification, debugging and plotting
%fp=fopen('test2.bin.subset','wb');
%fwrite(fp, orain(nx1:nx2, ny1:ny2),'float32');  %  rain rate
%fwrite(fp, nvalid(nx1:nx2, ny1:ny2),'float32');  % pixel count 
%fclose(fp);

% fill global with missing values: 
global_mrms(:, :)=-3; 
global_mrms(mx1:mx2, my1:my2)=orain; 

fprintf("mx1=%d, mx2=%d, my1=%d, my2=%d\n", mx1, mx2, my1, my2);  
fprintf("nx1=%d, nx2=%d, ny1=%d, ny2=%d\n", nx1, nx2, ny1, ny2);  

MRMS_PrecipRate=global_mrms(nx1:nx2, ny1:ny2); 

% verification, debugging and plotting
fp=fopen(binf + "_sub", 'wb');
fwrite(fp, MRMS_PrecipRate, 'float32');  %  rain rate
%fwrite(fp, nvalid(nx1:nx2, ny1:ny2),'float32');  % pixel count
fclose(fp);


% put to case directory 
save(case_loc + "/mrms.mat", "MRMS_PrecipRate", "n_overlap"); 

end 



