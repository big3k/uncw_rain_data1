
function upscale_subset(mrms_binf, nx1, nx2, ny1, ny2, mrms_subsetf) 

% 8/16/2024 
%  Upscale 0.01deg to 0.1-deg by averaging the 10x10 box 
% Input: 
%  mrms_binf, nx1, nx2, ny1, ny2: input 0.01-deg file and subset corners in the 0.1-deg grid
% Output
%  mrms_subsetf: 0.1-deg subset 

% load 0.01-deg data 

%mrms_binf='test2.bin'; 
% 0.01-deg grid
nr=7000;
nc=3500;

% 0.1-deg grid 
snr=700;
snc=350;

fp=fopen(mrms_binf,'rb');
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

MRMS_PrecipRate=orain(nx1:nx2, ny1:ny2); 
save(mrms_subsetf, "MRMS_PrecipRate"); 

end 



