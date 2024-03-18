% YDT 20230730: 
% Cdf-matching to produce a lookup table that maps the input src values to new values 
% so the new values' PDF/CDF matches the dst's. 
% Input data: 
%  entities: src, dst
%  types:    vector, vector
% 
% Output: a look up table for src -> dst mapping 
% Assuming all data are log(rain rates (mm/h)). 

%Round data to 0.01 precision for cdf computation 
function cdf_map = cdf_matching(src, dst) 

% round up data to avoid too many bins, plus the max value b/c of the round down 
r_src=[round(src*100)/100; max(src)]; 
r_dst=[round(dst*100)/100; max(dst)]; 

[psrc, xsrc]=ecdf(r_src); 
[pdst, xdst]=ecdf(r_dst); 

% change first rain value b/c 1st and 2nd row of xdst have same value from ecdf
xsrc(1, :)=log(0.1);  % set to minimal rain rate (0.1 mm/h) 
xdst(1, :)=log(0.1);  % set to minimal rain rate (0.1 mm/h) 

% Given any cdf value pdst, find the xdst value: 
xsrc2=interp1(pdst, xdst, psrc); %  (xsrc -> xsrc2)  becomes the sorted lookup table 

cdf_map=table(xsrc, xsrc2, 'VariableNames',{'from','to'}); 

% Test: 
%src2=interp1(cdf_map.from, cdf_map.to, xsrc2, src);

end






