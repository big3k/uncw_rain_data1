% YDT 20230723: 
% This function does "uniq -c" 
% Input data: 
%  entities: data
%  types:    vector 


function uniq_c(data) 

[C,ia,ic] = unique(data);  
a_counts = accumarray(ic,1);
value = C; 
counts=a_counts; 
[value, counts]

end






