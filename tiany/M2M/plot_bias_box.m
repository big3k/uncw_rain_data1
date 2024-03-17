% YDT 20230806
% function to plot y-x vs. x, assuming x, y are all rain rates (mm/hr) 

function plot_bias_box(x, y, title) 

sp1=-3:0.1:5;
edges=exp(sp1); 
xbin=discretize(x, edges); 

xvals=edges(:, xbin); 

boxplot(y-x, xvals)

Xs=[0.1, 0.2,0.5,1,2,4,8,16,32,64,128];
xticks(Xs)
xticklabels({'0.1', '0.2', '0.5', '1', '2', '4', '8', '16', '32', '64', '128'})
xlabel('precip. rate (mm/h)', 'FontSize',14);
ylabel('bias (mm/h)','FontSize',14);

text(0.02, 0.96, title, 'FontSize',18,'units','normalized');

end


