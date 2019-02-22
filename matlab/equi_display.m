%% ---------------------------------
%% PURPOSE: TO PLOT THE EQUILIBRIUM
%% ---------------------------------

%% INPUT EQUILIBRUIM
load ../input/iter.mat

%% DISPLAY OPTIONS
markersize = 20;
fontsize = 15;
linewidth = 3;
fweight='bold';

%% PLOT THE FIGURE
clf
set(gcf,'Color',[1 1 1]);
h=axes;
set(h,'FontSize',fontsize,'fontweight',fweight)
hold on ; grid on
contour(r2d,z2d,rho2d,50)
contour(r2d,z2d,psi2d,30,'b','linewidth',1)
axis equal
xlabel('R (m)','FontSize',fontsize,'FontWeight',fweight)
ylabel('Z (m)','FontSize',fontsize,'FontWeight',fweight)
set(gca,'xlim',[4. 8.5])
set(gca,'ylim',[-3.2 4.2])

print -dpng ../fig/equi.png
print -depsc ../fig/equi.eps


