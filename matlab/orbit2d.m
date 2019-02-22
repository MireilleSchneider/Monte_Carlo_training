%% ---------------------------------
%% PURPOSE: TO PLOT THE EQUILIBRIUM
%% ---------------------------------

%% INPUT EQUILIBRUIM
load ../input/iter.mat
try
  load('../output/ion_trajectory.txt');
catch
  disp('---------------------------------------')
  disp('You have not run the code with')
  disp('one_orbit=1 and orbit_following=1')
  disp('=> No ion trajectory has been saved')
  disp('=> Aborted.')
  disp('---------------------------------------')
  break
end
  
%% SECURITY
if length(ion_trajectory) < 1
  disp('---------------------------------------')
  disp('You have not run the code with')
  disp('one_orbit=1 and orbit_following=1')
  disp('=> No ion trajectory has been saved')
  disp('=> Aborted.')
  disp('---------------------------------------')
  break
end

%% ION TRAJECTORY
rpa_orb = ion_trajectory(:,1);
zpa_orb = ion_trajectory(:,2);

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
plot(rpa_orb,zpa_orb,'r','linewidth',linewidth-1)
plot(rpa_orb(1),zpa_orb(1),'*g')
plot(rpa_orb(end),zpa_orb(end),'.k')

print -dpng ../fig/orbit.png
print -depsc ../fig/orbit.eps


