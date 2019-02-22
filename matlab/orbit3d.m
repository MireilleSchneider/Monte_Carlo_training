%% ---------------------------------
%% PURPOSE: TO PLOT THE EQUILIBRIUM
%% ---------------------------------

%% ------------------
%% INPUT EQUILIBRUIM
%% ------------------
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
  
%% ---------
%% SECURITY
%% ---------
if length(ion_trajectory) < 1
  disp('---------------------------------------')
  disp('You have not run the code with')
  disp('one_orbit=1 and orbit_following=1')
  disp('=> No ion trajectory has been saved')
  disp('=> Aborted.')
  disp('---------------------------------------')
  break
end

%% ---------------
%% ION TRAJECTORY
%% ---------------
rpa_orb = ion_trajectory(:,1);
zpa_orb = ion_trajectory(:,2);
fpa_orb = ion_trajectory(:,3);

if ~exist('fpa_shift')
  fpa_shift = 0.;
end
if ~exist('nfinal')
  nfinal = 1000;
end
if nfinal>length(rpa_orb)
  nfinal = length(rpa_orb);
end
disp('---------------------------------------------------------')
disp('You may want to modify fpa_shift to rotate the trajectory')
disp(['Right now: fpa_shift = ',num2str(fpa_shift),' rad'])
disp('---------------------------------------------------------')
disp('You may want to modify nfinal, i.e. the number of points')
disp('to take into account for the orbit display')
disp(['Right now: nfinal = ',num2str(nfinal)])
disp('---------------------------------------------------------')


%% ---------------------
%% ION TRAJECTORY (BIS)
%% ---------------------
fpa_orb = fpa_orb+fpa_shift;
xpa_orb = rpa_orb.*cos(fpa_orb);
ypa_orb = rpa_orb.*sin(fpa_orb);

%% ----------------
%% DISPLAY OPTIONS
%% ----------------
markersize = 20;
fontsize = 15;
linewidth = 3;
fweight='bold';
if ~exist('ialpha')
  ialpha  = 0;
end

%% ---------------
%% TORUS EQUATION
%% ---------------

rsurf = squeeze(r2dpt(end,:));
zsurf = squeeze(z2dpt(end,:));

ntheta = 50;
theta  = pi * (0:2:2*ntheta)/ntheta ;

x_torus = double(rsurf' * cos(theta));
y_torus = double(rsurf' * sin(theta));
z_torus = double(zsurf' * ones(size(theta)));

%% PLOT THE FIGURE
clf
set(gcf,'Color',[1 1 1]);
h=axes;
set(h,'FontSize',fontsize,'fontweight',fweight)
hold on ; grid on

%% DISPLAY THE TORUS
lim2 = size(x_torus,2);
interval1 = 1:30;
interval2 = 40:lim2;
surf(x_torus(:,interval1),y_torus(:,interval1),z_torus(:,interval1),'facecolor','none','edgecolor','g')
surf(x_torus(:,interval2),y_torus(:,interval2),z_torus(:,interval2),'facecolor','none','edgecolor','g')
if ialpha==1
  alpha(0.4);
end
plot3(xpa_orb(1:nfinal),ypa_orb(1:nfinal),zpa_orb(1:nfinal),'r','linewidth',linewidth-1)
plot3(xpa_orb(1),ypa_orb(1),zpa_orb(1),'*g')
plot3(xpa_orb(end),ypa_orb(end),zpa_orb(end),'.k')
view([-79 34])
axis equal
xlabel('X (m)','FontSize',fontsize,'FontWeight',fweight)
ylabel('Y (m)','FontSize',fontsize,'FontWeight',fweight)
zlabel('Z (m)','FontSize',fontsize,'FontWeight',fweight)
print -dpng ../fig/orbit3d.png
print -depsc ../fig/orbit3d.eps


