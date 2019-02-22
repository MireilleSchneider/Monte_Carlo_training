load('../output/rpa_in.txt')
load('../output/zpa_in.txt')
load('../output/fpa_in.txt')
load('../output/vel_in.txt')
load('../output/energy_in.txt')
load('../output/vpara_in.txt')
load('../output/hweight_in.txt')

load('../output/rpa.txt')
load('../output/zpa.txt')
load('../output/fpa.txt')
load('../output/vel.txt')
load('../output/energy.txt')
load('../output/vpara.txt')
load('../output/hweight.txt')
load('../output/iescape.txt')

if length(unique(zpa_in)) < 3
  disp('-----------------------------------------------')
  disp('Not proper distribution function as an input')
  disp('You have probably run with option one_orbit=1')
  disp('=> Aborted.')
  disp('-----------------------------------------------')
  break
end
  
rmin = 3.679506;
rmax = 9.839997;
zmin = -3.599022;
zmax = 4.823022;

%% SELECT ONLY PARTICLES THAT ARE STILL INSIDE THE PLASMA
sel = find(iescape==0);

%% FLAG TO PLOT THE FINAL STATE OF THE DISTRIBUTION (IN CASE OF ORBIT FOLLOWING)
if ~exist('ifinal')
  ifinal = 0;
end
disp('--------------------------------------------------------------')
disp('You may want to modify ifinal:')
disp('=> ifinal = 0 to plot the initial distribution only')
disp('=> ifinal = 1 to plot both the initial and final distributions')
disp(['Right now: ifinal = ',num2str(ifinal)])
disp('--------------------------------------------------------------')

%% DISPLAY OPTIONS
markersize = 10;
fontsize = 15;
linewidth = 3;
fweight='bold';

%% INPUT EQUILIBRUIM
load ../input/iter.mat

%% DISPLAY FIGURES
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
plot(rpa_in(sel),zpa_in(sel),'.r','markersize',markersize)
if ifinal == 1
  plot(rpa(sel),zpa(sel),'.g','markersize',markersize)
end
print -dpng ../fig/distrib2d.png
print -depsc ../fig/distrib2d.eps


