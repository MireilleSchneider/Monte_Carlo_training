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
  disp('No proper distribution function as an input')
  disp('You have probably run with option one_orbit=1')
  disp('=> Aborted.')
  disp('-----------------------------------------------')
  break
end
  
rmin = 3.679506;
rmax = 9.839997;
zmin = -3.599022;
zmax = 4.823022;

%% PITCH ANGLE
pitch_in = vpara_in./vel_in;
pitch = vpara./vel;
pitch_in(isnan(pitch_in)) = 0.;
pitch(isnan(pitch)) = 0.;

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

%% NUMBER OF BINS FOR HISTORGRAMS
nbin = 100;

%% SELECT ONLY PARTICLES THAT ARE STILL INSIDE THE PLASMA
%% UNLESS ALL PARTICLES ARE ESCAPED (MEANING THAT THE SOURCE
%% IMPLEMENTATION HAS NOT BEEN DONE YET)
sel = find(iescape==0);
%if length(sel) == 0
%  sel = find(iescape);
%end
if length(sel) == 0
  disp('No particle left => Abort.')
  break
end

npart = size(sel);

%% PITCH ANGLE-COORDINATE
varminx = min(min(pitch(:)),min(pitch_in(:)));
varmaxx = max(max(pitch(:)),max(pitch_in(:)));
[histx_in,outx_in,dx_in]=histweight(npart,nbin,varminx,varmaxx,pitch_in(sel),hweight_in(sel));
[histx,outx,dx]=histweight(npart,nbin,varminx,varmaxx,pitch(sel),hweight(sel));

%% VELOCITY-COORDINATE
varminv = min(min(vel(:)),min(vel_in(:)));
varmaxv = max(max(vel(:)),max(vel_in(:)));
[histv_in,outv_in,dv_in]=histweight(npart,nbin,varminv,varmaxv,vel_in(sel),hweight_in(sel));
[histv,outv,dv]=histweight(npart,nbin,varminv,varmaxv,vel(sel),hweight(sel));

%% ENERGY-COORDINATE
varmine = min(min(energy(:)),min(energy_in(:)));
varmaxe = max(max(energy(:)),max(energy_in(:)));
[histe_in,oute_in,de_in]=histweight(npart,nbin,varmine,varmaxe,energy_in(sel),hweight_in(sel));
[histe,oute,de]=histweight(npart,nbin,varmine,varmaxe,energy(sel),hweight(sel));

%% R-COORDINATE
varminr = min(min(rpa(:)),min(rpa_in(:)));
varmaxr = max(max(rpa(:)),max(rpa_in(:)));
varminr = rmin;
varmaxr = rmax;
[histr_in,outr_in,dr_in]=histweight(npart,nbin,varminr,varmaxr,rpa_in(sel),hweight_in(sel));
[histr,outr,dr]=histweight(npart,nbin,varminr,varmaxr,rpa(sel),hweight(sel));

%% Z-COORDINATE
varminz = min(min(zpa(:)),min(zpa_in(:)));
varmaxz = max(max(zpa(:)),max(zpa_in(:)));
varminz = zmin;
varmaxz = zmax;
[histz_in,outz_in,dz_in]=histweight(npart,nbin,varminz,varmaxz,zpa_in(sel),hweight_in(sel));
[histz,outz,dz]=histweight(npart,nbin,varminz,varmaxz,zpa(sel),hweight(sel));

%% DISPLAY OPTIONS
markersize = 20;
fontsize = 15;
linewidth = 3;
fweight='bold';

%% DISPLAY FIGURES

%% PITCH ANGLE
close all
h=axes;
set(h,'FontSize',fontsize,'fontweight',fweight)
hold on ; grid on
plot(outx_in,histx_in,'linewidth',linewidth,'color','b')
if ifinal==1
  plot(outx,histx,'linewidth',linewidth,'color','r')
  legend('Initial','Final','location','northeast')
end
set(gca,'xlim',[varminx varmaxx])
xlabel('Pitch angle cosine (-)')
ylabel('Distribution (a.u)')
print -dpng ../fig/xi_distrib.png
print -depsc ../fig/xi_distrib.eps

%% VELOCITY
figure
h=axes;
set(h,'FontSize',fontsize,'fontweight',fweight)
hold on ; grid on
plot(outv_in,histv_in,'linewidth',linewidth,'color','b')
if ifinal==1
  plot(outv,histv,'linewidth',linewidth,'color','r')
  legend('Initial','Final','location','northeast')
end
%set(gca,'xlim',[3500-1500 3500+1500])
xlabel('Velocity (m/s)')
ylabel('Distribution (a.u)')
print -dpng ../fig/v_distrib.png
print -depsc ../fig/v_distrib.eps

%% ENERGY
figure
h=axes;
set(h,'FontSize',fontsize,'fontweight',fweight)
hold on ; grid on
plot(oute_in,histe_in,'linewidth',linewidth,'color','b')
if ifinal==1
  plot(oute,histe,'linewidth',linewidth,'color','r')
  legend('Initial','Final','location','northeast')
end
set(gca,'xlim',[3500-1500 3500+1500])
xlabel('Energy (keV)')
ylabel('Distribution (a.u)')
print -dpng ../fig/e_distrib.png
print -depsc ../fig/e_distrib.eps

%% R
figure
h=axes;
set(h,'FontSize',fontsize,'fontweight',fweight)
hold on ; grid on
plot(outr_in,histr_in,'linewidth',linewidth,'color','b')
if ifinal==1
  plot(outr,histr,'linewidth',linewidth,'color','r')
  legend('Initial','Final','location','northeast')
end
%set(gca,'xlim',[varminr varmaxr])
set(gca,'xlim',[rmin rmax])
xlabel('R (m)')
ylabel('Distribution (a.u)')
print -dpng ../fig/r_distrib.png
print -depsc ../fig/r_distrib.eps

%% Z
figure
h=axes;
set(h,'FontSize',fontsize,'fontweight',fweight)
hold on ; grid on
plot(outz_in,histz_in,'linewidth',linewidth,'color','b')
if ifinal==1
  plot(outz,histz,'linewidth',linewidth,'color','r')
  legend('Initial','Final','location','northeast')
end
%set(gca,'xlim',[varminz varmaxz])
set(gca,'xlim',[zmin zmax])
xlabel('Z (m)')
ylabel('Distribution (a.u)')
print -dpng ../fig/z_distrib.png
print -depsc ../fig/z_distrib.eps

