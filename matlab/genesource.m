
%% LOAD PROFILES OF ION TEMPERATURE, DEUTERIUM AND TRITIUM DENSITIES
load ../input/iter.mat ti ndeut ntrit

%% NEUTRON RATE PROFILE FROM D-T REACTIONS (= ALPHA PARTICLE RATE)
neutprof = neutsource(ti,ndeut,ntrit);

%% NORMALIZED NEUTRON RAITE
neutprof = neutprof./max(neutprof(:));

%% TOROIDAL FLUX COORDINATE (RADIAL COORDINATE)
rhonorm  = linspace(0.,1.,length(neutprof));

%% EXERCISE:
%% TO GENERATE THE FLUX COORDINATE OF 1000000 MC PARTICLES
%% WITH A DISTRIBUTION AS GIVEN BY THE PROFILE OF
%% NEUTRONS ROM DT REACTIONS (NEUTSOURCE.M)
%% AND TO STORE THEM IN RHOMC_SEL FOR BEING DISPLAYED

%% GENERATE MONTE CARLO PARTICLES
%% ...
rhomc_sel = zeros(1000000,1);

%% DISPLAY NEUTRON PROFILE
close all
h=axes;
set(h,'FontSize',15,'fontweight','bold')
hold on ; grid on
plot(rhonorm,neutprof,'linewidth',3,'color','b')
set(gca,'xlim',[0. 1.])
xlabel('x')
ylabel('Normalized distribution')
print -dpng ../fig/anadisplay.png
print -depsc ../fig/anadisplay.eps

%% DISPLAY DISTRIBUTION OF GENERATED MONTE CARLO PARTICLES
figure
h=axes;
set(h,'FontSize',15,'fontweight','bold')
hold on ; grid on
hist(rhomc_sel,100)
set(gca,'xlim',[0. 1.])
xlabel('x')
ylabel('Distribution of MC praticles')
print -dpng ../fig/genesource.png
print -depsc ../fig/genesource.eps



