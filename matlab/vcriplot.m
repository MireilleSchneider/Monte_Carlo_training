%load ../input/iter.mat

load ../input/te.txt
load ../input/ti.txt
load ../input/ntot.txt
load ../input/ni.txt
load ../input/ne.txt
load ..//input/rho_in.txt
load ../input/a_ions.txt
load ../input/z_ions.txt

a_ion = 4;
z_ion = 2;

%% ELECTRON DENSITY AND TEMPERATURE IN M-3 AND KEV

%% ANALYTIC SLOWING-DOWN TIME
nrho = length(te);
rhonorm = linspace(0.,1.,nrho);

%% COULOMB LOGARITHM
logle = 15.2-0.5*log(ne/1e20)+log(te)+log(z_ion);
logli = 17.3-0.5*log(ne/1e20)+1.5*log(ti)+log(z_ion);
logl  = (logle+logli)*0.5;
%logl = 17*ones(nrho,1);

tslow_ana = 6.27e8*a_ion*(te.^1.5)./(ne*1e-6*z_ion^2.*logl);  % SLOWING-DOWN TIME

%% ANALYTICAL CRITICAL VELOCITY
zi = z_ions; % Z OF ALL SPECIES
ai = a_ions; % A OF ALL SPECIES
nimp = length(zi);

%% ZBAR
zbar = zeros(1,nrho);
for i=1:nimp
  ik = (i-1)*101+1;
  zbar = zbar + ntot(ik)*zi(i).^2/ai(i);
end
zbar = zbar./ne;

%% CRITICAL ENERGY AND VELOCITY
ecri = 14.8 .* te .* a_ion .*zbar.^(2./3.);  % CRITICAL ENERGY (KEV)
vcri = sqrt(2000*1.6e-19*ecri/(a_ion*1.6726e-27)); % CRITICAL VELOCITY (M/S)

%% CORRECTION OF VCRI DUE TO NON-CONSTANT COULOMB LOGARITHM
vcri = vcri.*(logli./logle).^(1./3.);
ecri(end) = 0.;
vcri(end) = 0.;

%% DISPLAY FIGURE
fontsize = 20;
fweight = 'bold';
linewidth=3;
clf
set(gcf,'Color',[1 1 1]);
h=axes;
set(h,'FontSize',fontsize,'fontweight',fweight)
hold on ; grid on
plot(rho_in./rho_in(end),ecri,'linewidth',linewidth,'color','b')
xlabel('Normalized toroidal flux coordinate (-)')
ylabel('Critical energy (keV)')
hold off
print -dpng ../fig/critical_energy.png
print -depsc ../fig/critical_energy.eps


