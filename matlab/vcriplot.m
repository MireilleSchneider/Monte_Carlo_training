load ../input/iter.mat

a_ion = 4;
z_ion = 2;

%% ELECTRON DENSITY AND TEMPERATURE
ne = sum(ntot,2);
te = ti;
ti = ti*1e3;
te = te*1e3;

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
zi = compo.z; % Z OF ALL SPECIES
ai = compo.a; % A OF ALL SPECIES
nimp = length(compo.a);

%% ZBAR
zbar = zeros(nrho,1);
for i=1:nimp
  zbar = zbar + ntot(i)*zi(i).^2/ai(i);
end
zbar = zbar./ne;

%% CRITICAL ENERGY AND VELOCITY
ecri = 14.8 .* te*1.e-3 .* a_ion .*zbar.^(2./3.);     % CRITICAL ENERGY (KEV)
vcri = sqrt(2000*1.6e-19*ecri/(a_ion*1.6726e-27)); % CRITICAL VELOCITY (M/S)

%% CORRECTION OF VCRI DUE TO NON-CONSTANT COULOMB LOGARITHM
vcri = vcri.*(logli./logle).^(1./3.);
ecri(end) = 0.;
vcri(end) = 0.;
