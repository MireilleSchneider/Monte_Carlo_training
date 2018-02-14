function neutprof = neutsource(ti,ndeut,ntrit)
%% ---------------------------------------------------
%% H.-S. BOSCH AND G.M. HALE 1992 NUCL. FUSION 32 611
%% ---------------------------------------------------
sbg   = 34.3827;
smrc2 = 1124656;
sc1   = 1.17302e-9;
sc2   = 1.51361e-2;
sc3   = 7.51886e-2;
sc4   = 4.60643e-3;
sc5   = 1.35000e-2;
sc6   =-1.06750e-4;
sc7   = 1.36600e-5;

sx = ti./(1-(ti.*(sc2+ti.*(sc4+ti.*sc6)))./(1+ti.*(sc3+ti.*(sc5+ti.*sc7))));
se = (sbg.^2./(4.*sx)).^(1./3.);
neutprof = ndeut.*ntrit.*sc1.*sx.*sqrt(se./(smrc2.*ti.^3)).*exp(-3.*se).*1.e-6;



