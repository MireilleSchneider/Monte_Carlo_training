%% ---------------------------------
%% PURPOSE: TO PLOT THE EQUILIBRIUM
%% ---------------------------------

%% INPUT EQUILIBRUIM
load ../input/iter.mat
try
  load('../output/icrh_kick_positions.txt');
catch
  disp('----------------------------------------------')
  disp('You have not run the code with')
  disp('icrh_heating=1')
  disp('... OR YOU HAVE NO KICK!')
  disp('(Bad input tuning) => Aborted.')
  disp('=> Aborted.')
  disp('----------------------------------------------')
  break
end
  
%% SECURITY
if length(icrh_kick_positions) < 1
  disp('----------------------------------------------')
  disp('You have not run the code with icrh_heating=1')
  disp('... OR YOU HAVE NO KICK!')
  disp('(Bad input tuning) => Aborted.')
  disp('=> Aborted.')
  disp('----------------------------------------------')
  break
end

%% ION TRAJECTORY
rkick = icrh_kick_positions(:,1);
zkick = icrh_kick_positions(:,2);
r_res = icrh_kick_positions(:,3);
nharm = icrh_kick_positions(:,4);

%% DISPLAY OPTIONS
markersize = 20;
fontsize = 15;
linewidth = 3;
fweight='bold';

%% COLOR OF THE RESONANCE LAYER
if ~exist('mycol')
  mycol = 'r';
end
disp('----------------------------------------------')
disp('You may want to modify mycol, i.e. the color')
disp('of the resonance layer:')
disp(['Right now: mycol = ''',mycol,''''])
disp('-----------------------------------------------')
disp('Available colors: ''r''=red, ''k''=black, ''c''=cyan,')
disp('''m''=magenta, ''b''=blue, ''y''=yellow')
disp('-----------------------------------------------')

%% FLAG THE FIRST DISPLAY
if ~exist('ifirst')
  ifirst = 1;
end

%% PLOT THE FIGURE
if ifirst==1
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
end
plot(rkick,zkick,'.','color',mycol)
ifirst = 0;

print -dpng ../fig/icrh_resonance.png
print -depsc ../fig/icrh_resonance.eps


