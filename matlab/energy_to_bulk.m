try
  load('../output/col_ei_vs_time.txt')
catch
  disp('---------------------------------------')
  disp('You have not run the code with')
  disp('one_orbit=1 and collision=1')
  disp('=> No energy/collision data has been saved')
  disp('=> Aborted.')
  disp('---------------------------------------')
  break
end
if length(col_ei_vs_time) < 1
  disp('---------------------------------------')
  disp('You have not run the code with')
  disp('one_orbit=1 and collision=1')
  disp('=> No energy/collision data has been saved')
  disp('=> Aborted.')
  disp('---------------------------------------')
  break
end
  
time  = col_ei_vs_time(:,1);
pel   = col_ei_vs_time(:,2);
pion  = col_ei_vs_time(:,3);

%% ENERGIES IN KEV
ene_el  = -pel/1.6e-19*1.e-3;
ene_ion = -pion/1.6e-19*1.e-3;

%% DISPLAY OPTIONS
markersize = 20;
fontsize = 15;
linewidth = 3;
fweight='bold';

%% MULTIPLICATION FACTOR FOR FIGURE DISPLAY
if ~exist('norm_factor')
  norm_factor = 0.1;
end
disp('--------------------------------------------')
disp('You may want to modify norm_factor, i.e. the')
disp('multiplication factor for figure display')
disp(['Right now: norm_factor = ',num2str(norm_factor)])
disp('--------------------------------------------')

%% DISPLAY FIGURES
clf
h=axes;
set(h,'FontSize',fontsize,'fontweight',fweight)
hold on ; grid on
plot(time,ene_el,'linewidth',linewidth,'color','b')
plot(time,ene_ion,'linewidth',linewidth,'color','r')
plot(time,cumsum(ene_el+ene_ion)*norm_factor,'linewidth',linewidth,'color','g')
if norm_factor >= 0.1
  legend('To electrons','To ions',sprintf('Cumul X %.2f',norm_factor),'location','northwest')
else
  legend('To electrons','To ions',sprintf('Cumul X %.2e',norm_factor),'location','northwest')
end
set(gca,'xlim',[time(1) time(end)])
xlabel('Time (sec)')
ylabel('Energy to the bulk (keV)')
set(gca,'xlim',[time(1) time(end)])
%set(gca,'xlim',[0 1.15])
%set(gca,'ylim',[0 350])
hold off
print -dpng ../fig/energy_to_bulk.png
print -depsc ../fig/energy_to_bulk.eps


