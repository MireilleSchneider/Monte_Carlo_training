try
  load('../output/ion_versus_time.txt')
catch
  disp('---------------------------------------')
  disp('You have not run the code with')
  disp('one_orbit=1 and orbit_following=1')
  disp('=> No orbit data has been saved')
  disp('=> Aborted.')
  disp('---------------------------------------')
  break
end

if length(ion_versus_time) < 1
  disp('---------------------------------------')
  disp('You have not run the code with')
  disp('one_orbit=1 and orbit_following=1')
  disp('=> No orbit data has been saved')
  disp('=> Aborted.')
  disp('---------------------------------------')
  break
end

time  = ion_versus_time(:,1);
vel   = ion_versus_time(:,2);
vpara = ion_versus_time(:,3);

energy = 0.5*4*1.67e-27.*vel.^2./1.6e-19*1e-3;

%% DISPLAY OPTIONS
markersize = 20;
fontsize = 15;
linewidth = 3;
fweight='bold';

%% DISPLAY FIGURES

%% VELOCITY
figure     
set(gcf,'Color',[1 1 1]);
h=axes;    
set(h,'FontSize',fontsize,'fontweight',fweight)
hold on ; grid on                              
plot(time,vel,'linewidth',linewidth,'color','b')
set(gca,'xlim',[time(1) time(end)])             
xlabel('Time (sec)')                            
ylabel('Particle velocity (m/s)')               
hold off                                        
print -dpng ../fig/velocity_vs_time.png         
print -depsc ../fig/velocity_vs_time.eps        

%% ENERGY
figure
h=axes;
set(h,'FontSize',fontsize,'fontweight',fweight)
hold on ; grid on
plot(time,energy,'linewidth',linewidth,'color','b')
set(gca,'xlim',[time(1) time(end)])
xlabel('Time (sec)')
ylabel('Particle energy (keV)')
print -dpng ../fig/energy_vs_time.png
print -depsc ../fig/energy_vs_time.eps


