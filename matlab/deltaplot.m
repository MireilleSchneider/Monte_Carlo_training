load ../input/iter.mat
load ../input/delta.txt

delta2d = reshape(delta,301,301);
fweight = 'bold';

v=[1000,500,200,100,50,20,10,5,2,1,1.01,0.5,0.2,0.1,0.05,0.02,0.01]*1.e-4;

fontsize = 20;

clf
set(gcf,'Color',[1 1 1]);
h=axes;
set(h,'FontSize',fontsize,'fontweight',fweight)
hold on ; grid on
c=contour(r2d,z2d,delta2d,v);
clabel(c,'fontsize',10)
contour(r2d,z2d,rho2d,50,'r')
axis equal
set(gca,'xlim',[4 8.5])
set(gca,'ylim',[-3.5 4.5])
xlabel('R (m)','FontSize',fontsize,'FontWeight',fweight)
ylabel('Z (m)','FontSize',fontsize,'FontWeight',fweight)

