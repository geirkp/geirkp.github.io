% Examplescript that loads a gphov file and plots the surface:
% For both gphov2field and plotBathy, type help "mfilename" for options
% Typing help plotExample, this top text appears in the workspace

% Clear workspace and screen:
clear
clc

% The filename (taken from the near field simulations close to La Palma)
gphovFile = ['y5'];
% Load the headerdata and field.
[H,X,Y,x,y,dx,dy,nx,ny,x0,y0,xf,yf] = gphov2field(gphovFile);
% Plot the field
plotBathy(H,X,Y);
hold on             % For adding more plots on top of eachother
% Change color palette to show wave rather than bathy
colormap('jet');
% Limits color range to between -50 and 50 m
% Significant since land is marked by large negative numbers
caxis([-50 50]);
%Limit the depicted domain
xlim([-25 -12]);
ylim([23 35]);
% Add a few location and explanations
text(-18,29,1000,'La Palma','fontsize',12);
plot3(-15.5,28.1,1000,'marker','o','markerfacecolor','y','markersize',6);
text(-15.2,28.1,1000,'Las Palmas','fontsize',12);
text(-18,33,1000,'Madeira','fontsize',12);
text(-16.5,24,1000,'Western Sahara','fontsize',12,'color','w');