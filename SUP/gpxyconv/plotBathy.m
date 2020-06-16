% Plots a surface of a bathymetry
% The function returns a handle to the figure
% Input required :    H, X, Y, 2D field and coordinates
% Optional       :
% axes labels, subdomain indices, plot aspect ratio, light, cmapin
% 
% plotBathy(H,X,Y,xAxLbl,yAxLbl,...
%    startx,stopx,starty,stopy,aspectRat,addlight,cmapin)
%
% ------------------------------------------------------------------------
%
% Created by Finn Løvholt, 7/2-2008
% 
% ------------------------------------------------------------------------

function hndl = plotBathy(H,X,Y,xAxLbl,yAxLbl,...
    startx,stopx,starty,stopy,aspectRat,addlight,cmapin)

if nargin < 11; addlight = false(1); end
if nargin < 10; aspectRat = 300; end
if nargin < 6;
    [ny, nx] = size(H);
    startx = 1; stopx = nx;
    starty = 1; stopy = ny;
end
if nargin < 4
    xAxLbl = 'Longitude deg.';
    yAxLbl = 'Latitude deg.';
end

hMin = min(min(H(starty:stopy,startx:stopx)));
hMax = max(max(H(starty:stopy,startx:stopx)));

hndl = figure
surf(X(starty:stopy,startx:stopx),Y(starty:stopy,startx:stopx)...
    ,H(starty:stopy,startx:stopx),'linestyle','none',...
    'facecolor','interp','facelighting','phong');
view(0,90);
if nargin < 12
    cmap1 = jet(2048);
    cmap2 = bone(round(2048*hMax/abs(hMin)));
    cmap = [cmap1; flipud(cmap2)];
else
    cmap = cmapin;
end
colormap(cmap)
colorbar('horiz');
set(gca,'DataAspectRatio',[1 1 aspectRat]);
xlabel(xAxLbl,'fontsize',16);
ylabel(yAxLbl,'fontsize',16);
set(gca,'fontsize',16);
if addlight
    light
end