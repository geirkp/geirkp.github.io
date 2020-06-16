% Script that loads a datafile with an nx*ny matrix in a particular format 
% into workspace and shapes it properly
% The first line in the field is header data
%   nc      nx      ny     dx     dy     x0     y0     ndat
% 
% dx,dy are increments between the points, x0,y0 position of lower
% left corner. In subsequent lines the data are printed, separated
% by blanks (row by row).
% Matrices X and Y, for plotting, are also produced, in addition 
% to grid parameters
%
% [H,X,Y,x,y,dx,dy,nx,ny,x0,y0,xf,yf] = gphov2field(gphovFile,...
%    sclGrd,sclFld,flipData)
%
% ------------------------------------------------------------------------
%
% Created by Finn Løvholt : 7/2 - 2008, modified 1/9 by G. Pedersen
%
% ------------------------------------------------------------------------

function [H,X,Y,x,y,dx,dy,nx,ny,x0,y0,xf,yf] = gphov2field(gphovFile,...
    sclGrd,sclFld,flipData)

% Organize defaults
if nargin < 4; flipData=false(1); end
if nargin < 3; sclFld = 1; end
if nargin < 2; sclGrd = 1; end

fid = fopen(gphovFile,'r');
% d is integer, f is float
headerData = fscanf(fid,'%d %d %d %f %f %f %f %d',8);
nc = headerData(1);  
nx = headerData(2); ny = headerData(3);
dx = headerData(4); dy = headerData(5); 
x0 = headerData(6); y0 = headerData(7);
n = headerData(8);

% Reading of data. Absence of second argument in fscanf means: 
% as many as possible
H = fscanf(fid,'%f');
fclose(fid);

% Scale the grid and genereate axis system
x0 = x0*sclGrd; y0 = y0*sclGrd;
dx = dx*sclGrd; dy = dy*sclGrd;
xf = x0 + dx*(nx-1);
yf = y0 + dy*(ny-1);

%linspace instead of x0:dx:xf to avoid missing one point due to
%round-off errors
x = linspace(x0,xf,nx);
y = linspace(y0,yf,ny);
[X,Y] = meshgrid(x,y);


H=reshape(H,nx,ny)';
H = H*sclFld;

% If desirable, flip the field
if flipData
    H = flipud(H);
end