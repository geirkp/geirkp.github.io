% Writes a matlab field to gphov format
% Header information must be available 
%
% field2gphov(gphovFile,H,nc,nx,ny,dx,dy,x0,y0,n)
%
% H      : The field to be converted, must be 2D matrix (meshgrid like)
% nc     : Grid and field scales
% nx, ny : number of horizontal nodes
% dx, dy : grid resolutions
% x0, y0 : corner co-ordinates
% n      : number of fields (usually 1)
%   
% ------------------------------------------------------------------------
%
% Created by Finn Løvholt, 5/2-2008
%
% ------------------------------------------------------------------------

function field2gphov(gphovFile,H,nc,nx,ny,dx,dy,x0,y0,n)

fid = fopen(gphovFile,'w');
fprintf(fid,'%g %g %g %12.6f %12.6f %12.6f %12.6f %g \n',...
    nc,nx,ny,dx,dy,x0,y0,n);

n = 1; n_line = 1;

for i = 1:ny
    for j = 1:nx
        fprintf(fid,'%12.6f',H(i,j));
        if n_line < 10
           n_line = n_line + 1;
        else
           fprintf(fid,'\n');
           n_line = 1;
        end
        n = n + 1;
    end
    n_line = 1;
    fprintf(fid,'\n');
end
fclose(fid); 