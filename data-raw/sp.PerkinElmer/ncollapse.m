function Z = ncollapse(X,dim,n);
%function Z = ncollapse(X,dim,n);
%
%$ ncollapse $ Version 1.4 $ Claus A. Andersson (claus@andersson.dk) $ 11-Oct-1999 $ Copyright 1999 - $
%
% Programmed by Claus A. Andersson, Copyrighted 1999 - 
% E-mail: claus@andersson.dk
% This file and the code in it belongs to the holder of the
% copyrights and is made public under the following constraints:
% It must not be changed or modified and code cannot be added.
% It cannot be annoted by other authors and this file must be
% regarded as read-only. In case of doubt, contact
% the holder of the copyrights.
%
%This function can be used to reduce the array 'X' in one or more
%modes by sequentially grouping 'n' observations in mode 'dim' 
%to one new value calculated as the mean over these 'n' initial
%observations. It is assumed that missing values are designated NaN.
%
%Note: If all observations in any subgroup are NaN, the resulting
%value will become a NaN itself. If this occurs, try using
%larger 'n'. Since mostly very large datasets will be collapsed,
%this program uses a slow, but far less memory demanding, approach.
%
%Example:
%  Z=ncollapse(rand(3,7,8),3,4) %size(Z)=[3 7 2]; 
%  Z=ncollapse(rand(3,7,8),3,2) %size(Z)=[3 7 4]; 
%  Z=ncollapse(rand(3,6,8),2,2) %size(Z)=[3 3 8]; 

ndim = length(size(X));
X=shiftdim(X,dim-1);
dim_X1 = size(X);
X=reshape(X,dim_X1(1),prod(dim_X1(2:end)));

n_Z = ceil(size(X,1)/n);
Z = zeros(n_Z,size(X,2)); 
for i = 1:size(Z,1),
   l0 = (i-1)*n+1; 
   l1 = i*n;
   if l1 > size(X,1),
      l1 = size(X,1);
	end;
   dX = X(l0:l1,:);
	nnan = isnan(dX);
   a = find(nnan);
   dX(a) = 0;
   snnan = sum(~nnan);
   a = find(~snnan);
   snnan(a) = NaN;
   Z(i,:) = sum(dX) ./ snnan;
end;

dim_X1(1) = size(Z,1);
Z=reshape(Z,dim_X1);
Z=shiftdim(Z,ndim-dim+1);