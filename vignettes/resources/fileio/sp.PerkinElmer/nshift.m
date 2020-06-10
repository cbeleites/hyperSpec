function [X_,Axis1_,Axis2_,Axis0_,DimX_]=nshift(X,Axis1,Axis2,Axis0,DimX);
%[X,Axis1,Axis2,Axis0,DimX]=nshift(X,Axis1,Axis2,Axis0,DimX);
%
%$ nshift  $ Version 1.4 $ Claus A. Andersson $ 11-Oct-1999 $ Copyright 1999 - $
%
% Requires
%   Matlab 5.2 or newer
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
% This function shifts the order of the 2nd and 3rd modes, e.g.,
% if used with excitation-emission landscapes, it returns an unfolding
% that contains the same elements as before, but without breaking up
% the emission spectra. This may provide a better understanding of
% differences in the emission profiles.
%
% INPUT/OUTPUT SPECIFICATION
% 'X'        is the data array returned - can be used straightforward 'as is' in the 'N-way Toolbox'
%            this array is consitent with DimX, Axis0, Axis1, Axis2, and DimX.
% 'Axis1'    is the primary axis (always defined)
% 'Axis2'    is the secondary axis (always defined)
% 'Axis0'    is the tertiary axis (only defined for three-way structures)
% 'DimX'     is the dimensionality vector as used in the 'N-way Toolbox' (only defined for three-way structures)
%
% Copyright 1998 - Claus A. Andersson, claus@andersson.dk, www.andersson.dk

if isempty(DimX),
   warndlg('''nshift.m'' only works on three-way structures. Remember to select ''unfolded three-way structure'' as output format in ''inca''.')
end;

X_=zeros(size(X));
for i=1:length(Axis0);
   m=reshape(X(i,:),DimX(2),DimX(3))';
   X_(i,:)=m(:)';
end;
DimX_=DimX;
DimX_(2)=DimX(3);
DimX_(3)=DimX(2);
Axis1_=Axis2;
Axis2_=Axis1;
Axis0_=Axis0;