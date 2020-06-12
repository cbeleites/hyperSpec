function [X,Axis1,Axis2,Axis0,DimX,DataInfo]=inca(Cmd,src,act,RayW1,RayW2,maxmiss,ovrflw,nth);
%[X,Axis1,Axis2,Axis0,DimX,DataInfo]=inca(FileNames);
%
%$ inca  $ Version 1.42 $ Claus A. Andersson $ 11-Oct-1999 $ Copyright 1999 - $
%
% Requires
%   Matlab 5.2 or newer
% and access to following files
%   'inca1' , 'infilesca' , 'infilesca_cb', 'nshift'
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
% INPUT SPECIFICATION
% 'FileNames' is the mask for the files to read.
%
% OUTPUT SPECIFICATION
% 'X'        is the data array returned - can be used straightforward 'as is' in the 'N-way Toolbox for MATLAB'
%            From version 1.4 the emission spectra are appended rowwise to provide a
%            better basis for interpretation.
% 'Axis1'    is the primary axis (always defined)
% 'Axis2'    is the secondary axis (always defined)
% 'Axis0'    is the tertiary axis (only defined for three-way structures)
% 'DimX'     is the dimensionality vector as used in the 'N-way Toolbox' (only defined for three-way structures)
% 'DataInfo' is a 'struct' that contains a little info over the conversion conditions
%
%See also:
%  'nshift' and 'ncollapse'
%
%Examples:
%[X,Axis1,Axis2,Axis0,DimX,DataInfo]=inca('*.sp');
%[X,Axis1,Axis2,Axis0,DimX,DataInfo]=inca('p001*.sp');
%[X,Axis1,Axis2,Axis0,DimX,DataInfo]=inca('ab?1*.sp');
%[X,Axis1,Axis2,Axis0,DimX,DataInfo]=inca('a*.mat');
%
%Associated commands:
%  Plotting:
%    mesh(Axis1,Axis2,X');axis tight;rotate3d on;grid on %Plots the landscape
%    mesh(Axis1,Axis2,reshape(X(1,:),DimX(2),DimX(3))');axis tight;rotate3d on;grid on %Plots the first object

if nargin==0,
    help('inca.m');
end;

if exist('Cmd')~=1,
    Cmd=[];
end;
if isempty(Cmd),
    Cmd='';
end;

if exist('src')~=1,
    src=[];
end;
if isempty(src),
    src=-1;
end;

if exist('act')~=1,
    act=[];
end;
if isempty(act),
    act=1;
end;

if exist('RayW1')~=1,
    RayW1=[];
end;
if isempty(RayW1),
    RayW1=0;
end;

if exist('RayW2')~=1,
    RayW2=[];
end;
if isempty(RayW2),
    RayW2=0;
end;

if exist('maxmiss')~=1,
    maxmiss=[];
end;
if isempty(maxmiss),
    maxmiss=100;
end;

if exist('ovrflw')~=1,
    ovrflw=[];
end; 
if isempty(ovrflw),
    ovrflw=0;
end; 

if exist('nth')~=1,
    nth=[];
end; 
if isempty(nth),
    nth=0;
end; 

[X,Axis1,Axis2,Axis0,DimX,DataInfo]=inca1(Cmd,src,act,RayW1,RayW2,maxmiss,ovrflw,nth);

if ~isempty(DimX),
   [X,Axis1,Axis2,Axis0,DimX]=nshift(X,Axis1,Axis2,Axis0,DimX);
end;
