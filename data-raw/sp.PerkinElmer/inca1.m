function [X,Axis1,Axis2,Axis0,DimX,DataInfo]=inca1(Cmd,src,act,RayW1,RayW2,maxmiss,ovrflw,nth);

%$ inca1 $ Version 1.41 $ Claus A. Andersson $ 11-Oct-1999 $ Copyright 1999 - $
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

if ~exist('Cmd') | isempty(Cmd),
   Cmd='*.sp';
end
X=[];
Axis1=[];
Axis2=[];
Axis0=[];
DimX=[];
DataInfo.FileNames=[];
DataInfo.SourceType=[];
DataInfo.TargetType=[];
DataInfo.RayleighWidth1=[];
DataInfo.RayleighWidth2=[];
DataInfo.MissingPercentage=[];
DataInfo.ReplaceOverflow=[];
DataInfo.EveryNth=[];
nansmode=2;

if src==-1,
   [files,src,act,RayW1,RayW2,maxmiss,ovrflw,nth]=infilesca(Cmd);
   InterActive=1;
   Output_=1;
else
   files=Cmd;
   InterActive=0;
   Output_=0;
end;

mssggiven=0;

if ~isempty(files),
   
   DataInfo.FileNames=files;
   switch src
   case 1,
      DataInfo.SourceType='SP-files from LS50B, GEM software';
   case 2,
      DataInfo.SourceType='MAT-files from TIDAS, conversion software';
   end;
   switch act
   case 1,
      DataInfo.TargetType='Matrix - Landscape';
   case 2,
      DataInfo.TargetType='Matrix - Unfolded three-way array';
   case 3,
      DataInfo.TargetType='Matrix - no parameter control';
   end;
   DataInfo.RayleighWidth1=RayW1;
   DataInfo.RayleighWidth2=RayW2;
   DataInfo.MissingPercentage=maxmiss;
   switch ovrflw
   case 0,
      DataInfo.ReplaceOverflow='No';
   case 1,
      DataInfo.ReplaceOverflow='Yes';
   end;
   DataInfo.EveryNth=nth;
   n=size(files,1);
   
   switch src
      
   case 1, %'.sp'-files
      
      switch act   
         
      case 1, %Landscape and remove Rayleigh
         Axis2=[];
         for i=1:n,
            %Info(i)=spsubnew(deblank(files(i,:)),nth);
            Info(i)=spsubnew(deblank(files(i,:)),1);
            xi(i)=Info(i).FixedWaveLength;
            Axis2=unique([Axis2; Info(i).Axis]);
         end;
         Axis1=sort(xi);
         if length(unique(Axis1))~=length(Axis1),
            errordlg('Two files have the same excitation wavelength - use matrix or unfold-structure instead','Landscape error');
            error('Quitting');
         end;
         X=zeros(length(Axis1),length(Axis2));
         for i=1:n,
            Ax1=find(Axis1==xi(i));
            Ax2=find(ismember(Axis2,Info(i).Axis));
            y=Info(i).Data;
            X(Ax1,Ax2)=y';
         end;
         Axis1=Axis1';
         Axis2=Axis2';
         
         %Reduce emission variables
         Axis2=Axis2(1:nth:end);
         X=X(:,1:nth:end);
         
         if RayW1>0 | RayW2>0,
            X=incanans(X,Axis1,Axis2,RayW1,RayW2,NaN,nansmode,1);
         end;
         if ovrflw,
            p=find(X(:)>999.9);
            X(p)=NaN;
         end;
         if n>1,
            [X,Axis1,Axis2,Axis0,DimX]=checkmiss(X,Axis1,Axis2,Axis0,DimX,2,maxmiss,Output_);
         end;
         
      case 2, %Three-way array (unfolded) and remove Rayleigh
         Axis2=[];
         for i=1:n,
            %Info(i)=spsubnew(deblank(files(i,:)),nth);
            Info(i)=spsubnew(deblank(files(i,:)),1);
            xi(i)=Info(i).FixedWaveLength;
            Axis2=unique([Axis2; Info(i).Axis]);
         end;
         Axis1=unique(xi(:));
         lenAxis1=length(Axis1);
         lenAxis2=length(Axis2);
         d3=length(xi(:))/lenAxis1;
         Axis0=1:d3;
         if floor(d3)~=d3,
            errordlg('Chose only sets of compatible files - their wavelength ranges must be equal','Three-way structure error');
            error('Quitting');
         end;
         lenConf=lenAxis1*lenAxis2;
         X=zeros(d3,lenConf);
         Axis2_=Axis2;
         for j=1:d3,
            M=zeros(lenAxis1,lenAxis2);
            offset=(j-1)*lenAxis1;
            for i=1:lenAxis1,
               idx= offset + i;
               Ax1=find(Axis1==xi(idx));
               Ax2=find(ismember(Axis2_,Info(idx).Axis));
               y=Info(idx).Data;
               M(i,Ax2)=y';
            end;
            if RayW1>0 | RayW2>0,
               M=incanans(M,Axis1,Axis2,RayW1,RayW2,NaN,nansmode,1);
            end;
            X(j,:)=M(:)';
         end;
         Axis1=Axis1';
         Axis2=Axis2';
         Axis0=1:d3;
         DimX=[length(Axis0) lenAxis1 lenAxis2];
         
         %Reduce emission variables
         X=reshape(X,DimX);
         X=X(:,:,1:nth:end);
         DimX(3)=size(X,3);
         X=reshape(X,DimX(1),prod(DimX(2:end)));
         Axis2=Axis2(1:nth:end);
         
         if ovrflw,
            p=find(X(:)>999.9);
            X(p)=NaN;
         end;
         [X,Axis1,Axis2,Axis0,DimX]=checkmiss(X,Axis1,Axis2,Axis0,DimX,3,maxmiss,Output_);
         
      case 3, %Matrix
         for i=1:n,
            Info(i)=spsubnew(deblank(files(i,:)),1);
            xi(i)=Info(i).FixedWaveLength;
            y0(i)=Info(i).Axis(1);
            y1(i)=Info(i).Axis(end);
         end;
         Axis1=xi;
         
         lenSpec=length(Info(1).Data);
         Axis2_min = min(y0);
         Axis2_max = max(y1);
         Axis2_delta = (Axis2_max - Axis2_min)/(lenSpec-1);
         Axis2=[Axis2_min:Axis2_delta:Axis2_max]';
         lenAxis2=length(Axis2(:));
         
         X=zeros(length(Axis1),length(Axis2));
         for i=1:n,
            Ax2=find(ismember(Axis2,Info(i).Axis));
            y=Info(i).Data;
            X(i,Ax2)=y';
         end;
         Axis1=Axis1';
         Axis2=Axis2';
         
         %Reduce emission variables
         Axis2=Axis2(1:nth:end);
         X=X(:,1:nth:end);
         
      end;
      
   case 2, %'Converted SRB files from the TIDAS'
      
      switch act   
         
      case 1, %Landscape and remove Rayleigh
         Info=tidassubnew(deblank(files(1,:)),1);
         X=Info.Data;
         Axis1=(Info.ExAxis)';
         Axis2=Info.EmAxis;
         EmissSlit=10; %Empirical setup for the Uhl/Tidas system
         if RayW1>0 | RayW2>0,
            X=incanans(X,Axis1,Axis2,RayW1,RayW2,NaN,nansmode,1);
         end;
         %Reduce emission variables
         Axis2=Axis2(1:nth:end);
         X=X(:,1:nth:end);
         if ovrflw,
            p=find(X(:)>65535);
            X(p)=NaN;
         end;
         [X,Axis1,Axis2,Axis0,DimX]=checkmiss(X,Axis1,Axis2,Axis0,DimX,2,maxmiss,Output_);
         
      case 2, %Three-way array (unfolded) and remove Rayleigh
         err1=0;
         err2=0;
         for i=1:n,
            Info(i)=tidassubnew(deblank(files(i,:)),1);
            if i==1,
               Axis1=Info(i).ExAxis;
               Axis2=Info(i).EmAxis;
            else
               if ~all(Info(i).ExAxis==Axis1),
                  err1=1;
               end;
               if ~all(Info(i).EmAxis==Axis2),
                  err2=1;
               end;
            end;
         end;
         tmpstr='';
         if err1,
            tmpstr=strvcat(tmpstr,'The matrices have un-equal excitation axes.');
         end;
         if err1 & err2,
            tmpstr=strvcat(tmpstr,'and');
         end;
         if err2,
            tmpstr=strvcat(tmpstr,'The matrices have un-equal emission axes.');
         end;
         tmpstr=strvcat(tmpstr,'Choose only files with same ex- and em-axis.');
         if err1 | err2,
            error('Error: Non-compatible matrices selected. Quitting');
         end;
         d3=n;
         Axis0=1:d3;
         lenAxis1=length(Axis1);
         lenAxis2=length(Axis2);            
         EmissSlit=10; %Empirical setup for the Uhl/Tidas system
         lenConf=lenAxis1*lenAxis2;
         X=zeros(d3,lenConf);
         p1=1:lenAxis1;
         p2=1:lenAxis2;
         msggiven=0;
         for j=1:d3,
            M=Info(j).Data;
            if RayW1>0 | RayW2>0,
               if d3==1,
                  M=incanans(M,Axis1,Axis2,RayW1,RayW2,NaN,nansmode,1);
               else
                  M=incanans(M,Axis1,Axis2,RayW1,RayW2,NaN,nansmode,1);
               end;
            end;
            X(j,:)=M(:)';
         end;
         Axis1=Axis1';
         Axis2=Axis2';
         Axis0=1:d3;
         DimX=[d3 lenAxis1 lenAxis2];
         
         %Reduce emission variables
         X=reshape(X,DimX);
         X=X(:,:,1:nth:end);
         DimX(3)=size(X,3);
         X=reshape(X,DimX(1),prod(DimX(2:end)));
         Axis2=Axis2(1:nth:end);
         
         if ovrflw,
            p=find(X(:)>65535);
            X(p)=NaN;
         end;
         [X,Axis1,Axis2,Axis0,DimX]=checkmiss(X,Axis1,Axis2,Axis0,DimX,3,maxmiss,Output_);
         
      end;
      
   end;
   
else
   fprintf('No files selected. Quitting.\n');
   DimX=[];
   Axis0=[];
   Axis1=[];
   Axis2=[];
   X=[];
end;

return

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [Info]=spsubnew(FileName,nth);

fid=fopen(FileName,'r');

Info.Header1=fgets(fid);
Info.RemainLength=fgets(fid);
Info.SpectrumName=fgets(fid);
Info.Date=fgets(fid);
Info.Time=fgets(fid);
Info.ModifDate=fgets(fid);
Info.ModifTime=fgets(fid);
Info.AnalystName=fgets(fid);
Info.CommentsLine=fgets(fid);
LastX=str2num(char(fgets(fid)));
Info.LastX=LastX;
Info.AccumNum=str2num(fgets(fid));
Info.SpectrumManip=fgets(fid);
fgets(fid);
fgets(fid);
fgets(fid);
fgets(fid);
fgets(fid);
fgets(fid);
fgets(fid);
fgets(fid);
Info.ModelNumInstr=str2num(fgets(fid));
Info.FixedWaveLength=str2num(fgets(fid));
Info.ExcitSlit=str2num(fgets(fid));
Info.EmissSlit=str2num(fgets(fid));
Info.SpectrumType=fgets(fid);

S=fscanf(fid,'%s',1);
while ~strcmp(S,'#GR'),
   S=fscanf(fid,'%s',1);
end;

fgets(fid);
Info.XUnits=fgets(fid);
Info.YUnits=fgets(fid);
YScale=str2num(fgets(fid));
Info.YScale=YScale;
Info.YOffset=str2num(fgets(fid));
FirstX=str2num(fgets(fid));
Info.FirstX=FirstX;
DeltaX=str2num(fgets(fid));
Info.DeltaX=DeltaX;
NumOfPoints=str2num(fgets(fid));
Info.NumOfPoints=NumOfPoints;
Info.CompType=str2num(fgets(fid));
Info.MaxY=str2num(fgets(fid));
Info.MinY=str2num(fgets(fid));
fgets(fid);
[Y NumSucc]=fread(fid,NumOfPoints,'int32');
Y=Y*YScale;
Y=Y(1:nth:end); %Revised according to nth
X=FirstX:DeltaX:LastX;
X=X';
X=X(1:nth:end); %Revised according to nth

NumOfPoints=length(Y); %Revised according to nth
fclose(fid);
Info.Data=Y;
Info.Axis=X;

return

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function XCorr=incanans(X,ExAx,EmAx,WidthMin,WidthMax,Value,MaxOrder,Mode);
%XCorr=incanans(X,ExAx,EmAx,WidthMin,WidthMax,Value,MaxOrder,Mode);
%
%This algorithm takes a fluorescence landscape with
%excitation ranges and emission ranges defined by
%ExMin, ExMax, EmMin and EmMax. Given these specifications
%the algorithm identifies the Rayleigh-peak and replaces
%a certain number of neighboring wavelengths at each side
%of the peak with the 'Value' multiplied by
%the existing value in the range, - use NaN to set missing.
%
%Generally the peaks broaden as the excitation wavelength increase.
%The 'WidthMin' is the width of the Rayleigh peaks at the lowest
%excitation wavelength and 'WidthMax' is the peakwidth at the highest
%excitation wavelength. Giv also the width of the 2nd order Rayleigh
%peak of the first spectrum as 'Width2nd' since 2nd order scattering may be
%much wider than first order. The algorithm replaces up to and including
%scattering of 'MaxOrder' order starting from the 1st (no 0th order).
%'Mode'=1 uses the lowest emission wavelength as low limit, hence
%removing all emission wavelengths on the low side of the 0th order
%Rayleigh peak. 'Mode'=2 removes only the peak itself.
%
%The landscape is contained in the matrix X and each row in the matrix
%represents one emission spectrum. Hence, X has dimensions (a x b) where
%a is the number of excitation lines, and b is the number
%of measured emission wavelengths.
%
%                                  b
%          -------------------------
%          | 1st emission spectrum |
%          | 2nd emission spectrum |
%          |         ....          |
%          | ath emission spectrum |
%        a -------------------------
%
% By C.A.A. (1996)

XCorr=X;
[a b]=size(X);

exax=ExAx;
emax=EmAx;
EmMin=min(emax(:));
EmMax=max(emax(:));

Width=[WidthMin WidthMax];
for order=1:MaxOrder,
   for excount=1:a,
      exc=order*exax(excount);
      W = Width(order);
      rangemin = max(find(emax<=(exc-W)));
      rangemax = min(find(emax>=(exc+W)));
      if (exc-W) < EmMin,
         rangemin = 1;
      end;
      if (exc-W) > EmMax,
         rangemin = 0;
      end;
      if (exc+W) < EmMin,
         rangemax = 0;
      end;
      if (exc+W) > EmMax,
         rangemax = b;
      end;
      if rangemax*rangemin~=0 & rangemin<rangemax,
         if Mode==1 & order==1;
            rangemin = 1;
         end;
         if Mode==1 & order==2;
            rangemax = b;
         end;
         XCorr(excount,rangemin:rangemax)=Value*X(excount,rangemin:rangemax);
      end;
   end;
   %figure;contourf(exax,emax,XCorr');axis tight; grid on
end;

return

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [Info]=tidassubnew(FileName,nth);

load(FileName,'Emission','Excitation');
s=whos('-file',FileName);
n=length(s);
nem=length(Emission);
new=length(Excitation);

i=0;
didx=0;
srch=1;
while srch & i<n
   i=i+1;
   if all(s(i).size==[nem new]),
      didx=i;
      srch=0;
   end;
end;

if didx==0
   error(['File: ' FileName ' is not a properly converted .SRB-file. Try converting again.']);
end    

name=s(didx).name;
load(FileName,name);
eval(['X=' name ';']);
X=X(1:nth:end,:);
Emission=Emission(1:nth:end);
Info.Name=name;
Info.Data=X';
Info.EmAxis=Emission;
Info.ExAxis=Excitation;
return

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [X_,Axis1_,Axis2_,Axis0_,DimX_]=checkmiss(X,Axis1,Axis2,Axis0,DimX,arrtype,maxmiss,Output_);
% maximum '%' of missing values in any row/column

X_=X;
Axis0_=Axis0;
Axis1_=Axis1;
Axis2_=Axis2;
DimX_=DimX;

[a b]=size(X);

if maxmiss<100
   
   switch arrtype
      
   case 2, %Matrix
      
      isnX2=sum(isnan(X));
      isnX1=sum(isnan(X)');
      pX2=(100/a).*isnX2;
      pX1=(100/b).*isnX1;
      ValidX2=find( pX2<=maxmiss );
      ValidX1=find( pX1<=maxmiss );
      if Output_
         if isempty(ValidX2) | isempty(ValidX1),
            tmpstr=[];
            tmpstr=strvcat(tmpstr,'No valid object/variables were found. They were all eliminated');
            tmpstr=strvcat(tmpstr,'due to the presence of too many missing observations.');
            tmpstr=strvcat(tmpstr,'Check the complete range of premisses and/or modify');
            tmpstr=strvcat(tmpstr,'the maximum percentage of allowed missing values.');
            errordlg(tmpstr,'Error: No valid variables');
         end;
      end;
      
      if Output_
         if (length(ValidX2)~=length(isnX2)) | (length(ValidX1)~=length(isnX1)),
            tmpstr=[];
            tmpstr=strvcat(tmpstr,'Variables and/or objects have been eliminated according to the');
            tmpstr=strvcat(tmpstr,['defined maximum percentage of ' int2str(maxmiss) '% of allowed missing values.']);
            tmpstr=strvcat(tmpstr,'The involved variables have been removed automatically.');
            tmpstr=strvcat(tmpstr,'Inspect the returned axes.');
            warndlg(tmpstr,'Warning: Some variables have been removed');
         end;
      end;
      X_=X(ValidX1,:);
      X_=X(:,ValidX2);
      Axis1_=Axis1(ValidX1);
      Axis2_=Axis2(ValidX2);
      DimX_=[];
      Axis0=[];
      
   case 3, %Three-way array
      
      isnX0=sum(isnan(X)');
      tX=reshape(X',DimX(2),prod(DimX([1 3])));
      isnX1=sum(isnan(tX)');
      tX=reshape(tX',DimX(3),prod(DimX([1 2])));
      isnX2=sum(isnan(tX)');
      
      pX0=(100/prod(DimX([2 3]))).*isnX0;
      pX1=(100/prod(DimX([1 3]))).*isnX1;
      pX2=(100/prod(DimX([1 2]))).*isnX2;
      
      ValidX0=find( pX0<=maxmiss );
      ValidX1=find( pX1<=maxmiss );
      ValidX2=find( pX2<=maxmiss );
      
      if Output_
         if isempty(ValidX1) | isempty(ValidX2) | isempty(ValidX0),
            tmpstr=[];
            tmpstr=strvcat(tmpstr,'No valid object/variables were found. They were all eliminated');
            tmpstr=strvcat(tmpstr,'due to the presence of too many missing observations.');
            tmpstr=strvcat(tmpstr,'Check the complete range of premisses and/or modify');
            tmpstr=strvcat(tmpstr,'the maximum percentage of allowed missing values.');
            errordlg(tmpstr,'Error: No valid variables');
         end;
      end;
      
      if Output_
         if (length(ValidX1)~=length(isnX1)) | (length(ValidX2)~=length(isnX2)) | (length(ValidX0)~=length(isnX0)),
            tmpstr=[];
            tmpstr=strvcat(tmpstr,'Variables and/or objects have been eliminated according to the');
            tmpstr=strvcat(tmpstr,['defined maximum percentage of ' int2str(maxmiss) '% of allowed missing values.']);
            tmpstr=strvcat(tmpstr,'The involved variables have been removed automatically.');
            tmpstr=strvcat(tmpstr,'Inspect the returned axes.');
            warndlg(tmpstr,'Warning: Some variables have been removed');
         end;
      end;
      
      X_=X(ValidX0,:);
      Axis0_=Axis0(ValidX0);
      DimX_(1)=length(Axis0_);
      
      X_=reshape(X_',DimX_(2),prod(DimX_([1 3])));
      X_=X_(ValidX1,:);
      Axis1_=Axis1(ValidX1);
      DimX_(2)=length(Axis1_);
      
      X_=reshape(X_',DimX_(3),prod(DimX_([1 2])));
      X_=X_(ValidX2,:);
      Axis2_=Axis2(ValidX2);
      DimX_(3)=length(Axis2_);
      
      X_=reshape(X_',DimX_(1),prod(DimX_([2 3])));
   end
end;