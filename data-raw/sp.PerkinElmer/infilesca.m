function [FileNames,Src,Act,RayW1,RayW2,MaxMiss,OvrFlw,nth]=infilesca(Cmd)

%$ infilesca $ Version 1.41 $ Claus A. Andersson (claus@andersson.dk) $ 11-Oct-1999 $ Copyright 1999 - $
%
%1.41 : -Fixed interpreter compatibility for higher speed
%1.40 : -Bugs fixed in selection area, sorting and ex/em order
%1.30 : -Optimized for speed
%1.20 : -Bugs fixed
%1.02 : -now allows for non-interactive running
%1.01 : -Normalized font sizes for system font size independence
%1.0a : -Wider range of scatter broadness, now up to 60nm, changed to 'listbox'-format
%       -Fixed fonts to ensure equal looks over types of display setups 'MS sans serif, 8 pt'
%       -Progress indicator for initial sorting
%1.0  : -Initial version, CA 1998
%
% Programmed by Claus A. Andersson, Copyrighted 1999 - 
% E-mail: claus@andersson.dk
% This file and the code in it belongs to the holder of the
% copyrights and is made public under the following constraints:
% It must not be changed or modified and code cannot be added.
% It cannot be annoted by other authors and this file must be
% regarded as read-only. In case of doubt, contact
% the holder of the copyrights.

hw = waitbar(0,'Please wait while initializing INCA ...');

FileNames=[];
Src=[];
Act=[];
RayW1=0;
RayW2=0;
MaxMiss=90;
OvrFlw=1;
nth=2;
Files=dir(Cmd);
waitbar(0.20);
FName=[];
FDate=[];
IDm=find(~str2num(sprintf('%i ',(Files(:).isdir))));
n=length(IDm);

if n>0,
   
   FSize=zeros(n,1);
   if ~isempty(IDm),
      k=1;
      for i=IDm;
         FName=strvcat(FName,Files(i).name);
         FDate=strvcat(FDate,Files(i).date);
         FSize(k)=Files(i).bytes;
         k=k+1;
      end;
   end;
   waitbar(0.40);
   
   List=[FName char(32+zeros(n,5)) FDate char(32+zeros(n,5)) int2str(FSize)];
   
   waitbar(0.60);
   
   SrcList=strvcat('.SP-file type from LS50B with FLDM under GEM');
   SrcList=strvcat(SrcList,'.MAT-file type as converted from TIDAS');
   
   RayW1Def=18;
   RayW1DefIdx=19;
   RayWidth1=strvcat(' 0   (Inactive - no removal)',int2str([1:RayW1Def-1]'));
   RayWidth1=strvcat(RayWidth1,[int2str(RayW1Def) '   (Default)']);
   RayWidth1=strvcat(RayWidth1,int2str([RayW1Def+1:60]'));
   
   RayW2Def=20;
   RayW2DefIdx=21;
   RayWidth2=strvcat(' 0   (Inactive - no removal)',int2str([1:RayW2Def-1]'));
   RayWidth2=strvcat(RayWidth2,[int2str(RayW2Def) '   (Default)']);
   RayWidth2=strvcat(RayWidth2,int2str([RayW2Def+1:60]'));
   
   MaxMissDef=90;
   MaxMissDefIdx=19;    
   MaxMissList=strvcat('',' 0   (No missing values allowed)');
   MaxMissList=strvcat(MaxMissList,int2str([5:5:85]'));
   MaxMissList=strvcat(MaxMissList,'90   (Default)');
   MaxMissList=strvcat(MaxMissList,int2str([95:5:95]'));
   MaxMissList=strvcat(MaxMissList,'100  (No check performed)');
   
   strLS50B='Max. valid readout is 999.99 (PE LS50B). It is recommended to treat any overflowing observations as missing values. ';
   strTIDAS='Max. valid readout is 65535 (TIDAS). It is recommended to treat any overflowing observations as missing values. ';
   strINST=strLS50B;
   src_ini=1;
   nth_init=2;
   if(findstr(lower(Cmd),lower('.mat'))),
      nth_init=1;
      src_ini=2;
      strINST=strTIDAS;
   end;
   
   ActList=[];
   if src_ini==1,
      ActList=strvcat(ActList,'Landscape with Rayleigh signals set to NaN');
      ActList=strvcat(ActList,'Unfolded three-way structure with Rayleigh signals set to NaN');
      ActList=strvcat(ActList,'Matrix (no parameter control)');
   elseif src_ini==2,
      ActList=strvcat(ActList,'Landscape with Rayleigh signals set to NaN');
      ActList=strvcat(ActList,'Unfolded three-way structure with Rayleigh signals set to NaN');
   end;
   
   UData=[size(FName,2) 5+size(FDate,2)+size(FName,2)];
   
   colvec=[0.8,0.8,0.8];
   waitbar(0.90);
   
   save([tempdir 'temp.mat'],'RayW1DefIdx','RayW2DefIdx','MaxMissDefIdx','strLS50B','strTIDAS','UData','nth_init');
   waitbar(1.00);
   
   %Orig_size=[150 50 640 550];
   h0 = figure('Color',colvec, ...
      'BusyAction','cancel', ...
      'Interruptible','on', ...
      'Name','Multiple file selection - INCA 1.41 - claus@andersson.dk', ...
      'NumberTitle','off', ...
      'Units','points', ...
      'Position',[150 50 490 420], ...
      'Resize', 'on', ...
      'Tag','h0', ...
      'Units','normalized', ...
      'WindowStyle', 'normal');
   h1 = uicontrol('Parent',h0, ...
      'BackgroundColor',[1 1 1], ...
      'FontName','Courier', ...
      'FontUnits','points', ...
      'FontAngle','normal', ...
      'FontWeight','normal', ...
      'FontSize',8, ...
      'FontUnits','normalized', ...
      'ListboxTop',1, ...
      'Max',n, ...
      'Min',0, ...
      'Units','points', ...
      'Position',[10 110 350 300], ...
      'String',List, ...
      'Style','listbox', ...
      'Tag','h1', ...
      'TooltipString','Mark the files that apply', ...
      'Units','normalized',...
      'Value',[1]);
   
   h2 = uicontrol('Parent',h0, ...
      'ListboxTop',0, ...
      'Callback','infilesca_cb sort1', ...
      'Units','points', ...
      'Position',[380 385 90 15], ...
      'String','Sort by name', ...
      'TooltipString','Sort the file list by name', ...
      'Units','normalized',...
      'Tag','h2');
   h3 = uicontrol('Parent',h0, ...
      'ListboxTop',0, ...
      'Callback','infilesca_cb sort2', ...
      'Units','points', ...
      'Position',[380 365 90 15], ...
      'String','Sort by date', ...
      'TooltipString','Sort the file list by date', ...
      'Units','normalized',...
      'Tag','h3');
   h4 = uicontrol('Parent',h0, ...
      'ListboxTop',0, ...
      'Callback','infilesca_cb sort3', ...
      'Units','points', ...
      'Position',[380 345 90 15], ...
      'String','Sort by size', ...
      'TooltipString','Sort the file list by size', ...
      'Units','normalized',...
      'Tag','h4');
   h13 = uicontrol('Parent',h0, ...
      'ListboxTop',0, ...
      'Callback','infilesca_cb selall', ...
      'Units','points', ...
      'Position',[380 325 90 15], ...
      'String','Select all files', ...
      'TooltipString','Select all files', ...
      'Units','normalized',...
      'Tag','h13');
   
   h5 = uicontrol('Parent',h0, ...
      'ListboxTop',0, ...
      'ButtonDownFcn','infilesca_cb cancel', ...
      'Callback','infilesca_cb cancel', ...
      'Units','points', ...
      'Position',[380 280 90 20], ...
      'String','Cancel', ...
      'TooltipString','Cancel file selection', ...
      'Units','normalized',...
      'Tag','h5');
   h6 = uicontrol('Parent',h0, ...
      'ListboxTop',0, ...
      'ButtonDownFcn','infilesca_cb exit', ...
      'Callback','infilesca_cb exit', ...
      'Units','points', ...
      'Position',[380 250 90 20], ...
      'String','OK', ...
      'TooltipString','Accept the selected files', ...
      'Units','normalized',...
      'Tag','h6');
   
   h12= uicontrol('Parent',h0, ...
      'FontAngle','normal', ...
      'FontName','MS Sans Serif', ...
      'FontUnits','points', ...
      'FontAngle','normal', ...
      'FontWeight','normal', ...
      'FontSize',8, ...
      'FontUnits','normalized', ...
      'BackgroundColor',colvec, ...
      'HorizontalAlignment','left', ...
      'Units','points', ...
      'ListboxTop',0, ...
      'Units','points', ...
      'Position',[10 78 170 20], ...
      'String','Specify source file origination and history', ...
      'Style','text', ...
      'TooltipString','Specify the type of input file', ...
      'Units','normalized',...
      'Tag','h12');
   h7 = uicontrol('Parent',h0, ...
      'FontName','MS Sans Serif', ...
      'FontUnits','points', ...
      'FontAngle','normal', ...
      'FontWeight','normal', ...
      'FontSize',8, ...
      'FontUnits','normalized', ...
      'ListboxTop',0, ...
      'Callback','infilesca_cb lb_src', ...        
      'Units','points', ...
      'Position',[190 80 285 20], ...        
      'String',SrcList, ...
      'Style','popupmenu', ...
      'Tag','h7', ...
      'Units','normalized',...
      'Value',src_ini);
   
   h11= uicontrol('Parent',h0, ...
      'FontName','MS Sans Serif', ...
      'FontUnits','points', ...
      'FontAngle','normal', ...
      'FontWeight','normal', ...
      'FontSize',8, ...
      'FontUnits','normalized', ...
      'BackgroundColor',colvec, ...
      'HorizontalAlignment','left', ...
      'ListboxTop',0, ...
      'Units','points', ...
      'Position',[10 53 170 20], ...
      'String','Specify how to arrange the output data array', ...
      'Style','text', ...
      'TooltipString','Specify the output format of the resulting array', ...
      'Units','normalized',...
      'Tag','h11');
   h8 = uicontrol('Parent',h0, ...
      'FontName','MS Sans Serif', ...
      'FontUnits','points', ...
      'FontAngle','normal', ...
      'FontWeight','normal', ...
      'FontSize',8, ...
      'FontUnits','normalized', ...
      'ListboxTop',0, ...
      'Callback','infilesca_cb lb_act', ...        
      'Units','points', ...
      'Position',[190 55 285 20], ...        
      'String',ActList, ...
      'Style','popupmenu', ...
      'Tag','h8', ...
      'Units','normalized',...
      'Value',1);
   
   h10= uicontrol('Parent',h0, ...
      'FontName','MS Sans Serif', ...
      'FontUnits','points', ...
      'FontAngle','normal', ...
      'FontWeight','normal', ...
      'FontSize',8, ...
      'FontUnits','normalized', ...
      'BackgroundColor',colvec, ...
      'HorizontalAlignment','left', ...
      'Units','points', ...
      'Position',[10 28 170 20], ...
      'String','Width [nm] of Rayleigh scatter signals, first order', ...
      'Style','text', ...
      'TooltipString','Specify ''inactive'' for no removal of scatter signals', ...
      'Units','normalized',...
      'Tag','h10');
   h9 = uicontrol('Parent',h0, ...
      'BackgroundColor',colvec, ...
      'FontName','MS Sans Serif', ...
      'FontUnits','points', ...
      'FontAngle','normal', ...
      'FontWeight','normal', ...
      'FontSize',8, ...
      'FontUnits','normalized', ...
      'ListboxTop', 1, ...
      'Units','points', ...
      'Position',[190 35 115 15], ...
      'String',RayWidth1, ...
      'Style','popupmenu', ...
      'TooltipString','Specify width [nm] of the FIRST ORDER scatter peaks', ...
      'Tag','h9', ...
      'Units','normalized',...
      'Callback','infilesca_cb setRayW1');
   set(h9,'Value',RayW1DefIdx);
   h14= uicontrol('Parent',h0, ...
      'BackgroundColor',colvec, ...
      'HorizontalAlignment','left', ...
      'FontName','MS Sans Serif', ...
      'FontUnits','points', ...
      'FontAngle','normal', ...
      'FontWeight','normal', ...
      'FontSize',8, ...
      'FontUnits','normalized', ...
      'ListboxTop',0, ...
      'Units','points', ...
      'Position',[310 28 55 20], ...
      'String','second order', ...
      'Style','text', ...
      'TooltipString','Specify ''inactive'' for no removal of scatter signals', ...
      'Units','normalized',...
      'Tag','h14');
   h15 = uicontrol('Parent',h0, ...
      'FontName','MS Sans Serif', ...
      'FontUnits','points', ...
      'FontAngle','normal', ...
      'FontWeight','normal', ...
      'FontSize',8, ...
      'FontUnits','normalized', ...
      'Units','points', ...
      'Position',[360 35 115 15], ...
      'String',RayWidth2, ...
      'Style','popupmenu', ...
      'TooltipString','Specify width [nm] of the SECOND ORDER scatter peaks', ...
      'Tag','h15', ...
      'Enable','on', ...
      'Units','normalized',...
      'Callback','infilesca_cb setRayW2');
   set(h15,'Value',RayW2DefIdx);
   
   h16= uicontrol('Parent',h0, ...
      'FontName','MS Sans Serif', ...
      'FontUnits','points', ...
      'FontAngle','normal', ...
      'FontWeight','normal', ...
      'FontSize',8, ...
      'FontUnits','normalized', ...
      'BackgroundColor',colvec, ...
      'HorizontalAlignment','left', ...
      'ListboxTop',0, ...
      'Units','points', ...
      'Position',[10 3 170 20], ...
      'String','Maximum number of missing values [%] ', ...
      'Style','text', ...
      'TooltipString','Maximum number of missing values [%] in any row or column', ...
      'Units','normalized',...
      'Tag','h16');
   h17 = uicontrol('Parent',h0, ...
      'FontName','MS Sans Serif', ...
      'FontUnits','points', ...
      'FontAngle','normal', ...
      'FontWeight','normal', ...
      'FontSize',8, ...
      'FontUnits','normalized', ...
      'ListboxTop',0, ...
      'Units','points', ...
      'Position',[190 5 285 20], ...        
      'String',MaxMissList, ...
      'Style','popupmenu', ...
      'TooltipString','Specify the maximum number [%] of missing values in any row or column', ...
      'Tag','h17', ...
      'Units','normalized',...
      'Value',19);
   
   h20 = uicontrol('Parent',h0, ...
      'BackgroundColor', colvec, ...
      'HorizontalAlignment', 'left', ...
      'ListboxTop',0, ...
      'Units','points', ...
      'Position',[380 110 90 80], ...
      'Style','frame', ...
      'Units','normalized', ...
      'Tag','h20');
   h19 = uicontrol('Parent',h0, ...
      'FontName','MS Sans Serif', ...
      'FontUnits','points', ...
      'FontAngle','normal', ...
      'FontWeight','normal', ...
      'FontSize',8, ...
      'FontUnits','normalized', ...
      'BackgroundColor',colvec, ...
      'HorizontalAlignment','left', ...        
      'ListboxTop',0, ...
      'Units','points', ...
      'Position',[381 130 78 59], ...
      'Units','points', ...
      'String',strINST, ...
      'Style','text', ...
      'Units','normalized', ...
      'Tag','h19');
   h18 = uicontrol('Parent',h0, ...
      'FontName','MS Sans Serif', ...
      'FontUnits','points', ...
      'FontAngle','normal', ...
      'FontWeight','normal', ...
      'FontSize',8, ...
      'FontUnits','normalized', ...
      'BackgroundColor',colvec, ...
      'ListboxTop',0, ...
      'Units','points', ...
      'Position',[381 111 78 15], ...
      'String','Overflow=missing ', ...
      'Style','checkbox', ...
      'Tag','h18', ...
      'Units','normalized',...
      'Value',1);
   
   h21 = uicontrol('Parent',h0, ...
      'BackgroundColor', colvec, ...
      'HorizontalAlignment', 'left', ...
      'ListboxTop',0, ...
      'Units','points', ...
      'Position',[380 195 90 45], ...
      'Style','frame', ...
      'Units','normalized', ...
      'Tag','h21');
   h23 = uicontrol('Parent',h0, ...
      'FontName','MS Sans Serif', ...
      'FontUnits','points', ...
      'FontAngle','normal', ...
      'FontWeight','normal', ...
      'FontSize',8, ...
      'FontUnits','normalized', ...
      'BackgroundColor',colvec, ...
      'HorizontalAlignment','left', ...        
      'Units','points', ...
      'Position',[381 226 78 13], ...
      'Units','points', ...
      'String','Use every', ...
      'Style','text', ...
      'Units','normalized', ...
      'Tag','h23');
   h22 = uicontrol('Parent',h0, ...
      'FontName','MS Sans Serif', ...
      'FontUnits','points', ...
      'FontAngle','normal', ...
      'FontWeight','normal', ...
      'FontSize',8, ...
      'FontUnits','normalized', ...
      'Units','points', ...
      'Position',[381 211 88 14], ...
      'String',strvcat('1st','2nd','3rd','4th','5th','6th','7th','8th','9th','10th'), ...
      'Style','popupmenu', ...
      'TooltipString','Extract only every nth emission wavelength', ...
      'Tag','h22', ...
      'Enable','on', ...
      'Callback','infilesca_cb lb_nth', ...
      'Units','normalized',...
      'Value',nth_init);
   h24 = uicontrol('Parent',h0, ...
      'FontName','MS Sans Serif', ...
      'FontUnits','points', ...
      'FontAngle','normal', ...
      'FontWeight','normal', ...
      'FontSize',8, ...
      'FontUnits','normalized', ...
      'BackgroundColor',colvec, ...
      'HorizontalAlignment','left', ...        
      'ListboxTop',0, ...
      'Units','points', ...
      'Position',[381 196 78 14], ...
      'Units','points', ...
      'String','emission observation.', ...
      'Style','text', ...
      'Units','normalized', ...
      'Tag','h24');
   
   close(hw);
   
   if nargout>0,
      st=[];
   end;
   
   waitfor(h0);
   
   if exist('FileNamesFile.mat','file'), 
      load('FileNamesFile.mat','Act','Ln','MaxMiss','OvrFlw','RayW1','RayW2','Src','nth');
      FileNames=Ln;
      delete('FileNamesFile.mat');
   end;
else
   close(hw);
end;
