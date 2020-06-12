function st=InFilesCA_cb(cmd);

%$ infilesca_cb $ Version 1.41 $ Claus A. Andersson (claus@andersson.dk) $ 11-Oct-1999 $ Copyright 1999 - $
%
% Programmed by Claus A. Andersson, Copyrighted 1999 - 
% E-mail: claus@andersson.dk
% This file and the code in it belongs to the holder of the
% copyrights and is made public under the following constraints:
% It must not be changed or modified and code cannot be added.
% It cannot be annoted by other authors and this file must be
% regarded as read-only. In case of doubt, contact
% the holder of the copyrights.

UData=1;
strLS50B='';
strTIDAS='';
nth_init=0;
load([tempdir 'temp.mat']);

switch(cmd)
case 'sort1',
    pHandle=findobj(gcbf,'Tag','h1');
    List=get(pHandle,'String');
    Str=List(:,1:UData(1));
    [I,S]=sortrows(Str);
    S=S(:,1);
    NewList=List(S,:);
    set(pHandle,'String',NewList);
    Sel=get(pHandle,'Value');
    NewSel=S(Sel);
    set(pHandle,'Value',NewSel);
case 'sort2',
    pHandle=findobj(gcbf,'Tag','h1');
    List=get(pHandle,'String');
    Str=List(:,UData(1)+1:UData(2));
    [I,S]=sortrows(Str);
    S=S(:,1);
    NewList=List(S,:);
    set(pHandle,'String',NewList);
    Sel=get(pHandle,'Value');
    NewSel=S(Sel);
    set(pHandle,'Value',NewSel);
case 'sort3',
    pHandle=findobj(gcbf,'Tag','h1');
    List=get(pHandle,'String');
    Str=str2num(List(:,UData(2)+1:end));
    [I,S]=sortrows(Str);
    S=S(:,1);
    NewList=List(S,:);
    set(pHandle,'String',NewList);
    Sel=get(pHandle,'Value');
    NewSel=S(Sel);
    set(pHandle,'Value',NewSel);
case 'lb_src',
    pHandle=findobj(gcbf,'Tag','h7');
    Sels=get(pHandle,'Value');
    ActList=[];
    if Sels==1,
        ActList=strvcat(ActList,'Landscape with Rayleigh signals set to NaN');
        ActList=strvcat(ActList,'Unfolded three-way structure with Rayleigh signals set to NaN');
        ActList=strvcat(ActList,'Matrix (no parameter control)');
        pHandle=findobj(gcbf,'Tag','h19');
        set(pHandle,'String',strLS50B);
    elseif Sels==2,
        ActList=strvcat(ActList,'Landscape with Rayleigh signals set to NaN');
        ActList=strvcat(ActList,'Unfolded three-way structure with Rayleigh signals set to NaN');
        pHandle=findobj(gcbf,'Tag','h19');
        set(pHandle,'String',strTIDAS);
    end;
    pHandle=findobj(gcbf,'Tag','h8');
    set(pHandle,'String',ActList);
    set(pHandle,'Value',1);
case 'lb_act',
    pHandle=findobj(gcbf,'Tag','h8');
    Sels=get(pHandle,'Value');
    if Sels==3,
        pHandle=findobj(gcbf,'Tag','h9');
        set(pHandle,'Enable','inactive');
        set(pHandle,'Value',1);
        pHandle=findobj(gcbf,'Tag','h15');
        set(pHandle,'Enable','inactive');
        set(pHandle,'Value',1);
        pHandle=findobj(gcbf,'Tag','h17');
        set(pHandle,'Enable','inactive');
        set(pHandle,'Value',21);
    else
        pHandle=findobj(gcbf,'Tag','h9');
        set(pHandle,'Enable','on');
        pHandle=findobj(gcbf,'Tag','h15');
        set(pHandle,'Enable','on');
        pHandle=findobj(gcbf,'Tag','h17');
        set(pHandle,'Enable','on');
    end;
case 'setRayW1',
    pHandle=findobj(gcbf,'Tag','h9');
    Sels=get(pHandle,'Value');
    set(pHandle,'Value',Sels);
case 'setRayW2',
    pHandle=findobj(gcbf,'Tag','h15');
    Sels=get(pHandle,'Value');
    set(pHandle,'Value',Sels);
case 'selall',
    pHandle=findobj(gcbf,'Tag','h1');
    StrTmp=get(pHandle,'String');
    n=size(StrTmp,1);
    set(pHandle,'Value',1:n);
case 'lb_nth',
    pHandle=findobj(gcbf,'Tag','h22');
    Sels=get(pHandle,'Value');
    set(pHandle,'Value',Sels);
case 'lb_nth_init',
    pHandle=findobj(gcbf,'Tag','h22');
    set(pHandle,'Value',nth_init);    
case 'exit',
    pHandle=findobj(gcbf,'Tag','h1');
    List=get(pHandle,'String');
    Sels=get(pHandle,'Value');
    Ln=List(Sels,1:UData(1));
    pHandle=findobj(gcbf,'Tag','h7');
    Src=get(pHandle,'Value');
    pHandle=findobj(gcbf,'Tag','h8');
    Act=get(pHandle,'Value');
    pHandle=findobj(gcbf,'Tag','h9');
    RayW1=get(pHandle,'Value');
    RayW1=RayW1-1;
    pHandle=findobj(gcbf,'Tag','h15');
    RayW2=get(pHandle,'Value');
    RayW2=RayW2-1;
    pHandle=findobj(gcbf,'Tag','h17');
    MaxMiss=(get(pHandle,'Value')-1)*5;    
    pHandle=findobj(gcbf,'Tag','h18');
    OvrFlw=get(pHandle,'Value');    
    pHandle=findobj(gcbf,'Tag','h22');
    nth=get(pHandle,'Value');
    save('FileNamesFile.mat','Ln','Src','Act','RayW1','RayW2','MaxMiss','OvrFlw','nth');
    close(gcbf)
case 'cancel',
    Ln=[];
    Src=0;
    Act=0;
    RayW1=0;
    RayW2=0;
    MaxMiss=0;
    OvrFlw=0;
    nth=0;
    save('FileNamesFile.mat','Ln','Src','Act','RayW1','RayW2','MaxMiss','OvrFlw','nth');
    close(gcbf)
end