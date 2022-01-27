; $Id: irissim_showxml.pro,v 1.3 2014/02/13 16:40:44 mawiesma Exp $  ;

pro IRISsim_showXML_event, event
  widget_control, event.top, get_UValue=info, /No_Copy
  if size(info, /type) ne 8 then begin
    main=info
    widget_control, main, get_UValue=info, /No_Copy
  endif else main=event.top
  destroyed=0
  
  case tag_names(event, /structure_name) of
    'WIDGET_TABLE_CELL_SEL': begin
      if event.sel_left ge 0 then begin
        case event.id of
        
          info.OBS_table: begin
            frmtabin=info.frmINobs[event.sel_top]
            widget_control, info.FRMtab, set_tab_current=frmtabin;change to the correct FRM
            for i=0,2 do begin
              widget_control, info.FDBtab[i], set_tab_current=(*info.fdbINfrm[frmtabin])[0,i];change to the correct FDBs
              widget_control, info.CRStab[i], set_tab_current=(*info.crsINfrm[frmtabin])[0,i];change to the correct CRSs
            endfor
          end
          
          else: begin
            tab = where(info.FRM_table eq event.id, count)
            if count gt 0 then begin
              for i=0,2 do begin
                widget_control, info.FDBtab[i], set_tab_current=(*info.fdbINfrm[tab[0]])[event.sel_top,i];change to the correct FDBs
                widget_control, info.CRStab[i], set_tab_current=(*info.crsINfrm[tab[0]])[event.sel_top,i];change to the correct CRSs
              endfor
            endif
          end
        endcase
      endif
    end
    
    'WIDGET_TAB': begin
      case event.id of
      
        info.FRMtab: begin
          for i=0,2 do begin
            widget_control, info.FDBtab[i], set_tab_current=(*info.fdbINfrm[event.tab])[0,i];change to the correct FDBs
            widget_control, info.CRStab[i], set_tab_current=(*info.crsINfrm[event.tab])[0,i];change to the correct CRSs
          endfor
        end
        
        else: BEGIN
          tab = where(info.FDBtab eq event.id, count)
          if count gt 0 then begin
            widget_control, info.CRStab[tab[0]], set_tab_current=info.crsINfdb[event.tab, tab[0]];change to the correct CRS
          endif
        end
      endcase
    end
    
    'WIDGET_BASE': begin
      if event.id eq info.CRSWindow then $
        widget_control, event.id, xsize=event.x, ysize=event.y ;resize the CRS window
    end
    
    'WIDGET_BUTTON': begin
      case event.id of
        info.CCDbutton: begin
          widget_control, event.id, get_UValue=CCDwins, /No_Copy
          CRSind3 = lonarr(3)
          wincode=0L
          for i=0,2 do begin
            CRSind3[i] = widget_info(info.CRStab[i], /tab_current)
            wincode=wincode+(CRSind3[i])*1000L^i
            CRSind3[i]=info.CRSind[CRSind3[i],i]
          endfor
          ind = where(CCDwins[*,1] eq wincode, count)
          if count gt 0 then $
            widget_control, CCDwins[ind[0], 0], /show $
          else begin
          IRISsim_showCCDregions, info.CRSList, CRSind3, info.imagefac, info.imfuv, info.imnuv, info.imsji, main, CCDWindow
          CCDwintemp = lonarr(1,2)
          CCDwintemp[0,0] = CCDwindow
          CCDwintemp[0,1] = wincode
          CCDwins = [CCDwins, CCDwintemp]
        endelse
        widget_control, event.id, set_UValue=CCDwins, /No_Copy
      end
      
      else: ;other button pressed
    endcase
  end
  
  'WIDGET_KILL_REQUEST': begin
    widget_control, info.CCDbutton, get_UValue=CCDwins, /No_Copy
    ind = where(CCDwins[*,0] eq event.id, count)
    if count gt 0 then begin
      destroyed=1
      widget_control, CCDwins[ind,0], /destroy
      CCDwins[ind,0]=-1
      CCDwins[ind,1]=-1
    endif
    widget_control, info.CCDbutton, set_UValue=CCDwins, /No_Copy
  end
  
  else: ;other event type
endcase

widget_control, main, set_UValue=info, /No_Copy
if (main ne event.top) && ~destroyed then widget_control, event.top, set_UValue=main, /No_Copy
end



pro IRISsim_showXML, OBSfilein=OBSfilein, OBSdirin=OBSdirin, copyxmlin=copyxmlin, $
    FRMdir=FRMdir, FDBdir=FDBdir, CRSdir=CRSdir, OBSrep=OBSrep
    
  constants = obj_new('IRISsim_constants')
  PZTnoChangeValue=constants->get_PZTnoChangeValue()     ; if all PZT variables in OBS have this value, the position is not changed
  
  ; INPUT
  ; copyxml=1       ;set to 1 if you want to copy the xml-files to a destination folder, which will be defined below
  ; OBSfile = 'OBS-T-00007.xml'   ; The OBS filename
  ; OBSdir = 'version20121025/'       ; OBS-file directory
  
  if ~keyword_set(OBSfilein) then begin
    ;OBSfile = 'OBS-T-00901_Q.xml'   ; The OBS filename (e.g. 'OBS-T-00004.xml')
    if keyword_set(OBSdirin) then path=OBSdirin else path=''
    file=dialog_pickfile(/read, filter=['OBS*.xml'], path=path, title='Please select an OBS file')
    if file eq '' then return
    OBSfile = file_basename(file)
    OBSdir = file_dirname(file, /mark_directory)
  endif else begin
    if ~keyword_set(OBSdirin) then begin
      ;OBSdir = 'XML_bart_121025/OBS/'       ; OBS-file directory (e.g. 'XMLfiles/')
      OBSdir = file_dirname(OBSfilein, /mark_directory)
      OBSfile = file_basename(OBSfilein)
    endif else begin
      OBSfile = OBSfilein
      OBSdir = OBSdirin
    endelse
  endelse
  if ~N_ELEMENTS(copyxmlin) then $
    copyxml=0 $      ;set to 1 if you want to copy the xml-files to the destination folder
  else copyxml=copyxmlin
  
  
  ; get current date and time to create a new folder
  if copyxml then begin
    datenum=bin_date(systime(/utc))
    datestamp=''
    for i=0,5 do begin
      if datenum[i] lt 10 then datestamp=datestamp + '0'
      datestamp = datestamp + strcompress(string(datenum[i]),/remove_all)
      if i lt 5 then datestamp = datestamp + '_'
    endfor
    datestamp = datestamp + "_dep"
    print, 'saving data in folder: ', datestamp
    spawn, "mkdir "+datestamp
  endif
  
  
  ;get data from XML-files and save the xml-files to the above created folder
  IRISsim_readtables, OBSfile, OBSdir, OBSList, FRMList, FDBList, CRSList, error, copyxml, datestamp, $
    FRMdir=FRMdir, FDBdir=FDBdir, CRSdir=CRSdir
  OBSfile = OBSdir+OBSfile
  
  ; check for errors while reading the XML-files
  if error then begin
    print,'There is something wrong with the given OBS file, exiting program'
    return
  endif
  
  if N_ELEMENTS(OBSrep) eq 1 && OBSrep gt 0 then OBSList.Repeat_Obs=OBSrep
  
  
  ;get size of displaying screen, to determine the scroll area
  WINsize=lonarr(2)
  screens=OBJ_NEW('IDLsysMonitorInfo')
  rects=screens->GetRectangles(/Exclude_Taskbar)
  ind=where(rects[0,*] eq 0, counts)
  if counts eq 0 then begin
    ;no monitor, or at least none that starts at the very left edge somehow
    WINsize[0]=rects[2,0]
    WINsize[1]=rects[3,0]
  endif else if counts eq 1 then begin
    ;exactly one monitor is at the left edge, good
    WINsize[0]=rects[2,ind[0]]
    WINsize[1]=rects[3,ind[0]]
  endif else begin
    ;more than one monitor is at the left edge, so they are above each other
    ;ind=where((rects[1,*] eq 0) && (rects[2,*] eq 0), counts) ;this doesn't work like this!
    ;if counts gt 0 then begin
    ;  WINsize[0]=rects[2,ind[0]]
    ;  WINsize[1]=rects[3,ind[0]]
    ;endif else begin
    WINsize[0]=rects[2,0]
    WINsize[1]=rects[3,0]
  ;endelse
  endelse
  
  
  ; MainWindow ; Base-Widget
  MainWindow = WIDGET_BASE(/Column, title='Observing List Table', XOffset=0, YOffset=0, MBAR=mbar)
  
  ObsBase = WIDGET_BASE(MainWindow, /Column)
  
  ; Header-information
  headerBase1 = WIDGET_BASE(ObsBase, /Row)
  ID_field = CW_FIELD(headerBase1, Title='ID', Value=OBSList.ID, /NoEdit, xsize=12)
  ;Size_field = CW_FIELD(headerBase1, Title='Size', Value=OBSList.TableSize, /NoEdit)
  Entries_field = CW_FIELD(headerBase1, Title='Entries', Value=OBSList.NumEntries, /NoEdit, xsize=3)
  ;headerBase2 = WIDGET_BASE(ObsBase, /Column)
  Rep_field = CW_FIELD(headerBase1, Title='Repeat', Value=OBSList.Repeat_Obs, /NoEdit, xsize=5)
  Cad_field = CW_FIELD(headerBase1, Title='Cadence', Value=OBSList.Cadence_Obs, /NoEdit, xsize=10)
  Tref_field = CW_FIELD(headerBase1, Title='Tref', Value=OBSList.Tref_Obs, /NoEdit, xsize=10)
  headerBase3 = WIDGET_BASE(ObsBase, /Row)
  Desc_field = CW_FIELD(headerBase3, Title='Description', Value=OBSList.Description, /NoEdit, xsize=85)
  ;Time_field = CW_FIELD(headerBase3, Title='Time', Value=OBSList.Time, /NoEdit)
  ;DataS_field = CW_FIELD(headerBase3, Title='Datasize', Value=OBSList.DataSize, /NoEdit)
  
  
  ; Data
  ;first we create a new structure for the table
  void = {ObsTable, Tref:0L, FID:'', Rep:0L, Flush:0L, InhibitSkip:0L, Tag:'', Cad:0L, $
    PZTX:0.0d, PZTY:0.0d, dPZTX:0.0d, dPZTY:0.0d}
  showData = MAKE_ARRAY(OBSList.NumEntries, Value={ObsTable})
  frmINobs = lonarr(OBSList.NumEntries)
  
  for i=0,OBSList.NumEntries-1 do begin
    showData[i].Tref = (*(OBSList).Tref_FRM)[i]
    showData[i].FID = (*(OBSList).FRM_ID)[i]
    showData[i].Rep = (*(OBSList).Repeat_FRM)[i]
    showData[i].Flush = (*(OBSList).Flush)[i]
    showData[i].InhibitSkip = (*(OBSList).InhibitSkip)[i]
    showData[i].Tag = (*(OBSList).Tag)[i]
    showData[i].Cad = (*(OBSList).Cadence_FRM)[i]
    if (*(OBSList).PZT_A_Abs)[i] eq PZTnoChangeValue || $
      (*(OBSList).PZT_B_Abs)[i] eq PZTnoChangeValue || $
      (*(OBSList).PZT_C_Abs)[i] eq PZTnoChangeValue then $
      xy = [9999, 9999] $
    else $
      xy = IRIS_pzt2xy((*(OBSList).PZT_A_Abs)[i], (*(OBSList).PZT_B_Abs)[i], (*(OBSList).PZT_C_Abs)[i])
    showData[i].PZTX = xy[0]
    showData[i].PZTY = xy[1]
    xy = IRIS_pzt2xy((*(OBSList).PZT_A_Step)[i], (*(OBSList).PZT_B_Step)[i], (*(OBSList).PZT_C_Step)[i], /NoOffset)
    showData[i].dPZTX = xy[0]
    showData[i].dPZTY = xy[1]
    ;get current FRM index
    for j=0,N_ELEMENTS(FRMList)-1 do begin
      if (*(OBSList).FRM_ID)[i] eq FRMList[j].ID then begin
        frmINobs[i] = j
        break
      endif
    endfor
    if i eq 0 then begin
      alignments = [2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2]
      formats = ['(I8)', '', '(I4)', '(I1)', '(I1)', '', '(I6)', '(f7.2)', '(f7.2)', '(f7.2)', '(f7.2)']
    endif else begin
      alignments = [[alignments], [2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2]]
      formats = [[formats], ['(I8)', '', '(I4)', '(I1)', '(I1)', '', '(I6)', '(f7.2)', '(f7.2)', '(f7.2)', '(f7.2)']]
    endelse
  endfor
  
  ColumnLabels = ['rel. Time', 'Frame ID', 'Repeats', 'FLush', 'InSkip', 'Tag', 'Cadence', $
    'OFF X', 'OFF Y', 'Step X', 'Step Y']
  widths = [70, 70, 55, 40, 40, 50, 60, 55, 55, 55, 55]
  OBS_table = WIDGET_TABLE(MainWindow, Column_Labels=ColumnLabels, /No_Row_Headers, $
    Value=showData, Column_Widths=widths, Alignment=alignments, format=formats, /All_Events)
    
  WIDGET_CONTROL, MainWindow, /realize
  
  
  MainGeometry = widget_info(MainWindow, /Geometry)
  MainHeight = MainGeometry.SCR_YSIZE + 2 * MainGeometry.MARGIN
  MainWidth = MainGeometry.SCR_XSIZE + 2 * MainGeometry.MARGIN
  
  
  
  
  
  
  if MainHeight gt WINsize[1]-80 then FRMyoffset = WINsize[1]-80 else FRMyoffset = MainHeight+20
  
  ;for currentFRM=0,N_ELEMENTS(FRMList)-1 do begin
  FRMWindow = WIDGET_BASE(/COLUMN, group_leader=MainWindow, XOffset=0, YOffset=FRMyoffset, title='Frame List Tables', $
    tlb_frame_attr=8, /TLB_KILL_REQUEST_EVENTS)
    
  FRMtab = WIDGET_TAB(FRMWindow, multiline=5)
  
  FRM_table = lonarr(N_ELEMENTS(FRMList))
  fdbINfrm = ptrarr(N_ELEMENTS(FRMList))
  crsINfrm = ptrarr(N_ELEMENTS(FRMList))
  for tab=0,N_ELEMENTS(FRMList)-1 do begin
    FRMBase = WIDGET_BASE(FRMtab, /COLUMN, title=FRMList[tab].ID)
    
    ; Header-information
    headerBase1 = WIDGET_BASE(FRMBase, /ROW)
    ID_field = CW_FIELD(headerBase1, Title='ID', Value=FRMList[tab].ID, /NoEdit, xsize=9)
    ;Size_field = CW_FIELD(headerBase1, Title='Size', Value=FRMList[tab].Tablesize, /NoEdit)
    Entries_field = CW_FIELD(headerBase1, Title='Entries', Value=FRMList[tab].NumEntries, /NoEdit, xsize=3)
    ;headerBase3 = WIDGET_BASE(FRMBase, /ROW)
    Desc_field = CW_FIELD(headerBase1, Title='Description', Value=FRMList[tab].Description, /NoEdit, xsize=77)
    ;Time_field = CW_FIELD(headerBase3, Title='Time', Value=FRMList[tab].Time, /NoEdit)
    ;DataS_field = CW_FIELD(headerBase3, Title='Datasize', Value=FRMList[tab].DataSize, /NoEdit)
    
    ; Data
    ;first we create a new structure for the table
    void = {frmtable, Tref:0L, SJI:'', NUV:'', FUV:'', $
      SJI_AEC:'', NUV_AEC:'', FUV_AEC:'', $
      Flush:0L, InhibitSkip:0L, FW:'', Focus:0L, $
      PZTX:0.0d, PZTY:0.0d}
    showData = MAKE_ARRAY(FRMList[tab].NumEntries, Value={frmtable})
    fdbINfrm[tab] = ptr_new(lonarr(FRMList[tab].NumEntries,3))
    crsINfrm[tab] = ptr_new(lonarr(FRMList[tab].NumEntries,3))
    
    for i=0,FRMList[tab].NumEntries-1 do begin
      showData[i].Tref = (*(FRMList[tab]).Tref)[i]
      if (*(FRMList[tab]).SJI_FDB_ID)[i] eq '0000000000' then showData[i].SJI='0' $
      else showData[i].SJI=(*(FRMList[tab]).SJI_FDB_ID)[i]
      if (*(FRMList[tab]).NUV_SG_FDB_ID)[i] eq '0000000000' then showData[i].NUV='0' $
      else showData[i].NUV=(*(FRMList[tab]).NUV_SG_FDB_ID)[i]
      if (*(FRMList[tab]).FUV_SG_FDB_ID)[i] eq '0000000000' then showData[i].FUV='0' $
      else showData[i].FUV=(*(FRMList[tab]).FUV_SG_FDB_ID)[i]
      showData[i].SJI_AEC = (*(FRMList[tab]).SJI_AEC)[i]
      showData[i].NUV_AEC = (*(FRMList[tab]).NUV_AEC)[i]
      showData[i].FUV_AEC = (*(FRMList[tab]).FUV_AEC)[i]
      showData[i].Flush = (*(FRMList[tab]).Flush)[i]
      showData[i].InhibitSkip = (*(FRMList[tab]).InhibitSkip)[i]
      showData[i].FW = constants->get_FWname((*(FRMList[tab]).FW)[i])
      showData[i].Focus = (*(FRMList[tab]).FOCUS)[i]
      xy = IRIS_pzt2xy((*(FRMList[tab]).PZT_A_Rel)[i], (*(FRMList[tab]).PZT_B_Rel)[i], (*(FRMList[tab]).PZT_C_Rel)[i], /NoOffset)
      showData[i].PZTX = xy[0]
      showData[i].PZTY = xy[1]
      
      ;get current FDB
      currentFDB=[-1, -1, -1] ;[sji, nuv, fuv1, fuv2]
      for j=0,N_ELEMENTS(FDBList)-1 do begin
        if (*(FRMList[tab]).SJI_FDB_ID)[i] eq FDBList[j].ID then currentFDB[0] = j
        if (*(FRMList[tab]).NUV_SG_FDB_ID)[i] eq FDBList[j].ID then currentFDB[1] = j
        if (*(FRMList[tab]).FUV_SG_FDB_ID)[i] eq FDBList[j].ID then currentFDB[2] = j
      endfor
      ;get current CRS indices
      currentCRS=[-1, -1, -1] ;[sji, nuv, fuv1, fuv2]
      for k=0,N_ELEMENTS(CRSList)-1 do begin
        for j=0,2 do begin
          if currentFDB[j] ge 0 then $
            if FDBList[currentFDB[j]].CRSID eq CRSList[k].ID then currentCRS[j] = k
        endfor
      endfor
      (*fdbINfrm[tab])[i,*] = currentFDB
      (*crsINfrm[tab])[i,*] = currentCRS
      
      if i eq 0 then begin
        alignments = [2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2]
        formats = ['(I8)', '', '', '', '', '', '', '(I1)', '(I1)', '', '(I5)', '(f6.2)', '(f6.2)']
      endif else begin
        alignments = [[alignments], [2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2]]
        formats = [[formats], ['(I8)', '', '', '', '', '', '', '(I1)', '(I1)', '', '(I5)', '(f6.2)', '(f6.2)']]
      endelse
    endfor
    ColumnLabels = ['rel. Time', 'SJI ID', 'NUV ID', 'FUV ID', $
      'SJI_AEC', 'NUV_AEC', 'FUV_AEC', $
      'Flush', 'InSkip', 'FW', 'Focus', $
      'OFF X', 'Off Y']
    widths = [70, 70, 70, 70, 55, 55, 55, 40, 40, 60, 50, 55, 55]
    FRM_table[tab] = WIDGET_TABLE(FRMBase, Column_Labels=ColumnLabels, /No_Row_Headers, $
      Value=showData, Column_Widths=widths, Alignment=alignments, format=formats, /All_Events)
      
  endfor ;FRMList
  
  widget_control, FRMWindow, /realize
  widget_control, FRMtab, set_tab_current=frmINobs[0] ;change to the correct FRM
  xmanager, 'IRISsim_showXML', FRMWindow, /just_reg, /no_block
  
  FRMGeometry = widget_info(FRMWindow, /Geometry)
  ;FRMHeight = FRMGeometry.SCR_YSIZE + 2 * FRMGeometry.MARGIN
  FRMWidth = FRMGeometry.SCR_XSIZE + 2 * FRMGeometry.MARGIN
  
  
  
  ;calculate number of tabs in FDB and CRS windows, and the corresponding indices
  nrtabsfdb=lonarr(3)
  nrtabscrs=lonarr(3)
  FDBind=lonarr(N_ELEMENTS(FDBList),3)
  FDBind[*]=-2
  CRSind=lonarr(N_ELEMENTS(CRSList),3)
  CRSind[*]=-2
  crsINfdb=lonarr(N_ELEMENTS(FDBList),3)
  for tab=0,N_ELEMENTS(FRMList)-1 do begin
    for i=0,FRMList[tab].NumEntries-1 do begin
      for k=0,2 do begin
        ind = where(FDBind[*,k] eq (*fdbINfrm[tab])[i,k], count)
        if count eq 0 then begin
          FDBind[nrtabsfdb[k],k] = (*fdbINfrm[tab])[i,k]
          (*fdbINfrm[tab])[i,k] = nrtabsfdb[k]
          nrtabsfdb[k]=nrtabsfdb[k]+1
        endif else (*fdbINfrm[tab])[i,k] = ind
        
        ind = where(CRSind[*,k] eq (*crsINfrm[tab])[i,k], count)
        if count eq 0 then begin
          CRSind[nrtabscrs[k],k] = (*crsINfrm[tab])[i,k]
          (*crsINfrm[tab])[i,k] = nrtabscrs[k]
          nrtabscrs[k]=nrtabscrs[k]+1
        endif else (*crsINfrm[tab])[i,k] = ind
        
        crsINfdb[(*fdbINfrm[tab])[i,k],k] = (*crsINfrm[tab])[i,k]
      endfor
    endfor
  endfor
  
  
  
  
  FDBxoffset = max([MainWidth, FRMWidth])
  if FDBxoffset gt WINsize[0]-850 then FDBxoffset=Mainwidth
  if FDBxoffset gt WINsize[0]-150 then FDBxoffset = WINsize[0]-150
  
  FDBWindow = WIDGET_BASE(/COLUMN, group_leader=MainWindow, XOffset=FDBxoffset, YOffset=0, title='Frame Definition Block Tables', $
    tlb_frame_attr=8, /TLB_KILL_REQUEST_EVENTS)
  FDBtab = lonarr(3)
  FDB_table = lonarr(max(nrtabsfdb),3)
  
  for k=0,2 do begin
    case k of
      0: Title_Label = WIDGET_LABEL(FDBWindow, Value='FDB for Slitjaw Images', Frame=2)
      1: Title_Label = WIDGET_LABEL(FDBWindow, Value='FDB for NUV spectrum', Frame=2)
      2: Title_Label = WIDGET_LABEL(FDBWindow, Value='FDB for FUV spectrum', Frame=2)
    endcase
    FDBtab[k] = WIDGET_TAB(FDBWindow, multiline=5)
    for j=0,nrtabsfdb[k]-1 do begin
      if FDBind[j,k] ge 0 then begin
        FDBBase = WIDGET_BASE(FDBtab[k], /COLUMN, title=FDBList[FDBind[j,k]].ID)
        
        ; Header-information
        headerBase1 = WIDGET_BASE(FDBBase, /ROW)
        ID_field = CW_FIELD(headerBase1, Title='ID', Value=FDBList[FDBind[j,k]].ID, /NoEdit, xsize=9)
        ;Size_field = CW_FIELD(headerBase1, Title='Size', Value=FDBList[FDBind[j,k]].Tablesize, /NoEdit)
        ;headerBase3 = WIDGET_BASE(FDBBase, /ROW)
        Desc_field = CW_FIELD(headerBase1, Title='Description', Value=FDBList[FDBind[j,k]].Description, /NoEdit, xsize=41)
      ;Time_field = CW_FIELD(headerBase3, Title='Time', Value=FDBList[FDBind[j,k]].Time, /NoEdit)
      ;DataS_field = CW_FIELD(headerBase3, Title='Datasize', Value=FDBList[FDBind[j,k]].DataSize, /NoEdit)
        
      endif else begin
        FDBBase = WIDGET_BASE(FDBtab[k], /COLUMN, title='none')
        headerBase1 = WIDGET_BASE(FDBBase, /ROW)
        ID_field = CW_FIELD(headerBase1, Title='ID', Value='', /NoEdit, xsize=9)
        Desc_field = CW_FIELD(headerBase1, Title='Description', Value='', /NoEdit, xsize=41)
      endelse
      
      ; Data
      ;first we create a new structure for the table
      void = {fdbtable, CRSID:'', Exp_Duration:0L, Exp_Type:'', CompN:0L, CompK:0L, $
        LookupTableID:'', Exp_Max:0L, Exp_Min:0L}
      showData = MAKE_ARRAY(1, Value={fdbtable})
      
      if FDBind[j,k] ge 0 then begin
        for i=0,0 do begin
          showData[i].CRSID = FDBList[FDBind[j,k]].CRSID[i]
          showData[i].Exp_Duration = FDBList[FDBind[j,k]].Exp_Duration[i]
          showData[i].Exp_Type = constants->get_ExpTypeName(FDBList[FDBind[j,k]].Exp_Type[i])
          showData[i].CompN = FDBList[FDBind[j,k]].CompN[i]
          showData[i].CompK = FDBList[FDBind[j,k]].CompK[i]
          showData[i].LookupTableID = FDBList[FDBind[j,k]].LookupTableID[i]
          showData[i].Exp_Max = FDBList[FDBind[j,k]].Exp_Max[i]
          showData[i].Exp_Min = FDBList[FDBind[j,k]].Exp_Min[i]
          if i eq 0 then begin
            alignments = [2, 2, 2, 2, 2, 2, 2, 2]
            formats = ['', '(I6)', '', '(I6)', '(I6)', '', '(I6)', '(I6)']
          endif else begin
            alignments = [[alignments], [2, 2, 2, 2, 2, 2, 2, 2]]
            formats = [[formats], ['', '(I6)', '', '(I6)', '(I6)', '', '(I6)', '(I6)']]
          endelse
        endfor
      endif
      ColumnLabels = ['CRS ID', 'Exp_Dur', 'Exp_Type', 'CompN', 'CompK', 'LUT', 'Exp_Max', 'Exp_Min']
      widths = [70, 54, 54, 50, 50, 50, 54, 54]
      FDB_table[j,k] = WIDGET_TABLE(FDBBase, Column_Labels=ColumnLabels, /No_Row_Headers, $
        Value=showData, Column_Widths=widths, Alignment=alignments, format=formats)
    endfor
  endfor
  
  obj_destroy, constants
  
  widget_control, FDBWindow, /realize
  for i=0,2 do begin
    widget_control, FDBtab[i], set_tab_current=(*fdbINfrm[frmINobs[0]])[0,i];change to the correct FDBs
  endfor
  xmanager, 'IRISsim_showXML', FDBWindow, /just_reg, /no_block
  
  FDBGeometry = widget_info(FDBWindow, /Geometry)
  FDBHeight = FDBGeometry.SCR_YSIZE + 2 * FDBGeometry.MARGIN
  FDBWidth = FDBGeometry.SCR_XSIZE + 2 * FDBGeometry.MARGIN
  
  
  if FDBxoffset+FDBWidth gt WINsize[0]-300 then CRSxoffset = WINsize[0]-300 else CRSxoffset = FDBxoffset+FDBWidth
  
  for crsdisp=0,1 do begin
  
    if crsdisp eq 0 then begin
      CRSWindow = WIDGET_BASE(/COLUMN, group_leader=MainWindow, XOffset=CRSxoffset, YOffset=0, title='CCD Readout Spec Tables', $
        tlb_frame_attr=8, /TLB_KILL_REQUEST_EVENTS)
    endif else if needscroll then begin
      CRSWindow = WIDGET_BASE(/COLUMN, group_leader=MainWindow, XOffset=CRSxoffset, YOffset=0, title='CCD Readout Spec Tables' $
        ,/scroll, x_scroll_size=CRSwidth, y_scroll_size=CRSheight, /TLB_SIZE_EVENTS, tlb_frame_attr=8, /TLB_KILL_REQUEST_EVENTS)
    endif else $
      CRSWindow = WIDGET_BASE(/COLUMN, group_leader=MainWindow, XOffset=CRSxoffset, YOffset=0, title='CCD Readout Spec Tables', $
      tlb_frame_attr=8, /TLB_KILL_REQUEST_EVENTS)
      
    CRSheight=0
    CRSwidth=0
    
    CCDbutton = WIDGET_BUTTON(CRSWindow, value='Show regions on CCD')
    geo=widget_info(CCDbutton, /Geometry)
    CRSheight=CRSheight+geo.scr_ysize+2*geo.margin
    CCDwins = lonarr(1,2)
    CCDwins[0,0]=-1
    CCDwins[0,1]=-1
    widget_control, CCDbutton, set_UValue=CCDwins, /No_Copy
    
    
    CRStab = lonarr(3)
    CRS_table = lonarr(max(nrtabscrs),3)
    for k=0,2 do begin
      case k of
        0: Title_Label = WIDGET_LABEL(CRSWindow, Value='CRS for Slitjaw Images', Frame=2)
        1: Title_Label = WIDGET_LABEL(CRSWindow, Value='CRS for NUV spectrum', Frame=2)
        2: Title_Label = WIDGET_LABEL(CRSWindow, Value='CRS for FUV spectrum', Frame=2)
      endcase
      geo=widget_info(Title_Label, /Geometry)
      CRSheight=CRSheight+geo.scr_ysize+2*geo.margin
      CRStab[k] = WIDGET_TAB(CRSWindow, multiline=5)
      for j=0,nrtabscrs[k]-1 do begin
        if CRSind[j,k] ge 0 then begin
          CRSBase = WIDGET_BASE(CRStab[k],/COLUMN, title=CRSList[CRSind[j,k]].ID)
          
          ; Header-information
          headerBase1 = WIDGET_BASE(CRSBase, /ROW)
          ID_field = CW_FIELD(headerBase1, Title='ID', Value=CRSList[CRSind[j,k]].ID, /NoEdit, xsize=9)
          ;Size_field = CW_FIELD(headerBase1, Title='Size', Value=CRSList[CRSind[j,k]].Tablesize, /NoEdit)
          Entries_field = CW_FIELD(headerBase1, Title='Subreg', Value=CRSList[CRSind[j,k]].SubRegions, /NoEdit, xsize=2)
          ;headerBase2 = WIDGET_BASE(CRSBase, /ROW)
          Spectral_field = CW_FIELD(headerBase1, Title='Spec', Value=CRSList[CRSind[j,k]].Spectral, /NoEdit, xsize=2)
          Spatial_field = CW_FIELD(headerBase1, Title='Spat', Value=CRSList[CRSind[j,k]].Spatial, /NoEdit, xsize=2)
          headerBase3 = WIDGET_BASE(CRSBase, /ROW)
          Desc_field = CW_FIELD(headerBase3, Title='Desc', Value=CRSList[CRSind[j,k]].Description, /NoEdit, xsize=44)
        ;Time_field = CW_FIELD(headerBase3, Title='Time', Value=CRSList[CRSind[j,k]].Time, /NoEdit)
        ;DataS_field = CW_FIELD(headerBase3, Title='Datasize', Value=CRSList[CRSind[j,k]].DataSize, /NoEdit)
        endif else begin
          CRSBase = WIDGET_BASE(CRStab[k],/COLUMN, title='none')
          headerBase1 = WIDGET_BASE(CRSBase, /ROW)
          ID_field = CW_FIELD(headerBase1, Title='ID', Value='', /NoEdit, xsize=9)
          ;Size_field = CW_FIELD(headerBase1, Title='Size', Value=0, /NoEdit)
          Entries_field = CW_FIELD(headerBase1, Title='Subreg', Value=0, /NoEdit, xsize=2)
          ;headerBase2 = WIDGET_BASE(CRSBase, /ROW)
          Spectral_field = CW_FIELD(headerBase1, Title='Spec', Value=0, /NoEdit, xsize=2)
          Spatial_field = CW_FIELD(headerBase1, Title='Spat', Value=0, /NoEdit, xsize=2)
          headerBase3 = WIDGET_BASE(CRSBase, /ROW)
          Desc_field = CW_FIELD(headerBase3, Title='Desc', Value='', /NoEdit, xsize=44)
        endelse
        
        ; Data
        ;first we create a new structure for the table
        void = {crstable, SubRegionID:0L, StartRow:0L, NumRows:0L, StartCol:0L, NumCols:0L}
        if CRSind[j,k] ge 0 then begin
          showData = MAKE_ARRAY(CRSList[CRSind[j,k]].SubRegions, Value={crstable})
          for i=0,CRSList[CRSind[j,k]].SubRegions-1 do begin
            showData[i].SubRegionID = (*(CRSList[CRSind[j,k]]).SubRegionID)[i]
            showData[i].StartRow = (*(CRSList[CRSind[j,k]]).StartRow)[i]
            showData[i].NumRows = (*(CRSList[CRSind[j,k]]).NumRows)[i]+(*(CRSList[CRSind[j,k]]).StartRow)[i]-1
            showData[i].StartCol = (*(CRSList[CRSind[j,k]]).StartCol)[i]
            showData[i].NumCols = (*(CRSList[CRSind[j,k]]).NumCols)[i]+(*(CRSList[CRSind[j,k]]).StartCol)[i]-1
            if i eq 0 then begin
              alignments = [2, 2, 2, 2, 2]
              formats = ['(I1)', '(I4)', '(I4)', '(I4)', '(I4)']
            endif else begin
              alignments = [[alignments], [2, 2, 2, 2, 2]]
              formats = [[formats], ['(I1)', '(I4)', '(I4)', '(I4)', '(I4)']]
            endelse
          endfor
        endif else showData = MAKE_ARRAY(1, Value={crstable})
        ColumnLabels = ['SubRegion', 'StartRow', 'EndRow', 'StartCol', 'EndCol']
        widths = [58, 58, 58, 58, 58]
        Data_table = WIDGET_TABLE(CRSBase, Column_Labels=ColumnLabels, /No_Row_Headers, $
          Value=showData, Column_Widths=widths, Alignment=alignments, format=formats)
          
      endfor
      geo=widget_info(CRStab[k], /Geometry)
      CRSheight=CRSheight+geo.scr_ysize+2*geo.margin
      CRSwidth=geo.scr_xsize+2*geo.margin
    endfor
    
    if crsdisp eq 0 then begin
      ;resize window according to content and screen size
      dosecondrun=0
      needscroll=0
      CRSheight=CRSheight+40
      if CRSheight gt WINsize[1]-50 then begin
        CRSheight=WINsize[1]-50
        if CRSheight lt 50 then CRSheight=50
        dosecondrun=1
        needscroll=1
      endif
      CRSwidth=CRSwidth+40
      if CRSwidth gt WINsize[0]-50-CRSxoffset then begin
        CRSxoffset = WINSize[0]-50-CRSwidth
        if CRSxoffset lt 0 then begin
          CRSxoffset=0
          CRSwidth=WINsize[0]-50
          if CRSwidth lt 100 then CRSwidth=100
          needscroll=1
        endif
        dosecondrun=1
      endif
      
      if dosecondrun then $ ;a strange way to create a window with the right size, and right scroll area, is there a better way?
        widget_control, CRSWindow, /destroy $
      else break
      
    endif
    
  endfor ;do CRSWindow twice it the screen is too small
  
  widget_control, CRSWindow, /realize
  for i=0,2 do begin
    widget_control, CRStab[i], set_tab_current=(*crsINfrm[frmINobs[0]])[0,i];change to the correct CRSs
  endfor
  xmanager, 'IRISsim_showXML', CRSWindow, /just_reg, /no_block
  
  
  
  
  
  ;prepare images to show the regions on the CCD
  filefuv = programrootdir()+'Documents/images/fuv_2072x548_jun2012.png'
  filenuv = programrootdir()+'Documents/images/nuv_1036x548.png'
  filesji = programrootdir()+'Documents/images/sji_1036x548_slitline.png'
  
  imfuv = read_image(filefuv)
  imnuv = read_image(filenuv)
  imsji = read_image(filesji)
  
  ;reize if screen is small
  if WINsize[0] lt 1200 then begin
    imagefac = WINsize[0]/2100.0
  endif else imagefac=1200/2100.0
  imfuv = congrid(imfuv, 4, 2072*imagefac, 548*imagefac)
  imnuv = congrid(imnuv, 4, 1036*imagefac, 548*imagefac)
  imsji = congrid(imsji, 4, 1036*imagefac, 548*imagefac)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ;define structure with information to be passed around
  info = { $
    OBSfile:OBSfile, $
    ;OBSList:OBSList, $
    ;FRMList:FRMList, $
    ;FDBList:FDBList, $
    CRSList:CRSList, $
    OBS_table:OBS_table, $
    frmINobs:frmINobs, $
    FRMtab:FRMtab, $
    FRM_table:FRM_table, $
    fdbINfrm:fdbINfrm, $
    FDBtab:FDBtab, $
    crsINfrm:crsINfrm, $
    crsINfdb:crsINfdb, $
    CRSWindow:CRSWindow, $
    CRStab:CRStab, $
    CRSind:CRSind, $
    CCDbutton:CCDbutton, $
    imagefac:imagefac, $
    imfuv:imfuv, $
    imnuv:imnuv, $
    imsji:imsji $
    ;loadbutton:loadbutton, $
    ;savebutton:savebutton, $
    ;startbutton:startbutton $
    }
    
  ;set this structure as the user-defined value of mainwindow, and give the other windows access to mainwindow
  widget_control, MainWindow, set_UValue=info, /No_Copy
  widget_control, FRMWindow, set_UValue=MainWindow
  widget_control, FDBWindow, set_UValue=MainWindow
  widget_control, CRSWindow, set_UValue=MainWindow
  
  xmanager, 'IRISsim_showXML', MainWindow, /no_block
end
