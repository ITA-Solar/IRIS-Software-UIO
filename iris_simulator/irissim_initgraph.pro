pro IRISsim_InitGraph, OBSList, FRMList, FDBList, CRSList, $
    FRMinOBS, FDBinFRM, CRSinFDB, $
    SJIframe, PZTnoChangeValue, PixCCDxfuv1, PixCCDxfuvSJI, $
    SpecData, SJIdata, rasterSJI, WINsize, PosClock, step, xmax=xmax, ymax=ymax, debug=debug
    
  ; $Id: irissim_initgraph.pro,v 1.10 2013/09/04 15:25:38 mawiesma Exp $  ;
    
  ;y-direction
  ytop=5 ;top margin
  ybottom=6 ;bottom margin
  ydist=8 ;distance between plots in y-direction
  
  ;x-direction
  xdist=7 ;distance between plots in x-direction (when there are no axis in between)
  xaxisL=12 ;margin for left axis
  xaxisR=8 ;margin for right axis
  xaxisRextra=2 ;additional margin on the very right side
  
  ;SJI
  minSizeSJI=300 ;minimum size of a single SJI (larger axis)
  maxSizeSJI=2100 ;maximum size of a single SJI (larger axis)
  maxfacy=1.2 ;factor by which the overview SJI can be larger in y-direction, compared to other SJI
  maxfacx=2 ;factor by which the overview SJI can be larger in x-direction
  
  
  
  ;structures for each spectral window
  specstruct = {SpectralData, $
    name:'', $
    name2:'', $
    sum_spat:1, $
    sum_spec:1, $
    Exp_Duration:0, $; see comment for FDBindex (in syntool, will be taken directly from FDB)
    Exp_Type:0, $;TODO (value is in the structure, but ignored), see comment for FDBindex
    CRSindex:0, $
    FDBindex:0, $;could be more than 1
    SubRegionID:0, $
    type:'', $
    WINcoords:dblarr(4), $
    SIMcoords:intarr(6), $
    nrticksx:0, $
    nrticksy:0, $
    PZTx:0d, $
    PZTy:0d, $
    data:ptr_new(), $
    min_show:0d, $
    max_show:0d, $
    axisL:0, $
    axisR:0}
    
  ;structures for each SJI window
  sjistruct = {SJIData, $
    name:'', $
    name2:'', $
    sum_spatX:1, $
    sum_spatY:1, $
    Exp_Duration:0, $; see comment for FDBindex (in syntool, will be taken directly from FDB)
    Exp_Type:0, $;TODO (value is in the structure, but ignored), see comment for FDBindex
    CRSindex:0, $
    FDBindex:0, $;could be more than 1
    SubRegionID:0, $
    type:'', $;might change, according to FW in FRM
    WINcoords:dblarr(4), $
    Boxcoords:intarr(6), $
    Showcoords:intarr(6), $
    nrticksx:0, $
    nrticksy:0, $
    PZTx:0d, $
    PZTy:0d, $
    data:ptr_new(), $
    min_show:0d, $
    max_show:0d, $
    min_showFW:dblarr(7), $
    max_showFW:dblarr(7), $
    axisL:0, $
    axisR:0}
    
    
    
  ;for debugging:
  if ~N_ELEMENTS(debug) then begin
    debug=0         ;set to 1 to control directly, how many pictures will be calculated
    nrpics=1        ;set the number of pictures to be calculated in debug-mode (program stops when this number is reached)
  endif else nrpics=debug
  
  instrument = obj_new('IRISsim_instrument', OBSList, FRMList, FDBList, CRSList, '', $
    FRMinOBS, FDBinFRM, CRSinFDB, $
    0, 0, '', 0, '', '', $
    '', '', $
    '')
    
  ; get min and max position of left lower corner
  ;i.e. run through whole experiment and get all PZT values
  minX = [1000, 1000]
  maxX = [0, 0]
  step = 0
  for nrOBSrepeats=0,OBSList.Repeat_Obs-1 do begin ; loop over OBS repeats
    instrument->newOBSrepeat, nrOBSrepeats
    for nrFRM=0,OBSList.NumEntries-1 do begin ;loop over whole OBS, different FRMs
      instrument->newFRM, nrFRM, currentFRM
      ;if currentFRM eq -1 then return
      for nrFRMrepeats=0,(*(OBSList).Repeat_FRM)[nrFRM]-1 do begin ; loop over FRM repeats
        instrument->newFRMrepeat, nrFRMrepeats
        for nrFDB=0,FRMList[currentFRM].NumEntries-1 do begin ; loop over current FRM, different entries
          instrument->newFDB, nrFDB, 0, step, sim1, sim2, sim3, sim4, $
            PZTcurrent, filterwheel, currentFDB, currentCRS, error, /init
          ;if error then return
          PZTcurrent = round(6d*PZTcurrent)
          for i=0,1 do begin
            if PZTcurrent[i] lt minX[i] then minX[i] = PZTcurrent[i]
            if PZTcurrent[i] gt maxX[i] then maxX[i] = PZTcurrent[i]
          endfor
          ;print, pZTcurrent, minX, maxX
          if ~debug || (step lt nrpics) then step = step + 1
        endfor ;nrFDB=0,FRMList[useFRM].NumEntries-1
      endfor ;nrFRMrepeats=0,(*(OBSList).Repeat_FRM)[nrFRM]-1
    endfor ;nrFRM=0,OBSList.NumEntries-1
  endfor ;nrOBSrepeats=0,OBSList.Repeat_Obs-1
  ;print, minX, maxX
  obj_destroy, instrument
  
  
  ;get number of windows
  sji=0
  nuv=0
  fu1=0
  ;fu2=0
  for nrCRS = 0,N_ELEMENTS(CRSList)-1 do begin
    case CRSList[nrCRS].Type of
      'sji':sji = sji + CRSList[nrCRS].Subregions
      'nuv':nuv = nuv + CRSList[nrCRS].Subregions
      'fu1':fu1 = fu1 + CRSList[nrCRS].Subregions
      ;'fu2':fu2 = fu2 + CRSList[nrCRS].Subregions ; doesn't exist anymore
      else: box_message,'unknown window type'
    endcase
  endfor
  
  nWindows = nuv + fu1
  if nWindows gt 0 then begin
  
    SpecData = MAKE_ARRAY(nWindows, Value=specstruct) ; structure for spectral data
    
    ;populate spectral data structure
    n=0
    constants = obj_new('IRISsim_constants')
    for nrCRS = 0,N_ELEMENTS(CRSList)-1 do begin
      if (CRSList[nrCRS].Type eq 'fu1') || (CRSList[nrCRS].Type eq 'nuv') then begin
        ;descr = CRSList[nrCRS].Description
        ;windescr = strsplit(descr,',',/extract, count=nWindescr)
        ;if nWindescr ne CRSList[nrCRS].Subregions then $
        ;  print, 'number of subregions and number of elements in description do not fit (not critical), CRS-file: ', CRSList[nrCRS].ID
      
        for nrFDB=0,N_ELEMENTS(FDBList)-1 do begin
          if FDBList[nrFDB].CRSID eq CRSList[nrCRS].ID then begin
            inFDB=nrFDB
            break
          endif
        endfor
        
        for i=0,CRSList[nrCRS].Subregions-1 do begin
          ;if i gt nWindescr-1 then SpecData[n].name = '???' $
          ;else SpecData[n].name = windescr[i]
          SpecData[n].CRSindex = nrCRS
          SpecData[n].sum_spat = CRSList[nrCRS].Spatial
          SpecData[n].sum_spec = CRSList[nrCRS].Spectral
          if (SpecData[n].sum_spat gt 1) || (SpecData[n].sum_spec gt 1) then $
            SpecData[n].name2 = string(SpecData[n].sum_spec,format='(I1)') + 'x' + string(SpecData[n].sum_spat,format='(I1)')
          SpecData[n].FDBindex = inFDB ;useless here
          SpecData[n].Exp_Duration = FDBList[inFDB].Exp_Duration ;useless here
          SpecData[n].Exp_Type = FDBList[inFDB].Exp_Type ;useless here
          SpecData[n].SubRegionID = (*(CRSList[nrCRS]).SubRegionID)[i]
          if (CRSList[nrCRS].Type eq 'fu1') && ((*(CRSList[nrCRS]).StartRow)[i] ge PixCCDxfuv1) then $
            SpecData[n].type = 'fu2' $
          else SpecData[n].type = CRSList[nrCRS].Type
          if CRSList[nrCRS].Type eq 'fu1' then $
            coords = IRISsim_flipcoords( (*(CRSList[nrCRS]).StartCol)[i], (*(CRSList[nrCRS]).EndCol)[i], $
            (*(CRSList[nrCRS]).StartRow)[i], (*(CRSList[nrCRS]).EndRow)[i], 1, 1, /fuv, /startatzero ) $
          else $
            coords = IRISsim_flipcoords( (*(CRSList[nrCRS]).StartCol)[i], (*(CRSList[nrCRS]).EndCol)[i], $
            (*(CRSList[nrCRS]).StartRow)[i], (*(CRSList[nrCRS]).EndRow)[i], 1, 1, /nuv, /startatzero )
            
          SpecData[n].SIMcoords[0] = coords.tsc
          SpecData[n].SIMcoords[1] = coords.tec - coords.tsc + 1
          SpecData[n].SIMcoords[2] = coords.tsr
          SpecData[n].SIMcoords[3] = coords.ter - coords.tsr + 1
          SpecData[n].SIMcoords[4] = coords.tec
          SpecData[n].SIMcoords[5] = coords.ter
          ;          SpecData[n].SIMcoords[0] = (*(CRSList[nrCRS]).StartRow)[i]-1 ;in Ankurs tool it starts at 1
          ;          SpecData[n].SIMcoords[1] = (*(CRSList[nrCRS]).NumRows)[i]
          ;          SpecData[n].SIMcoords[2] = (*(CRSList[nrCRS]).StartCol)[i]-1
          ;          SpecData[n].SIMcoords[3] = (*(CRSList[nrCRS]).NumCols)[i]
          ;          SpecData[n].SIMcoords[4] = SpecData[n].SIMcoords[0] + SpecData[n].SIMcoords[1] - 1
          ;          SpecData[n].SIMcoords[5] = SpecData[n].SIMcoords[2] + SpecData[n].SIMcoords[3] - 1
          SpecData[n].data = ptr_new(fltarr(SpecData[n].SIMcoords[1]/SpecData[n].sum_spec, SpecData[n].SIMcoords[3]/SpecData[n].sum_spat))
          SpecData[n].name = constants->get_CRSlineDescription(SpecData[n].SIMcoords, SpecData[n].type)
          n = n+1
        endfor
      endif ;else if CRSList[nrCRS].Type eq 'sji' then usesji=nrCRS
    endfor
    obj_destroy, constants
    
    ;now we have to rearrange the order of the spectral data
    ;first group those together with the same y-pixel-numbers, then increasing wavelength (x)
    specgroup=intarr(nWindows)
    groupy=lonarr(nWindows,2)
    groups=0
    specx=lonarr(nWindows)
    for i=0,nWindows-1 do begin
      ind = where(groupy[*,0] eq SpecData[i].SIMcoords[2], count)
      if count eq 0 then begin
        groupy[groups,0] = SpecData[i].SIMcoords[2]
        groupy[groups,1] = SpecData[i].SIMcoords[5]
        specgroup[i] = groups
        groups = groups + 1
      endif else begin
        ind2 = where(groupy[ind,1] eq SpecData[i].SIMcoords[5], count2)
        if count2 eq 0 then begin
          groupy[groups,0] = SpecData[i].SIMcoords[2]
          groupy[groups,1] = SpecData[i].SIMcoords[5]
          specgroup[i] = groups
          groups = groups + 1
        endif else begin
          specgroup[i] = ind[ind2[0]]
        endelse
      endelse
      if SpecData[i].type eq 'nuv' then $
        specx[i] = SpecData[i].SIMcoords[0] + 2*PixCCDxfuv1 $
      else $
        specx[i] = SpecData[i].SIMcoords[0]
    endfor
    
    for i=0,groups-1 do begin
      ind = where(specgroup eq i)
      if i eq 0 then begin
        SpecOrder = sort(specx[ind])
      endif else begin
        temp = sort(specx[ind])
        SpecOrder = [SpecOrder, ind[temp]]
      endelse
    endfor
    SpecData = SpecData[SpecOrder]
    
  endif ;if nWindows gt 0
  
  
  
  
  
  ;populate SJI data structure
  SJIdata = MAKE_ARRAY(sji+1, Value=sjistruct) ; structure for SJI data
  n=1 ;first SJI is reserved for the overview SJI, will be populated further down
  minstartrow=PixCCDxfuvSJI
  maxendrow=0
  minstartcol=PixCCDxfuvSJI
  maxendcol=0
  minsize=PixCCDxfuvSJI
  maxsize=0
  for nrCRS = 0,N_ELEMENTS(CRSList)-1 do begin
    if (CRSList[nrCRS].Type eq 'sji') then begin
    
      for nrFDB=0,N_ELEMENTS(FDBList)-1 do begin
        if FDBList[nrFDB].CRSID eq CRSList[nrCRS].ID then begin
          inFDB=nrFDB
          break
        endif
      endfor
      
      for i=0,CRSList[nrCRS].Subregions-1 do begin
        SJIData[n].CRSindex = nrCRS
        SJIData[n].SubRegionID = (*(CRSList[nrCRS]).SubRegionID)[i]
        SJIData[n].FDBindex =  inFDB ;useless here
        SJIData[n].Exp_Duration = FDBList[inFDB].Exp_Duration ;useless here
        SJIData[n].Exp_Type = FDBList[inFDB].Exp_Type ;useless here
        SJIData[n].sum_spatX = CRSList[nrCRS].Spectral
        SJIData[n].sum_spatY = CRSList[nrCRS].Spatial
        if (SJIData[n].sum_spatX gt 1) || (SJIData[n].sum_spatY gt 1) then $
          SJIData[n].name2 = string(SJIData[n].sum_spatX,format='(I1)') + 'x' + string(SJIData[n].sum_spatY,format='(I1)')
        ;        SJIData[n].Boxcoords[0] = (*(CRSList[nrCRS]).StartRow)[i]-1
        ;        SJIData[n].Boxcoords[1] = (*(CRSList[nrCRS]).NumRows)[i]
        ;        SJIData[n].Boxcoords[2] = (*(CRSList[nrCRS]).StartCol)[i]-1
        ;        SJIData[n].Boxcoords[3] = (*(CRSList[nrCRS]).NumCols)[i]
        ;        SJIData[n].Boxcoords[4] = SJIData[n].Boxcoords[0] + SJIData[n].Boxcoords[1] - 1
        ;        SJIData[n].Boxcoords[5] = SJIData[n].Boxcoords[2] + SJIData[n].Boxcoords[3] - 1
        if (*(CRSList[nrCRS]).StartRow)[i] gt PixCCDxfuvSJI then begin
          SJIData[n].type = 'fuv'
          SJIData[n].name = 'FUV'
        endif else if (*(CRSList[nrCRS]).EndRow)[i] gt PixCCDxfuvSJI then begin
          SJIData[n].type = 'both'
          SJIData[n].name = 'NUV+FUV'
        endif else begin
          SJIData[n].type = 'nuv'
          SJIData[n].name = 'NUV'
        endelse
        if SJIData[n].type eq 'fuv' then $
          coords = IRISsim_flipcoords( (*(CRSList[nrCRS]).StartCol)[i], (*(CRSList[nrCRS]).EndCol)[i], $
          (*(CRSList[nrCRS]).StartRow)[i], (*(CRSList[nrCRS]).EndRow)[i], 1, 1, /sjifuv, /startatzero ) $
        else $
          coords = IRISsim_flipcoords( (*(CRSList[nrCRS]).StartCol)[i], (*(CRSList[nrCRS]).EndCol)[i], $
          (*(CRSList[nrCRS]).StartRow)[i], (*(CRSList[nrCRS]).EndRow)[i], 1, 1, /sjinuv, /startatzero )
        SJIData[n].Boxcoords[0] = coords.tsc
        SJIData[n].Boxcoords[1] = coords.tec - coords.tsc + 1
        SJIData[n].Boxcoords[2] = coords.tsr
        SJIData[n].Boxcoords[3] = coords.ter - coords.tsr + 1
        SJIData[n].Boxcoords[4] = coords.tec
        SJIData[n].Boxcoords[5] = coords.ter
        if SJIData[n].Boxcoords[0] lt minstartrow then minstartrow=SJIData[n].Boxcoords[0]
        if SJIData[n].Boxcoords[4] gt maxendrow then maxendrow=SJIData[n].Boxcoords[4]
        if SJIData[n].Boxcoords[2] lt minstartcol then minstartcol=SJIData[n].Boxcoords[2]
        if SJIData[n].Boxcoords[5] gt maxendcol then maxendcol=SJIData[n].Boxcoords[5]
        if SJIData[n].Boxcoords[1] gt SJIData[n].Boxcoords[3] then ind=1 else ind=3
        if SJIData[n].Boxcoords[ind] gt maxsize then maxsize=SJIData[n].Boxcoords[ind]
        if SJIData[n].Boxcoords[ind] lt minsize then minsize=SJIData[n].Boxcoords[ind]
        SJIData[n].data = ptr_new(fltarr(SJIData[n].Boxcoords[1]/SJIData[n].sum_spatX, SJIData[n].Boxcoords[3]/SJIData[n].sum_spatY))
        n=n+1
      endfor
    endif
  endfor
  
  ;check if we have to adjust size according to set minimum and maximum size (minSizeSJI, maxSizeSJI)
  ;maximum has priority over minimum
  factorSJI=1d
  if maxsize gt maxSizeSJI then factorSJI = double(maxSizeSJI)/double(maxsize)
  
  
  if sji eq 0 then begin
    minstartrow = PixCCDxfuvSJI/2 - 10
    maxendrow = PixCCDxfuvSJI/2 + 10
    minstartcol = SpecData[0].SIMcoords[2]
    maxendcol = SpecData[0].SIMcoords[5]
  endif
  
  
  ;overview SJI
  case rasterSJI of
    'fu1': SJIData[0].name = 'FUV1'
    'fu2': SJIData[0].name = 'FUV2'
    'nu1': SJIData[0].name = 'NUV1'
    'nu2': SJIData[0].name = 'NUV2'
    else: SJIData[0].name = '???'
  ENDCASE
  SJIData[0].name2 = 'Overview'
  SJIData[0].type = 'Overview'
  SJIData[0].Boxcoords[0] = minstartrow + minX[0] - SJIframe
  SJIData[0].Boxcoords[1] = maxendrow - minstartrow + 1 + maxX[0] - minX[0] + 2*SJIframe
  SJIData[0].Boxcoords[2] = minstartcol + minX[1] - SJIframe
  SJIData[0].Boxcoords[3] = maxendcol - minstartcol + 1 + maxX[1] - minX[1] + 2*SJIframe
  SJIData[0].Boxcoords[4] = SJIData[0].Boxcoords[0] + SJIData[0].Boxcoords[1] - 1
  SJIData[0].Boxcoords[5] = SJIData[0].Boxcoords[2] + SJIData[0].Boxcoords[3] - 1
  SJIData[0].data = ptr_new(fltarr(SJIData[0].Boxcoords[1], SJIData[0].Boxcoords[3]))
  factor2 = double(maxSizeSJI)*maxfacx/double(SJIData[0].Boxcoords[1])
  factor3 = double(maxSizeSJI)*maxfacy/double(SJIData[0].Boxcoords[3])
  factorSJI = min([factorSJI, factor2, factor3])
  if factorSJI ge 1d then begin
    if SJIData[0].Boxcoords[1] gt SJIData[0].Boxcoords[3] then ind=1 else ind=3
    if SJIData[0].Boxcoords[ind] lt minsize then minsize=SJIData[0].Boxcoords[ind] ;this should actually never be true
    if minsize lt minSizeSJI then factorSJI = double(minSizeSJI)/double(minsize)
  endif
  if sji gt 0 then begin
    SJIData[0].Showcoords[0] = SJIData[1].Boxcoords[0]
    SJIData[0].Showcoords[1] = SJIData[1].Boxcoords[1]
    SJIData[0].Showcoords[2] = SJIData[1].Boxcoords[2]
    SJIData[0].Showcoords[3] = SJIData[1].Boxcoords[3]
    SJIData[0].Showcoords[4] = SJIData[1].Boxcoords[4]
    SJIData[0].Showcoords[5] = SJIData[1].Boxcoords[5]
  endif else begin
    SJIData[0].Showcoords[0] = minstartrow
    SJIData[0].Showcoords[1] = maxendrow - minstartrow + 1
    SJIData[0].Showcoords[2] = minstartcol
    SJIData[0].Showcoords[3] = maxendcol - minstartcol + 1
    SJIData[0].Showcoords[4] = SJIData[0].Showcoords[0] + SJIData[0].Showcoords[1] - 1
    SJIData[0].Showcoords[5] = SJIData[0].Showcoords[2] + SJIData[0].Showcoords[3] - 1
  endelse
  
  
  
  if sji gt 0 then begin
    ;now we have to rearrange the order of the sji data
    ;first group those together with the same y-pixel-numbers, then increasing wavelength (x)
    sjigroup=intarr(sji)
    groupy=lonarr(sji,2)
    groups=0
    sjix=lonarr(sji)
    for i=1,sji do begin
      ind = where(groupy[*,0] eq SJIData[i].Boxcoords[2], count)
      if count eq 0 then begin
        groupy[groups,0] = SJIData[i].Boxcoords[2]
        groupy[groups,1] = SJIData[i].Boxcoords[5]
        sjigroup[i-1] = groups
        groups = groups + 1
      endif else begin
        ind2 = where(groupy[ind,1] eq SJIData[i].Boxcoords[5], count2)
        if count2 eq 0 then begin
          groupy[groups,0] = SJIData[i].Boxcoords[2]
          groupy[groups,1] = SJIData[i].Boxcoords[5]
          sjigroup[i-1] = groups
          groups = groups + 1
        endif else begin
          sjigroup[i-1] = ind[ind2[0]]
        endelse
      endelse
      case SJIData[i].type of
        'fuv': sjix[i-1]=1
        'nuv': sjix[i-1]=2
        'both': sjix[i-1]=0
      endcase
    endfor
    
    for i=0,groups-1 do begin
      ind = where(sjigroup eq i)
      if i eq 0 then begin
        SJIOrder = sort(sjix[ind])
      endif else begin
        temp = sort(sjix[ind])
        SJIOrder = [SJIOrder, ind[temp]]
      endelse
    endfor
    SJIData[1:sji] = SJIData[SJIOrder+1]
  endif ;if sji qt 0
  
  
  
  
  ;get the size of SJI windows
  nxchar=intarr(4) ;number of characters in x-direction
  xRow=intarr(4) ;size of all graphs in one row in x-direction
  yRow=intarr(4) ;size of all graphs in one row in y-direction
  nWin=intarr(4) ;number of graphs in one row
  nWin[0]=sji+1
  for n=0,sji do begin
    xRow[0] = xRow[0] + SJIData[n].Boxcoords[1] * factorSJI
    if (SJIData[n].Boxcoords[3] * factorSJI) gt yRow[0] then yRow[0] = SJIData[n].Boxcoords[3] * factorSJI
    if n eq 0 then begin
      SJIData[n].axisL=1
      nxchar[0]=nxchar[0]+xaxisL
    endif
    if n eq 1 then begin
      SJIData[n].axisL=1
      nxchar[0]=nxchar[0]+xaxisL
    endif
    if n eq sji then begin
      SJIData[n].axisR=1
      nxchar[0]=nxchar[0]+xaxisR
    endif
    if n gt 1 then begin
      if sjigroup[n-1] ne sjigroup[n-2] then begin
        if SJIData[n-1].axisR eq 0 then begin
          SJIData[n-1].axisR=1
          nxchar[0]=nxchar[0]+xaxisR
        endif
        if SJIData[n].axisL eq 0 then begin
          SJIData[n].axisL=1
          nxchar[0]=nxchar[0]+xaxisL
        endif
      endif else begin
        nxchar[0]=nxchar[0]+xdist
      endelse
    endif
  endfor
  
  
  ;define size of characters, if not defined yet
  if (!p.charsize ne 0) then pcharsize=!p.charsize else pcharsize=1.0
  xannotS=intarr(4)
  xsizeS=intarr(4)
  
  
  xannotS[0]=nxchar[0]*!d.x_ch_size*pcharsize ;total size in pixels of margins in x-direction
  
  
  
  
  if nWindows gt 0 then begin
    ;get the size of all Spec windows in one row
    xRowRow = intarr(nWindows)
    xannotRow = intarr(nWindows)
    xsizeRow = intarr(nWindows)
    row=1
    for n=0,nWindows-1 do begin
      xRow[row] = xRow[row] + SpecData[n].SIMcoords[1]
      nWin[row] = nWin[row] + 1
      
      if n eq 0 then begin
        SpecData[n].axisL=1
        nxchar[row]=nxchar[row]+xaxisL
      endif
      if n eq nWindows-1 then begin
        SpecData[n].axisR=1
        nxchar[row]=nxchar[row]+xaxisR
      endif
      if n gt 0 then begin
        if specgroup[n] ne specgroup[n-1] then begin
        
          if SpecData[n-1].axisR eq 0 then begin
            SpecData[n-1].axisR=1
            nxchar[row]=nxchar[row]+xaxisR
          endif
          if SpecData[n].axisL eq 0 then begin
            SpecData[n].axisL=1
            nxchar[row]=nxchar[row]+xaxisL
          endif
        endif else begin
          if nWin[row] gt 1 then $
            nxchar[row]=nxchar[row]+xdist
        endelse
      endif
      xRowRow[n] = xRow[row]
      xannotRow[n] = nxchar[row]*!d.x_ch_size*pcharsize
    endfor
    xsizeRow = xRowRow+xannotRow
    
    
    
    
    ;x-direction
    ;xannotS=intarr(4)
    ;xsizeS=intarr(4)
    xannotS=nxchar*!d.x_ch_size*pcharsize ;total size in pixels of margins in x-direction
    xsizeS=xRow+xannotS ;total size in pixels of spectral graphs in x-direction
    ;xsize = max(xsizeS,maxin)
    
    
    rowfact = xsizeS / double(xmax)
    if rowfact[0] gt 1. then $
      rowfact = rowfact[1] / rowfact[0] $
    else rowfact = rowfact[1]
    
    rows=1
    if (rowfact gt 1.4) && (nWindows gt 1) then rows=2
    if (rowfact gt 2.5) && (nWindows gt 1) then rows=3
    
    ;  print, 'vorher xRow', xRow
    ;  print, 'vorher xsizeS', xsizeS
    ;  print, 'vorher xannotS', xannotS
    if rows gt 1 then begin
      linesize = xsizeRow[nWindows-1]/rows
      xsizeRow2 = xsizeRow-linesize
      ind=min(xsizeRow2, minin, /absolute)
      nWin[1]=minin+1
      xRow[1]=xRowRow[minin]
      xannotS[1]=xannotRow[minin]
      if ~SpecData[minin].axisR && ~SpecData[minin+1].axisL then $
        xannotRow[minin]=xannotRow[minin]+xdist*!d.x_ch_size*pcharsize
      if SpecData[minin].axisR eq 0 then begin
        SpecData[minin].axisR=1
        xannotS[1]=xannotS[1]+xaxisR*!d.x_ch_size*pcharsize
      endif
      nWin[2]=nWindows-nWin[1]
      xRow[2]=xRowRow[nWindows-1]-xRowRow[minin]
      xannotS[2]=xannotRow[nWindows-1]-xannotRow[minin]
      if SpecData[minin+1].axisL eq 0 then begin
        SpecData[minin+1].axisL=1
        xannotS[2]=xannotS[2]+xaxisL*!d.x_ch_size*pcharsize
      endif
      
      
      if rows eq 3 then begin
        xsizeRow = xsizeRow[minin+1:nWindows-1]-xsizeRow[minin]
        linesize = xsizeRow[nWin[2]-1]/2
        xsizeRow2 = xsizeRow-linesize
        ind=min(xsizeRow2, minin2, /absolute)
        nWin[2] = minin2+1
        xRow[2] = xRowRow[minin+1+minin2]-xRowRow[minin]
        xannotS[2]=xannotRow[minin+1+minin2]-xannotRow[minin]
        if SpecData[minin+1].axisL eq 1 then $
          xannotS[2]=xannotS[2]+xaxisL*!d.x_ch_size*pcharsize
          
        if ~SpecData[minin+1+minin2].axisR && ~SpecData[minin+1+minin2+1].axisL then $
          xannotRow[minin+1+minin2]=xannotRow[minin+1+minin2]+xdist*!d.x_ch_size*pcharsize
        if SpecData[minin+1+minin2].axisR eq 0 then begin
          SpecData[minin+1+minin2].axisR=1
          xannotS[2]=xannotS[2]+xaxisR*!d.x_ch_size*pcharsize
        endif
        nWin[3]=nWindows-nWin[1]-nWin[2]
        xRow[3]=xRowRow[nWindows-1]-xRowRow[minin+1+minin2]
        xannotS[3]=xannotRow[nWindows-1]-xannotRow[minin+1+minin2]
        if SpecData[minin+1+minin2+1].axisL eq 0 then begin
          SpecData[minin+1+minin2+1].axisL=1
          xannotS[3]=xannotS[3]+xaxisL*!d.x_ch_size*pcharsize
        endif
      endif
    endif
  ;  print, 'nachher xRow', xRow
  ;  print, 'nachher xannotS', xannotS
    
    
    
  ;if nWindows gt 0
  endif else rows=0
  
  
  
  xannotS = xannotS+xaxisRextra*!d.x_ch_size*pcharsize ;total size in pixels of margins in x-direction for SJI
  xsizeS=xRow+xannotS ;total size in pixels of SJI graphs in x-direction
  xsize = max(xsizeS,maxin)
  
  ;  print, 'nachher2 xsizeS', xsizeS
  ;  print, 'nachher2 xannotS', xannotS
  
  ;y-direction
  n=0
  for row=1,3 do begin
    for wind=0,nWin[row]-1 do begin
      if SpecData[n].SIMcoords[3] gt yRow[row] then yRow[row] = SpecData[n].SIMcoords[3]
      n=n+1
    endfor
  endfor
  
  nychar=ytop+ybottom+rows*ydist ;sizes of margins in y-direction in units of char-size
  yannot=nychar*!d.y_ch_size*pcharsize ;total size in pixels of margins in y-direction
  ysize=total(yRow)+yannot ;total size in pixels of window in y-direction
  
  
  
  
  
  fact=1.0
  ;decrease size if necessary
  if(n_elements(xmax) ne 0) then begin
    if(xsize gt xmax) then begin
      for i=0,3 do begin
        newx = fact*xRow[i]+xannotS[i]
        if newx gt xmax then fact=double(xmax-xannotS[i])/double(xRow[i])
      endfor
      ysize=fact*total(yRow)+yannot
      xsize=max(fact*xRow+xannotS)
    endif
  endif
  if(n_elements(ymax) ne 0) then begin
    if ysize gt ymax then begin
      facty=double(ymax-yannot)/double(fact*total(yRow))
      fact=fact*facty
      ysize=fact*total(yRow)+yannot
      xsize=max(fact*xRow+xannotS)
    endif
  endif
  
  WINsize=[xsize,ysize]
  halfpixelsize=0.55/WINsize ; the plot's width and height must be at least half a pixel
  
  ;caculate positons of each plot within window
  dxPosaxL = xaxisL*!d.x_ch_size*pcharsize / xsize
  dxPosaxR = xaxisR*!d.x_ch_size*pcharsize / xsize
  dxPos = xdist*!d.x_ch_size*pcharsize / xsize
  dyPos = ydist*!d.y_ch_size*pcharsize / ysize
  
  
  top = 1d - ytop*!d.y_ch_size*pcharsize / ysize
  bottom = top - fact * yRow[0] / ysize
  left = dxPosaxL
  for j=0,nWin[0]-1 do begin
    top = bottom + fact * factorSJI * SJIData[j].Boxcoords[3] / ysize
    right = left + fact * factorSJI * SJIData[j].Boxcoords[1] / xsize
    if right-left lt halfpixelsize[0] then right=left+halfpixelsize[0]
    if top-bottom lt halfpixelsize[1] then top=bottom+halfpixelsize[1]
    SJIData[j].WINcoords = [left, bottom, right, top]
    SJIData[j].nrticksx = fix(fact*factorSJI*double(SJIData[j].Boxcoords[1])/77./!p.charsize)+1
    SJIData[j].nrticksy = fix(fact*factorSJI*double(SJIData[j].Boxcoords[3])/77./!p.charsize)+1
    if j lt nWin[0]-1 then begin
      left = right
      if SJIData[j].axisR then left = left + dxPosaxR
      if SJIData[j+1].axisL then left = left + dxPosaxL
      if ~SJIData[j].axisR && ~SJIData[j+1].axisL then left = left + dxPos
    endif
    if j eq 0 then begin
      ;calculate also position of analog clock
      regionsize = fact * factorSJI * min([SJIData[j].Boxcoords[1], SJIData[j].Boxcoords[3]])
      PosClock=dblarr(4)
      PosClock[2] = regionsize/3./xsize
      PosClock[3] = regionsize/3./ysize
      PosClock[0] = right - PosClock[2]; * 9. / 24. / xsize
      PosClock[1] = top - PosClock[3]; - fact * factorSJI * SJIData[j].Boxcoords[3] /24. /ysize
    endif
  endfor
  
  n=0
  for i=1,3 do begin
    bottom = bottom - fact * yRow[i] / ysize - dyPos
    left = dxPosaxL
    ;    originalsize=left*xsize
    ;    originalxannot=originalsize
    for j=0,nWin[i]-1 do begin
      top = bottom + fact * SpecData[n].SIMcoords[3] / ysize
      right = left + fact * SpecData[n].SIMcoords[1] / xsize
      ;      originalsize=originalsize+SpecData[n].SIMcoords[1]
      if right-left lt halfpixelsize[0] then right=left+halfpixelsize[0]
      if top-bottom lt halfpixelsize[1] then top=bottom+halfpixelsize[1]
      SpecData[n].WINcoords = [left, bottom, right, top]
      SpecData[n].nrticksx = fix(fact*double(SpecData[n].SIMcoords[1])/77./!p.charsize)+1
      SpecData[n].nrticksy = fix(fact*double(SpecData[n].SIMcoords[3])/77./!p.charsize)+1
      if j lt nWin[i]-1 then begin
        left = right
        if SpecData[n].axisR then left = left + dxPosaxR
        if SpecData[n+1].axisL then left = left + dxPosaxL
        if ~SpecData[n].axisR && ~SpecData[n+1].axisL then left = left + dxPos
      ;        if SpecData[n].axisR then originalsize = originalsize + dxPosaxR*xsize
      ;        if SpecData[n+1].axisL then originalsize = originalsize + dxPosaxL*xsize
      ;        if ~SpecData[n].axisR && ~SpecData[n+1].axisL then originalsize = originalsize + dxPos*xsize
      ;        if SpecData[n].axisR then originalxannot = originalxannot + dxPosaxR*xsize
      ;        if SpecData[n+1].axisL then originalxannot = originalxannot + dxPosaxL*xsize
      ;        if ~SpecData[n].axisR && ~SpecData[n+1].axisL then originalxannot = originalxannot + dxPos*xsize
      endif ;else begin
      ;        print,i,'total',originalsize+ dxPosaxR*xsize+xaxisRextra*!d.x_ch_size*pcharsize
      ;        print,i,'annot',originalxannot+ dxPosaxR*xsize+xaxisRextra*!d.x_ch_size*pcharsize
      ;      endelse
      n=n+1
    endfor
  endfor
;  print,'L', dxPosaxL*xsize
;  print,'R', dxPosaxR*xsize
;  print,'d', dxPos*xsize
;  print,'e', xaxisRextra*!d.x_ch_size*pcharsize
end
