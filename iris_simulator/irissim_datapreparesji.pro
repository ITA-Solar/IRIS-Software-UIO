pro IRISsim_DataPrepareSJI, fuvSJI1, fuvSJI2, nuvSJI1, nuvSJI2, rasterSJI, CCDx, CCDy, timefacfuv, timefacnuv, PZT, SJIData, $
    FRM, filterwheel, nrFDB, FDB, CRS, SJI, SJIwinUpdate, $
    threshold_SJI_Overview, threshold_SJI
    
  ; INPUT:
  ; fuvSJI1: input data from SIM, containing FUV SJI time t
  ; fuvSJI2: input data from SIM, containing FUV SJI time t+1
  ; nuvSJI1: input data from SIM, containing NUV SJI time t
  ; nuvSJI2: input data from SIM, containing NUV SJI time t+1
  ; rasterSJI: which wavelength should be used for overview graph ('fu1, 'fu2', 'nu1', 'nu2')
  ; CCDx: size of detector in pixels in x-direction
  ; CCDy: size of detector in pixels in y-direction
  ; timefac: factor between t and t+1 (((t+1)-t)/dt)
  ; PZT: the current PZT values
  ; SJIData: created by initgraph.pro, will be changed, i.e. data is updated
  ; (SJIind: the index of the current SJI)
  ; SJIwinUpdate: vector indicating which SJIData have to be updated
  ; FRM: Structure containing current FRM
  ; nrFDB: entry number within the FRM
  ; FDB: Structure containing current FDB
  ; CRS: Structure containing current CRS
  ;
  ; OUTPUT:
  ; SJI: the resulting SJI
  ;
  ; created by Martin Wiesmann (UIO), Sept 2012
; $Id: irissim_datapreparesji.pro,v 1.5 2013/09/04 15:25:38 mawiesma Exp $  ;
    
    
  ;name='FUV1 1330 ' + ' [!3' + String("305B) + '!X]'
    
  ;SJI calculation
  ;Overview plot
  ;interpolate SJI and get right SJI
  case rasterSJI of
    'fu1': BEGIN
      UV=1
      SJI = timefacfuv * fuvSJI2[*,*,0] + (1-timefacfuv) * fuvSJI1[*,*,0]
    end
    'fu2': BEGIN
      UV=1
      SJI = timefacfuv * fuvSJI2[*,*,1] + (1-timefacfuv) * fuvSJI1[*,*,1]
    end
    'nu1': BEGIN
      UV=0
      SJI = timefacnuv * nuvSJI2[*,*,0] + (1-timefacnuv) * nuvSJI1[*,*,0]
    end
    'nu2': BEGIN
      UV=0
      SJI = timefacnuv * nuvSJI2[*,*,1] + (1-timefacnuv) * nuvSJI1[*,*,1]
    end
    else: BEGIN
      UV = -1
      rSJI = fltarr(SJIData[0].Boxcoords[4]+1, SJIData[0].Boxcoords[5]+1)
    end
  endcase
  
  if UV ge 0 then begin
    sSJI = size(SJI)
    
    ;simulation has half the resolution in x-direction, so interpolate missing values (only FUV)
    if UV then begin
      SJI=interpolate(SJI,findgen(sSJI[1]*2)/2,findgen(sSJI[2]),/grid)
      sSJI = size(SJI)
    endif
    
    
    ;tile SJI as well
    ;we only calculate the area which we actually need, note this might be bigger than the actual detector due to PZT movements
    CCDxtemp=SJIData[0].Boxcoords[4]+1
    CCDytemp=SJIData[0].Boxcoords[5]+1
    rSJI = make_array(CCDxtemp, CCDytemp, /float)
    keepcopying=1
    low=0
    high=low+sSJI[2]
    while keepcopying do begin      ;vertical
      if low ge CCDytemp then begin
        keepcopying=0
      endif else begin
        if high gt CCDytemp-1 then begin
          miss=CCDytemp-1-low
          rSJI[0:sSJI[1]-1,low:CCDytemp-1] = SJI[*,0:miss]
          keepcopying=0
        endif else begin
          rSJI[0:sSJI[1]-1,low:high-1] = SJI
          low=low+sSJI[2]
          high=high+sSJI[2]
        endelse
      endelse
    endwhile
    keepcopying=1
    low=sSJI[1]
    high=low+sSJI[1]
    while keepcopying do begin      ;horizontal
      if low ge CCDxtemp then begin
        keepcopying=0
      endif else begin
        if high gt CCDxtemp-1 then begin
          miss=CCDxtemp-1-low
          rSJI[low:CCDxtemp-1,*] = rSJI[0:miss,*]
          keepcopying=0
        endif else begin
          rSJI[low:high-1,*] = rSJI[0:sSJI[1]-1,*]
          low=low+sSJI[1]
          high=high+sSJI[1]
        endelse
      endelse
    endwhile
    
  endif
  ;finally, set the data
  ;the start row can be negative (because of SJI frame)
  if SJIData[0].Boxcoords[0] lt 0 then begin
    rSJI2 = make_array(CCDxtemp-SJIData[0].Boxcoords[0], CCDytemp, /float)
    fa=ceil((-1.) * SJIData[0].Boxcoords[0] / sSJI[1])
    rSJI2[0:SJIData[0].Boxcoords[0]*(-1)-1, *] = rSJI[fa*sSJI[1]+SJIData[0].Boxcoords[0]:fa*sSJI[1]-1, *]
    rSJI2[SJIData[0].Boxcoords[0]*(-1):CCDxtemp-SJIData[0].Boxcoords[0]-1, *] = rSJI
    rSJI = rSJI2
    startx=0
    addx=SJIData[0].Boxcoords[0]*(-1)
  endif else begin
    startx=SJIData[0].Boxcoords[0]
    addx=0
  endelse
  ;the start column can be negative (because of SJI frame)
  if SJIData[0].Boxcoords[2] lt 0 then begin
    rSJI2 = make_array((size(rSJI))[1], CCDytemp-SJIData[0].Boxcoords[2], /float)
    fa=ceil((-1.) * SJIData[0].Boxcoords[2] / sSJI[2])
    rSJI2[*, 0:SJIData[0].Boxcoords[2]*(-1)-1] = rSJI[*, fa*sSJI[2]+SJIData[0].Boxcoords[2]:fa*sSJI[2]-1]
    rSJI2[*, SJIData[0].Boxcoords[2]*(-1):CCDytemp-SJIData[0].Boxcoords[2]-1] = rSJI
    rSJI = rSJI2
    starty=0
    addy=SJIData[0].Boxcoords[2]*(-1)
  endif else begin
    starty=SJIData[0].Boxcoords[2]
    addy=0
  endelse
  *(SJIData[0]).data = rSJI[startx:SJIData[0].Boxcoords[4]+addx, starty:SJIData[0].Boxcoords[5]+addy]
  
  ;the first time this spectral window is populated, we need to set the minium and maximum value for scaling
  if (SJIData[0].min_show eq 0) && (SJIData[0].max_show eq 0) then begin
    temp = Histo_Opt(*(SJIData[0]).data, threshold_SJI_Overview)
    SJIData[0].max_show = max(temp, min=m)
    SJIData[0].min_show = m
  endif
  
  
  
  
  
  
  
  
  
  
  
  ;real SJI, only when there is actually an SJI taken
  if total(SJIwinUpdate) gt 1 then begin
  
    ;get indices of SJIs to update
    indices = where(SJIwinUpdate[1:N_ELEMENTS(SJIwinUpdate)-1] eq 1, counts)
    indices = indices+1
    
    ;first we set the box of the overview SJI to the box of the current SJI
    ;but what if there are more than one SJI regions within a single CRS?
    ;for now, we just take the first region
    SJIData[0].Showcoords = SJIData[indices[0]].Boxcoords
    if SJIData[0].Showcoords[0] ge CCDx then begin
      SJIData[0].Showcoords[0] = SJIData[0].Showcoords[0] - CCDx
      SJIData[0].Showcoords[4] = SJIData[0].Showcoords[4] - CCDx
    endif

    case filterwheel of
      1: begin
        typecurrent = 'Glass'
        UV = -1
      end
      
      31: begin
        typecurrent = 'FUV1'
        SJI = timefacfuv * fuvSJI2[*,*,0] + (1-timefacfuv) * fuvSJI1[*,*,0]
        UV = 1
      end
      
      61: begin
        typecurrent = 'NUV1'
        SJI = timefacnuv * nuvSJI2[*,*,0] + (1-timefacnuv) * nuvSJI1[*,*,0]
        UV = 0
      end
      
      91: begin
        typecurrent = 'FUV2'
        SJI = timefacfuv * fuvSJI2[*,*,1] + (1-timefacfuv) * fuvSJI1[*,*,1]
        UV = 1
      end
      
      121: begin
        typecurrent = 'NUV2'
        SJI = timefacnuv * nuvSJI2[*,*,1] + (1-timefacnuv) * nuvSJI1[*,*,1]
        UV = 0
      end
      
      151: begin
        typecurrent = 'Broadband'
        UV = -1
      end
      
      else: begin
        typecurrent = '???'
        UV = -1
      end
    endcase
    
    
    if UV ge 0 then begin
    
      sSJI = size(SJI)
      
      ;simulation has half the resolution in x-direction, so interpolate missing values (only FUV)
      if UV then begin
        SJI=interpolate(SJI,findgen(sSJI[1]*2)/2,findgen(sSJI[2]),/grid)
        sSJI = size(SJI)
      endif
      
      ;before tiling, we shift the SJI, according to PZT
      x = PZT[0]*6 mod sSJI[1]
      if x ne 0 then begin
        if x lt 0 then x=sSJI[1]+x
        x1 = floor(x)
        spacefacx = x-x1
        
        ;shift x
        if x1 gt 0 then begin
          SJItemp = fltarr(sSJI[1], sSJI[2])
          SJItemp[0:sSJI[1]-x1-1,*] = SJI[x1:sSJI[1]-1,*]
          SJItemp[sSJI[1]-x1:sSJI[1]-1,*] = SJI[0:x1-1,*]
          SJI = SJItemp
        endif
        ;interpolate x
        SJItemp = fltarr(sSJI[1]+1, sSJI[2])
        SJItemp[0:sSJI[1]-1,*] = SJI
        SJItemp[sSJI[1],*] = SJI[0,*]
        SJI = interpolate(SJItemp, findgen(sSJI[1])+spacefacx, findgen(sSJI[2]), /grid)
      endif
      
      
      y = PZT[1]*6 mod sSJI[2]
      if y ne 0 then begin
        if y lt 0 then y=sSJI[2]+y
        y1 = floor(y)
        spacefacy = y-y1
        
        ;shift y
        if y1 gt 0 then begin
          SJItemp = fltarr(sSJI[1], sSJI[2])
          SJItemp[*,0:sSJI[2]-y1-1] = SJI[*,y1:sSJI[2]-1]
          SJItemp[*,sSJI[2]-y1:sSJI[2]-1] = SJI[*,0:y1-1]
          SJI = SJItemp
        endif
        ;interpolate y
        SJItemp = fltarr(sSJI[1], sSJI[2]+1)
        SJItemp[*,0:sSJI[2]-1] = SJI
        SJItemp[*,sSJI[2]] = SJI[*,0]
        SJI = interpolate(SJItemp, findgen(sSJI[1]), findgen(sSJI[2])+spacefacy, /grid)
      endif
      
      
      ;tile SJI as well
      rSJI = make_array(CCDx, CCDy, /float)
      keepcopying=1
      low=0
      high=low+sSJI[2]
      while keepcopying do begin      ;vertical
        if low ge CCDy then begin
          keepcopying=0
        endif else begin
          if high gt CCDy-1 then begin
            miss=CCDy-1-low
            rSJI[0:sSJI[1]-1,low:CCDy-1] = SJI[*,0:miss]
            keepcopying=0
          endif else begin
            rSJI[0:sSJI[1]-1,low:high-1] = SJI
            low=low+sSJI[2]
            high=high+sSJI[2]
          endelse
        endelse
      endwhile
      keepcopying=1
      low=sSJI[1]
      high=low+sSJI[1]
      while keepcopying do begin      ;horizontal
        if low ge CCDx then begin
          keepcopying=0
        endif else begin
          if high gt CCDx-1 then begin
            miss=CCDx-1-low
            rSJI[low:CCDx-1,*] = rSJI[0:miss,*]
            keepcopying=0
          endif else begin
            rSJI[low:high-1,*] = rSJI[0:sSJI[1]-1,*]
            low=low+sSJI[1]
            high=high+sSJI[1]
          endelse
        endelse
      endwhile
      
      SJItemp = fltarr(2*CCDx, CCDy)
      ;if (typecurrent eq 'FUV1') || (typecurrent eq 'FUV2') $
      ;  then SJItemp[CCDx:2*CCDx-1, *] = rSJI $
      ;else SJItemp[0:CCDx-1, *] = rSJI
      SJItemp[0:CCDx-1, *] = rSJI
      rSJI = SJItemp
    endif else rSJI = fltarr(2*CCDx, CCDy)
    
    ;check if we need to rebin the data
    sumX=SJIData[indices[0]].sum_spatX
    sumY=SJIData[indices[0]].sum_spatY
    if (sumX gt 1) || (sumY gt 1) then begin
      rSJI = rebin(rSJI, 2*CCDx/sumX, CCDy/sumY) * sumX * sumY
    endif
    
    ;multiply with defined exposure duratioin
    rSJI = rSJI * SJIData[indices[0]].Exp_Duration/1000d
    
    SJI = fltarr((size(rSJI))[1], (size(rSJI))[2])
    
    
    
    for nrSJI=0,counts-1 do begin
      SJIind=indices[nrSJI]
      
      if UV eq 0 then begin
        case SJIData[SJIind].type of
          'fuv': SJIData[SJIind].name = 'FUV0'
          'nuv': SJIData[SJIind].name = typecurrent
          'both': SJIData[SJIind].name = typecurrent+'+FUV0'
          else: SJIData[SJIind].name = '???'
        endcase
      endif else if UV eq 1 then begin
        case SJIData[SJIind].type of
          'fuv': SJIData[SJIind].name = typecurrent
          'nuv': SJIData[SJIind].name = 'NUV0'
          'both': SJIData[SJIind].name = 'NUV0+'+typecurrent
          else: SJIData[SJIind].name = '???'
        endcase
      endif else SJIData[SJIind].name = typecurrent
      
      
      ; set all data to zero, except for CRS windows
      startR = SJIData[SJIind].Boxcoords[0]
      endR = SJIData[SJIind].Boxcoords[4]
      startC = SJIData[SJIind].Boxcoords[2]
      endC = SJIData[SJIind].Boxcoords[5]
      
      ;adjust for rebinning
      if sumX gt 1 then begin
        startR = startR / sumX
        endR = endR / sumX
      endif
      if sumY gt 1 then begin
        startC = startC / sumY
        endC = endC / sumY
      endif
      
      ;finally, set the data
      SJI[startR:endR, startC:endC] = rSJI[startR:endR, startC:endC]
        *(SJIData[SJIind]).data = SJI[startR:endR, startC:endC]
      
      ;the first time this spectral window is populated, we need to set the minium and maximum value for scaling
      ;for each FW value, there has to be different min and max
      case typecurrent of
        'Glass': FWind=0
        'FUV1': FWind=1
        'NUV1': FWind=2
        'FUV2': FWind=3
        'NUV2': FWind=4
        'Broadband': FWind=5
        '???': FWind=6
      endcase
      if (SJIData[SJIind].min_showFW[FWind] eq 0) && (SJIData[SJIind].max_showFW[FWind] eq 0) then begin
        temp = Histo_Opt(*(SJIData[SJIind]).data, threshold_SJI)
        SJIData[SJIind].max_showFW[FWind] = max(temp, min=m)
        SJIData[SJIind].min_showFW[FWind] = m
      endif
      SJIData[SJIind].max_show = SJIData[SJIind].max_showFW[FWind]
      SJIData[SJIind].min_show = SJIData[SJIind].min_showFW[FWind]
      
      ;we save the PZT offset for each SJI indivually, for the axis of the plot
      SJIData[SJIind].PZTx = PZT[0]
      SJIData[SJIind].PZTy = PZT[1]
      
    endfor ;SJI regions
  endif
end
