PRO IRISl12_log2rasterstruct, simlog, rasters, rastersSJI, l1to2log, simendtime
  ; $Id: irisl12_log2rasterstruct.pro,v 1.22 2019/08/08 08:09:11 mawiesma Exp $

  ;this procedure goes through the simlog (=output structure from IRISsim_syntool)
  ;and returns a strucuter containing the indices of the simlog entries, for each rastertype/-repetition

  print, 'new    step  logind      fuvind      nuvind      sjiind'
  
  laststeptime = max(simlog.simtime)
  
  ;define the structure for a raster
  rasterdef = {SJIlogind:ptr_new(), $
    FUVlogind:ptr_new(), $
    NUVlogind:ptr_new(), $
    FUVnRegions:0, $
    NUVnRegions:0, $
    FuvCRS:ptr_new(), $
    NuvCRS:ptr_new(), $
    FUVsr:intarr(8), $
    FUVer:intarr(8), $
    FUVsc:intarr(8), $
    FUVec:intarr(8), $
    NUVsr:intarr(8), $
    NUVer:intarr(8), $
    NUVsc:intarr(8), $
    NUVec:intarr(8), $
    nSteps:0L, $
    rasterPos:0L, $
    sitandstare:0, $
    cadpl_av:0.0, $
    cadpl_dv:0.0, $
    PZTxy:ptr_new(), $
    time:ptr_new(), $
    endtime:ptr_new(), $
    exptimeFUV:ptr_new(), $
    exptimeNUV:ptr_new(), $
    exptimeSJI:ptr_new(), $
    sumspecFUV:ptr_new(), $
    sumspatFUV:ptr_new(), $
    sumspecNUV:ptr_new(), $
    sumspatNUV:ptr_new(), $
    sumspecSJI:ptr_new(), $
    sumspatSJI:ptr_new(), $
    nRasters:0L}
    
  craster = rasterdef
  
  step=-1L
  for logind=0,N_ELEMENTS(simlog)-1 do begin
    newraster=0
    if simlog[logind].step gt step then begin ;new step
      step=simlog[logind].step
      stepind=where(simlog.step eq step)
      
      fuvind=where(simlog[stepind].type eq 'FUV', count)
      if count gt 0 then $ ;there is a FUV exposure for this step
        fuvind=fuvind[0]+logind
        
      nuvind=where(simlog[stepind].type eq 'NUV', count)
      if count gt 0 then $ ;there is a NUV exposure for this step
        nuvind=nuvind[0]+logind
        
      sjiind=where(simlog[stepind].type eq 'SJI', count)
      if count gt 0 then $ ;there is a SJI exposure for this step
        sjiind=sjiind[0]+logind
        
      if step gt 0 then begin ;not the first take within the run
        dpztx = simlog[logind].pztxy[0] - pztxold
        if dpztx ne 0 then dpztx=dpztx/abs(dpztx)
        if dpztxold eq 9 then dpztxold = dpztx ;if this is the second take of a raster we have to recalibrate the sign of x
        
        if dpztx ne dpztxold then begin ;raster ends, start a new one, due to sign of dpztx
          newraster=1
          ;print,'dpztx sign changed'
          dpztxold = 9
        endif else begin ;same sign of dpztx
        
        
          ;check if we have a FUV in this exposure
          addFUVcrs=0
          addFUVregions=0
          if fuvind ge 0 then begin ;there is a FUV exposure for this step
            if craster.FUVnRegions gt 0 then begin
              if craster.FUVnRegions ne simlog[fuvind].numreg then begin ;nr of regions unequal
                newraster=1
                dpztxold=9
              ;print,'different numbers of regions, FUV'
              endif else begin ;same sign of dpztx, same number of regions
                ;check CRS id, if it is already in the list
                ind = where(*(craster).FuvCRS eq simlog[fuvind].CRSid, count)
                if count eq 0 then begin ;not in the list, so we need to check whether the regions are the same
                  ;but for this we need to normalize the coordinates first according to summing !!!!!!!!!!!!!!!
                  ;but only if we change the simulator output
                  if Total(Abs(craster.FUVsr - simlog[fuvind].startrow) lt 0.1) ne 8 || $
                    Total(Abs(craster.FUVer - simlog[fuvind].endrow) lt 0.1) ne 8 || $
                    Total(Abs(craster.FUVsc - simlog[fuvind].startcol) lt 0.1) ne 8 || $
                    Total(Abs(craster.FUVec - simlog[fuvind].endcol) lt 0.1) ne 8 then begin
                    newraster=1
                    dpztxold=9
                  ;print,'different regions, FUV'
                  endif else addFUVcrs=1 ;check regions, same regions but different CRS
                endif ;check CRS ids already in the list
              endelse ;same sign of dpztx, same number of regions
            endif else addFUVregions=1 ;no FUV exposure before this step (in this raster), so we add it
          endif ;no FUV exposure in this step
          
          
          
          
          ;check if we have a NUV in this exposure
          addNUVcrs=0
          addNUVregions=0
          if nuvind ge 0 then begin ;there is a NUV exposure for this step
            if craster.NUVnRegions gt 0 then begin
              if craster.NUVnRegions ne simlog[nuvind].numreg then begin ;nr of regions unequal
                newraster=1
                dpztxold=9
              ;print,'different numbers of regions, NUV'
              endif else begin ;same sign of dpztx, same number of regions
                ;check CRS id, if it is already in the list
                ind = where(*(craster).NuvCRS eq simlog[nuvind].CRSid, count)
                if count eq 0 then begin ;not in the list, so we need to check whether the regions are the same
                  if Total(Abs(craster.NUVsr - simlog[nuvind].startrow) lt 0.1) ne 8 || $
                    Total(Abs(craster.NUVer - simlog[nuvind].endrow) lt 0.1) ne 8 || $
                    Total(Abs(craster.NUVsc - simlog[nuvind].startcol) lt 0.1) ne 8 || $
                    Total(Abs(craster.NUVec - simlog[nuvind].endcol) lt 0.1) ne 8 then begin
                    newraster=1
                    dpztxold=9
                  ;print,'different regions, NUV'
                  endif else addNUVcrs=1 ;check regions, same regions but different CRS
                endif ;check CRS ids already in the list
              endelse ;same sign of dpztx, same number of regions
            endif else addNUVregions=1 ;no NUV exposure before this step (in this raster), so we add it
          endif ;no NUV exposure in this step
          
          
        endelse ;same sign of dpztx
        
      endif else begin ;first take within the run
        newraster = 1
        dpztxold = 9
      endelse
      pztxold = simlog[logind].pztxy[0]
      
      
      
      if newraster then begin
        ; we have a new raster
        print, 'new', step, logind, fuvind, nuvind, sjiind
        nraster = craster
        
        ;initialize the new raster
        craster=rasterdef
        craster.nRasters = 1
        craster.nSteps = 1
        craster.PZTxy = ptr_new(dblarr(2,1))
        *(craster).PZTxy = simlog[logind].PZTXy
        craster.time = ptr_new(lonarr(1))
        *(craster).time = simlog[logind].SimTime
        craster.exptimeFUV = ptr_new(lonarr(1))
        *(craster).exptimeFUV = simlog[fuvind].ExpTime
        craster.exptimeNUV = ptr_new(lonarr(1))
        *(craster).exptimeNUV = simlog[nuvind].ExpTime
        craster.exptimeSJI = ptr_new(lonarr(1))
        *(craster).exptimeSJI = simlog[sjiind].ExpTime
        craster.sumspecFUV = ptr_new(lonarr(1))
        *(craster).sumspecFUV = simlog[fuvind].sumspec
        craster.sumspatFUV = ptr_new(lonarr(1))
        *(craster).sumspatFUV = simlog[fuvind].sumspat
        craster.sumspecNUV = ptr_new(lonarr(1))
        *(craster).sumspecNUV = simlog[nuvind].sumspec
        craster.sumspatNUV = ptr_new(lonarr(1))
        *(craster).sumspatNUV = simlog[nuvind].sumspat
        craster.sumspecSJI = ptr_new(lonarr(1))
        *(craster).sumspecSJI = simlog[sjiind].sumspec
        craster.sumspatSJI = ptr_new(lonarr(1))
        *(craster).sumspatSJI = simlog[sjiind].sumspat
        craster.FUVlogind = ptr_new(lonarr(1))
        *(craster).FUVlogind = fuvind
        craster.NUVlogind = ptr_new(lonarr(1))
        *(craster).NUVlogind = nuvind
        craster.SJIlogind = ptr_new(lonarr(1))
        *(craster).SJIlogind = sjiind
        
        if fuvind ge 0 then begin ;add FUV exposure to first position
          craster.FUVnRegions = simlog[fuvind].numreg
          craster.FuvCRS = ptr_new(strarr(1))
          *(craster).FuvCRS = simlog[fuvind].CRSid
          craster.FUVsr = simlog[fuvind].startrow
          craster.FUVer = simlog[fuvind].endrow
          craster.FUVsc = simlog[fuvind].startcol
          craster.FUVec = simlog[fuvind].endcol
        endif
        if nuvind ge 0 then begin ;add NUV exposure to first position
          craster.NUVnRegions = simlog[nuvind].numreg
          craster.NuvCRS = ptr_new(strarr(1))
          *(craster).NuvCRS = simlog[nuvind].CRSid
          craster.NUVsr = simlog[nuvind].startrow
          craster.NUVer = simlog[nuvind].endrow
          craster.NUVsc = simlog[nuvind].startcol
          craster.NUVec = simlog[nuvind].endcol
        endif
        
        
        
        
      endif else begin
        ;add to same raster
        ;print, 'old', step, logind, fuvind, nuvind, sjiind
      
        ; still in the same raster, add step to raster
        craster.nSteps = craster.nSteps + 1
        *(craster).PZTxy = [[*(craster).PZTxy], [simlog[logind].PZTXy]]
        *(craster).time = [*(craster).time, simlog[logind].SimTime]
        if fuvind ge 0 then begin
          *(craster).exptimeFUV = [*(craster).exptimeFUV, simlog[fuvind].ExpTime]
          *(craster).sumspecFUV = [*(craster).sumspecFUV, simlog[fuvind].sumspec]
          *(craster).sumspatFUV = [*(craster).sumspatFUV, simlog[fuvind].sumspat]
        endif else begin
          *(craster).exptimeFUV = [*(craster).exptimeFUV, 0]
          *(craster).sumspecFUV = [*(craster).sumspecFUV, 0]
          *(craster).sumspatFUV = [*(craster).sumspatFUV, 0]
        endelse
        if nuvind ge 0 then begin
          *(craster).exptimeNUV = [*(craster).exptimeNUV, simlog[nuvind].ExpTime]
          *(craster).sumspecNUV = [*(craster).sumspecNUV, simlog[nuvind].sumspec]
          *(craster).sumspatNUV = [*(craster).sumspatNUV, simlog[nuvind].sumspat]
        endif else begin
          *(craster).exptimeNUV = [*(craster).exptimeNUV, 0]
          *(craster).sumspecNUV = [*(craster).sumspecNUV, 0]
          *(craster).sumspatNUV = [*(craster).sumspatNUV, 0]
        endelse
        if sjiind ge 0 then begin
          *(craster).exptimeSJI = [*(craster).exptimeSJI, simlog[sjiind].ExpTime]
          *(craster).sumspecSJI = [*(craster).sumspecSJI, simlog[sjiind].sumspec]
          *(craster).sumspatSJI = [*(craster).sumspatSJI, simlog[sjiind].sumspat]
        endif else begin
          *(craster).exptimeSJI = [*(craster).exptimeSJI, 0]
          *(craster).sumspecSJI = [*(craster).sumspecSJI, 0]
          *(craster).sumspatSJI = [*(craster).sumspatSJI, 0]
        endelse
        *(craster).FUVlogind = [*(craster).FUVlogind, fuvind]
        *(craster).NUVlogind = [*(craster).NUVlogind, nuvind]
        *(craster).SJIlogind = [*(craster).SJIlogind, sjiind]
        
        if addFUVcrs then $
          *(craster).FuvCRS = [*(craster).FuvCRS, simlog[fuvind].CRSid]
        if addNUVcrs then $
          *(craster).NuvCRS = [*(craster).NuvCRS, simlog[nuvind].CRSid]
          
        if addFUVregions then begin
          craster.FUVnRegions = simlog[fuvind].numreg
          craster.FuvCRS = ptr_new(strarr(1))
          *(craster).FuvCRS = simlog[fuvind].CRSid
          craster.FUVsr = simlog[fuvind].startrow
          craster.FUVer = simlog[fuvind].endrow
          craster.FUVsc = simlog[fuvind].startcol
          craster.FUVec = simlog[fuvind].endcol
        endif
        if addNUVregions then begin
          craster.NUVnRegions = simlog[nuvind].numreg
          craster.NuvCRS = ptr_new(strarr(1))
          *(craster).NuvCRS = simlog[nuvind].CRSid
          craster.NUVsr = simlog[nuvind].startrow
          craster.NUVer = simlog[nuvind].endrow
          craster.NUVsc = simlog[nuvind].startcol
          craster.NUVec = simlog[nuvind].endcol
        endif
      endelse
      
    endif ;new step
    
    
    
    ;add raster to structure if necessary
    if (newraster && (step gt 0)) || (logind eq N_ELEMENTS(simlog)-1) then begin
      if logind eq N_ELEMENTS(simlog)-1 then nraster=craster
      
      ;we need to add the endtime of the raster
      nraster.endtime = ptr_new(lonarr(1))
      if max(*(nraster).time) eq laststeptime then begin
        *(nraster).endtime = simendtime
      endif else begin
        timeind = where(simlog.SimTime gt max(*(nraster).time))
        *(nraster).endtime = simlog[timeind[0]].SimTime
      endelse
      
      ;check if we already have a rastertype, if not create one
      if N_ELEMENTS(rasters) eq 0 then begin
        rasters = nraster
      endif else begin
        ;check if rastertype already exists, if yes, add it to the corresponding structure
        ;if no, add a new structure to structure array
        member=0
        for i=0,N_ELEMENTS(rasters)-1 do begin
          if nraster.nSteps eq rasters[i].nSteps && $
            nraster.FUVnRegions eq rasters[i].FUVnRegions && $
            nraster.NUVnRegions eq rasters[i].NUVnRegions && $
            Total(Abs(*(nraster).PZTxy - *(rasters[i]).PZTxy) lt 0.1) - 2*nraster.nSteps lt 0.1 && $
            Total(Abs(nraster.NUVsr - rasters[i].NUVsr) lt 0.1) - 8 lt 0.1 && $
            Total(Abs(nraster.NUVer - rasters[i].NUVer) lt 0.1) - 8 lt 0.1 && $
            Total(Abs(nraster.NUVsc - rasters[i].NUVsc) lt 0.1) - 8 lt 0.1 && $
            Total(Abs(nraster.NUVec - rasters[i].NUVec) lt 0.1) - 8 lt 0.1 then begin
            
            ;print, 'rastertype exists'
            member=1
            
            rasters[i].nRasters = rasters[i].nRasters + 1
            *(rasters[i]).FUVlogind = [[*(rasters[i]).FUVlogind], [*(nraster).FUVlogind]]
            *(rasters[i]).NUVlogind = [[*(rasters[i]).NUVlogind], [*(nraster).NUVlogind]]
            *(rasters[i]).SJIlogind = [[*(rasters[i]).SJIlogind], [*(nraster).SJIlogind]]
            *(rasters[i]).time = [[*(rasters[i]).time], [*(nraster).time]]
            *(rasters[i]).endtime = [*(rasters[i]).endtime, *(nraster).endtime]
            *(rasters[i]).exptimeFUV = [[*(rasters[i]).exptimeFUV], [*(nraster).exptimeFUV]]
            *(rasters[i]).exptimeNUV = [[*(rasters[i]).exptimeNUV], [*(nraster).exptimeNUV]]
            *(rasters[i]).exptimeSJI = [[*(rasters[i]).exptimeSJI], [*(nraster).exptimeSJI]]
            *(rasters[i]).sumspecFUV = [[*(rasters[i]).sumspecFUV], [*(nraster).sumspecFUV]]
            *(rasters[i]).sumspatFUV = [[*(rasters[i]).sumspatFUV], [*(nraster).sumspatFUV]]
            *(rasters[i]).sumspecNUV = [[*(rasters[i]).sumspecNUV], [*(nraster).sumspecNUV]]
            *(rasters[i]).sumspatNUV = [[*(rasters[i]).sumspatNUV], [*(nraster).sumspatNUV]]
            *(rasters[i]).sumspecSJI = [[*(rasters[i]).sumspecSJI], [*(nraster).sumspecSJI]]
            *(rasters[i]).sumspatSJI = [[*(rasters[i]).sumspatSJI], [*(nraster).sumspatSJI]]
            break
          endif
        endfor
        if ~member then begin
          ;print, 'new rastertype'
          rasters = [rasters, nraster]
        endif
      endelse ;added last raster to rasterstructure
    endif
    
    
  endfor ;next simlog entry
  
  
  ;calculate number of unique rasterpositions
  for i=0,N_ELEMENTS(rasters)-1 do begin
    temp = UNIQ((*(rasters[i]).PZTxy)[0,*], SORT((*(rasters[i]).PZTxy)[0,*]))
    rasters[i].rasterPos = N_ELEMENTS(temp)
    if rasters[i].rasterPos eq 1 then rasters[i].sitandstare=1
  endfor
  
  
  ;calculate cadence of rasters
  for rastertypenr=0,N_ELEMENTS(rasters)-1 do begin ;loop over all rastertypes
    cadpl=0
    for rasternr=0,rasters[rastertypenr].nRasters-1 do begin ;loop over all rasters within a rastertype
      cadpltemp = ((*(rasters[rastertypenr]).endtime)[rasternr] - (*(rasters[rastertypenr]).time)[0,rasternr]) / 1000.0
      cadpl = [cadpl, cadpltemp]
    endfor
    cadpl = cadpl[1:*]
    ; the endtime is calculated and might be wrong, so exclude last raster from calculation
    if N_ELEMENTS(cadpl) gt 1 then cadpl = cadpl[0:N_ELEMENTS(cadpl)-2]
    rasters[rastertypenr].cadpl_av = mean(cadpl)
    if N_ELEMENTS(cadpl) gt 1 then rasters[rastertypenr].cadpl_dv = stddev(cadpl) $
    else rasters[rastertypenr].cadpl_dv = 0
  endfor
  
  
  
  ;assign rastertype- and rasterrepetition number to missing files
  if l1to2log.nmissing gt 0 then begin
    for rastertypenr=0,N_ELEMENTS(rasters)-1 do begin ;loop over all rastertypes
      for rasternr=0,rasters[rastertypenr].nRasters-1 do begin ;loop over all rasters within a rastertype
        loginds = [(*(rasters[rastertypenr]).FUVlogind)[*, rasternr], $
          (*(rasters[rastertypenr]).NUVlogind)[*, rasternr], $
          (*(rasters[rastertypenr]).SJIlogind)[*, rasternr]]
        for i=0,l1to2log.nmissing-1 do begin
          dum = where(loginds eq l1to2log.nofilelogind[i], count)
          if count gt 0 then begin
            l1to2log.missrtype[i] = rastertypenr
            l1to2log.missrtrep[i] = rasternr
          endif
        endfor
      endfor
    endfor
  endif
  
  
  
  
  
  constants = obj_new('IRISsim_constants')
  
  
  ;let's make the rasters for the SJI
  sjiind = where(simlog.type eq 'SJI', count)
  
  if count gt 0 then begin
  
    ;define the structure for a SJI raster
    rasterSJIdef = {SJIlogind:ptr_new(), $
      SJInRegions:0, $
      SJICRS:'', $
      FW:0, $
      FWdesc:'', $
      nSteps:0L, $
      rasterPos:0L, $
      PZTxy:ptr_new(), $
      time:ptr_new(), $
      endtime:0L, $
      sumspec:ptr_new(), $
      sumspat:ptr_new(), $
      exptime:ptr_new()}
      
    rasterFWdef = {rasterCRS:ptr_new(), $
      nRasters:0L}
      
      
    crasterSJI = rasterSJIdef
    
    
    ;get different filterwheel positions for this CRS
    sjicrsfw = simlog[sjiind[UNIQ(simlog[sjiind].FW, SORT(simlog[sjiind].FW))]].FW
    
    rastersSJI = make_array(N_ELEMENTS(sjicrsfw), value=rasterFWdef)
    
    for sjicrsfwind=0,N_ELEMENTS(sjicrsfw)-1 do begin
      sjiind2 = sjiind[where(simlog[sjiind].FW eq sjicrsfw[sjicrsfwind])]
      
      ;get different crs
      sjicrs = simlog[sjiind2[UNIQ(simlog[sjiind2].CRSid, SORT(simlog[sjiind2].CRSid))]].CRSid
      
      rastersSJI[sjicrsfwind].nRasters = N_ELEMENTS(sjicrs)
      rastersSJI[sjicrsfwind].rasterCRS = ptr_new(make_array(N_ELEMENTS(sjicrs), value=rasterSJIdef))
      
      for sjicrsind=0,N_ELEMENTS(sjicrs)-1 do begin
        sjiind3 = sjiind2[where(simlog[sjiind2].CRSid eq sjicrs[sjicrsind])]
        ;print,sjiind3, simlog[sjiind3].crsid,simlog[sjiind3].fw
        
        crasterSJI.SJIlogind = ptr_new(sjiind3)
        crasterSJI.SJInRegions = simlog[sjiind3[0]].numreg
        crasterSJI.SJICRS = sjicrs[sjicrsind]
        crasterSJI.FW = sjicrsfw[sjicrsfwind]
        crasterSJI.FWdesc = constants->get_FWname(sjicrsfw[sjicrsfwind], /noAngstrom, /frequency)
        crasterSJI.nSteps = N_ELEMENTS(sjiind3)
        crasterSJI.PZTxy = ptr_new(simlog[sjiind3].PZTxy)
        crasterSJI.time = ptr_new(simlog[sjiind3].SimTime)
        if max(simlog[sjiind3].SimTime) eq laststeptime then begin
          crasterSJI.endtime = simendtime
        endif else begin
          timeind = where(simlog.SimTime gt max(simlog[sjiind3].SimTime))
          crasterSJI.endtime = simlog[timeind[0]].SimTime
        endelse
        crasterSJI.exptime = ptr_new(simlog[sjiind3].ExpTime)
        crasterSJI.sumspec = ptr_new(simlog[sjiind3].sumspec)
        crasterSJI.sumspat = ptr_new(simlog[sjiind3].sumspat)
        temp = UNIQ((*(crasterSJI).PZTxy)[0,*], SORT((*(crasterSJI).PZTxy)[0,*]))
        crasterSJI.rasterPos = N_ELEMENTS(temp)
        
        (*rastersSJI[sjicrsfwind].rasterCRS)[sjicrsind] = crasterSJI
        
      endfor ;sjicrsind=0,N_ELEMENTS(sjicrs)-1
    endfor ;sjicrsfwind=0,N_ELEMENTS(sjicrsfw)-1
    
  endif;there are SJI
  
  
  
  
  ;assign FW-CRS-type number to missing files
  if l1to2log.nmissing gt 0 then begin
    for nrFW=0,N_ELEMENTS(rastersSJI)-1 do begin
      for nrCRS=0,rastersSJI[nrFW].nrasters-1 do begin
        loginds = *((*rastersSJI[nrFW].rastercrs)[nrCRS]).SJIlogind
        for i=0,l1to2log.nmissing-1 do begin
          dum = where(loginds eq l1to2log.nofilelogind[i], count)
          if count gt 0 then begin
            l1to2log.missfwt[i] = nrCRS
          endif
        endfor
      endfor
    endfor
  endif
  
  obj_destroy, constants
  
END
