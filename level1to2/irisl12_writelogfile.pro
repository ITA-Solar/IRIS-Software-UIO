pro IRISl12_writeLogFile, l1to2log, OBSvars, filename, memlog=memlog, timestr=timestr, traceback=traceback, version=version, $
  SJIonly=SJIonly, spectralonly=spectralonly, files=files, obstab=obstab
  ; $Id: irisl12_writelogfile.pro,v 1.23 2018/03/23 10:06:33 mawiesma Exp $  ;
  
  method = ''
  if keyword_set(SJIonly) then method='_sji'
  if keyword_set(spectralonly) then method='_raster'
  
  if keyword_set(traceback) then begin
    file=filename+method+'.errorlog'
    OpenW, unit, file, /get_lun
    if keyword_set(version) then printf, unit, version
    for i=0,N_ELEMENTS(traceback)-1 do printf, unit, traceback[i]
    Close, unit
    free_lun, unit
    if keyword_set(files) then begin
      if keyword_set(obstab) then save, files, obstab, filename=filename+'.sav' $
      else save, files, filename=filename+'.sav'
    endif else begin
      if keyword_set(obstab) then save, obstab, filename=filename+'.sav'
    endelse
  endif
  
  if N_ELEMENTS(l1to2log) gt 0 then begin
    l1to2log.OBSstart = OBSvars.OBSstart
    l1to2log.OBSid = OBSvars.OBSid
    l1to2log.OBSend = OBSvars.OBSend
    l1to2log.version = OBSvars.version
    
    file = filename + method + '.log'
    
    OpenW, unit, file, /get_lun
    log='Log of IRIS level 1 to 2, Version: ' + l1to2log.version
    printf, unit, log
    log='Date: ' + l1to2log.date
    printf, unit, log
    log='OBS ID             : ' + l1to2log.OBSid
    printf, unit, log
    log='OBS Start          : ' + l1to2log.OBSstart
    printf, unit, log
    log='OBS End            : ' + l1to2log.OBSend
    printf, unit, log
    log='recieved files     : '+STRING(l1to2log.filesreceived, format='(I7)')
    printf, unit, log
    log='expected files     : '+string(l1to2log.filesexpected, format='(I7)')
    printf, unit, log
    log='reported missing   : '+string(l1to2log.nmissing, format='(I7)')+$
      ' ('+string(double(l1to2log.nmissing)/double(l1to2log.filesexpected)*100d, format='(f4.1)')+'%)'
    printf, unit, log
    log='unused files       : '+string(l1to2log.nunused, format='(I7)')+$
      ' ('+string(double(l1to2log.nunused)/double(l1to2log.filesreceived)*100d, format='(f4.1)')+'%)'
    printf, unit, log
    log='bad files          : '+string(l1to2log.nbadfiles, format='(I7)')+$
      ' ('+string(double(l1to2log.nbadfiles)/double(l1to2log.filesreceived)*100d, format='(f4.1)')+'%)'
    printf, unit, log
    mem=l1to2log.memory
    memst=0B
    while mem gt 1024d do begin
      mem = mem / 1024d
      memst = memst + 1B
    endwhile
    case memst of
      0: memunit='B'
      1: memunit='kB'
      2: memunit='MB'
      3: memunit='GB'
      4: memunit='TB'
      5: memunit='PB'
      else: memunit='?B'
    endcase
    if mem ge 10d then format='(I4)' $
    else format='(F3.1)'
    log = 'memory used        : ' + string(mem, format=format)+' '+memunit
    memlog=log
    printf, unit, log
    totaltimetext = IRIS_humanreadabletime(l1to2log.timemeasure.total)
    totaltimetextlength = strlen(totaltimetext)
    log = 'time used          : ' + totaltimetext
    timestr=log
    printf, unit, log
    log='======================================================================================================================================'
    printf, unit, log
    log = 'reading header     : ' + IRIS_humanreadabletime(l1to2log.timemeasure.readheader, totaltimetextlength=totaltimetextlength) + $
      '   (' + string(l1to2log.timemeasure.readheader/l1to2log.timemeasure.total*100, format='(f4.1)') + '%)'
    printf, unit, log
    log = 'xml files          : ' + IRIS_humanreadabletime(l1to2log.timemeasure.xmlfiles, totaltimetextlength=totaltimetextlength) + $
      '   (' + string(l1to2log.timemeasure.xmlfiles/l1to2log.timemeasure.total*100, format='(f4.1)') + '%)'
    printf, unit, log
    log = 'saveSpectralData   : ' + IRIS_humanreadabletime(l1to2log.timemeasure.saveSpectralData, totaltimetextlength=totaltimetextlength) + $
      '   (' + string(l1to2log.timemeasure.saveSpectralData/l1to2log.timemeasure.total*100, format='(f4.1)') + '%)'
    printf, unit, log
    log = 'spectral statistics: ' + IRIS_humanreadabletime(l1to2log.timemeasure.specStats, totaltimetextlength=totaltimetextlength) + $
      '   (' + string(l1to2log.timemeasure.specStats/l1to2log.timemeasure.total*100, format='(f4.1)') + '%)'
    printf, unit, log
    log = 'saveSJIData        : ' + IRIS_humanreadabletime(l1to2log.timemeasure.saveSJIData, totaltimetextlength=totaltimetextlength) + $
      '   (' + string(l1to2log.timemeasure.saveSJIData/l1to2log.timemeasure.total*100, format='(f4.1)') + '%)'
    printf, unit, log
    log = 'SJI statistics     : ' + IRIS_humanreadabletime(l1to2log.timemeasure.SJIStats, totaltimetextlength=totaltimetextlength) + $
      '   (' + string(l1to2log.timemeasure.SJIStats/l1to2log.timemeasure.total*100, format='(f4.1)') + '%)'
    printf, unit, log
    log = 'reading/iris_prep  : ' + IRIS_humanreadabletime(l1to2log.timemeasure.readfile, totaltimetextlength=totaltimetextlength) + $
      '   (' + string(l1to2log.timemeasure.readfile/l1to2log.timemeasure.total*100, format='(f4.1)') + '%)'
    printf, unit, log
    log = 'writing files      : ' + IRIS_humanreadabletime(l1to2log.timemeasure.writefile, totaltimetextlength=totaltimetextlength) + $
      '   (' + string(l1to2log.timemeasure.writefile/l1to2log.timemeasure.total*100, format='(f4.1)') + '%)'
    printf, unit, log
    rest = l1to2log.timemeasure.total - $
      l1to2log.timemeasure.readheader - l1to2log.timemeasure.xmlfiles - l1to2log.timemeasure.saveSpectralData - $
      l1to2log.timemeasure.saveSJIData
    log = 'rest               : ' + IRIS_humanreadabletime(rest, totaltimetextlength=totaltimetextlength) + $
      '   (' + string(rest/l1to2log.timemeasure.total*100, format='(f4.1)') + '%)'
    printf, unit, log
    log='======================================================================================================================================'
    printf, unit, log
    if l1to2log.nmissing gt 0 then begin
      log='missing files: (steps: 0-'+strcompress(l1to2log.totalsteps, /remove_all)+')'
      printf, unit, log
      log='  step |        approx. OBS_DATE | Typ |     FRM ID |     FDB ID |     CRS ID | Orep | Oent | Frep | Fent | Rty |  Rrep |   FW | FWt |'
      printf, unit, log
      log='======================================================================================================================================'
      printf, unit, log
      tstart = str2utc(l1to2log.OBSstart)
      for i=0,l1to2log.nmissing-1 do begin
        OBSdate=l1to2log.missdate[i]
        for m=strlen(OBSdate),22 do OBSdate=' '+OBSdate
        FRMid=l1to2log.missfrm[i]
        for m=strlen(FRMid),9 do FRMid=' '+FRMid
        FDBid=l1to2log.missfdb[i]
        for m=strlen(FDBid),9 do FDBid=' '+FDBid
        CRSid=l1to2log.misscrs[i]
        for m=strlen(CRSid),9 do CRSid=' '+CRSid
        fw=l1to2log.missfw[i]
        for m=strlen(fw),3 do fw=' '+fw
        
        log=string(l1to2log.missstep[i], format='(I6)')+' | '+$
          OBSdate+' | '+$
          l1to2log.misstype[i]+' | '+$
          FRMid+' | '+$
          FDBid+' | '+$
          CRSid+' | '+$
          string(l1to2log.missOBSrep[i], format='(I4)')+' | '+$
          string(l1to2log.missOBSentry[i], format='(I4)')+' | '+$
          string(l1to2log.missFRMrep[i], format='(I4)')+' | '+$
          string(l1to2log.missFRMentry[i], format='(I4)')+' | '+$
          string(l1to2log.missrtype[i], format='(I3)')+' | '+$
          string(l1to2log.missrtrep[i], format='(I5)')+' | '+$
          fw+' | '+$
          string(l1to2log.missfwt[i], format='(I3)')+' | '
        printf, unit, log
      endfor
      log='======================================================================================================================================'
      printf, unit, log
    endif ;l1to2log.nmissing gt 0
    if l1to2log.nunused gt 0 then begin
      log='unused files: (nr: 0-'+strcompress(l1to2log.filesreceived-1, /remove_all)+')'
      printf, unit, log
      log='    nr |                OBS_DATE | Typ |     FRM ID |     FDB ID |     CRS ID | Orep | Oent | Frep | Fent |     OBS ID | file'
      printf, unit, log
      log='======================================================================================================================================'
      printf, unit, log
      for i=0,l1to2log.nunused-1 do begin
        OBSdate=l1to2log.unuseddate[i]
        for m=strlen(OBSdate),22 do OBSdate=' '+OBSdate
        OBSid=l1to2log.unusedobsid[i]
        for m=strlen(OBSid),9 do OBSid=' '+OBSid
        type=l1to2log.unusedtype[i]
        for m=strlen(type),2 do type=' '+type
        FRMid=l1to2log.unusedfrmid[i]
        for m=strlen(FRMid),9 do FRMid=' '+FRMid
        FDBid=l1to2log.unusedfdbid[i]
        for m=strlen(FDBid),9 do FDBid=' '+FDBid
        CRSid=l1to2log.unusedcrsid[i]
        for m=strlen(CRSid),9 do CRSid=' '+CRSid
        
        log=string(l1to2log.unusedfilesind[i], format='(I6)')+' | '+$
          OBSdate+' | '+$
          type+' | '+$
          FRMid+' | '+$
          FDBid+' | '+$
          CRSid+' | '+$
          string(l1to2log.unusedOBSrep[i], format='(I4)')+' | '+$
          string(l1to2log.unusedOBSentry[i], format='(I4)')+' | '+$
          string(l1to2log.unusedFRMrep[i], format='(I4)')+' | '+$
          string(l1to2log.unusedFRMentry[i], format='(I4)')+' | '+$
          OBSid+' | '+$
          l1to2log.unusedfiles[i]
        printf, unit, log
      endfor
      log='======================================================================================================================================'
      printf, unit, log
    endif ;l1to2log.nunused gt 0
    if l1to2log.possiblefits[0] ne '' then begin
      log='files that are unused, but possibly are one of the missing files according to DATE_OBS:
      printf, unit, log
      for i=0,N_ELEMENTS(l1to2log.possiblefits)-1 do begin
        log=l1to2log.possiblefits[i]
        printf, unit, log
      endfor
      log='======================================================================================================================================'
      printf, unit, log
    endif ;l1to2log.possiblefits[0] ne ''
    if l1to2log.nbadfiles gt 0 then begin
      log='bad l1 files:
      printf, unit, log
      for i=0,l1to2log.nbadfiles-1 do begin
        log=(*l1to2log.badfilesreason)[i] + ' | ' + (*l1to2log.badfiles)[i]
        printf, unit, log
      endfor
      log='======================================================================================================================================'
      printf, unit, log
    endif ;l1to2log.badfiles[0] ne ''
    Close, unit
    free_lun, unit
  endif
end
