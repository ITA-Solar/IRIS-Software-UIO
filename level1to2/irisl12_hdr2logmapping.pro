PRO IRISl12_hdr2logMapping, simlog, hdr, files, hdrmap, l1to2log, timemeasure, level2dir=level2dir
  ; $Id: irisl12_hdr2logmapping.pro,v 1.4 2018/03/23 10:06:33 mawiesma Exp $
  
  ;this procedure returns a log-to-fitsheader-mapping
  
  
  ;start time of OBS
  dateok=0
  ;check if the output folder contains a valid date, if yes, use that one
  if keyword_set(level2dir) then begin
    temp = extract_fids(file_basename(level2dir), fidfound=fidfound)
    if fidfound then begin
      OBSstart = file2time(file_basename(level2dir))
      trealstart = str2utc(OBSstart)
      ;dateok=1
    endif
  endif  
  
  
  
  
  
  fileused = bytarr(N_ELEMENTS(hdr))
  hdrmap = lonarr(N_ELEMENTS(simlog))
  for logind=0,N_ELEMENTS(simlog)-1 do begin
  
    hdrind = where((hdr.ISQOLTID eq simlog[logind].OBSid) AND $
      (hdr.IIOLRPT eq simlog[logind].OBSrep) AND $
      (hdr.ISQOLTDX eq simlog[logind].OBSentry) AND $
      (hdr.IIFLRPT eq simlog[logind].FRMrep) AND $
      (hdr.ISQFLTDX eq simlog[logind].FRMentry) AND $
      (hdr.INSTRUME eq simlog[logind].Type), count)
      
    if count eq 1 then begin
      ;there is exactly one file which fulfills the criteria
      ;so it's the one we want
      hdrmap[logind] = hdrind[0]
      fileused[hdrind[0]] = 1
      
    endif else if count eq 0 then begin
      ;no files found fulfilling the criteria
      ;might be a missing file or a missing/corrupt header
      ;we check for that later
      hdrmap[logind] = -1
      
    endif else begin
      ;several files found, which all fulfill the criteria
      ;files from different executions of same/different OBS or corrupt header?
      print,'more than one file fits the counters'
      print,'checking which one is the right one', logind, hdrind
      hdrind2 = where((hdr[hdrind].ISQFLTID eq simlog[logind].FRMid) AND $
        (hdr[hdrind].IIFDBID eq simlog[logind].FDBid) AND $
        (hdr[hdrind].IICRSID eq simlog[logind].CRSid), count2)
        
      if count2 eq 1 then begin
        ;found the correct file
        print,'found the right one, according to FRM-, FDB- and CRS-IDs'
        hdrmap[logind] = hdrind[hdrind2[0]]
        fileused[hdrind[hdrind2[0]]] = 1
        
      endif else if count2 gt 1 then begin
        ;more than one file fits the criteria
        ;file might be multiple times in the list
        
        uniqind = uniq(files[hdrind[hdrind2]], sort(files[hdrind[hdrind2]]))
        if N_ELEMENTS(uniqind) eq 1 then begin
          print,'a file is multiple times in the list'
          print,files[hdrind[hdrind2]]
          print,'picking the first one'
          hdrmap[logind] = hdrind[hdrind2[0]]
          fileused[hdrind[hdrind2[0]]] = 1
        endif else begin
        
          if N_ELEMENTS(trealstart) gt 0 then begin
            t2 = gt_tagval(hdr[hdrind[hdrind2]], 'DATE_OBS', missing='')
            ind = where(~valid_time(t2), count)
            if count gt 0 then t2[ind] = gt_tagval(hdr[hdrind[hdrind2[ind]]], 'T_OBS', missing='')
            ind = where(valid_time(t2), count)
            if count gt 0 then begin
              t2 = str2utc(t2[ind])
              time = (t2.mjd-trealstart.mjd)*86400000L + t2.time-trealstart.time
              ;check if the time fits with the time from the simulation (+-2s)
              simind = where(abs(simlog[logind].simtime-time) lt 2000, count)
              if count eq 1 then begin
                print,'found fitting file according to time'
                hdrmap[logind] = hdrind[hdrind2[ind[0]]]
                fileused[hdrind[hdrind2[ind[0]]]] = 1
              endif else if count gt 1 then begin
                mi = min(abs(simlog[logind].simtime-time), minind)
                print,'found fitting files according to time, picking the closest one'
                hdrmap[logind] = hdrind[hdrind2[ind[minind]]]
                fileused[hdrind[hdrind2[ind[minind]]]] = 1
              endif else begin
                print,'multiple files fit to the searched keywords, but none of them has the right time stamp'
              endelse
            endif
          endif
        endelse
        
      endif else begin
        ;none of the fits files fit to these criteria
        ;we revisit these files further down
        hdrmap[logind] = -1
        
      endelse
      
    endelse
    
    if ~dateok then begin
      if hdrmap[logind] ge 0 then begin
        temp = gt_tagval(hdr[hdrmap[logind]], /DATE_OBS)
        if valid_time(temp) then begin
          OBSstart = temp
          trealstart = str2utc(OBSstart)
          trealstart.time = trealstart.time - simlog[logind].simtime
          while trealstart.time lt 0 do begin
            trealstart.time = trealstart.time + 86400000L
            trealstart.mjd = trealstart.mjd - 1
          endwhile
          dateok=1
        endif
      endif
    endif
    
  endfor
  
  
  
  
  ;checking the unused files, if they fit to a simlog-entry without a file association
  unused = where((fileused eq 0) AND (hdr.ISQOLTID gt 0), nunused)
  nofile = where(hdrmap eq -1, nnofile)
  if (nunused gt 0) && (nnofile gt 0) then begin
    if N_ELEMENTS(trealstart) gt 0 then begin
      for filenr=0,nunused-1 do begin
        t2 = gt_tagval(hdr[unused[filenr]], 'DATE_OBS', missing='')
        if ~valid_time(t2) then $
          t2 = gt_tagval(hdr[unused[filenr]], 'T_OBS', missing='')
        if valid_time(t2) then begin
          t2 = str2utc(t2)
          time = (t2.mjd-trealstart.mjd)*86400000L + t2.time-trealstart.time
          ;check if the time fits with the time from the simulation (+-2s)
          simind = where(abs(simlog[nofile].simtime-time) lt 2000, count)
          if count gt 0 then begin
            ;file might fit to one of the simlog entries...
            print,'found a file which might be one of the missing files'
            print,files[unused[filenr]]
            if N_ELEMENTS(possiblefits) eq 0 then possiblefits=files[unused[filenr]] $
            else possiblefits=[possiblefits, files[unused[filenr]]]
          endif
        endif
      endfor
    endif
  endif
  if N_ELEMENTS(possiblefits) eq 0 then possiblefits=''
  
  
  
  
  
  
  constants = obj_new('IRISsim_constants')
  
  ;get information about unused files
  unused = where(fileused eq 0, nunused)
  if nunused gt 0 then begin
    unusedfiles = files[unused]
    read_iris, files[unused], hdrunused, /nodata
    unuseddate = hdrunused.DATE_OBS
    unusedtype = hdrunused.INSTRUME
    unusedobsid = strcompress(hdrunused.ISQOLTID, /remove_all)
    if isnumeric(unusedobsid) then begin
      for i=0,N_ELEMENTS(unusedobsid)-1 do begin
        temp=long64(unusedobsid[i])
        if (temp ge 0LL) && (temp le 9999999999LL) then unusedobsid[i]=fns64('##########', temp)
      endfor
    endif
    unusedfrmid = strcompress(hdrunused.ISQFLTID, /remove_all)
    if isnumeric(unusedfrmid) then begin
      for i=0,N_ELEMENTS(unusedfrmid)-1 do begin
        temp=long64(unusedfrmid[i])
        if (temp ge 0LL) && (temp le 9999999999LL) then unusedfrmid[i]=fns64('##########', temp)
      endfor
    endif
    unusedfdbid = strcompress(hdrunused.IIFDBID, /remove_all)
    if isnumeric(unusedfdbid) then begin
      for i=0,N_ELEMENTS(unusedfdbid)-1 do begin
        temp=long64(unusedfdbid[i])
        if (temp ge 0LL) && (temp le 9999999999LL) then unusedfdbid[i]=fns64('##########', temp)
      endfor
    endif
    unusedcrsid = strcompress(hdrunused.IICRSID, /remove_all)
    if isnumeric(unusedcrsid) then begin
      for i=0,N_ELEMENTS(unusedcrsid)-1 do begin
        temp=long(unusedcrsid[i])
        if (temp ge 0L) && (temp le 99999L) then unusedcrsid[i]=fns('#####', temp)
      endfor
    endif
    unusedOBSrep = hdrunused.IIOLRPT
    unusedOBSentry = hdrunused.ISQOLTDX
    unusedFRMrep = hdrunused.IIFLRPT
    unusedFRMentry = hdrunused.ISQFLTDX
  endif else begin
    unusedfiles=''
    unuseddate = ''
    unusedtype = ''
    unusedobsid = ''
    unusedfrmid = ''
    unusedfdbid = ''
    unusedcrsid = ''
    unusedOBSrep = -1
    unusedOBSentry = -1
    unusedFRMrep = -1
    unusedFRMentry = -1
  endelse
  
  
  
  
  
  ;get information about missing files
  nofile = where(hdrmap eq -1, nnofile)
  if nnofile gt 0 then begin
    missstep = simlog[nofile].step
    misstype = simlog[nofile].type
    missfrm = simlog[nofile].FRMid
    missfdb = simlog[nofile].FDBid
    misscrs = simlog[nofile].CRSid
    missdate = strarr(nnofile)
    if N_ELEMENTS(trealstart) gt 0 then begin
      for i=0,nnofile-1 do begin
        time = {mjd:0L, time:simlog[nofile[i]].SimTime}
        time = addmjd(trealstart, time)
        missdate[i] = utc2str(time)
      endfor
    endif
    missrtype = lonarr(nnofile)
    missrtrep = lonarr(nnofile)
    missfw = make_array(nnofile, value='----')
    sjiind = where(simlog[nofile].type eq 'SJI', count)
    for i=0,count-1 do begin
      missfw[sjiind[i]] = constants->get_FWname(simlog[nofile[sjiind[i]]].fw, /noAngstrom, /frequency)
    endfor
    missfwt = make_array(nnofile, value=-1)
    ;missfwt[sjiind] = 9
    missOBSrep = simlog[nofile].OBSrep
    missOBSentry = simlog[nofile].OBSentry
    missFRMrep = simlog[nofile].FRMrep
    missFRMentry = simlog[nofile].FRMentry
  endif else begin
    missstep = -1
    misstype = ''
    missfrm = ''
    missfdb = ''
    misscrs = ''
    missdate = ''
    missrtype = -1
    missrtrep = -1
    missfw = ''
    missfwt = -1
    missOBSrep = -1
    missOBSentry = -1
    missFRMrep = -1
    missFRMentry = -1
  endelse
  
  
  
  
  ;bad l1 files
  indbadfiles = where(hdr.ISQOLTID eq 0, nbadfiles)
  if nbadfiles gt 0 then begin
    badfiles = files[indbadfiles]
    badfilesreason = replicate('ISQOLTID eq 0', nbadfiles)
  endif else begin
    badfiles = ''
    badfilesreason = ''
  endelse
  
  
  
  
  ;set the log structure
  l1to2log = { OBSid:'', $
    OBSstart:'', $
    OBSend:'', $
    version:'', $
    date:'', $
    nmissing:nnofile, $
    nofilelogind:nofile, $
    missstep:missstep, $
    misstype:misstype, $
    missfrm:missfrm, $
    missfdb:missfdb, $
    misscrs:misscrs, $
    missdate:missdate, $
    missrtype:missrtype, $
    missrtrep:missrtrep, $
    missfw:missfw, $
    missfwt:missfwt, $
    missOBSrep:missOBSrep, $
    missOBSentry:missOBSentry, $
    missFRMrep:missFRMrep, $
    missFRMentry:missFRMentry, $
    nunused:nunused, $
    unusedfilesind:unused, $
    unusedfiles:unusedfiles, $
    unuseddate:unuseddate, $
    unusedtype:unusedtype, $
    unusedobsid:unusedobsid, $
    unusedfrmid:unusedfrmid, $
    unusedfdbid:unusedfdbid, $
    unusedcrsid:unusedcrsid, $
    unusedOBSrep:unusedOBSrep, $
    unusedOBSentry:unusedOBSentry, $
    unusedFRMrep:unusedFRMrep, $
    unusedFRMentry:unusedFRMentry, $
    possiblefits:possiblefits, $
    nbadfiles:nbadfiles, $
    badfiles:ptr_new(badfiles), $
    badfilesreason:ptr_new(badfilesreason), $
    filesreceived:N_ELEMENTS(files), $
    filesexpected:N_ELEMENTS(simlog), $
    totalsteps:simlog[N_ELEMENTS(simlog)-1].step, $
    memory:0LL, $
    timemeasure:timemeasure }
    
    
  obj_destroy, constants
END
