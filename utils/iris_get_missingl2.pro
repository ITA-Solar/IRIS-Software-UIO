;+
; NAME:
;       IRIS_get_missingl2
;
; PURPOSE:
;       IRIS_get_missingl2 is used to check for missing level 2 IRIS data, and which OBS crashed or didn't complete
;
; CATEGORY:
;       Martin Wiesmann / IRIS Data processing
;
; CALLING SEQUENCE:
;       IRIS_get_missingl2, days, logdestination=logdestination, startday=startday, stopday=stopday, files=files, $
;         lmsal=lmsal, path=path, filter=filter, maxline=maxline
;
; INPUTS:
;       days: number of days to be checked, default is 30, is ignored if both startday and stopday are given
;       logdestination: path to an existing directory in which the log file will be saved, 
;         default is '/sanhome2/mawiesma/iris/missingl2logs/' if /lmsal is set, 
;         or '~/iris/missingl2logs/' if /lmsal is not set
;
; OPTIONAL KEYWORD PARAMETERS:
;       startday: the startday to be checked, calculated if not given, stopday-days
;       stopday: the stopday to be checked, calculated if not given, startday+days, or current day if startday is not given
;       path: path to be checked, default is '/irisa/data/level2/' if /lmsal is set, or '/mn/xsan/d2/iris/data/level2/' if /lmsal is not set
;       filter: default is *.errorlog, doesn't need to be changed
;       maxline: maximum number of lines per errorlog written into log and on command line, default is 33
;       lmsal: set to 1 if this tool is run at LMSAL and path and/or logdestination or not set,
;         if not set, path and logdestination defaults to a directory at UIO
;
; OUTPUTS:
;       a log file will be written to the logdestination
;       files: string array containing all the errorlogs which were found
;
; CALLS:
;
; COMMON BLOCKS:
;       none
;
; PROCEDURE:
;       IRIS_get_missingl2
;
; RESTRICTIONS:
;
;
; MODIFICATION HISTORY:
;       5-Nov-2013: Martin Wiesmann (ITA, UIO).
;
; $Id: iris_get_missingl2.pro,v 1.28 2015/11/03 14:05:38 mawiesma Exp $



pro IRIS_get_missingl2, days, logdestination=logdestination, startday=startday, stopday=stopday, files=files, $
  lmsal=lmsal, path=path, filter=filter, maxline=maxline
  
  if N_PARAMS() eq 0 then days=30
  if ~keyword_set(path) then $
    if keyword_set(lmsal) then path='/irisa/data/level2/' $
    else path='/mn/stornext/d10/HDC2/iris/data/level2/'
  if ~keyword_set(logdestination) then $
    if keyword_set(lmsal) then logdestination='/sanhome2/mawiesma/iris/missingl2logs/' $
    else logdestination='~/iris/missingl2logs/'
  if ~keyword_set(filter) then filter='*.errorlog'
  if ~keyword_set(maxline) then maxline=33
  
  case !version.os of
    'MacOS': cr=string(13b)
    'Win32': cr=string(13b)+string(10b)
    else: cr=string(10b)
  endcase
  dirsep = path_sep()
  
  GET_UTC, currenttime, /stime, /truncate
  
  if keyword_set(startday) then begin
    tstartval = startday
    if keyword_set(stopday) then begin
      tstopval = stopday
    endif else begin
      temp = str2utc(tstartval)
      temp.mjd = temp.mjd+days
      tstopval = utc2str(temp, /STIME, /truncate)
    endelse
  endif else begin
    if keyword_set(stopday) then begin
      tstopval = stopday
      temp = str2utc(tstopval)
      temp.mjd = temp.mjd-days
      tstartval = utc2str(temp, /STIME, /truncate)
    endif else begin
      tstopval = currenttime
      temp = str2utc(tstopval)
      temp.mjd = temp.mjd-days
      tstartval = utc2str(temp, /STIME, /truncate)
    endelse
  endelse
  
  strtemp = 'looking in the path '+path
  print, strtemp
  inputstring = strtemp
  strtemp = 'looking in the time interval from '+tstartval+' to '+tstopval
  print, strtemp
  inputstring = [inputstring, strtemp]
  
  goodOBSall=0L
  log=''
  day = reltime(reltime(tstartval, /days, /day_only), days=-1, /day_only)
  paths = ssw_time2paths(tstartval, tstopval, path)
  for p=0,N_ELEMENTS(paths)-1 do begin
    goodOBS=0
    strtemp=''
    print,strtemp
    log=[log, strtemp]
    strtemp='looking in directory: '+paths[p]
    print,strtemp
    log=[log,strtemp]
    if p gt 0 then day = reltime(day, /days, /day_only)
    tl = iris_time2timeline(day)
    strtemp='there should be '+string(N_ELEMENTS(tl), format='(I3)')
    print,strtemp
    log=[log,strtemp]
    nmissing=0
    ntoomany=0
    if file_test(paths[p]) then begin
      if strmid(paths[p], 0,1, /reverse_offset) ne dirsep then paths[p] = paths[p]+dirsep
      paths[p] = paths[p]+'*'
      dirs = file_search(paths[p], /test_dir)
      dirs = strtrim(dirs,2)
      if N_ELEMENTS(dirs) eq 1 && dirs eq '' then dirs=!NULL
      strtemp='found           '+string(N_ELEMENTS(dirs), format='(I3)')
      print,strtemp
      log=[log,strtemp]
      nmissing = N_ELEMENTS(tl) - N_ELEMENTS(dirs)
      if nmissing lt 0 then begin
        strtemp='too many directories here, according to timeline'
        print,strtemp
        log=[log,strtemp]
        nmissing=0
      endif ;else if nmissing gt 0 then begin
      haveitTL=intarr(N_ELEMENTS(tl))
      if N_ELEMENTS(dirs) gt 0 then haveitDIRS=intarr(N_ELEMENTS(dirs))
      if size(tl, /type) eq 8 then begin
        tldates=anytim(tl.date_obs, out_style='mjd')
        ;for iii=0,N_ELEMENTS(tl)-1 do print,tl[iii].date_obs+'  '+strtrim(string(tldates[iii].mjd),2)+'  '+strtrim(string(tldates[iii].time),2)+'  '+strtrim(string(tl[iii].obsid),2)
        ;print,''
        for dd=0,N_ELEMENTS(dirs)-1 do begin
          filedate=file2time(dirs[dd], out_style='mjd')
          fileobs=strsplit(dirs[dd],'_',/extract)
          fileobs=ulong(fileobs[N_ELEMENTS(fileobs)-1])
          ;print,file2time(dirs[dd])+'  '+strtrim(string(filedate.mjd),2)+'  '+strtrim(string(filedate.time),2)+'  '+strtrim(string(fileobs),2)
          ind = where((tl.OBSID eq fileobs) AND $
            (tldates.mjd eq filedate.mjd) AND $
            (abs(tldates.time-filedate.time) lt 1000000L), count)
          if count eq 1 then begin
            haveitTL[ind]=1
            haveitDIRS[dd]=1
          endif
          ;print,ind
        endfor
        ind = where(haveitTL eq 0, count)
        nmissing=count
        if count gt 0 then begin
          strtemp='missing OBS:'
          print,strtemp
          log=[log,strtemp]
          for m=0,count-1 do begin
            strtemp=tl[ind[m]].date_obs+'  '+strcompress(string(tl[ind[m]].obsid),/remove_all)
            print,strtemp
            log=[log,strtemp]
            if N_ELEMENTS(missingOBS) eq 0 then missingOBS=strtemp $
            else missingOBS=[missingOBS,strtemp]
          endfor
        endif
        if N_ELEMENTS(dirs) gt 0 then begin
          ind = where(haveitDIRS eq 0, count)
          ntoomany=count
          if count gt 0 then begin
            strtemp='OBS not in timeline:'
            print,strtemp
            log=[log,strtemp]
            for m=0,count-1 do begin
              strtemp=dirs[m]
              print,strtemp
              log=[log,strtemp]
              if N_ELEMENTS(toomanyOBS) eq 0 then toomanyOBS=strtemp $
              else toomanyOBS=[toomanyOBS,strtemp]
            endfor
          endif
        endif
      endif
      nerrors=0
      nempty=0
      for d=0,N_ELEMENTS(dirs)-1 do begin
        if strmid(dirs[d], 0,1, /reverse_offset) ne dirsep then dirs[d] = dirs[d]+dirsep
        temp = file_search(dirs[d], filter, count=fcount)
        if fcount gt 0 then begin ;found errorlog
          if N_ELEMENTS(files) eq 0 then files=temp $
          else files=[files, temp]
          nerrors = nerrors+fcount
          if N_ELEMENTS(errorOBS) eq 0 then errorOBS=dirs[d] else errorOBS=[errorOBS,dirs[d]]
          
          for f=0,fcount-1 do begin
            strtemp=temp[f]
            print,strtemp
            log=[log,strtemp]
            OPENR, inunit, temp[f], /GET_LUN
            textall=''
            line = ''
            linenr=0
            ; While there is text left, read it:
            WHILE ~EOF(inunit) && (linenr lt maxline) DO BEGIN
              READF, inunit, line
              textall=textall+line+cr
              linenr=linenr+1
              if linenr eq 1 then if N_ELEMENTS(errorOBSversion) eq 0 then errorOBSversion=line else errorOBSversion=[errorOBSversion,line]
              if linenr eq 2 then begin
                a=strtrim(strmid(line,1),2)
                if N_ELEMENTS(errorOBSmessage) eq 0 then errorOBSmessage=a else errorOBSmessage=[errorOBSmessage,a]
              endif
              if strpos(line,'Execution halted at:') ge 0 then begin
                a=strtrim((strsplit((strsplit(line,':',/extract))[1],' ',/extract))[0],2)
                if N_ELEMENTS(errorOBShalted) eq 0 then errorOBShalted=a else errorOBShalted=[errorOBShalted,a]
              endif
            ENDWHILE
            ; Close the files and deallocate the units:
            FREE_LUN, inunit
            strtemp=textall
            print,strtemp
            log=[log,strtemp]
            strtemp='==========================='
            print,strtemp
            log=[log,strtemp]
          endfor
        endif else begin ;no errorlog
          temp = file_search(dirs[d], '*.log', count=fcount)
          if fcount eq 0 then begin ;no log
            strtemp=dirs[d]
            print,strtemp
            log=[log,strtemp]
            strtemp='no logfile'
            print,strtemp
            log=[log,strtemp]
            nempty=nempty+1
            if N_ELEMENTS(emptyOBS) eq 0 then emptyOBS=dirs[d] else emptyOBS=[emptyOBS,dirs[d]]
          endif else begin ;good OBS (log but no errorlog)
            goodOBS=goodOBS+1
          endelse
        endelse
      endfor
      if (nerrors eq 0) && (nempty eq 0) && (nmissing eq 0) && (ntoomany eq 0) then begin
        strtemp='all good'
        print,strtemp
        log=[log,strtemp]
      endif
    endif else begin
      strtemp='directory does not exist'
      print,strtemp
      log=[log,strtemp]
      if N_ELEMENTS(tl) gt 0 then begin
        strtemp='missing OBS:'
        print,strtemp
        log=[log,strtemp]
        if size(tl, /type) eq 8 then begin
          for m=0,N_ELEMENTS(tl)-1 do begin
            strtemp=tl[m].date_obs+'  '+strcompress(string(tl[m].obsid),/remove_all)
            print,strtemp
            log=[log,strtemp]
            if N_ELEMENTS(missingOBS) eq 0 then missingOBS=strtemp $
            else missingOBS=[missingOBS,strtemp]
          endfor
        endif
      endif
    endelse
    goodOBSall=goodOBSall+goodOBS
    strtemp='successfully completed OBS:'+string(goodOBS,format='(I4)')
    print,strtemp
    log=[log,strtemp]
  endfor
  
  errorOBS = FIX_STRLEN_ARR(errorOBS, max(strlen(errorOBS)))
  errorOBShalted = FIX_STRLEN_ARR(errorOBShalted, max(strlen(errorOBShalted)))
  
  if strmid(logdestination, 0,1, /reverse_offset) ne dirsep then logdestination = logdestination+dirsep
  file = logdestination+'missingl2_'+time2file(currenttime)+'_'+file_basename(path)+'.log'
  OpenW, unit, file, /get_lun
  for i=0,N_ELEMENTS(inputstring)-1 do printf, unit, inputstring[i]
  printf,unit,''
  printf,unit,'successfully completed OBS:'+string(goodOBSall,format='(I6)')
  printf,unit,''
  printf,unit,'crashed: '+strtrim(string(N_ELEMENTS(errorOBS)),2)
  if N_ELEMENTS(errorOBS) eq 0 then printf,unit,'none' $
  else for i=0,N_ELEMENTS(errorOBS)-1 do printf,unit,errorOBS[i] + '   ' + errorOBSversion[i] + '   ' + errorOBShalted[i] + '   ' + errorOBSmessage[i]
  printf,unit,''
  printf,unit,'incomplete: '+strtrim(string(N_ELEMENTS(emptyOBS)),2)
  if N_ELEMENTS(emptyOBS) eq 0 then printf,unit,'none' $
  else for i=0,N_ELEMENTS(emptyOBS)-1 do printf,unit,emptyOBS[i]
  printf,unit,''
  printf,unit,'missing: '+strtrim(string(N_ELEMENTS(missingOBS)),2)
  if N_ELEMENTS(missingOBS) eq 0 then printf,unit,'none' $
  else for i=0,N_ELEMENTS(missingOBS)-1 do printf,unit,missingOBS[i]
  printf,unit,''
  printf,unit,'not in timeline: '+strtrim(string(N_ELEMENTS(toomanyOBS)),2)
  if N_ELEMENTS(toomanyOBS) eq 0 then printf,unit,'none' $
  else for i=0,N_ELEMENTS(toomanyOBS)-1 do printf,unit,toomanyOBS[i]
  printf,unit,''
  printf,unit,''
  printf,unit,'detailed log:'
  if N_ELEMENTS(log) gt 2 then log=log[2:N_ELEMENTS(log)-1]
  for i=0,N_ELEMENTS(log)-1 do printf, unit, log[i]
  Close, unit
  free_lun, unit
end
