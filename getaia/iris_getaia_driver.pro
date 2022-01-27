; $Id: iris_getaia_driver.pro,v 1.2 2016/03/31 12:54:54 mawiesma Exp $  ;


pro iris_getaia_driver, startdate, stopdate, days=days

  if N_ELEMENTS(startdate) eq 0 then begin
    t0 = '1-feb-2014T00:00:00'
    t1 = '30-apr-2014T23:59:59'
  endif else if N_ELEMENTS(startdate) gt 1 || N_ELEMENTS(stopdate) gt 1 then begin
    print,'only one startdate and stopdate allowed, scalar date/time input'
    return
  endif else begin
    if valid_time(startdate) then begin
      t0 = startdate
      if N_ELEMENTS(stopdate) eq 0 then begin
        t0 = anytim2utc(t0, /int)
        t1 = t0
        if ~keyword_set(days) then days=30
        t1.mjd = t1.mjd + fix(days)
      endif else begin
        if valid_time(stopdate) then begin
          t1 = stopdate
        endif else begin
          print, 'stopdate is not a valid date/time input'
          return
        endelse
      endelse
    endif else begin
      print, 'startdate is not a valid date/time input'
      return
    endelse
  endelse

  path = '/irisa/data/level2/'
  paths = ssw_time2paths(t0, t1, path)
  help,paths
  dirsep = path_sep()

  for p=0,N_ELEMENTS(paths)-1 do begin
    if strmid(paths[p], 0,1, /reverse_offset) ne dirsep then paths[p] = paths[p]+dirsep
    outdirp = paths[p]
    spawn, 'ls ' + paths[p], temp0
    dum = extract_fids(temp0, fidsfound=fidsfound)
    dirgood = where(fidsfound, fcount2)
    fcount=0
    if fcount2 gt 0 then begin
      temp0=temp0[dirgood]
      help,temp0
      for iobs=0,fcount2-1 do begin
        outdir = outdirp + temp0[iobs]
        if strmid(outdir, 0,1, /reverse_offset) ne dirsep then outdir = outdir+dirsep
        print,outdir
        spawn, 'ls ' + outdir, files
        log = total(strmatch(files, '*.log'))
        errorlog = total(strmatch(files, '*.errorlog'))
        if log eq 0 then begin
          box_message, 'no logfile (unfinished)'
        endif else if errorlog gt 0 then begin
          box_message, 'errorlogfile (crashed)'
        endif else begin
          outdir = outdir + 'aia/'
          print, 'saving to: ' + outdir
          file_delete, outdir, /allow_nonexistent, /recursive, /verbose
          iris_getAIAdata, temp0[iobs], outdir=outdir, /nowidget
        endelse
      endfor
    endif
  endfor

  exit
end
