; $Id: 2024-03-20 14:43 CET $  ;


pro iris_gethinode_driver, startdate, stopdate, days=days

;  gapfile = '/earth/sanhome2/mawiesma/iris/hinode/hinodegap.sav'
;  gapfile = '/irisa/data/level2/hinodegap.sav'
  constants = obj_new('IRISsim_constants')

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

  ;outdir = '~/iris/hinode/nowidget3'

  ;path = constants->get_data_path_uio_l2()
  path = constants->get_data_path_lmsal_l2()
  paths = ssw_time2paths(t0, t1, path)
  help,paths
  dirsep = path_sep()

  ;  if 0 then begin
  ;    obs='20151110_101944_3620263362'
  ;    iris_getHINODEdata, obs, outdir=outdir, /addobsid, /adddatetree, /nowidget
  ;  endif else begin
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
        fits = total(strmatch(files, '*.fits'))
        if log eq 0 then begin
          box_message, 'no logfile (unfinished)'
        endif else if errorlog gt 0 then begin
          box_message, 'errorlogfile (crashed)'
        endif else if fits eq 0 then begin
          box_message, 'no fits files'
        endif else begin
;          ind = where(strmatch(files, '*.fits') eq 1)
;          mreadfits_header, outdir+files[ind[0]], hdr, only_tags='STARTOBS,ENDOBS'
;          time_window,[hdr.startobs,hdr.endobs],minute=[-10,10],t0,t1
;          gap = gap_check([t0,t1])
;          if gap then begin
;            box_message, ['gap discovered, saving obs into gapfile',gapfile]
;            if file_test(gapfile) then begin
;              restore, gapfile
;              ind = where(gapobs eq temp0[iobs], count)
;              if count eq 0 then begin
;                gapobs = [gapobs, temp0[iobs]]
;                save, gapobs, filename = gapfile
;              endif
;            endif else begin
;              gapobs = temp0[iobs]
;              save, gapobs, filename = gapfile
;            endelse
;          endif else begin
            outdir = outdir + 'hinode/'
            print, 'saving to: ' + outdir
            file_delete, outdir, /allow_nonexistent, /recursive, /verbose
            iris_getHINODEdata, temp0[iobs], outdir=outdir, /nowidget
;          endelse
        endelse
      endfor
    endif
  endfor
  ;  endelse

  print,'done, exiting...'
  exit
end
