; $Id: irisgetaia_writelogfile.pro,v 1.9 2018/04/28 18:42:56 mawiesma Exp $  ;

pro IRISgetAIA_writelogfile, aialog, obs2fov, filename, traceback=traceback, sswjobid=sswjobid

  if keyword_set(traceback) then begin
    file=filename+'.errorlog'
    OpenW, unit, file, /get_lun
    if N_ELEMENTS(aialog) gt 0 then begin
      printf, unit, 'AIA proc version: ' + strcompress(string(aialog.procversion), /remove_all)
      printf, unit, aialog.version
    endif
    for i=0,N_ELEMENTS(traceback)-1 do printf, unit, traceback[i]
    Close, unit
    free_lun, unit
  endif

  if N_ELEMENTS(aialog) gt 0 then begin
    file=filename+'.log'

    OpenW, unit, file, /get_lun
    log = 'Log of AIA2IRIS, proc version: ' + strcompress(string(aialog.procversion), /remove_all)
    printf, unit, log
    log = 'l1to2aia version: ' + aialog.version
    printf, unit, log
    log = 'Date            : ' + aialog.date
    printf, unit, log
    if keyword_set(sswjobid) then begin
      log = 'SSW_JobID       : ' + sswjobid
      printf, unit, log
    endif
    log = 'OBS ID             : ' + obs2fov->get_obsid()
    printf, unit, log
    log = 'OBS Start          : ' + obs2fov->get_startobs()
    printf, unit, log
    log = 'OBS End            : ' + obs2fov->get_endobs()
    printf, unit, log
    timeexpand = obs2fov->get_timeexpand()
    log = 'time expand [min]  : ' + STRING(timeexpand[0], format='(f5.1)') + $
      ' / ' + STRING(timeexpand[1], format='(f5.1)')
    printf, unit, log
    obs2fov->get_startendtimes, starttime, endtime
    log = 'Start time         : ' + starttime
    printf, unit, log
    log = 'End time           : ' + endtime
    printf, unit, log
    fovexpand = obs2fov->get_fovexpand()
    log = 'FOV expand [arcsec]: ' + STRING(fovexpand[0], format='(f5.1)') + $
      ' / ' + STRING(fovexpand[1], format='(f5.1)')
    printf, unit, log
    temp = obs2fov->get_rollangle(source=source)
    log = 'source of rollangle: ' + source
    printf, unit, log
    log = 'recieved files     : ' + STRING(aialog.filesreceived, format='(I7)')
    printf, unit, log
    log = 'bad files          : ' + string((aialog.nbadcdelt1+aialog.nbadcdelt2+aialog.nbadquality), format='(I7)') + $
      ' ('+string(double(aialog.nbadcdelt1+aialog.nbadcdelt2+aialog.nbadquality)/double(aialog.filesreceived)*100d, format='(f4.1)')+'%)'
    printf, unit, log
    log = 'created files      : ' + STRING(aialog.filescreated, format='(I7)')
    printf, unit, log
    mem=aialog.memory
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
    printf, unit, log
    totaltimetext = IRIS_humanreadabletime(aialog.ttotal)
    totaltimetextlength = strlen(totaltimetext)
    log = 'time used          : ' + totaltimetext
    timestr=log
    printf, unit, log
    log='======================================================================================================================================'
    printf, unit, log
    percent = aialog.tprep/aialog.ttotal*100d
    if percent ge 99.95 then format='(f4.0)' else format='(f4.1)'
    log = 'preparation        : ' + IRIS_humanreadabletime(aialog.tprep, totaltimetextlength=totaltimetextlength) + $
      '   (' + string(percent, format=format) + '%)'
    printf, unit, log
    percent = aialog.treq/aialog.ttotal*100d
    if percent ge 99.95 then format='(f4.0)' else format='(f4.1)'
    log = 'request            : ' + IRIS_humanreadabletime(aialog.treq, totaltimetextlength=totaltimetextlength) + $
      '   (' + string(percent, format=format) + '%)'
    printf, unit, log
    percent = aialog.twait/aialog.ttotal*100d
    if percent ge 99.95 then format='(f4.0)' else format='(f4.1)'
    log = 'wait for data      : ' + IRIS_humanreadabletime(aialog.twait, totaltimetextlength=totaltimetextlength) + $
      '   (' + string(percent, format=format) + '%)'
    printf, unit, log
    percent = aialog.tdownload/aialog.ttotal*100d
    if percent ge 99.95 then format='(f4.0)' else format='(f4.1)'
    log = 'download data      : ' + IRIS_humanreadabletime(aialog.tdownload, totaltimetextlength=totaltimetextlength) + $
      '   (' + string(percent, format=format) + '%)'
    printf, unit, log
    percent = aialog.t122/aialog.ttotal*100d
    if percent ge 99.95 then format='(f4.0)' else format='(f4.1)'
    log = 'level 1 to 2       : ' + IRIS_humanreadabletime(aialog.t122, totaltimetextlength=totaltimetextlength) + $
      '   (' + string(percent, format=format) + '%)'
    printf, unit, log
    percent = aialog.tread/aialog.ttotal*100d
    if percent ge 99.95 then format='(f4.0)' else format='(f4.1)'
    log = 'reading/aia_prep   : ' + IRIS_humanreadabletime(aialog.tread, totaltimetextlength=totaltimetextlength) + $
      '   (' + string(percent, format=format) + '%)'
    printf, unit, log
    percent = aialog.twrite/aialog.ttotal*100d
    if percent ge 99.95 then format='(f4.0)' else format='(f4.1)'
    log = 'writing            : ' + IRIS_humanreadabletime(aialog.twrite, totaltimetextlength=totaltimetextlength) + $
      '   (' + string(percent, format=format) + '%)'
    printf, unit, log
    rest = aialog.t122 - aialog.tread - aialog.twrite
    percent = rest/aialog.ttotal*100d
    if percent ge 99.95 then format='(f4.0)' else format='(f4.1)'
    log = 'rest of l1to2      : ' + IRIS_humanreadabletime(rest, totaltimetextlength=totaltimetextlength) + $
      '   (' + string(percent, format=format) + '%)'
    printf, unit, log
    log='======================================================================================================================================'
    printf, unit, log
    if aialog.nbadcdelt1 gt 0 then begin
      log = 'files with bad cdelt1 values (i.e. unequal to the first one)'
      printf, unit, log
      log='CDELT1[0] | CDELT1[1] | file'
      printf, unit, log
      log='======================================================================================================================================'
      printf, unit, log
      for i=0,aialog.nbadcdelt1-1 do begin
        log = string((*aialog.cdelt1)[i], format='(f9.6)') + ' | ' + $
          string((*aialog.badcdelt1)[i], format='(f9.6)') + ' | ' + $
          (*aialog.badcdelt1_file)[i]
        printf, unit, log
      endfor
      log='======================================================================================================================================'
      printf, unit, log
    endif
    if aialog.nbadcdelt2 gt 0 then begin
      log = 'files with bad cdelt2 values (i.e. unequal to the first one)'
      printf, unit, log
      log='CDELT2[0] | CDELT2[1] | file'
      printf, unit, log
      log='======================================================================================================================================'
      printf, unit, log
      for i=0,aialog.nbadcdelt2-1 do begin
        log = string((*aialog.cdelt2)[i], format='(f9.6)') + ' | ' + $
          string((*aialog.badcdelt2)[i], format='(f9.6)') + ' | ' + $
          (*aialog.badcdelt2_file)[i]
        printf, unit, log
      endfor
      log='======================================================================================================================================'
      printf, unit, log
    endif
    if aialog.nbadquality gt 0 then begin
      log = 'files with bad quality values (i.e. ForbiddenBits=[0,1,3,4,7,12,13,14,15,16,17,18,20,21,31];if any of these bits is set - reject the image)'
      printf, unit, log
      log='   quality | file'
      printf, unit, log
      log='======================================================================================================================================'
      printf, unit, log
      for i=0,aialog.nbadquality-1 do begin
        log = string((*aialog.badquality)[i], format='(I10)') + ' | ' + $
          (*aialog.badquality_file)[i]
        printf, unit, log
      endfor
      log='======================================================================================================================================'
      printf, unit, log
    endif
    Close, unit
    free_lun, unit
  endif
end
