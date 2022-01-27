; $Id: irisgethinode_writelogfile.pro,v 1.2 2015/12/17 09:44:30 mawiesma Exp $  ;

pro IRISgetHINODE_writelogfile, hinodelog, obs2fov, filename, traceback=traceback

  if keyword_set(traceback) then begin
    file=filename+'.errorlog'
    OpenW, unit, file, /get_lun
    if N_ELEMENTS(hinodelog) gt 0 then begin
      printf, unit, 'HINODE proc version: ' + strcompress(string(hinodelog.procversion), /remove_all)
      printf, unit, hinodelog.version
    endif
    for i=0,N_ELEMENTS(traceback)-1 do printf, unit, traceback[i]
    Close, unit
    free_lun, unit
  endif

  if N_ELEMENTS(hinodelog) gt 0 then begin
    file=filename+'.log'

    OpenW, unit, file, /get_lun
    log = 'Log of SOT2IRIS, proc version: ' + strcompress(string(hinodelog.procversion), /remove_all)
    printf, unit, log
    log = 'l1to2sot version: ' + hinodelog.version
    printf, unit, log
    log = 'Date: ' + hinodelog.date
    printf, unit, log
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
    log = 'recieved files     : ' + STRING(hinodelog.filesreceived, format='(I7)')
    printf, unit, log
    log = 'bad files          : ' + string(0, format='(I7)') + $
      ' ('+string(double(0)/double(hinodelog.filesreceived)*100d, format='(f4.1)')+'%)'
    printf, unit, log
    log = 'created files      : ' + STRING(hinodelog.filescreated, format='(I7)')
    printf, unit, log
    mem=hinodelog.memory
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
    totaltimetext = IRIS_humanreadabletime(hinodelog.ttotal)
    totaltimetextlength = strlen(totaltimetext)
    log = 'time used          : ' + totaltimetext
    timestr=log
    printf, unit, log
    log='======================================================================================================================================'
    printf, unit, log
    percent = hinodelog.tprep/hinodelog.ttotal*100d
    if percent ge 99.95 then format='(f4.0)' else format='(f4.1)'
    log = 'preparation        : ' + IRIS_humanreadabletime(hinodelog.tprep, totaltimetextlength=totaltimetextlength) + $
      '   (' + string(percent, format=format) + '%)'
    printf, unit, log
    percent = hinodelog.tdownload/hinodelog.ttotal*100d
    if percent ge 99.95 then format='(f4.0)' else format='(f4.1)'
    log = 'download data      : ' + IRIS_humanreadabletime(hinodelog.tdownload, totaltimetextlength=totaltimetextlength) + $
      '   (' + string(percent, format=format) + '%)'
    printf, unit, log
    percent = hinodelog.t122/hinodelog.ttotal*100d
    if percent ge 99.95 then format='(f4.0)' else format='(f4.1)'
    log = 'level 1 to 2       : ' + IRIS_humanreadabletime(hinodelog.t122, totaltimetextlength=totaltimetextlength) + $
      '   (' + string(percent, format=format) + '%)'
    printf, unit, log
    percent = hinodelog.tread/hinodelog.ttotal*100d
    if percent ge 99.95 then format='(f4.0)' else format='(f4.1)'
    log = 'reading/fg_prep    : ' + IRIS_humanreadabletime(hinodelog.tread, totaltimetextlength=totaltimetextlength) + $
      '   (' + string(percent, format=format) + '%)'
    printf, unit, log
    percent = hinodelog.twrite/hinodelog.ttotal*100d
    if percent ge 99.95 then format='(f4.0)' else format='(f4.1)'
    log = 'writing            : ' + IRIS_humanreadabletime(hinodelog.twrite, totaltimetextlength=totaltimetextlength) + $
      '   (' + string(percent, format=format) + '%)'
    printf, unit, log
    rest = hinodelog.t122 - hinodelog.tread - hinodelog.twrite
    percent = rest/hinodelog.ttotal*100d
    if percent ge 99.95 then format='(f4.0)' else format='(f4.1)'
    log = 'rest of l1to2      : ' + IRIS_humanreadabletime(rest, totaltimetextlength=totaltimetextlength) + $
      '   (' + string(percent, format=format) + '%)'
    printf, unit, log
    log='======================================================================================================================================'
    printf, unit, log
    Close, unit
    free_lun, unit
  endif
end
