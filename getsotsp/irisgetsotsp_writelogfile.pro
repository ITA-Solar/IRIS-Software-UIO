; $Id: irisgetsotsp_writelogfile.pro,v 1.1 2019/12/04 13:58:05 mawiesma Exp $  ;

pro IRISgetSotSP_writelogfile, sotsplog, obs2fov, filename, traceback=traceback

  if keyword_set(traceback) then begin
    file=filename+'.errorlog'
    OpenW, unit, file, /get_lun
    if N_ELEMENTS(sotsplog) gt 0 then begin
      printf, unit, 'HINODE proc version: ' + strcompress(string(sotsplog.procversion), /remove_all)
      printf, unit, sotsplog.version
    endif
    for i=0,N_ELEMENTS(traceback)-1 do printf, unit, traceback[i]
    Close, unit
    free_lun, unit
  endif

  if N_ELEMENTS(sotsplog) gt 0 then begin
    file=filename+'.log'

    OpenW, unit, file, /get_lun
    log = 'Log of SOT2IRIS, proc version: ' + strcompress(string(sotsplog.procversion), /remove_all)
    printf, unit, log
    log = 'l1to2sot version: ' + sotsplog.version
    printf, unit, log
    log = 'Date: ' + sotsplog.date
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
    log = 'recieved files     : ' + STRING(sotsplog.filesreceived, format='(I7)')
    printf, unit, log
    log = 'unused files       : ' + STRING(sotsplog.filesunused, format='(I7)')
    printf, unit, log
    log = 'bad files          : ' + string(0, format='(I7)') + $
      ' ('+string(double(0)/double(sotsplog.filesreceived)*100d, format='(f4.1)')+'%)'
    printf, unit, log
    log = 'created files      : ' + STRING(sotsplog.filescreated, format='(I7)')
    printf, unit, log
    mem=sotsplog.memory
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
    totaltimetext = IRIS_humanreadabletime(sotsplog.ttotal)
    totaltimetextlength = strlen(totaltimetext)
    log = 'time used          : ' + totaltimetext
    timestr=log
    printf, unit, log
    log='======================================================================================================================================'
    printf, unit, log
    percent = sotsplog.tprep/sotsplog.ttotal*100d
    if percent ge 99.95 then format='(f4.0)' else format='(f4.1)'
    log = 'preparation        : ' + IRIS_humanreadabletime(sotsplog.tprep, totaltimetextlength=totaltimetextlength) + $
      '   (' + string(percent, format=format) + '%)'
    printf, unit, log
    percent = sotsplog.tdownload/sotsplog.ttotal*100d
    if percent ge 99.95 then format='(f4.0)' else format='(f4.1)'
    log = 'download data      : ' + IRIS_humanreadabletime(sotsplog.tdownload, totaltimetextlength=totaltimetextlength) + $
      '   (' + string(percent, format=format) + '%)'
    printf, unit, log
    percent = sotsplog.t122/sotsplog.ttotal*100d
    if percent ge 99.95 then format='(f4.0)' else format='(f4.1)'
    log = 'level 1 to 2       : ' + IRIS_humanreadabletime(sotsplog.t122, totaltimetextlength=totaltimetextlength) + $
      '   (' + string(percent, format=format) + '%)'
    printf, unit, log
    percent = sotsplog.tread/sotsplog.ttotal*100d
    if percent ge 99.95 then format='(f4.0)' else format='(f4.1)'
    log = 'reading/fg_prep    : ' + IRIS_humanreadabletime(sotsplog.tread, totaltimetextlength=totaltimetextlength) + $
      '   (' + string(percent, format=format) + '%)'
    printf, unit, log
    percent = sotsplog.twrite/sotsplog.ttotal*100d
    if percent ge 99.95 then format='(f4.0)' else format='(f4.1)'
    log = 'writing            : ' + IRIS_humanreadabletime(sotsplog.twrite, totaltimetextlength=totaltimetextlength) + $
      '   (' + string(percent, format=format) + '%)'
    printf, unit, log
    rest = sotsplog.t122 - sotsplog.tread - sotsplog.twrite
    percent = rest/sotsplog.ttotal*100d
    if percent ge 99.95 then format='(f4.0)' else format='(f4.1)'
    log = 'rest of l1to2      : ' + IRIS_humanreadabletime(rest, totaltimetextlength=totaltimetextlength) + $
      '   (' + string(percent, format=format) + '%)'
    printf, unit, log
    log='======================================================================================================================================'
    printf, unit, log
    log='files are unused because all images within those files are outside of the requested time window'
    printf, unit, log
    Close, unit
    free_lun, unit
  endif
end
