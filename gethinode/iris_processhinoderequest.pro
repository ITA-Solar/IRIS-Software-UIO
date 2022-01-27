; $Id: iris_processhinoderequest.pro,v 1.13 2019/07/04 09:31:04 mawiesma Exp $  ;

pro IRIS_processHINODErequest, outdir, umodes, files,  no_download, obs2fov, fulltime=fulltime, fullfov=fullfov, $
  deletetempfiles=deletetempfiles, noprep=noprep, debug=debug, _extra=_extra, tprep1=tprep1, $
  redownload=redownload, redol2=redol2

  ; Get current allocation and reset the high water mark:
  start_mem = MEMORY(/CURRENT)

  procversion = 8 ;version of IRIS_processHINODErequest, including iris_downloadHINODE
  GET_UTC, daterf, /CCSDS

  hinodelog = { $
    date:daterf, $
    tprep:0d, $
    tdownload:0d, $
    t122:0d, $
    ttotal:0d, $
    tread:0d, $
    twrite:0d, $
    memory:0LL, $
    version:'', $
    procversion:procversion, $
    filesreceived:0L, $
    filescreated:0 $
  }

  except_orig = !except
  if ~keyword_set(debug) then begin
    !except = 0
    ; launch the error handler:
    catch, error_status
    ; begin error handler
    if error_status ne 0 then begin
      catch, /cancel
      catch, error_inerror
      if error_inerror ne 0 then begin
        catch, /cancel
        Help, /Last_Message, Output=traceback
        box_message,traceback
        box_message,['error handler crashed',$
          'exiting without writing an errorfile']
        !except = except_orig
        return
      endif
      Help, /Last_Message, Output=traceback
      box_message,traceback
      box_message,'writing errorlogfile, and returning'
      IRISgetHINODE_writelogfile, hinodelog, obs2fov, logfilename, traceback=traceback
      !except = except_orig
      return
    endif
  endif else !except = 2


  t1=systime(1)

  if keyword_set(outdir) then $
    if strmid(outdir, 0,1, /reverse_offset) ne path_sep() then outdir = outdir+path_sep()

  paramfile=0
  if N_PARAMS() lt 2 then begin
    if file_test(outdir+'cutout_params.sav') then paramfile=1 $
    else begin
      print,'not enough input'
      return
    endelse
  endif

  com = 'IRIS_processHINODErequest, '''+outdir+'''

  if paramfile then begin
    if keyword_set(noprep) then nopreporig=1 else nopreporig=0
    restore, outdir+'cutout_params.sav'
    if nopreporig then noprep=1
  endif else begin
    file_mkdir, outdir
    downloadfinished=0
    l2created=0
    save, umodes, files, outdir, no_download, obs2fov, fulltime, fullfov, $
      deletetempfiles, noprep, $
      downloadfinished, l2created, $
      filename=outdir+'cutout_params.sav'
    box_message, ['to continue/redo the process:', com]
    OpenW, unit, outdir+'readme.txt', /get_lun
    printf, unit, 'If you need to recreate the level 2,'
    printf, unit, 'or if the process was disrupted and you want to continue it,'
    printf, unit, 'then enter the following command into an IDL session:'
    printf, unit, ' '
    printf, unit, com
    printf, unit, ' '
    printf, unit, 'set the keywords /redownload and/or /redol2 if desired'
    printf, unit, 'the processes which have not finished will be redone automatically'
    Close, unit
    free_lun, unit
  endelse

  logfilename = outdir + 'sot_l2_' + obs2fov->get_obsid()

  t2=systime(1)
  hinodelog.tprep=t2-t1
  if keyword_set(tprep1) then hinodelog.tprep = hinodelog.tprep + tprep1

  if ~downloadfinished || keyword_set(redownload) then begin
    l2created=0

    iris_downloadHINODE, umodes, files, outdir, no_download, obs2fov, debug=debug, fulltime=fulltime

    downloadfinished=1
    save, umodes, files, outdir, no_download, obs2fov, fulltime, fullfov, $
      deletetempfiles, noprep, $
      downloadfinished, l2created, $
      filename=outdir+'cutout_params.sav'
    box_message, ['to continue/redo the process:', com+'[, /redownload]']
  endif

  t4=systime(1)
  hinodelog.tdownload=t4-t2

  if ~l2created || keyword_set(redol2) then begin
    deletetempfiles = ~no_download && keyword_set(deletetempfiles)
    noprep = keyword_set(noprep)
    iris_l1to2hinode, umodes, files, outdir, obs2fov, fullfov=fullfov, deletetempfiles=deletetempfiles, $
      noprep=noprep, debug=debug, hinodelog=hinodelog, _extra=_extra

    l2created=1
    save, umodes, files, outdir, no_download, obs2fov, fulltime, fullfov, $
      deletetempfiles, noprep, $
      downloadfinished, l2created, $
      filename=outdir+'cutout_params.sav'
    box_message, ['to continue/redo the process:', com+'[, /redol2, /redownload]']
  endif

  t5=systime(1)
  hinodelog.t122=t5-t4
  hinodelog.ttotal = t5-t1
  if keyword_set(tprep1) then hinodelog.ttotal = hinodelog.ttotal + tprep1

  hinodelog.memory = MEMORY(/HIGHWATER) - start_mem
  IRISgetHINODE_writelogfile, hinodelog, obs2fov, logfilename

  ttotal = IRIS_humanreadabletime(hinodelog.ttotal)
  tlen = strlen(ttotal)
  print, 'time total:         '+ttotal
  print, 'time preparation:   '+IRIS_humanreadabletime(hinodelog.tprep, totaltimetextlength=tlen)
  print, 'time download:      '+IRIS_humanreadabletime(hinodelog.tdownload, totaltimetextlength=tlen)
  print, 'time level 1 to 2:  '+IRIS_humanreadabletime(hinodelog.t122, totaltimetextlength=tlen)

  !except = except_orig
end
