;+
; NAME:
;       IRIS_processXRTrequest
;
; PURPOSE:
;       IRIS_processXRTrequest processes the request created by IRIS_getXRTdata, it calls the download process (IRIS_downloadXRT)
;       and then it calls the level1-to-2 transformator (IRIS_l1to2XRT)
;
; CATEGORY:
;       Martin Wiesmann / IRIS Data processing
;       IRIS_getXRTdata
;
; CALLING SEQUENCE:
;       IRIS_processXRTrequest, outdir [, umodes, files,  fileurl, obs2fov, fulltime=fulltime, fullfov=fullfov, $
;       deletetempfiles=deletetempfiles, debug=debug, _extra=_extra, tprep1=tprep1, $
;       redownload=redownload, redol2=redol2]
;
; INPUTS:
;       outdir: The path into which the resulting files should be saved, can be the only input to this process, if
;         this process ran before with this outdir. This can be used to redownlad and/or reprocess the data, or to continue
;         with the process if it was interrupted
;
; OPTIONAL KEYWORD PARAMETERS:
;       umodes: modified version of result from xrt_umodes, provided by IRIS_getXRTdata
;       files: a pointer array, one pointer per umode, each pointer points to a string array of filenames
;       fileurl: boolean, indicates whether filenames are urls
;       obs2fov: An obs2fov object with the IRIS OBS as input
;       fulltime: If set, all HINODE files of the selected umodes will be used, regardless of the time window
;       fullfov: If set, the original FOV of the HINODE umode will be transfered, regardless of IRIS FOV
;       deletetempfiles: if set, the downloaded files will be deleted after use
;       tprep1: time in seconds that was used in IRIS_getXRTdata
;       redownload: If set, all files will be downloaded even if they have been downloaded already,
;           if not set, the program first checks for each file whether it already exist in the outdir
;        redol2: If set, the level 2 files will be redone
;       _extra: not used
;
; OUTPUTS:
;
; CALLS:
;       Usually, this process is not used on its own, and thus not called by the user.
;       It can be used, if processing has been interrupted and the user doesn't want to restart the whole process
;       Then it is called like this:
;       IRIS_processXRTrequest, '~/data/xrt/20140304_092952_3860110476/' [,/redownload, redol2]
;
; COMMON BLOCKS:
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;       15-Feb-2019: Martin Wiesmann (ITA, UIO)
;
; $Id: iris_processxrtrequest.pro,v 1.3 2019/02/28 09:47:40 mawiesma Exp $  ;



pro IRIS_processXRTrequest, outdir, umodes, files,  fileurl, obs2fov, fulltime=fulltime, fullfov=fullfov, $
  deletetempfiles=deletetempfiles, debug=debug, _extra=_extra, tprep1=tprep1, $
  redownload=redownload, redol2=redol2

  ; Get current allocation and reset the high water mark:
  start_mem = MEMORY(/CURRENT)

  procversion = 1 ;version of IRIS_processXRTrequest, including iris_downloadHINODE
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

  com = 'IRIS_processXRTrequest, '''+outdir+'''


  if paramfile then begin
    restore, outdir+'cutout_params.sav'
    if keyword_set(fileurl) && N_ELEMENTS(urls) eq 0 then urls=files
  endif else begin

    if keyword_set(fileurl) then urls=files

    file_mkdir, outdir
    downloadfinished=0
    l2created=0
    save, umodes, files, outdir, fileurl, urls, obs2fov, fulltime, fullfov, $
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

  logfilename = outdir + 'xrt_l2_' + obs2fov->get_obsid()

  t2=systime(1)
  hinodelog.tprep=t2-t1
  if keyword_set(tprep1) then hinodelog.tprep = hinodelog.tprep + tprep1


  iris_downloadXRT, umodes, files, outdir, fileurl, obs2fov, urls=urls, debug=debug, fulltime=fulltime, redownload=redownload, redol2=redol2

  downloadfinished=1
  save, umodes, files, outdir, fileurl, urls, obs2fov, fulltime, fullfov, $
    deletetempfiles, noprep, $
    downloadfinished, l2created, $
    filename=outdir+'cutout_params.sav'
  box_message, ['to continue/redo the process:', com+'[, /redownload]']

  t4=systime(1)
  hinodelog.tdownload=t4-t2


  if ~l2created || keyword_set(redol2) then begin
    deletetempfiles = fileurl && keyword_set(deletetempfiles)
    noprep = keyword_set(noprep) || ~fileurl
    IRIS_l1to2xrt, umodes, files, outdir, obs2fov, fullfov=fullfov, deletetempfiles=deletetempfiles, $
      noprep=noprep, debug=debug, hinodelog=hinodelog, _extra=_extra

    l2created=1
    save, umodes, files, outdir, fileurl, urls, obs2fov, fulltime, fullfov, $
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
