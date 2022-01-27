;+
; NAME:
;       IRIS_processSotSPrequest
;
; PURPOSE:
;       IRIS_processSotSPrequest processes the request created by IRIS_getSotSPdata, it calls the download process (IRIS_downloadSotSP)
;       and then it calls the level1-to-2 transformator (IRIS_l1to2SotSP)
;
; CATEGORY:
;       Martin Wiesmann / IRIS Data processing
;       IRIS_getSotSPdata
;
; CALLING SEQUENCE:
;       IRIS_processSotSPrequest, outdir [, sot_sp_events, obs2fov, fulltime=fulltime, fullfov=fullfov, $
;       deletetempfiles=deletetempfiles, debug=debug, _extra=_extra, tprep1=tprep1, $
;       redownload=redownload, redol2=redol2]
;
; INPUTS:
;       outdir: The path into which the resulting files should be saved, can be the only input to this process, if
;         this process ran before with this outdir. This can be used to redownlad and/or reprocess the data, or to continue
;         with the process if it was interrupted
;
; OPTIONAL KEYWORD PARAMETERS:
;       sot_sp_events: modified version of result from event download for SOT SP, provided by IRIS_getSotSPdata
;       obs2fov: An obs2fov object with the IRIS OBS as input
;       fulltime: If set, all HINODE files of the selected umodes will be used, regardless of the time window
;       fullfov: If set, the original FOV of the HINODE umode will be transfered, regardless of IRIS FOV
;       deletetempfiles: if set, the downloaded files will be deleted after use
;       tprep1: time in seconds that was used in IRIS_getSotSPdata
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
;       IRIS_processSotSPrequest, '~/data/sotsp/20140304_092952_3860110476/' [,/redownload, redol2]
;
; COMMON BLOCKS:
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;       31-Oct-2019: Martin Wiesmann (ITA, UIO)
;
; $Id: iris_processsotsprequest.pro,v 1.3 2020/01/15 14:44:20 mawiesma Exp $  ;



pro IRIS_processSotSPrequest, outdir, sot_sp_events, obs2fov, fulltime=fulltime, fullfov=fullfov, $
  deletetempfiles=deletetempfiles, debug=debug, _extra=_extra, tprep1=tprep1, $
  redownload=redownload, redol2=redol2, view_images=view_images

  ; Get current allocation and reset the high water mark:
  start_mem = MEMORY(/CURRENT)

  procversion = 2 ;version of IRIS_processSotSPrequest, including iris_downloadSotSP
  GET_UTC, daterf, /CCSDS

  sotsplog = { $
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
    filesunused:0L, $
    filescreated:0L $
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
      IRISgetSotSP_writelogfile, sotsplog, obs2fov, logfilename, traceback=traceback
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

  com = 'IRIS_processSotSPrequest, '''+outdir+'''


  if paramfile then begin
    restore, outdir+'cutout_params.sav'
  endif else begin

    file_mkdir, outdir
    downloadfinished=0
    l2created=0
    save, sot_sp_events, files, outdir, obs2fov, fulltime, fullfov, $
      deletetempfiles, $
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

  logfilename = outdir + 'sotsp_l2_' + obs2fov->get_obsid()

  t2=systime(1)
  sotsplog.tprep=t2-t1
  if keyword_set(tprep1) then sotsplog.tprep = sotsplog.tprep + tprep1


  if ~downloadfinished || keyword_set(redownload) then begin
    IRIS_downloadSotSP, sot_sp_events, outdir, files=files, deletetempfiles=deletetempfiles, debug=debug

    downloadfinished=1
    save, sot_sp_events, files, outdir, obs2fov, fulltime, fullfov, $
      deletetempfiles, $
      downloadfinished, l2created, $
      filename=outdir+'cutout_params.sav'
    box_message, ['to continue/redo the process:', com+'[, /redownload]']
  endif

  t4=systime(1)
  sotsplog.tdownload=t4-t2


  if ~l2created || keyword_set(redol2) then begin
    IRIS_l1to2SotSP, sot_sp_events, files, outdir, obs2fov, fullfov=fullfov, deletetempfiles=deletetempfiles, $
      debug=debug, sotsplog=sotsplog, _extra=_extra, view_images=view_images

    l2created=1
    save, sot_sp_events, files, outdir, obs2fov, fulltime, fullfov, $
      deletetempfiles, $
      downloadfinished, l2created, $
      filename=outdir+'cutout_params.sav'
    box_message, ['to continue/redo the process:', com+'[, /redol2, /redownload]']
  endif

  t5=systime(1)
  sotsplog.t122=t5-t4
  sotsplog.ttotal = t5-t1
  if keyword_set(tprep1) then sotsplog.ttotal = sotsplog.ttotal + tprep1

  sotsplog.memory = MEMORY(/HIGHWATER) - start_mem
  IRISgetSotSP_writelogfile, sotsplog, obs2fov, logfilename

  ttotal = IRIS_humanreadabletime(sotsplog.ttotal)
  tlen = strlen(ttotal)
  print, 'time total:         '+ttotal
  print, 'time preparation:   '+IRIS_humanreadabletime(sotsplog.tprep, totaltimetextlength=tlen)
  print, 'time download:      '+IRIS_humanreadabletime(sotsplog.tdownload, totaltimetextlength=tlen)
  print, 'time level 1 to 2:  '+IRIS_humanreadabletime(sotsplog.t122, totaltimetextlength=tlen)

  !except = except_orig
end
