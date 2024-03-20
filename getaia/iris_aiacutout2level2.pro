;+
; NAME:
;       IRIS_aiacutout2level2
;
; PURPOSE:
;
;       IRIS_aiacutout2level2 gets AIA data that were created by ssw_cutout_service and correspond to a specific IRIS OBS
;       and transforms it to IRIS-SJI-like level 2 fits file
;
; CATEGORY:
;       Martin Wiesmann / IRIS Data processing
;
; CALLING SEQUENCE:
;       IRIS_aiacutout2level2, obs, waves=waves, $
;         fovexpand=fovexpand, timeexpand=timeexpand, outdir=outdir, adddatetree=adddatetree, addobsid=addobsid, $
;         debug=debug, _extra=_extra
;
;
; INPUTS:
;       obs: where obs is the OBS ID of the IRIS OBS. It can be given in 3 different styles:
;         1) Just the OBS ID (e.g. '20130829_005500_4203300028')
;           This option only works if you're either at LMSAL and have access to IRISsim_constants->get_data_path_lmsal_l2()
;           or if you're at ITA/UIO and have access to IRISsim_constants->get_data_path_uio_l2()
;         2) The directory in which the IRIS files reside (e.g. '/mn/xsan/d2/iris/data/level2/2014/01/01/20140101_000431_3840257196/')
;         3) The full path of one of the IRIS files, which you can print out from iris_xfiles (e.g. '/mn/xsan/d2/iris/data/level2/2014/01/01/20140101_000431_3840257196/iris_l2_20140101_000431_3840257196_SJI_1400_t000.fits')
;
;
; OPTIONAL KEYWORD PARAMETERS:
;       outdir: the directory in which to save the resulting fits files, may be modified by the keywords adddatetree and
;         addobsid. If not set, the files will be saved into the IRIS folder under a subdirectory called 'aia'
;       adddatetree: adds a date-tree structure to outdir (i.e. outdir/2015/10/23/)
;       addobsid: adds the OBS ID to outdir (i.e. outdir/20151023_171023_4203300028/), if adddatetree is also set
;         the OBS ID will be added at the end (i.e. outdir/2015/10/23/20151023_171023_4203300028/)
;       waves: a list of wavelengths to be transformed, must be an array of strings
;         (default: all available wavelengths))
;       fovexpand: defines by how much the FOV should be expanded, as compared to IRIS FOV, in arcsecond, can be scalar,
;         then the same expansion will be applied to both the x- and y-axis. It can also be a 2-element vector, one element for
;         x- and one for y-axis, respectively.
;         (default: 100.0 arcsec)
;       timeexpand: defines by how much the time window of the IRIS OBS is expanded for AIA data, in minutes. If scalar,
;         the value will be applied to the start and the end time. Can be also a 2-element vector, for different expansions
;         at the start and at the end, respectively. (Positive numbers result in a wider time-window)
;         (default: 10.0 minutes)
;       _extra: will be passed to aia_prep (/use_shared, /noshell, /uncomp_delete are set)
;
; OUTPUTS:
;       IRIS-SJI-like level 2 fits file of AIA data
;
; CALLS:
;
; COMMON BLOCKS:
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;       2016-04-22: Martin Wiesmann (ITA, UIO).
;
; $Id: 2024-03-20 14:43 CET $  ;


pro IRIS_aiacutout2level2, obs, waves=waves, $
  fovexpand=fovexpand, timeexpand=timeexpand, outdir=outdir, adddatetree=adddatetree, addobsid=addobsid, $
  debug=debug, _extra=_extra

  ; Get current allocation and reset the high water mark:
  start_mem = MEMORY(/CURRENT)

  fovexpanddefault = 100.0 ;arcseconds ;;;;;;;;;;;;;;;;;DEFAULT
  timeexpanddefault = 10.0 ;minutes ;;;;;;;;;;;;;;;;;DEFAULT

  t1=systime(1)

  GET_UTC, daterf, /CCSDS

  aialog = { $
    date:daterf, $
    tprep:0d, $
    treq:0d, $
    twait:0d, $
    tdownload:0d, $
    t122:0d, $
    ttotal:0d, $
    tread:0d, $
    twrite:0d, $
    memory:0LL, $
    version:'', $
    procversion:0, $
    filesreceived:0L, $
    filescreated:0, $
    nbadcdelt1:0L, $
    cdelt1:ptr_new(), $
    badcdelt1:ptr_new(), $
    badcdelt1_file:ptr_new(), $
    nbadcdelt2:0L, $
    cdelt2:ptr_new(), $
    badcdelt2:ptr_new(), $
    badcdelt2_file:ptr_new(), $
    nbadquality:0L, $
    badquality:ptr_new(), $
    badquality_file:ptr_new() $
    }

  obs2fov = obj_new('IRIS_obs2fov', obs)
  if obs2fov->get_error() then begin
    print, 'Invalid OBS'
    return
  endif

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
      IRISgetAIA_writelogfile, aialog, obs2fov, logfilename, traceback=traceback
      !except = except_orig
      return
    endif
  endif else !except = 2

  if N_ELEMENTS(fovexpand) eq 0 then obs2fov->set_fovexpand, fovexpanddefault $
  else obs2fov->set_fovexpand, fovexpand
  if N_ELEMENTS(timeexpand) eq 0 then obs2fov->set_timeexpand, timeexpanddefault $
  else obs2fov->set_timeexpand, timeexpand
  if N_ELEMENTS(outdir) eq 0 then begin
    outdiruse=obs2fov->get_obsdir() + 'aia/'
    outdir=outdiruse
  endif else begin
    outdiruse = outdir
    if strmid(outdiruse, 0,1, /reverse_offset) ne path_sep() then outdiruse = outdiruse+path_sep()
    if keyword_set(adddatetree) then begin
      outdiruse = ssw_time2paths(obs2fov->get_startobs(),obs2fov->get_startobs(), outdiruse)
      if strmid(outdiruse, 0,1, /reverse_offset) ne path_sep() then outdiruse = outdiruse+path_sep()
    endif
    if keyword_set(addobsid) then outdiruse = outdiruse + obs2fov->get_obsid() + '/'
  endelse
  logfilename = outdiruse+'aia_l2_'+obs2fov->get_obsid()

  file_delete, outdiruse, /allow_nonexistent, /recursive, /verbose
  file_mkdir, outdiruse
  
  method = 3

  hcr=iris_obs2hcr(obs,count=count) ; obsid->HCR
  if count ne 1 then begin
    box_message,'Need one, and only one fully qualified IRIS obsids; no action taken.'
    return ; EARLY EXIT on illegal input
  endif
  if required_tags(hcr, /SDO_SSW_JOBID) then begin
    if hcr.SDO_SSW_JOBID eq '' then begin
      box_message,'No Job ID defined yet (SDO_SSW_JOBID tag empty). No action taken.'
      return
    endif else sswjobid=hcr.SDO_SSW_JOBID
  endif else begin
    hcr2 = iris_time2hcr(hcr.starttime,hcr.stoptime,/struct)
    if required_tags(hcr2, /SDO_SSW_JOBID) then begin
      sswjobid = hcr2.SDO_SSW_JOBID
    endif else begin
      box_message,'No Job ID defined yet (no SDO_SSW_JOBID tag). No action taken.'
      return
    endelse
  endelse

  if N_ELEMENTS(waves) eq 0 then ssw_service_read_data, sswjobid, available_waves=waves, /only_available
  aiafilesl1 = ptrarr(N_ELEMENTS(waves))
  nfilesl1 = lonarr(N_ELEMENTS(waves))
  for iwave=0,N_ELEMENTS(waves)-1 do begin
    files=!NULL
    ssw_service_read_data, sswjobid, files, /files_only, wave=waves[iwave]
    if N_ELEMENTS(files) gt 0 && files[0] ne '' then begin
      nfilesl1[iwave] = N_ELEMENTS(files)
      aiafilesl1[iwave] = ptr_new(files)
    endif
  endfor

  t4=systime(1)
  aialog.tprep=t4-t1


  iris_l1to2aia, outdiruse, obs2fov, waves=waves, $
    deletetempfiles=deletetempfiles, method=method, aiafilesl1=aiafilesl1, nfilesl1=nfilesl1, $
    aialog=aialog, sswjobid=sswjobid, debug=debug, _extra=_extra


  t5=systime(1)
  aialog.t122=t5-t4
  aialog.ttotal = t5-t1

  aialog.memory = MEMORY(/HIGHWATER) - start_mem
  IRISgetAIA_writelogfile, aialog, obs2fov, logfilename, sswjobid=sswjobid

  ttotal = IRIS_humanreadabletime(aialog.ttotal)
  tlen = strlen(ttotal)
  print, 'time total:         '+ttotal
  print, 'time preparation:   '+IRIS_humanreadabletime(aialog.tprep, totaltimetextlength=tlen)
  print, 'time level 1 to 2:  '+IRIS_humanreadabletime(aialog.t122, totaltimetextlength=tlen)

  !except = except_orig
end
