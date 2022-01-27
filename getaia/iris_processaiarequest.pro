; $Id: iris_processaiarequest.pro,v 1.35 2017/12/08 20:26:46 mawiesma Exp $  ;

pro IRIS_processAIArequest, obs2fov, waves=waves, $
  tres=tres, maxframes=maxframes, method=method, $
  outdir=outdir, email=email, deletetempfiles=deletetempfiles, $
  instrument=instrument, query_source=query_source, $
  _extra=_extra, tprep1=tprep1, $
  redol2=redol2, redownload=redownload, rerequest=rerequest, $
  debug=debug
  
  ; Get current allocation and reset the high water mark:
  start_mem = MEMORY(/CURRENT)
  
  procversion = 8 ;version of IRIS_processAIArequest, including IRIS_requestAIA and IRIS_downloadAIA
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
    procversion:procversion, $
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


  t1=systime(1)
  
  if keyword_set(outdir) then $
    if strmid(outdir, 0,1, /reverse_offset) ne path_sep() then outdir = outdir+path_sep()

  paramfile=0
  if N_PARAMS() lt 1 then begin
    if keyword_set(outdir) then begin
      if file_test(outdir+'cutout_params.sav') then paramfile=1 $
      else begin
        print,'not enough input'
        return
      endelse
    endif else begin
      print,'not enough input'
      return
    endelse
  endif
  
  com = 'IRIS_processAIArequest, outdir='''+outdir+'''
  
  if paramfile then begin
    if N_ELEMENTS(waves) gt 0 then waves2=waves
    restore, outdir+'cutout_params.sav'
    if N_ELEMENTS(waves2) gt 0 then waves=waves2
  endif else begin
    file_mkdir, outdir
    if ~keyword_set(waves) then begin
      print, 'no wavelengths defined, downloading all'
      waves=['94','131','171','193','211','304','335','1600','1700','4500']
      print, waves
    endif
    obs2fov->get_startendtimes, starttime, endtime
    jobID=''
    datarequested=0
    downloadfinished=0
    l2created=0
    save, starttime, endtime, waves, obs2fov, $
      maxframes, query_source, email, instrument, _extra, deletetempfiles, $
      jobID, datarequested, downloadfinished, l2created, $
      tres, method, aiafilesl1, nfilesl1, filesfound, $
      filename=outdir+'cutout_params.sav'
    box_message, ['to continue/redo the process:', com]
    OpenW, unit, outdir+'readme.txt', /get_lun
    printf, unit, 'If you need to recreate the level 2,'
    printf, unit, 'or if the process was disrupted and you want to continue it,'
    printf, unit, 'then enter the following command into an IDL session:'
    printf, unit, ' '
    printf, unit, com
    printf, unit, ' '
    printf, unit, 'set the keywords /rerequest, /redownload and/or /redol2 if desired'
    printf, unit, 'the processes which have not finished will be redone automatically'
    Close, unit
    free_lun, unit
  endelse
  
  if N_ELEMENTS(method) eq 0 then method=0
  if N_ELEMENTS(tres) eq 0 then tres=12.0
  
  logfilename = outdir+'aia_l2_'+obs2fov->get_obsid()

  t2=systime(1)
  aialog.tprep=t2-t1
  if keyword_set(tprep1) then aialog.tprep = aialog.tprep + tprep1
  
  if ~datarequested || keyword_set(rerequest) then begin
    downloadfinished=0
    l2created=0
    case method of
      0: begin
        safeadd = 20
        IRIS_requestAIA, starttime, endtime, obs2fov->get_xcen(), obs2fov->get_ycen(), $
          obs2fov->get_fovx(/expsatrot)+safeadd, obs2fov->get_fovy(/expsatrot)+safeadd, waves, $
          obsid=obs2fov->get_obsid(), $
          email=email, maxframes=maxframes, instrument=instrument, query_source=query_source, $
          _extra=_extra, $
          jobID=jobID
      end
      
      1: begin
        filesfound = ptrarr(N_ELEMENTS(waves))
        nfilesl1 = lonarr(N_ELEMENTS(waves))
        for iwave=0,N_ELEMENTS(waves)-1 do begin
          print,'Wavelength: '+waves[iwave]+' A'
          files = vso_search(starttime, endtime, instr=instrument, res=1, wave=waves[iwave], sample=tres, count=count)
          nfilesl1[iwave] = count
          if count gt 0 then filesfound[iwave] = ptr_new(files)
        endfor
      end
      
      2: begin
        aiafilesl1 = ptrarr(N_ELEMENTS(waves))
        nfilesl1 = lonarr(N_ELEMENTS(waves))
        startdate=anytim2cal(starttime,form=8)
        stopdate=anytim2cal(endtime,form=8)
        for iwave=0,N_ELEMENTS(waves)-1 do begin
          ;old procversion = 4
          ;files = ssw_time2paths(starttime, endtime, '/archive/sdo/AIA/lev1/', file_pat='*'+waves[iwave]+'.fits', /hourly, count=count)
          ;new procversion = 5
          ssw_jsoc_time2data, starttime, endtime, index, files, /files, wave=waves[iwave]
          if size(index, /type) eq 8 then begin
            good = where(index.quality EQ 0, count)
            if count gt 0 then files = files[good]
          endif else count=0
          ;endnew 5
          nfilesl1[iwave] = count
          if count gt 0 then aiafilesl1[iwave] = ptr_new(files)
;          if count gt 0 then begin
;            ;remove unnecessary files
;            filedates=anytim2cal(file2time(file_basename(files)), form=8)
;            fileind=where((filedates ge startdate) AND (filedates le stopdate), count)
;            nfilesl1[iwave] = count
;            if count gt 0 then aiafilesl1[iwave] = ptr_new(files[fileind])
;          endif
        endfor
        downloadfinished=1
      end
      
      else: begin
        print,'unknown method'
        return
      end
    endcase

    datarequested=1
    save, starttime, endtime, waves, obs2fov, $
      maxframes, query_source, email, instrument, _extra, deletetempfiles, $
      jobID, datarequested, downloadfinished, l2created, $
      tres, method, aiafilesl1, nfilesl1, filesfound, $
      filename=outdir+'cutout_params.sav'
    box_message, ['to continue/redo the process:', com+'[, /rerequest]']
  endif
  
  t3=systime(1)
  aialog.treq=t3-t2
  
  twait=0
  if ~downloadfinished || keyword_set(redownload) then begin
    l2created=0
    case method of
      0: begin
        IRIS_downloadAIA, outdir, jobID, twait=twait
        aialog.twait = twait
      end
      
      1: begin
        aiafilesl1 = ptrarr(N_ELEMENTS(waves))
        file_mkdir,outdir+'original/'
        for iwave=0,N_ELEMENTS(waves)-1 do begin
          if nfilesl1[iwave] gt 0 then begin
            result = vso_get(*filesfound[iwave], out_dir=outdir+'original/', filenames=filenames, /force)
            aiafilesl1[iwave] = ptr_new(filenames)
          endif
        endfor
      end
      
      2: ;no download needed
      
      else: begin
        print,'unknown method'
        return
      end
    endcase

    downloadfinished=1
    save, starttime, endtime, waves, obs2fov, $
      maxframes, query_source, email, instrument, _extra, deletetempfiles, $
      jobID, datarequested, downloadfinished, l2created, $
      tres, method, aiafilesl1, nfilesl1, filesfound, $
      filename=outdir+'cutout_params.sav'
    box_message, ['to continue/redo the process:', com+'[, /redownload, /rerequest]']
  endif
  
  t4=systime(1)
  aialog.tdownload=t4-t3-aialog.twait
  
  if ~l2created || keyword_set(redol2) then begin
    iris_l1to2aia, outdir, obs2fov, waves=waves, $
      deletetempfiles=deletetempfiles, method=method, aiafilesl1=aiafilesl1, nfilesl1=nfilesl1, $
      aialog=aialog, debug=debug, _extra=_extra
      
    l2created=1
    save, starttime, endtime, waves, obs2fov, $
      maxframes, query_source, email, instrument, _extra, deletetempfiles, $
      jobID, datarequested, downloadfinished, l2created, $
      tres, method, aiafilesl1, nfilesl1, filesfound, $
      filename=outdir+'cutout_params.sav'
    box_message, ['to continue/redo the process:', com+'[, /redol2, /redownload, /rerequest]']
  endif
  
  t5=systime(1)
  aialog.t122=t5-t4
  aialog.ttotal = t5-t1

  aialog.memory = MEMORY(/HIGHWATER) - start_mem
  IRISgetAIA_writelogfile, aialog, obs2fov, logfilename, sswjobid=jobID
  
  ttotal = IRIS_humanreadabletime(aialog.ttotal)
  tlen = strlen(ttotal)
  print, 'time total:         '+ttotal
  print, 'time preparation:   '+IRIS_humanreadabletime(aialog.tprep, totaltimetextlength=tlen)
  print, 'time request:       '+IRIS_humanreadabletime(aialog.treq, totaltimetextlength=tlen)
  print, 'time wait for data: '+IRIS_humanreadabletime(twait, totaltimetextlength=tlen)
  print, 'time download:      '+IRIS_humanreadabletime(aialog.tdownload, totaltimetextlength=tlen)
  print, 'time level 1 to 2:  '+IRIS_humanreadabletime(aialog.t122, totaltimetextlength=tlen)

  !except = except_orig
end
