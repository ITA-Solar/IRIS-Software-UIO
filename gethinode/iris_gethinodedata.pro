;+
; NAME:
;       IRIS_GETINODEDATA
;
; PURPOSE:
;
;       IRIS_GETHINODEDATA downloads (if necessary) Hinode data that correspond to a specific IRIS OBS
;       and transforms it to IRIS-SJI-like level 2 fits file
;
; CATEGORY:
;       Martin Wiesmann / IRIS Data processing
;
; CALLING SEQUENCE:
;       IRIS_getHINODEdata, obs [, nodisplay=nodisplay, noprep=noprep, yscrsize=yscrsize, usehcr=usehcr,  $
;         nowidget=nowidget, outdir=outdir, adddatetree=adddatetree, addobsid=addobsid, $
;         fovexpand=fovexpand, timeexpand=timeexpand, deletetempfiles=deletetempfiles, _extra=_extra, $
;         fulltime=fulltime, fullfov=fullfov, $
;         timelowlimit=timelowlimit, arealowlimit=arealowlimit, $
;         debug=debug]
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
;       nodisplay: if set, suppresses the graphical output of iris_sot_umodes()
;       noprep: if set, suppresses the use of fg_prep (can also be set within the widget)
;       yscrsize: maximum size of the widget as a fraction of the screen size (default 0.85)
;       usehcr: instead of getting some information from IRIS level 2 files, the program will call iris_obs2hcr()
;         this option is not (yet) supported
;       nowidget: suppresses the widget interface, the parameters to use for the processing of the HINODE data are either
;         taken from the provided keywords, or the defaults. If nowidget is set, outdir is required as well.
;       outdir: the directory in which to save the resulting fits files, may be modified by the keywords adddatetree and
;         addobsid. If download is necessary, a subfolder 'original' will be created and the original files saved into that.
;       adddatetree: adds a date-tree structure to outdir (i.e. outdir/2015/10/23/)
;       addobsid: adds the OBS ID to outdir (i.e. outdir/20151023_171023_4203300028/), if adddatetree is also set
;         the OBS ID will be added at the end (i.e. outdir/2015/10/23/20151023_171023_4203300028/)
;       fovexpand: defines by how much the FOV should be expanded, as compared to IRIS FOV, in arcsecond, can be scalar,
;         then the same expansion will be applied to both the x- and y-axis. It can also be a 2-element vector, one element for
;         x- and one for y-axis, respectively.
;         (default: 0.0 arcsec)
;       timeexpand: defines by how much the time window of the IRIS OBS is expanded for AIA data, in minutes. If scalar,
;         the value will be applied to the start and the end time. Can be also a 2-element vector, for different expansions
;         at the start and at the end, respectively. (Positive numbers result in a wider time-window)
;         (default: 10.0 minutes)
;       fulltime: If set, all HINODE files of the selected umodes will be used, regardless of the time window
;       fullfov: If set, the original FOV of the HINODE umode will be transfered, regardless of IRIS FOV
;       deletetempfiles: if set, the downloaded files will be deleted after use
;       timelowlimit: used only if nowidget is set, Defines the lower limit of the IRIS time window covered by HINODE
;         in percent (default: 10.0 %)
;       arealowlimit: used only if nowidget is set, Defines the lower limit of the IRIS raster FOV covered by HINODE
;         in percent (default: 10.0 %)
;       _extra: will be passed to fg_prep
;
; OUTPUTS:
;       IRIS-SJI-like level 2 fits file of Hinode data, output directory is set within the widget
;
; CALLS:
;       A widget appears with some information about the IRIS OBS, and where you can also choose in which directory the
;       resulting files should be saved into. You can also expand the FOV of the Hinode data, both in X and Y direction,
;       and expand the time window. A first list shows the SOT entries that overlap with the IRIS OBS
;       (AREA_OVERLAP gives a rough estimate on the spatial overlap of HINODE and IRIS),
;       the second window shows the modes that the currently selected SOT entry contains. Pick one or more of the modes,
;       then click on 'Request Data' and the program creates IRIS-like level 2 files from the SOT files.
;       This tool works both within LMSAL (directly on the SOT database) and anywhere in the world with an internet connection.
;
; COMMON BLOCKS:
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;       2015: Martin Wiesmann (ITA, UIO). Gradually developed through the year.
;
; $Id: 2024-03-20 14:43 CET $  ;


pro iris_getHINODEdata_prepMODEdesc, info
  widget_control, info.b_fulltime, get_value=fulltime
  if fulltime[0] then begin
    info.umodesall[*].files_win = info.umodesall[*].files_tot
  endif else begin
    widget_control, info.ui_startobs, get_value=startobs
    widget_control, info.ui_expstart, get_value=startexpand
    startexpand=double(startexpand[0])
    widget_control, info.ui_endobs, get_value=endobs
    widget_control, info.ui_expstop, get_value=stopexpand
    stopexpand=double(stopexpand[0])
    time_window,[startobs[0],endobs[0]],mt0,mt1,minutes=[-startexpand,stopexpand] ; this expands input time +/- per user window
    startdatem=anytim2cal(mt0,form=8)
    stopdatem=anytim2cal(mt1,form=8)
    for isot=0,(size(info.umodesall, /dimensions))[0]-1 do begin
      for iumode=0,info.modespersot[isot]-1 do begin
        fileind=where((*info.fitsfiledates[isot,iumode] ge startdatem) AND (*info.fitsfiledates[isot,iumode] le stopdatem), count)
        info.umodesall[isot,iumode].files_win = count
      endfor
    endfor
  endelse
  isot = widget_info(info.foundSOT, /list_select)
  isot = isot[0]-1
  if isot ge 0 then begin
    modesel = widget_info(info.foundmodes, /list_select)
    ;    MODEdesc = get_infox(info.umodesall[isot,0:info.modespersot[isot]-1], 'OBS_TYPE, WAVE, NAXIS1, NAXIS2, NAXIS3, NAXIS, FILES_TOT, FILES_WIN', header=header, $
    ;      format='a,a,(I6),(I6),(I6),(I6),(I9),(I9)')
    MODEdesc = get_infox(info.umodesall[isot,0:info.modespersot[isot]-1], 'OBS_TYPE, WAVE, NAXIS1, NAXIS2, FILES_TOT, FILES_WIN, TIMESOT_OVERLAP, TIMEIRIS_OVERLAP', $
      titles='OBS_TYPE, WAVE, NAXIS1, NAXIS2, FILES_TOT, FILES_WIN, % TIME SOT, % TIME IRIS', $
      header=header, $
      format='a,a,(I6),(I6),(I9),(I9),(I10),(I11)')
    MODEdesc = [header, MODEdesc]
    widget_control, info.foundmodes, set_value = MODEdesc
    widget_control, info.foundmodes, set_list_select = modesel
  endif
end


pro IRIS_getHINODEdata_event, event
  widget_control, event.top, get_UValue=info, /No_Copy
  destroyed=0
  case event.id of
    info.b_outdir: begin
      out = info.outdir
      if out eq '' then cd, current=out
      outfile=dialog_pickfile(path=out, title='Please select a directory', get_path=outdir)
      if outdir ne '' then begin
        if strmid(outdir, 0,1, /reverse_offset) ne path_sep() then outdir = outdir+path_sep()
        info.outdir=outdir
        widget_control, info.b_addobsid, get_value=outoptions
        if outoptions[1] then outdir = outdir + info.obs2fov->get_obsid() + '/'
        widget_control, info.ui_outdir, set_value=outdir
      endif
    end

    info.b_addobsid: begin
      case event.value of
        0: begin
          if event.select then begin
            widget_control, info.b_addobsid, get_value=outoptions
            outoptions[1]=1
            widget_control, info.b_addobsid, set_value=outoptions
            widget_control, info.ui_outdir, set_value=info.dirl3
          endif else begin
            widget_control, info.ui_outdir, set_value=info.outdir+info.obsid+'/'
          endelse
        end
        1: begin
          widget_control, info.b_addobsid, get_value=outoptions
          if ~outoptions[0] then begin
            if event.select then begin
              widget_control, info.ui_outdir, set_value=info.outdir+info.obsid+'/'
            endif else begin
              widget_control, info.ui_outdir, set_value=info.outdir
            endelse
          endif else begin
            outoptions[1]=1
            widget_control, info.b_addobsid, set_value=outoptions
          endelse
        end
        else:
      endcase
    end

    info.b_fullfov: begin
      if event.select then begin
        widget_control, info.ui_expfovx, SENSITIVE=0
        widget_control, info.ui_expfovy, SENSITIVE=0
      endif else begin
        widget_control, info.ui_expfovx, SENSITIVE=1
        widget_control, info.ui_expfovy, SENSITIVE=1
      endelse
    end

    info.b_fulltime: begin
      if event.select then begin
        widget_control, info.ui_expstart, SENSITIVE=0
        widget_control, info.ui_expstop, SENSITIVE=0
      endif else begin
        widget_control, info.ui_expstart, SENSITIVE=1
        widget_control, info.ui_expstop, SENSITIVE=1
      endelse
      iris_getHINODEdata_prepMODEdesc, info
    end

    info.foundSOT: begin
      if event.index gt 0 then begin
        ;        MODEdesc = get_infox(info.umodesall[event.index-1,0:info.modespersot[event.index-1]-1], 'OBS_TYPE, WAVE, NAXIS1, NAXIS2, NAXIS3, NAXIS, FILES_TOT, FILES_WIN', header=header, $
        ;          format='a,a,(I6),(I6),(I6),(I6),(I9),(I9)')
        MODEdesc = get_infox(info.umodesall[event.index-1,0:info.modespersot[event.index-1]-1], 'OBS_TYPE, WAVE, NAXIS1, NAXIS2, FILES_TOT, FILES_WIN, TIMESOT_OVERLAP, TIMEIRIS_OVERLAP', $
          titles='OBS_TYPE, WAVE, NAXIS1, NAXIS2, FILES_TOT, FILES_WIN, % TIME SOT, % TIME IRIS', $
          header=header, $
          format='a,a,(I6),(I6),(I9),(I9),(I10),(I11)')
        MODEdesc = [header, MODEdesc]
        widget_control, info.foundmodes, set_value = MODEdesc
      endif else begin
        widget_control, info.foundmodes, set_value = ''
      endelse
    end

    info.ui_expstart: begin
      iris_getHINODEdata_prepMODEdesc, info
    end

    info.ui_expstop: begin
      iris_getHINODEdata_prepMODEdesc, info
    end

    info.b_quit: begin
      widget_control, event.top, /destroy
      destroyed=1
    end

    info.b_request: begin
      widget_control, info.ui_outdir, get_value=outdiruse
      outdiruse=outdiruse[0]
      if strmid(outdiruse, 0,1, /reverse_offset) ne path_sep() then outdiruse = outdiruse+path_sep()
      outdir = info.outdir
      widget_control, info.b_addobsid, get_value=outoptions
      deletetempfiles = outoptions[2]
      noprep = outoptions[3]
      widget_control, info.ui_expfovx, get_value=fovxexpand
      fovxexpand=double(fovxexpand[0])
      widget_control, info.ui_expfovy, get_value=fovyexpand
      fovyexpand=double(fovyexpand[0])
      info.obs2fov->set_fovexpand, fovxexpand, fovyexpand
      widget_control, info.ui_expstart, get_value=startexpand
      startexpand=double(startexpand[0])
      widget_control, info.ui_expstop, get_value=stopexpand
      stopexpand=double(stopexpand[0])
      info.obs2fov->set_timeexpand, startexpand, stopexpand
      widget_control, info.b_fulltime, get_value=fulltime
      fulltime=fulltime[0]
      widget_control, info.b_fullfov, get_value=fullfov
      fullfov=fullfov[0]
      if size(info.ex, /type) eq 8 then _extra=info.ex

      isot = widget_info(info.foundSOT, /list_select)
      isot = isot[0]-1
      if isot lt 0 then begin
        print, 'need to select a valid SOT entry'
        widget_control, event.top, set_UValue=info, /No_Copy
        return
      endif
      iumodes = widget_info(info.foundmodes, /list_select)
      iumodes = iumodes-1
      if iumodes[0] lt 0 then begin
        print, 'need to select at least one umode'
        widget_control, event.top, set_UValue=info, /No_Copy
        return
      endif
      umodes = (info.umodesall[isot,*])[iumodes]
      files = ptrarr(N_ELEMENTS(iumodes))
      for i=0,N_ELEMENTS(iumodes)-1 do begin
        files[i] = ptr_new(*(info.filesall[isot,iumodes[i]]))
      endfor


      save, outdir, outoptions, fovxexpand, fovyexpand, startexpand, stopexpand, $
        filename=IRISgetHINODE_appReadme()+'/iris_gethinodedata_params.sav'

      widget_control, event.top, /destroy
      destroyed=1

      IRIS_processHINODErequest, outdiruse, umodes, files,  info.no_download, info.obs2fov, fulltime=fulltime, fullfov=fullfov, $
        deletetempfiles=deletetempfiles, noprep=noprep, debug=info.debug, _extra=_extra, tprep1=info.tprep1

    end

    else:
  endcase
  if ~destroyed then widget_control, event.top, set_UValue=info, /No_Copy
end


pro IRIS_getHINODEdata, obs, nodisplay=nodisplay, noprep=noprep, yscrsize=yscrsize, usehcr=usehcr,  $
  nowidget=nowidget, outdir=outdir, adddatetree=adddatetree, addobsid=addobsid, no_download=no_download, $
  fovexpand=fovexpand, timeexpand=timeexpand, deletetempfiles=deletetempfiles, _extra=_extra, $
  fulltime=fulltime, fullfov=fullfov, $
  timelowlimit=timelowlimit, arealowlimit=arealowlimit, $
  debug=debug

  fovexpanddefault = 0.0 ;arcseconds ;;;;;;;;;;;;;;;;;DEFAULT
  timeexpanddefault = 10.0 ;minutes ;;;;;;;;;;;;;;;;;DEFAULT
  timelowlimitdefault = 10.0 ;percentage ;;;;;;;;;;;;;;;;;DEFAULT
  arealowlimitdefault = 10.0 ;percentage ;;;;;;;;;;;;;;;;;DEFAULT

  expandtimewindowforsearch=[-60,60]
  if keyword_set(nowidget) then expandtimewindowforsearch=[0,0]
  no_download = keyword_set(no_download)

  if N_ELEMENTS(timelowlimit) eq 0 then timelim = timelowlimitdefault $
  else timelim = timelowlimit
  if N_ELEMENTS(arealowlimit) eq 0 then arealim = arealowlimitdefault $
  else arealim = arealowlimit

  t01=systime(1)

  ;debug=1
  if keyword_set(debug) then begin
    ;obs='20140325_042023_3882010194'
    ;obs='20140101_230129_3840257196'
    ;obs='20150903_114923_3680338902'
    ;obs='20150302_000419_3820259492'
    ;obs='20151021_020318_3680188902' ; nice overlap but not nice images
    ;obs='20151006_080417_3620106129' ; nice example
    ;obs='20151002_080421_3680088902' ; another nice example
    ;nodisplay=1
    ;expandtimewindowforsearch=[-600,600]
  endif else debug=0

  obs2fov = obj_new('IRIS_obs2fov', obs, usehcr=usehcr, /hinode)
  if obs2fov->get_error() then begin
    print, 'Invalid input'
    return
  endif

  display = ~keyword_set(nodisplay) && ~keyword_set(nowidget)

  ;search SOT HCR for entries within time window of IRIS OBS +-1hr
  time_window,[obs2fov->get_startobs(),obs2fov->get_endobs()],minute=expandtimewindowforsearch,t0,t1
  sothcr=ssw_hcr_query(ssw_hcr_make_query(t0,t1,instrument='SOT'),/remove_dummy, count=count)
  if count eq 0 then begin
    box_message,'No SOT HCR entry within the time window of this IRIS OBS'
    sothcr=!NULL
  endif else begin
    
    ;get spatial overlap between IRIS and HINODE
    obs2fov->calc_overlap, sothcr

    ;create list of umodes of each SOT entry
    umodestemp = {sotid:'', obs_type:'', wave:'', naxis1:0, naxis2:0, naxis3:0, naxis:0, files_tot:0L, files_win:0L, $
      timesot_overlap:0.0, timeiris_overlap:0.0, sotstart:'', sotend:'', eventid:'', eventlink:'', $
      rasterOverlap:0, rasterOverlapPercent:0.0, SjiOverlapPercent:0.0, fileurl:0B}
    umodesall = make_array(N_ELEMENTS(sothcr), 30, value=umodestemp)
    filesall = ptrarr(N_ELEMENTS(sothcr),30)
    fitsfiledates = ptrarr(N_ELEMENTS(sothcr),30)
    modespersot = intarr(N_ELEMENTS(sothcr))
    taglist='obs_type,wave,naxis1,naxis2';,naxis3,naxis'

    ;time window to calculate number of files
    startdatem=anytim2cal(obs2fov->get_startobs(),form=8)
    stopdatem=anytim2cal(obs2fov->get_endobs(),form=8)
    startobs = obs2fov->get_startobs()
    tstart = str2utc(startobs)
    for isot=0,N_ELEMENTS(sothcr)-1 do begin
      files=''
      fileurl=0
      sot_cat,sothcr[isot].starttime,sothcr[isot].stoptime,cat,files,/use_level1_cat;, search=['OBSDIR=FG']
      if N_ELEMENTS(files) eq 1 && files[0] eq '' && ~no_download then begin
        sot_cat,sothcr[isot].starttime,sothcr[isot].stoptime,cat,files,/level0,/urls
        fileurl=1
      endif
      sotid = anytim2cal(strmid(sothcr[isot].eventid, strpos(sothcr[isot].eventid, 'Obs')+4, 19), form=8)
      if N_ELEMENTS(files) ge 1 && files[0] ne '' then begin
        umodes=iris_sot_umodes(incat=cat, mcount=mcount, taglist=taglist, display=display, $
          iris_start=obs2fov->get_startobs(), iris_end=obs2fov->get_endobs(), $
          iris_obsid=obs2fov->get_obsid()) ; uniq "modes" in this CAT
        modespersot[isot] = N_ELEMENTS(umodes)
        ;create list of files and structure of each umode
        for imode=0, modespersot[isot]-1 do begin
          temp = strtrim(strsplit(umodes[imode], '  ', /regex, /extract),2)
          umodesall[isot,imode].sotid = sotid
          umodesall[isot,imode].obs_type = temp[0]
          umodesall[isot,imode].wave = temp[1]
          umodesall[isot,imode].naxis1 = temp[2]
          umodesall[isot,imode].naxis2 = temp[3]
          ;umodesall[isot,imode].naxis3 = temp[4]
          ;umodesall[isot,imode].naxis = temp[5]
          umodesall[isot,imode].files_tot = mcount[imode]
          ss=sot_umodes(incat=cat,where_mode=umodes[imode]) ; subscripts of
          filesall[isot,imode]=ptr_new(files[ss])
          fitsfiledates[isot,imode] = ptr_new(anytim2cal(file2time(file_basename(files[ss])), form=8))
          fileind=where((*fitsfiledates[isot,imode] ge startdatem) AND (*fitsfiledates[isot,imode] le stopdatem), count)
          umodesall[isot,imode].files_win = count
          umodesall[isot,imode].timesot_overlap = 100.0 * umodesall[isot,imode].files_win / $
            umodesall[isot,imode].files_tot
          sottime = str2utc(cat[ss].date_obs)
          sottime = (sottime.mjd-tstart.mjd)*86400L + (sottime.time-tstart.time)/1000d
          timeind = where((sottime ge 0) AND (sottime le obs2fov->get_tobs()), count)
          if count gt 0 then umodesall[isot,imode].timeiris_overlap = 100.0 * $
            (sottime[timeind[count-1]]-sottime[timeind[0]]) / obs2fov->get_tobs()
          umodesall[isot,imode].sotstart = sothcr[isot].starttime
          umodesall[isot,imode].sotend = sothcr[isot].stoptime
          umodesall[isot,imode].eventid = sothcr[isot].eventid
          umodesall[isot,imode].eventlink = 'http://www.lmsal.com/hek/hcr?cmd=view-event&event-id=' + $
            url_encode(sothcr[isot].eventid)
          umodesall[isot,imode].rasterOverlap = sothcr[isot].arearas_overlap ge arealim
          umodesall[isot,imode].rasterOverlapPercent = sothcr[isot].arearas_overlap
          umodesall[isot,imode].SjiOverlapPercent = sothcr[isot].areasji_overlap
          umodesall[isot,imode].fileurl = fileurl
        endfor
      endif
      overlap = 100.0 * total(umodesall[isot,*].files_win) / total(umodesall[isot,*].files_tot)
      newsottemp = create_struct(sothcr[isot], 'timesot_overlap', overlap, 'timeiris_overlap', $
        mean(umodesall[isot,0:modespersot[isot]-1].timeiris_overlap))
      if isot eq 0 then newsot = newsottemp $
      else newsot = [newsot, newsottemp]
    endfor
    sothcr = newsot
  endelse ;check if there is a sot entry

  t2=systime(1)
  tprep1=t2-t01




  if keyword_set(nowidget) then begin
    ;no widget interface
    
    if N_ELEMENTS(outdir) eq 0 then begin
      print, 'need an output directory (outdir)'
      return
    endif
    outdiruse = outdir
    if strmid(outdiruse, 0,1, /reverse_offset) ne path_sep() then outdiruse = outdiruse+path_sep()
    if keyword_set(adddatetree) then begin
      outdiruse = ssw_time2paths(obs2fov->get_startobs(),obs2fov->get_startobs(), outdiruse)
      if strmid(outdiruse, 0,1, /reverse_offset) ne path_sep() then outdiruse = outdiruse+path_sep()
    endif
    if keyword_set(addobsid) then outdiruse = outdiruse + obs2fov->get_obsid() + '/'
    if N_ELEMENTS(fovexpand) eq 0 then obs2fov->set_fovexpand, fovexpanddefault $
    else obs2fov->set_fovexpand, fovexpand
    if N_ELEMENTS(timeexpand) eq 0 then obs2fov->set_timeexpand, timeexpanddefault $
    else obs2fov->set_timeexpand, timeexpand

    ; calculate which umodes to use (and files)
    ; might be multiple SOT entries
    umodes=!NULL
    for isot=0,N_ELEMENTS(sothcr)-1 do begin
      if sothcr[isot].areasji_overlap ge arealim then begin
        for imode=0, modespersot[isot]-1 do begin
          if umodesall[isot,imode].timeiris_overlap ge timelim then begin
            if N_ELEMENTS(umodes) eq 0 then begin
              umodes = umodesall[isot,imode]
              files = ptr_new(*(filesall[isot,imode]))
            endif else begin
              umodes = [umodes, umodesall[isot,imode]]
              files = [files, ptr_new(*(filesall[isot,imode]))]
            endelse
          endif
        endfor
      endif
    endfor

    if N_ELEMENTS(umodes) eq 0 then box_message,'No SOT OBS overlaps with this IRIS OBS'
    IRIS_processHINODErequest, outdiruse, umodes, files,  no_download, obs2fov, fulltime=fulltime, fullfov=fullfov, $
      deletetempfiles=deletetempfiles, noprep=noprep, debug=debug, _extra=_extra, tprep1=tprep1






  endif else begin
    ;use widget interface

    if N_ELEMENTS(sothcr) eq 0 then return

    if ~keyword_set(yscrsize) then begin
      yscrsize=0.85
    endif else begin
      if yscrsize gt 1 || yscrsize lt 0 then begin
        print, 'yscrsize has to be between 0 and 1, setting it to 0.85'
        yscrsize=0.85
      endif
    endelse

    paramfile=IRISgetHINODE_appReadme()+'/iris_gethinodedata_params.sav'
    if file_test(paramfile) then begin
      restore,paramfile
      ;paramfile contains the variables:
    endif
    if N_ELEMENTS(outdir) eq 0 then cd,current=outdir
    if strmid(outdir, 0,1, /reverse_offset) ne path_sep() then outdir = outdir+path_sep()
    if N_ELEMENTS(outoptions) eq 0 then outoptions=[0,1,0,0]
    if N_ELEMENTS(outoptions) eq 1 then outoptions=[outoptions,1,0,0]
    if N_ELEMENTS(outoptions) eq 2 then outoptions=[outoptions,0,0]
    if N_ELEMENTS(outoptions) eq 3 then outoptions=[outoptions,0]
    if N_ELEMENTS(addobsid) eq 1 then outoptions[1] = addobsid
    if N_ELEMENTS(deletetempfiles) eq 1 then outoptions[2] = deletetempfiles
    if N_ELEMENTS(noprep) eq 1 then outoptions[3]=noprep
    if N_ELEMENTS(fovxexpand) eq 0 then fovxexpand=fovexpanddefault
    if N_ELEMENTS(fovyexpand) eq 0 then fovyexpand=fovexpanddefault
    if keyword_set(fovexpand) then begin
      fovxexpand = fovexpand[0]
      fovyexpand = fovexpand[N_ELEMENTS(fovxexpand)-1]
    endif
    if N_ELEMENTS(startexpand) eq 0 then startexpand=timeexpanddefault
    if N_ELEMENTS(stopexpand) eq 0 then stopexpand=timeexpanddefault
    if keyword_set(timeexpand) then begin
      startexpand = timeexpand[0]
      stopexpand = timeexpand[N_ELEMENTS(timeexpand)-1]
    endif
    fulltime = keyword_set(fulltime)
    fullfov = keyword_set(fullfov)
    if N_ELEMENTS(_extra) eq 0 then ex=0 $
    else ex = _extra

    ;create list of found SOT entries
    SOTdesc = get_infox(sothcr, 'STARTTIME, STOPTIME, TARGET, XCEN, YCEN, XFOV, YFOV, '+$
      'AREASJI_OVERLAP, AREARAS_OVERLAP, TIMESOT_OVERLAP, TIMEIRIS_OVERLAP', $
      header=header, $
      titles='STARTTIME, STOPTIME, TARGET, XCEN, YCEN, XFOV, YFOV, % AREA SJI, % AREA RAS, % TIME SOT, % TIME IRIS', $
      format='a,a,a,(f7.1),(f7.1),(f7.1),(f7.1),(I10),(I10),(I10),(I11)')
    SOTdesc = [header, SOTdesc]


    constants = obj_new('IRISsim_constants')
    ;  rootl3=getenv('IRIS_DATA')+'/level3' ;is usually not implemented
    rootl3=constants->get_data_path_uio_l3()
    yyyy=strmid(obs2fov->get_obsid(),0,4)
    mm=strmid(obs2fov->get_obsid(),4,2)
    dd=strmid(obs2fov->get_obsid(),6,2)
    dirl3=rootl3+'/'+yyyy+'/'+mm+'/'+dd+'/'+obs2fov->get_obsid()+'/'
    if outoptions[0] then outdiruse=dirl3 $
    else if outoptions[1] then outdiruse=outdir+obs2fov->get_obsid()+'/' $
    else outdiruse=outdir



    ; MainWindow ; Base-Widget
    MainWindow = WIDGET_BASE(/Column, title='IRIS - Get HINODE overlap', XOffset=100, YOffset=100)

    leftlabel=100
    textsize=80

    Base1 = WIDGET_BASE(MainWindow, /Row)
    label = WIDGET_LABEL(Base1, value='OBS ID', /align_left, xsize=leftlabel)
    ui_obsid = WIDGET_TEXT(Base1, value=obs2fov->get_obsid(), xsize=textsize)

    Base2 = WIDGET_BASE(MainWindow, /Row)
    label = WIDGET_LABEL(Base2, value='OBS Directory', /align_left, xsize=leftlabel)
    ui_obsdir = WIDGET_TEXT(Base2, value=obs2fov->get_obsdir(), xsize=textsize)

    Base3 = WIDGET_BASE(MainWindow, /Row)
    label = WIDGET_LABEL(Base3, value='Output directory', /align_left, xsize=leftlabel)
    ui_outdir = WIDGET_TEXT(Base3, value=outdiruse, /editable, xsize=textsize)
    b_outdir = WIDGET_BUTTON(Base3, value='Change')

    Base3a = WIDGET_BASE(MainWindow, /Row)
    label = WIDGET_LABEL(Base3a, value=' ', /align_left, xsize=leftlabel)
    b_addobsid = CW_BGROUP(Base3a, ['Save in IRIS_DATA/level3 folder', $
      'Add OBS ID to output directory',$
      'Delete HINODE fits files after use',$
      'do not use fg_prep'], $
      /nonexclusive, set_value=outoptions)

    ;  Base5 = WIDGET_BASE(MainWindow, /Row)
    ;  label = WIDGET_LABEL(Base5, value=' ', /align_left, xsize=leftlabel)

    Base6 = WIDGET_BASE(MainWindow, /Row)
    label = WIDGET_LABEL(Base6, value='Cutout center', /align_left, xsize=leftlabel)
    ui_xcen = CW_FIELD(Base6, title='X', value=string(obs2fov->get_xcen(), format='(f8.1)'), xsize=8, /noedit)
    label = WIDGET_LABEL(Base6, value=' ', /align_left, xsize=86)
    ui_ycen = CW_FIELD(Base6, title='Y', value=string(obs2fov->get_ycen(), format='(f8.1)'), xsize=8, /noedit)
    label = WIDGET_LABEL(Base6, value=' arcsec', /align_left)

    Base7 = WIDGET_BASE(MainWindow, /Row)
    label = WIDGET_LABEL(Base7, value='Cutout FOV', /align_left, xsize=leftlabel)
    ui_xfov = CW_FIELD(Base7, title='X', value=string(obs2fov->get_fovx(), format='(f8.1)'), xsize=8, /noedit)
    ui_expfovx = CW_FIELD(Base7, title='+', value=fovxexpand, /float, xsize=4)
    label = WIDGET_LABEL(Base7, value=' ', /align_left, xsize=20)
    ui_yfov = CW_FIELD(Base7, title='Y', value=string(obs2fov->get_fovy(), format='(f8.1)'), xsize=8, /noedit)
    ui_expfovy = CW_FIELD(Base7, title='+', value=fovyexpand, /float, xsize=4)
    label = WIDGET_LABEL(Base7, value=' arcsec', /align_left)
    label = WIDGET_LABEL(Base7, value=' ', /align_left, xsize=20)
    b_fullfov = CW_BGROUP(Base7, ['Use full SOT FOV'], $
      /nonexclusive, set_value=[fullfov])

    Base8 = WIDGET_BASE(MainWindow, /Row)
    label = WIDGET_LABEL(Base8, value='Start time', /align_left, xsize=leftlabel)
    ui_startobs = WIDGET_TEXT(Base8, value=obs2fov->get_startobs(), xsize=22)
    ui_expstart = CW_FIELD(Base8, title='-', value=startexpand, /float, xsize=4, /all_events)
    label = WIDGET_LABEL(Base8, value='minutes', /align_left)
    label = WIDGET_LABEL(Base8, value=' ', /align_left, xsize=20)
    b_fulltime = CW_BGROUP(Base8, ['Use full SOT time-window'], $
      /nonexclusive, set_value=[fulltime])

    Base9 = WIDGET_BASE(MainWindow, /Row)
    label = WIDGET_LABEL(Base9, value='Stop time', /align_left, xsize=leftlabel)
    ui_endobs = WIDGET_TEXT(Base9, value=obs2fov->get_endobs(), xsize=22)
    ui_expstop = CW_FIELD(Base9, title='+', value=stopexpand, /float, xsize=4, /all_events)
    label = WIDGET_LABEL(Base9, value='minutes', /align_left)

    case !version.os of
      'MacOS': begin
        xsize = 150
        ysize = 4
      end
      else: begin
        xsize = 150 ; in cm as units=2
        ysize = 4  ; in cm as units=2
      end
    endcase

    label = WIDGET_LABEL(MainWindow, value='Select SOT entry', /align_left, xsize=leftlabel)
    foundSOT=widget_list(MainWindow, value='', /frame, xsize = xsize $
      , scr_ysize = 0, units = 2)
    label = WIDGET_LABEL(MainWindow, value='Select umode(s)', /align_left, xsize=leftlabel)
    foundmodes=widget_list(MainWindow, value='', /frame, xsize = xsize, /multiple $
      , scr_ysize = 0, units = 2)

    Base11 = WIDGET_BASE(MainWindow, /Row)
    label = WIDGET_LABEL(Base11, value=' ', /align_left, xsize=20)
    b_quit = WIDGET_BUTTON(Base11, value='QUIT')
    label = WIDGET_LABEL(Base11, value=' ', /align_left, xsize=80)
    b_request = WIDGET_BUTTON(Base11, value='Request Data', xsize=220)

    geometry = widget_info(MainWindow,/geometry)
    screen = get_screen_size()
    space = float(screen[1])*yscrsize - float(geometry.scr_ysize)
    if space lt 1100 then widget_control,MainWindow,yoffset=0
    if space gt 900 then space = 900
    if space lt 100 then space = 100
    space = space / 5.0
    widget_control, foundSOT, scr_ysize=space*2
    widget_control, foundmodes, scr_ysize=space*3
    widget_control, foundSOT, set_value = SOTdesc
    widget_control, foundSOT, set_list_select = 1


    widget_control, MainWindow, /realize



    ;define structure with information to be passed around
    info = { $
      obs:obs, $
      obs2fov:obs2fov, $
      ui_obsid:ui_obsid, $
      ui_obsdir:ui_obsdir, $
      ui_outdir:ui_outdir, $
      outdir:outdir, $
      dirl3:dirl3, $
      b_outdir:b_outdir, $
      b_addobsid:b_addobsid, $
      ui_xcen:ui_xcen, $
      ui_ycen:ui_ycen, $
      ui_xfov:ui_xfov, $
      ui_yfov:ui_yfov, $
      ui_expfovx:ui_expfovx, $
      ui_expfovy:ui_expfovy, $
      b_fullfov:b_fullfov, $
      ui_startobs:ui_startobs, $
      ui_endobs:ui_endobs, $
      ui_expstart:ui_expstart, $
      ui_expstop:ui_expstop, $
      b_fulltime:b_fulltime, $
      foundSOT:foundSOT, $
      foundmodes:foundmodes, $
      umodesall:umodesall, $
      filesall:filesall, $
      fitsfiledates:fitsfiledates, $
      modespersot:modespersot, $
      no_download:no_download, $
      b_quit:b_quit, $
      b_request:b_request, $
      ex:ex, $
      tprep1:tprep1, $
      debug:debug $
    }

    iris_getHINODEdata_prepMODEdesc, info

    ;set this structure as the user-defined value of mainwindow
    widget_control, MainWindow, set_UValue=info, /No_Copy

    xmanager, 'IRIS_getHINODEdata', MainWindow, /no_block

  endelse ;use widget interface
end
