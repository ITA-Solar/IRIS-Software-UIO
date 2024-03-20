;+
; NAME:
;       IRIS_getAIAdata
;
; PURPOSE:
;
;       IRIS_getAIAdata downloads (if necessary) AIA data that correspond to a specific IRIS OBS
;       and transforms it to IRIS-SJI-like level 2 fits file
;
; CATEGORY:
;       Martin Wiesmann / IRIS Data processing
;
; CALLING SEQUENCE:
;       IRIS_getAIAdata, obs [, usehcr=usehcr, $
;         nowidget=nowidget, outdir=outdir, adddatetree=adddatetree, addobsid=addobsid, $
;         waves=waves, fovexpand=fovexpand, timeexpand=timeexpand, method=method, $
;         deletetempfiles=deletetempfiles, maxframes=maxframes, tres=tres, _extra=_extra]
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
;       usehcr: instead of getting some information from IRIS level 2 files, the program will call iris_obs2hcr()
;         this option is not (yet) supported
;       nowidget: suppresses the widget interface, the parameters to use for the processing of the AIA data are either
;         taken from the provided keywords, or the defaults. If nowidget is set, outdir is required as well.
;       outdir: the directory in which to save the resulting fits files, may be modified by the keywords adddatetree and
;         addobsid. If download is necessary, a subfolder 'original' will be created and the original files saved into that.
;       adddatetree: adds a date-tree structure to outdir (i.e. outdir/2015/10/23/)
;       addobsid: adds the OBS ID to outdir (i.e. outdir/20151023_171023_4203300028/), if adddatetree is also set
;         the OBS ID will be added at the end (i.e. outdir/2015/10/23/20151023_171023_4203300028/)
;       waves: a list of wavelengths to be transformed, must be an array of strings
;         (default: ['94','131','171','193','211','304','335','1600','1700','4500'] (all wavelengths))
;       fovexpand: defines by how much the FOV should be expanded, as compared to IRIS FOV, in arcsecond, can be scalar,
;         then the same expansion will be applied to both the x- and y-axis. It can also be a 2-element vector, one element for
;         x- and one for y-axis, respectively.
;         (default: 100.0 arcsec)
;       timeexpand: defines by how much the time window of the IRIS OBS is expanded for AIA data, in minutes. If scalar,
;         the value will be applied to the start and the end time. Can be also a 2-element vector, for different expansions
;         at the start and at the end, respectively. (Positive numbers result in a wider time-window)
;         (default: 10.0 minutes)
;       maxframes: maximum number of frames per wavelength, used in method 0 (default: 50)
;       tres: temporal resolution of AIA data, used in method 1
;       method: choose the method (set to the corresponding number)
;         0: SSW_CUTOUT: Calls ssw_cutout_service
;              The data is prepared on the server and when it is ready
;              it will be downloaded, the files to download are very small,
;              but the preparation can take a very long time (up to several hours).
;         1: FULL FRAME: The full frame AIA data is first downloaded and then prepared locally
;              the files to download are very large, but download speed is much higher
;              compared to ssw_cutout, and there is no waiting time for the data.
;              This method is recommended if you have good internet connection and lots of space
;              on your harddisk
;         2: ARCHIVE: No download of any data, it is assumed that you have access to
;              /archive/sdo/AIA/lev1/
;              preparation is done directly on the archived file
;              max # frames is ignored, all available file are used
;              recommended if you run this program at Lockheed
;              (default when nowidget keyword is set)
;       deletetempfiles: if set, the downloaded files will be deleted after use
;       _extra: will be passed to aia_prep (/use_shared, /noshell, /uncomp_delete are set)
;               and ssw_cutout_service
;       
; OUTPUTS:
;       IRIS-SJI-like level 2 fits file of AIA data, output directory is set within the widget
;
; CALLS:
;       A widget appears with some information about the IRIS OBS, and where you can also choose in which directory the 
;       resulting files should be saved into. You can also expand the FOV of the AIA data, both in X and Y direction,
;       and expand the time window. Pick one or more of the wavelengths, 
;       then click on 'Request Data' and the program creates IRIS-like level 2 files from the AIA files.
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

pro IRIS_getAIAdata_event, event
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
        if outoptions[1] then outdir = outdir + info.obs + '/'
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
            widget_control, info.ui_outdir, set_value=info.outdir+info.obs+'/'
          endelse
        end
        1: begin
          widget_control, info.b_addobsid, get_value=outoptions
          if ~outoptions[0] then begin
            if event.select then begin
              widget_control, info.ui_outdir, set_value=info.outdir+info.obs+'/'
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

    info.b_wavesall: BEGIN
      widget_control, info.b_wavesall, get_value=val
      if val[0] eq 1 then widget_control, info.b_waves, set_value=make_array(12,value=1) $
      else widget_control, info.b_waves, set_value=make_array(12,value=0)
    end

    info.b_waves: begin
      widget_control, info.b_waves, get_value=val
      if total(val) eq 12 then widget_control, info.b_wavesall, set_value=[1] $
      else widget_control, info.b_wavesall, set_value=[0]
    end

    info.ui_maxframes: begin
      widget_control, info.ui_maxframes, get_value=maxframes
      tres=fix(info.obs2fov->get_tobs()/maxframes)
      if tres lt 12 then tres=12
      widget_control, info.ui_tres, set_value=tres
    end

    info.ui_tres: begin
      widget_control, info.ui_tres, get_value=tres
      if tres lt 12 then tres=12
      maxframes=ceil(info.obs2fov->get_tobs()/tres)
      widget_control, info.ui_tres, set_value=tres
      widget_control, info.ui_maxframes, set_value=strtrim(string(maxframes),2)
    end

    info.b_quit: begin
      widget_control, event.top, /destroy
      destroyed=1
    end

    info.b_request: begin
      ;widget_control, info.ui_obsid, get_value=obsid
      ;obsid=obsid[0]
      ;widget_control, info.ui_obsdir, get_value=obsdir
      ;obsdir=obsdir[0]
      ;if strmid(obsdir, 0,1, /reverse_offset) ne path_sep() then obsdir = obsdir+path_sep()
      widget_control, info.ui_email, get_value=email
      email=email[0]
      widget_control, info.b_email, get_value=emailnot
      if emailnot[0] eq 0 then email2='' else email2=email
      widget_control, info.ui_outdir, get_value=outdiruse
      outdiruse=outdiruse[0]
      if strmid(outdiruse, 0,1, /reverse_offset) ne path_sep() then outdiruse = outdiruse+path_sep()
      outdir = info.outdir
      widget_control, info.b_addobsid, get_value=outoptions
      deletetempfiles = outoptions[2]
      widget_control, info.ui_instrume, get_value=instrument
      instrument=STRLOWCASE(instrument[0])
      widget_control, info.b_waves, get_value=wavesoption
      ind = where(wavesoption eq 1, c)
      if c gt 0 then waves=info.waves_aia[ind] $
      else begin
        print, 'no wavelengths chosen'
        widget_control, event.top, set_UValue=info, /No_Copy
        return
      endelse
      ;widget_control, info.ui_xcen, get_value=xcen
      ;xcen=double(xcen[0])
      ;widget_control, info.ui_ycen, get_value=ycen
      ;ycen=double(ycen[0])
      ;widget_control, info.ui_xfov, get_value=fovx
      widget_control, info.ui_expfovx, get_value=fovxexpand
      fovxexpand=double(fovxexpand[0])
      ;widget_control, info.ui_yfov, get_value=fovy
      widget_control, info.ui_expfovy, get_value=fovyexpand
      fovyexpand=double(fovyexpand[0])
      info.obs2fov->set_fovexpand, fovxexpand, fovyexpand
      ;      fovx=double(fovx[0])+fovxexpand*abs(cos(info.satrot))+fovyexpand*abs(sin(info.satrot))+30
      ;      fovy=double(fovy[0])+fovyexpand*abs(cos(info.satrot))+fovxexpand*abs(sin(info.satrot))+30
      ;widget_control, info.ui_startobs, get_value=startobs
      widget_control, info.ui_expstart, get_value=startexpand
      startexpand=double(startexpand[0])
      ;widget_control, info.ui_endobs, get_value=endobs
      widget_control, info.ui_expstop, get_value=stopexpand
      stopexpand=double(stopexpand[0])
      info.obs2fov->set_timeexpand, startexpand, stopexpand
      widget_control, info.ui_maxframes, get_value=maxframes
      maxframes=long(maxframes[0])
      widget_control, info.ui_tres, get_value=tres
      tres=tres[0]
      instrumentindex=info.instrumentindex
      methodoption=info.methodoption
      if size(info.ex, /type) eq 8 then _extra=info.ex

      save, email, emailnot, outdir, outoptions, wavesoption, fovxexpand, fovyexpand, methodoption, $
        startexpand, stopexpand, instrumentindex, $
        filename=IRISgetAIA_appReadme()+'/iris_getaiadata_params.sav'

      widget_control, event.top, /destroy
      destroyed=1

      IRIS_processAIArequest, info.obs2fov, waves=waves, $
        tres=tres, maxframes=maxframes, method=methodoption, $
        outdir=outdiruse, email=email2, deletetempfiles=deletetempfiles, $
        instrument=instrument, query_source='IRIS_getAIAdata', tprep1=info.tprep1, $
        debug=info.debug, _extra=_extra
    end

    info.ui_instrume: begin
      info.instrumentindex = event.index
    end

    info.ui_method: begin
      info.methodoption = event.index
    end

    info.ui_methodinfo: begin
      print,'info'
      a = widget_base(group_leader=event.top)
      b = widget_text(a, value=[$
        'SSW_CUTOUT: Calls ssw_cutout_service', $
        '            The data is prepared on the server and when it is ready', $
        '            it will be downloaded, the files to download are very small,', $
        '            but the preparation can take a very long time (up to several hours),', $
        'FULL FRAME: The full frame AIA data is first downloaded and then prepared locally', $
        '            the files to download are very large, but download speed is much higher', $
        '            compared to ssw_cutout, and there is no waiting time for the data', $
        '            this method is recommended if you have good internet connection and lots of space', $
        '            on your harddisk', $
        'ARCHIVE   : No download of any data, it is assumed that you have access to', $
        '            /archive/sdo/AIA/lev1/', $
        '            preparation is done directly on the archived file', $
        '            max # frames is ignored, all available file are used', $
        '            recommended if you run this program at Lockheed'], $
        xsize=100,ysize=13)
      widget_control,a,/realize
    end

    else:
  endcase
  if ~destroyed then widget_control, event.top, set_UValue=info, /No_Copy
end



pro IRIS_getAIAdata, obs, usehcr=usehcr, $
  nowidget=nowidget, outdir=outdir, adddatetree=adddatetree, addobsid=addobsid, $
  waves=waves, fovexpand=fovexpand, timeexpand=timeexpand, method=method, $
  deletetempfiles=deletetempfiles, maxframes=maxframes, tres=tres, _extra=_extra, $
  debug=debug

  fovexpanddefault = 100.0 ;arcseconds ;;;;;;;;;;;;;;;;;DEFAULT
  timeexpanddefault = 10.0 ;minutes ;;;;;;;;;;;;;;;;;DEFAULT
  wavesdefault = ['94','131','171','193','211','304','335','1600','1700','4500'] ;;;;;;;;;;;;DEFAULT
  maxframesdefault = 50    ;;;;;;;;;;;;DEFAULT

  constants = obj_new('IRISsim_constants')

  t1=systime(1)

  obs2fov = obj_new('IRIS_obs2fov', obs, usehcr=usehcr)
  if obs2fov->get_error() then begin
    print, 'Invalid OBS'
    return
  endif

  t2=systime(1)
  tprep1=t2-t1



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
    if ~keyword_set(waves) then begin
      print, 'no wavelengths defined, downloading all'
      wavesuse = wavesdefault
      print, wavesuse
    endif else wavesuse = waves
    if N_ELEMENTS(fovexpand) eq 0 then obs2fov->set_fovexpand, fovexpanddefault $
    else obs2fov->set_fovexpand, fovexpand
    if N_ELEMENTS(timeexpand) eq 0 then obs2fov->set_timeexpand, timeexpanddefault $
    else obs2fov->set_timeexpand, timeexpand
    if N_ELEMENTS(method) eq 1 then methoduse = method $
    else methoduse = 2
    
    IRIS_processAIArequest, obs2fov, waves=wavesuse, $
      tres=tres, maxframes=maxframes, method=methoduse, $
      outdir=outdiruse, email=email2, deletetempfiles=deletetempfiles, $
      instrument='AIA', query_source='IRIS_getAIAdata', $
      _extra=_extra, tprep1=tprep1, $
      debug=debug





  endif else begin
    ;use widget interface

    if keyword_set(outdir) then inoutdir = outdir
    paramfile=IRISgetAIA_appReadme()+'/iris_getaiadata_params.sav'
    if file_test(paramfile) then begin
      restore,paramfile
      ;paramfile contains the variables:
    endif
    if N_ELEMENTS(email) eq 0 then email=''
    if N_ELEMENTS(emailnot) eq 0 then emailnot=[0]
    if N_ELEMENTS(outdir) eq 0 then cd,current=outdir
    if N_ELEMENTS(inoutdir) eq 1 then outdir=inoutdir
    if strmid(outdir, 0,1, /reverse_offset) ne path_sep() then outdir = outdir+path_sep()
    if N_ELEMENTS(outoptions) ne 3 then outoptions=[0,1,0]
    if N_ELEMENTS(addobsid) eq 1 then outoptions[1] = addobsid
    if N_ELEMENTS(deletetempfiles) eq 1 then outoptions[2] = deletetempfiles
    if N_ELEMENTS(instrumentindex) eq 0 then instrumentindex=0
    if N_ELEMENTS(wavesoption) eq 0 then wavesoption=intarr(12)
    if keyword_set(waves) then begin
      ;translate here... TO DO
    endif
    if total(wavesoption) eq 12 then allwavesoption=1 else allwavesoption=0
    if N_ELEMENTS(startexpand) eq 0 then startexpand=timeexpanddefault
    if N_ELEMENTS(stopexpand) eq 0 then stopexpand=timeexpanddefault
    if keyword_set(timeexpand) then begin
      startexpand = timeexpand[0]
      stopexpand = timeexpand[N_ELEMENTS(timeexpand)-1]
    endif
    if N_ELEMENTS(fovxexpand) eq 0 then fovxexpand=fovexpanddefault
    if N_ELEMENTS(fovyexpand) eq 0 then fovyexpand=fovexpanddefault
    if keyword_set(fovexpand) then begin
      fovxexpand = fovexpand[0]
      fovyexpand = fovexpand[N_ELEMENTS(fovxexpand)-1]
    endif
    if N_ELEMENTS(methodoption) eq 0 then methodoption=0
    if N_ELEMENTS(method) eq 1 then methodoption=method
    if N_ELEMENTS(_extra) eq 0 then ex=0 $
    else ex = _extra
    if ~keyword_set(maxframes) then begin
      if keyword_set(tres) then begin
        if tres lt 12 then tres=12
        maxframes = ceil(obs2fov->get_tobs()/tres)
      endif else begin
        maxframes = maxframesdefault
        tres = fix(obs2fov->get_tobs()/maxframes)
        if tres lt 12 then tres=12
      endelse
    endif else begin
      tres = fix(obs2fov->get_tobs()/maxframes)
      if tres lt 12 then tres=12      
    endelse
    
    ;rootl3=getenv('IRIS_DATA')+'/level3' ;is usually not implemented
    rootl3=constants->get_data_path_uio_l3()
    yyyy=strmid(obs2fov->get_obsid(),0,4)
    mm=strmid(obs2fov->get_obsid(),4,2)
    dd=strmid(obs2fov->get_obsid(),6,2)
    dirl3=rootl3+'/'+yyyy+'/'+mm+'/'+dd+'/'+obs2fov->get_obsid()+'/'
    if outoptions[0] then outdiruse=dirl3 $
    else if outoptions[1] then outdiruse=outdir+obs2fov->get_obsid()+'/' $
    else outdiruse=outdir

    ; MainWindow ; Base-Widget
    MainWindow = WIDGET_BASE(/Column, title='IRIS - Get AIA coutout', XOffset=100, YOffset=100)

    leftlabel=100
    textsize=80

    Base1 = WIDGET_BASE(MainWindow, /Row)
    label = WIDGET_LABEL(Base1, value='OBS ID', /align_left, xsize=leftlabel)
    ui_obsid = WIDGET_TEXT(Base1, value=obs2fov->get_obsid(), xsize=textsize)

    Base2 = WIDGET_BASE(MainWindow, /Row)
    label = WIDGET_LABEL(Base2, value='OBS Directory', /align_left, xsize=leftlabel)
    ui_obsdir = WIDGET_TEXT(Base2, value=obs2fov->get_obsdir(), xsize=textsize)

    Base2a = WIDGET_BASE(MainWindow, /Row)
    label = WIDGET_LABEL(Base2a, value='Email (optional)', /align_left, xsize=leftlabel)
    ui_email = WIDGET_TEXT(Base2a, value=email, /editable, xsize=textsize)

    Base2b = WIDGET_BASE(MainWindow, /Row)
    label = WIDGET_LABEL(Base2b, value=' ', /align_left, xsize=leftlabel)
    b_email = CW_BGROUP(Base2b, ['Send email notification'], $
      /nonexclusive, set_value=emailnot)

    Base3 = WIDGET_BASE(MainWindow, /Row)
    label = WIDGET_LABEL(Base3, value='Output directory', /align_left, xsize=leftlabel)
    ui_outdir = WIDGET_TEXT(Base3, value=outdiruse, /editable, xsize=textsize)
    b_outdir = WIDGET_BUTTON(Base3, value='Change')

    Base3a = WIDGET_BASE(MainWindow, /Row)
    label = WIDGET_LABEL(Base3a, value=' ', /align_left, xsize=leftlabel)
    b_addobsid = CW_BGROUP(Base3a, ['Save in IRIS_DATA/level3 folder', $
      'Add OBS ID to output directory',$
      'Delete AIA level 1.5 fits files after use'], $
      /nonexclusive, set_value=outoptions)

    Base4 = WIDGET_BASE(MainWindow, /Row)
    label = WIDGET_LABEL(Base4, value='Instrument', /align_left, xsize=leftlabel)
    ui_instrume = WIDGET_COMBOBOX(Base4, value=['AIA']);, 'EIT', 'MDI'])
    widget_control, ui_instrume, SET_COMBOBOX_SELECT=instrumentindex

    Base4a = WIDGET_BASE(MainWindow, /Row)
    b_wavesall = CW_BGROUP(Base4a,['all'],/nonexclusive, label_left='Wavelengths    ', set_value=allwavesoption)
    waves_aia = ['94','131','171','193','211','304','335','1600','1700','4500','blos','cont']
    ;  waves_eit = ['171','195','284','304','','','','','','','','']
    ;  waves_mdi = ['mag','cont','','','','','','','','','','']
    b_waves = CW_BGROUP(Base4a,waves_aia,column=6,/nonexclusive, set_value=wavesoption)

    Base5 = WIDGET_BASE(MainWindow, /Row)
    label = WIDGET_LABEL(Base5, value=' ', /align_left, xsize=leftlabel)

    Base6 = WIDGET_BASE(MainWindow, /Row)
    label = WIDGET_LABEL(Base6, value='Cutout center', /align_left, xsize=leftlabel)
    ui_xcen = CW_FIELD(Base6, title='X', value=string(obs2fov->get_xcen(), format='(f8.1)'), xsize=8)
    label = WIDGET_LABEL(Base6, value=' ', /align_left, xsize=86)
    ui_ycen = CW_FIELD(Base6, title='Y', value=string(obs2fov->get_ycen(), format='(f8.1)'), xsize=8)
    label = WIDGET_LABEL(Base6, value=' arcsec', /align_left)

    Base7 = WIDGET_BASE(MainWindow, /Row)
    label = WIDGET_LABEL(Base7, value='Cutout FOV', /align_left, xsize=leftlabel)
    ui_xfov = CW_FIELD(Base7, title='X', value=string(obs2fov->get_fovx(), format='(f8.1)'), xsize=8)
    ui_expfovx = CW_FIELD(Base7, title='+', value=fovxexpand, /float, xsize=4)
    label = WIDGET_LABEL(Base7, value=' ', /align_left, xsize=20)
    ui_yfov = CW_FIELD(Base7, title='Y', value=string(obs2fov->get_fovy(), format='(f8.1)'), xsize=8)
    ui_expfovy = CW_FIELD(Base7, title='+', value=fovyexpand, /float, xsize=4)
    label = WIDGET_LABEL(Base7, value=' arcsec', /align_left)

    Base8 = WIDGET_BASE(MainWindow, /Row)
    label = WIDGET_LABEL(Base8, value='Start time', /align_left, xsize=leftlabel)
    ui_startobs = WIDGET_TEXT(Base8, value=obs2fov->get_startobs(), xsize=22)
    ui_expstart = CW_FIELD(Base8, title='-', value=startexpand, /float, xsize=4)
    label = WIDGET_LABEL(Base8, value='minutes', /align_left)

    Base9 = WIDGET_BASE(MainWindow, /Row)
    label = WIDGET_LABEL(Base9, value='Stop time', /align_left, xsize=leftlabel)
    ui_endobs = WIDGET_TEXT(Base9, value=obs2fov->get_endobs(), xsize=22)
    ui_expstop = CW_FIELD(Base9, title='+', value=stopexpand, /float, xsize=4)
    label = WIDGET_LABEL(Base9, value='minutes', /align_left)

    Base10 = WIDGET_BASE(MainWindow, /Row)
    label = WIDGET_LABEL(Base10, value='Max # frames', /align_left, xsize=leftlabel)
    ui_maxframes = WIDGET_TEXT(Base10, value='50', /editable, /all_events, xsize=5)
    label = WIDGET_LABEL(Base10, value='=>', /align_center, xsize=30)
    ui_tres = CW_FIELD(Base10, title='Temp. resolution', value=tres, /integer, /all_events, xsize=8)
    label = WIDGET_LABEL(Base10, value='seconds', /align_left)

    Base10a = WIDGET_BASE(MainWindow, /Row)
    label = WIDGET_LABEL(Base10a, value='Method', /align_left, xsize=leftlabel)
    ui_method = WIDGET_COMBOBOX(Base10a, value=['SSW_cutout', 'Full Frame', 'Archive'])
    label = WIDGET_LABEL(Base10a, value=' ', /align_left, xsize=30)
    ui_methodinfo = WIDGET_BUTTON(Base10a, value='INFO')
    widget_control, ui_method, SET_COMBOBOX_SELECT=methodoption

    Base11 = WIDGET_BASE(MainWindow, /Row)
    label = WIDGET_LABEL(Base11, value=' ', /align_left, xsize=20)
    b_quit = WIDGET_BUTTON(Base11, value='QUIT')
    label = WIDGET_LABEL(Base11, value=' ', /align_left, xsize=80)
    b_request = WIDGET_BUTTON(Base11, value='Request Data', xsize=220)



    widget_control, MainWindow, /realize



    ;define structure with information to be passed around
    info = { $
      debug:keyword_set(debug), $
      obs:obs, $
      obs2fov:obs2fov, $
      ui_obsid:ui_obsid, $
      ui_obsdir:ui_obsdir, $
      ui_email:ui_email, $
      b_email:b_email, $
      ui_outdir:ui_outdir, $
      outdir:outdir, $
      dirl3:dirl3, $
      b_outdir:b_outdir, $
      b_addobsid:b_addobsid, $
      ui_instrume:ui_instrume, $
      instrumentindex:instrumentindex, $
      b_wavesall:b_wavesall, $
      b_waves:b_waves, $
      waves_aia:waves_aia, $
      ;    waves_eit:waves_eit, $
      ;    waves_mdi:waves_mdi, $
      ui_xcen:ui_xcen, $
      ui_ycen:ui_ycen, $
      ui_xfov:ui_xfov, $
      ui_yfov:ui_yfov, $
      ui_expfovx:ui_expfovx, $
      ui_expfovy:ui_expfovy, $
      ui_startobs:ui_startobs, $
      ui_endobs:ui_endobs, $
      ui_expstart:ui_expstart, $
      ui_expstop:ui_expstop, $
      ui_maxframes:ui_maxframes, $
      ui_tres:ui_tres, $
      b_quit:b_quit, $
      b_request:b_request, $
      ui_method:ui_method, $
      ui_methodinfo:ui_methodinfo, $
      methodoption:methodoption, $
      tprep1:tprep1, $
      ex:ex $
    }

    ;set this structure as the user-defined value of mainwindow
    widget_control, MainWindow, set_UValue=info, /No_Copy

    xmanager, 'IRIS_getAIAdata', MainWindow, /no_block

  endelse ;use widget interface
end
