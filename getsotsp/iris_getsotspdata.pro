;+
; NAME:
;       IRIS_getSotSPdata
;
; PURPOSE:
;       IRIS_getSotSPdata downloads Hinode SP data that correspond to a specific IRIS OBS
;       and transforms it to IRIS-SJI-like level 2 fits file
;
; CATEGORY:
;       Martin Wiesmann / IRIS Data processing
;
; CALLING SEQUENCE:
;       IRIS_getSotSPdata, obs, $
;           outdir=outdir [, adddatetree=adddatetree, addobsid=addobsid, $
;           fovexpand=fovexpand, timeexpand=timeexpand, deletetempfiles=deletetempfiles, _extra=_extra, $
;           fulltime=fulltime, fullfov=fullfov, $
;           timelowlimit=timelowlimit, arealowlimit=arealowlimit, $
;           debug=debug]
;
; INPUTS:
;       obs: where obs is the OBS ID of the IRIS OBS. It can be given in 3 different styles:
;         1) Just the OBS ID (e.g. '20130829_005500_4203300028')
;           This option only works if you're either at LMSAL and have access to /irisa/data/level2/
;           or if you're at ITA/UIO and have access to /mn/stornext/d10/HDC2/iris/data/level2/
;         2) The directory in which the IRIS files reside (e.g. '/mn/xsan/d2/iris/data/level2/2014/01/01/20140101_000431_3840257196/')
;         3) The full path of one of the IRIS files, which you can print out from iris_xfiles (e.g. '/mn/xsan/d2/iris/data/level2/2014/01/01/20140101_000431_3840257196/iris_l2_20140101_000431_3840257196_SJI_1400_t000.fits')
;       outdir: the directory in which to save the resulting fits files, may be modified by the keywords adddatetree and
;         addobsid. If download is necessary, a subfolder 'original' will be created and the original files saved into that.
;
; OPTIONAL KEYWORD PARAMETERS:
;       adddatetree: adds a date-tree structure to outdir (i.e. outdir/2015/10/23/)
;       addobsid: adds the OBS ID to outdir (i.e. outdir/20151023_171023_4203300028/), if adddatetree is also set
;         the OBS ID will be added at the end (i.e. outdir/2015/10/23/20151023_171023_4203300028/)
;       fovexpand: defines by how much the FOV should be expanded, as compared to IRIS FOV, in arcsecond, can be scalar,
;         then the same expansion will be applied to both the x- and y-axis. It can also be a 2-element vector, one element for
;         x- and one for y-axis, respectively.
;         (default: 100.0 arcsec)
;       timeexpand: defines by how much the time window of the IRIS OBS is expanded for AIA data, in minutes. If scalar,
;         the value will be applied to the start and the end time. Can be also a 2-element vector, for different expansions
;         at the start and at the end, respectively. (Positive numbers result in a wider time-window)
;         (default: 10.0 minutes)
;       fulltime: If set, all HINODE files of the selected umodes will be used, regardless of the time window
;       fullfov: If set, the original FOV of the HINODE umode will be transfered, regardless of IRIS FOV
;       deletetempfiles: If set, the downloaded files will be deleted after use
;       timelowlimit: Defines the lower limit of the IRIS time window covered by HINODE in percent (default: 10.0 %)
;         if covarage is below that limit, the whole umode will not be processed
;       arealowlimit: Defines the lower limit of the IRIS raster FOV covered by HINODE in percent (default: 10.0 %)
;         if covarage is below that limit, the whole umode will not be processed
;       _extra: not used
;
; OUTPUTS:
;       IRIS-SJI-like level 2 fits file of Hinode SP data
;
; CALLS:
;       IRIS_getSotSPdata, '20140304_092952_3860110476', outdir='/irisa/data/level2/2014/03/04/20140304_092952_3860110476/sotsp/'
;       IRIS_getSotSPdata, '~/data/iris/20140304_092952_3860110476/', outdir='~/data/iris/sotsp/', /addobsid, /adddatetree
;
; COMMON BLOCKS:
;
; RESTRICTIONS:
;       Needs an internet connection to download data
;
; MODIFICATION HISTORY:
;       31-Oct-2019: Martin Wiesmann (ITA, UIO)
;
; $Id: iris_getsotspdata.pro,v 1.3 2020/01/15 14:44:20 mawiesma Exp $  ;



pro IRIS_getSotSPdata, obs, $
  outdir=outdir, adddatetree=adddatetree, addobsid=addobsid, $
  fovexpand=fovexpand, timeexpand=timeexpand, deletetempfiles=deletetempfiles, _extra=_extra, $
  fulltime=fulltime, fullfov=fullfov, $
  timelowlimit=timelowlimit, arealowlimit=arealowlimit, $
  debug=debug, view_images=view_images

  t01=systime(1)

  ;obs = '/Users/mawiesma/data/iris/level2/20140302_230341_3810263287/'
  ;outdir = '/Users/mawiesma/data/iris/level2/20140302_230341_3810263287/sotsp/'
  ;debug=1

  fovexpanddefault = 100.0 ;arcseconds ;;;;;;;;;;;;;;;;;DEFAULT
  timeexpanddefault = 10.0 ;minutes ;;;;;;;;;;;;;;;;;DEFAULT
  timelowlimitdefault = 10.0 ;percentage ;;;;;;;;;;;;;;;;;DEFAULT
  arealowlimitdefault = 10.0 ;percentage ;;;;;;;;;;;;;;;;;DEFAULT

  obs2fov = obj_new('IRIS_obs2fov', obs, usehcr=usehcr, /hinode)
  if obs2fov->get_error() then begin
    print, 'Invalid input'
    return
  endif

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
  if keyword_set(addobsid) then outdiruse = outdiruse + obs2fov->get_obsid() + path_sep()

  if N_ELEMENTS(timelowlimit) eq 0 then timelim = timelowlimitdefault $
  else timelim = timelowlimit
  if N_ELEMENTS(arealowlimit) eq 0 then arealim = arealowlimitdefault $
  else arealim = arealowlimit


  if N_ELEMENTS(fovexpand) eq 0 then obs2fov->set_fovexpand, fovexpanddefault $
  else obs2fov->set_fovexpand, fovexpand
  if N_ELEMENTS(timeexpand) eq 0 then obs2fov->set_timeexpand, timeexpanddefault $
  else obs2fov->set_timeexpand, timeexpand


  fileurl=0
  ;expandtimewindowforsearch=[-1000,2000]
  expandtimewindowforsearch=[0,0]

  ;search SOT SP HCR for entries within time window of IRIS OBS
  time_window,[obs2fov->get_startobs(),obs2fov->get_endobs()],minute=expandtimewindowforsearch,t0,t1
  print, 'IRIS OBS start: ', t0
  print, 'IRIS OBS stops: ', t1

  starttime = anytim2utc(t0, /ccsds, /truncate)
  ind = strpos(starttime, ':', /reverse_search)
  starttime = strmid(starttime, 0, ind)

  stoptime = anytim2utc(t1, /ccsds, /truncate)
  ind = strpos(stoptime, ':', /reverse_search)
  stoptime = strmid(stoptime, 0, ind)

  url = 'http://www.lmsal.com/hek/hcr?cmd=search-events-corr&outputformat=json&startTime=' + $
    starttime + $
    '&stopTime=' + $
    stoptime + $
    '&instrument=SOTSP'

  ;print,url

  sock_list, url, json
  ;help,json
  results=json_parse(json, /tostruct)

  ;help,results
  ;help,results.events
  ;print,N_ELEMENTS(results.events)

  if N_ELEMENTS(results.events) eq 0 then begin
    box_message,'No SOT SP OBS overlaps with this IRIS OBS'
  endif else begin

    ;create list of umodes of each SOT entry
    umodestemp = {sotid:'', philsp_compurl:'', $
      timesot_overlap:0.0, timeiris_overlap:0.0, sotstart:'', sotend:'', eventid:'', eventlink:'', $
      rasterOverlap:0, rasterOverlapPercent:0.0, SjiOverlapPercent:0.0, SotOverlapPercent:0.0}
    sot_sp_events = make_array(N_ELEMENTS(results.events), value=umodestemp)

    ;time window to calculate number of files
    startdatem=anytim2cal(obs2fov->get_startobs(),form=8)
    stopdatem=anytim2cal(obs2fov->get_endobs(),form=8)
    startobs = obs2fov->get_startobs()
    tstart = str2utc(startobs)
    tend = str2utc(obs2fov->get_endobs())
    iristime = (tend.mjd-tstart.mjd)*86400L + (tend.time-tstart.time)/1000d

    for ievent=0,N_ELEMENTS(results.events)-1 do begin
      if tag_exist(results.events[ievent], 'philsp_compurl') then begin
        event = results.events[ievent]

        print, '==========================='
        ;get spatial overlap between IRIS and HINODE
        print, 'SOT SP OBS start: ', event.starttime
        print, 'SOT SP OBS stops: ', event.stoptime

        obs2fov->calc_overlap, event
        print, 'SJI area overlap [%]   : ', event.areasji_overlap
        print, 'raster area overlap [%]: ', event.arearas_overlap
        print, 'SOT area overlap [%]: ', event.areasot_overlap

        sotid = anytim2cal(strmid(event.eventid, strpos(event.eventid, 'Obs')+5, 19), form=8)
        sot_sp_events[ievent].sotid = sotid
        sot_sp_events[ievent].philsp_compurl = event.philsp_compurl
        sot_sp_events[ievent].sotstart = event.starttime
        sot_sp_events[ievent].sotend = event.stoptime
        sot_sp_events[ievent].eventid = event.eventid
        sot_sp_events[ievent].eventlink = 'http://www.lmsal.com/hek/hcr?cmd=view-event&event-id=' + $
          url_encode(event.eventid)
        sot_sp_events[ievent].rasterOverlap = event.arearas_overlap ge arealim
        sot_sp_events[ievent].rasterOverlapPercent = event.arearas_overlap
        sot_sp_events[ievent].SjiOverlapPercent = event.areasji_overlap
        sot_sp_events[ievent].SotOverlapPercent = event.areasot_overlap

        sotstart = str2utc(event.starttime)
        sotend = str2utc(event.stoptime)
        sotIrisStart = (sotstart.mjd-tstart.mjd)*86400L + (sotstart.time-tstart.time)/1000d
        sotIrisEnd = (sotend.mjd-tend.mjd)*86400L + (sotend.time-tend.time)/1000d
        sottime = (sotend.mjd-sotstart.mjd)*86400L + (sotend.time-sotstart.time)/1000d

        if sotIrisStart lt 0 then begin
          ; SOT OBS started before IRIS OBS started
          if sotIrisEnd lt 0 then begin
            ;SOT OBS ended before IRIS ended
            overlap_time = (sotend.mjd-tstart.mjd)*86400L + (sotend.time-tstart.time)/1000d
          endif else begin
            ;SOT OBS ended after IRIS ended
            overlap_time = iristime
          endelse
        endif else begin
          ; SOT OBS started after IRIS started
          if sotIrisEnd lt 0 then begin
            ;SOT OBS ended before IRIS ended
            overlap_time = sottime
          endif else begin
            ;SOT OBS ended after IRIS ended
            overlap_time = (tend.mjd-sotstart.mjd)*86400L + (tend.time-sotstart.time)/1000d
          endelse
        endelse
        if ((sotstart.mjd-tend.mjd)*86400L + (sotstart.time-tend.time)/1000d) ge 0 or $
          ((sotend.mjd-tstart.mjd)*86400L + (sotend.time-tstart.time)/1000d) le 0 then begin
          ; SOT OBS started after IRIS OBS ended or
          ; SOT OBS ended before IRIS OBS started
          overlap_time = 0
        endif
        if overlap_time lt 0 then begin
          print, 'BUG, overlap_time lt 0 ; ', overlap_time
        endif

        sot_sp_events[ievent].timesot_overlap = 100.0 * overlap_time / sottime
        sot_sp_events[ievent].timeiris_overlap = 100.0 * overlap_time / iristime

        print, 'SOT SP OBS time overlap [%]: ',sot_sp_events[ievent].timesot_overlap
        print, 'IRIS OBS time overlap [%]  : ',sot_sp_events[ievent].timeiris_overlap
      endif ;tag_exist(results.events[ievent], 'philsp_compurl')
    endfor ;ievent=0,N_ELEMENTS(results.events)-1

  endelse ;check if there is a sot sp overlap

  print,'number of sot SP events found: ' + strcompress(string(N_ELEMENTS(sot_sp_events)),/remove_all)

  if N_ELEMENTS(sot_sp_events) gt 0 then begin
    ind = where((sot_sp_events.SjiOverlapPercent ge arealim OR sot_sp_events.SotOverlapPercent ge arealim) AND sot_sp_events.timeiris_overlap ge timelim, count)
    if count gt 0 then sot_sp_events = sot_sp_events[ind] $
    else sot_sp_events = []
  endif

  print,'number of sot SP events to be processed: ' + strcompress(string(N_ELEMENTS(sot_sp_events)),/remove_all)

  t2=systime(1)
  tprep1=t2-t01

  IRIS_processSotSPrequest, outdiruse, sot_sp_events, obs2fov, fulltime=fulltime, fullfov=fullfov, $
    deletetempfiles=deletetempfiles, debug=debug, _extra=_extra, tprep1=tprep1, view_images=view_images

end
