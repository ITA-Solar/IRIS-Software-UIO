;+
; NAME:
;       IRIS_getXRTdata
;
; PURPOSE:
;       IRIS_getXRTdata downloads (if necessary) Hinode XRT data that correspond to a specific IRIS OBS
;       and transforms it to IRIS-SJI-like level 2 fits file
;
; CATEGORY:
;       Martin Wiesmann / IRIS Data processing
;
; CALLING SEQUENCE:
;       IRIS_getXRTdata, obs, $
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
;       IRIS-SJI-like level 2 fits file of Hinode XRT data
;
; CALLS:
;       IRIS_getXRTdata, '20140304_092952_3860110476', outdir='/irisa/data/level2/2014/03/04/20140304_092952_3860110476/xrt/'
;       IRIS_getXRTdata, '~/data/iris/20140304_092952_3860110476/', outdir='~/data/iris/xrt/', /addobsid, /adddatetree
;       
; COMMON BLOCKS:
;
; RESTRICTIONS:
;       Needs an internet connection to download data
;
; MODIFICATION HISTORY:
;       15-Feb-2019: Martin Wiesmann (ITA, UIO)
;
; $Id: iris_getxrtdata.pro,v 1.7 2019/06/04 12:45:31 mawiesma Exp $  ;



pro IRIS_getXRTdata, obs, $
  outdir=outdir, adddatetree=adddatetree, addobsid=addobsid, $
  fovexpand=fovexpand, timeexpand=timeexpand, deletetempfiles=deletetempfiles, _extra=_extra, $
  fulltime=fulltime, fullfov=fullfov, $
  timelowlimit=timelowlimit, arealowlimit=arealowlimit, $
  debug=debug

  t01=systime(1)

  ;obs = '/Users/mawiesma/data/iris/level2/20140304_092952_3860110476/'
  ;obs = '20140304_092952_3860110476'
  ;outdir = '/Users/mawiesma/data/iris/level2/20140304_092952_3860110476/xrt/'
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

  ;search XRT HCR for entries within time window of IRIS OBS
  time_window,[obs2fov->get_startobs(),obs2fov->get_endobs()],minute=expandtimewindowforsearch,t0,t1
  help,t0,t1
  sothcr=ssw_hcr_query(ssw_hcr_make_query(t0,t1,instrument='XRT'),/remove_dummy, count=count)
  if count eq 0 then begin
    box_message,'No SOT HCR entry within the time window of this IRIS OBS'
    sothcr=!NULL
  endif else begin ;count eq 0 (number hcr entries)

    ;get spatial overlap between IRIS and HINODE
    obs2fov->calc_overlap, sothcr

    ;create list of umodes of each SOT entry
    umodestemp = {sotid:'', datatype:'', ec_fw1:'', ec_fw2:'', ec_imty:'', chip_sum:0, naxis1:0, naxis2:0, naxis3:0, naxis:0, files_tot:0L, files_win:0L, $
      timesot_overlap:0.0, timeiris_overlap:0.0, sotstart:'', sotend:'', eventid:'', eventlink:'', $
      rasterOverlap:0, rasterOverlapPercent:0.0, SjiOverlapPercent:0.0}
    umodesall = make_array(N_ELEMENTS(sothcr), 30, value=umodestemp)
    filesall = ptrarr(N_ELEMENTS(sothcr),30)
    fitsfiledates = ptrarr(N_ELEMENTS(sothcr),30)
    modespersot = intarr(N_ELEMENTS(sothcr))
    taglist='naxis1,naxis2,datatype,ec_fw1_,ec_fw2_,ec_imty_,chip_sum'

    ;time window to calculate number of files
    startdatem=anytim2cal(obs2fov->get_startobs(),form=8)
    stopdatem=anytim2cal(obs2fov->get_endobs(),form=8)
    startobs = obs2fov->get_startobs()
    tstart = str2utc(startobs)
    for isot=0,N_ELEMENTS(sothcr)-1 do begin
      files=''
      ;xrt_cat,sothcr[isot].starttime,sothcr[isot].stoptime,cat,files ;check if files are locally available
      ;commented out because it returns level 0 files, those are locally available at LMSAL
      ;but we want level 1 files, which have to be downloaded from SAO
      ;if N_ELEMENTS(files) eq 1 && files[0] eq '' then begin
        xrt_cat,sothcr[isot].starttime,sothcr[isot].stoptime,cat,files,/urls ;get file urls if files are not locally available
        fileurl=1
      ;endif
      sotid = anytim2cal(strmid(sothcr[isot].eventid, strpos(sothcr[isot].eventid, 'Obs')+4, 19), form=8)

      if N_ELEMENTS(files) ge 1 && files[0] ne '' then begin
        umodes=xrt_umodes(cat, taglist, mcount=mcount) ; uniq "modes" in this CAT
        modespersot[isot] = N_ELEMENTS(umodes)
        files = iris_xrtFileNameConv(files)

        ;create list of files and structure of each umode
        for imode=0, modespersot[isot]-1 do begin
          temp = strtrim(strsplit(umodes[imode], '  ', /regex, /extract),2)
          umodesall[isot,imode].sotid = sotid
          umodesall[isot,imode].naxis1 = temp[0]
          umodesall[isot,imode].naxis2 = temp[1]
          ;umodesall[isot,imode].naxis3 = temp[4]
          ;umodesall[isot,imode].naxis = temp[5]
          umodesall[isot,imode].datatype = temp[2]
          umodesall[isot,imode].ec_fw1 = temp[3]
          umodesall[isot,imode].ec_fw2 = temp[4]
          umodesall[isot,imode].ec_imty = temp[5]
          umodesall[isot,imode].chip_sum = temp[6]
          umodesall[isot,imode].files_tot = mcount[imode]
          ss=xrt_umodes(cat,taglist,where_mode=umodes[imode]) ; subscripts of
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
        endfor ;imode=0, modespersot[isot]-1
      endif ;N_ELEMENTS(files) ge 1 && files[0] ne ''
      overlap = 100.0 * total(umodesall[isot,*].files_win) / total(umodesall[isot,*].files_tot)
      newsottemp = create_struct(sothcr[isot], 'timesot_overlap', overlap, 'timeiris_overlap', $
        mean(umodesall[isot,0:modespersot[isot]-1].timeiris_overlap))
      if isot eq 0 then newsot = newsottemp $
      else newsot = [newsot, newsottemp]
    endfor ;isot=0,N_ELEMENTS(sothcr)-1
    sothcr = newsot
  endelse ;check if there is a sot entry

  t2=systime(1)
  tprep1=t2-t01


  print,'number of sothcr: ' + strcompress(string(N_ELEMENTS(sothcr)),/remove_all)

  ; calculate which umodes to use (and files)
  ; might be multiple SOT entries
  umodes=!NULL
  for isot=0,N_ELEMENTS(sothcr)-1 do begin
    print, 'SOTHCR number: ' + strcompress(string(isot),/remove_all) + ' ; number of umodes in this SOTHCR : ' + strcompress(string(modespersot[isot]),/remove_all)
    if sothcr[isot].areasji_overlap ge arealim then begin
      for imode=0, modespersot[isot]-1 do begin
        
        if umodesall[isot,imode].chip_sum eq 8 then print, 'found chip_sum = 8, not processing'
        if strcompress(strlowcase(umodesall[isot,imode].ec_fw2),/remove_all) eq 'gband' then print, 'found gband, not processing'
        
        if umodesall[isot,imode].timeiris_overlap ge timelim && $
          umodesall[isot,imode].chip_sum lt 8 && $
          strcompress(strlowcase(umodesall[isot,imode].ec_fw2),/remove_all) ne 'gband' then begin
            
          if N_ELEMENTS(umodes) eq 0 then begin
            umodes = umodesall[isot,imode]
            files = ptr_new(*(filesall[isot,imode]))
          endif else begin
            umodes = [umodes, umodesall[isot,imode]]
            files = [files, ptr_new(*(filesall[isot,imode]))]
          endelse
          
        endif
      endfor
      print, 'SOTHCR number: ' + strcompress(string(isot),/remove_all) + ' ; total number of umodes to process : ' + strcompress(string(N_ELEMENTS(umodes)),/remove_all)
    endif else print, 'no spatial overlap'
  endfor

  if N_ELEMENTS(umodes) eq 0 then box_message,'No XRT OBS overlaps with this IRIS OBS'
  IRIS_processXRTrequest, outdiruse, umodes, files,  fileurl, obs2fov, fulltime=fulltime, fullfov=fullfov, $
    deletetempfiles=deletetempfiles, debug=debug, _extra=_extra, tprep1=tprep1



end
