; $Id: iris_obs2fov__define.pro,v 1.19 2020/01/15 14:44:20 mawiesma Exp $  ;


FUNCTION IRIS_obs2fov::init, obs, usehcr=usehcr, hinode=hinode
  if N_ELEMENTS(obs) eq 1 then begin
    if size(obs, /type) eq 7 then begin
      self.obs = obs
      self.usehcr = keyword_set(usehcr)
      self.hinode = keyword_set(hinode)
      self->docalculations
    endif else begin
      print, 'input must be a scalar string'
      self.error = 1
    endelse
  endif else begin
    print, 'input must be a scalar string'
    self.error = 1
  endelse
  return, 1
END


PRO IRIS_obs2fov::cleanup
  if ptr_valid(self.files_sji) then ptr_free, self.files_sji
  if ptr_valid(self.files_ras) then ptr_free, self.files_ras
END


PRO IRIS_obs2fov::setobs, obs, usehcr=usehcr, hinode=hinode
  self.obs = ''
  self.usehcr = 0
  self.hinode = 0
  self.error = 0
  self.obsid = ''
  self.obsdir = ''
  self.startobs = ''
  self.endobs = ''
  self.tobs = 0.0d
  self.xcen = 0.0
  self.ycen = 0.0
  self.satrot = 0.0
  self.satrotradeg = 0.0
  self.roll_angle = 0.0
  self.roll_angle_radeg = 0.0
  self.fovx_nomove = 0.0
  self.fovy_nomove = 0.0
  self.fovx = 0.0
  self.fovy = 0.0
  self.fovx_satrot = 0.0
  self.fovy_satrot = 0.0
  self.files_sji_nr = 0
  self.files_ras_nr = 0
  if ptr_valid(self.files_sji) then ptr_free, self.files_sji
  if ptr_valid(self.files_ras) then ptr_free, self.files_ras

  if N_ELEMENTS(obs) eq 1 then begin
    if size(obs, /type) eq 7 then begin
      self.obs = obs
      self.usehcr = keyword_set(usehcr)
      self.hinode = keyword_set(hinode)
      self->docalculations
    endif else begin
      print, 'input must be a scalar string'
      self.error = 1
    endelse
  endif else begin
    print, 'input must be a scalar string'
    self.error = 1
  endelse
END


PRO IRIS_obs2fov::docalculations
  obs = self.obs
  obsid=''
  obsdir=''
  if ~self.usehcr then begin
    if file_test(obs) then begin
      if file_test(obs, /directory) then begin
        obsdir = obs
        obsid = file_basename(obs)
      endif else begin
        obsdir = file_dirname(obs, /mark_directory)
        obsid = file_basename(file_dirname(obs))
      endelse
    endif else begin
      obsid=obs
      dat = extract_fids(obs, fidfound=fidfound)
      if fidfound then begin
        pathdate = strmid(dat,0,4)+'/'+strmid(dat,4,2)+'/'+strmid(dat,6,2)+'/'
        print,'testing LMSAL: /irisa/data/level2/'+pathdate+obs
        if file_test('/irisa/data/level2/'+pathdate+obs) then begin
          obsdir = '/irisa/data/level2/'+pathdate+obs
        endif else begin
          print,'testing UIO: /mn/stornext/d10/HDC2/iris/data/level2/'+pathdate+obs
          if file_test('/mn/stornext/d10/HDC2/iris/data/level2/'+pathdate+obs) then begin
            obsdir = '/mn/stornext/d10/HDC2/iris/data/level2/'+pathdate+obs
          endif else begin
            ;self.usehcr=1
            print, 'Cannot find OBS directory'
            self.error=1
            return
          endelse
        endelse
      endif else begin
        print, 'Cannot find OBS directory'
        self.error=1
        return
      endelse
    endelse
    if strmid(obsdir, 0,1, /reverse_offset) ne path_sep() then obsdir = obsdir+path_sep()
  endif

  if self.usehcr then begin
    self.obsid=obs
    hcr=iris_obs2hcr(self.obsid,count=count) ; obsid->HCR
    if count ne 1 then begin
      box_message,'Need one, and only one fully qualified IRIS obsid; no action taken.'
      self.error=1
      return ; EARLY EXIT on illegal input
    endif
    self.xcen=hcr.xcen
    self.ycen=hcr.ycen
    self.fovx=hcr.xfov
    self.fovy=hcr.yfov
    self.fovx_move=hcr.xfov
    self.fovy_move=hcr.yfov
    self.startobs=hcr.starttime
    self.endobs=hcr.stoptime
    tstart = str2utc(self.startobs)
    tend = str2utc(self.endobs)
    self.tobs = ((tend.mjd-tstart.mjd)*86400000L + tend.time-tstart.time) / 1000d
    ;we have to incorporate the satellite rotation
    ;but satrot doesn't exist in hcr
    ;self.satrot = hcr.sat_rot
    ;self.fovx_satrot = self.fovx*abs(cos(self.satrot)) + self.fovy*abs(sin(self.satrot))
    ;self.fovy_satrot = self.fovy*abs(cos(self.satrot)) + self.fovx*abs(sin(self.satrot))
  endif else begin
    files = file_search(obsdir+'*.fits')
    if N_ELEMENTS(files) eq 1 && files[0] eq '' then begin
      print,'no files found in OBS directory'
      print,obsdir
      self.error=1
      return
    endif else begin
      mreadfits_header, files[0], hdr, only_tags='STARTOBS,ENDOBS'
      startobs=hdr.startobs
      endobs=hdr.endobs
      tstart = str2utc(startobs)
      tend = str2utc(endobs)
      tobs = ((tend.mjd-tstart.mjd)*86400000L + tend.time-tstart.time) / 1000d
      areacovered = 0
      fovxmax = 0d
      fovymax = 0d
      tmax=0d
      tmin=tobs
      xcenmin=5000
      xcenmax=-5000
      ycenmin=xcenmin
      ycenmax=xcenmax
      nexpsji=0
      nexpras=0
      sjifiles = where(strmatch(files,'*_SJI_*', /fold_case) eq 1, nsji, complement=rasfiles, ncomplement=nras)
      if nsji gt 0 then begin
        ;let's get all the information out of the SJI if there are enough
        mreadfits_header, files[sjifiles], hdrsji, only_tags='FOVX,FOVY,SAT_ROT'+$
          ',NAXIS,NAXIS1,NAXIS2,CRPIX1,CRPIX2,CRVAL1,CRVAL2,CDELT1,CDELT2'+$
          ',CTYPE1,CTYPE2,CUNIT1,CUNIT2'+$
          ',PC1_1,PC1_2,PC2_1,PC2_2,PC3_1,PC3_2'
        fovxmax = max(hdrsji.fovx)
        fovymax = max(hdrsji.fovy)
        satrot = hdrsji.sat_rot
        for sji=0,nsji-1 do begin
          aux = readfits(files[sjifiles[sji]], hdrs, exten_no=1, /silent)
          hdrs = fitshead2struct(hdrs)
          goodexp = where(aux[hdrs.dsrcsix,*] gt 0, count)
          if count gt 0 then begin
            wcs = fitshead2wcs(hdrsji[sji])
            if required_tags(wcs, /roll_angle) then begin
              if N_ELEMENTS(rollangle) eq 0 then rollangle = wcs.roll_angle $
              else rollangle = [rollangle, wcs.roll_angle]
            endif
            if N_ELEMENTS(times) eq 0 then begin
              times = reform(aux[hdrs.time,goodexp])
              xcens = reform(aux[hdrs.xcenix,goodexp])
              ycens = reform(aux[hdrs.ycenix,goodexp])
              slitpixel = reform(aux[hdrs.SLTPX1IX,goodexp]) * hdrsji[sji].cdelt1
            endif else begin
              times = [times, reform(aux[hdrs.time,goodexp])]
              xcens = [xcens, reform(aux[hdrs.xcenix,goodexp])]
              ycens = [ycens, reform(aux[hdrs.ycenix,goodexp])]
              slitpixel = [slitpixel, reform(aux[hdrs.SLTPX1IX,goodexp]) * hdrsji[sji].cdelt1]
            endelse
            nexpsji=nexpsji+count
            tmins = min(aux[hdrs.time,goodexp], max=tmaxs)
            if tmins lt tmin then tmin=tmins
            if tmaxs gt tmax then tmax=tmaxs
            xcenmins = min(aux[hdrs.xcenix,goodexp], max=xcenmaxs)
            if xcenmins lt xcenmin then xcenmin=xcenmins
            if xcenmaxs gt xcenmax then xcenmax=xcenmaxs
            ycenmins = min(aux[hdrs.ycenix,goodexp], max=ycenmaxs)
            if ycenmins lt ycenmin then ycenmin=ycenmins
            if ycenmaxs gt ycenmax then ycenmax=ycenmaxs
          endif
        endfor
        if (tmin lt 120) && (abs(tobs-tmax) lt 120) && (nexpsji gt tobs/120d) then areacovered=1
      endif ;sji files
areacovered=1
      ;if (nras gt 0) && (~areacovered || self.hinode) then begin
      if (nras gt 0) && ~areacovered then begin
        ;there is not enough information in the SJI, so we need to look at the rasters
        mreadfits_header, files[rasfiles], hdrras, only_tags='FOVX,FOVY,SAT_ROT,NWIN'
        fovxmaxr = max(hdrras.fovx)
        if fovxmaxr gt fovxmax then fovxmax=fovxmaxr
        fovymaxr = max(hdrras.fovy)
        if fovymaxr gt fovymax then fovymax=fovymaxr
        if N_ELEMENTS(satrot) eq 0 then satrot = hdrras.sat_rot $
        else satrot = [satrot, hdrras.sat_rot]
        for ras=0,nras-1 do begin
          aux = readfits(files[rasfiles[ras]], hdrr, exten_no=hdrras[ras].nwin+1, /silent)
          hdrr = fitshead2struct(hdrr)
          goodexp = where((aux[hdrr.DSRCFIX,*] gt 0) OR (aux[hdrr.DSRCNIX,*] gt 0), count)
          if count gt 0 then begin
            win1 = readfits(files[rasfiles[ras]], hdrwin1, exten_no=1, /silent, nslice=1)
            hdrwin1 = fitshead2struct(hdrwin1)
            if ~required_tags(hdrwin1, /naxis2) then begin
              win1 = readfits(files[rasfiles[ras]], hdrwin1, exten_no=1, /silent)
              hdrwin1 = fitshead2struct(hdrwin1)
            endif
            wcs = fitshead2wcs(hdrwin1)
            if required_tags(wcs, /roll_angle) then begin
              if N_ELEMENTS(rollangle) eq 0 then rollangle = wcs.roll_angle $
              else rollangle = [rollangle, wcs.roll_angle]
            endif
            if N_ELEMENTS(times) eq 0 then begin
              times = reform(aux[hdrr.time,goodexp])
              xcens = reform(aux[hdrr.xcenix,goodexp])
              ycens = reform(aux[hdrr.ycenix,goodexp])
            endif else begin
              times = [times, reform(aux[hdrr.time,goodexp])]
              xcens = [xcens, reform(aux[hdrr.xcenix,goodexp])]
              ycens = [ycens, reform(aux[hdrr.ycenix,goodexp])]
            endelse
            nexpras=nexpras+count
            tmins = min(aux[hdrr.time,goodexp], max=tmaxs)
            if tmins lt tmin then tmin=tmins
            if tmaxs gt tmax then tmax=tmaxs
            xcenmins = min(aux[hdrr.xcenix,goodexp], max=xcenmaxs)
            if xcenmins lt xcenmin then xcenmin=xcenmins
            if xcenmaxs gt xcenmax then xcenmax=xcenmaxs
            ycenmins = min(aux[hdrr.ycenix,goodexp], max=ycenmaxs)
            if ycenmins lt ycenmin then ycenmin=ycenmins
            if ycenmaxs gt ycenmax then ycenmax=ycenmaxs
          endif
        endfor
      endif

      self.obsid = obsid
      self.obsdir = obsdir
      self.startobs = startobs
      self.endobs = endobs
      self.tobs = tobs

      self.files_sji_nr = nsji
      if nsji gt 0 then self.files_sji = ptr_new(files[sjifiles])
      self.files_ras_nr = nras
      if nras gt 0 then self.files_ras = ptr_new(files[rasfiles])

      self.xcen = (xcenmin+xcenmax)/2.0
      self.ycen = (ycenmin+ycenmax)/2.0
      self.fovx = fovxmax
      self.fovy = fovymax
      self.fovx_move = fovxmax + xcenmax-xcenmin
      self.fovy_move = fovymax + ycenmax-ycenmin
      ;we have to incorporate the satellite rotation
      self.satrot = mean(satrot)
      self.satrotradeg = self.satrot/!radeg
      if N_ELEMENTS(rollangle) gt 0 then begin
        self.roll_angle = mean(rollangle)
        self.roll_angle_radeg = self.roll_angle/!radeg
        self.roll_angle_source = 'fitshead2wcs'
      endif else begin
        self.roll_angle = (-1) * self.satrot - 0.6463
        self.roll_angle_radeg = self.roll_angle/!radeg
        self.roll_angle_source = 'SATROT'
      endelse
      self.fovx_satrot = fovxmax*abs(cos(self.roll_angle_radeg)) + fovymax*abs(sin(self.roll_angle_radeg)) + xcenmax-xcenmin
      self.fovy_satrot = fovymax*abs(cos(self.roll_angle_radeg)) + fovxmax*abs(sin(self.roll_angle_radeg)) + ycenmax-ycenmin

      if N_ELEMENTS(fovxmaxr) gt 0 then begin
        self.fovx_ras = fovxmaxr
        self.fovy_ras = fovymaxr
        self.fovx_ras_move = fovxmaxr + xcenmax-xcenmin
        self.fovy_ras_move = fovymaxr + ycenmax-ycenmin
        self.fovx_ras_satrot = fovxmaxr*abs(cos(self.roll_angle_radeg)) + fovymaxr*abs(sin(self.roll_angle_radeg)) + xcenmax-xcenmin
        self.fovy_ras_satrot = fovymaxr*abs(cos(self.roll_angle_radeg)) + fovxmaxr*abs(sin(self.roll_angle_radeg)) + ycenmax-ycenmin
      endif
      
      ;get min and max of the slit pixel
      minslit = min(slitpixel, max=maxslit)
      self.slitpix = [minslit, maxslit]

      ;calculate regression of xcen and ycen of IRIS SJI
      if N_ELEMENTS(times) gt 1 then begin
        self.regxslope = regress(times, xcens, const=regxconst)
        self.regxconst = regxconst
        self.regyslope = regress(times, ycens, const=regyconst)
        self.regyconst = regyconst
      endif
      ;a test
;      times2 = min(times, max=maxtim)
;      times2 = [times2, maxtim]
;      x = [regxconst+self.regxslope*times2[0], regxconst+self.regxslope*times2[1]]
;      plot, times, xcens, xstyle=1,ystyle=1
;      oplot, times2, x, thick=3
;      stop
;      y = [regyconst+self.regyslope*times2[0], regyconst+self.regyslope*times2[1]]
;      plot, times, ycens, xstyle=1,ystyle=1
;      oplot, times2, y, thick=3
;      stop
     
    endelse ;files found
  endelse ;not usehcr
END


FUNCTION IRIS_obs2fov::get_error
  return, self.error
END

FUNCTION IRIS_obs2fov::get_xcen
  return, self.xcen
END

FUNCTION IRIS_obs2fov::get_ycen
  return, self.ycen
END

FUNCTION IRIS_obs2fov::get_fovx, move=move, satrot=satrot, expsatrot=expsatrot, raster=raster
  if keyword_set(raster) then begin
    if keyword_set(move) then return, self.fovx_ras_move
    if keyword_set(satrot) then return, self.fovx_ras_satrot
    if keyword_set(expsatrot) then return, self.fovx_ras_satrot + $
      self.fovexpand[0]*abs(cos(self.roll_angle_radeg)) + $
      self.fovexpand[1]*abs(sin(self.roll_angle_radeg))
    return, self.fovx_ras
  endif else begin
    if keyword_set(move) then return, self.fovx_move
    if keyword_set(satrot) then return, self.fovx_satrot
    if keyword_set(expsatrot) then return, self.fovx_satrot + $
      self.fovexpand[0]*abs(cos(self.roll_angle_radeg)) + $
      self.fovexpand[1]*abs(sin(self.roll_angle_radeg))
    return, self.fovx
  endelse
END

FUNCTION IRIS_obs2fov::get_fovy, move=move, satrot=satrot, expsatrot=expsatrot, raster=raster
  if keyword_set(raster) then begin
    if keyword_set(move) then return, self.fovy_ras_move
    if keyword_set(satrot) then return, self.fovy_ras_satrot
    if keyword_set(expsatrot) then return, self.fovy_ras_satrot + $
      self.fovexpand[1]*abs(cos(self.roll_angle_radeg)) + $
      self.fovexpand[0]*abs(sin(self.roll_angle_radeg))
    return, self.fovy_ras
  endif else begin
    if keyword_set(move) then return, self.fovy_move
    if keyword_set(satrot) then return, self.fovy_satrot
    if keyword_set(expsatrot) then return, self.fovy_satrot + $
      self.fovexpand[1]*abs(cos(self.roll_angle_radeg)) + $
      self.fovexpand[0]*abs(sin(self.roll_angle_radeg))
    return, self.fovy
  endelse
END

FUNCTION IRIS_obs2fov::get_fovexpand
  return, self.fovexpand
END

PRO IRIS_obs2fov::set_fovexpand, fovexpand, fovyexpand
  if N_ELEMENTS(fovexpand) eq 1 then begin
    self.fovexpand[0] = fovexpand
    if N_ELEMENTS(fovyexpand) eq 1 then self.fovexpand[1] = fovyexpand $
    else self.fovexpand[1] = fovexpand
  endif else if N_ELEMENTS(fovexpand) eq 2 then self.fovexpand = fovexpand
END

FUNCTION IRIS_obs2fov::get_timeexpand
  return, self.timeexpand
END

PRO IRIS_obs2fov::get_startendtimes, starttime, endtime
  time_window,[self.startobs,self.endobs], starttime, endtime, minutes=[-self.timeexpand[0],self.timeexpand[1]] ; this expands input time +/- per user window
END

PRO IRIS_obs2fov::set_timeexpand, startexpand, stopexpand
  if N_ELEMENTS(startexpand) eq 1 then begin
    self.timeexpand[0] = startexpand
    if N_ELEMENTS(stopexpand) eq 1 then self.timeexpand[1] = stopexpand $
    else self.timeexpand[1] = startexpand
  endif else if N_ELEMENTS(startexpand) eq 2 then self.timeexpand = startexpand
END

FUNCTION IRIS_obs2fov::get_satrot
  return, self.satrot
END

FUNCTION IRIS_obs2fov::get_rollangle, source=source
  source = self.roll_angle_source
  return, self.roll_angle
END

FUNCTION IRIS_obs2fov::get_obsid
  return, self.obsid
END

FUNCTION IRIS_obs2fov::get_obsdir
  return, self.obsdir
END

FUNCTION IRIS_obs2fov::get_files, sji=sji, raster=raster
  if keyword_set(sji) then return, *self.files_sji
  if keyword_set(raster) then return, *self.files_ras
  return, [*self.files_sji, *self.files_ras]
END

FUNCTION IRIS_obs2fov::get_startobs
  return, self.startobs
END

FUNCTION IRIS_obs2fov::get_endobs
  return, self.endobs
END

FUNCTION IRIS_obs2fov::get_tobs
  return, self.tobs
END

PRO IRIS_obs2fov::get_xycenreg, regxslope, regxconst, regyslope, regyconst
  regxslope = self.regxslope
  regxconst = self.regxconst
  regyslope = self.regyslope
  regyconst = self.regyconst
END

FUNCTION IRIS_obs2fov::get_slitpix
  return, self.slitpix
END

PRO IRIS_obs2fov::calc_overlap, sothcr
  startx = self.xcen - self.fovx_satrot/2
  endx = self.xcen + self.fovx_satrot/2
  starty = self.ycen - self.fovy_satrot/2
  endy = self.ycen + self.fovy_satrot/2

  startxras = self.xcen - self.fovx_ras_satrot/2
  endxras = self.xcen + self.fovx_ras_satrot/2
  startyras = self.ycen - self.fovy_ras_satrot/2
  endyras = self.ycen + self.fovy_ras_satrot/2

  for i=0,N_ELEMENTS(sothcr)-1 do begin
    startxsot = sothcr[i].xcen - sothcr[i].xfov/2
    endxsot = sothcr[i].xcen + sothcr[i].xfov/2
    startysot = sothcr[i].ycen - sothcr[i].yfov/2
    endysot = sothcr[i].ycen + sothcr[i].yfov/2

    ;total overlap SJI + raster
    if endxsot lt startx || startxsot gt endx then begin
      overlap=0.0
      overlapsot=0.0
    endif else begin ;endxsot lt startx || startxsot gt endx
      if endysot lt starty || startysot gt endy then begin
        overlap=0.0
        overlapsot=0.0
      endif else begin ;endysot lt starty || startysot gt endy
        ;there is some overlap
        if startxsot le startx then begin
          if endxsot ge endx then begin
            overlap=1.0
          endif else begin
            overlap = (endxsot-startx) / (endx-startx)
          endelse
        endif else begin ;startxsot le startx
          if endxsot ge endx then begin
            overlap = (endx-startxsot) / (endx-startx)
          endif else begin
            overlap = (endxsot-startxsot) / (endx-startx)
          endelse
        endelse ;startxsot le startx
        if startysot le starty then begin
          if endysot ge endy then begin
            overlap = overlap * 1.0
          endif else begin
            overlap = overlap * (endysot-starty) / (endy-starty)
          endelse
        endif else begin ;startysot le starty
          if endysot ge endy then begin
            overlap = overlap * (endy-startysot) / (endy-starty)
          endif else begin
            overlap = overlap * (endysot-startysot) / (endy-starty)
          endelse
        endelse ;startysot le starty
        
        ;calculate overlap of IRIS OBS on SOT
        if startx le startxsot then begin
          if endx ge endxsot then begin
            overlapsot=1.0
          endif else begin
            overlapsot = (endx-startxsot) / (endxsot-startxsot)
          endelse
        endif else begin ;startx le startxsot
          if endx ge endxsot then begin
            overlapsot = (endxsot-startx) / (endxsot-startxsot)
          endif else begin
            overlapsot = (endx-startx) / (endxsot-startxsot)
          endelse
        endelse ;startx le startxsot
        if starty le startysot then begin
          if endy ge endysot then begin
            overlapsot = overlapsot * 1.0
          endif else begin
            overlapsot = overlapsot * (endy-startysot) / (endysot-startysot)
          endelse
        endif else begin ;starty le startysot
          if endy ge endysot then begin
            overlapsot = overlapsot * (endysot-starty) / (endysot-startysot)
          endif else begin
            overlapsot = overlapsot * (endy-starty) / (endysot-startysot)
          endelse
        endelse ;starty le startysot
        
      endelse ;endysot lt starty || startysot gt endy
    endelse ;endxsot lt startx || startxsot gt endx

    ;overlap with raster only
    if endxsot lt startxras || startxsot gt endxras then begin
      overlapras=0.0
    endif else begin
      if endysot lt startyras || startysot gt endyras then begin
        overlapras=0.0
      endif else begin
        ;there is some overlap
        if startxsot le startxras then begin
          if endxsot ge endxras then begin
            overlapras=1.0
          endif else begin
            overlapras = (endxsot-startxras) / (endxras-startxras)
          endelse
        endif else begin
          if endxsot ge endxras then begin
            overlapras = (endxras-startxsot) / (endxras-startxras)
          endif else begin
            overlapras = (endxsot-startxsot) / (endxras-startxras)
          endelse
        endelse
        if startysot le startyras then begin
          if endysot ge endyras then begin
            overlapras = overlapras * 1.0
          endif else begin
            overlapras = overlapras * (endysot-startyras) / (endyras-startyras)
          endelse
        endif else begin
          if endysot ge endyras then begin
            overlapras = overlapras * (endyras-startysot) / (endyras-startyras)
          endif else begin
            overlapras = overlapras * (endysot-startysot) / (endyras-startyras)
          endelse
        endelse
      endelse
    endelse

    newsottemp = create_struct(sothcr[i], 'areasji_overlap', overlap*100.0, 'arearas_overlap', overlapras*100.0, 'areasot_overlap', overlapsot*100.0)
    if i eq 0 then newsot = newsottemp $
    else newsot = [newsot, newsottemp]
  endfor
  sothcr = newsot
END


PRO IRIS_obs2fov__define

  void = {IRIS_obs2fov, $
    obs:'', $
    obsid:'', $
    obsdir:'', $
    startobs:'', $
    endobs:'', $
    tobs:0.0d, $
    xcen:0.0, $
    ycen:0.0, $
    fovx:0.0, $
    fovy:0.0, $
    fovx_move:0.0, $
    fovy_move:0.0, $
    fovx_satrot:0.0, $
    fovy_satrot:0.0, $
    fovx_ras:0.0, $
    fovy_ras:0.0, $
    fovx_ras_move:0.0, $
    fovy_ras_move:0.0, $
    fovx_ras_satrot:0.0, $
    fovy_ras_satrot:0.0, $
    fovexpand:[0.0, 0.0], $
    timeexpand:[0.0, 0.0], $
    satrot:0.0, $
    satrotradeg:0.0d, $
    roll_angle:0.0, $
    roll_angle_radeg:0.0, $
    roll_angle_source:'', $
    regxslope:0.0, $
    regxconst:0.0, $
    regyslope:0.0, $
    regyconst:0.0, $
    slitpix:[0.0, 0.0], $
    files_sji:ptr_new(), $
    files_sji_nr:0, $
    files_ras:ptr_new(), $
    files_ras_nr:0, $
    error:0, $
    hinode:0, $
    usehcr:0 $
  }

END
