; $Id: iris_obs2fov_new__define.pro,v 1.3 2015/11/09 15:14:49 mawiesma Exp $  ;


FUNCTION IRIS_obs2fov_NEW::init, obs, usehcr=usehcr
  if N_ELEMENTS(obs) eq 1 then begin
    if size(obs, /type) eq 7 then begin
      self.obs = obs
      self.usehcr = keyword_set(usehcr)
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


PRO IRIS_obs2fov_NEW::cleanup
  if ptr_valid(self.files_sji) then ptr_free, self.files_sji
  if ptr_valid(self.files_ras) then ptr_free, self.files_ras
END


PRO IRIS_obs2fov_NEW::setobs, obs, usehcr=usehcr
  self.obs = ''
  self.usehcr = 0
  self.error = 0
  self.obsid = ''
  self.obsdir = ''
  self.startobs = ''
  self.endobs = ''
  self.tobs = 0.0d
  self.xcen = 0.0
  self.ycen = 0.0
  self.satrot = 0.0
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


PRO IRIS_obs2fov_NEW::docalculations
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
        print,'testing UIO: /mn/stornext/d10/HDC2/iris/data/level2/'+pathdate+obs
        if file_test('/irisa/data/level2/'+pathdate+obs) then obsdir = '/irisa/data/level2/'+pathdate+obs $
        else if file_test('/mn/stornext/d10/HDC2/iris/data/level2/'+pathdate+obs) then obsdir = '/mn/stornext/d10/HDC2/iris/data/level2/'+pathdate+obs $
        else self.usehcr=1
      endif else begin
        print, 'Cannot find OBS'
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
    self.fovx_nomove=hcr.xfov
    self.fovy_nomove=hcr.yfov
    self.startobs=hcr.starttime
    self.endobs=hcr.stoptime
    tstart = str2utc(self.startobs)
    tend = str2utc(self.endobs)
    self.tobs = ((tend.mjd-tstart.mjd)*86400000L + tend.time-tstart.time) / 1000d
    ;we have to incorporate the satellite rotation
    self.satrot = hcr.sat_rot
    self.fovx_satrot = self.fovx*abs(cos(self.satrot)) + self.fovy*abs(sin(self.satrot))
    self.fovy_satrot = self.fovy*abs(cos(self.satrot)) + self.fovx*abs(sin(self.satrot))
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
      sjifiles = where(strmatch(files,'*SJI*') eq 1, nsji, complement=rasfiles, ncomplement=nras)
      if nsji gt 0 then begin
        ;let's get all the information out of the SJI if there are enough
        mreadfits_header, files[sjifiles], hdrsji, only_tags='FOVX,FOVY,SAT_ROT'
        fovxmax = max(hdrsji.fovx)
        fovymax = max(hdrsji.fovy)
        satrot = hdrsji.sat_rot
        for sji=0,nsji-1 do begin
          aux = readfits(files[sjifiles[sji]], hdrs, exten_no=1, /silent)
          hdrs = fitshead2struct(hdrs)
          goodexp = where(aux[hdrs.dsrcsix,*] gt 0, count)
          if count gt 0 then begin
            if N_ELEMENTS(times) eq 0 then begin
              times = reform(aux[hdrs.time,goodexp])
              xcens = reform(aux[hdrs.xcenix,goodexp])
              ycens = reform(aux[hdrs.ycenix,goodexp])
            endif else begin
              times = [times, reform(aux[hdrs.time,goodexp])]
              xcens = [xcens, reform(aux[hdrs.xcenix,goodexp])]
              ycens = [ycens, reform(aux[hdrs.ycenix,goodexp])]
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

      if ~areacovered && nras gt 0 then begin
        ;there is not enough information in the SJI, so we need to look at the rasters
        mreadfits_header, files[rasfiles], hdrras, only_tags='FOVX,FOVY,SAT_ROT,NWIN'
        fovxmaxr = max(hdrsji.fovx)
        if fovxmaxr gt fovxmax then fovxmax=fovxmaxr
        fovymaxr = max(hdrsji.fovy)
        if fovymaxr gt fovymax then fovymax=fovymaxr
        if N_ELEMENTS(satrot) eq 0 then satrot = hdrras.sat_rot $
        else satrot = [satrot, hdrras.sat_rot]
        for ras=0,nras-1 do begin
          aux = readfits(files[rasfiles[ras]], hdrr, exten_no=hdrras[ras].nwin+1, /silent)
          hdrr = fitshead2struct(hdrr)
          goodexp = where((aux[hdrr.DSRCFIX,*] gt 0) OR (aux[hdrr.DSRCNIX,*] gt 0), count)
          if count gt 0 then begin
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
      self.fovx_nomove = fovxmax
      self.fovy_nomove = fovymax
      self.fovx = fovxmax + xcenmax-xcenmin
      self.fovy = fovymax + ycenmax-ycenmin
      ;we have to incorporate the satellite rotation
      self.satrot = mean(satrot)
      satrotradeg = self.satrot/!radeg
      self.fovx_satrot = fovxmax*abs(cos(satrotradeg)) + fovymax*abs(sin(satrotradeg)) + xcenmax-xcenmin
      self.fovy_satrot = fovymax*abs(cos(satrotradeg)) + fovxmax*abs(sin(satrotradeg)) + ycenmax-ycenmin

      ;calculate regression of xcen and ycen of IRIS SJI
      if N_ELEMENTS(times) gt 1 then begin
        self.regxslope = regress(times, xcens, const=regxconst)
        self.regxconst = regxconst
        self.regyslope = regress(times, ycens, const=regyconst)
        self.regyconst = regyconst
      endif

    endelse ;files found
  endelse ;not usehcr
END


FUNCTION IRIS_obs2fov_NEW::get_error
  return, self.error
END

FUNCTION IRIS_obs2fov_NEW::get_xcen
  return, self.xcen
END

FUNCTION IRIS_obs2fov_NEW::get_ycen
  return, self.ycen
END

FUNCTION IRIS_obs2fov_NEW::get_fovx, nomove=nomove, satrot=satrot
  if keyword_set(nomove) then return, self.fovx_nomove
  if keyword_set(satrot) then return, self.fovx_satrot
  return, self.fovx
END

FUNCTION IRIS_obs2fov_NEW::get_fovy, nomove=nomove, satrot=satrot
  if keyword_set(nomove) then return, self.fovy_nomove
  if keyword_set(satrot) then return, self.fovy_satrot
  return, self.fovy
END

FUNCTION IRIS_obs2fov_NEW::get_fovexpand
  return, self.fovexpand
END

PRO IRIS_obs2fov_NEW::set_fovexpand, fovexpand, fovyexpand
  if N_ELEMENTS(fovexpand) eq 1 then self.fovexpand[0] = fovexpand
  if N_ELEMENTS(fovexpand) eq 2 then self.fovexpand = fovexpand
  if N_ELEMENTS(fovyexpand) eq 1 then self.fovexpand[1] = fovyexpand
END

FUNCTION IRIS_obs2fov_NEW::get_satrot
  return, self.satrot
END

FUNCTION IRIS_obs2fov_NEW::get_obsid
  return, self.obsid
END

FUNCTION IRIS_obs2fov_NEW::get_obsdir
  return, self.obsdir
END

FUNCTION IRIS_obs2fov_NEW::get_files, sji=sji, raster=raster
  if keyword_set(sji) then return, *self.files_sji
  if keyword_set(raster) then return, *self.files_ras
  return, [*self.files_sji, *self.files_ras]
END

FUNCTION IRIS_obs2fov_NEW::get_startobs
  return, self.startobs
END

FUNCTION IRIS_obs2fov_NEW::get_endobs
  return, self.endobs
END

FUNCTION IRIS_obs2fov_NEW::get_tobs
  return, self.tobs
END

PRO IRIS_obs2fov_NEW::get_xycenreg, regxslope, regxconst, regyslope, regyconst
  regxslope = self.regxslope
  regxconst = self.regxconst
  regyslope = self.regyslope
  regyconst = self.regyconst
END

PRO IRIS_obs2fov_NEW::calc_overlap, sothcr
  for i=0,N_ELEMENTS(sothcr)-1 do begin
    startxsot = sothcr[i].xcen - sothcr[i].xfov/2
    endxsot = sothcr[i].xcen + sothcr[i].xfov/2
    startysot = sothcr[i].ycen - sothcr[i].yfov/2
    endysot = sothcr[i].ycen + sothcr[i].yfov/2

    startx = self.xcen - self.fovx_satrot/2
    endx = self.xcen + self.fovx_satrot/2
    starty = self.ycen - self.fovy_satrot/2
    endy = self.ycen + self.fovy_satrot/2

    if endxsot lt startx || startxsot gt endx then begin
      overlap=0.0
    endif else begin
      if endysot lt starty || startysot gt endy then begin
        overlap=0.0
      endif else begin
        ;there is some overlap
        if startxsot le startx then begin
          if endxsot ge endx then begin
            overlap=1.0
          endif else begin
            overlap = (endxsot-startx) / (endx-startx)
          endelse
        endif else begin
          if endxsot ge endx then begin
            overlap = (endx-startxsot) / (endx-startx)
          endif else begin
            overlap = (endxsot-startxsot) / (endx-startx)
          endelse
        endelse
        if startysot le starty then begin
          if endysot ge endy then begin
            overlap = overlap * 1.0
          endif else begin
            overlap = overlap * (endysot-starty) / (endy-starty)
          endelse
        endif else begin
          if endysot ge endy then begin
            overlap = overlap * (endy-startysot) / (endx-startx)
          endif else begin
            overlap = overlap * (endysot-startysot) / (endy-starty)
          endelse
        endelse
      endelse
    endelse
    newsottemp = create_struct(sothcr[i], 'area_overlap', overlap*100.0)
    if i eq 0 then newsot = newsottemp $
    else newsot = [newsot, newsottemp]
  endfor
  sothcr = newsot
END


PRO IRIS_obs2fov_NEW__define

  void = {IRIS_obs2fov_NEW, $
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
    fovx_nomove:0.0, $
    fovy_nomove:0.0, $
    fovx_satrot:0.0, $
    fovy_satrot:0.0, $
    fovexpand:[0.0, 0.0], $
    satrot:0.0, $
    regxslope:0.0, $
    regxconst:0.0, $
    regyslope:0.0, $
    regyconst:0.0, $
    files_sji:ptr_new(), $
    files_sji_nr:0, $
    files_ras:ptr_new(), $
    files_ras_nr:0, $
    error:0, $
    usehcr:0 $
  }

END
