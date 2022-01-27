;+
; NAME:
;       create_irisSJI_l2_like
;
; PURPOSE:
;       create_irisSJI_l2_like creates an IRIS-SJI-like level 2 fits file
;
; CATEGORY:
;       Martin Wiesmann / IRIS Data processing
;
; CALLING SEQUENCE:
;       create_irisSJI_l2_like, data, timearr=timearr, xycen=xycen, cdelt=cdelt, outfile=outfile, $
;       crpix=crpix, crval=crval, ctype=ctype, cunit=cunit, btype=btype, bunit=bunit, $
;       windesc=windesc, winwave=winwave, rollangle=rollangle, telescop=telescop
;
;
; INPUTS:
;       data: stack of images, 2- or 3-dimensional
;       timearr: a vector of date/time stamps (any solar soft compatible time format)
;       xycen: a matrix containing the x-/ycen for each image [2,nImages] ([0,*]=xcen, [1,*]=ycen)
;         this can be substituted with crpix and crval, in which case x-/ycen is calculated
;       cdelt: a 2-elements vector containing the cdelt values [cdelt1, cdelt2]
;       outfile: the path and filename in which to save the fits file
;
; OPTIONAL KEYWORD PARAMETERS:
;       crpix: a 2-elements vector containing the crpix values [crpix1, crpix2]
;         default = [(NAXIS1+1)/2, (NAXIS2+1)/2]
;       crval: a matrix containing the crval values for each image [2,nImages] ([0,*]=crval1, [1,*]=crval2)
;        default = [xcen, ycen]
;       ctype:  a 2-elements vector containing the ctype [ctype1, ctype2]
;         default = ['HPLN-TAN', 'HPLT-TAN'] (other values might cause problems with crispex)
;       cunit: a 2-elements vector containing the cunit [cunit1, cunit2]
;         default = ['arcsec', 'arcsec'] (other values might cause problems with crispex)
;       btype: a scalar string describing type of data values (e.g. Intensity)
;         default = ''
;       bunit: a scalar string describing the unit of the data values (e.g. DN)
;         default = ''
;       windesc: a scalar string describing the window (e.g. SJI_1440)
;         default = ''
;       windesc: a scalar number containing the main wavelength of the window in Angstrom (e.g. 1440)
;         default = 0
;       rollangle: either a scalar number or a vector of numbers with the same length as nImages
;         default = 0
;         will be used to calculate the PC-matrix of the wcs, which is used in crispex
;       telescop: a scalar string describing the telescope
;         default = '' ('IRIS' and 'SDO' might cause problems with crispex)
;         
; OUTPUTS:
;       IRIS-SJI-like level 2 fits file of input data, written to outfile
;
; CALLS:
;
; COMMON BLOCKS:
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;       2016-06-02: Martin Wiesmann (ITA, UIO).
;
; $Id: create_irissji_l2_like.pro,v 1.2 2016/06/02 12:27:36 mawiesma Exp $  ;


pro create_irisSJI_l2_like, data, timearr=timearr, xycen=xycen, cdelt=cdelt, outfile=outfile, $
  crpix=crpix, crval=crval, ctype=ctype, cunit=cunit, btype=btype, bunit=bunit, $
  windesc=windesc, winwave=winwave, rollangle=rollangle, telescop=telescop

  sizedata = size(data)
  if sizedata[0] lt 2 || sizedata[0] gt 3 then begin
    box_message, 'data must be at least 2-dimensional and maximum 3-dimensional'
    return
  endif
  if sizedata[0] eq 2 then nsteps=1 $
  else nsteps=sizedata[3]

  sizetimearr = size(timearr)
  if sizetimearr[0] ne 1 then begin
    box_message, 'time array must be provided as a 1-dimensional vector'
    return
  endif else begin
    if sizetimearr[1] ne nsteps then begin
      box_message, 'time array must be the same length as data'
      return
    endif
  endelse

  sizexycen = size(xycen)
  sizecrpix = size(crpix)
  sizecrval = size(crval)
  if sizexycen[0] eq 0 then begin
    if sizecrpix[0] eq 0 || sizecrval[0] eq 0 then begin
      box_message, 'Either xycen or crval and crpix must be provided'
      return
    endif
  endif else begin
    if nsteps eq 1 then begin
      if N_ELEMENTS(xycen) ne 2 then begin
        box_message, 'wrong format in xycen ([2,nsteps])'
        return
      endif
    endif else begin
      if sizexycen[0] ne 2 || sizexycen[2] ne nsteps then begin
        box_message, 'wrong format in xycen ([2,nsteps])'
        return
      endif
    endelse
  endelse

  if sizecrpix[0] gt 0 then begin
;    if nsteps eq 1 then begin
      if N_ELEMENTS(crpix) ne 2 then begin
        box_message, 'crpix must be a 2-elements vector'
        return
      endif
;    endif else begin
;      if sizecrpix[0] ne 2 || sizecrpix[2] ne nsteps then begin
;        box_message, 'wrong format in crpix ([2,nsteps])'
;        return
;      endif
;    endelse
  endif

  if sizecrval[0] gt 0 then begin
    if nsteps eq 1 then begin
      if N_ELEMENTS(crval) ne 2 then begin
        box_message, 'wrong format in crval ([2,nsteps])'
        return
      endif
    endif else begin
      if sizecrval[0] ne 2 || sizecrval[2] ne nsteps then begin
        box_message, 'wrong format in crval ([2,nsteps])'
        return
      endif
    endelse
  endif

  ;sizecdelt = size(cdelt)
  ;if nsteps eq 1 then begin
    if N_ELEMENTS(cdelt) ne 2 then begin
      box_message, 'cdelt must be a 2-elements vector'
      return
    endif
;  endif else begin
;    if sizecdelt[0] ne 2 || sizecdelt[2] ne nsteps then begin
;      box_message, 'wrong format in cdelt ([2,nsteps])'
;      return
;    endif
;  endelse
  
  if N_ELEMENTS(ctype) eq 0 then begin
    ctype = ['HPLN-TAN', 'HPLT-TAN']
  endif else if N_ELEMENTS(ctype) ne 2 then begin
    box_message, 'ctype must be a 2-elements string array'
    return
  endif
  
  if N_ELEMENTS(cunit) eq 0 then begin
    cunit = ['arcsec', 'arcsec']
  endif else if N_ELEMENTS(cunit) ne 2 then begin
    box_message, 'cunit must be a 2-elements string array'
    return
  endif

  if N_ELEMENTS(btype) eq 0 then begin
    btype = ''
  endif else if N_ELEMENTS(btype) ne 1 then begin
    box_message, 'btype must be a scalar string'
    return
  endif

  if N_ELEMENTS(bunit) eq 0 then begin
    bunit = ''
  endif else if N_ELEMENTS(bunit) ne 1 then begin
    box_message, 'bunit must be a scalar string'
    return
  endif

  if N_ELEMENTS(windesc) eq 0 then begin
    windesc = ''
  endif else if N_ELEMENTS(windesc) ne 1 then begin
    box_message, 'windesc must be a scalar string'
    return
  endif

  if N_ELEMENTS(winwave) eq 0 then begin
    winwave = 0
  endif else if N_ELEMENTS(winwave) ne 1 then begin
    box_message, 'winwave must be a scalar number'
    return
  endif

  if N_ELEMENTS(rollangle) eq 0 then begin
    rollangle = fltarr(nsteps)
  endif else if N_ELEMENTS(rollangle) ne 1 && N_ELEMENTS(rollangle) ne nsteps then begin
    box_message, 'rollangle must be a scalar number or a vector the size of data'
    return
  endif else begin
    if N_ELEMENTS(rollangle) eq 1 then rollangle = make_array(nsteps, value=rollangle)
  endelse
  
  if N_ELEMENTS(telescop) eq 0 then begin
    telescop = ''
  endif else if N_ELEMENTS(telescop) ne 1 then begin
    box_message, 'telescop must be a scalar string'
    return
  endif

  if N_ELEMENTS(outfile) ne 1 then begin
    box_message, 'outfile must be provided'
    return
  endif
  
  startobs = anytim2utc(timearr[0],/ccsds)
  endobs = anytim2utc(timearr[nsteps-1],/ccsds)
  tstart = str2utc(startobs)
  timetemp = anytim2utc(timearr, /internal)
  timearr0 = (timetemp.mjd-tstart.mjd)*86400L + (timetemp.time-tstart.time)/1000d

  ;calculate basic step size
  if nsteps gt 1 then begin
    cdelt3 = dblarr(nsteps-1)
    for i=1,nsteps-1 do begin
      cdelt3[i-1] = timearr0[i] - timearr0[i-1]
    endfor
  endif else cdelt3=0.0
  cdelt3 = mean(cdelt3)

  crpix3 = fix(nsteps/2.0)+1
  crval3 = timearr0[crpix3-1]
  ctype3 = 'Time'
  cunit3 = 'seconds'
  

  if sizecrpix[0] eq 0 || sizecrval[0] eq 0 then begin
    crpix = [(sizedata[1]+1)/2.0, (sizedata[2]+1)/2.0]
    crval = xycen
  endif else begin
    if sizexycen[0] eq 0 then begin
      a=rollangle[crpix3-1]/!radeg
      xcenall = crval[0,crpix3-1] + cdelt[0] * cos(a) * ((sizedata[1]+1)/2.0 - crpix[0]) $
        - cdelt[1] * sin(a) * ((sizedata[2]-1)/2.0 - crpix[1])
      ycenall = crval[1,crpix3-1] + cdelt[0] + sin(a) * ((sizedata[1]+1)/2.0 - crpix[0]) $
        + cdelt[1] * cos(a) * ((sizedata[2]-1)/2.0 - crpix[1])
    endif
  endelse
  if N_ELEMENTS(xcenall) eq 0 then begin
    xcenall = xycen[0,crpix3-1]
    ycenall = xycen[1,crpix3-1]
  endif

  pc1_1 = fltarr(nsteps)
  pc1_2 = fltarr(nsteps)
  pc2_1 = fltarr(nsteps)
  pc2_2 = fltarr(nsteps)
  pc3_1 = fltarr(nsteps)
  pc3_2 = fltarr(nsteps)
  
  for i=0,nsteps-1 do begin
    a=rollangle[i]/!radeg
    pc1_1[i] = cos(a)
    pc1_2[i] = -sin(a) * (cdelt[1]/cdelt[0])
    pc2_1[i] = sin(a) * (cdelt[0]/cdelt[1])
    pc2_2[i] = cos(a)
  endfor

  ;stop
  ;first region is written into primary block
  mkhdr, mainheader, data, /extend
  ;add some information to the primary header
  ;if required_tags(hdrall, /telescop) then telescop=IRISl12_mostcommonvalue(hdrall.Telescop) $
  ;else telescop=''
  sxaddpar, mainheader, 'TELESCOP', telescop
  ;if required_tags(hdrall, /instrume) then instrume=IRISl12_mostcommonvalue(hdrall.instrume) $
  ;else instrume=''
  ;sxaddpar, mainheader, 'INSTRUME', instrume
  ;sxaddpar, mainheader, 'DATA_LEV', 2.0;OBSvars.DATA_LEV
  ;sxaddpar, mainheader, 'LVL_NUM', 2.0;OBSvars.LVL_NUM
  ;sxaddpar, mainheader, 'VER_RF2', version;OBSvars.version
  ;sxaddpar, mainheader, 'DATE_RF2', Date_RF2;OBSvars.Date_RF2
  ;if required_tags(hdrall, /lvl_num) then lvl_num=hdrall.lvl_num $
  ;else lvl_num=dblarr(N_ELEMENTS(hdrall))
  ;sxaddpar, mainheader, 'DATA_SRC', mean(lvl_num);OBSvars.DATA_SRC
  ;if required_tags(hdrall, /ORIGIN) then ORIGIN=IRISl12_mostcommonvalue(hdrall.ORIGIN) $
  ;else ORIGIN=''
  ;sxaddpar, mainheader, 'ORIGIN', ORIGIN
  ;if required_tags(hdrall, /BLD_VERS) then BLD_VERS=IRISl12_mostcommonvalue(hdrall.BLD_VERS) $
  ;else BLD_VERS=''
  ;sxaddpar, mainheader, 'BLD_VERS', BLD_VERS
  ;sxaddpar, mainheader, 'LUTID', 0;OBSvars.LUTID
  ;sxaddpar, mainheader, 'OBSID', obs2fov->get_obsid();OBSvars.OBSid
  ;sxaddpar, mainheader, 'OBS_DESC', '';OBSvars.OBS_Desc
  ;sxaddpar, mainheader, 'OBSLABEL', '';OBSvars.OBSLABEL
  ;sxaddpar, mainheader, 'OBSTITLE', '';OBSvars.OBSTITLE
  sxaddpar, mainheader, 'DATE_OBS', startobs
  sxaddpar, mainheader, 'DATE_END', endobs
  sxaddpar, mainheader, 'STARTOBS', startobs
  sxaddpar, mainheader, 'ENDOBS', endobs;OBSvars.OBSend
  ;sxaddpar, mainheader, 'OBSREP', 1;OBSvars.OBSrep
  ;if required_tags(hdrall, /CAMERA) then CAMERA=IRISl12_mostcommonvalue(hdrall.CAMERA) $
  ;else CAMERA=0
  ;sxaddpar, mainheader, 'CAMERA', CAMERA
  ;sxaddpar, mainheader, 'STATUS', '';OBSvars.STATUS
  ;if required_tags(hdrall, /BTYPE) then BTYPE=IRISl12_mostcommonvalue(hdrall.BTYPE) $
  ;else BTYPE=''
  sxaddpar, mainheader, 'BTYPE', BTYPE;OBSvars.btype
  ;if required_tags(hdrall, /BUNIT) then BUNIT=IRISl12_mostcommonvalue(hdrall.BUNIT) $
  ;else BUNIT=''
  sxaddpar, mainheader, 'BUNIT', BUNIT;OBSvars.bunit
  sxaddpar, mainheader, 'BSCALE', 1.0, format="f4.2";, ' True_value = BZERO + BSCALE*Array_value', after='BZERO'
  sxaddpar, mainheader, 'BZERO', 0;, ' True_value = BZERO + BSCALE*Array_value', after='BTYPE'
  ;sxaddpar, mainheader, 'HLZ', '';OBSvars.HLZ
  ;sxaddpar, mainheader, 'SAA', '';OBSvars.SAA
  sxaddpar, mainheader, 'SAT_ROT',  mean(rollangle)
  ;;sxaddpar, mainheader, 'AECNOBS', 0;OBSvars.AECNOBS
  ;;sxaddpar, mainheader, 'AECNRAS', 0;OBSvars.AECNRAS
  ;;sxaddpar, mainheader, 'ACS_ECLP', OBSvars.ACS_ECLP
  ;;sxaddpar, mainheader, 'ACS_MODE', OBSvars.ACS_MODE
  ;;sxaddpar, mainheader, 'ACS_SAFE', OBSvars.ACS_SAFE
  ;;sxaddpar, mainheader, 'ACS_SUNP', OBSvars.ACS_SUNP
  ;;sxaddpar, mainheader, 'ASD_REC', OBSvars.ASD_REC
  ;;sxaddpar, mainheader, 'DATE', OBSvars.DAT
  ;if required_tags(hdrall, /DSUN_OBS) then DSUN_OBS=mean(hdrall.DSUN_OBS) $
  ;else DSUN_OBS=0d
  ;sxaddpar, mainheader, 'DSUN_OBS', DSUN_OBS
  ;;if required_tags(hdrall, /DSUN_REF) then DSUN_OBS=mean(hdrall.DSUN_REF) $
  ;;else DSUN_REF=0d
  ;;sxaddpar, mainheader, 'DSUN_REF', DSUN_REF
  ;
  ;;sxaddpar, mainheader, 'IAECEVFL', '';OBSvars.IAECEVFL
  ;;sxaddpar, mainheader, 'IAECFLAG', '';OBSvars.IAECFLAG
  ;;sxaddpar, mainheader, 'IAECFLFL', '';OBSvars.IAECFLFL
  ;sxaddpar, mainheader, 'TR_MODE', '';OBSvars.TR_MODE

  sxaddpar, mainheader, 'FOVY', sizedata[2]*cdelt[1]
  sxaddpar, mainheader, 'FOVX', sizedata[1]*cdelt[0]
  sxaddpar, mainheader, 'XCEN', xcenall
  sxaddpar, mainheader, 'YCEN', ycenall
  ;sxaddpar, mainheader, 'SUMSPTRL', 1;OBSvars.sumspecmin
  ;sxaddpar, mainheader, 'SUMSPAT', 1;OBSvars.sumspatmin
  ;if required_tags(hdrall, /EXPTIME) then EXPTIME=hdrall.EXPTIME $
  ;else EXPTIME=dblarr(N_ELEMENTS(hdrall))
  ;sxaddpar, mainheader, 'EXPTIME', mean(exptime);OBSvars.exptime
  ;sxaddpar, mainheader, 'EXPMIN', min(exptime, max=maxexp);OBSvars.expmin
  ;sxaddpar, mainheader, 'EXPMAX', maxexp;OBSvars.expmax
  ;sxaddpar, mainheader, 'DATAMEAN', statistics.datamean
  ;sxaddpar, mainheader, 'DATARMS', statistics.datarms
  ;sxaddpar, mainheader, 'DATAMEDN', statistics.datamedn
  ;sxaddpar, mainheader, 'DATAMIN', statistics.datamin
  ;sxaddpar, mainheader, 'DATAMAX', statistics.datamax
  ;sxaddpar, mainheader, 'DATAVALS', statistics.datavals
  ;sxaddpar, mainheader, 'MISSVALS', 0;cmissing
  ;sxaddpar, mainheader, 'NSATPIX', 0;csaturated
  ;sxaddpar, mainheader, 'NSPIKES', 0
  ;sxaddpar, mainheader, 'TOTVALS', statistics.datavals;+cmissing+csaturated+0
  ;sxaddpar, mainheader, 'PERCENTD', 100.0;float(statistics.vals) / (statistics.vals+cmissing+csaturated+0) *100
  ;sxaddpar, mainheader, 'DATASKEW', statistics.dataskew
  ;sxaddpar, mainheader, 'DATAKURT', statistics.kurtosis
  ;sxaddpar, mainheader, 'DATAP01', statistics.datap01
  ;sxaddpar, mainheader, 'DATAP10', statistics.datap10
  ;sxaddpar, mainheader, 'DATAP25', statistics.datap25
  ;sxaddpar, mainheader, 'DATAP75', statistics.datap75
  ;sxaddpar, mainheader, 'DATAP90', statistics.datap90
  ;sxaddpar, mainheader, 'DATAP95', statistics.datap95
  ;sxaddpar, mainheader, 'DATAP98', statistics.datap98
  ;sxaddpar, mainheader, 'DATAP99', statistics.datap99
  ;;
  ;;      sxaddpar, mainheader, 'NEXP_PRP', float(nsteps)/float((*(rastersSJI[nrFW].rastercrs[nrCRS])).rasterPos)
  ;sxaddpar, mainheader, 'NEXP', nfiles;nsteps
  ;;      sxaddpar, mainheader, 'NEXPOBS', l1to2log.filesexpected
  ;;      sxaddpar, mainheader, 'NRASTERP', (*(rastersSJI[nrFW].rastercrs[nrCRS])).rasterPos
  ;;      sxaddpar, mainheader, 'RASTYPDX', 1;nrCRS+1
  ;;      sxaddpar, mainheader, 'RASTYPNX', 1;rastersSJI[nrFW].nrasters
  ;;      sxaddpar, mainheader, 'RASRPT', 1
  ;;      sxaddpar, mainheader, 'RASNRPT', 1
  ;;      sxaddpar, mainheader, 'CADPL_AV', cadplav
  ;;      sxaddpar, mainheader, 'CADPL_DV', cadpldv
  ;;      sxaddpar, mainheader, 'CADEX_AV', cadexav
  ;;      sxaddpar, mainheader, 'CADEX_DV', cadexdv
  ;;      sxaddpar, mainheader, 'MISSOBS', l1to2log.nmissing
  ;sxaddpar, mainheader, 'MISSRAS', 0
  ;;
  sxaddpar, mainheader, 'PC1_1', mean(pc1_1);OBSvars.PC1_1
  sxaddpar, mainheader, 'PC1_2', mean(pc1_2);OBSvars.PC1_2
  sxaddpar, mainheader, 'PC2_1', mean(pc2_1);OBSvars.PC2_1
  sxaddpar, mainheader, 'PC2_2', mean(pc2_2);OBSvars.PC2_2
  sxaddpar, mainheader, 'PC3_1', mean(pc3_1);OBSvars.PC3_1
  sxaddpar, mainheader, 'PC3_2', mean(pc3_2);OBSvars.PC3_2

  sxaddpar, mainheader, 'NWIN', 1

  ;add window-specific keywords with counter to mainheader
  number=string(1, format='(I1)')
  ;sxaddpar, mainheader, 'TDET'+number, 'SJI';OBSvars.SJIdet[0]
  ;if required_tags(hdrall, /WAVE_STR) then WAVE_STR=hdrall.WAVE_STR $
  ;else WAVE_STR=''
  sxaddpar, mainheader, 'TDESC'+number, windesc
  ;if required_tags(hdrall, /WAVELNTH) then WAVELNTH=hdrall.WAVELNTH $
  ;else WAVELNTH=0
  sxaddpar, mainheader, 'TWAVE'+number, winwave
  ;;      sxaddpar, mainheader, 'TWMIN'+number, OBSvars.SJIwavemin[0]
  ;;      sxaddpar, mainheader, 'TWMAX'+number, OBSvars.SJIwavemax[0]
  ;sxaddpar, mainheader, 'TDMEAN'+number, statistics.datamean
  ;sxaddpar, mainheader, 'TDRMS'+number, statistics.datarms
  ;sxaddpar, mainheader, 'TDMEDN'+number, statistics.datamedn
  ;sxaddpar, mainheader, 'TDMIN'+number, statistics.datamin
  ;sxaddpar, mainheader, 'TDMAX'+number, statistics.datamax
  ;sxaddpar, mainheader, 'TDVALS'+number, statistics.datavals
  ;sxaddpar, mainheader, 'TMISSV'+number, 0;cmissing
  ;sxaddpar, mainheader, 'TSATPX'+number, 0;csaturated
  ;sxaddpar, mainheader, 'TSPIKE'+number, 0
  ;sxaddpar, mainheader, 'TTOTV'+number, statistics.datavals;+cmissing+csaturated+0
  ;sxaddpar, mainheader, 'TPCTD'+number, 100.0;float(statistics.vals) / (statistics.vals+cmissing+csaturated+0) *100
  ;sxaddpar, mainheader, 'TDSKEW'+number, statistics.dataskew
  ;sxaddpar, mainheader, 'TDKURT'+number, statistics.kurtosis
  ;sxaddpar, mainheader, 'TDP01_'+number, statistics.datap01
  ;sxaddpar, mainheader, 'TDP10_'+number, statistics.datap10
  ;sxaddpar, mainheader, 'TDP25_'+number, statistics.datap25
  ;sxaddpar, mainheader, 'TDP75_'+number, statistics.datap75
  ;sxaddpar, mainheader, 'TDP90_'+number, statistics.datap90
  ;sxaddpar, mainheader, 'TDP95_'+number, statistics.datap95
  ;sxaddpar, mainheader, 'TDP98_'+number, statistics.datap98
  ;sxaddpar, mainheader, 'TDP99_'+number, statistics.datap99
  ;sxaddpar, mainheader, 'TSR'+number, 1;OBSvars.tsr+1
  ;sxaddpar, mainheader, 'TER'+number, naxis2;OBSvars.ter+1
  ;sxaddpar, mainheader, 'TSC'+number, 1;OBSvars.SJIsc_rtype[0]+1
  ;sxaddpar, mainheader, 'TEC'+number, naxis1;OBSvars.SJIec_rtype[0]+1
  ;;
  ;;      ;sxaddpar, mainheader, 'CCDTYPE', 'SJI'
  ;
  fxaddpar, mainheader, 'CDELT1', cdelt[0];OBSvars.CDELT1SJI     ;image scale in the x-direction
  fxaddpar, mainheader, 'CDELT2', cdelt[1];OBSvars.CDELT2SJI     ;image scale in the y-direction
  fxaddpar, mainheader, 'CDELT3', cdelt3      ;slit width for FUV,NUV


  fxaddpar, mainheader, 'CRPIX1', crpix[0]      ;CRPIX1: location of sun/wave center in CCD x direction
  fxaddpar, mainheader, 'CRPIX2', crpix[1]      ;CRPIX2: location of sun/wave center in CCD y direction
  fxaddpar, mainheader, 'CRPIX3', crpix3;OBSvars.CRPIX3SJI      ;"1" for FUV/NUV

  ;fxaddpar, mainheader, 'CRS_TYPE', hdr[sjiind[0]].CRS_TYPE   ;CRS Type

  fxaddpar, mainheader, 'CRVAL1', crval[0,crpix3-1]     ;SOLARX (SJI), wavelength (FUV&NUV)
  fxaddpar, mainheader, 'CRVAL2', crval[1,crpix3-1]     ;SOLARY
  fxaddpar, mainheader, 'CRVAL3', crval3;OBSvars.CRVAL3SJI     ;SOLARX (FUV/NUV), time (SJI)

  fxaddpar, mainheader, 'CTYPE1', ctype[0]     ;HPLN-TAN (SOLARX); WAVE for FUV/NUV
  fxaddpar, mainheader, 'CTYPE2', ctype[1]     ;HPLT-TAN (SOLARY)
  fxaddpar, mainheader, 'CTYPE3', 'Time'     ;HPLN-TAN (SOLARX) for FUV/NUV

  fxaddpar, mainheader, 'CUNIT1', cunit[0];OBSvars.CUNIT1SJI     ;arcsec for SJI, Angstrom for second FUV CCD
  fxaddpar, mainheader, 'CUNIT2', cunit[1];OBSvars.CUNIT2SJI
  fxaddpar, mainheader, 'CUNIT3', 'seconds'

  ;if required_tags(hdrall, /KEYWDDOC) then KEYWDDOC=hdrall.KEYWDDOC $
  ;else KEYWDDOC=''
  ;sxaddpar, mainheader, 'KEYWDDOC', IRISl12_mostcommonvalue(KEYWDDOC)
  ;if required_tags(hdrall, /HISTORY) then HISTORY=hdrall[0].HISTORY $
  ;else HISTORY=''
  ;for i=0,N_ELEMENTS(HISTORY)-1 do begin
  ;  if strcompress(HISTORY[i], /remove_all) ne '' then sxaddpar, mainheader, 'HISTORY', HISTORY[i]
  ;endfor
  ;sxaddpar, mainheader, 'HISTORY', 'IRIS-AIA RF2'



  ;file = outdir+'aia_l2_'+obs2fov->get_obsid()+'_'+waves[iwave]+'.fits'
  writefits, outfile, data, mainheader

  ;write extension with auxiliary data (time, PZTX, PZTY, Exposure duration, etc.)
  auxdata = dblarr(20, nsteps)
  auxdata[ 0, *] = timearr0
  auxdata[ 1, *] = 0;pztx
  auxdata[ 2, *] = 0;pzty
  auxdata[ 3, *] = 0;exptime
  auxdata[ 4, *] = 0;slitx
  auxdata[ 5, *] = 0;slity
  auxdata[ 6, *] = 1;sumsptrlSJI
  auxdata[ 7, *] = 1;sumspatSJI
  auxdata[ 8, *] = 1;DataSrcSJI
  auxdata[ 9, *] = 0;LUTIDsji
  auxdata[10, *] = crval[0,*]
  auxdata[11, *] = crval[1,*]
  auxdata[12, *] = 0;obs_vr
  auxdata[13, *] = 0;ophase
  auxdata[14, *] = pc1_1
  auxdata[15, *] = pc1_2
  auxdata[16, *] = pc2_1
  auxdata[17, *] = pc2_2
  auxdata[18, *] = pc3_1
  auxdata[19, *] = pc3_2
  mkhdr, header, auxdata, /image
  sxaddpar, header, 'TIME', 0, 'time of each exposure in s after start of OBS (rowindex)'
  sxaddpar, header, 'PZTX', 1, 'PZTX of each exposure in arcsec (rowindex)'
  sxaddpar, header, 'PZTY', 2, 'PZTY of each exposure in arcsec (rowindex)'
  sxaddpar, header, 'EXPTIMES', 3, 'SJI Exposure duration of each exposure in s (rowindex)'
  sxaddpar, header, 'SLTPX1IX', 4, 'Slit center in X of each exposure in window-pixels (rowindex)'
  sxaddpar, header, 'SLTPX2IX', 5, 'Slit center in Y of each exposure in window-pixels (rowindex)'
  sxaddpar, header, 'SUMSPTRS', 6, 'SJI spectral summing (rowindex)'
  sxaddpar, header, 'SUMSPATS', 7, 'SJI spatial summing (rowindex)'
  sxaddpar, header, 'DSRCSIX', 8, 'SJI data source level (rowindex)'
  sxaddpar, header, 'LUTIDS', 9, 'SJI LUT ID (rowindex)'
  sxaddpar, header, 'XCENIX', 10, 'XCEN (rowindex)'
  sxaddpar, header, 'YCENIX', 11, 'YCEN (rowindex)'
  sxaddpar, header, 'OBS_VRIX', 12, 'Speed of observer in radial direction (rowindex)'
  sxaddpar, header, 'OPHASEIX', 13, 'Orbital phase (rowindex)'
  sxaddpar, header, 'PC1_1IX', 14, 'PC1_1 (rowindex)'
  sxaddpar, header, 'PC1_2IX', 15, 'PC1_2 (rowindex)'
  sxaddpar, header, 'PC2_1IX', 16, 'PC2_1 (rowindex)'
  sxaddpar, header, 'PC2_2IX', 17, 'PC2_2 (rowindex)'
  sxaddpar, header, 'PC3_1IX', 18, 'PC3_1 (rowindex)'
  sxaddpar, header, 'PC3_2IX', 19, 'PC3_2 (rowindex)'
  writefits, outfile, auxdata, header, /append

end
