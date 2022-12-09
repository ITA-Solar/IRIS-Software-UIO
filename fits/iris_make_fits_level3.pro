;
;+
; NAME:
;       IRIS_MAKE_FITS_LEVEL3
;
; PURPOSE:
;       iris_make_fits_level3 writes a four dimensional data cube
;       with axes (x,y,lambda,t) for use with CRISPEX software
;
; CATEGORY:
;       IRIS Data analysis SW
;
; CALLING SEQUENCE:
;       iris_make_fits_level3,file ,iwin,sp=sp,tmp_size=tmp_size,/all
;
; INPUTS:
;       file:  iris level2 fits rasters (lambda,y,x) to be put into
;              sequence
;       iwin:  array containing the spectral windows to be included
;              (if not given, list is printed)
;
; KEYWORD PARAMETERS:
;       sp:   if set also make specral "sp" cube used by CRISPEX,
;             containing data in the order (lambda,t,x,y)
;       tmp_size: buffer size in GB used by br_transpose_fits_level3
;                 for transposing data for sp cube
;       sjifile: name of level 2 file containing slit jaw images to
;                be used in the reference cube
;       all:  if set, includes all windows
;       wdir: if set, write output files into this directory instead of
;             current directory
;       yshift - three element array with shift in y to be applied to
;                raster files in [fuv1,fuv2,nuv] with respect to SJI
;       wcscorr - correct wcs parameters when dx .ne. dy (default)
;       replace- replace existing level3 data if already existing
;       silent: if set, no printout of version
;
; OUTPUTS:
;       file in crispex format with name 'iris_'+line_ids'_im.fits'
;
; CALLS:
;       br_transpose_fits_level3
;
; COMMON BLOCKS:
;
; PROCEDURE:
;       The procedure opens and populates an object of class IRIS_DATA
;       based on the files name given, then populates the header and
;       data section of the level 3 fits file using the mehtods of
;       that class.
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;       22-February-2013: Viggo Hansteen - first version with multiple
;                                          windows
;       1.21 2013/09/30 matsc   - fixed wdir problem
;       1.22 2013/09/30 matsc   - added version printout
;       1.23 2013/10/02 matsc   - can now handle filenames with
;                                 leading directory specification
;       1.24 2013/10/03 matsc   - added parameter checking. made
;                                 version printout default if /silent
;                                 is not given
;       1.25 2013/10/03 matsc   - adds DATA_LEV and LVL_NUM to header
;       1.26 2013/10/22 matsc   - temporary fix of raster coordinates on slit image.
;       1.27 2013/10/22 matsc   - as above
;       1.28 2013/10/23 matsc   - sets missing data to NaN
;       1.29 2013/10/25 matsc   - added version number to fits keyword VER_RF3 treats also -199 as missing
;       1.30 2013/10/31 matsc   - will not make sp file if nt=1, more robust calculation of slit position
;       1.31 2013/11/04 matsc   - /replace keyword introduced, added OBSID and OBS_DESC to FITS header
;       1.32 2013/11/13 matsc   - yshift keyword introduced. Updated position keywords
;       1.33 2013/11/22 matsc   - corrected error indexing region. was OK for /all but not otherwise
;       1.34 2013/12/20 matsc   - corrected crpix1, crval1 and slitpos for sit-and-stare, added date_obs
;       1.35 2014/01/14 matsc   - improved handling of raster position when there are missing data
;       1.36 2014/01/23 matsc   - wcscorr keyword introduced, dx for sit-and-stare set to dy value for wcs purposes
;       1.37 2014/01/26 matsc   - made /wcscorr the default (can be switched off with wcscorr=0)
;       1.38 2014/02/14 matsc   - changed xys[*,*,1] from 0.0 (IDL-notation) to 1.0 (FITS-notation)
;       1.39 2014/05/14 matsc   - if PC3_3 is present in level2 sets wcscorr=0 as default
;       1.40 2014/05/16 matsc   - works both with PC matrix in main header (old format) and in extension headers (new format)
;       1.41 2014/06/13 matsc   - added STARTOBS keyword, sets DATE_OBS to be STARTOBS for backward compatability
;       1.41 2014/06/13 matsc   - uses mid-raster in time-series for header-parameters if ver_rf2 is equal to or later than 2014-05-15
;       1.42 2014/11/21 matsc   - made CRPIX4 and CRVAL4 consistent with convention for level2 -
;                                 significant change only for cases with missing data
;       1.43 2015/01/13 matsc   - speeded up large sit-and-stare datasets. uses maximum of tmp_size GB for output array (default 12 GB)
;       1.44 2015/12/10 matsc   - changed sit-and-stare test to use method getsit_and_stare from iris_data
;       1.45 2017/06/23 Nick Crump - added L2 HISTORY to L3 header to keep track of iris_prep version
;       1.46 2017/06/27 matsc   - added VER_RF2 and DATE_RF2 to L3
;                                 header
;       1.47 2022/12/06 matsc   - made modifications to CRPIX1, CDELT1 and WCS keywords to properly display
;                                 and show coordinates in CRISPEX for W->E scans
;
; $Id: 2022-12-06 12:29 CET $
;
;-
pro iris_make_fits_level3,file,iwin,sp=sp,tmp_size=tmp_size,sjifile=sjifile,all=all,wdir=wdir,replace=replace,yshift=yshift,wcscorr=wcscorr,$
 silent=silent,debug=debug
verstring="$Id: iris_make_fits_level3.pro,v 1.46 2017/06/27 11:57:13 matsc Exp $"
ic=strpos(verstring,'pro,v')+6
ver_rf3=strmid(verstring,ic,strlen(verstring)-ic-6)
if(~keyword_set(silent)) then begin
  message,'version '+ver_rf3,/info
endif
if n_params() lt 1 then begin
  message,'iris_make_fits_level3,file ,iwin,/sp,sjifile=sjifile,tmp_size=tmp_size,/all,wdir=wdir,/wcscorr,yshift=yshift,/replace,/silent',/cont
  return
end
if n_elements(wdir) eq 0 then begin
  cd,current=wdir
endif
nfile=n_elements(file)

; check existence of files

error=0
for i=0,nfile-1 do begin
  dum=file_search(file[i],count=count)
  if(count ne 1) then begin
    message,'file does not exist: '+file[i],/info
    error=1
  endif
endfor
if(error ne 0) then return

; check existence and write privilige of wdir

dum=file_search(wdir,count=count)
if(count ne 1) then begin
  message,'directory does not exist: '+wdir,/info
  return
endif
dum=file_search(wdir,/test_write,count=count)
if(count ne 1) then begin
  message,'no write privilige in directory: '+wdir,/info
  return
endif

; more parameter checks

if((n_elements(yshift) gt 0) and (n_elements(yshift) ne 3)) then begin
  message,'yshift must have three elements if given, [fuv1,fuv2,nuv]',/info
  return
endif
if(n_elements(tmp_size) eq 0) then tmp_size=12.    ; max temporary storage size in GB
d=obj_new('iris_data')
; read first raster or mid-raster
; set up main fits header
mrd_head,file[0],hdr0
tai_ver_rf2=anytim2tai(strmid(sxpar(hdr0,'ver_rf2'),4,10))
tai0_ver_rf2=anytim2tai('2014-05-15')
if(tai_ver_rf2 ge tai0_ver_rf2) then ifile=(nfile-1)/2 else ifile=0

f=file[ifile]
if n_elements(sjifile) ne 0 then f=[f,sjifile]
d->read,f
if(keyword_set(all)) then iwin=indgen(d->getnwin())
if(n_elements(iwin) eq 0) then begin
  print,'Syntax: iris_make_fits_level3,file ,iwin,/sp,sjifile=sjifile,tmp_size=tmp_size,/all,wdir=wdir,/silent'
  print,' iwin is array with window indices or give keyword /all to include all. Available windows:'
  for i=0,(d->getnwin())-1 do begin
    print,string(i,d->getline_id(i),format='(i3," ",a)')
  endfor
  return
endif
nx=d->getnraster(iwin[0]) ; number of raster positions
ny=d->getnslit(iwin[0])   ; number of slit positions

nw=total(d->getxw(iwin))  ; number of wavelength positions
nt=nfile                  ; number of rasters
cvar=d->getline_id(iwin)
bunit=d->getvariableunit() ; unit of intensity
if(n_elements(yshift) ne 0) then begin
  cregion=d->getregion(iwin,/full) ; get region name
  region=intarr(n_elements(iwin))
  for i=0,n_elements(iwin)-1 do region[i]=where(cregion[i] eq ['FUV1','FUV2','NUV'])
endif
neg=0
dx=d->getdx(0)
dy=d->getresy()
dt=d->getinfo('cadex_av')
if(keyword_set(debug)) then stop
if (d->getsit_and_stare(0) eq 1) then begin
  sit_and_stare=1
  dx=dy
  nt=nx
  nx=1
  time=d->gettime(iwin[0])
endif else begin
  if dx eq 0.0 then dx=d->getinfo('CDELT3',1)
  if dx lt 0 then neg=2
  sit_and_stare=0
  time=fltarr(nx,nt)
  time[*,0]=rotate(d->gettime(iwin[0]),neg)
endelse
dw=d->getdispersion(iwin[0])
if n_elements(sjifile) ne 0 then begin
  xys=fltarr(nx,nt,2)
  crpix1=d->getinfo('CRPIX1',/sji)
  crpix2=d->getinfo('CRPIX2',/sji)
  if sit_and_stare then begin
    isji=0                                  ; isji = index of SJI closest in time to start of sit-and-stare
    sltpx1=(d->sji_info()).sltpx1
    iw=where(sltpx1 ne 0.0,count)
    if(count ge 2) then sltpx1_isji=interpol(sltpx1[iw],iw,isji) else sltpx1_isji=sltpx1[isji]
    xys[0,*,0]=sltpx1_isji
    xys[0,*,1]=0.0
  endif else begin
    tsji=d->gettime_sji(/all)
    tref=time[(nx+1)/2,0]
    dum=min(abs(tsji-tref),isji) ; isji = index of SJI closest in time to mid-point of raster
    pztx=d->fixpos(d->getpztx())
    if((tsji[isji] ge time[0,0]-0.5) and (tsji[isji] le time[nx-1,0]+0.5)) then begin
      dum=min(abs(time[*,0]-tsji[isji]),iref) ; iref = index of raster taken at the same time as SJI[isji]
    endif else begin
      pztx_sji=(d->sji_info()).pztx
      dum=min(abs(pztx-pztx_sji[isji]),iref)
    endelse
    sltpx1=(d->sji_info()).sltpx1
    iw=where(sltpx1 ne 0.0,count)
    if(count ge 2) then sltpx1_isji=interpol(sltpx1[iw],iw,isji) else sltpx1_isji=sltpx1[isji]
    xys[*,0,0]=(pztx-pztx[iref])/(d->getinfo('CDELT1',/sji))+sltpx1_isji
    xys[*,0,1]=1.0
  endelse
endif
obsid=d->getobsid(0)
obs_desc=d->getinfo('OBS_DESC')
date_obs=d->getinfo('STARTOBS')
if(keyword_set(debug)) then stop
;
strip=strsplit(f[0],'iris_l2_',/extract,/regex)
nstrip=n_elements(strip)
strip=strip[nstrip-1]
strip=strjoin(strsplit(strip,'raster_',/extract,/regex),'')
strip=strjoin(strsplit(strip,'r.....',/extract,/regex),'')
strip=strsplit(strip,'.fits$',/extract,/regex)
;
if n_elements(iwin) eq d->getnwin() then begin
  lines='all'
endif else begin
  lines=strjoin(strsplit(strjoin(cvar,'_'),/extract),'')
endelse
;
filename_im=wdir+'/iris_l3_'+strip+lines+'_im.fits'
if(~keyword_set(replace)) then begin
  dum=file_search(filename_im,count=count)
  if(count gt 0) then begin
    message,'level3 data already exists, give /replace to replace it',/info
    return
  endif
endif
;
type=(size(1.0))[1]
mkhdr,hdr,type,round([nx,ny,nw,nt]),/extend
; keywords
sxaddpar,hdr,'INSTRUME','IRIS',' Data generated in IRIS format',before='DATE'
sxaddpar,hdr,'DATA_LEV',3.0,' Data level',before='DATE'
sxaddpar,hdr,'LVL_NUM',3.0,' Data level',before='DATE'
sxaddpar,hdr,'VER_RF2',d->getinfo('VER_RF2'),' Version of level2 reformatter',before='DATE'
sxaddpar,hdr,'DATE_RF2',d->getinfo('DATE_RF2'),' Date of level2 reformatting',before='DATE'
sxaddpar,hdr,'VER_RF3',ver_rf3,' Version of iris_make_fits_level3',before='DATE'
sxaddpar,hdr,'OBJECT','Sun',' Type of solar area',before='DATE'
sxaddpar,hdr,'OBSID',obsid,' obsid',before='DATE'
sxaddpar,hdr,'OBS_DESC',obs_desc,' ',before='DATE'
sxaddpar,hdr,'DATE_OBS',date_obs,' ',before='DATE'
sxaddpar,hdr,'STARTOBS',date_obs,' ',before='DATE'
sxaddpar,hdr,'BTYPE','Intensity',before='DATE'
sxaddpar,hdr,'BUNIT',bunit,before='DATE'
sxaddpar,hdr,'CDELT1',abs(dx),' [arcsec] x-coordinate increment',before='DATE'
sxaddpar,hdr,'CDELT2',dy,' [arcsec] y-coordinate increment',before='DATE'
sxaddpar,hdr,'CDELT3',dw,' [AA] wavelength increment',before='DATE'
sxaddpar,hdr,'CDELT4',dt,' [s] t-coordinate axis increment',before='DATE'
if(sit_and_stare) then begin
  sxaddpar,hdr,'CRPIX1',1,' reference pixel x-coordinate',before='DATE'
  crpix1=1
endif else begin
  crpix1=d->getinfo('CRPIX3',iwin[0]+1)
  if(dx lt 0.0) then crpix1=nx-crpix1+1  ; for W->E scan, image is reversed to E->W
  sxaddpar,hdr,'CRPIX1',crpix1,' reference pixel x-coordinate',before='DATE'
endelse
sxaddpar,hdr,'CRPIX2',d->getinfo('CRPIX2',iwin[0]+1),' reference pixel y-coordinate',before='DATE'
sxaddpar,hdr,'CRPIX3',d->getinfo('CRPIX1',iwin[0]+1),' reference pixel lambda-coordinate',before='DATE'
if(sit_and_stare) then begin
  sxaddpar,hdr,'CRPIX4',1,' reference pixel t-coordinate',before='DATE'
  xcen=(d->aux_info()).xcen   ; get x-coordinate as function of time
  xcen=d->fixpos(xcen)        ; repair missing coordinates
  sxaddpar,hdr,'CRVAL1',xcen[0],' [arcsec] Position refpixel x-coordinate',before='DATE'
endif else begin
  sxaddpar,hdr,'CRPIX4',ifile+1,' reference pixel t-coordinate',before='DATE'
  sxaddpar,hdr,'CRVAL1',d->getinfo('CRVAL3',iwin[0]+1),' [arcsec] Position refpixel x-coordinate',before='DATE'
endelse
sxaddpar,hdr,'CRVAL2',d->getinfo('CRVAL2',iwin[0]+1),' [arcsec] Position refpixel y-coordinate',before='DATE'
sxaddpar,hdr,'CRVAL3',d->getinfo('CRVAL1',iwin[0]+1),' [Angstrom] wavelength refpixel  lambda-coordinate',before='DATE'
sxaddpar,hdr,'CRVAL4',(d->gettime(iwin[0]))[crpix1-1],' [s] Time mid-pixel t-coordinate',before='DATE'
sxaddpar,hdr,'CTYPE1','x',' [arcsec]',before='DATE'
sxaddpar,hdr,'CTYPE2','y',' [arcsec]',before='DATE'
sxaddpar,hdr,'CTYPE3','wave',' [Angstrom]',before='DATE'
sxaddpar,hdr,'CTYPE4','time',' [s]',before='DATE'
sxaddpar,hdr,'CUNIT1','arcsec',before='DATE'
sxaddpar,hdr,'CUNIT2','arcsec',before='DATE'
sxaddpar,hdr,'CUNIT3','Angstrom',before='DATE'
sxaddpar,hdr,'CUNIT4','s',before='DATE'
sxaddpar,hdr,'XCEN',d->getinfo('XCEN'),' [arcsec] x-coordinate center of FOV 1 raster',before='DATE'
sxaddpar,hdr,'YCEN',d->getinfo('YCEN'),' [arcsec] y-coordinate center of FOV 1 raster',before='DATE'

; check whether PC values are in the main header (old format) or in
; the extensions (new format)
pc12=d->getinfo('PC3_2')
pc22=d->getinfo('PC2_2')
if((pc12 eq 0) and (pc22 eq 0)) then begin
  iext=iwin[0]+1
  pc12=d->getinfo('PC3_2',iext)
  pc22=d->getinfo('PC2_2',iext)
endif else begin
  iext=0
endelse

; wcs coordinates. x-axis is axis3 in level2, axis1 here, y-axis is axis2 in both cases
; check existence of PC3_3: if present set wcscorr default to 0 and use PC matrix from level2
pc11=d->getinfo('PC3_3',iext)
pc21=d->getinfo('PC2_3',iext)
if((pc11 eq 0) and (pc21 eq 0)) then begin    ; PC3_3 and PC2_3 not populated, use PC2_2 and PC3_2 to calculate
  if(n_elements(wcscorr) eq 0) then wcscorr=1 ; set default wcscorr to 1 if PC values are missing in level2 file
  pc11=d->getinfo('PC2_2',iext)                    ; PC3_3 is not populated in level2, should always be equal to PC2_2
  pc21=-pc12                                  ; PC2_3 is not populated in level2, should always be negative of PC3_2 for dx=dy
endif else begin
  if(n_elements(wcscorr) eq 0) then wcscorr=0 ; set default wcscorr to 0 if PC values are present in level2 file
endelse
if(keyword_set(wcscorr)) then begin
  pc12=pc12*dy/dx
  pc21=pc21*dx/dy
endif
if(dx lt 0.0) then begin
  pc12=-pc12
  pc21=-pc21
endif
sxaddpar,hdr,'PC1_1',pc11,before='DATE'
sxaddpar,hdr,'PC1_2',pc12,before='DATE'
sxaddpar,hdr,'PC2_1',pc21,before='DATE'
sxaddpar,hdr,'PC2_2',pc22,before='DATE'
;
sxaddpar,hdr,'NWIN',n_elements(iwin),' Number of windows concatenated',before='DATE'
istart=0
for i=0,n_elements(iwin)-1 do begin
  ciwin=strtrim(string(iwin[i]+1,format='(i2)'),2)
  sxaddpar,hdr,'WSTART'+ciwin,istart,' Start pixel for subwindow',before='DATE'
  sxaddpar,hdr,'WWIDTH'+ciwin,d->getxw(iwin[i]),' Width of subwindow',before='DATE'
  sxaddpar,hdr,'WDESC'+ciwin,d->getline_id(iwin[i]),' Name of subwindow',before='DATE'
  sxaddpar,hdr,'TWAVE'+ciwin,d->getinfo('TWAVE'+ciwin),' Line center wavelength in subwindow',before='DATE'
  istart=istart+d->getxw(iwin[i])
endfor
;
if(n_elements(yshift) ne 0) then begin
  sxaddpar,hdr,'DYFUV1',round(yshift[0]),' Shift in y from L2 to L3 for FUV1',before='DATE'
  sxaddpar,hdr,'DYFUV2',round(yshift[1]),' Shift in y from L2 to L3 for FUV2',before='DATE'
  sxaddpar,hdr,'DYNUV',round(yshift[2]),' Shift in y from L2 to L3 for NUV',before='DATE'
endif
;
if n_elements(sjifile) ne 0 then begin
  nsji=n_elements(sjifile)
  for i=0,nsji-1 do begin
    cisji=strtrim(string(i+1,format='(i2)'),2)
    sxaddpar,hdr,'SJIFIL'+cisji,sjifile[i],'Slit jaw reference cube filename',before='DATE'
  endfor
endif
;
sxaddpar,hdr,'COMMENT',$
  "Index order is (x,y,lambda,t)",before='DATE'
hist = d->getinfo('HISTORY')
for histindx=0, n_elements(hist)-1 do sxaddpar,hdr,'HISTORY',hist[histindx]
;
endstring=string(' ',format='(a80)')
strput,endstring,'END',0
last_rec=where(hdr eq endstring)
hdr=hdr[0:last_rec]
;
nmax=n_elements(hdr)
endline=nmax-1
nblock=endline*80/2880+1
offset=nblock*2880
openw,luw,filename_im,/get_lun,/swap_if_little_endian
; Convert to byte and force into 80 character lines
bhdr=replicate(32b,80l*nmax)
for n=0l,endline[0] do bhdr[80*n]=byte(hdr[n])
npad=80l*nmax mod 2880
writeu,luw,bhdr
if npad GT 0 then writeu,luw,replicate(32b,2880-npad)

; Write data, then pad
if(sit_and_stare) then begin
  nsiz=float(ny)*float(nw)*float(nt)*4./1024./1024./1024.   ; size of result array in GB
  if(nsiz gt tmp_size) then begin
    nsweep=fix(nsiz/tmp_size)+1
    dk=round(nt/nsweep+1)
  endif else begin
    nsweep=1
    dk=nt-1
  endelse
  k0=0
  k1=(k0+dk) < (nt-1)
  nwi=d->getxw(iwin)  ; nwi[i] is number of wavelength positions in window i
  while(k0 le (nt-1)) do begin
    r=fltarr(ny,nw,k1-k0+1)  ; result array
    for i=0,n_elements(iwin)-1 do begin
      if(i eq 0) then j0=0 else j0=total(nwi[0:i-1])  ; first wavelength index in result array
      j1=total(nwi[0:i])-1                            ; last wavelength index in result array
      r0=transpose((d->getvar(iwin[i],/load,/revnegdx)),[1,0,2])
      r0=r0[*,*,k0:k1]
      if(n_elements(yshift) ne 0) then begin
        ishift=round(yshift[region[0]]) ; make shift integer
        r0=shift(r0,ishift,0,0)                 ; cyclic shift
        if(ishift gt 0) then r0[0:ishift-1,*,*]=-200 else if (ishift lt 0) then r0[ny+ishift:ny-1,*,*]=-200 ; set wrap-around to missing
      endif
      missing=where(r0 lt -198.5,cmissing)
      if(cmissing gt 0) and ((d->getinfo('DATAVALS')) gt 0) then r0[missing]=!values.f_nan
      r[*,j0:j1,*]=r0
    endfor
    sz=size(r)
    message,'writing rasters '+string(k0,' - ',k1,format='(i5,a,i5)')+' size '+string(1,sz[1:2],format='(3(1x,i4))'),/info
    writeu,luw,r
    k0=k1+1
    k1=(k0+dk) < (nt-1)
  endwhile
  d->close
endif else begin
; loop over files ; i.e. over raster
  for ifile=0,nfile-1 do begin
    f=file[ifile]
    if n_elements(sjifile) ne 0 then f=[f,sjifile]
    d->read,f
    r=transpose((d->getvar(iwin[0],/load,/revnegdx)),[2,1,0])
    if(n_elements(yshift) ne 0) then begin
      ishift=round(yshift[region[0]]) ; make shift integer
      r=shift(r,0,ishift,0)                 ; cyclic shift
      if(ishift gt 0) then r[*,0:ishift-1,*]=-200 else if (ishift lt 0) then r[*,ny+ishift:ny-1,*]=-200 ; set wrap-around to missing
    endif
    for i=1,n_elements(iwin)-1 do begin
      r0=transpose((d->getvar(iwin[i],/load,/revnegdx)),[2,1,0])
      if(n_elements(yshift) ne 0) then begin
        ishift=round(yshift[region[i]])  ; make shift integer
        r0=shift(r0,0,ishift,0)                 ; cyclic shift
        if(ishift gt 0) then r0[*,0:ishift-1,*]=-200 else if (ishift lt 0) then r0[*,ny+ishift:ny-1,*]=-200 ; set wrap-around to missing
      endif
      r=[[[r]],[[r0]]]
    endfor
    missing=where(r lt -198.5,cmissing)
    if(cmissing gt 0) then r[missing]=!values.f_nan
    sz=size(r)
    message,'writing raster '+strtrim(string(ifile,format='(i3)'),2)+' size '+string(sz[1:3],format='(3(1x,i4))'),/info
    writeu,luw,float(r)
    time[*,ifile]=rotate(d->gettime(iwin[0]),neg)
    if n_elements(sjifile) ne 0 then begin
      tsji=d->gettime_sji(/all)
      tref=time[(nx+1)/2,ifile]
      dum=min(abs(tsji-tref),isji) ; isji = index of SJI closest in time to mid-point of raster
      pztx=d->fixpos(d->getpztx())
      if((tsji[isji] ge time[0,ifile]-0.5) and (tsji[isji] le time[nx-1,ifile]+0.5)) then begin
        dum=min(abs(time[*,ifile]-tsji[isji]),iref) ; iref = index of raster taken at the same time as SJI[isji]
      endif else begin
        pztx_sji=(d->sji_info()).pztx
        dum=min(abs(pztx-pztx_sji[isji]),iref)
      endelse
      sltpx1=(d->sji_info()).sltpx1
      iw=where(sltpx1 ne 0.0,count)
      if(count ge 2) then sltpx1_isji=interpol(sltpx1[iw],iw,isji) else sltpx1_isji=sltpx1[isji]
      xys[*,ifile,0]=(pztx-pztx[iref])/(d->getinfo('CDELT1',/sji))+sltpx1_isji
      xys[*,ifile,1]=1.0
    endif
    d->close
  endfor
endelse
bitpix=sxpar(hdr,'BITPIX' )
ndata=long64(nx*ny)*long64(nw*nt)
nbytes=long64(ndata)*(abs(bitpix)/8)
npad = 2880 - (nbytes mod 2880)
if npad GT 0 and npad LT 2880 then begin
  bpad=bytarr(npad)
  rec=assoc(luw,bpad,offset+nbytes)
  rec[0]=bpad
endif
;; npad=2880-(nbytes mod 2880)
;; if npad GT 0 then writeu,luw,replicate(0b,npad)
free_lun,luw

; Add lambda and time axes
lambda=d->getlam(iwin[0])
for i=1,n_elements(iwin)-1 do begin
  lambda=[lambda,d->getlam(iwin[i])]
endfor
mkhdr,hdr,lambda,/image
sxaddpar,hdr,'EXTNAME','lambda-coordinate'
sxaddpar,hdr,'BTYPE','lambda axis'
sxaddpar,hdr,'BUNIT','[AA]'
writefits,filename_im,lambda,hdr,/append
;
if (min(time) lt 0) and (nt ge 3) then begin  ; interpolate time for missing frames
  for i=0,nx-1 do begin
    iw=where(time[i,*] ge 0.0,count)
    if(count ge 2) then begin
      ix=indgen(nt)
      time[i,*]=interpol(time[i,iw],iw,ix)
    endif
  endfor
endif

mkhdr,hdr,time,/image
sxaddpar,hdr,'EXTNAME','time-coordinates'
sxaddpar,hdr,'BTYPE','t axes'
sxaddpar,hdr,'BUNIT','[s]'
writefits,filename_im,time,hdr,/append
; And optionally [xs,ys] corrdinates of the slit on the
; slit jaw images
if n_elements(sjifile) ne 0 then begin
  mkhdr,hdr,xys,/image
  sxaddpar,hdr,'EXTNAME','slit-coordinates on sji image'
  sxaddpar,hdr,'BTYPE','xs/ys coordinates'
  sxaddpar,hdr,'BUNIT','[pixel]'
  writefits,filename_im,xys,hdr,/append
endif
;
obj_destroy,d
if keyword_set(sp) then begin
  if(sit_and_stare) or (nfile gt 1) then begin
    br_transpose_fits_level3,filename_im,verbose=verbose,tmp_size=tmp_size
  endif else begin
    if(~keyword_set(silent)) then message,'nt=1, sp cube not made',/info
  endelse
endif
;
end
