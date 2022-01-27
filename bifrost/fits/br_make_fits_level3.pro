;+
; NAME:
;	BR_MAKE_FITS_LEVEL3
;
; PURPOSE:
;	make a crispex type cube for a mhd variable from Bifrost FITS files
;
; CATEGORY:
;	BIFROST
;
; CALLING SEQUENCE:
;	BR_MAKE_FITS_LEVEL3,run_name,snaps,cvar ,iz=iz,/int,/byte,bmin=bmin,bmax=bmax,fbase=fbase,/log,/addt,/verbose
;
; INPUTS:
;	run_name - root name of snap files
;	snaps       - snapshot numbers
;	cvar     - variable name (used by load method)
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;	iz       - z indices to save, default is all
;	int      - scale values between bmin and bmax and store as
;                  2-byte integer
;	byte     - scale values between bmin and bmax and store as
;                  byte
;	bmin      - min value for scaling (defaults to min value of
;                  first snap)
;	bmax      - max value for scaling (defaults to max value of
;                  first snap)
;       log      - take the log of the variable
;	fbase    - output file name base, (defaults to BIFROST_run_name)
;       addt     - add the given snaps to an existing file
;       verbose  - give informational printout
;
; OUTPUTS:
;	writes fits file BIFROST_fbase_cvar_im.fits
;	to be used as input to crispex
;
; OPTIONAL OUTPUTS:
;
;
; COMMON BLOCKS:
;
;
; SIDE EFFECTS:
;
;
; RESTRICTIONS:
;
;
; PROCEDURE:
;
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
;   v1.1  23-Sep-2012 Mats Carlsson - first version
;   v1.2  30-Sep-2012 Mats Carlsson - changed name of some keywords
;   v1.3  31-Oct-2012 Mats Carlsson - changed idlparam to run_name,
;                     added fbase keyword
;   v1.4  15-Mar-2013 Mats Carlsson - added BIFROST to default fbase,
;                     changed it to snaps
;   v1.5  26-Mar-2013 Mats Carlsson - bugfix
;   v1.6  27-Mar-2013 Mats Carlsson - fixed error for /addt
;   v1.7  22-Apr-2013 Mats Carlsson - allows single snapshot to be added
;                     changed "pixel 1" to "ref-pixel" in comment
;   v1.8  27-Nov-2013 Mats Carlsson - now works with iz specified
;   v1.2  09-Oct-2013 juanms        - moved to ssw tree
;   v1.3  2014/01/31 matsc     - implemented verstring printout
;
; $Id$
;-
pro br_make_fits_level3,run_name,snaps,cvar ,iz=iz,int=int,byte=byte,bmin=bmin,bmax=bmax,log=log,fbase=fbase,verbose=verbose,addt=addt,debug=debug
verstring="$Id$"
ic=strpos(verstring,'pro,v')+6
ver=strmid(verstring,ic,strlen(verstring)-ic-6)
if(~keyword_set(silent)) then begin
  message,'version '+ver,/info
endif

if(n_params() lt 3) then begin
  message,'br_make_fits_level3,run_name,snaps,cvar ,iz=iz,/int,/byte,bmin=bmin,bmax=bmax,fbase=fbase,/log,/verbose,/addt,/debug',/info
  return
endif

if(n_elements(fbase) eq 0) then fbase='BIFROST_'+run_name
if(n_elements(snaps) le 1) and (not keyword_set(addt)) then begin
  message,'number of snaps must be greater than one',/info
  return
endif

filename_im=fbase+'_'+cvar+'_im.fits'                       ; output file-name

; read first and second snapshot to get delta-t

nt1=n_elements(snaps)                                       ; number of timesteps to add
for i=0,(1 < (nt1-1)) do begin
  fits_file='BIFROST_'+run_name+'_'+cvar+'_'+br_string3(snaps[i])+'.fits'   ; construct input fits filename
  openr,lur,fits_file,/get_lun,error=error
  if(error ne 0) then begin
    message,'error opening '+fits_file,/info
    return
  endif
  fxhread,lur,hdr_in,status                                 ; read fits-file header
  free_lun,lur
  if(i eq 0) then begin
    hdr=hdr_in                                              ; use first header as basis for header for level3 file
    nx=fxpar(hdr,'NAXIS1')
    ny=fxpar(hdr,'NAXIS2')
    nz=fxpar(hdr,'NAXIS3')
    if(n_elements(iz) eq 0) then iz=indgen(nz) else nz=n_elements(iz)
    if(keyword_set(verbose)) then print,'nx,ny,nz=',nx,ny,nz
    t0=fxpar(hdr,'ELAPSED')
    z=readfits(fits_file,ext=1)
    if(keyword_set(int) or keyword_set(byte)) then begin    ; scaling to be performed
      if(n_elements(bmin) eq 0) or (n_elements(bmax) eq 0) then begin
        data=readfits(fits_file)
        if(n_elements(bmin) eq 0) then bmin=min(data)
        if(n_elements(bmax) eq 0) then bmax=max(data)
      endif
    endif
  endif else begin
    t1=fxpar(hdr_in,'ELAPSED')
    dt=t1-t0                                                ; first delta-t
    if(keyword_set(verbose)) then print,'dt=',dt
  endelse
endfor

if(keyword_set(addt)) then begin                            ; we are to extend an old file
  openr,lur,filename_im,/get_lun,error=error
  if(error ne 0) then begin
    message,'error opening '+filename_im,/info
    return
  endif
  fxhread,lur,hdr,status
  free_lun,lur
  nt0=fxpar(hdr,'NAXIS4')
  t=[readfits(filename_im,ext=2),fltarr(nt1)]
endif else begin
  nt0=0
; construct header
  sxaddpar,hdr,'NAXIS3',nz
  sxaddpar,hdr,'NAXIS',4,' Number of data axes'
  sxaddpar,hdr,'CDELT4',dt,' [s] (non-uniform) t-coordinate axis increment',after='CDELT3'
  sxaddpar,hdr,'CRPIX4',1,' Reference pixel t-coordinate',after='CRPIX3'
  sxaddpar,hdr,'CRVAL4',t0,' [s] Time ref-pixel t-coordinate',after='CRVAL3'
  sxaddpar,hdr,'CTYPE4','time',' [s] Label for t-coordinate',after='CTYPE3'
  sxaddpar,hdr,'CUNIT4','s',' Unit for t-coordinate',after='CUNIT3'
  sxaddpar,hdr,'DATA_LEV',3,' Data level'
  sxaddpar,hdr,'LVL_NUM',3,' Data level',after='DATA_LEV'
  sxdelpar,hdr,'ELAPSED'
  sxdelpar,hdr,'SNAP_NO'
  t=fltarr(nt1)
endelse
if(keyword_set(log)) then begin   ; btype= lg(btype) if /log
  btype=fxpar(hdr,'BTYPE')
  btype='lg('+btype+')'
  sxaddpar,hdr,'BTYPE',btype
endif
nt=nt0+nt1
sxaddpar,hdr,'NAXIS4',nt,after='NAXIS3'
if(keyword_set(int)) then begin
  imax=32767
  imin=-32768
  sxaddpar,hdr,'BITPIX',16
endif
if(keyword_set(byte)) then begin
  imax=0
  imin=255
  sxaddpar,hdr,'BITPIX',8
endif
if(keyword_set(int) or keyword_set(byte)) then begin
  if(keyword_set(log)) then begin
    bmin=alog10(bmin)
    bmax=alog10(bmax)
  endif
  bscale=float(bmax-bmin)/float(imax-imin)
  bzero=float(bmax)-float(imax)*bscale
  sxaddpar,hdr,'BZERO',bzero,' True_value = BZERO + BSCALE*Array_value',after='BTYPE'
  sxaddpar,hdr,'BSCALE',bscale,' True_value = BZERO + BSCALE*Array_value',after='BZERO'
endif
if(not keyword_set(addt)) then sxaddpar,hdr,'COMMENT',$
  "Non-uniform t-coordinate",before='DATE'

; remove hdr lines after 'END'

endstring=string(' ',format='(a80)')
strput,endstring,'END',0
last_rec=where(hdr eq endstring)
hdr=hdr[0:last_rec]

; pad header with blanks

nmax=n_elements(hdr)
nblock=(nmax-1)*80/2880+1
offset=nblock*2880
bitpix=sxpar(hdr,'BITPIX')
ndata=long64(nx*ny)*long64(nz*nt)
nbytes=long64(ndata)*(abs(bitpix)/8)
if(keyword_set(addt)) then begin
  openu,luw,filename_im,/get_lun,/swap_if_little_endian
  ndata0=long64(nx*ny)*long64(nz*nt0)
  nbytes0=long64(ndata0)*(abs(bitpix)/8)                    ; number of already writen data-bytes
endif else begin
  nbytes0=0
  openw,luw,filename_im,/get_lun,/swap_if_little_endian
endelse
; Convert to byte and force into 80 character lines
bhdr=replicate(32b,offset)
for n=0L,nmax-1 do bhdr[80*n:80*n+79]=byte(hdr[n])
writeu,luw,bhdr
if(keyword_set(debug)) then stop

for i=0,nt1-1 do begin
  fits_file='BIFROST_'+run_name+'_'+cvar+'_'+br_string3(snaps[i])+'.fits'   ; construct input fits filename
  var=readfits(fits_file,hdr)
  if(keyword_set(log)) then var=alog10(var)
  if(keyword_set(byte)) then begin                          ; byte scaling to be performed
    var=(var-bzero)/bscale
    var=byte((var > imin) < imax)
  endif else if(keyword_set(int)) then begin                ; int scaling to be performed
    var=(var-bzero)/bscale
    var=round((var > imin) < imax)
  endif
  if(i eq 0) then rec=assoc(luw,var[*,*,iz],offset+nbytes0)
  rec[i]=var[*,*,iz]
  t[i+nt0]=fxpar(hdr,'ELAPSED')
endfor
npad = 2880 - (nbytes mod 2880)
if(npad eq 2880) then npad=0
if npad GT 0 then begin
  bpad=bytarr(npad)
  rec=assoc(luw,bpad,offset+nbytes)
  rec[0]=bpad
endif
if(keyword_set(debug)) then stop
free_lun,luw

; Add z and t axes
mkhdr,hdr,z[iz],/image
sxaddpar,hdr,'EXTNAME','z-coordinate'
sxaddpar,hdr,'BTYPE','z',' Data variable'
sxaddpar,hdr,'BUNIT','Mm',' Unit for z-coordinate'
writefits,filename_im,z[iz],hdr,/append
;
mkhdr,hdr,t,/image
sxaddpar,hdr,'EXTNAME','t-coordinate',' Data variable'
sxaddpar,hdr,'BTYPE','time',' Data variable'
sxaddpar,hdr,'BUNIT','s',' Unit for t-coordinate'
writefits,filename_im,t,hdr,/append

end
