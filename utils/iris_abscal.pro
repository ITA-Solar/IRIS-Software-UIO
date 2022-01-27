function iris_abscal,dnps0,wave0 ,date,fac=fac,version0=version0,verbose=verbose,cgs=cgs,debug=debug
;+
;   tr=iris_abscal(dnps,wave,/debug)
;
;            produce intensity from DN/s/pixel
;            dnps should be compensated for any binning outside this routine
;            date is in anytim format (not needed if version != '003')
;            The wavelength dimension should be the last dimension of dnps
;            wavelength in A or nm
;            version='24' takes version='002' and multiplies result
;            with 1.6/1.23 (from ITN24). This is now the default
;-
if(n_params() lt 2) then begin
  message,'intensity=iris_abscal(dnps,wave ,date,fac=fac,version=version,/cgs,/verbose,/debug)',/info
  return,0
endif
if(n_elements(version0) eq 0) then version0='24'
if(fix(version0) eq 3) and (n_elements(date) eq 0) then begin
  message,"for version='003' you need to provide date as third argument",/info
  return,0
endif

if(fix(version0) eq 24) then begin  ; set version
  version='002'
endif else begin
  version=version0
endelse

siz=size(dnps0)
if(siz[0] eq 0) then dnps=[dnps0] else dnps=dnps0  ; make array
siz=size(wave0)
if(siz[0] eq 0) then wave=[wave0] else wave=wave0  ; make array
nwave=n_elements(wave)
siz=size(dnps)
ndim=siz[0]
nwave_dnps=siz[ndim]
nelements_dnps=siz[ndim+2]
if(nwave_dnps ne nwave) then begin
  message,'dimensions do not match: dnps should have wavelength as the last index',/info
  if(keyword_set(debug)) then stop
  return,0
endif

if(mean(wave) lt 1000.) then xl_nm=wave else xl_nm=wave*0.1
xl_a=xl_nm*10.
iresp=iris_get_response(date,version=version)
fac=fltarr(nwave)              ; radiometric calibration going from DN/s to erg/cm^2/s/ster/A
area=fltarr(nwave)
dn2phot=fltarr(nwave)
pix_xl=fltarr(nwave)
hh=6.626176E-27
cc=2.99792458E+10

iw=where(xl_nm lt 200.,count)   ; FUV
if(count gt 0) then begin
  area[iw]=interpol(iresp.area_sg[*,0],iresp.lambda,xl_nm[iw])
  if(version eq '003') then dn2phot[iw]=iresp.dn2phot_sg[0] else dn2phot[iw]=4.
  pix_xl[iw]=0.01298
endif
if(fix(version0) eq 24) then begin
  iw=where(xl_nm lt 136.,count) ; FUVS
  if(count gt 0) then begin
    area[iw]=area[iw]*1.23/1.6  ; scale result if version='24'
  endif
endif

iw=where(xl_nm gt 200.,count)  ; NUV
if(count gt 0) then begin
  area[iw]=interpol(iresp.area_sg[*,1],iresp.lambda,xl_nm[iw])
  if(version eq '003') then dn2phot[iw]=iresp.dn2phot_sg[1] else dn2phot[iw]=18.
  pix_xl[iw]=0.02546
endif
e_xl=hh*cc*1.e7/xl_nm
pix_xy=!pi/(180.*3600.*6.)
w_slit=!pi/(180.*3600.*3.)
fac=e_xl*dn2phot/area/pix_xy/pix_xl/w_slit
if(nwave eq 1) and (keyword_set(verbose)) then begin
  print,'fac=',fac
  print,'area=',area
endif
im_all=reform(dnps,nelements_dnps/nwave_dnps,nwave) ; im*fac gives erg/cm^2/s/sr/A
for i=0,nwave-1 do begin
  im=im_all[*,i]
;  iw=where(im ne im,count)
;  if(count gt 0) then im[iw]=-200
  iw=where(im eq im,count)
  if(count gt 0) then begin
    im[iw]=im[iw]*fac[i]                ; I_lambda [erg/cm^2/s/sr/A]
    im[iw]=im[iw]*xl_a[i]*xl_a[i]/3.e18  ; convert to I_nu,  I_lambda[A-1]=I_nu[Hz-1]*3e18/xl/xl
    if(~keyword_set(cgs)) then im[iw]=im[iw]*1.e-3  ; convert to W/m^2/sr/Hz
    im_all[*,i]=im     ; store result in place of input in im_all array
  endif
endfor
im_all=reform(im_all,siz[1:ndim])  ; reform im_all array to original dimensions of dnps
if(keyword_set(debug)) then stop
return,im_all

end

