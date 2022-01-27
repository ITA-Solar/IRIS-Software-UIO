;   MODIFICATION HISTORY
;          18-Mar-2008 A. Gardini - Changed the column's order of moments.
;           4-Jul-2008 A. Gardini - Added the init keyword.
;          11-Jul-2008 A. Gardini - Added group_leader in xmoment's call.
;          19-Sep-2008 A. Gardini - Added the error of intensity
;          23-Sep-2008 A. Gardini - Added the error of velocity
;          12-Mar-2009 V. Hansteen - Added possibility of setting
;                                    aeffrelerr.
;          31-Feb-2013 V. Hansteen - First IRIS version
pro iris_moment::moment, data, indx
;
  cc = 2.99792e5  ; light speed
  missing=data->missing()
;
; read line window data
;
  wd=data->getvar(indx,/load)
  good=finite(wd)
  if (where(good))[0] eq -1 then begin
    message,'All data is NaN! Return',/info
    return
  endif
  bad = where(good eq 0,nbad)
;  if nbad ne 0 then wd[bad]=missing
;  err=data->geterr(indx)
;  badpix=wd*0.
;  badpix[where(wd eq missing)]=missing
  err= sqrt(wd>0.) ; kluge since the pedestal is incorrect and FUV data < 0.
;
  nwave = data->getxw(indx)
  nspat = data->getyw(indx)
  nexp = data->getnexp(indx)
  nraster = data-> getnraster(indx)
; get wavelength scales
  lambda = data->getlam(indx)
; set "laboratory wavelength"
  refwvl=data->getline_wvl(indx,wscale='AA')
; Launch widget tool to mark line and continuum pixels
  if self.line_def then begin
    file=self->appreadme()+path_sep()+'line_def_'+strtrim(string(indx), 2)+'.tmp'
    on_ioerror, nofile
    restore, file
  endif else begin
    mspec=iris_mean_spec(wd,missing=data->missing())
    self.mspec[indx]=ptr_new(mspec)
    xmoment_moment, data, mspec, indx, lambda, /wlref, groupl = self.group_leader
    if not(obj_valid(data)) then return ; if the xcontrol window closes
    line_px = (data->getwd_def())[indx].line_px
    cont_px = (data->getwd_def())[indx].cont_px
    refwvl=data->getline_wvl(indx,wscale='AA')
    save, line_px, cont_px, refwvl, $
      file=self->appreadme()+path_sep()+'line_def_'+strtrim(string(indx), 2)+'.tmp'
  endelse
  lpx = indgen(line_px[1]-line_px[0]+1)+line_px[0]
  cpx = indgen(cont_px[1]-cont_px[0]+1)+cont_px[0]
  nlpx= float(n_elements(lpx))
  ncpx= float(n_elements(cpx))
  sum = fltarr(nwave)
  sumc= fltarr(nwave)
  sum[lpx] = 1.
  sumc[cpx] = 1.
; set up of moment array completed, now calculate moments
; -------------------------------------------------------
  pix = fltarr(nwave) 
  pix[lpx] = float(lpx)
  pix2= fltarr(nwave) 
  pix2[lpx] = float(lpx)*float(lpx)
;
  lam = fltarr(nwave) 
  lam[lpx] = float(lambda[lpx])
  lambda1 = lambda[line_px[0]]
  disp=data->getdispersion(indx)
; moments and related quantities
  intc= fltarr(nspat, nexp)
  int = fltarr(nspat, nexp)
  bad = fltarr(nspat, nexp)
  vel = fltarr(nspat, nexp)
  wid = fltarr(nspat, nexp)
  wdl = fltarr(nwave, nspat, nexp) 
  mu  = fltarr(nspat, nexp) 
  interr= fltarr(nspat, nexp)
  intcerr= fltarr(nspat, nexp)
  err2= fltarr(nwave, nspat, nexp) 
  err2=err*err 
;  err2[where(err eq missing)]=0.
  velb = fltarr(nspat, nexp)
  lambdac = fltarr(nspat, nexp)
  velerrfac = fltarr(nwave, nspat, nexp)
  velerr= fltarr(nspat, nexp)
  for i = 0, nexp-1 do begin
    twd=wd[*,*,i]
    if (where(finite(twd) eq 0))[0] ne -1 then begin
      twd[where(finite(twd) eq 0)]=mean(twd,/nan)
    endif
;    
    intc[*, i]= reform(sumc#twd)/ncpx ; specif. intensity of the cont.
    intcerr[*, i] = reform(sumc#err2[*, *, i])
    for k = 0, nwave-1 do $
; specif. intens. continuum subtracted
      wdl[k,*,i]= twd[k,*]-intc[*,i] > 0.
; 0th order moment
    int[*, i] = reform(sum#wdl[*, *, i])
;    interr[*, i] = reform(sum#err2[*, *, i])
; 1st order moment
    mu[*, i] = reform(pix#wdl[*, *, i])/int[*, i] 
    vel[*,i] = (lambda1+(mu[*,i]-line_px[0])*disp - refwvl)/refwvl*cc ;AG
    lambdac[*, i] = reform(lam#wdl[*, *, i])/int[*, i] 
    velb[*,i] = (lambdac[*,i] - refwvl)/refwvl*cc ;AG
; 2nd order moment
    wid[*, i]= reform(pix2#wdl[*,*,i])/int[*,i]-mu[*,i]*mu[*,i]
;    bad[*, i] = total(badpix[lpx,*,i],1)+total(badpix[cpx,*,i],1)
  endfor
;; ; Computation of the velocity errors
;;   for i = 0, nexp-1 do $
;;     for k = lpx[0], lpx[nlpx-1] do $
;;       for j = 0, nspat-1 do $
;;         velerrfac[k,j,i]= lam[k]/int[j,i]*(1.-lambdac[j,i]/lam[k])
;;   velerrfac=velerrfac*err
;;   velerrfac=velerrfac^2
;;   for i = 0, nexp-1 do $
;;     velerr[*, i] = reform(sum#velerrfac[*,*,i])
;;   velerr = sqrt(velerr)
;;   velerr = velerr/refwvl*cc ;AG
;; ; Computation of the intensity errors (square root of the sum of the squares)
;;   gain=*self.cal->getgain()
;;   gainerr=*self.cal->getgainerr()
;;   aeffrelerr=*self.cal->getaeffrelerr()
;;   interr=sqrt(interr+intcerr*(float(nlpx)/float(ncpx))^2)
;;   interr=int*sqrt((interr/int)^2+(gainerr/gain)^2+aeffrelerr^2)
; Final scalings:
  int=int*disp
;  interr=interr*disp

; Final computation of the line width: the factor 2.35 is introduced to get 
; the FWHM ( = 2*sqrt(2ln2) * sigma for a Gaussian distribution).
  wid = sqrt(wid)*disp*1000.*2.35
; Now put everything in place in the w array
  (*self.w[indx])[0,*,*] = int
  (*self.w[indx])[1,*,*] = vel 
  (*self.w[indx])[2,*,*] = wid 
  (*self.w[indx])[3,*,*] = interr
  (*self.w[indx])[4,*,*] = velerr
  (*self.w[indx])[5,*,*] = intc
  self->setvariablename,['Int', 'Vel','Wid', 'Ierr', 'Verr', 'I(cont)']
  return

  nofile:
  ok = dialog_message('No pixel information file')
  return
end
