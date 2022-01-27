;   MODIFICATION HISTORY
;          18-Mar-2008 A. Gardini - Changed the column's order of moments.
;           4-Jul-2008 A. Gardini - Added the init keyword.
;          11-Jul-2008 A. Gardini - Added group_leader in xmoment's call.
;          19-Sep-2008 A. Gardini - Added the error of intensity
;          23-Sep-2008 A. Gardini - Added the error of velocity
;          12-Mar-2009 V. Hansteen - Added possibility of setting
;                                    aeffrelerr.
;          31-Feb-2013 V. Hansteen - First IRIS version :-)
;	   13-Mar-2013 S. McIntosh - Single Gaussian Fit

pro xgaussian_debug_destroy, event
  widget_control, event.top, /destroy
end

pro xgaussian_debug_draw, event
  widget_control, event.top, get_uvalue = info
  wset,(*info).window_id
  data=(*info).data
  plot,(*info).lambda,data,xmargin=[10,2],ymargin=[4,2]
  oplot,(*info).lambda,(*info).fit,line=2
  refwvl=(*info).refwvl
  oplot,[refwvl,refwvl],[min(data),max(data)],line=1
end

pro iris_moment::gaussian, data, indx
;
  cc = 2.99792e5  ; light speed
  missing=data->missing()

; read line window data
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
  disp=data->getdispersion(indx)   
  refwvl=data->getline_wvl(indx,wscale='AA')
; Launch widget tool to mark line and continuum pixels
  if self.line_def then begin
    file=self->appreadme()+path_sep()+'line_def_gauss_'+strtrim(string(indx), 2)+'.tmp'
    on_ioerror, nofile
    restore, file
  endif else begin
    mspec=iris_mean_spec(wd,missing=data->missing())
    self.mspec[indx]=ptr_new(mspec)
    xmoment_gauss, data, mspec, indx, lambda, groupl = self.group_leader
    if not(obj_valid(data)) then return ; if the xcontrol window closes
    line_px = (data->getwd_def())[indx].line_px
    cont_px = (data->getwd_def())[indx].cont_px
    refwvl=data->getline_wvl(indx,wscale='AA')
    save, line_px, cont_px, refwvl, $
      file=self->appreadme()+path_sep()+'line_def_gauss_'+strtrim(string(indx), 2)+'.tmp'
  endelse

; moments and related quantities
  intc= fltarr(nspat, nexp)
  int = fltarr(nspat, nexp)
  vel = fltarr(nspat, nexp)
  wid = fltarr(nspat, nexp)
  interr= fltarr(nspat, nexp)
  velerr= fltarr(nspat, nexp)
;
  ;; if self->getdebug() then begin
  ;;   tlb = widget_base(title = 'IRIS gaussian debug :'+self->getline_id(indx),/row,xoffset=100,yoffset=100)
  ;;   lcol = widget_base(tlb, /frame, /column)
  ;;   rcol = widget_base(tlb, /column)
  ;;   displaybase = widget_base(rcol, /row)
  ;;   drawbase = widget_draw(displaybase,retain=2,xsize=200,ysize=200)
  ;;   closefield = widget_base(lcol,/column)
  ;;   closebutton = widget_button(closefield, value = 'Close', $
  ;;                               event_pro = 'xgaussian_debug_destroy')
  ;;   widget_control, tlb, /realize, tlb_get_size=tlb_sz

  ;;   widget_control, drawbase, get_value = window_id
  ;;   wset, window_id

  ;;   info = {tlb:tlb,window_id:window_id,data:wd[*,0,0],fit:wd[*,0,0],lambda:lambda,refwvl:refwvl}
  ;;   info = ptr_new(info, /no_copy)
  ;;   widget_control, tlb, set_uvalue = info

  ;;   xmanager, 'xgaussian_debug', tlb, /no_block
  ;; endif

  lam=lambda[line_px[0]:line_px[1]]
  window,/free,xs=250,ys=250
  for i = 0, nexp-1 do begin
    twd=wd[*,*,i]
    if (where(finite(twd) eq 0))[0] ne -1 then begin
      twd[where(finite(twd) eq 0)]=mean(twd,/nan)
    endif
    for j = 0, nspat-1 do begin
       ; MPFit Single Gaussian Fit
       tmp = mpfitpeak(lam, twd[line_px[0]:line_px[1],j], aa, terms = 4, /positive, /double, $
		measure_error = err[line_px[0]:line_px[1],j,i], perror = perr,/nan)
       if j eq nspat/2 then tmp1=tmp
       intc[j,i] = aa[3]
       int[j,i] = aa[0]
       interr[j,i] = perr[0]
       vel[j,i] = cc * (aa[1] - refwvl)/refwvl
       velerr[j,i] = (perr[1] / disp) * (disp/refwvl) * cc
       wid[j,i] = aa[2]; * (alog(2)/disp) * (disp/refwvl) * cc ; km/s
       if j mod 50 eq 0 then begin
         plot,lam,twd[line_px[0]:line_px[1],j], $
           title='i = '+string3(i)+'/'+string3(nexp)+' j = '+string3(j)+'/'+string3(nspat)
         oplot,lam,tmp,line=2
         wait,0.1
       endif

 ;      print, aa
       ;; if self->getdebug() and xregistered('xgaussian_debug',/noshow) ne 0 and j eq fix(nspat/2.) then begin
       ;;   (*info).data=twd[*,j]
       ;;   (*info).fit=tmp
       ;;   pseudoevent={widget_button,id:0l, $
       ;;         top:tlb, handler:0l, select:1}
       ;;   xgaussian_debug_draw,pseudoevent
       ;;   wait,0.1
       ;; endif
    endfor ; j
  endfor ; i
  wdelete,!d.window
  if ptr_valid(info) then ptr_free,info
;;  widget_control,tlb,/destroy
;
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
