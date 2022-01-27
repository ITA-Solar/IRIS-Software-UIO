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
;	   13-Mar-2013 S. McIntosh - RB-Driven Double Gaussian Fit

pro iris_moment::dgf, data, indx, debug = debug
;
cc = 2.99792e5  ; light speed
missing=-100.

loadct, 39, /silent

; read line window data
  data-> getwin, indx, wd, pos, /load
;  err=data->geterr(indx)
  err= sqrt(wd) ; wd*0.

  badpix=err*0.
  badpix[where(err eq missing)]=-100.
  nwave = data->getxw(indx)
  nspat = data->getyw(indx)
  nexp = data->getnexp(indx)
  nraster = data-> getnraster(indx)
; get wavelength scales
  lambda = data->getlam(indx)
  disp=data->getdispersion(indx)   
  refwvl=data->getline_wvl(indx,wscale='AA')

  vpix = (disp/refwvl)*cc

; moments and related quantities
  intc= fltarr(nspat, nexp)
  int = fltarr(nspat, nexp, 2)
  vel = fltarr(nspat, nexp, 2)
  wid = fltarr(nspat, nexp, 2)
  interr= fltarr(nspat, nexp, 2)
  velerr= fltarr(nspat, nexp, 2)
  rb_mask = bytarr(nspat, nexp)
  rb_image = fltarr(nspat, nexp)

  rblimit = 0.05 ; only looks for DGF when RB/I > RBLIMIT
  nsteps = 20
  steps = 5. + findgen(nsteps)*5. ; R-B Steps in km/s
  rb_int = where(steps ge 30 and steps le 50)
  dv = 5. ; RB is integrated over this value - in km/s
  rb = fltarr(nspat,nexp,nsteps) ; The RB diagnostic!

 ; MPFit Driven Double Gaussian Fit - Using R-B Analysis
  if keyword_set(debug) then begin
    window, 0, xs = 400, ys = 300
    window, 1, xs = 400, ys = 300
    window, 2, xs = 400, ys = 300
  endif

  for i = 0, nexp-1 do begin
    for j = 0, nspat-1 do begin

       profile = reform(wd[*,j,i])
       error = reform(err[*,j,i])

       ; first the MPFIT Single Fit.
       tmp = mpfitpeak(lambda, profile, sfit, terms = 4, /positive, /double, $
		measure_error = sqrt(profile), perror = perr)
		if N_ELEMENTS(perr) eq 0 then continue

       intc[j,i] = sfit[3]
       int[j,i,0] = sfit[0]
       ;if N_ELEMENTS(perr) gt 0 then $
       interr[j,i,0] = perr[0]
       vel[j,i,0] = cc * (sfit[1] - refwvl)/refwvl
       ;if N_ELEMENTS(perr) gt 1 then $
       velerr[j,i,0] = (perr[1] / disp) * (disp/refwvl) * cc
       wid[j,i,0] = sqrt(sfit[2])*disp*1000.*2.35 ; m�
       rb_mask[j,i] = 1

       ; Find the Doppler Shift
       v = cc * (sfit[1] - refwvl)/refwvl

       if keyword_set(debug) then begin
         wset, 0
         plot, cc * (sfit[1] - lambda)/refwvl, profile, psym = 5, xr= 50.*[-1,1], xst = 1
         oplot, cc * (sfit[1] - lambda)/refwvl, tmp
         empty
       endif

       ; increase spectral resolution by factor of 10!
       new_profile = interpol(profile,nwave*10)
       new_lambda = interpol(lambda,nwave*10)
       new_error = interpol(error,nwave*10)
       new_vel = ((new_lambda - sfit[1])/disp)*vpix

       bpp = where(new_profile gt 0., bppc)

       ; Compute the RB Profile.
       rbstr = iris_gen_rb_profile(new_vel, new_profile, steps, dv)
       rbs = (rbstr.red - rbstr.blue)
       rb[j,i,*] = rbs
       ; now we use these values as input for the DGF.
       rbs = smooth(rbs/sfit[0], 3, /edge_truncate)
       bad = where(finite(rbs) eq 0, brc)
       if brc ne 0 then goto, skip
       m = round(zerocrossings(rbs, n_cross = nc))
       if n_elements(m) eq 1 then m = [m[0], nsteps-1]

       tmpx = steps[m[0]:m[1]]
       tmpy = rbs[m[0]:m[1]]

       if (nc ne 0) and (max(m) lt nsteps-1) and (n_elements(tmpx) ge 4) then begin

	  gg = mpfitpeak(tmpx, tmpy, /double, aa, nterms = 4)
	
	  if abs(aa[0]) ge rblimit then begin

             gg = mpfitpeak(tmpx, tmpy, /double, aa, nterms = 4)

	     rbmax = max(rbs > 0.)
             rbmin = min(rbs < 0.)
	     if ((rbmax - rbmin) ge 2*rblimit) then sfit[1] = sfit[1] + 0.05
		     
	     mindex = (aa[1]/vpix) * disp ; in �
	     rbmax = aa[0]
  	     cp = sfit[1]
             if rbmax gt 0 then wp = double(cp + mindex) ;(mindex/vpix)*disp)
             if rbmax lt 0 then wp = double(cp - mindex) ;(mindex/vpix)*disp)
             wp = double(wp)

             if keyword_set(debug) then begin
	       wset, 1
               plot, tmpx, tmpy, psym = 5, yr = [-0.2, 0.2], yst = 1, xr = minmax(steps), xst = 1
	       oplot, steps, rbs, lines = 3, color = 128 
	       plots, !x.crange, [0.,0.], lines = 2
	       plots, !x.crange, rblimit*[1,1], lines = 3
	       plots, !x.crange, rblimit*[-1,-1], lines = 3
  	       oplot, tmpx, gg
               empty
	     endif

	     fit0 = double([sfit[3], sfit[0], cp, sqrt(2*alog(2))*sfit[2]])
	     result = iris_mp_dgf(new_lambda, new_profile, new_error, $
				fit0, wp, double(abs(rbmax)), disp, bpp, /double)

             b0 = result.b
             i0 = result.i1
             x0 = result.p1
             w0 = result.w1
             i1 = result.i2
             x1 = result.p2
             w1 = result.w2

             if keyword_set(debug) then begin
	       fit1 = i0*exp(-((new_lambda - x0)/w0)^2)
               fit2 = i1*exp(-((new_lambda - x1)/w1)^2)
               fit = b0 + fit1 + fit2

	       wset, 2
	       plot, lambda, profile, psym = 5, xr = refwvl + 0.2*[-1,1]
	       oplot, new_lambda, b0 + fit1, color = 50
	       oplot, new_lambda, b0 + fit2, color = 250	 
	       oplot, new_lambda, fit, lines = 3
  	       str = '' & read, str
	     endif

	    intc[j,i] = b0
            int[j,i,*] = [i0, i1]
            ;if N_ELEMENTS(result.error) gt 3 then $
            interr[j,i,*] = [result.error[1],result.error[3]] 
            vel[j,i,*] = cc * ([x0, x1] - refwvl)/refwvl
            ;if N_ELEMENTS(result.error) gt 5 then $
            velerr[j,i,*] = ([result.error[2],result.error[5]] / disp) * (disp/refwvl) * cc
            wid[j,i,*] = sqrt([result.w1, result.w2])*disp*1000.*2.35 ; m�
	    rb_mask[j,i] = 2
	    rb_image[j, i] = total(rbs[rb_int])

	  endif
       endif
       skip:
    endfor ; j
  endfor ; i

  ;stop

; Now put everything in place in the w array

  (*self.w[indx])[0,*,*] = int[*,*,0]
  (*self.w[indx])[1,*,*] = vel[*,*,0] 
  (*self.w[indx])[2,*,*,*] = wid[*,*,0] 
  (*self.w[indx])[3,*,*,*] = interr[*,*,0]
  (*self.w[indx])[4,*,*,*] = velerr[*,*,0]
  (*self.w[indx])[5,*,*] = int[*,*,1]
  (*self.w[indx])[6,*,*] = vel[*,*,1] 
  (*self.w[indx])[7,*,*,*] = wid[*,*,1] 
  (*self.w[indx])[8,*,*,*] = interr[*,*,1]
  (*self.w[indx])[9,*,*,*] = velerr[*,*,1]
  ; continuum
  (*self.w[indx])[10,*,*] = intc
  ; num components
  (*self.w[indx])[11,*,*] = rb_mask
  ; integrated rbimage
  (*self.w[indx])[12,*,*] = rb_image
  self->setvariablename,['I(1)', 'V(1)','W(1)', 'Ierr(1)', 'Verr(1)', $
                         'I(2)', 'V(2)','W(2)', 'Ierr(2)', 'Verr(2)', $
                         'I(cont)','RB mask','RB image']

  return

  nofile:
  ok = dialog_message('No pixel information file')
  return
end
