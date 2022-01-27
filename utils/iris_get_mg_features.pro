;+
; NAME:
;	IRIS_GET_MG_FEATURES
;
; PURPOSE:
;	Extract positions of Mg II spectral features from IRIS NUV spectra.
;       The algorithm is detailed in:
;       Pereira, T. M. D. , Leenarts, J., De Pontieu, B., Carlsson, M.,
;       Uitenbroek, H., 2013, ApJ, 778, 143, http://arxiv.org/abs/1310.1926
;
; CATEGORY:
;	IRIS_ANALYSYS
;
; CALLING SEQUENCE:
;	IRIS_GET_MG_FEATURES, spec, wave, vrange, lc, rp, bp, /onlyh, /onlyk
;
; INPUTS:
;       wave:   1-D array of wavelength values (nm in vacuum). Wavelengths
;               should be increasing with index.
;       spec:   2-D array (wavelength, spatial position) of spectra
;       vrange: array with two elements: starting and ending velocities
;               to define velocity range (from line centre) where to find
;               the spectral features. In km/s. Recommended so far: [-40, 40]
;
; KEYWORD PARAMETERS:
;       onlyk:  if set, will only calculate properties for the Mg II k line.
;       onlyh:  if set, will only calculate properties for the Mg II h line.
;               Obviously, these are mutually exclusive.
;
; OUTPUTS:
;       lc:     3-D array (line, feature, spatial position) with line centre
;               positions. Line is defined by wave_ref (typically Mg II k first
;               and then Mg II h), and feature is velocity shift (first index,
;               in km/s from rest wavelength) or spectral intensity (second
;               index, same units as spec).
;       bp:     3-D array (line, feature, spatial position) with blue peak
;               positions. Same structure and units as lc.
;       rp:     3-D array (line, feature, spatial position) with red peak
;               positions. Same structure and units as lc.
;
; MODIFICATION HISTORY:
;    Written: Tiago M. D. Pereira, UiO, July 2013.
;    22-10-2013: Tiago M. D. Pereira (added new keywords, other minor changes)
;    01-11-2013: Minor bug fixes. --Tiago
;    20-03-2014: Added paper reference. --Tiago
;
;-
pro peakdetect_lcl, y_axis, x_axis, ma, mi, lookahead=lookahead, sorted=sorted
;   Wrapper to lclxtrem
    maxima = lclxtrem(y_axis, lookahead, /maxima)
    minima = lclxtrem(y_axis, lookahead)
    if keyword_set(sorted) then begin
        maxima = maxima[sort(maxima)]
        minima = minima[sort(minima)]
    endif
    ma = [[x_axis[maxima]], [y_axis[maxima]]]
    mi = [[x_axis[minima]], [y_axis[minima]]]
end

function gaussian_kernel, sd
    ; Calculates a normalised Gaussian kernel for a given standard deviation
    lw = fix(4.0 * sd + 0.5)
    kernel = fltarr(2 * lw + 1)
    kernel[lw] = 1.0
    sum = 1.0
    sd = sd * sd
    ; calculate the kernel:
    for ii = 1, lw do begin
        tmp = exp(-0.5 * float(ii * ii) / sd)
        kernel[lw + ii] = tmp
        kernel[lw - ii] = tmp
    endfor
    return, kernel / total(kernel)
end

function mg_single, vel, spec, guess, use_deriv=use_deriv, $
                    force_guess=force_guess
    ; Calculates position and intensity for k3/h3 peaks
    ;guess = 5      ; initial guess for line centre (in km/s)
    pts_min = 15    ; number of points to find the minimum around guess of lc
    pfit = 3        ; number of points around minimum to fit parabola
    wdiff_max = 4   ; max number of points away from minimum for acceptable fit
    margin = 15     ; for derivative method: number of points to calculate der.
    lookahead = 10  ; for peak finding, peaks separated by less are not used
    deriv_flag = 0
    nsp = n_elements(spec) - 1
    ; Get peaks
    peakdetect_lcl, spec, vel, pmax, pmin, lookahead=lookahead, /sorted
    ; Decide which minimum is line centre, if any are found
    if not keyword_set(force_guess) then begin
        if (n_elements(pmax) gt 0) and (n_elements(pmin) gt 0) then begin
            ; get rid of peaks within 10 km/s of the edges
            idx_filt = where((pmax[*, 0] gt vel[0] + 10) and $
                             (pmax[*, 0] lt vel[nsp] - 10.), count)
            if count eq 0 then begin
                pmax = [0]
                lpmax = 0
            endif else begin
                pmax = pmax[idx_filt, *]
                lpmax = n_elements(pmax[*, 0])
            endelse
            idx_filt = where((pmin[*, 0] gt vel[0] + 10) and $
                             (pmin[*, 0] lt vel[nsp] - 10.), count)
            if count eq 0 then begin
                pmin = [0]
                lpmin = 0
            endif else begin
                pmin = pmin[idx_filt, *]
                lpmin = n_elements(pmin[*, 0])
            endelse            
            ; string with number of minima and maxima
            lptag = strtrim(string(lpmin), 2) + strtrim(string(lpmax), 2)
            case 1 of
                where(['12', '31', '32', '34', '54', '76'] eq lptag) ge 0: begin
                    ; most straightforward case: take the middle minimum
                    guess = pmin[lpmin / 2, 0]
                    imin = pmin[lpmin / 2, 1]
                    pts_min = pts_min / 2
                end
                where(['22', '23', '33', '42', '43', '44'] eq lptag) ge 0: begin
                    ; take the lowest minimum between the two largest maxima
                    ; find two largest maxima
                    tmp = sort(pmax[*, 1])
                    ntmp = n_elements(tmp)  ; this was previously above the tmp=sort() line
                    tmp = pmax[tmp[ntmp - 2:*], 0]
                    tmp = tmp[sort(tmp)]
                    ; minima inside the above window
                    idx = where((pmin[*, 0] gt tmp[0]) and $
                                (pmin[*, 0] lt tmp[1]), count)
                    if count gt 0 then begin
                        ; get lowest minimum
                        xx = min(pmin[idx, 1], mmin)
                        tmp = pmin[idx, 0]
                        tmpi = pmin[idx, 0]
                        guess = tmp[mmin]
                        imin = tmpi[mmin]
                    endif
                end
                lptag eq '21': begin
                    ; Use lowest minimum if much weaker
                    if max(pmin[*, 1]) / min(pmin[*, 1]) gt 1.3 then begin
                        xx = min(pmin[*, 1], mmin)
                        guess = pmin[mmin, 0]
                        imin = pmin[mmin, 1]
                        pts_min = pts_min / 2
                    endif else deriv_flag = 1
                end
                lpmax eq 1: begin
                    ; Candidate for deriv method
                    deriv_flag = 1
                end
                lpmin eq 1: begin
                    ; Just take single minimum
                    guess = pmin[0, 0]
                    imin = pmin[0, 1]
                end
                else: break
            endcase
        endif else if (n_elements(pmax) gt 0) then begin   ; no minima
            lpmax = n_elements(pmax[*, 0])
            if lpmax eq 1 then deriv_flag = 1
        endif
    endif else begin
        ; Set deriv_flag if only one maximum within 20 km/s
        if (n_elements(pmax) gt 0) then begin
            idx_filt = where((pmax[*, 0] gt vel[0] + 20) and $
                              (pmax[*, 0] lt vel[nsp] - 20.), count)
            if count eq 1 then deriv_flag = 1   
        ;    pmax = pmax[idx_filt, *]
        endif
        ;if (n_elements(pmax) gt 0) then begin
        ;    if n_elements(pmax[*, 0] eq 1) then deriv_flag = 1
        ;endif
    endelse

    if deriv_flag eq 1 then begin   ; Derivative method for blended peaks
        if not keyword_set(use_deriv) then $
            return, [!VALUES.F_NAN, !VALUES.F_NAN]
        dd = reform(abs(spec[1:*] - spec[0:nsp]))
        ; define the boundaries to inspect for peak asymmetry and derivative
        incr = fix(15. / (vel[1] - vel[0]))
        xx = min(abs(vel - pmax[0, 0]), idxm)
        vv = [max([0, idxm - incr]), min([idxm + incr, nsp])]
        ;print, pmax[0, 0], idxm, incr, vv
        if spec[vv[0]] gt spec[vv[1]] then begin
            if (idxm - margin - 1 lt vv[0] + margin) then $
                return, [!VALUES.F_NAN, !VALUES.F_NAN]
            der = dd[vv[0] + margin:idxm - margin - 1]
            xx = min(der, dermin)
            pidx = vv[0] + dermin + margin + 1
        endif else begin
             if (vv[1] - margin - 1 lt idxm + margin) then $
                return, [!VALUES.F_NAN, !VALUES.F_NAN]
            der = dd[idxm + margin:vv[1] - margin - 1]
            xx = min(der, dermin)
            pidx = idxm + dermin + margin + 1
        endelse
        lc = vel[pidx]
        lc_int = spec[pidx]
    endif else begin                ; Most common approach, do parabolic fit
        ; Approximate index of guess and of spectral minimum around it
        xx = min(abs(vel - guess), idg)
        xx = min(spec[max([0, idg - pts_min]): $
                 min([idg + pts_min - 1, nsp])], isp)
        ini = max([0, isp + idg - pts_min])
        ; if no points, return masked
        wi = max([0, ini - pfit])
        wf = min([ini + pfit, nsp])
        if n_elements(vel[wi:wf]) lt 2 then $
            return, [!VALUES.F_NAN, !VALUES.F_NAN]
        fit = poly_fit(vel[wi:wf], spec[wi:wf], 2)
        ; Convert poly to parabola coefficients
        lc = -fit[1] / (2. * fit[2])
        lc_int = fit[0] - fit[2] * lc^2
        ; If fitted minimum is further than wdiff_max points, mask result
        if abs(lc - vel[ini]) gt wdiff_max * (vel[1] - vel[0]) then $
            return, [!VALUES.F_NAN, !VALUES.F_NAN]
    endelse
    return, [lc, lc_int]
end


function mg_peaks_single, vel, spec, lc
    ; Calculates the positions of the red and blue peaks (velocity and int.)
    ;
    ; Returns array with [bp_v, bp_i, rp_v, rp_i]
    pp = 45           ; number of points for fine interpolation of peaks
    lookahead = 10    ; for peak finding, peaks separated by less are not used
    nsp = n_elements(spec) - 1
    bp = [!VALUES.F_NAN, !VALUES.F_NAN]
    rp = [!VALUES.F_NAN, !VALUES.F_NAN]
    if ~finite(lc) then lc = 0.   ; approximate lc at 0 km/s when missing
    ; Find peaks
    peakdetect_lcl, spec, vel, pmax, pmin, lookahead=lookahead, /sorted
    ; Decide which maxima are red and blue peaks, if any are found
    if (n_elements(pmax) gt 0) and (n_elements(pmin) gt 0) then begin
        ; remove peaks more than 50 km/s from line centre
        if finite(lc) then $
            ;pmax = pmax[where(abs(pmax[*, 0] - lc) lt 50), *] $
            idx_filt = where(abs(pmax[*, 0] - lc) lt 50, count) $
        else $
            ;pmax = pmax[where(abs(pmax[*, 0] - 0) lt 50), *]
            idx_filt = where(abs(pmax[*, 0] - 0) lt 50, count) 
        if count eq 0 then begin
            pmax = [0]
            lpmax = 0
        endif else begin
            pmax = pmax[idx_filt, *]
            lpmax = n_elements(pmax[*, 0])
        endelse
        ; selection of two peaks
        if lpmax gt 4 then begin   ; too many maxima, take inner 4
            pmax = pmax[lpmax/2 - 2 : lpmax/2 + 1, *]
            lpmax = 4
        endif
        case lpmax of
            1: begin
                if pmax[0, 0] gt lc then $
                    rp = pmax[0, *] $
                else $   ; by default assign single max to blue peak if no core
                    bp = pmax[0, *]
            end
            2: begin
                bp = pmax[0, *]
                rp = pmax[1, *]
            end
            3: begin
            ; take the one close to the line core, and from the
            ; remaining two chose the strongest
                case 1 of
                    (pmax[0, 0] lt lc) and (lc lt pmax[1, 0]): begin
                        bp = pmax[0, *]
                        xx = max(pmax[1:*, 1], pmm)
                        rp = pmax[pmm + 1, *]
                    end 
                    (pmax[1, 0] lt lc) and (lc lt pmax[2, 0]): begin
                        xx = max(pmax[0:lpmax - 2, 1], pmm)
                        bp = pmax[pmm, *]
                        rp = pmax[lpmax - 1, *]
                    end
                    else: begin
                        ; take two strongest peaks
                        aa = pmax[where(pmax[*, 1] ne min(pmax[*, 1])), *]
                        bp = aa[0, *]
                        rp = aa[1, *]
                    end
                endcase
            end
            4: begin
                ; first look for special case when two close weak inner peaks
                ; are taken as peaks. In this case take the outer peaks.
                sep_out = pmax[3, 0] - pmax[0, 0]
                sep_in = pmax[2, 0] - pmax[1, 0]
                rt = (pmax[0, 1] gt 1.06 * pmax[1, 1]) and $
                     (pmax[3, 1] gt 1.06 * pmax[2, 1])
                case 1 of
                    (sep_out lt 40) and (sep_in gt 13) and rt: begin
                        bp = pmax[0, *]
                        rp = pmax[3, *]
                    end
                    ; if line core in the middle of the four, or outside the
                    ; maxima region, take inner two maxima
                    (lc gt pmax[1, 0]) and (lc lt pmax[2, 0]): begin
                        bp = pmax[1, *]
                        rp = pmax[2, *]
                    end
                    (lc gt pmax[3, 0]) or (lc lt pmax[0, 0]): begin
                        bp = pmax[1, *]
                        rp = pmax[2, *]                       
                    end
                    lc lt pmax[1, 0]: begin   ; proceed like for 3 maxima
                        bp = pmax[0, *]
                        xx = max(pmax[1:*, 1], pmm)
                        rp = pmax[pmm + 1, *]
                    end
                    lc lt pmax[3, 0]: begin
                        xx = max(pmax[0:2, 1], pmm)
                        bp = pmax[pmm, *]
                        rp = pmax[3, *]
                    end
                    else: break
                endcase
            end
            else: break
        endcase
    endif
    bp = reform(bp)
    rp = reform(rp)
    ; interpolation for more precise peaks
    if finite(bp[0]) then begin
        xx = min(vel - bp[0], idg, /absolute)
        llim = max([0, idg - 2])
        hlim = min([idg + 2, nsp])
        nvel = dindgen(pp) / (pp - 1.d0) * (vel[llim] - vel[hlim]) + vel[hlim]
        llim = max([0, idg - 3])
        hlim = min([idg + 3, nsp])
        ; spline tension should be set > 0 in the future
        if hlim ge llim + 2 then begin
            nspec = spline(vel[llim:hlim], spec[llim:hlim], nvel, 0.)
            xx = max(nspec, midx)
            bp[0] = nvel[midx]
            bp[1] = nspec[midx]
        endif
    endif
    if finite(rp[0]) then begin
        xx = min(vel - rp[0], idg, /absolute)
        llim = max([0, idg - 2])
        hlim = min([idg + 2, nsp])
        nvel = dindgen(pp) / (pp - 1.d0) * (vel[llim] - vel[hlim]) + vel[hlim]
        llim = max([0, idg - 3])
        hlim = min([idg + 3, nsp])
        ; spline tension should be set > 0 in the future
        if hlim ge llim + 2 then begin
            nspec = spline(vel[llim:hlim], spec[llim:hlim], nvel, 0.)
            xx = max(nspec, midx)
            rp[0] = nvel[midx]
            rp[1] = nspec[midx]
        endif
    endif
    return, [[bp], [rp]]
end


pro iris_get_mg_features, spec, wave, vrange, lc, rp, bp, onlyk=onlyk, $
                            onlyh=onlyh
    ; Calculates the velocity shift and intensity of the line centre,
    ; blue and red peaks for the Mg II h & k lines.
    c = 299792.4580d0   ; in km/s
    npi = 300           ; number of points for fine interpolate spec in vrange
    ;wave_ref = [279.55266863d0, 280.27037042d0]  ; Mg II k, h in air (nm)
    wave_ref = [279.63509493d0, 280.35297192d0]   ; Mg II k, h in vac (nm)
;    if keyword_set(onlyk) then wave_ref = [279.63509493d0]
    if keyword_set(onlyk) then wave_ref = [279.64400000d0]
    if keyword_set(onlyh) then wave_ref = [280.35297192d0]
    if keyword_set(onlyk) and keyword_set(onlyh) then $
        message, 'can only select either /onlyk or /onlyh'
    nline = n_elements(wave_ref)
    s = size(spec)
    nsp= s[1] - 1
    nspec = s[2]
    lc = fltarr(nline, 2, nspec) + !VALUES.F_NAN
    bp = fltarr(nline, 2, nspec) + !VALUES.F_NAN
    rp = fltarr(nline, 2, nspec) + !VALUES.F_NAN
    spc = fltarr(npi, nspec)
    kernel = gaussian_kernel(2.)
    if nspec lt n_elements(kernel) then begin
        kernel = [1.]
        message, 'Not enough spectra, Gaussian convolution will not be used.',$
            /info
    endif
    ; main loop over lines
    for l=0, nline - 1 do begin
        vaxis = c * (wave - wave_ref[l]) / wave_ref[l]
        if (vaxis[0] le vrange[0]) and (vaxis[nsp] ge vrange[1]) then begin
            idx = where((vaxis ge vrange[0] - 3) and (vaxis le vrange[1] + 3))
            ivel = vaxis[idx]
            nv = n_elements(ivel) - 1
            ispc = spec[idx, *]
            ; interpolate spectra to higher resolution
            vel = dindgen(npi) / (npi - 1.d0) * (ivel[nv] - ivel[0]) + ivel[0]
            ; get line centre
            for i=0, nspec - 1 do begin
                spc[*, i] = spline(ivel, ispc[*, i], vel, 0.)
                lc[l, *, i] = mg_single(vel, spc[*, i], 0.)  
            endfor
            ; two iterations for cleaning up
            for n=0, 1 do begin
                lci = reform(lc[l, 0, *])
                lcc = lci * 1.
                ; find outliers, distant more than 3 km/s from a Gaussian blur 
                lcc[where(~finite(lcc))] = 0.0
                ;diff = abs(lcc - gauss_smooth(lcc, 2., /edge_truncate))
                diff = abs(lcc - convol(lcc, kernel, /edge_truncate))
                idx_filt = where(diff gt 3, count)
                if count gt 0 then lci[idx_filt] = !VALUES.F_NAN
                ; spline interpolation to guess missing line centres
                good_idx = where(finite(lci), count)
                if count ge 3 then begin
                    lc_guess = spline(good_idx, lci[good_idx], findgen(nspec), 1.)
                    ; repeat calculation for missing values using new guess
                    for i=0, nspec - 1 do begin
                        if ~finite(lci[i]) then begin
                            lc[l, *, i] = mg_single(vel, spc[*, i], $
                                                    lc_guess[i], /force_guess)
                        endif
                    endfor
                endif
            endfor
            ; last run with use_deriv
            for i=0, nspec-1 do begin
                if ~finite(lc[l, 0, i]) then $
                    lc[l, *, i] = mg_single(vel, spc[*, i], 5., /use_deriv)
            endfor
            ; calculate peaks
            for i=0, nspec-1 do begin
                res = mg_peaks_single(vel, spc[*, i], lc[l, 0, i])
                bp[l, *, i] = res[*, 0]
                rp[l, *, i] = res[*, 1]
            endfor
        endif else begin
            print, '*** Line not present, skipping: ', string(wave_ref[l])
        endelse
    endfor
end
