;+
; NAME:
;	IRIS_GET_MG_FEATURES_LEV2
;
; PURPOSE:
;	Extract positions of Mg II spectral features from IRIS level 2 data.
;       Wrapper around IRIS_GET_MG_FEATURES for level2 data.
;       The algorithm is detailed in:
;       Pereira, T. M. D. , Leenarts, J., De Pontieu, B., Carlsson, M.,
;       Uitenbroek, H., 2013, ApJ, 778, 143, http://arxiv.org/abs/1310.1926
;
; CATEGORY:
;	IRIS_ANALYSYS
;
; CALLING SEQUENCE:
;	IRIS_GET_MG_FEATURES_LEV2, file, iwin, vrange, lc, rp, bp,
;                                  /onlyh, /onlyk
;
; INPUTS:
;       file:   Name of level 2 spectral raster file.
;       iwin:   Number of the window containing the Mg II h & k spectra.
;       vrange: array with two elements: starting and ending velocities
;               to define velocity range (from line centre) where to find
;               the spectral features. In km/s. Recommended so far: [-40, 40]
;
; KEYWORD PARAMETERS:
;       onlyk:  if set, will only calculate properties for the Mg II k line.
;       onlyh:  if set, will only calculate properties for the Mg II h line.
;               Obviously, these are mutually exclusive.
;       wave_comp: Wavelength compensation to adjust any systematics in the
;               wavelength calibration. Units shall be the same as in level2
;               files (Angstrom). Default is zero.
;
; OUTPUTS:
;       lc:     4-D array (line, feature, slit pos., raster pos.) with line
;               centre positions. Line is defined by wave_ref (typically
;               Mg II k first and then Mg II h), and feature is velocity shift
;               (first index, in km/s from rest wavelength) or spectral
;               intensity (second index, same units as spec).
;       bp:     4-D array (line, feature, slit pos., raster pos.) with blue peak
;               positions. Same structure and units as lc.
;       rp:     4-D array (line, feature, slit pos., raster pos.) with red peak
;               positions. Same structure and units as lc.
;
; MODIFICATION HISTORY:
;    Written: Tiago M. D. Pereira, UiO, October 2013.
;
;-
pro iris_get_mg_features_lev2, file, iwin, vrange, lc, rp, bp, onlyk=onlyk, $
                            onlyh=onlyh, wave_comp=wave_comp
    if not keyword_set(wave_comp) then wave_comp = 0.
    iobj = iris_obj(file)
    wave = (iobj->getlam(iwin) + wave_comp) / 10.
    spec = iobj->getvar(iwin)
    ss = size(spec)
    nspec = ss[2]
    nraster = iobj->getinfo('NAXIS3', iwin)
    nlines = 2
    if keyword_set(onlyk) or keyword_set(onlyh) then nlines = 1
    lc = fltarr(nlines, 2, nspec, nraster, /n)
    bp = fltarr(nlines, 2, nspec, nraster, /n)
    rp = fltarr(nlines, 2, nspec, nraster, /n)
    for i = 0, nraster - 1 do begin
        iris_get_mg_features, iobj->descale_array(spec[*, *, i]), wave, $
            vrange, ltmp, rtmp, btmp, onlyk=onlyk, onlyh=onlyh
        lc[*, *, *, i] = ltmp
        bp[*, *, *, i] = btmp
        rp[*, *, *, i] = rtmp
    endfor
end
    