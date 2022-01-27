; 
;+
; NAME:
;       mean_spec
;
; PURPOSE:
;       MEAN_SPEC calculates the mean spectrum of a 2D or 3D data
;       cube. 
;
; CATEGORY:
;       Hansteen/Wikstol Data analysis SW
;
; CALLING SEQUENCE:
;       mspec = mean_spec(data)
;
; INPUTS:
;       data: The spectral data from which the average spectrum will
;       be calculated. Typically this will be of type: data(lambda,
;       x,y)
;
; KEYWORD PARAMETERS:
;       retain_x: Keep x-dimension. Only average over y
;       retain_y: Keep y-dimension. Only average over x
;
; OUTPUTS:
;       Average spectrum: mspec(lambda)
;
; CALLS:
;
;
; COMMON BLOCKS:
;       
;
; PROCEDURE:
;  
; 
; RESTRICTIONS:
;       The input data needs to have lambda as the first dimension
;
;
; MODIFICATION HISTORY:
;       09-Feb-2004: Oivind Wikstol, version 1.0
; $Id: iris_mean_spec.pro,v 1.4 2013/10/09 12:51:09 viggoh Exp $
;-
function iris_mean_spec, data, retain_x = retain_x, retain_y = retain_y, missing=missing

  if n_params() eq 0 then begin
    print, 'usage: mean_spec,data'
    return, -99
  endif

  if n_elements(missing) eq 0 then missing=-999999.

  sz = size(data)
  dum = data
  if (where(data eq missing))[0] ne -1 then begin 
    dum[where(data eq missing)]=!values.f_nan
  endif

  if sz[0] lt 2 then begin
    print, 'mean_spec: Data must be at least 2D'
    return, -99
  endif

  if sz[0] lt 3 then begin
    mspec = total(dum, 2,/nan)/sz[2]
    return, mspec
  endif else begin 
    if n_elements(retain_x) ne 0 then begin
      mspec = total(dum, 3,/nan)/sz[3]
      return, mspec
    endif
    if n_elements(retain_y) ne 0 then begin
      mspec = total(dum, 2,/nan)/sz[2]
      return, mspec
    endif
    mspec = total(dum, 2,/nan)/sz[2]
    mspec = total(mspec, 2,/nan)/sz[3]
 endelse
  return, mspec
end



  
