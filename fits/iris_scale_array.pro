;+
; NAME:
;       IRIS_SCALE_ARRAY
;
; PURPOSE:
;       make IRIS scaled integer out of float data
;
; CATEGORY:
;       IRIS
;
; CALLING SEQUENCE:
;       Result = IRIS_SCALE_ARRAY(data)
;
; INPUTS:
;       data - floating point array, NaN is treated as missing data,
;              -200 is not unless missing is set to -200
;
; KEYWORD PARAMETERS:
;       missing - this value is treated as missing data (in addition to NaN)
;
; OUTPUTS:
;       Result - scaled integer array
;
; MODIFICATION HISTORY:
;       1.1  2013/10/08 matsc   Same as iris_scale v 1.2 that has been depricated
;       1.2  2013/10/11 matsc   added missing keyword
;
;-
function iris_scale_array,data,missing=missing
if(n_params() lt 1) then begin
  message,'Syntax: Result = iris_scale_array(data,missing=missing)',/info
  return,-1
endif

if(n_elements(missing) eq 0) then begin
  missing_index = where(finite(data) ne 1,count)
endif else begin
  missing_index = where(finite(data) ne 1 or data eq missing,count)
endelse
clipdata = (-199) > data < (16383-200)
intdata = fix(round(( clipdata + 200) * 4-32768))
if(count gt 0) then intdata[missing_index] = -32768

return,intdata
end

