;+
; NAME:
;       IRIS_DESCALE_ARRAY
;
; PURPOSE:
;       make IRIS float data out of scaled integer
;
; CATEGORY:
;       IRIS
;
; CALLING SEQUENCE:
;       Result = IRIS_DESCALE_ARRAY(data)
;
; INPUTS:
;       data - scaled integer array
;
; KEYWORD PARAMETERS:
;       missing - missing data is set to this value (defaults to -200.)
;
; OUTPUTS:
;       Result - floating point array
;
; MODIFICATION HISTORY:
;       1.1  2013/10/08 matsc   First version
;       1.2  2013/10/11 matsc   added missing keyword
;
;-
function iris_descale_array,data,missing=missing
if(n_params() lt 1) then begin
  message,'Syntax: Result = iris_descale_array(data,missing=missing)',/info
  return,-1
endif

if(n_elements(missing) eq 0) then missing=-200
missing_index = where(data eq -32768,count)
floatdata=data*0.25+7992.
if(count gt 0) then begin
  floatdata[missing_index] = missing
endif

return,floatdata
end

