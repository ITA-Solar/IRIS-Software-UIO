;
;+
; NAME:
;       IRIS_LOAD
;
; PURPOSE:
;
;       loads level 2 IRIS raster + slit jaw images into an IRIS_DATA
;       object or (optionally) a structure
;
;
; CATEGORY:
;       Hansteen/Wikstøl Data analysis SW
;
; CALLING SEQUENCE:
;       iris_load,file,structure=structure
;
; INPUTS:
;       file - file name of iris raster or iris slit jaw file
;
; KEYWORD PARAMETERS:
;       structure - set if a structure containing iris data, instead
;                   of data object is desired (not yet implemented).
;
; OUTPUTS:
;       data - IRIS_DATA object
;
; CALLS:
;
; COMMON BLOCKS:
;
; PROCEDURE:
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;       26-May-2013: Viggo Hansteen
;       23-Mar-2016: Martin Wiesmann, search for 'raster' in filename
;         rather than 'SJI' to decide whether it is a raster or SJI
;         SOT and AIA files don't contain SJI but are
;   
;$Id: iris_load.pro,v 1.9 2016/03/23 09:58:36 mawiesma Exp $
;-
function iris_load,file,structure=structure,verbose=verbose
if n_elements(file) eq 0 then begin
  message,'d=iris_load(file ,structure=structure, verbose=verbose)',/info
  return,-1
endif
if n_elements(structure) eq 0 then structure=0
for i=0,n_elements(file)-1 do begin
  if not (file_info(file[i])).exists then begin
    message,file[i]+' not found',/info
    return,-1
  endif
endfor
if stregex(file,/fold_case,'raster',/bool) then begin 
; find eventual slit jaw images
  message,'loading IRIS_DATA raster',/info
  file_head=strmid(file,0,strpos(file,'raster')-1)
  fsji=file_search(file_head+'_SJI*.fits')
  if fsji[0] ne '' then message,'loading IRIS_SJI images',/info
  d=iris_obj([file,fsji],verbose=verbose)
endif else begin
; should be a slit jaw image file...
  message,'loading IRIS_SJI image',/info
  d=iris_sji(file,verbose=verbose)
endelse 
if structure then begin
  s=d->getdata()
  obj_destroy,d
  return,s
endif else return,d
end
