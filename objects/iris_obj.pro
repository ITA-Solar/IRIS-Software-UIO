function iris_obj,file,verbose=verbose
;
if not stregex(file[0],/fold_case,'.fits',/bool) then begin 
  message,'FITS file must have ".fits" extension!!',/info
  return,-1
end
if stregex(file[0],/fold_case,'raster',/bool) then begin 
  return,obj_new('iris_data',file,verbose=verbose)
endif else begin
  message,'SJI file detected, returning iris_sji object',/info
  return,iris_sji(file)
endelse
;
end
