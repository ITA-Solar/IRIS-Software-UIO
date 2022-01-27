function iris_sji,file,verbose=verbose,structure=structure
;
if n_elements(structure) eq 0 then structure=0 
d=obj_new('iris_sji',file,verbose=verbose)
if structure then begin
  s=d->getdata()
  obj_destroy,d
  return,s
endif else return,d
;
end
