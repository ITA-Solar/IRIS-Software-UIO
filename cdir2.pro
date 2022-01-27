pro cdir2 ,obs
;+
;   cdir2 ,obs
;
;            change directory to level2 tree from level3 tree
;-
if(n_elements(obs) eq 0) then begin
  cd,current=cwd
  ic=strpos(cwd,'level3')
  dir2=cwd
  strput,dir2,'2',ic+5
  cd,dir2
endif else begin
  rootl2=getenv('IRIS_DATA')+'/level2'
  yyyy=strmid(obs,0,4)
  mm=strmid(obs,4,2)
  dd=strmid(obs,6,2)
  dirl2=rootl2+'/'+yyyy+'/'+mm+'/'+dd+'/'+obs
  dum=file_search(dirl2,count=count)
  if(count ne 1) then begin
    message,dirl2+' does not exist',/info
    return
  endif
  cd,dirl2
endelse

end
