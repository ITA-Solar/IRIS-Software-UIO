pro cdir3 ,obs,rootl3=rootl3,debug=debug
;+
;   cdir3 ,obs,rootl3=rootl3,/debug
;
;            change directory to level3 tree from level2 tree
;-
if(n_elements(obs) eq 0) then begin
  cd,current=cwd
  ic=strpos(cwd,'level2')
  dir3=cwd
  strput,dir3,'3',ic+5
  cd,dir3
endif else begin
  if(n_elements(rootl3) eq 0) then rootl3=getenv('IRIS_DATA')+'/level3'
  yyyy=strmid(obs,0,4)
  mm=strmid(obs,4,2)
  dd=strmid(obs,6,2)
  dirl3=rootl3+'/'+yyyy+'/'+mm+'/'+dd+'/'+obs
  dum=file_search(dirl3,count=count)
  if(count ne 1) then begin
    print,dirl3+' does not exist'
    return
  endif
  cd,dirl3
endelse
if(keyword_set(debug)) then stop

end
