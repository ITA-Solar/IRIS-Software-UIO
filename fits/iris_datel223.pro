pro iris_datel223,date1,date2 ,_extra=e
;+
;   iris_datel223,date1,date2 ,_extra=e
;
;            make level3 data of all obs from date1 to date2
;-
if(n_params() lt 2) then begin
  message,'Syntax: iris_datel223,date1,date2',/info
  return
endif

l2=iris_time2files(date1,date2,level=2,prelim=4,/obsdirs)
tl2=iris_files2timeline(l2)
iw=where(tl2.obsid lt 4.2e9 and tl2.obsid gt 3e9)
tl2=tl2[iw]

nobs=n_elements(tl2)
for i=0,nobs-1 do begin
  dum=tl2[i].date_obs
  obs=strmid(dum,0,4)+strmid(dum,5,2)+strmid(dum,8,2)+'_'+strmid(dum,11,2)+strmid(dum,14,2)+strmid(dum,17,2)+'_'+strtrim(tl2[i].obsid,2)
  iris_obsl223,obs,/all,/sp,_extra=e
endfor

end
