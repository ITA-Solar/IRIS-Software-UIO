pro iris_level2_listwin,file
;+
;   iris_level2_listwin,file
;
;            list windows in level2 raster file
;-
if(n_params() lt 1) then begin
  message,'Syntax: iris_level2_listwin,file',/info
  return
endif

d=obj_new('iris_data')
d->read,file
print,'Available windows:'
for i=0,(d->getnwin())-1 do begin
  print,string(i,d->getline_id(i),format='(i3," ",a)')
endfor
obj_destroy,d

end
