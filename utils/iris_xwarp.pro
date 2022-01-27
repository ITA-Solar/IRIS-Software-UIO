function iris_xwarp,wimage,d
;
; find number of domains with different dx
  if d->getsit_and_stare() then x=d->gettime() else x=d->getxpos()
  badf=where((d->aux_info()).dsrcfix lt 0)
  goodf=where((d->aux_info()).dsrcfix gt 0)
  xorg=x
  if badf[0] ne -1 then begin
    for i=0,n_elements(badf)-1 do begin
      j=1
      repeat begin 
        lb=badf[i]-j > (-1)
        j=j+1
      endrep until where(lb eq badf) eq -1 or lb eq -1
      j=1
      repeat begin 
        ub=badf[i]+j < n_elements(x)
        j=j+1
      endrep until where(ub eq badf) eq -1 or ub eq n_elements(x)
      if lb gt -1 and ub lt n_elements(x) then x[badf[i]]=interpolate([x[lb],x[ub]],(badf[i]-lb)/float(ub-lb)) else begin
        if n_elements(goodf) lt 2 then begin
          message,'Only 1 good position found, giving up warp',/info
          return,wimage
        endif
        if lb eq -1 then begin
          p=goodf[0:1]
          x[0]=x[p[0]]+p[0]*(x[p[1]]-x[p[0]])/(p[1]-p[0])
        endif
        if ub eq n_elements(x) then begin
          p=goodf[n_elements(goodf)-2:n_elements(goodf)-1]
          x[n_elements(x)-1]=x[p[1]]+(ub-p[1])*(x[p[1]]-x[p[0]])/(p[0]-p[1])
        endif
      endelse
    endfor  
  endif
  dx=shift(x,-1)-x
  dx[n_elements(x)-1]=dx[n_elements(x)-2]
;      
  if dx[0] lt 0 then neg=1 else neg=0
  dx=abs(dx)
  dxmin=min(dx)
  idx=where(abs(dx-shift(dx,1)) gt 0.001/mean(dx))
  if idx[0] ne -1 then begin
    if idx[0] eq 0 then idx=idx[1:*]
    ndx=n_elements(idx)
; loop through each region and warp image onto finest dx size
    sz=size(wimage)
    image=congrid(wimage[0:idx[0],*],fix(dx[0]/dxmin)*idx[0],sz[2])
    for ir=1,ndx do begin
      is=idx[ir-1]
      if ir lt ndx then ie=idx[ir] else ie=sz[1]-1
      image=[image,congrid(wimage[is+1:ie,*],fix(dx[ie-1]/dxmin)*(ie-is),sz[2])]
   endfor
 endif else image=wimage
 if neg then image=rotate(image,5)
 return,image
end

