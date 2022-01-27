pro br_stretch,var,nx,ny,dx,z1,varst,zst=zst,time=time,dzt=dzt,fixnz=fixnz
  nz1=n_elements(z1)
;
  sz=size(var)

  if (keyword_set(dzt)) then dzn=min([dx,min(dzt)]) else dzn=dx

  nzst=long((max(z1)-min(z1))*(1+1e-6)/dzn)+1
  if nzst ge 3e4 and n_elements(fixnz) eq 0 then fixnz=0
  if nzst lt 0 or n_elements(fixnz) ne 0 then begin
     if n_elements(fixnz) eq 0 then fixnz=3.e4
     if fixnz le 1 then  fixnz=3.e4
     dzn=(max(z1)-min(z1))/(fixnz*1.0)
     nzst=fixnz
  endif
  zst=findgen(nzst)*dzn+min(z1)

if n_elements(time) eq 0 then begin
    x=fltarr(nx,ny,nzst)
    y=fltarr(nx,ny,nzst)
    z=fltarr(nx,ny,nzst)
    for i=0,nx-1 do x[i,*,*]=i
    for j=0,ny-1 do y[*,j,*]=j
    zindex=interpol(findgen(nz1),z1,zst)
    for k=0,nzst-1 do z[*,*,k]=zindex[k]
    if sz(0) ne 3 then begin
       var2=fltarr(nx,ny,nz1)
;;       if nx ne 1 or ny ne 1 then print,'Warning, the variable has wrong size'
       var2(0:nx-1,0:ny-1,0:nz1-1)=var
       var=var2
    endif
    varst=reform(interpolate(var,x,y,z))

  endif else begin
    if sz(0) ne 3 then begin
       var2=fltarr(nx,nz1,nt)
;;       if nx ne 1 or nz1 ne 1 then print,'Warning, the variable has wrong size'
       var2(0:nx-1,0:nz-1,0:nt-1)=var
       var=var2
    endif
    nt=sz(3)
    x=fltarr(nx,nzst,nt)
    z=fltarr(nx,nzst,nt)
    t=fltarr(nx,nzst,nt)
    for i=0,nx-1 do x[i,*,*]=i
    zindex=interpol(findgen(nz1),z1,zst)
    for j=0,nzst-1 do z[*,j,*]=zindex[j]
    for k=0,nt-1 do t[*,*,k]=k
    varst=reform(interpolate(var,x,z,t))
 endelse

end
