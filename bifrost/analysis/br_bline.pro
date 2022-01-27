;+       
;       
; NAME :  BR_BLINE       
; PURPOSE :       
;         Calculates a field line given a 3-dimensional B field       
; CALLING SEQUENCE :       
;         BR_BLINE, Bx, By, Bz, R0, R, Dx=Dx, Dy=Dy, Dz=Dz,Ds=Ds       
; INPUTS:       
;         Bx, By, Bz : 3-dimensional arrays of B components       
;         R0 : starting point (1-d array with 3 elements)        
; KEYWORD OPTIONAL INPUTS:       
;         Dx, Dy: increment values in x and y coordiantes       
;                  (Default=1)       
;         Dz : increment values in the z-direction
;               
;               It should be  a vector of two elements which define
;               a grid system    
;                   z = dz(0)*x^dz(1) (x=0, 1, .., Nz-1)               
;
;               Default is dz=[1, 1] (linear inrement)          
;                     
;         Ds : increment value of the arc length along the field       
;              lines( default=min(Dx, Dy, Dz))       
;              if Ds is negative, then the integration is done       
;              in the direction opposite to magnetic field.       
; OUPUTS :       
;         R : 2-d array. A series of spatial points defining       
;         the field line. 3 x N elements. N : number of       
;         elements.       
; RESTRICTION:       
;         R0 should be inside the spatial domain defined by       
;                          0 =< x =< (Nx-1)*Dx       
;                          0 =< y =< (Ny-1)*Dy       
;                          0 =< Z =<  zmax      
;        and integration will be done while        
;        while the spatial points are inside that domain.       
; Modfication History          
;  June 1997, Jongchul Chae       
;-       
pro br_bline, bx, by, bz,r0, r, dx=dx, dy=dy,dz=dz, ds=ds, $
    xyperiodic=xyperiodic,deltas=deltas,niter=niter,zmax=zmax

ny = n_elements(bx(0,*,0))       
if (size(bx))[0] eq 3 then begin       
nx = n_elements(bx(*,0,0))       
ny = n_elements(bx(0,*,0))       
nz = n_elements(bz(0,0,*))    
endif else begin
nx = n_elements(bx(*,0))       
ny = 1
nz = n_elements(bz(0,*))    
bx=reform(bx,nx,1,nz)
by=reform(by,nx,1,nz)
bz=reform(bz,nx,1,nz)
endelse   
   
if n_elements(r0) eq 2 then r0=[r0[0],0,r0[1]]

if n_elements(dx) eq 0 then dx=1.       
if n_elements(dy) eq 0 then dy=1.       
if n_elements(dz) ne 2 then dz=[1.,1.]       
if n_elements(ds) eq 0 then ds=dx<dy<dz(0)   
if n_elements(xyperiodic) eq 0 then xyperiodic=0    
if n_elements(niter) eq 0 then niter = 500
if n_elements(deltas) eq 0 then deltas = 1e10

xmax=(nx-1)*dx       
ymax=(ny-1)*dy 

z= dz(0)*findgen(nz)^dz(1)     
if n_elements(zmax) eq 0 then zmax=max(z)  
r2=r0             

if xyperiodic then $
inside = r2[2]*(r2[2]-zmax) le 0. else $
inside = r2[0]*(r2[0]-xmax) le 0. and  $       
         r2[1]*(r2[1]-ymax) le 0. and  $       
         r2[2]*(r2[2]-zmax) le 0.
if not inside then begin                
  message,'Starting point is outside the spatial domain.',/info
return       
endif else begin       
r=[transpose(r2)]   
xmod = (r2[0]+xmax) mod xmax
ymod = (r2[1]+ymax) mod ymax
zindex = (r2(2)/dz(0))^(1./dz(1))  
b2 = [interpolate(bx, xmod/dx, ymod/dy, zindex), $       
      interpolate(by, xmod/dx, ymod/dy, zindex), $       
      interpolate(bz, xmod/dx, ymod/dy, zindex)]       
       
b2=b2/norm(b2)       
       
endelse       
       
count=0       
              
while inside do begin       
r1=r2       
b1=b2     
iter2=0     
repeat begin       
r2_0 =r2       
r2=r1+(b1+b2)*0.5*ds
zindex = (r2(2)/dz(0))^(1./dz(1))     
xmod = (r2[0]+xmax) mod xmax
ymod = (r2[1]+ymax) mod ymax
b2 = [interpolate(bx, xmod/dx, ymod/dy, zindex), $       
      interpolate(by, xmod/dx, ymod/dy, zindex), $       
      interpolate(bz, xmod/dx, ymod/dy, zindex)]   
b2=b2/norm(b2)             
iter2=iter2+1       
   
endrep until (norm(r2_0-r2) le ds*0.2) or iter2 ge 10       
;print, 'iter2=', iter2 
;plots, [r1(0), r2(0)]*f+(f-1)/2.,[r1(1), r2(1)]*f+(f-1)/1, /dev     
r=[r, transpose(r2)]
drs=[r-shift(r,-1,0)]
drs(0,*)=[0,0,0]
nrg=n_elements(drs(*,0))
drs(nrg-1,*)=[0,0,0]
ranges=total(sqrt(drs(*,0)^2+drs(*,1)^2+drs(*,2)^2))       
if xyperiodic then $
   inside = r2[2]*(r2[2]-zmax) le 0. and count lt niter-2 and ranges le deltas else $
      inside = r2[0]*(r2[0]-xmax) le 0. and  $       
               r2[1]*(r2[1]-ymax) le 0. and  $       
               r2[2]*(r2[2]-zmax) le 0. and  $
               count lt niter-2 and ranges le deltas
count=count+1
endwhile        

end       

  
                         
                       
       
