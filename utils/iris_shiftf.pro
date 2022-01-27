function iris_shiftf,image,dx,dy ,linear=linear
;+
;   shiftf,image,dx,dy ,/linear
;
;            shifts image dx pixels in x and dy pixels in y where
;            dx and dy are floating point numbers
;            uses poly_2d
;
;  $Id: iris_shiftf.pro,v 1.1 2014/01/28 13:10:53 viggoh Exp $
;
;-
if(n_params() lt 3) then begin
  print,'result=iris_shiftf(image,dx,dy ,/linear)'
  return,0
endif

dum=size(image)
if(dum(0) ne 2) then begin
  print,'image has to be 2-D'
  return,0
endif

p=[-dx,0,1,0]
q=[-dy,1,0,0]

if(keyword_set(linear)) then cubic=0 else cubic=-0.5
return,poly_2d(image,p,q,cubic=cubic)
end
