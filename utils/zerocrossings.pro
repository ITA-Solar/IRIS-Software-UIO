; NAME: ZEROCROSSINGS
; 
; PURPOSE: This functions returns the index array where an array changes its sign.
;
; INPUT: y = The input array
;
; OUTPUTS: As the result, the index array where y changes sign (fractions of index are given).
;		
; OPTIONAL KEYWORDS: n_crossings = Set this keyword to a named variable to contain the number
;				of zero crossings found by zerocrossings.
;
; HISTORY: Written by Wolfgang Finsterle, 23 April 2002
;

function zerocrossings,y,n_crossings=n_crossings,max=max,min=min

  x0=where(sgn(y[1:*]) eq -sgn((shift(y,1))[1:*]) or y eq 0,n_crossings)
  if n_crossings gt 0 then x0=x0-y[x0]/(y[x0+1]-y[x0])

  return,x0
end
