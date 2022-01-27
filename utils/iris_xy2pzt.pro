function IRIS_xy2pzt, x, y, NoOffset=NoOffset
  ;
  ;+
  ;   Name: IRIS_xy2pzt
  ;
  ;   Purpose: conversion of x-y-coordinates into IRIS PZT values
  ;
  ;   Input Parameters:
  ;      x - horizontal component in arcseconds
  ;      y - vertical component in arcseconds
  ;      alternatively: x can be an array containing the coordinates
  ;
  ;   Output:
  ;      function returns integer array with the 3 IRIS PZT values
  ;
  ;   Keyword Parameters:
  ;      NoOffset - the offset will no be applied to the calculation
  ;                 (used for example to convert PZT values from FRM-lists, and PZT steps in OBS-lists, but not PZT Off in OBS)
  ;
  ;   History:
  ;      2012 - M.Wiesmann
  ;      24-apr-2013 - M.Wiesmann: new values for the constants and offset
  ;
  ;   Restrictions:
  ;      no test on input yet
  ;
  ; $Id: iris_xy2pzt.pro,v 1.2 2013/04/25 18:13:17 mawiesma Exp $  ;
  ;-

  pixel = 0.1684
  s1 = 0.174*pixel ; = 0.0293 arcsec/DN
  s2 = 0.1725*pixel; = 0.02905 arcsec/DN
  s3 = 0.1725*pixel; = 0.02905 arcsec/DN
  
  if keyword_set(NoOffset) then offset = 0 $
  else offset = -250
  
  if N_PARAMS() eq 1 then begin
    s=size(x)
    if (s[0] eq 1) && (s[1] eq 2) then begin
      xx=x[0]
      yy=x[1]
    endif else begin
      print,'wrong input format, IRIS_xy2pzt(a[,b])'
      return, -1
    endelse
  endif else if N_PARAMS() eq 2 then begin
    xx=x
    yy=y
  endif else begin
    print,'wrong input format, IRIS_xy2pzt(a[,b])'
    return, -1
  endelse
  
  pzt = intarr(3)
  
  pzt[0] = round(offset + 2./3. * yy/s1)
  pzt[1] = round(offset - 1./3. * yy/s2 + 1./sqrt(3) * xx/s2)
  pzt[2] = round(offset - 1./3. * yy/s3 - 1./sqrt(3) * xx/s3)
  
  return, pzt
end
