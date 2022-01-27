function IRIS_pzt2xy, a, b, c, NoOffset=NoOffset
  ;
  ;+
  ;   Name: IRIS_pzt2xy
  ;
  ;   Purpose: conversion of IRIS PZT values into x-y-coordinates
  ;
  ;   Input Parameters:
  ;      a - PZTA
  ;      b - PZTB
  ;      c - PZTC
  ;      alternatively: a can be an array containing the 3 PZT-values
  ;
  ;   Output:
  ;      function returns double array with the x- and y-values in arcseconds
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
  ; $Id: iris_pzt2xy.pro,v 1.2 2013/04/25 18:13:17 mawiesma Exp $  ;
  ;-

  pixel = 0.1684
  s1 = 0.174*pixel ; = 0.0293 arcsec/DN
  s2 = 0.1725*pixel; = 0.02905 arcsec/DN
  s3 = 0.1725*pixel; = 0.02905 arcsec/DN
  
  if keyword_set(NoOffset) then offset = 0 $
  else offset = -250
  
  if N_PARAMS() eq 1 then begin
    s=size(a)
    if (s[0] eq 1) && (s[1] eq 3) then begin
      aa=a[0]
      bb=a[1]
      cc=a[2]
    endif else begin
      print,'wrong input format, IRIS_pzt2xy(a[,b,c])'
      return, -1
    endelse
  endif else if N_PARAMS() eq 3 then begin
    aa=a
    bb=b
    cc=c
  endif else begin
    print,'wrong input format, IRIS_pzt2xy(a[,b,c])'
    return, -1
  endelse
  
  xy = dblarr(2)
  
  A0 = aa - offset
  B0 = bb - offset
  C0 = cc - offset
  
  xy[0] = 1./2. * sqrt(3.) * (B0*s2 - C0*s3)
  xy[1] = A0*s1 - 1./2. * (B0*s2 + C0*s3)
  
  return, xy
end
