function IRISsim_pzt2xy, aa, bb, cc, NoOffset=NoOffset
  ;converts PZT-values into x-y-coordinates

  s1 = 0.0275
  s2 = 0.0275
  s3 = 0.0275
  
  if keyword_set(NoOffset) then begin
    A0 = aa
    B0 = bb
    C0 = cc
  endif else begin
    A0 = aa + 248
    B0 = bb + 248
    C0 = cc + 248
  endelse
  
  xy = dblarr(2)
  
  xy[0] = 0.866 * (C0*s3 - B0*s2)
  xy[1] = -A0*s1 + B0*s2/2 + C0*s3/2
  
  return, xy
end
