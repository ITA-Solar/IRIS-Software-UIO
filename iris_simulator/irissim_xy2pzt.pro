function IRISsim_xy2pzt, xx, yy, NoOffset=NoOffset
  ;converts x-y-coordinates into PZT-values

  s1 = 0.0275
  s2 = 0.0275
  s3 = 0.0275
  
  pzt = intarr(3)
  
  if keyword_set(NoOffset) then offset = 0 $
  else offset = -248
  
  pzt[0] = round(offset - 2./3. * yy/s1)
  pzt[1] = round(offset + 1./3. * yy/s2 - 1./sqrt(3) * xx/s2)
  pzt[2] = round(offset + 1./3. * yy/s3 + 1./sqrt(3) * xx/s3)
  
  return, pzt
end
