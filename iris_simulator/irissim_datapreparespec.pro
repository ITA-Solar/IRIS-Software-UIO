function IRISsim_DataPrepareSpec, dataA, dataB, CCDx, CCDy, PZT, timefac, rightpad=rightpad, nuv=nuv

; $Id: irissim_datapreparespec.pro,v 1.5 2013/07/09 09:27:09 mawiesma Exp $  ;

  cut=0.05 ;value, below which no interpolation is done
  
  ;assumption about data input: (lambda, y, x)
  ;dispersion: y=1/6 arcsec, x=1/3 arcsec
  ;for NUV this is not true, x comes before y, and x-dispersion = 1/6 arcsec
  ;new NUV dataset has y before x (after 1/29/13)
  ;even newer NUV dataset has again x before y (after 7/2/13) and now x-dispersion as FUV 1/3 arcsec
  if keyword_set(nuv) then begin
    data1 = transpose(dataA,[0,2,1])
    data2 = transpose(dataB,[0,2,1])
    ;data1 = dataA
    ;data2 = dataB
    dispfac = 3
  endif else begin
    data1 = dataA
    data2 = dataB
    dispfac = 3
  endelse
  
  sdata = size(data1)
  
  ;get x-position and interpolate if necessary
  x = (86.3333333+PZT[0])*dispfac MOD sdata[3]
  x1 = floor(x)
  spacefacx = x-x1
  
  if (spacefacx gt cut) && ((1-spacefacx) gt cut) then begin ;we have to do some interpolation in x-direction
    x2 = ceil(x)
    if x2 eq sdata[3] then x2=0
    if x1 lt 0 then x1=sdata[3]+x1
    if x2 lt 0 then x2=sdata[3]+x2
    d11 =  data1[*,*,x1]
    d12 =  data1[*,*,x2]
    d1 = spacefacx * d12 + (1-spacefacx) * d11
    
    d21 =  data2[*,*,x1]
    d22 =  data2[*,*,x2]
    d2 = spacefacx * d22 + (1-spacefacx) * d21
    
  endif else begin ;no interpolation necessary
    x = round(x)
    if x eq sdata[3] then x=0
    if x lt 0 then x=sdata[3]+x
    d1 = data1[*,*,x]
    d2 = data2[*,*,x]
  endelse
  
  
  ;interpolate between 2 time steps if necessary
  if (timefac gt cut) && ((1-timefac) gt cut) then $ ;we have to do some interpolation in time
    data = timefac * d2 + (1-timefac) * d1 $
  else $ ;no interpolation necessary
    if round(timefac) eq 0 then data = d1 else data = d2
    
    
  ;get y-position and interpolate if necessary
  y = PZT[1]*6 MOD sdata[2]
  if y lt 0 then y=sdata[2]+y
  y1 = floor(y)
  spacefacy = y-y1
  
  if y gt cut then begin ; we have to shift the spectrum in y-direction
    if (1-spacefacy) le cut then begin ;no interpolation necessary
      y = ceil(y)
      y1 = y
      spacefacy = 0
    endif
    if y1 gt 0 then begin
      datatemp = fltarr((size(data))[1],(size(data))[2])
      datatemp[*,0:(size(data))[2]-y1-1] = data[*,y1:(size(data))[2]-1]
      datatemp[*,(size(data))[2]-y1:(size(data))[2]-1] = data[*,0:y1-1]
      data = datatemp
    endif
    
    if spacefacy gt cut then begin ;interpolation necessary
      datatemp = fltarr((size(data))[1],(size(data))[2]+1)
      datatemp[*,0:(size(data))[2]-1] = data
      datatemp[*,(size(data))[2]] = data[*,0]
      data = interpolate(datatemp, findgen((size(data))[1]), findgen((size(data))[2])+spacefacy, /grid)
    endif
  endif
  
  
  ;tile data to cover whole detector in y-direction
  datatemp = make_array(sdata[1], CCDy, /float)
  keepcopying=1
  low=0
  high=low+sdata[2]
  while keepcopying do begin
    if low ge CCDy then begin
      keepcopying=0
    endif else begin
      if high gt CCDy-1 then begin
        miss=CCDy-1-low
        datatemp[*,low:CCDy-1] = data[*,0:miss]
        keepcopying=0
      endif else begin
        datatemp[*,low:high-1] = data
        low=low+sdata[2]
        high=high+sdata[2]
      endelse
    endelse
  endwhile
  data=datatemp
  
  
  ;check if simulated data covers whole CCD also in x-direction
  if sdata[1] lt CCDx then begin
    ;add zeros on the right side
    datatemp = make_array(CCDx, CCDy, /float)
    if keyword_set(rightpad) then datatemp[0:sdata[1]-1,*] = data $ ;add zeros on the right of spectrum
    else datatemp[CCDx-sdata[1]:CCDx-1,*] = data ;add zeros on the left of spectrum
    data=datatemp
  endif
  if sdata[1] gt CCDx then begin
    ;???
    print,'too much data in input (x-direction has more pixels than on CCD)'
  endif
  
  return, data
  
end