; $Id: iris_cube_statistics.pro,v 1.12 2014/10/21 22:10:20 mawiesma Exp $  ;

FUNCTION iris_cube_percentiles, cube, percentiles
  sorteddata = cube[sort(cube)]
  nc = n_elements(cube)
  np = n_elements(percentiles)
  values = fltarr(np)
  
  FOR i=0,np-1 DO values[i] = sorteddata[ulong64(percentiles[i])*nc/100]
  
  return, values
END



FUNCTION iris_cube_statistics, cube, missing=missing, reduced=reduced, scaled=scaled

  IF ~keyword_set(missing) THEN missing = -32768
  
  ; We only want to consider pixels that are not flagged as missing
  ;if keyword_set(scaled) then goodix = where((cube GE -199) AND (cube LE 16182) AND FINITE(cube), count) $
  ;else goodix = where((cube GT missing+0.01) AND FINITE(cube), count)
  goodix = where(FINITE(cube), count)
  if count gt 0 then begin
  
    if ~keyword_set(reduced) then begin
      ;; Moment provides lots of useful info
      dumy = moment(cube[goodix], sdev=sdev)
      mean = dumy[0]
      skewness = dumy[2]
      kurtosis = dumy[3]
      
      ;; Calculate some percentiles
      datap = iris_cube_percentiles(cube[goodix], [1,10,25,75,90,95,98,99])
      
      ;Median
      datamedn=median(cube[goodix])
      
    endif else begin
      ;reduced statistics
      dumy = moment(cube[goodix], sdev=sdev, maxmoment=2)
      mean = dumy[0]
      kurtosis=0
      skewness=0
      datap=intarr(8)
      datamedn=0
    endelse
    
    ;calculate maximum and minimum
    datamax=max(cube[goodix], min=datamin)
    
    ;Number of data elements
    datavals=n_elements(cube[goodix])
    
  endif else begin
    kurtosis=0
    datamax=0
    mean=0
    datamedn=0
    datamin=0
    sdev=0
    skewness=0
    datavals=0
    datap=intarr(8)
  endelse
  
  ; Then return structure containing all statistics information
  statistics = {kurtosis:kurtosis, datamax:datamax, datamean:mean, $
    datamedn:datamedn, datamin:datamin, datarms:sdev, $
    dataskew:skewness, datavals:datavals, datap01:datap[0],$
    datap10:datap[1], datap25:datap[2], datap75:datap[3], datap90:datap[4], $
    datap95:datap[5], datap98:datap[6], datap99:datap[7]}
    
  return, statistics
END
