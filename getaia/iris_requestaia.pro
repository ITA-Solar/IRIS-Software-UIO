; $Id: iris_requestaia.pro,v 1.2 2014/07/17 11:58:17 mawiesma Exp $  ;

pro IRIS_requestAIA, starttime, endtime, xcen, ycen, fovx, fovy, waves, obsid=obsid, $
    email=email, maxframes=maxframes, instrument=instrument, query_source=query_source, $
    _extra=_extra, $
    jobID=jobID
    
  if N_PARAMS() lt 7 then begin
    if N_PARAMS() lt 6 then begin
      print, 'usage: IRIS_requestAIA, starttime, endtime, xcen, ycen, fovx, fovy [, waves, obsid=obsid, $'
      print, 'email=email, maxframes=maxframes, instrument=instrument, query_source=query_source, $'
      print, '_extra=_extra, $'
      print, 'jobID=jobID]'
      return
    endif else begin
      print, 'no wavelengths defined, downloading all'
      waves=['94','131','171','193','211','304','335','1600','1700','4500','blos','cont']
      print, waves
    endelse
  endif
  if ~keyword_set(instrument) then instrument='aia'
  if ~keyword_set(obsid) then obsid=''
  if ~keyword_set(query_source) then query_source='IRIS_requestAIA'
  notrack=1
    
  ssw_cutout_service,starttime,endtime,query, query_info, waves=strjoin(waves,','), xcen=xcen, ycen=ycen, fovx=fovx, fovy=fovy, $
    /hcr, obstitle=obsid, description='IRIS '+obsid, max_frames=maxframes, $
    query_source=query_source, email=email, notrack=notrack, instrument=instrument, $
    _extra=_extra, DATA_LEVEL=1.5
    
  jobind = where(strmatch(query_info, '*JobID*') eq 1, c1)
  if c1 gt 0 then begin
    res = strsplit(query_info[jobind], '<>', /extract, count=c1a)
    if c1a eq 3 then begin
      jobID = res[1]
      print,'jobID: '+jobID
      statind = where(strmatch(query_info, '*STATUS*') eq 1, c2)
      if c2 gt 0 then begin
        res = strsplit(query_info[statind], '<>', /extract, count=c2a)
        if c2a eq 3 then begin
          status = res[1]
          print, 'Status: '+status
        endif
      endif
    endif
  endif
end
