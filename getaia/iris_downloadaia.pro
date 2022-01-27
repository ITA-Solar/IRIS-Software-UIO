; $Id: iris_downloadaia.pro,v 1.11 2016/06/17 08:55:47 mawiesma Exp $  ;

pro IRIS_downloadAIA, outdir, jobID, twait=twait

  if N_PARAMS() lt 2 then begin
    print, 'usage: IRIS_downloadAIA, starttime, endtime, xcen, ycen, fovx, fovy [, waves, obsid=obsid, obsdir=obsdir, $'
    print, 'outdir=outdir, email=email, maxframes=maxframes, deletetempfiles=deletetempfiles, $'
    print, 'createl3=createl3, instrument=instrument, $'
    print, '_extra=_extra]'
    return
  endif
  
  status=''
  n=0L
  t1=systime(1)
  if n_elements(status) gt 0 then begin
    while status ne 'finished' do begin
      urls=''
      wt=60-n
      if wt lt 15 then wt=15
      if n gt 0 then wait,wt
      sock_list,'http://www.lmsal.com/solarsoft/ssw_service/queue/finished/'+jobid,urls
      if N_ELEMENTS(urls) gt 0 && urls[0] ne '' then status='finished' else begin
        sock_list,'http://www.lmsal.com/solarsoft/ssw_service/queue/requested/'+jobid,urls
        if N_ELEMENTS(urls) gt 0 && urls[0] ne '' then status='queued' else begin
          sock_list,'http://www.lmsal.com/solarsoft/ssw_service/queue/current/'+jobid,urls
          if N_ELEMENTS(urls) gt 0 && urls[0] ne '' then status='running' else status='unknown'
        endelse
      endelse
      t2=systime(1)
      n=n+1L
      print,'status: '+status+', attempt: '+string(n,format='(I4)')+'  time: '+IRIS_humanreadabletime(t2-t1)
    endwhile
  endif
  twait = t2-t1
  
  destination = outdir+'original/'
  file_mkdir, destination
  
  ssw_service_get_data, jobID, out_dir=destination, /loud
end
