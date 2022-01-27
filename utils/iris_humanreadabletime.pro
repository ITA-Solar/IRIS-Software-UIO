function IRIS_humanreadabletime, t, totaltimetextlength=totaltimetextlength
  ; $Id: iris_humanreadabletime.pro,v 1.6 2014/01/17 11:55:40 mawiesma Exp $ ;
  timestr=''
  ts=t
  if ts ge 59.95 then begin
    tm = floor((ts+0.05)/60.0)
    ts = abs(ts - tm*60.0)
    if tm ge 60 then begin
      th = floor(tm/60.0)
      tm = tm - th*60
      timestr = timestr + strcompress(string(th), /remove_all) + ' h '
    endif
    timestr = timestr + string(tm, format='(I2)') + ' m '
  endif
  timestr = timestr + string(ts, format='(F4.1)') + ' s'
  if keyword_set(totaltimetextlength) then $
    while strlen(timestr) lt totaltimetextlength do timestr=' '+timestr
  return, timestr
end
