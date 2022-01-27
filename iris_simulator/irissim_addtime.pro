FUNCTION IRISsim_addTime, baseTime, addTime, seconds=seconds
; $Id: irissim_addtime.pro,v 1.3 2014/09/05 14:25:50 mawiesma Exp $  ;

  endTime = anytim2utc(baseTime)
  if keyword_set(seconds) then addTime2=addTime*1000 else addTime2=addTime
  endTime.time = endTime.time + addTime2
  dayms = 86400000l
  if endTime.time ge dayms then begin
    endTime.mjd = endTime.mjd + endTime.time/dayms
    endTime.time = endTime.time MOD dayms
  endif
  return, endTime
END
