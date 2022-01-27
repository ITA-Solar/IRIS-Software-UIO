FUNCTION IRISsim_strtime, time, seconds=seconds
  ;+
  ; NAME:
  ;       STRTIME
  ; PURPOSE:
  ;       convert a number, representing miliseconds into a string
  ; CATEGORY:
  ;
  ; CALLING SEQUENCE:
  ;       result = STRTIME(time [,/seconds])
  ; INPUTS:
  ;       TIME: the time in miliseconds
  ; KEYWORDS:
  ;       SECONDS : (Flag) return time as a string of seconds
  ; OUTPUTS:
  ;       returns the time as a string in the format:
  ;       HH:MM:SS:MSMS
  ;       or if the SECONDS keyword is set: SSSSS.MSMS
  ; MODIFICATION HISTORY:
  ;       06-Jul-2012  M.Wiesmann, UIO
  ;-
; $Id: irissim_strtime.pro,v 1.2 2013/04/25 20:19:46 mawiesma Exp $  ;


  if keyword_set(seconds) then $
    strtime=strtrim(string(fix((time)/1000.),format='(i5.5)'),2)+'.' $
    +strtrim(string(fix((time) mod 1000.),format='(i3.3)'),2) $
    
  else $
  strtime=strtrim(string(fix((time)/60./60./1000.),format='(i2.2)'),2)+':' $
    +strtrim(string(fix((time)/60./1000. mod 60),format='(i2.2)'),2)+':' $
    +strtrim(string(fix((time)/1000. mod 60.),format='(i2.2)'),2)+'.' $
    +strtrim(string(fix((time) mod 1000.),format='(i3.3)'),2)
    
  return,strtime
end
