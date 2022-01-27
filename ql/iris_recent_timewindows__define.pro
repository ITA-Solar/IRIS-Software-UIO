;+
; NAME:
;       IRIS_recent_timewindows
;
; PURPOSE:
;
;       IRIS_recent_timewindows is an object that contains
;       recent time windows that were used in the searches.
;       If there is a new search, this object reorders the 
;       time windows, and adds and/or deletes time windows
;       if necessary
;
;
; CATEGORY:
;       IRIS Data analysis
;
; CALLING SEQUENCE:
;       result = iris_xfiles_editpatternwidget(spatterns, mainwindow)
;
; INPUTS:
;       starttimes: a list of starttimes
;       endtimes: a list of endtimes
;
; KEYWORD PARAMETERS:
;       none
;
;
; OUTPUTS:
;
; CALLS:
;
;
; COMMON BLOCKS:
;
;
;
; RESTRICTIONS:
;
;
; MODIFICATION HISTORY:
;       2013:   Martin Wiesmann
;
; $Id: iris_recent_timewindows__define.pro,v 1.2 2020/01/30 09:07:20 mawiesma Exp $  ;

FUNCTION IRIS_recent_timewindows::init, starttimes, endtimes

  self.maxnum = 10
  
  n = min([N_ELEMENTS(starttimes), N_ELEMENTS(endtimes)])
  if n gt self.maxnum then n=self.maxnum
  self.num = n
  if n gt 0 then begin
    self.starttimes[0:n-1] = starttimes[0:n-1]
    self.endtimes[0:n-1] = endtimes[0:n-1]
    self.starts[0:n-1] = str2utc(starttimes[0:n-1])
    self.ends[0:n-1] = str2utc(endtimes[0:n-1])
    self.window[0:n-1] = utc2str(self.starts[0:n-1], /date_only, /VMS) + ' - ' + utc2str(self.ends[0:n-1], /date_only, /VMS)
  endif
  
  return, 1
end


pro IRIS_recent_timewindows::cleanup
end


pro IRIS_recent_timewindows::gettimes, starttimes, endtimes, index=index
  if N_ELEMENTS(index) gt 0 then begin
    starttimes = self.starttimes[index]
    endtimes = self.endtimes[index]
  endif else begin
    starttimes = self.starttimes[0:self.num-1]
    endtimes = self.endtimes[0:self.num-1]
  endelse
end


function IRIS_recent_timewindows::getwindows
  return, self.window[0:self.num-1]
end


pro IRIS_recent_timewindows::newsearch, starttime, endtime
  startt = str2utc(starttime)
  endt = str2utc(endtime)
  windt = utc2str(startt, /date_only, /VMS) + ' - ' + utc2str(endt, /date_only, /VMS)
  temp = indgen(self.maxnum)
  ind = where(self.window eq windt, count)
  
  if count gt 0 then begin
    if ind gt 0 then begin
      ;need to change the order of the timewindows
      temp[1:ind] = temp[1:ind] - 1
      temp[0] = ind
    endif
  endif else begin
    temp[1:self.maxnum-1] = temp[1:self.maxnum-1] - 1
    if self.num lt self.maxnum then self.num=self.num+1
  endelse
  
  self.starttimes = self.starttimes[temp]
  self.endtimes = self.endtimes[temp]
  self.starts = self.starts[temp]
  self.ends = self.ends[temp]
  self.window = self.window[temp]
  
  self.starttimes[0] = starttime
  self.endtimes[0] = endtime
  self.starts[0] = startt
  self.ends[0] = endt
  self.window[0] = windt
end


pro IRIS_recent_timewindows__define

  maxnum = 10
  
  timestruct = {CDS_INT_TIME, $
    MJD:0L, $
    TIME:0L}
    
  void = {IRIS_recent_timewindows, $
    maxnum:maxnum, $
    num:0, $
    starttimes:strarr(maxnum), $
    endtimes:strarr(maxnum), $
    starts:make_array(maxnum, value=timestruct), $
    ends:make_array(maxnum, value=timestruct), $
    window:strarr(maxnum) $
    }
    
end
