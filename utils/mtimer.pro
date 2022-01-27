pro mtimer,text,t,nt,start=start,stop=stop,remaining=remaining
;+
;   mtimer,text,t,nt,start=start,remaining=remaining
;
;      /start will set the clock to zero
;      /remaining prints remaining time
;-
common cmtimer,time0

if(keyword_set(start)) then begin
  time0=systime(/sec)
  print,''
  return
endif
if(keyword_set(stop)) then begin
  print,''
  return
endif

if(n_params() lt 3) then begin
  message,'mtimer,text,t,nt,/start,/end,/remaining',/info
  return
endif

msg=string(13b)+text+' % finished: '+string((t+1.)*100./nt,format='(f4.0) ')
if(keyword_set(remaining)) then begin
  rest=(systime(/sec)-time0)/float(t+1)*(nt-1-t)
  restm=fix(rest/60.)
  rests=fix(rest-restm*60)
  resth=fix(restm/60.)
  restm=restm-resth*60
  msg=msg+' remaining: '+string(resth,restm,rests,format="(i2,':',i2,':',i2)")
endif

print,msg,format='(a,$)'

end
