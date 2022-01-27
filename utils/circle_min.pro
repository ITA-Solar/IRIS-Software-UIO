PRO circle_min,x,a,f,pder

rr=a[2]
if(a[1] gt 200.) then begin
  f = a[1]-sqrt((rr)^2-(x-a[0])^2)
  pder=[[-(x-a[0])/sqrt((rr)^2-(x-a[0])^2)],[1.+x*0.],[x*0.]]
endif else begin
  f = a[1]+sqrt((rr)^2-(x-a[0])^2)
  pder=[[(x-a[0])/sqrt((rr)^2-(x-a[0])^2)],[1.+x*0.],[x*0.]]
endelse

END
