pro iris_limb_fit,im_in,date,x,y,level=level,pix=pix,verbose=verbose,fita=fita,aain=aain,irot=irot,x0=x0,x1=x1
;+
;   Limb_fit,im_in,date,x,y,level=level,/verbose
;
;            fits limb and returns x and y for solar center (in
;            pixels)
;
;   $Id: iris_limb_fit.pro,v 1.1 2014/01/28 13:11:05 viggoh Exp $
;-
if(n_params() lt 4) then begin
  message,'syntax: iris_limb_fit,im_in,date,x,y,level=level,pix=pix,/verbose',/info
  return
endif

if(n_elements(fita) eq 0) then fita=[1,1,0]
if(n_elements(pix) eq 0) then begin
  print,'give pixel size in arcseconds (e.g. 0.0544, 0.1088)'
  read,pix
endif

; rotate image to orient it as north limb
Siz=size(im_in)
nx=siz[1]
ny=siz[2]
if(n_elements(irot) eq 0) then begin
  edge=fltarr(4)  ; find which way limb is going
  edge[0]=mean(im_in[*,ny-5:ny-1])  ; N limb
  edge[1]=mean(im_in[nx-5:nx-1,*])  ; E limb
  edge[2]=mean(im_in[*,0:4])        ; S limb
  edge[3]=mean(im_in[0:4,*])        ; W limb
  dum=min(edge,irot)             ; find limb
endif
im=rotate(im_in,irot)

Siz=size(im)
nx=siz[1]
ny=siz[2]

if(n_elements(level) eq 0) then begin
; set limb intensity level to half of the intensity of the disk 
 dum=reform(smooth(im[nx/2,*],5))
 Level=0.5*mean(dum[0:50])
endif else if(level lt 2) then begin
; set limb intensity level to half of the intensity of the disk*level
 dum=reform(smooth(im[nx/2,*],5))
 Level=0.5*mean(dum[0:50])*level
endif

wc=fltarr(nx)
for i=0,nx-1 do begin
;   iw=where(im[i,*] lt level)
  Wc[i]=(where(abs(im[i,*]-level) eq min(abs(im[i,*]-level)),ndum))[0]
endfor

wc2=wc
rr=pb0r(date)*60./pix
rr=rr[2]

; Make first guess, first assume center is above in y

if(n_elements(x0) eq 0) then begin
  x0=min(where(im[*,10] gt 0.9*mean(im[*,10])))
endif
if(n_elements(x1) eq 0) then begin
  x1=max(where(im[*,10] gt 0.9*mean(im[*,10])))
endif
y0=wc[x0]
y1=wc[x1]
d2=sqrt(float(x1-x0)^2+float(y1-y0)^2)/2
a2=acos(d2/rr)
d1=(y1-y0)/2.
a1=asin(d1/d2)
aa=fltarr(3)
aa[0]=rr*cos(a1+a2)+x0
aa[1]=rr*sin(a1+a2)+y0
aa[2]=rr

; center is below in y

aa[0]=(x0+x1)-aa[0]
aa[1]=(y0+y1)-aa[1]

if(keyword_set(verbose)) then print,'first guess (x,y,r)=',aa

ncirc=x1-x0+1              ; number of points to fit in circle
ww=fltarr(ncirc)+1.      ; equal weight
xdum=findgen(ncirc)+x0
dum=smooth(wc[x0:x1],11,/edge_truncate)
if(n_elements(aain) ne 0) then aa=aain
odum=curvefit(xdum,dum,ww,aa,fita=fita,func='circle_min')
if(keyword_set(verbose)) then print,'final guess (x,y,r)=',aa

case irot of
  0 : begin
        x=aa[0]
        y=aa[1]
      end
  1 : begin
        x=aa[1]
        y=nx-1-aa[0]
      end
  2 : begin
        x=nx-1-aa[0]
        y=ny-1-aa[1]
      end
  3 : begin
        x=nx-1-aa[1]
        y=aa[0]
      end
endcase  

if(keyword_set(verbose)) then begin
  npt=1000
  mag=500./nx < 500./ny
  mwin,nx*mag,ny*mag,30
  mplot_image,im_in,/norm
  xx=cos(findgen(npt)/(npt-1)*2*!pi)*aa[2]+x
  yy=sin(findgen(npt)/(npt-1)*2*!pi)*aa[2]+y
  oplot,xx,yy
  text=''
  read,'<cr> to continue',text
  wdelete,30
  if(text eq 'q') then verbose=0
endif

End
