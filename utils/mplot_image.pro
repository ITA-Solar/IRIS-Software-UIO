pro mplot_image,img, x0,y0,xtitle=xtitle,ytitle=ytitle,title=title,$
     xrange=xrange,yrange=yrange,norm=norm,old=old,interp=interp,$
     nxinterp=nxinterp,nyinterp=nyinterp,min=min,max=max,$
     bgblack=bgblack,bgwhite=bgwhite,true=true,_extra=e
;+
;   mplot_image,image, x,y
;
;            plot image with scales
;
;-
;  the x and y scales are defined by the first and last
;  elements in the arrays x and y so the procedure may be called by
;  plot_image,image,[xmin,xmax],[ymin,ymax]
;  the tickmarks are black and the background white
;  Keywords to plot are inherited
;  Placement of plots with !p.multi are obeyed
;  coordinates x and y are for pixel mid-points, limits are adjusted
;  accordingly. xrange and yrange refer to mid-point coordinates
;  if x and xrange have different directions image will be mirror-imaged
;  same for y and yrange
;  if x is non-equidistant, interpolation is performed to nxinterp pixels
;  if y is non-equidistant, interpolation is performed to nyinterp pixels
;
if(n_params(0) lt 1) then begin
  print,'mplot_image,image ,x,y'
  return
endif
if(n_elements(true) eq 0) then true=0

image=reform(img)
if(n_elements(xtitle) eq 0) then xtitle=''
if(n_elements(ytitle) eq 0) then ytitle=''
if(n_elements(title)  eq 0) then title=''
if(true eq 0) then begin
  nx=n_elements(image[*,0])
  ny=n_elements(image[0,*])
  nchan=1
endif else begin
  if(true eq 1) then begin
    image=transpose(image,[1,2,0])
  endif else if(true eq 2) then begin
    image=transpose(image,[0,2,1])
  endif
  nx=n_elements(image[*,0,0])
  ny=n_elements(image[0,*,0])
  nchan=3
endelse

if(n_elements(old) eq 0) then begin
  if(n_elements(x0) eq 0) then x=[0,nx-1] else x=x0
  if(n_elements(y0) eq 0) then y=[0,ny-1] else y=y0
endif else begin
  if(n_elements(x0) eq 0) then x=[0,nx] else x=x0
  if(n_elements(y0) eq 0) then y=[0,ny] else y=y0
endelse
if(n_elements(nxinterp) eq 0) then nxinterp=1000
if(n_elements(nyinterp) eq 0) then nyinterp=1000

; interpolation if not equidistant in x

if(n_elements(x) gt 2) then begin
  dx=abs(x[1:nx-1]-x[0:nx-2])         ; delta-x
  if((max(dx)-min(dx))/min(dx) gt 1.e-3) then begin
    xold=x
    nxold=n_elements(xold)
    if(n_elements(xrange) eq 0) then begin
      x=findgen(nxinterp)/(nxinterp-1)*(xold[nxold-1]-xold[0])+xold[0]
    endif else begin
      x=findgen(nxinterp)/(nxinterp-1)*(xrange[1]-xrange[0])+xrange[0]
    endelse
    image0=image
    image=fltarr(nxinterp,ny,nchan)
    for j=0,nchan-1 do begin
      for i=0,ny-1 do begin
        image[*,i,j]=interpol(image0[*,i,j],xold,x)
      endfor
    endfor
    nx=nxinterp
  endif
endif

; interpolation in y

if(n_elements(y) gt 2) then begin
  dy=abs(y[1:ny-1]-y[0:ny-2])         ; delta-y
  if((max(dy)-min(dy))/min(dy) gt 1.e-3) then begin
;    print,'interpolating'
    yold=y
    nyold=n_elements(yold)
    if(n_elements(yrange) eq 0) then begin
      y=findgen(nyinterp)/(nyinterp-1)*(yold[nyold-1]-yold[0])+yold[0]
    endif else begin
      y=findgen(nyinterp)/(nyinterp-1)*(yrange[1]-yrange[0])+yrange[0]
    endelse
    image0=image
    image=fltarr(nx,nyinterp,nchan)
    for j=0,nchan-1 do begin
      for i=0,nx-1 do begin
        image[i,*,j]=interpol(reform(image0[i,*,j]),yold,y)
      endfor
    endfor
    ny=nyinterp
  endif
endif

xx=[x(0),x(n_elements(x)-1)]             ; make 2-element array with 
yy=[y(0),y(n_elements(y)-1)]             ; first and last pixel coordinate
xpix=findgen(nx)/(nx-1)*(xx(1)-xx(0))+xx(0) ; store pixel coordinates
ypix=findgen(ny)/(ny-1)*(yy(1)-yy(0))+yy(0)
if(n_elements(interp) eq 0) then interp=0

if((n_elements(xrange) ne 0) or (n_elements(yrange) ne 0)) and $
 (n_elements(old) eq 0) then begin
  if(n_elements(xrange) eq 0) then xrange=xx
  if(n_elements(yrange) eq 0) then yrange=yy
  xpix=findgen(nx)/(nx-1)*(xx(1)-xx(0))+xx(0) ; store pixel coordinates
  ypix=findgen(ny)/(ny-1)*(yy(1)-yy(0))+yy(0)
  iw=where((xpix ge min(xrange)) and (xpix le max(xrange))) ; trim image
  image=image[iw,*,*]
  xx=[xpix(min(iw)),xpix(max(iw))]
  iw=where((ypix ge min(yrange)) and (ypix le max(yrange)))
  image=image[*,iw,*]
  yy=[ypix(min(iw)),ypix(max(iw))]
  if((xrange(1)-xrange(0))*(xx(1)-xx(0)) lt 0.) then begin
    for j=0,nchan-1 do begin
      image[*,*,j]=rotate(image[*,*,j],5)
    endfor
    xx=reverse(xx)
  endif
  if((yrange(1)-yrange(0))*(yy(1)-yy(0)) lt 0.) then begin
    for j=0,nchan-1 do begin
      image[*,*,j]=rotate(image[*,*,j],7)
    endfor
    yy=reverse(yy)
  endif
endif

; adjust xx and yy with half a pixel

if(n_elements(old) eq 0) then begin
  dx=0.5*abs(xpix(1)-xpix(0))
  xx=xx+[-dx,dx]
  dy=0.5*abs(ypix(1)-ypix(0))
  yy=yy+[-dy,dy]
endif

pmulti=!p.multi
black=0
if n_elements(bgblack) eq 0 then bgblack=0
if n_elements(bgwhite) eq 0 then white=!d.n_colors-1 else white=127
if bgblack then begin 
  bgcol=black
  fgcol=white
endif else begin
  bgcol=white
  fgcol=black
endelse

plot,xx,yy,xstyle=1,ystyle=1,/nodata,$    ; set up plotting window and axes
 xtitle=xtitle,ytitle=ytitle,title=title,$
 xrange=xx,yrange=yy,$
 color=fgcol,back=bgcol,_extra=e
pmulti2=!p.multi
!p.multi=pmulti
n_colors=!d.n_colors < 256
if(n_elements(norm) eq 0) then norm=1
if(n_elements(min) eq 0) then begin
  ind=where(finite(image), count)
  if count eq 0 then min=0 $
  else min=min(image[ind])
endif
if(n_elements(max) eq 0) then begin
  ind=where(finite(image),count)
  if count eq 0 then max=0 $
  else max=max(image[ind])
endif
if(norm eq 0) or (true gt 0) then im=image else begin
  im=bytscl(image,min=min,max=max,top=252)
  im=im+2
endelse
if(!d.name eq 'PS') then begin
  if(keyword_set(interp)) then begin
    size_im=float(size(im))
    nx=1000
    ny=nx*size_im[2]/size_im[1]
    im2=fltarr(nx,ny,nchan)
    for j=0,nchan-1 do begin
      im2[*,*,j]=congrid(im[*,*,j],nx,ny,/interp)
    endfor
    if(true eq 1) then begin
      im2=transpose(im2,[2,0,1])
    endif else if(true eq 2) then begin
      im2=transpose(im2,[0,2,1])
    endif
    tv,im2,!x.window(0),!y.window(0),$    ; display image
     xsize=!x.window(1)-!x.window(0),$
     ysize=!y.window(1)-!y.window(0),$
     /norm,true=true
  endif else begin
    if(true eq 1) then begin
      im=transpose(im,[2,0,1])
    endif else if(true eq 2) then begin
      im=transpose(im,[0,2,1])
    endif
    tv,im,!x.window(0),!y.window(0),$    ; display image
     xsize=!x.window(1)-!x.window(0),$
     ysize=!y.window(1)-!y.window(0),$
     /norm,true=true
  endelse
endif else begin
  px=!x.window*!d.x_vsize
  py=!y.window*!d.y_vsize
  im2=fltarr(px[1]-px[0],py[1]-py[0],nchan)
  for j=0,nchan-1 do begin
    im2[*,*,j]=congrid(im[*,*,j],px(1)-px(0),py(1)-py(0),interp=interp)  ; regrid image to fit plot window
  endfor
;  if(interp ne 0) then begin
;    shiftx=(float(px[1]-px[0])/n_elements(im[*,0])-1.)/2.
;    shifty=(float(py[1]-py[0])/n_elements(im[0,*])-1.)/2.
;    im2=shiftf(im2,shiftx,shifty)
;  endif
  if(true eq 1) then begin
    im2=transpose(im2,[2,0,1])
  endif else if(true eq 2) then begin
    im2=transpose(im2,[0,2,1])
  endif
  tv,im2,!x.window(0),!y.window(0),$    ; display image
   xsize=!x.window(1)-!x.window(0),$
   ysize=!y.window(1)-!y.window(0),$
   /norm,true=true
endelse
plot,xx,yy,xstyle=1,ystyle=1,/nodata,$    ; plot axes again
 xtitle=xtitle,ytitle=ytitle,title=title,$
 xrange=xx,yrange=yy,$
 color=fgcol,back=bgcol,/noerase,_extra=e
!p.multi=pmulti2

end

  
