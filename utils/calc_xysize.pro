pro calc_xysize,xpsize,ypsize,xsize,ysize ,nxchar=nxchar,nychar=nychar,xmax=xmax,ymax=ymax
;+
;   calc_xysize,xpsize,ypsize,xsize,ysize ,nxchar=nxchar,nychar=nychar,xmax=xmax,ymax=ymax
;
;            calculates xsize and ysize (in pixels or cm depending on device)
;            to get correct aspect ratio given constant pixel size in x and y
;            xpsize   xsize for data area only
;            ypsize   ysize for data area only
;            xsize    xsize including annotation areas
;            ysize    ysize including annotation areas
;            nxchar   number of characters in x, default total(xmargin)
;            nychar   number of characters in y, default total(ymargin)
;            xmax     maximum size of xsize allowed
;            ymax     maximum size of ysize allowed
;
;-
if(n_params() lt 4) then begin
  print,'calc_xysize,xpsize,ypsize,xsize,ysize ,nxchar=nxchar,nychar=nychar,$'
  print,'  xmax=xmax,ymax=ymax'
  return
endif

if(n_elements(nxchar) eq 0) then nxchar=total(!x.margin)
if(n_elements(nychar) eq 0) then nychar=total(!y.margin)

; find space used for annotation

if(!p.charsize ne 0) then pcharsize=!p.charsize else pcharsize=1.0
xannot=nxchar*!d.x_ch_size*pcharsize
yannot=nychar*!d.y_ch_size*pcharsize
if(!d.name eq 'PS') then begin
  xannot=xannot/!d.x_px_cm
  yannot=yannot/!d.y_px_cm
endif

; add annotation space to data area

xsize=xpsize+xannot
ysize=ypsize+yannot

if(n_elements(xmax) ne 0) then begin
  if(xsize gt xmax) then begin
    ysize=(xmax-xannot)*ypsize/xpsize+yannot
    xsize=xmax
  Endif
endif
if(n_elements(ymax) ne 0) then begin
  if(ysize gt ymax) then begin
    xsize=(ymax-yannot)*xpsize/ypsize+xannot
    ysize=ymax
  endif
endif

end
