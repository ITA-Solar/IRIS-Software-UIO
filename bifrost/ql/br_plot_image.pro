pro BR_PLOT_IMAGE,IMAGE,X,Y,ORIGIN=ORIGIN,SCALE=SCALE,NOERASE=NOERASE,	$
		NOSQUARE=NOSQUARE,SMOOTH=SMOOTH,NOSCALE=NOSCALE,	$
		MISSING=MISSING,COLOR=COLOR,MAX=MAX,MIN=MIN,TOP=TOP,	$
		BOTTOM=BOTTOM,VELOCITY=VELOCITY,COMBINED=COMBINED,      $
                LOWER=LOWER,NOADJUST=NOADJUST,TITLE=TITLE,XTITLE=XTITLE,$
                YTITLE=YTITLE,CHARSIZE=CHARSIZE,XTICK_GET=XTICK_GET,    $
                YTICK_GET=YTICK_GET,TRUE=K_TRUE,BSCALED=BSCALED,        $
                _EXTRA=_EXTRA,XRANGE=XRANGE,YRANGE=YRANGE,FIXNZ=FIXNZ,    $
                YLOG=YLOG,NOSTRETCH=NOSTRETCH,LOGIM=LOGIM,LOGZERO=LOGZERO

IF N_PARAMS() EQ 0 THEN BEGIN
   print,'BR_PLOT_IMAGE,IMAGE,X,Y,ORIGIN=ORIGIN,SCALE=SCALE,NOERASE=NOERASE,	'$
         +'NOSQUARE=NOSQUARE,SMOOTH=SMOOTH,NOSCALE=NOSCALE,	'$
         +'MISSING=MISSING,COLOR=COLOR,MAX=MAX,MIN=MIN,TOP=TOP,	'$
         +'BOTTOM=BOTTOM,VELOCITY=VELOCITY,COMBINED=COMBINED,      '$
         +'LOWER=LOWER,NOADJUST=NOADJUST,TITLE=TITLE,XTITLE=XTITLE,'$
         +'YTITLE=YTITLE,CHARSIZE=CHARSIZE,XTICK_GET=XTICK_GET,    '$
         +'YTICK_GET=YTICK_GET,TRUE=K_TRUE,BSCALED=BSCALED,YLOG=YLOG,'$
         +'_EXTRA=_EXTRA,XRANGE=XRANGE,YRANGE=YRANGE,NOSTRETCH=NOSTRETCH' $
         +',LOGIM=LOGIM,LOGZERO=LOGZERO'
   return
END

imageinit = image
  

if n_elements(logim) ne 0 then begin
   image2=image
   var1=alog10(reform(image))
   var2=alog10(-reform(image))
   image=var1*0.0
   if n_elements(logzero) eq 0 then logzero=-3
   image(where(var2 gt logzero))=-var2(where(var2 gt logzero))+logzero                    
   image(where(var1 gt logzero))=var1(where(var1 gt logzero))-logzero
   image(where(image ne image)) = 0.0
endif
IF N_PARAMS() EQ 1 THEN BEGIN

   if n_elements(xrange) ne 0 then begin
      xpos0=min([max([br_locval(indgen(n_elements(image(*,0))),xrange(0),/fix),0]),n_elements(image(*,0))-1])
      xpos1=min([max([br_locval(indgen(n_elements(image(*,0))),xrange(1),/fix),0]),n_elements(image(*,0))-1])
      if xpos0 eq xpos1 then begin
         print,'Warning: xrange does not match with the range of the x axis'
         return
      endif
       if xpos0 lt xpos1 then image=image(xpos0:xpos1,*) else image=image(xpos1:xpos0,*)
      origin(0)=xrange(0)
   endif

   if n_elements(yrange) ne 0 then begin
      ypos0=min([max([br_locval(indgen(n_elements(image(0,*))),yrange(0),/fix),0]),n_elements(image(0,*))-1])
      ypos1=min([max([br_locval(indgen(n_elements(image(0,*))),yrange(1),/fix),0]),n_elements(image(0,*))-1])
      if ypos0 eq ypos1 then begin
         print,'Warning: yrange does not match with the range of the y axis'
         return
      endif
      image=image(*,ypos0:ypos1)
      origin(1)=yrange(0)
   endif

   PLOT_IMAGE,reform(IMAGE),ORIGIN=ORIGIN,SCALE=SCALE,NOERASE=NOERASE,	$
		NOSQUARE=NOSQUARE,SMOOTH=SMOOTH,NOSCALE=NOSCALE,	$
		MISSING=MISSING,COLOR=COLOR,MAX=MAX,MIN=MIN,TOP=TOP,	$
		BOTTOM=BOTTOM,VELOCITY=VELOCITY,COMBINED=COMBINED,      $
                LOWER=LOWER,NOADJUST=NOADJUST,TITLE=TITLE,XTITLE=XTITLE,$
                YTITLE=YTITLE,CHARSIZE=CHARSIZE,XTICK_GET=XTICK_GET,    $
                YTICK_GET=YTICK_GET,TRUE=K_TRUE,BSCALED=BSCALED,_EXTRA=_EXTRA
end else begin
   xinit = x
   yinit = y

   if  N_PARAMS() NE 3 then MESSAGE,'Syntax:  BR_PLOT_IMAGE, IMAGE,X,Y'

   if n_elements(ylog) ne 0 then begin
      if min(y) gt 0. then begin
         y1=alog10(y)  
         ystold=!y.style
         !y.style=5
      endif else begin
         print,'Warning: /ylog: y is negative or 0'
         return
      endelse
   endif else begin
      y1=y
   endelse
 
   dx=x(1)-x(0)
   nx=n_elements(x)
   ny=n_elements(y1)
   dy=y1-shift(y1,1)
   if abs((max(dy(1:ny-2))-min(dy(1:ny-2)))/mean(dy(1:ny-2))) ge 1.e-4 then begin
      if dy[3] le 0 then begin
         br_stretch,reform(image),nx,1,abs(dx),-y1,imgst,zst=zst,dzt=min(-dy(1:ny-2)),fixnz=fixnz
         zst=-zst
      endif else begin
         br_stretch,reform(image),nx,1,abs(dx),y1,imgst,zst=zst,dzt=min(dy(1:ny-2)),fixnz=fixnz
      endelse
   endif else begin
      imgst=reform(image)
      zst=y
   endelse

   dy=x-shift(x,1)
   nz=n_elements(zst)
   if abs((max(dy(1:nx-2))-min(dy(1:nx-2)))/mean(dy(1:nx-2))) ge 1.e-4 then begin
      if dy[3] le 0 then begin
         dx=zst(1)-zst(0)
         br_stretch,transpose(imgst),nz,1,abs(dx),-x,imgst2,zst=xst,dzt=min(-dy(1:nx-2)),fixnz=fixnz
         imgst=reform(transpose(imgst2))
         dx=-(xst(1)-xst(0))
         xst=-(xst)
      endif else begin
         dx=zst(1)-zst(0)
         br_stretch,transpose(imgst),nz,1,abs(dx),x,imgst2,zst=xst,dzt=min(dy(1:nx-2)),fixnz=fixnz
         imgst=reform(transpose(imgst2))
         dx=xst(1)-xst(0)
      endelse
   endif else begin
      xst=x
   endelse
   nxst=n_elements(xst)
   dy=zst(1)-zst(0)
   if dy lt 0 then begin
      if dx lt 0 then begin
         imgst=rotate(imgst,2)
         dy=-dy & dx=-dx
         xst=reverse(xst) & zst=reverse(zst) 
      endif else begin
         imgst=rotate(imgst,7)
         dy=-dy
         zst=reverse(zst) 
      endelse
   endif else begin
      if dx lt 0 then begin
         imgst=rotate(imgst,5)
         dx=-dx
         xst=reverse(xst) 
      endif 
   endelse

   if n_elements(xrange) eq 0 then xrange=[xst(0),xst(nxst-1)]
   if n_elements(yrange) eq 0 then yrange=[zst(0),zst(nz-1)]

   if yrange(0) gt yrange(1) then begin
      imgst=rotate(imgst,7)
      dy=-dy
      zst=reverse(zst) 
   endif
   if xrange(0) gt xrange(1) then begin
      imgst=rotate(imgst,5)
      dx=-dx
      x=reverse(x) 
   endif
   scale=[dx,dy]
   origin=[x(0),zst(0)]

   xpos0=min([max([br_locval(xst,xrange(0),/fix),0]),nxst-1])
   xpos1=min([max([br_locval(xst,xrange(1),/fix),0]),nxst-1])
   if xpos0 eq xpos1 then begin
      print,'Warning: xrange does not match with the range of the x axis'
      return
   endif

   if xpos0 lt xpos1 then begin
      imgst=imgst(xpos0:xpos1,*) 
      if xst(xpos0) gt xrange(0) and abs(xst(xpos0)- xrange(0)) ge scale(0) then begin
         niter=abs(fix((xrange(0)-xst(xpos0))/scale(0)))
         imgst2=findgen(niter+n_elements(imgst(*,0)),n_elements(imgst(0,*)))*0+min(imgst)
         imgst2(niter:*,*)=imgst
         imgst=imgst2
      endif
      if xst(xpos1) lt xrange(1) and abs(xst(xpos1)- xrange(1)) ge scale(0) then begin
         niter=abs(fix((xrange(1)-xst(xpos1))/scale(0)))
         imgst2=findgen(niter+n_elements(imgst(*,0)),n_elements(imgst(0,*)))*0.+min(imgst)
         imgst2(0:n_elements(imgst(*,0))-1,*)=imgst
         imgst=imgst2
      endif
   endif else begin 
      imgst=imgst(xpos1:xpos0,*)
      if xst(xpos0) lt xrange(1) and abs(xst(xpos0)- xrange(1)) ge scale(0) then begin
         niter=abs(fix((xrange(1)-xst(xpos0))/scale(0)))
         imgst2=findgen(niter+n_elements(imgst(*,0)),n_elements(imgst(0,*)))*0+min(imgst)
         imgst2(0:n_elements(imgst(*,0))-1,*)=imgst
         imgst=imgst2
      endif
      if xst(xpos1) gt xrange(0) and abs(xst(xpos1)- xrange(0)) ge scale(0) then begin
         niter=abs(fix((xrange(0)-xst(xpos0))/scale(0)))
         imgst2=findgen(niter+n_elements(imgst(*,0)),n_elements(imgst(0,*)))*0.+min(imgst)
         imgst2(niter:*,*)=imgst
         imgst=imgst2
      endif
   endelse
   origin(0)=xrange(0)

   ypos0=min([max([br_locval(zst,yrange(0),/fix),0]),nz-1])
   ypos1=min([max([br_locval(zst,yrange(1),/fix),0]),nz-1])
   if ypos0 eq ypos1 then begin
      print,'Warning: yrange does not match with the range of the y axis'
      return
   endif
   if ypos0 lt ypos1 then begin
      imgst=imgst(*,ypos0:ypos1)
      if zst(ypos0) gt yrange(0) and abs(zst(ypos0)- yrange(0)) ge scale(1) then begin
         niter=abs(fix((yrange(0)-zst(ypos0))/scale(1)))
         imgst2=findgen(n_elements(imgst(*,0)),niter+n_elements(imgst(0,*)))*0+min(imgst)
         imgst2(*,niter:*)=imgst
         imgst=imgst2
      endif
      if zst(ypos1) lt yrange(1) and abs(zst(ypos1)- yrange(1)) ge scale(1) then begin
         niter=abs(fix((yrange(1)-zst(ypos1))/scale(1)))
         imgst2=findgen(n_elements(imgst(*,0)),niter+n_elements(imgst(0,*)))*0.+min(imgst)
         imgst2(*,0:n_elements(imgst(0,*))-1)=imgst
         imgst=imgst2
      endif 
   endif else begin
      imgst=imgst(*,ypos1:ypos0)
     if zst(ypos0) lt yrange(1) and  abs(zst(ypos0)- yrange(1)) ge scale(1) then begin
         niter=abs(fix((yrange(1)-zst(ypos0))/scale(1)))
         imgst2=findgen(n_elements(imgst(*,0)),niter+n_elements(imgst(0,*)))*0+min(imgst)
         imgst2(*,0:n_elements(imgst(0,*))-1)=imgst
         imgst=imgst2
      endif
      if zst(ypos1) gt yrange(0) and  abs(zst(ypos1)- yrange(0)) ge scale(1) then begin
         niter=abs(fix((yrange(0)-zst(ypos0))/scale(1)))
         imgst2=findgen(n_elements(imgst(*,0)),niter+n_elements(imgst(0,*)))*0.+min(imgst)
         imgst2(*,niter:*)=imgst
         imgst=imgst2
      endif
   endelse
   if n_elements(ylog) ne 0 then origin(1)= alog10(yrange(0)) else origin(1)=yrange(0)


   PLOT_IMAGE,reform(IMGST),ORIGIN=ORIGIN,SCALE=SCALE,NOERASE=NOERASE,	$
		NOSQUARE=NOSQUARE,SMOOTH=SMOOTH,NOSCALE=NOSCALE,	$
		MISSING=MISSING,COLOR=COLOR,MAX=MAX,MIN=MIN,TOP=TOP,	$
		BOTTOM=BOTTOM,VELOCITY=VELOCITY,COMBINED=COMBINED,      $
                LOWER=LOWER,NOADJUST=NOADJUST,TITLE=TITLE,XTITLE=XTITLE,$
                YTITLE=YTITLE,CHARSIZE=CHARSIZE,XTICK_GET=XTICK_GET,    $
                YTICK_GET=YTICK_GET,TRUE=K_TRUE,BSCALED=BSCALED,_EXTRA=_EXTRA
   
   if n_elements(ylog) ne 0 then begin
      !y.style=ystold

      if n_elements(xlog) ne 0 then begin
         if n_elements(xrange) ne 0 then xpos=alog10(xrange(0)-scale(0)/2.) else xpos=min(x)-scale(0)/2.
      endif else begin
         if n_elements(xrange) ne 0 then xpos=xrange(0)-scale(0)/2. else xpos=min(x)-scale(0)/2.
      endelse
      ypos=min(y1)
      if n_elements(yrange) ne 0 then endpos=yrange(1) else endpos=max(y)
      if n_elements(yrange) ne 0 then initpos=yrange(0) else initpos=min(y)
      axis,xpos,ypos,yaxis=0,/ylog,COLOR=COLOR,YTITLE=YTITLE,yrange=[initpos,endpos],$
                CHARSIZE=CHARSIZE,XTICK_GET=XTICK_GET,_EXTRA=_EXTRA,/yst

      if n_elements(xlog) ne 0 then begin
         if n_elements(xrange) ne 0 then xpos=alog10(xrange(1)+scale(0)/2.) else xpos=max(x) +scale(0)/2.
      endif else begin
         if n_elements(xrange) ne 0 then xpos=xrange(1)+scale(0)/2. else xpos=max(x)+scale(0)/2.
      endelse
      axis,xpos,ypos,yaxis=1,/ylog,COLOR=COLOR,yrange=[initpos,endpos],$
               CHARSIZE=1.e-4,XTICK_GET=XTICK_GET,_EXTRA=_EXTRA,/yst
   endif
   x = xinit 
   y = yinit 
endelse
if n_elements(logim) ne 0 then image=image2


  image = imageinit
  

end
