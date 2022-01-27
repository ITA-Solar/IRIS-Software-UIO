
;display image in the draw window:
pro xmhd_draw, event

  widget_control, event.top, get_uvalue = info

  if strupcase(!d.name) ne 'PS' and strupcase(!d.name) ne 'Z' then  wset, (*info).wid
;  widget_control, (*info).drawid, xsize = (*info).d_xsz, $
;      ysize = (*info).d_ysz, draw_xsize = (*info).d_xsz, $
;      draw_ysize = (*info).d_ysz

  drawimage = reform(*(*info).data->getvar((*info).line))
  sz = size(drawimage)
  nx=sz[1] & ny=sz[2] & nz=sz[3]

  case (*info).mode of
       0: begin
              xax=*(*info).aux->getxtitle()
              yax=*(*info).aux->getytitle()
              zax=*(*info).aux->getxrastertitle()
              drawimage = reform(drawimage[*,*,(*info).rpos])
          end
       1: begin
              xax=*(*info).aux->getytitle()
              yax=*(*info).aux->getxrastertitle()
              zax=*(*info).aux->getxtitle()
              drawimage = reform(drawimage[(*info).rpos,*,*])
          end
       2: begin
              xax=*(*info).aux->getxtitle()
              yax=*(*info).aux->getxrastertitle()
              zax=*(*info).aux->getytitle()
              drawimage = reform(drawimage[*,(*info).rpos,*])
          end
    endcase

  (*info).xtitle = xax
  revdim=0

  case strupcase(strmid(xax,0,1)) of
    'X': xscale = *(*info).hdr->getx()
    'Y': xscale = *(*info).hdr->gety()
    'Z': begin
         xscale = *(*info).hdr->getz()
         revdim=1
         end
    'T': xscale = *(*info).hdr->gett()
    else:
  endcase

  (*info).ytitle = yax
  case strupcase(strmid(yax,0,1)) of
    'X': yscale = *(*info).hdr->getx()
    'Y': yscale = *(*info).hdr->gety()
    'Z': begin
         yscale = *(*info).hdr->getz()
         revdim = 2
         end
    'T': yscale = *(*info).hdr->gett()
    else:
  endcase

  case strupcase(strmid(zax,0,1)) of
    'X': zscale = *(*info).hdr->getx()
    'Y': zscale = *(*info).hdr->gety()
    'Z': begin
         zscale = *(*info).hdr->getz()
         end
    'T': zscale = *(*info).hdr->gett()
    else:
 endcase

  if (*info).absoption then begin
     drawimage=abs(drawimage)
  endif

  if (*info).lgoption then begin
     eps=1.e-30
     rr=where(drawimage gt 0,count)
     if (count gt 0) then drawimage=drawimage > min(drawimage[rr])
     drawimage=alog10(drawimage)
  endif

  if (*info).expoption then begin
     drawimage=10^(drawimage)
  endif

  if (sz[0] eq 1) then begin

    if (ny eq 1) then begin
      plot,xscale,reform(drawimage),position=imagepos,xstyle=1,ystyle=1, $
        xtitle = (*info).xtitle,ytitle=*(*info).data->getvarname()
    endif

    if (nx eq 1) then begin
      plot,yscale,reform(drawimage),position=imagepos,xstyle=1,ystyle=1, $
        xtitle = (*info).ytitle,ytitle=*(*info).data->getvarname()
    endif

    (*info).drawimage = ptr_new(fltarr(sz[1], sz[2]))
    *(*info).drawimage = drawimage
    (*info).xscale = ptr_new(xscale)
    (*info).yscale = ptr_new(yscale)
    return
  endif

  if (*(*info).aux->getrev()) and revdim ne 0 then $
      drawimage=reverse(drawimage,revdim)
  xr=[xscale[(*info).xrmin],xscale[(*info).xrmax]]
  yr=[yscale[(*info).yrmin],yscale[(*info).yrmax]]

  ;flip the image when flip button is checked
  if (*info).flip then begin
                                ; check that z is the y-axis, otherwise do not flip the image
     if strupcase(strmid(yax,0,1)) eq 'Z' then begin
        yr = [yscale[(*info).yrmax],yscale[(*info).yrmin]]
     endif else begin
        if strupcase(strmid(xax,0,1)) eq 'Z' then begin
           xr = [xscale[(*info).xrmax],xscale[(*info).xrmin]]
        endif else begin
           message, 'warning: option only works when y-axis or x-axis is z',/cont
        endelse
     endelse
  endif

  if (*info).iroption then ir=[min(drawimage),max(drawimage)] else begin
    imn=(*info).intmin
    imx=(*info).intmax
    ir=[imn+(imx-imn)/255.*(*info).irmin, $
        imn+(imx-imn)/255.*(*info).irmax]
  endelse

 varname=*(*info).data->getvarname()
 if (*info).absoption then varname='Abs('+varname+')'
 if (*info).lgoption then varname='Log!d10!n('+varname+')'
 if (*info).expoption then varname='10^!d10!n('+varname+')'
 if (*(*info).fc).swt then varname='Field & '+ varname
  (*info).title=varname+' at pos '+ $
             zax+'='+string(zscale((*info).rpos),format='(1x,f7.3)')
 if strupcase(!d.name) eq 'PS' then bgblack = 0 else bgblack = 1

 if (*info).ircoord then ir=(*info).ircoordrange
 if (*info).ircoord and (*info).lgoption then ir=alog10(ir)
 if (*info).ircoord and (*info).expoption then ir=10^(ir)

 if (*info).nosqoption eq 0 then nosq=0

 if ((strupcase(strmid(xax,0,1)) ne 'T') and (strupcase(strmid(yax,0,1)) ne 'T')) then begin
    nosq=0
    isotropic=1
 endif else begin
    nosq=1
    isotropic=0
 endelse

 if (*info).nosqoption eq 1 then nosq=1

 if strupcase(!d.name) ne 'PS' then begin
    br_plot_image,drawimage, xscale,yscale,max=ir[1],min=ir[0],nosq=nosq,xr=xr,yr=yr, $
                xtitle = (*info).xtitle, ytitle = (*info).ytitle, isotropic=isotropic,fixnz=(*info).fixnz, $
                title=(*info).title,bgblack=bgblack,top=(*info).topcolor,bottom=(*info).bottomcolor
 endif  else begin
    br_plot_image,drawimage, xscale,yscale,max=ir[1],min=ir[0],nosq=nosq, $
                xtitle = (*info).xtitle, ytitle = (*info).ytitle,isotropic=isotropic, $
                xr=xr,yr=yr,bgblack=bgblack,top=(*info).topcolor,bottom=(*info).bottomcolor
 endelse

   if (*(*info).fc).swt eq 1 or (*(*info).fc).swt eq 2 then begin  ;; mag field lines
     if nosq eq 0 then begin ; determine cases for a specific axis: x,y,z,or t
        if (*(*info).fc).swt eq 2 then begin
            r1 = (*(*info).fc).data
            ;rviejo=r1
           if not (((size(r1))[0] eq 3 or (size(r1))[0] eq 2) and ((size(r1))[2] eq 3 or (size(r1))[0] eq 2)) then begin
              message,'warning: the dimensions and number of axes of size(inputfield) must be 2 or 3'
           endif
        endif else begin
           xmhd_calcfield,info,strupcase(strmid(xax,0,1)),strupcase(strmid(yax,0,1)),strupcase(strmid(zax,0,1))
           r1 = (*(*info).fc).data
        endelse
        nl = n_elements(r1(0,0,*))
        dim = n_elements(r1(0,*,0))
        nseeds = n_elements(r1(*,0,0))/2
        case dim of
           3:begin
              for i=0,nl-1 do begin
                 if strupcase(strmid(xax,0,1)) eq 'X' and  strupcase(strmid(yax,0,1)) eq 'Y' then begin
                    xnlmin=min([min(where(r1(*,0,i) gt min(xr) and r1(*,0,i) lt max(xr))),$
                                max(where(r1(*,0,i) gt min(xr) and r1(*,0,i) lt max(xr)))])
                    xnlmax=max([min(where(r1(*,0,i) gt min(xr) and r1(*,0,i) lt max(xr))),$
                                max(where(r1(*,0,i) gt min(xr) and r1(*,0,i) lt max(xr)))])
                    if xnlmin ge 0 and xnlmax ge 0 then oplot,r1(xnlmin:xnlmax,0,i),r1(xnlmin:xnlmax,1,i), $
                                                              color=(*(*info).fc).ct
                 endif

                 if strupcase(strmid(xax,0,1)) eq 'X' and  strupcase(strmid(yax,0,1)) eq 'Z' then begin
                    xnlmin=min([min(where(r1(*,0,i) gt min(xr) and r1(*,0,i) lt max(xr))),$
                                max(where(r1(*,0,i) gt min(xr) and r1(*,0,i) lt max(xr)))])
                    xnlmax=max([min(where(r1(*,0,i) gt min(xr) and r1(*,0,i) lt max(xr))),$
                                max(where(r1(*,0,i) gt min(xr) and r1(*,0,i) lt max(xr)))])
                    if xnlmin ge 0 and xnlmax ge 0 then oplot,r1(xnlmin:xnlmax,0,i),r1(xnlmin:xnlmax,2,i), $
                                                              color=(*(*info).fc).ct
                 endif
                 if strupcase(strmid(xax,0,1)) eq 'Y' and  strupcase(strmid(yax,0,1)) eq 'Z' then begin
                    xnlmin=min([min(where(r1(*,1,i) gt min(xr) and r1(*,1,i) lt max(xr))),$
                                max(where(r1(*,1,i) gt min(xr) and r1(*,1,i) lt max(xr)))])
                    xnlmax=max([min(where(r1(*,1,i) gt min(xr) and r1(*,1,i) lt max(xr))),$
                                max(where(r1(*,1,i) gt min(xr) and r1(*,1,i) lt max(xr)))])
                    if xnlmin ge 0 and xnlmax ge 0 then oplot,r1(xnlmin:xnlmax,1,i),r1(xnlmin:xnlmax,2,i), $
                                                              color=(*(*info).fc).ct
                 endif
              endfor
           end
           2: begin
              for i=0,nl-1 do begin
                 xnlmin=min([min(where(r1(*,0,i) gt min(xr) and r1(*,0,i) lt max(xr))),$
                             max(where(r1(*,0,i) gt min(xr) and r1(*,0,i) lt max(xr)))])
                 xnlmax=max([min(where(r1(*,0,i) gt min(xr) and r1(*,0,i) lt max(xr))),$
                             max(where(r1(*,0,i) gt min(xr) and r1(*,0,i) lt max(xr)))])
                 if xnlmin ge 0 and xnlmax ge 0 then oplot,r1(xnlmin:xnlmax,0,i),r1(xnlmin:xnlmax,1,i), $
                                                              color=(*(*info).fc).ct
              endfor
           end
        endcase
        if (*(*info).fc).ldirect eq 1 then begin
           length = 0.1
           magmax = 1.
           dim = n_elements((*(*info).fc).seedl(*,0))
           nl = n_elements((*(*info).fc).linedir(0,*))
           case dim of
              3:begin
                 for i=0,nl-1 do begin
                    if strupcase(strmid(xax,0,1)) eq 'X' and  strupcase(strmid(yax,0,1)) eq 'Y' then begin
                       nux=(*(*info).fc).linedir(0,i)
                       nuz=(*(*info).fc).linedir(1,i)
                       mag=length*sqrt(nux^2 + nuz^2)/magmax
                       signz=nuz/(abs(nuz))
                       signx=nux/(abs(nux))
                       angle = atan(abs(nuz/nux))
                       if (signz eq  1) and (signx eq -1 ) then angle = 180*!dtor-angle
                       if (signz eq -1) and (signx eq -1 ) then angle = angle+180*!dtor
                       if (signz eq -1) and (signx eq  1 ) then angle = 360*!dtor-angle
                       if (signz eq  1) and (signx eq  1 ) then angle = angle
                       x1=mag*cos(angle)
                       z1=mag*sin(angle)
                       arrow,(*(*info).fc).seedl(0,i),(*(*info).fc).seedl(1,i),(*(*info).fc).seedl(0,i)+x1,(*(*info).fc).seedl(1,i)+z1, $
                             /data, thick=1, hsize=8, /solid, color=(*(*info).fc).ct
                    endif

                    if strupcase(strmid(xax,0,1)) eq 'X' and  strupcase(strmid(yax,0,1)) eq 'Z' then begin
                       nux=(*(*info).fc).linedir(0,i)
                       nuz=(*(*info).fc).linedir(2,i)
                       mag=length*sqrt(nux^2 + nuz^2)/magmax
                       signz=nuz/(abs(nuz))
                       signx=nux/(abs(nux))
                       angle = atan(abs(nuz/nux))
                       if (signz eq  1) and (signx eq -1 ) then angle = 180*!dtor-angle
                       if (signz eq -1) and (signx eq -1 ) then angle = angle+180*!dtor
                       if (signz eq -1) and (signx eq  1 ) then angle = 360*!dtor-angle
                       if (signz eq  1) and (signx eq  1 ) then angle = angle
                       x1=mag*cos(angle)
                       z1=mag*sin(angle)
                       arrow,(*(*info).fc).seedl(0,i),(*(*info).fc).seedl(2,i)+yscale[0],$
                             (*(*info).fc).seedl(0,i)+x1,(*(*info).fc).seedl(2,i)+yscale[0]+z1, $
                             /data, thick=1, hsize=8, /solid, color=(*(*info).fc).ct
                    endif
                    if strupcase(strmid(xax,0,1)) eq 'Y' and  strupcase(strmid(yax,0,1)) eq 'Z' then begin
                       nux=(*(*info).fc).linedir(1,i)
                       nuz=(*(*info).fc).linedir(2,i)
                       mag=length*sqrt(nux^2 + nuz^2)/magmax
                       signz=nuz/(abs(nuz))
                       signx=nux/(abs(nux))
                       angle = atan(abs(nuz/nux))
                       if (signz eq  1) and (signx eq -1 ) then angle = 180*!dtor-angle
                       if (signz eq -1) and (signx eq -1 ) then angle = angle+180*!dtor
                       if (signz eq -1) and (signx eq  1 ) then angle = 360*!dtor-angle
                       if (signz eq  1) and (signx eq  1 ) then angle = angle
                       x1=mag*cos(angle)
                       z1=mag*sin(angle)
                       arrow,(*(*info).fc).seedl(1,i),(*(*info).fc).seedl(2,i)+yscale[0],$
                             (*(*info).fc).seedl(1,i)+x1,(*(*info).fc).seedl(2,i)+yscale[0]+z1, $
                             /data, thick=1, hsize=8, /solid, color=(*(*info).fc).ct
                    endif
                 endfor
              end
              2: begin
                 for i=0,nl-1 do begin
                    if strupcase(strmid(xax,0,1)) eq 'X' and  strupcase(strmid(yax,0,1)) eq 'Y' then begin
                       nux=(*(*info).fc).linedir(0,i)
                       nuz=(*(*info).fc).linedir(1,i)
                       mag=length*sqrt(nux^2 + nuz^2)/magmax
                       signz=nuz/(abs(nuz))
                       signx=nux/(abs(nux))
                       angle = atan(abs(nuz/nux))
                       if (signz eq  1) and (signx eq -1 ) then angle = 180*!dtor-angle
                       if (signz eq -1) and (signx eq -1 ) then angle = angle+180*!dtor
                       if (signz eq -1) and (signx eq  1 ) then angle = 360*!dtor-angle
                       if (signz eq  1) and (signx eq  1 ) then angle = angle
                       x1=mag*cos(angle)
                       z1=mag*sin(angle)
                       arrow,(*(*info).fc).seedl(0,i),(*(*info).fc).seedl(1,i),(*(*info).fc).seedl(0,i)+x1,(*(*info).fc).seedl(1,i)+z1, $
                             /data, thick=1, hsize=8, /solid, color=(*(*info).fc).ct
                    endif

                    if strupcase(strmid(xax,0,1)) eq 'X' and  strupcase(strmid(yax,0,1)) eq 'Z' then begin
                       nux=(*(*info).fc).linedir(0,i)
                       nuz=(*(*info).fc).linedir(2,i)
                       mag=length*sqrt(nux^2 + nuz^2)/magmax
                       signz=nuz/(abs(nuz))
                       signx=nux/(abs(nux))
                       angle = atan(abs(nuz/nux))
                       if (signz eq  1) and (signx eq -1 ) then angle = 180*!dtor-angle
                       if (signz eq -1) and (signx eq -1 ) then angle = angle+180*!dtor
                       if (signz eq -1) and (signx eq  1 ) then angle = 360*!dtor-angle
                       if (signz eq  1) and (signx eq  1 ) then angle = angle
                       x1=mag*cos(angle)
                       z1=mag*sin(angle)
                       arrow,(*(*info).fc).seedl(0,i),(*(*info).fc).seedl(1,i)+yscale[0], $
                             (*(*info).fc).seedl(0,i)+x1,(*(*info).fc).seedl(1,i)+yscale[0]+z1, $
                             /data, thick=1, hsize=8, /solid, color=(*(*info).fc).ct
                    endif
                    if strupcase(strmid(xax,0,1)) eq 'Y' and  strupcase(strmid(yax,0,1)) eq 'Z' then begin
                       nux=(*(*info).fc).linedir(1,i)
                       nuz=(*(*info).fc).linedir(2,i)
                       mag=length*sqrt(nux^2 + nuz^2)/magmax
                       signz=nuz/(abs(nuz))
                       signx=nux/(abs(nux))
                       angle = atan(abs(nuz/nux))
                       if (signz eq  1) and (signx eq -1 ) then angle = 180*!dtor-angle
                       if (signz eq -1) and (signx eq -1 ) then angle = angle+180*!dtor
                       if (signz eq -1) and (signx eq  1 ) then angle = 360*!dtor-angle
                       if (signz eq  1) and (signx eq  1 ) then angle = angle
                       x1=mag*cos(angle)
                       z1=mag*sin(angle)
                       arrow,(*(*info).fc).seedl(0,i),(*(*info).fc).seedl(1,i)+yscale[0], $
                             (*(*info).fc).seedl(0,i)+x1,(*(*info).fc).seedl(1,i)+yscale[0]+z1, $
                             /data, thick=1, hsize=8, /solid, color=(*(*info).fc).ct
                    endif
                 endfor
              end
           endcase
        endif
     endif else begin
        message, 'Warning magnetic field can not be drawn when one of the plot axes is time',/cont
     endelse
  endif


  ; draw beta contours:
  if (*(*info).bt).swt eq 1 then begin
;     if ((strupcase(strmid(xax,0,1)) ne 'T') and (strupcase(strmid(yax,0,1)) ne 'T')) then begin ; if the x and y axes are not time, then begin
;        if n_elements(reform(*(*info).data->getvar((*info).line))) ne
;        n_elements((*(*info).bt).data) then begin
     xmhd_calcbeta,info         ; calculate plasma beta from xmhd_calcbeta
;        endif
     beta = reform((*(*info).bt).data)
     dim = (size(beta))[0]
     nodraw=0
     x = *(*info).data->getx()
     y = *(*info).data->gety()
     z = *(*info).data->getz()
     time = *(*info).data->gett()
     itsnap = (*(*info).data->getisnaps())
     mini=min((*(*info).bt).ir)
     maxi=max((*(*info).bt).ir)
     nlevels=findgen((*(*info).bt).nlevels)*(maxi-mini)/(*(*info).bt).nlevels+mini
                                ; for y-slice

     case strupcase(strmid(xax,0,1)) of
        'X': begin
           xr=x(min((*(*info).bt).xr):max((*(*info).bt).xr))
           if strupcase(strmid(yax,0,1)) eq 'Y' then begin
              iz = (*(*info).data)->getislice()
              yr=y(min((*(*info).bt).yr):max((*(*info).bt).yr))
           endif else begin
                                ; for y-slice
              if strupcase(strmid(yax,0,1)) eq 'Z' then begin
                 yr=z(min((*(*info).bt).yr):max((*(*info).bt).yr))
                 if strupcase(strmid(zax,0,1)) eq 'Y' then iy = (*(*info).data)->getislice() else $
                    iz = (*(*info).data)->getislice()
              endif else begin
                                ; when y axis is 'T'
                 iy = (*(*info).data)->getislice()
                 yr=time(min((*(*info).bt).yr):max((*(*info).bt).yr))
              endelse
           endelse
        end
        'Y': begin
           xr=y(min((*(*info).bt).xr):max((*(*info).bt).xr))
                                ; for x-slice
           if strupcase(strmid(yax,0,1)) eq 'Z' then begin
              ix = (*(*info).data)->getislice()
              yr=z(min((*(*info).bt).yr):max((*(*info).bt).yr))
           endif else begin
              if strupcase(strmid(zax,0,1)) eq 'Z' then iy = (*(*info).data)->getislice() else $
                 ix=(*(*info).data)->getislice()
              yr=time(min((*(*info).bt).yr):max((*(*info).bt).yr))
           endelse
        end
        'Z': begin
           ix= (*(*info).data)->getislice()
           xr=z(min((*(*info).bt).xr):max((*(*info).bt).xr))
           yr=time(min((*(*info).bt).yr):max((*(*info).bt).yr))
        end
     endcase

     if dim eq 3 then begin
        case (*info).mode of
           0: begin
              imageb=beta(min((*(*info).bt).xr):max((*(*info).bt).xr), $
                          min((*(*info).bt).yr):max((*(*info).bt).yr),(*info).rpos)
           end
           1: begin
              imageb=beta((*info).rpos,min((*(*info).bt).xr):max((*(*info).bt).xr), $
                          min((*(*info).bt).yr):max((*(*info).bt).yr))
           end
           2: begin
              imageb=beta(min((*(*info).bt).xr):max((*(*info).bt).xr),(*info).rpos, $
                          min((*(*info).bt).yr):max((*(*info).bt).yr))
              end
        endcase

     endif else begin
        if dim eq 2 then begin
           imageb=beta(min((*(*info).bt).xr):max((*(*info).bt).xr), $
                       min((*(*info).bt).yr):max((*(*info).bt).yr))
        endif else begin
           message, 'The variables must be 2 or 3d',/cont
           nodraw=1
        endelse
     endelse
     if nodraw eq 0 then begin
        contour,reform(imageb),xr,yr,/overplot,levels=(*(*info).bt).thickv,$
                c_labels=(*(*info).bt).label,thick=2,c_colors=(*(*info).bt).ct
        if n_elements(nlevels) eq 1 then begin
           contour,reform(imageb),xr,yr,/overplot,levels=nlevels, $
                   c_labels=(*(*info).bt).label,thick=1,c_colors=indgen((*(*info).bt).nlevels)*0+(*(*info).bt).ct
        endif else begin
           if nlevels[0] le nlevels[1] then contour,reform(imageb),xr,yr,/overplot,levels=nlevels, $
                   c_labels=(*(*info).bt).label,thick=1,c_colors=indgen((*(*info).bt).nlevels)*0+(*(*info).bt).ct
        endelse
     endif
  endif

   ;arrow
   uv=*(*info).uv
   if (uv.swt eq 1) and (strupcase(strmid(xax,0,1)) ne 'T') and  (strupcase(strmid(yax,0,1)) ne 'T') then begin
      x = *(*info).data->getx()
      y = *(*info).data->gety()
      z = *(*info).data->getz()
      xmax=max(x) & xmin=min(x)
      zmax=max(z) & zmin=min(z)
      seeds=uv.nseeds
                                ; for y-slice
      if strupcase(strmid(xax,0,1)) eq 'X' and  strupcase(strmid(yax,0,1)) eq 'Y' then begin
         xmin=x(uv.xr[0]) & xmax=x(uv.xr[1])
         zmin=y(uv.yr[0]) & zmax=y(uv.yr[1])
         nelx=n_elements(x) & nelz=n_elements(y)
      endif else begin
                                ; for y-slice
         if strupcase(strmid(xax,0,1)) eq 'X' and  strupcase(strmid(yax,0,1)) eq 'Z' then begin
            xmin=x(uv.xr[0]) & xmax=x(uv.xr[1])
            zmin=z(uv.yr[0]) & zmax=z(uv.yr[1])
            nelx=n_elements(x) & nelz=n_elements(z)
         endif else begin
                                ; for x-slice
            if strupcase(strmid(xax,0,1)) eq 'Y' and  strupcase(strmid(yax,0,1)) eq 'Z' then begin
               xmin=y(uv.xr[0]) & xmax=y(uv.xr[1])
               zmin=z(uv.yr[0]) & zmax=z(uv.yr[1])
               nelx=n_elements(y) & nelz=n_elements(z)
            endif else begin
               message, 'Warning vectors can not be drawn when one of the plot axes is time',/cont
               uv.swt = 0
            endelse
         endelse
      endelse

      if  uv.swt ne 0 then begin
         factx=nelx/(xmax-xmin)
         factz=nelz/(zmax-zmin)
         if uv.method eq 1 then begin
            xx=(xmax-xmin)*findgen(seeds)/(seeds-1)+xmin
            zz=(zmax-zmin)*findgen(seeds)/(seeds-1)+zmin
         endif

         if uv.method eq 0 then begin
            xx=(xmax-xmin)*randomu(seed,seeds*seeds)+xmin
            zz=(zmax-zmin)*randomu(seed,seeds*seeds)+zmin
         endif

         length=uv.length/10
         magmax=uv.maxsq
         dim=size(uv.datax)
         if dim[0] eq 3 then begin
            case (*info).mode of
               0: begin
                  ux=reform(uv.datax(*,*,(*info).rpos))
                  uy=reform(uv.datay(*,*,(*info).rpos))
               end
               1: begin
                  ux=reform(uv.datax((*info).rpos,*,*))
                  uy=reform(uv.datay((*info).rpos,*,*))
               end
               2: begin
                  ux=reform(uv.datax(*,(*info).rpos,*))
                  uy=reform(uv.datay(*,(*info).rpos,*))
               end
            endcase
         endif else begin
            ux=uv.datax
            uy=uv.datay
         endelse

         if uv.method eq 1 then begin
            for j=0, seeds-1 do begin
               for i=0,seeds-1 do begin
                  posx=(xx(i)-xmin)*factx
                  posz=(zz(j)-zmin)*factz
                  nux=bilinear(ux,posx,posz)*10
                  nuz=bilinear(uy,posx,posz)*10
                  mag=length*sqrt(nux^2 + nuz^2)/magmax
                  signz=nuz/(abs(nuz))
                  signx=nux/(abs(nux))
                  angle = atan(abs(nuz/nux))
                  if (signz eq  1) and (signx eq -1 ) then angle = 180*!dtor-angle
                  if (signz eq -1) and (signx eq -1 ) then angle = angle+180*!dtor
                  if (signz eq -1) and (signx eq  1 ) then angle = 360*!dtor-angle
                  if (signz eq  1) and (signx eq  1 ) then angle = angle
                  x1=mag*cos(angle)
                  z1=mag*sin(angle)
                  arrow,xx(i),zz(j),xx(i)+x1,zz(j)+z1, $
                        /data, thick=uv.thick, hsize=uv.head, /solid, color=uv.ct
               endfor
            endfor
         endif

         if uv.method eq 0 then begin
            for i=0, seeds*seeds-1 do begin
               posx=(xx(i)-xmin)*factx
               posz=(zz(i)-zmin)*factz
               nux=bilinear(ux,posx,posz)*10
               nuz=bilinear(uy,posx,posz)*10
               mag=length*sqrt(nux^2 + nuz^2)/magmax
               signz=nuz/(abs(nuz))
               signx=nux/(abs(nux))
               angle = atan(abs(nuz/nux))
               if (signz eq  1) and (signx eq -1 ) then angle = 180*!dtor-angle
               if (signz eq -1) and (signx eq -1 ) then angle = angle+180*!dtor
               if (signz eq -1) and (signx eq  1 ) then angle = 360*!dtor-angle
               if (signz eq  1) and (signx eq  1 ) then angle = angle
               x1=mag*cos(angle)
               z1=mag*sin(angle)
               arrow,xx(i),zz(i),xx(i)+x1,zz(i)+z1, $
                     /data, thick=uv.thick, hsize=uv.head, /solid, color=uv.ct
            endfor
         endif
      endif
   endif

  (*info).drawimage = ptr_new(fltarr(sz[1], sz[2]))
  *(*info).drawimage = drawimage
  (*info).xscale = ptr_new(xscale)
  (*info).yscale = ptr_new(yscale)

; create colorbar:
 if strupcase(!d.name) ne 'PS' and strupcase(!d.name) ne 'Z' then begin
    widget_control, (*info).colorbarid, get_value = wid
    ;print,'xmhd_draw colorbar'
    wset, wid
  ; first erase previous colorbar
    bar = intarr((*info).cb_xsz, (*info).d_ysz)*0
    tv, bar
    plot,[0,1],[0,1],background=!p.background, color=!p.background
  ; find max and min values of drawimage to produce color bar y-scale
    ymin = ir[0] ; min(drawimage)
    ymax = ir[1] ; xsmax(drawimage)
    if ymax-ymin eq 0.0 then ymax=ymin+max([1.e-4*ymin,1.e-4])
    format='(e11.3)'
    colorbar, position = [0.73, 0.15, 0.95, 0.9], range = [ymin, ymax],charsize=!p.charsize,background=!p.background, color=!p.color,$
            /vertical , format=format,bottom=(*info).bottomcolor, ncolors=(*info).topcolor-(*info).bottomcolor;, back=!d.n_colors-1
    pseudoevent={widget_button,id:(*info).action, $
                 top:event.top, handler:0l, select:1}
    widget_control,event.top,set_uvalue=info

    xmhd_prdraw,pseudoevent
    xmhd_pcdraw,pseudoevent
 endif else begin
    if strupcase(!d.name) eq 'PS' then begin
       ymin = ir[0]             ; min(drawimage)
       ymax = ir[1]             ; xsmax(drawimage)
       format='(e11.1)'
       colorbar, position = [0.12, 0.93, 0.92, 0.95], range = [ymin, ymax], $
                 format=format,title=(*info).title,bottom=(*info).bottomcolor, ncolors=(*info).topcolor-(*info).bottomcolor ;, back=!d.n_colors-1
    endif else begin
       img=tvrd()
       if (*info).rpos eq 0 then begin
          szim=size(img)
          (*info).anim = ptr_new(bytarr(szim(1),szim(2),n_elements(zscale)))
       endif
       device,decomposed=0
       (*(*info).anim)(*,*,(*info).rpos)=img
    endelse
 endelse

end

; ++++++++++++++++++++++++++1d plots below the main 2d plot++++++++++++++++++++++
; draw left plot
pro xmhd_pcdraw, event

  widget_control, event.top, get_uvalue=info
  widget_control, (*info).pcid, get_value = wid
  varname=*(*info).data->getvarname()
  if (*info).absoption then varname='Abs('+varname+')'
  if (*info).lgoption then varname='Log!d10!n('+varname+')'
  if (*info).expoption then varname='10^!d10!n('+varname+')'
  wset,wid
  iy=(*info).pciy
  case (*info).mode of
     0: yax=*(*info).aux->getytitle()
     1: yax=*(*info).aux->getxrastertitle()
     2: yax=*(*info).aux->getxrastertitle()
  endcase
  if (strupcase(strmid(yax,0,1)) eq 'X') then yy=*(*info).data->getx()
  if (strupcase(strmid(yax,0,1)) eq 'Y') then yy=*(*info).data->gety()
  if (strupcase(strmid(yax,0,1)) eq 'Z') then yy=*(*info).data->getz()
  if (strupcase(strmid(yax,0,1)) eq 'T') then yy=*(*info).data->gett()
  varname=varname+ ' at pos ' +yax+ ' = ' +strtrim(string(yy(iy),format='(f8.3)'),2) 
  plot,*(*info).xscale,(reform(*(*info).drawimage))[*,iy], /yst,$
       xtitle=(*info).xtitle,ytitle=varname,xst=1, position=[.3,.15,0.95,.95]
end

;--------- slider for left 1d plot in main window----------
;selectes between coord and index tab
pro xmhd_pctab, event

  widget_control, event.top,get_uvalue=info
end

;selects the possition of the 1d plot in index
pro xmhd_fclider, event

  widget_control, event.top,get_uvalue=info
  widget_control,event.id,get_uvalue=uvalue,get_value=value
  widget_control, (*info).pcid, get_value = wid


  (*info).pciy=floor(value)
  y=*(*info).yscale
  widget_control, (*info).ffclider,set_value=y((*info).pciy)
  xmhd_pcdraw,event
end

;selects the possition of the 1d plot in coord
pro xmhd_ffclider, event

    widget_control, event.top,get_uvalue=info
    widget_control,event.id,get_uvalue=uvalue,get_value=value
    widget_control, (*info).pcid, get_value = wid

    y=*(*info).yscale
    miny=min(abs(y-value),min_subscript)
    (*info).pciy=min_subscript
    widget_control, (*info).fclider,set_value=min_subscript
    xmhd_pcdraw,event
end

; draw right plot
pro xmhd_prdraw, event

  widget_control, event.top, get_uvalue=info
  widget_control, (*info).prid, get_value = wid
  varname=*(*info).data->getvarname()
  if (*info).absoption then varname='Abs('+varname+')'
  if (*info).lgoption then varname='Log!d10!n('+varname+')'
  if (*info).lgoption then varname='10^!d10!n('+varname+')'
  wset,wid
  ix=(*info).prix
  case (*info).mode of
     0: xax=*(*info).aux->getxtitle()
     1: xax=*(*info).aux->getytitle()
     2: xax=*(*info).aux->getxtitle()
  endcase
  if (strupcase(strmid(xax,0,1)) eq 'X') then yy=*(*info).data->getx()
  if (strupcase(strmid(xax,0,1)) eq 'Y') then yy=*(*info).data->gety()
  if (strupcase(strmid(xax,0,1)) eq 'Z') then yy=*(*info).data->getz()
  if (strupcase(strmid(xax,0,1)) eq 'T') then yy=*(*info).data->gett()
  varname=varname+ ' at pos ' +xax+ ' = ' +strtrim(string(yy(ix),format='(f8.3)'),2) 
  plot,*(*info).yscale,(reform(*(*info).drawimage))[ix,*],/yst, $
       xtitle=(*info).ytitle,ytitle=varname,xst=1, position=[.3,.15,.95,.95]
end

;--------- slider for right 1d plot in main window----------
;selectes between coord and index tab
pro xmhd_prtab, event
  widget_control, event.top,get_uvalue=info
end

;selects the possition of the 1d plot in index
pro xmhd_frlider, event

  widget_control, event.top,get_uvalue=info
  widget_control,event.id,get_uvalue=uvalue,get_value=value
  widget_control, (*info).prid, get_value = wid

  (*info).prix=floor(value)
  x=*(*info).xscale
  widget_control, (*info).ffrlider,set_value=x((*info).prix)
  xmhd_prdraw,event
end

;selects the possition of the 1d plot in coord
pro xmhd_ffrlider, event

    widget_control, event.top,get_uvalue=info
    widget_control,event.id,get_uvalue=uvalue,get_value=value
    widget_control, (*info).prid, get_value = wid
    x=*(*info).xscale
    minx=min(abs(x-value),min_subscript)
    (*info).prix=min_subscript
    widget_control, (*info).frlider,set_value=min_subscript
    xmhd_prdraw,event
end

; ++++++++++++++++++++++++++window action++++++++++++++++++++++++++
; get the value of the draw window option menu (between zoom, aver x
; or y and value):
function xmhd_dwoption, event

  widget_control, event.top, get_uvalue = info
  (*info).dwoption = event.value
  return, 0
end

; plays the new window for zoom, aver or writes down in cursor info in draw window:
pro xmhd_zoom, event


  widget_control,event.top,get_uvalue=info
;  if (*info).mode eq 1 then goto, noaction
  if event.type gt 2 then return
  events=['Down','Up','Motion']
  thisevent=events[event.type]
  window, /pixmap, /free, xsize = (*info).d_xsz, ysize = (*info).d_ysz
  xscale=*(*info).xscale
  yscale=*(*info).yscale
  xr=[xscale[(*info).xrmin],xscale[(*info).xrmax]]
  yr=[yscale[(*info).yrmin],yscale[(*info).yrmax]]

  image=reform(*(*info).drawimage) ;; this is done because br_plot_image returns nx,1,nz!
  sz = size(image)

  revdim = 0
  case (*info).mode of
    0: begin
         xax=*(*info).aux->getxtitle()
         yax=*(*info).aux->getytitle()
       end
    1: begin
         xax=*(*info).aux->getytitle()
         yax=*(*info).aux->getxrastertitle()
       end
    2: begin
         xax=*(*info).aux->getxtitle()
         yax=*(*info).aux->getxrastertitle()
       end
  endcase
  xax =  strupcase(strmid(xax,0,1))
  yax =  strupcase(strmid(yax,0,1))

  if (xax eq 'Z') then revdim=1
  if (yax eq 'Z') then revdim=2


  if (*info).flip then begin
                                ; check that z is the y-axis, otherwise do not flip the image
     if yax eq 'Z' then begin
        yr = [yscale[(*info).yrmax],yscale[(*info).yrmin]]
     endif else begin
        if xax eq 'Z' then begin
           xr = [xscale[(*info).xrmax],xscale[(*info).xrmin]]
        endif else begin
           message, 'warning: option only works when y-axis or x-axis is z',/cont
        endelse
     endelse
  endif

  revdiv=(*info).flip

  if (*info).iroption then ir=[min(image),max(image)] else begin
    imn=(*info).intmin
    imx=(*info).intmax
    ir=[imn+(imx-imn)/255.*(*info).irmin, $
        imn+(imx-imn)/255.*(*info).irmax]
 endelse

  br_plot_image,image,xscale,yscale,max=ir[1],min=ir[0], $
     xtitle = (*info).xtitle, ytitle = (*info).ytitle, $
     title=(*info).title,xr=xr,yr=yr,/bgblack,top=(*info).topcolor,bottom=(*info).bottomcolor

  px = (!x.window * !d.x_vsize)
  py = (!y.window * !d.y_vsize)
  swx = px[1]-px[0]
  swy = py[1]-py[0]
  (*info).pixid = !d.window
  imagex = [px,px+swx]
  imagey = [py,py+swy]
  imagepos = [imagex[0],imagey[0],imagex[1],imagey[1]]


  z=(*(*info).data)->getz()
  nelz=n_elements(z)
  maxz=max(z) & minz=min(z)
  newz=minz+(maxz-minz)/(nelz-1)*findgen(nelz)

  case revdim of
     1: begin
        if (*info).uneven then begin
           if revdiv eq 0 then begin
              for iy=0,sz[2]-1 do begin
                 image[*,iy] = interpol( image[*,iy], z, newz  )
              endfor
           endif else begin
              for iy=0,sz[2]-1 do begin
                    image[*,iy] = reverse(interpol( image[*,iy], z, newz  ),1)
              endfor
           endelse
        endif
     end
     2: begin
        if (*info).uneven then begin
           if revdiv eq 0 then begin
              for ix=0,sz[1]-1 do begin
                 image[ix,*] = interpol( image[ix,*], z, newz  )
              endfor
           endif else begin
              for ix=0,sz[1]-1 do begin
                 image[ix,*] = reverse(interpol( image[ix,*], z, newz  ),revdiv)
              endfor
           endelse
        endif
     end
     else : image =image
  endcase


  case thisevent of
     'Down': begin
                                ;  turn motion events on
                                ;  set static corner
        widget_control,(*info).drawid,draw_motion_events=1
        (*info).sx=event.x
        (*info).sy=event.y
     end  ;;
     'Up': begin
                                ;  erase last box
                                ;  turn motion events off
        x = *(*info).xscale
        y = *(*info).yscale

        if (*info).flip ne 0 then begin
                                ; check that z is the y-axis, otherwise do not flip the image
           if yax eq 'Z' then begin
           yscale=-reverse(yscale)
           yr=-yr
           endif else begin
              if xax eq 'Z' then begin
                 xscale=-reverse(xscale)
                 xr=-xr
              endif
           endelse
        endif

        device,copy=[0,0,(*info).d_xsz,(*info).d_ysz,0,0,(*info).pixid]
        widget_control,(*info).drawid,draw_motion_events=0
        dxfac = 1./(imagepos[2]-imagepos[0])
        dyfac = 1./(imagepos[3]-imagepos[1])
        sx=fix(((*info).sx-imagepos[0])*sz[1]*dxfac)<sz[1]-1>0
        yscalen=interpol(yscale,yscale,findgen(n_elements(yscale))*(max(yscale)-min(yscale))/n_elements(yscale)+min(yscale))
        xscalen=interpol(xscale,xscale,findgen(n_elements(xscale))*(max(xscale)-min(xscale))/n_elements(xscale)+min(xscale))
        sx=fix(interpol(indgen(n_elements(xscalen)),sz[1]*(xscalen-xr[0])/(xr[1]-xr[0]),sx))
        dx=fix((event.x-imagepos[0])*sz[1]*dxfac)<sz[1]-1>0
        dx=fix(interpol(indgen(n_elements(xscalen)),sz[1]*(xscalen-xr[0])/(xr[1]-xr[0]),dx))

        sy=(((*info).sy-imagepos[1])*sz[2]*dyfac)<sz[2]-1>0
        dy=((event.y-imagepos[1])*sz[2]*dyfac)<sz[2]-1>0
        sy=fix(interpol(indgen(n_elements(yscalen)),sz[2]*(yscalen-yr[0])/(yr[1]-yr[0]),sy))
        dy=fix(interpol(indgen(n_elements(yscalen)),sz[2]*(yscalen-yr[0])/(yr[1]-yr[0]),dy))
        image=image[sx<dx:sx>dx,sy<dy:sy>dy]

        xscale = xscalen[sx<dx:sx>dx]
        yscale = yscalen[sy<dy:sy>dy]
        sz=size(image)
        case (*info).dwoption of
           0: begin
              if sz[0] ge 2 then begin
                 magf = 2
                 image=congrid(image, sz[1]*magf, sz[2]*magf )
                 xscale = interpol(xscale, sz[1]*magf )
                 yscale = interpol(yscale, sz[2]*magf )
                 xzoom, image, xscale, yscale, xtitle = (*info).xtitle, $
                        ytitle = (*info).ytitle,group_leader=event.top
              endif
           end
           1: begin
                 dmean = total(image, 1)/sz[1]
                 if sz[0] ge 2 then begin
                    case (*info).mode of
                       0: begin
                          xlineplot, dmean, xscale=yscale, title = *(*info).data->getvarname()+$
                                     ': row average', $
                                     xtitle = *(*info).aux->getytitle(), $
                                     ytitle = *(*info).data->getvarname(), groupl=event.top
                       end
                       1: begin
                          xlineplot, dmean, xscale=yscale, title = *(*info).data->getvarname()+$
                                     ': row average', $
                                     xtitle = *(*info).aux->getxrastertitle(), $
                                     ytitle = *(*info).data->getvarname(), groupl=event.top
                       end
                       2: begin
                          xlineplot, dmean, xscale=yscale, title = *(*info).data->getvarname()+$
                                     ': row average', $
                                     xtitle = *(*info).aux->getxrastertitle(), $
                                     ytitle = *(*info).data->getvarname(), groupl=event.top
                       end
                    endcase
                 endif
              end
           2: begin
              dmean = total(image, 2)/sz[2]
              if sz[0] ge 2 then begin
                                ; making cases with mode makes it
                                ; possible to change the names of x-
                                ; and y-axis
                 case (*info).mode of
                    0: begin
                       xlineplot, dmean, xscale=xscale, title = *(*info).data->getvarname()+$
                                  ': column average', $
                                  xtitle = *(*info).aux->getxtitle(), $
                                  ytitle = *(*info).data->getvarname(), groupl = event.top
                                ; note that
                                ; *(*info).aux->getvariablename does
                                ; not work here for title
                    end
                    1: begin
                       xlineplot, dmean, xscale=xscale, title = *(*info).data->getvarname()+$
                                  ': column average', $
                                  xtitle = *(*info).aux->getytitle(), $
                                  ytitle = *(*info).data->getvarname(), groupl = event.top
                    end
                    2: begin
                       xlineplot, dmean, xscale=xscale, title = *(*info).data->getvarname()+$
                                  ': column average', $
                                  xtitle = *(*info).aux->getxtitle(), $
                                  ytitle = *(*info).data->getvarname(), groupl = event.top
                    end
                 endcase
              endif
           end
           3: begin
              case (*info).mode of
                 0: begin
                    xax=*(*info).aux->getxtitle()
                    yax=*(*info).aux->getytitle()
                 end
                 1: begin
                    xax=*(*info).aux->getytitle()
                    yax=*(*info).aux->getxrastertitle()
                 end
                 2: begin
                    xax=*(*info).aux->getxtitle()
                    yax=*(*info).aux->getxrastertitle()
                 end
              endcase
              val=image[0,0]
              xval = xscale[0]
              yval = yscale[0]
              xcur=widget_info(event.top, find_by_uname='xcursor')
              ycur=widget_info(event.top, find_by_uname='ycursor')
              vcur=widget_info(event.top, find_by_uname='valcurs')
              widget_control,xcur,set_value=strmid(xax,0,1)+'['+strtrim(string(min(where(x ge xval))),2)$
                             +'] = '+ strtrim(string(xval,format='(f6.2)'),2)+' Mm'
              widget_control,ycur,set_value=strmid(yax,0,1)+'['+strtrim(string(min(where(y ge yval))),2)$
                             +'] = '+ strtrim(string(yval,format='(f6.2)'),2)+' Mm'
              varname=*(*info).data->getvarname()
              widget_control,vcur,set_value=varname+' = '+strtrim(string(val),2)
           end
        endcase
     end
     'Motion':  begin
       ;  erase previous box
       ;  draw new box
          dx=event.x
          dy=event.y
          sx=(*info).sx
          sy=(*info).sy
          wset,(*info).wid
          device,copy=[0,0,(*info).d_xsz,(*info).d_ysz,0,0,(*info).pixid]
          plots,[sx,sx,dx,dx,sx],[sy,dy,dy,sy,sy],/device,$
                color=(*info).drawcolor
       endcase
     endcase
  wdelete, (*info).pixid
  noaction:
end

;select display mode:
pro xmhd_mode, event

  widget_control, event.top, get_uvalue=info
  thisevent=tag_names(event,/structure_name)
  case thisevent of
    'WIDGET_DROPLIST': begin
      (*info).mode = event.index
      end
      else:
  endcase
  nx=(*info).ccd_xsz
  ny=(*info).ccd_ysz
  nr=(*info).nraster
  case (*info).mode of
    0: begin
         xax=*(*info).aux->getxtitle()
         yax=*(*info).aux->getytitle()
         rax=*(*info).aux->getxrastertitle()
         if (*info).rslider gt 0 then begin
            widget_control, (*info).rslider, set_slider_min=0, $
               set_slider_max=nr-1, set_value=0, sensitive=1
         endif
         if (*info).wdlist gt 0 then $
             widget_control, (*info).wdlist, sensitive=0
         if (*info).dwoption_menu gt 0 then $
            widget_control, (*info).dwoption_menu, sensitive = 1
       end
    1: begin
         xax=*(*info).aux->getytitle()
         yax=*(*info).aux->getxrastertitle()
         rax=*(*info).aux->getxtitle()
         if (*info).rslider gt 0 then begin
            widget_control, (*info).rslider, set_slider_min=0, $
               set_slider_max=nx-1, set_value=0, sensitive=1
         endif
         if (*info).wdlist gt 0 then $
            widget_control, (*info).wdlist, sensitive=0
         if (*info).dwoption_menu gt 0 then $
            widget_control, (*info).dwoption_menu, sensitive = 1
       end
    2: begin
         xax=*(*info).aux->getxtitle()
         yax=*(*info).aux->getxrastertitle()
         rax=*(*info).aux->getytitle()
         if (*info).rslider gt 0 then begin
            widget_control, (*info).rslider, set_slider_min=0, $
               set_slider_max=ny-1, set_value=0, sensitive=1
         endif
         if (*info).wdlist gt 0 then $
            widget_control, (*info).wdlist, sensitive=0
         if (*info).dwoption_menu gt 0 then $
            widget_control, (*info).dwoption_menu, sensitive = 1
       end
   else:
   endcase
   (*info).rpos=0
   (*info).rtitle=rax
   aspect=0
   case strupcase(strmid(xax,0,1)) of
     'X': x = *(*info).hdr->getx()
     'Y': x = *(*info).hdr->gety()
     'Z': x = *(*info).hdr->getz()
     'T': begin
          aspect=1
          x= *(*info).hdr->gett()
          end
    else:
  endcase
  case strupcase(strmid(yax,0,1)) of
    'X': y = *(*info).hdr->getx()
    'Y': y = *(*info).hdr->gety()
    'Z': y = *(*info).hdr->getz()
    'T': begin
         aspect=1
         y= *(*info).hdr->gett()
         end
    else:
  endcase
   case strupcase(strmid(rax,0,1)) of
     'X': r = *(*info).hdr->getx()
     'Y': r = *(*info).hdr->gety()
     'Z': r = *(*info).hdr->getz()
     'T': r = *(*info).hdr->gett()
    else:
  endcase
  (*info).xscale=ptr_new(x)
  (*info).yscale=ptr_new(y)
  (*info).xrmin=0
  (*info).xrmax=n_elements(x)-1
  (*info).yrmin=0
  (*info).yrmax=n_elements(y)-1
  (*(*info).fc).xr=[0,n_elements(x)-1]
  (*(*info).fc).yr=[0,n_elements(y)-1]
  (*(*info).bt).xr=[0,n_elements(x)-1]
  (*(*info).bt).yr=[0,n_elements(y)-1]
  (*(*info).uv).xr=[0,n_elements(x)-1]
  (*(*info).uv).yr=[0,n_elements(y)-1]
  xmhd_vectordraw,info=info,uv=(*(*info).uv)

; reset the values of the x and y sliders
  widget_control, (*info).xminslider, set_value=[(*info).xrmin,(*info).xrmin,(*info).xrmax], sensitive=1
  widget_control, (*info).xmaxslider, set_value=[(*info).xrmax,(*info).xrmin,(*info).xrmax], sensitive=1
  widget_control, (*info).yminslider, set_value=[(*info).yrmin,(*info).yrmin,(*info).yrmax], sensitive=1
  widget_control, (*info).ymaxslider, set_value=[(*info).yrmax,(*info).yrmin,(*info).yrmax], sensitive=1

  widget_control, (*info).xxminslider, set_value=[min(x),min(x),max(x)], sensitive=1
  widget_control, (*info).xxmaxslider, set_value=[max(x),min(x),max(x)], sensitive=1
  widget_control, (*info).yyminslider, set_value=[min(y),min(y),max(y)], sensitive=1
  widget_control, (*info).yymaxslider, set_value=[max(y),min(y),max(y)], sensitive=1

;reset the values of the sliders of the two 1d plots
   widget_control, (*info).ffrlider,get_value=val
   widget_control, (*info).ffrlider,set_value=[max([min([val,max(x)]),min(x)]),min(x),max(x)]
   widget_control, (*info).frlider,get_value=val
   widget_control, (*info).frlider,set_value=[max([min([val,(*info).xrmax]),(*info).xrmin]),(*info).xrmin,(*info).xrmax]
   (*info).prix=max([min([val,(*info).xrmax]),(*info).xrmin])
   widget_control, (*info).ffclider,get_value=val
   widget_control, (*info).ffclider,set_value=[max([min([val,max(y)]),min(y)]),min(y),max(y)]
   widget_control, (*info).fclider,get_value=val
   widget_control, (*info).fclider,set_value=[max([min([val,(*info).yrmax]),(*info).yrmin]),(*info).yrmin,(*info).yrmax]
   (*info).pciy=max([min([val,(*info).yrmax]),(*info).yrmin])

  info_txt=' '+(*info).rtitle+' '+strtrim(string(r[(*info).rpos],format='(f7.3)'),2)

  widget_control,(*info).info_rslider,set_value=info_txt

  if n_elements(x) gt 1 then begin
    if aspect eq 0 then aspect = abs((x[(*info).xrmax]-x[(*info).xrmin])/ $
                                     (y[(*info).yrmax]-y[(*info).yrmin]))
;    (max(x)-min(x))/(max(y)-min(y))
;    if *(*info).aux->getaspect() ne 0.0 then aspect=*(*info).aux->getaspect()
    (*info).aspect=aspect
    d_xsz = (*info).d_xsz; + (*info).menu_xsz +(*info).cb_xsz > 1
    d_ysz = d_xsz/aspect + (*info).menu_ysz > 1
    (*info).d_ysz=d_ysz
; now prepare for uneven grid spacing
    dx=x-shift(x,1) & dx[0]=dx[1]
    dy=y-shift(y,1) & dy[0]=dy[1]
    xx=x#(intarr(n_elements(y))+1.)
    yy=(intarr(n_elements(x))+1.)#y
    *(*info).xx=xx
    *(*info).yy=yy
    if abs((max(dx)-min(dx))/(max(dx)+min(dx))) gt 0.01 or $
       abs((max(dy)-min(dy))/(max(dy)+min(dy))) gt 0.01 then begin
;       triangulate,xx,yy,tr
;       print,'triangulage done',abs((max(dx)-min(dx))/(max(dx)+min(dx))),abs((max(dy)-min(dy))/(max(dy)+min(dy)))
;       *(*info).tr=tr
       (*info).uneven=1
    endif else (*info).uneven=0
  endif else begin
    d_xsz = (*info).d_xsz
    d_ysz = (*info).d_ysz
  endelse
  pseudoevent={widget_base,id:0l,top:(*info).tlb,handler:0l, $
                 x: d_xsz,y: d_ysz}
  widget_control,event.top,set_uvalue=info
      xmhd_resize,pseudoevent
end

; slider to select raster position of the third axis
pro xmhd_rslider, event

  widget_control, event.top,get_uvalue=info
  (*info).rpos=event.value
  case strupcase(strmid((*info).rtitle,0,1)) of
     'X': r = *(*info).hdr->getx()
     'Y': r = *(*info).hdr->gety()
     'Z': r = *(*info).hdr->getz()
     'T': r = *(*info).hdr->gett()
    else:
  endcase
  info_txt=' '+(*info).rtitle+' '+strtrim(string(r[(*info).rpos],format='(f7.3)'),2)
  widget_control,(*info).info_rslider,set_value=info_txt
  pseudoevent={widget_button,id:(*info).action, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  xmhd_draw,pseudoevent
end

;+++++++++options for x range sliders+++++++++++++++++++++
; selects between index and coord tabs for x range min/max
pro xmhd_xtab, event
  widget_control, event.top,get_uvalue=info
end

; runs index xrange min or max
pro xmhd_xtabi, event

  widget_control, event.top,get_uvalue=info
  widget_control,event.id,get_uvalue=uvalue,get_value=value
  case uvalue of
     'xminslider' : xmhd_xminslider, event
     'xmaxslider' : xmhd_xmaxslider, event
  endcase
end

; runs coord xrange min or max
pro xmhd_xtabc, event

  widget_control, event.top,get_uvalue=info
  widget_control,event.id,get_uvalue=uvalue,get_value=value
  case uvalue of
      'xxminslider' : xmhd_xxminslider, event
      'xxmaxslider' : xmhd_xxmaxslider, event
   endcase
end

; selects xrange min for coord
pro xmhd_xxminslider, event

  widget_control, event.top,get_uvalue=info

  x=*(*info).xscale
  y=*(*info).yscale

  minx=min(abs(x-event.value),min_subscript)
  (*info).xrmin=min_subscript
  widget_control, (*info).xminslider,set_value=min_subscript
  if (*info).aspect ne 1 then (*info).aspect=abs((x[(*info).xrmax]-x[(*info).xrmin])/ $
                                                 (y[(*info).yrmax]-y[(*info).yrmin]))
  aspect=(*info).aspect
  d_xsz = (*info).d_xsz
  d_ysz = d_xsz/aspect + (*info).menu_ysz > 1
  (*info).d_ysz=d_ysz

  pseudoevent={widget_base,id:0l,top:(*info).tlb,handler:0l, $
                  x: d_xsz,y: d_ysz}
   widget_control,event.top,set_uvalue=info
   xmhd_resize,pseudoevent
end

; selects xrange max for coord
pro xmhd_xxmaxslider, event

    widget_control, event.top,get_uvalue=info

    x=*(*info).xscale
    y=*(*info).yscale
    maxx=min(abs(x-event.value),min_subscript)
    (*info).xrmax=min_subscript
    widget_control, (*info).xmaxslider,set_value=min_subscript
    if (*info).aspect ne 1 then (*info).aspect=abs((x[(*info).xrmax]-x[(*info).xrmin])/ $
                                                  (y[(*info).yrmax]-y[(*info).yrmin]))
    aspect=(*info).aspect
    d_xsz = (*info).d_xsz
    d_ysz = d_xsz/aspect + (*info).menu_ysz > 1
    (*info).d_ysz=d_ysz
    pseudoevent={widget_base,id:0l,top:(*info).tlb,handler:0l, $
                  x: d_xsz,y: d_ysz}
    widget_control,event.top,set_uvalue=info
    xmhd_resize,pseudoevent
 end

; selects xrange min for index
pro xmhd_xminslider, event

  widget_control, event.top,get_uvalue=info
  (*info).xrmin=event.value
  x=*(*info).xscale
  y=*(*info).yscale
  widget_control, (*info).xxminslider,set_value=x((*info).xrmin)
  if (*info).aspect ne 1 then (*info).aspect=abs((x[(*info).xrmax]-x[(*info).xrmin])/ $
                                                 (y[(*info).yrmax]-y[(*info).yrmin]))
  aspect=(*info).aspect
  d_xsz = (*info).d_xsz; + (*info).menu_xsz +(*info).cb_xsz > 1
  d_ysz = d_xsz/aspect + (*info).menu_ysz > 1
  (*info).d_ysz=d_ysz
  pseudoevent={widget_base,id:0l,top:(*info).tlb,handler:0l, $
                 x: d_xsz,y: d_ysz}
  widget_control,event.top,set_uvalue=info
      xmhd_resize,pseudoevent
end

; selects xrange max for index
pro xmhd_xmaxslider, event

  widget_control, event.top,get_uvalue=info
  (*info).xrmax=event.value

  x=*(*info).xscale
  y=*(*info).yscale
  widget_control, (*info).xxmaxslider,set_value=x((*info).xrmax)
  if (*info).aspect ne 1 then (*info).aspect=abs((x[(*info).xrmax]-x[(*info).xrmin])/ $
                                                 (y[(*info).yrmax]-y[(*info).yrmin]))
  aspect=(*info).aspect
  d_xsz = (*info).d_xsz; + (*info).menu_xsz +(*info).cb_xsz > 1
  d_ysz = d_xsz/aspect + (*info).menu_ysz > 1
  (*info).d_ysz=d_ysz
  pseudoevent={widget_base,id:0l,top:(*info).tlb,handler:0l, $
                 x: d_xsz,y: d_ysz}
  widget_control,event.top,set_uvalue=info
  xmhd_resize,pseudoevent

end

;+++++++++options for y range sliders+++++++++++++++++++++
; selects yrange index or coord tab
pro xmhd_ytab, event
  widget_control, event.top,get_uvalue=info
end

; runs index yrange min or max
pro xmhd_ytabi, event

  widget_control, event.top,get_uvalue=info
  widget_control,event.id,get_uvalue=uvalue,get_value=value
  case uvalue of
      'yminslider' : xmhd_yminslider, event
      'ymaxslider' : xmhd_ymaxslider, event
   endcase
end

; runs coords yrange min or max
pro xmhd_ytabc, event

  widget_control, event.top,get_uvalue=info
  widget_control,event.id,get_uvalue=uvalue,get_value=value
  case uvalue of
      'yyminslider' : xmhd_yyminslider, event
      'yymaxslider' : xmhd_yymaxslider, event
   endcase
end

; selects yrange min for coord
pro xmhd_yyminslider, event

  widget_control, event.top,get_uvalue=info
  x=*(*info).xscale
  y=*(*info).yscale

  miny=min(abs(y-event.value),min_subscript)
  (*info).yrmin=min_subscript
  widget_control, (*info).yminslider,set_value=min_subscript
  if (*info).aspect ne 1 then (*info).aspect=abs((x[(*info).xrmax]-x[(*info).xrmin])/ $
                                                 (y[(*info).yrmax]-y[(*info).yrmin]))
  aspect=(*info).aspect
  d_xsz = (*info).d_xsz
  d_ysz = d_xsz/aspect + (*info).menu_ysz > 1
  (*info).d_ysz=d_ysz
  pseudoevent={widget_base,id:0l,top:(*info).tlb,handler:0l, $
                 x: d_xsz,y: d_ysz}
  widget_control,event.top,set_uvalue=info
  xmhd_resize,pseudoevent
end

; selects yrange min for coord
pro xmhd_yymaxslider, event

  widget_control, event.top,get_uvalue=info
  x=*(*info).xscale
  y=*(*info).yscale

  maxy=min(abs(y-event.value),min_subscript)
  (*info).yrmax=min_subscript
  widget_control, (*info).ymaxslider,set_value=min_subscript
  if (*info).aspect ne 1 then (*info).aspect=abs((x[(*info).xrmax]-x[(*info).xrmin])/ $
                                                 (y[(*info).yrmax]-y[(*info).yrmin]))
  aspect=(*info).aspect
  d_xsz = (*info).d_xsz
  d_ysz = d_xsz/aspect + (*info).menu_ysz > 1
  (*info).d_ysz=d_ysz
  pseudoevent={widget_base,id:0l,top:(*info).tlb,handler:0l, $
                 x: d_xsz,y: d_ysz}
  widget_control,event.top,set_uvalue=info
  xmhd_resize,pseudoevent
end

; selects yrange min for index
pro xmhd_yminslider, event

  widget_control, event.top,get_uvalue=info
  (*info).yrmin=event.value
  x=*(*info).xscale
  y=*(*info).yscale
  widget_control, (*info).yyminslider,set_value=y((*info).yrmin)
  if (*info).aspect ne 1 then (*info).aspect=abs((x[(*info).xrmax]-x[(*info).xrmin])/ $
                                                 (y[(*info).yrmax]-y[(*info).yrmin]))
  aspect=(*info).aspect
  d_xsz = (*info).d_xsz; + (*info).menu_xsz +(*info).cb_xsz > 1
  d_ysz = d_xsz/aspect + (*info).menu_ysz > 1
  (*info).d_ysz=d_ysz
  pseudoevent={widget_base,id:0l,top:(*info).tlb,handler:0l, $
                 x: d_xsz,y: d_ysz}
  widget_control,event.top,set_uvalue=info
  xmhd_resize,pseudoevent
end

; selects yrange max for index
pro xmhd_ymaxslider, event

  widget_control, event.top,get_uvalue=info
  (*info).yrmax=event.value
  x=*(*info).xscale
  y=*(*info).yscale
  widget_control, (*info).yymaxslider,set_value=y((*info).yrmax)
  if (*info).aspect ne 1 then (*info).aspect=abs((x[(*info).xrmax]-x[(*info).xrmin])/ $
                                                 (y[(*info).yrmax]-y[(*info).yrmin]))
  aspect=(*info).aspect
  d_xsz = (*info).d_xsz; + (*info).menu_xsz +(*info).cb_xsz > 1
  d_ysz = d_xsz/aspect + (*info).menu_ysz > 1
  (*info).d_ysz=d_ysz
  pseudoevent={widget_base,id:0l,top:(*info).tlb,handler:0l, $
                 x: d_xsz,y: d_ysz}
  widget_control,event.top,set_uvalue=info
  xmhd_resize,pseudoevent
end

;+++++++++options for intensity range sliders+++++++++++++++++++++
; selects irange index or coord tab
pro xmhd_itab, event

  widget_control, event.top,get_uvalue=info
end

; runs index irange min or max
pro xmhd_itabi, event

  widget_control, event.top,get_uvalue=info
  widget_control,event.id,get_uvalue=uvalue,get_value=value
  case uvalue of
      'iminslider' : xmhd_iminslider, event
      'imaxslider' : xmhd_imaxslider, event
   endcase
end

; runs coords irange min or max
pro xmhd_itabc, event

  widget_control, event.top,get_uvalue=info
  widget_control,event.id,get_uvalue=uvalue,get_value=value
  case uvalue of
      'iiminslider' : xmhd_iiminslider, event
      'iimaxslider' : xmhd_iimaxslider, event
   endcase
end

; selects irange min for coord
pro xmhd_iiminslider, event

  widget_control, event.top,get_uvalue=info
  (*info).ircoord=1
  (*info).ircoordrange[0]=event.value
    imn=(*info).intmin
    imx=(*info).intmax
    tmax=(*info).ivmax
    tmin=(*info).ivmin
    iv=(event.value-tmin)*255/(tmax-tmin)
    widget_control, (*info).iminslider,set_value=floor(iv)
  pseudoevent={widget_button,id:(*info).action, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  xmhd_draw,pseudoevent
end

; selects irange max for coord
pro xmhd_iimaxslider, event

  widget_control, event.top,get_uvalue=info
  (*info).ircoord=1
  (*info).ircoordrange[1]=event.value
  imn=(*info).intmin
  imx=(*info).intmax
  tmax=(*info).ivmax
  tmin=(*info).ivmin
  iv=(event.value-tmin)*255/(tmax-tmin)
  widget_control, (*info).imaxslider,set_value=floor(iv)
  pseudoevent={widget_button,id:(*info).action, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  xmhd_draw,pseudoevent
end

; selects irange min for index
pro xmhd_iminslider, event

  widget_control, event.top,get_uvalue=info
  (*info).irmin=event.value
  (*info).ircoord=0
  imn=(*info).intmin
  imx=(*info).intmax
  tmax=(*info).ivmax
  tmin=(*info).ivmin
  iv=(tmax-tmin)*floor(event.value)/255.+tmin
  widget_control, (*info).iiminslider,set_value=iv
  pseudoevent={widget_button,id:(*info).action, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  xmhd_draw,pseudoevent
end

; selects irange max for index
pro xmhd_imaxslider, event

  widget_control, event.top,get_uvalue=info
  (*info).irmax=event.value
  (*info).ircoord=0
  imn=(*info).intmin
  imx=(*info).intmax
  tmax=(*info).ivmax
  tmin=(*info).ivmin
  iv=(tmax-tmin)*floor(event.value)/255.+tmin
  widget_control, (*info).iimaxslider,set_value=iv
  pseudoevent={widget_button,id:(*info).action, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  xmhd_draw,pseudoevent
end

;++++++++++++++++++++buttons options of the main window++++++++++++++
; for automatic scale
function xmhd_iroption, event

  widget_control, event.top, get_uvalue = info
  (*info).iroption = ((*info).iroption + 1) mod 2
  if (*info).iroption then sensitive=0 else sensitive=1
  widget_control, (*info).iminslider, set_slider_min=0, $
               set_slider_max=255, set_value=0, sensitive=sensitive
  widget_control, (*info).imaxslider, set_slider_min=0, $
               set_slider_max=255, set_value=255, sensitive=sensitive
  pseudoevent={widget_button,id:(*info).action, $
               top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  xmhd_draw,pseudoevent
  return,0
end

; for logarithmic scale
function xmhd_lgoption, event

  eps=1.e-30
  widget_control, event.top, get_uvalue = info
  (*info).lgoption = ((*info).lgoption + 1) mod 2
  if (*info).lgoption then begin
    sensitive=0
    (*info).intmin=alog10(abs((*info).intmin)>eps)
    (*info).intmax=alog10(abs((*info).intmax)>eps)
  endif else begin
    sensitive=1
    (*info).intmin=(*info).intminold
    (*info).intmax=(*info).intmaxold
  endelse
  pseudoevent={widget_button,id:(*info).action, $
               top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  xmhd_draw,pseudoevent
  return,0
end

; for 10^() scale
function xmhd_expoption, event

  widget_control, event.top, get_uvalue = info
  (*info).expoption = ((*info).expoption + 1) mod 2
  if (*info).expoption then begin
    sensitive=0
    (*info).intmin=10^((*info).intmin)
    (*info).intmax=10^((*info).intmax)
  endif else begin
    sensitive=1
    (*info).intmin=(*info).intminold
    (*info).intmax=(*info).intmaxold
  endelse
  pseudoevent={widget_button,id:(*info).action, $
               top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  xmhd_draw,pseudoevent

  return,0

end

;for \nosq option
function xmhd_nosqoption, event

  widget_control, event.top, get_uvalue = info
  (*info).nosqoption = ((*info).nosqoption + 1) mod 2
  if (*info).nosqoption then  sensitive=0 else sensitive=1
  pseudoevent={widget_button,id:(*info).action, $
               top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  xmhd_draw,pseudoevent
  return,0

end

; for logarithmic scale
function xmhd_absoption, event
  widget_control, event.top, get_uvalue = info
  (*info).absoption = ((*info).absoption + 1) mod 2
  if (*info).absoption then begin
    sensitive=0
    if (*info).intmin lt 0.0 then (*info).intmin=0.0
    (*info).intmax=abs((*info).intmax)
  endif else begin
    sensitive=1
    (*info).intmin=(*info).intminold
    (*info).intmax=(*info).intmaxold
  endelse
  pseudoevent={widget_button,id:(*info).action, $
               top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  xmhd_draw,pseudoevent
  return,0
end

; button for flip z axis
function xmhd_flip, event

  widget_control, event.top, get_uvalue = info
  (*info).flip = ((*info).flip + 1) mod 2
  if (*info).flip then sensitive=0 else sensitive=1
  pseudoevent={widget_button,id:(*info).action, $
               top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  xmhd_draw,pseudoevent
  return,0
end

; button for contours
function xmhd_betacontour, event

  widget_control, event.top, get_uvalue = info
  (*(*info).bt).swt = ((*(*info).bt).swt + 1) mod 2
  if (*(*info).bt).swt eq 1 then sensitive=1 else sensitive=0
  if (*(*info).bt).name eq '' then begin
     (*(*info).bt).name='beta'
     xmhd_calcbeta,info
  endif
  widget_control, (*(*info).bt).wcont, set_value=sensitive
  pseudoevent={widget_button,id:(*info).action, $
               top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  xmhd_draw,pseudoevent
  return,0
end

; button for vectors
function xmhd_vectfield, event

  widget_control, event.top, get_uvalue = info
  widget_control, (*(*info).uv).wvectfield, get_value=sensitive
  (*(*info).uv).swt = sensitive
  uv=*(*info).uv
  widget_control,event.top,set_uvalue=info
  widget_control, (*(*info).uv).wvectfield, set_value=sensitive
  xmhd_vectordraw, info=info, uv=uv
  xmhd_vector_define, uv, info=info, datax=uv.datax, swt= (*(*info).uv).swt,wvectfield=(*(*info).uv).wvectfield, $
                      xr=(*(*info).uv).xr,yr=(*(*info).uv).yr
  pseudoevent={widget_button,id:(*info).action, $
               top:event.top, handler:0l, select:1}
  xmhd_draw, pseudoevent
  return,0
end

; button for lines
function xmhd_magfield, event

  widget_control, event.top, get_uvalue = info
  if (*(*info).fc).swt le 1 then (*(*info).fc).swt = ((*(*info).fc).swt + 1) mod 2
  if (*(*info).fc).swt gt 1 then (*(*info).fc).swt = ((*(*info).fc).swt + 1) mod 2 + 2
  if ((*(*info).fc).swt eq 1 or (*(*info).fc).swt eq 2) then begin
     sensitive=1
  endif else begin
     sensitive=0
  endelse
  widget_control, (*(*info).fc).wfield, set_value=sensitive
  pseudoevent={widget_button,id:(*info).action, $
               top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  xmhd_draw,pseudoevent
  return,0
end

;++++++++++ activates the window for 1d plot from the 3 buttons.
function xmhd_pixplot, event

  widget_control, event.top, get_uvalue = info
  (*info).pxoption = event.value
  mode=(*info).pxoption+1
  data = *(*info).data->getvar((*info).line)
  case mode of
     1: begin
        xtitle=*(*info).aux->getxtitle()
        mode = 2
        case strupcase(strmid(xtitle,0,1)) of
           'X': xscale = *(*info).hdr->getx()
           'Y': xscale = *(*info).hdr->gety()
           'Z': xscale = *(*info).hdr->getz()
           'T': xscale = *(*info).hdr->gett()
           else:
        endcase
        xpixplot,data,mode,xtitle=xtitle,ytitle=*(*info).data->getvarname(), $
                 xscale=xscale,groupl = (*info).tlb
     end
     2: begin
        xtitle=*(*info).aux->getytitle()
        mode = 1
        case strupcase(strmid(xtitle,0,1)) of
           'X': xscale = *(*info).hdr->getx()
           'Y': xscale = *(*info).hdr->gety()
           'Z': xscale = *(*info).hdr->getz()
           'T': xscale = *(*info).hdr->gett()
           else:
        endcase
        xpixplot,data,mode,xtitle=xtitle,ytitle=*(*info).data->getvarname(), $
                 xscale=xscale,groupl = (*info).tlb
     end
     3: begin
        xtitle=*(*info).aux->getxrastertitle()
        mode = 2
        data = transpose(data)
        case strupcase(strmid(xtitle,0,1)) of
           'X': xscale = *(*info).hdr->getx()
           'Y': xscale = *(*info).hdr->gety()
           'Z': xscale = *(*info).hdr->getz()
           'T': xscale = *(*info).hdr->gett()
           else:
        endcase
        xpixplot,data,mode,xtitle=xtitle,ytitle=*(*info).data->getvarname(), $
                 xscale=xscale,groupl = (*info).tlb
     end
     else: begin
        print,'xmhd_pixplot: impossible mode!!'
     end
  endcase
  return,0
end

; +++++++++ three big bottuns in main window (close, animation,
; params) +++++++++++++++++++++++++++++++++++++++++++++++++++++

; selection of the animation widget mode: 1) Without overlying things (faster)
; 2) with overlying things (Slower)
pro xmhd_animsel, event

  widget_control, event.top, get_uvalue = info
  if (*(*info).fc).swt eq 0 and (*(*info).uv).swt eq 0 and (*(*info).bt).swt eq 0 and (*info).iroption eq 0 then begin
     xmhd_anim, event
  endif else begin
     xmhd_animtvrd, event
  endelse
end

; create animation widget and launch animation without overlying things
pro xmhd_anim, event

  widget_control, event.top, get_uvalue = info
  *(*info).data-> getwin, (*info).line, wd, pos
  sz=size(wd)
  nx=sz[1]
  ny=sz[2]
  nz=sz[3]

  z=(*(*info).data)->getz()
  dx=(*(*info).data)->getdx()
  nelz=n_elements(z)
  maxz=max(z) & minz=min(z)
  newnz=(maxz-minz)/dx
  newz=minz+dx*findgen(newnz)

  case (*info).mode of
    0: begin
         xax=*(*info).aux->getxtitle()
         yax=*(*info).aux->getytitle()
         nt = nz
         mx = nx
         my = ny
       end
    1: begin
         xax=*(*info).aux->getytitle()
         yax=*(*info).aux->getxrastertitle()
         nt = nx
         mx = ny
         my = nz
         wd=transpose(wd,[1,2,0])
       end
    2: begin
         xax=*(*info).aux->getxtitle()
         yax=*(*info).aux->getxrastertitle()
         nt = ny
         mx = nx
         my = nz
         wd=transpose(wd,[0,2,1])
       end
  endcase
  revdim = 0
  xax =  strupcase(strmid(xax,0,1))
  yax =  strupcase(strmid(yax,0,1))
  if (xax eq 'Z') then begin
     revdim=1
     mx=newnz
  endif
  if (yax eq 'Z') then begin
     revdim=2
     my=newnz
  endif
  image=fltarr(mx,my,nt)

  revdiv=(*info).flip

  case revdim of
     1: begin
        if (*info).uneven then begin
           if revdiv eq 0 then begin
              for iy=0,my-1 do begin
                 for it=0,nt-1 do begin
                    image[*,iy,it] = interpol( wd[*,iy,it], z, newz  )
                 endfor
              endfor
           endif else begin
              for iy=0,my-1 do begin
                 for it=0,nt-1 do begin
                    image[*,iy,it] = reverse(interpol( wd[*,iy,it], z, newz  ),revdiv)
                 endfor
              endfor
           endelse
        endif else begin
           image=wd
        endelse
     end
     2: begin
        if (*info).uneven then begin
           if revdiv eq 0 then begin
              for ix=0,mx-1 do begin
                 for it=0,nt-1 do begin
                    image[ix,*,it] = interpol( wd[ix,*,it], z, newz  )
                 endfor
              endfor
           endif else begin
              for ix=0,mx-1 do begin
                 for it=0,nt-1 do begin
                    image[ix,*,it] = reverse(interpol( wd[ix,*,it], z, newz  ),revdiv)
                 endfor
              endfor
           endelse
        endif else begin
           image=wd
        endelse
     end
     else : image=wd
  endcase

  if (*info).iroption then ir=[min(image),max(image)] else begin
    imn=(*info).intmin
    imx=(*info).intmax
    ir=[imn+(imx-imn)/255.*(*info).irmin, $
        imn+(imx-imn)/255.*(*info).irmax]
  endelse
  if (*info).ircoord then ir=(*info).ircoordrange
  if (*info).ircoord and (*info).lgoption then ir=alog10(ir)
  if (*info).ircoord and (*info).expoption then ir=10^(ir)

;  magnification=fix(0.5*(*info).screensize[0]/float(mx))
;  magnification=(fix(0.5*(*info).screensize[1]/float(my))>1) < magnification
  if (*info).absoption then begin
     image=abs(image)
  endif
  if (*info).lgoption then begin
     eps=1.e-30
     rr=where(image gt 0,count)
     if (count gt 0) then image=image > min(image[rr])
     image=alog10(image)
  endif
  if (*info).expoption then begin
     image=10^(image)
  endif
  image[where(image ge ir[1])] = ir[1]
  image[where(image le ir[0])] = ir[0]
  minimage = min(image)
  maximage = max(image)
  assoc_file = *(*info).aux-> getassoc_file()
  openw,lu, assoc_file, /get_lun
  rec=assoc(lu,image)
  rec[0]=image
  close,lu
  free_lun,lu
  aspect=my/(mx*0.9999)
  ximovie,assoc_file,mx,my,/float,nframes=nt, $
          aspect=aspect, $      ;imin=min(wd), imax=max(wd)
          magnification=magnification,imin=minimage,imax=maximage
end

; create animation widget and launch animation with overlying things
pro xmhd_animtvrd, event

  widget_control, event.top, get_uvalue = info
  *(*info).data-> getwin, (*info).line, wd, pos
  sz=size(wd)
  nx=sz[1]
  ny=sz[2]
  nz=sz[3]

  case (*info).mode of
    0: nt = nz
    1: nt = nx
    2: nt = ny
  endcase

  set_plot,'z'
  device,decomposed=0
  for it=0,nt-1 do begin
     (*info).rpos=it
     if where(indgen(200)*10 eq it) ne -1 then print,'Creating animation '+strtrim(string(100.*(it*1.)/(nt*1.0),format='(F4.1)'),2)+'% done'
     pseudoevent={widget_button,id:(*info).action, $
                  top:event.top, handler:0l, select:1}
     xmhd_draw,pseudoevent
  endfor
  set_plot,'x'
  assoc_file = *(*info).aux-> getassoc_file()
  openw,lu, assoc_file, /get_lun
  mx=n_elements((*(*info).anim)(*,0,0))
  my=n_elements((*(*info).anim)(0,*,0))
  nt=n_elements((*(*info).anim)(0,0,*))
  rec=assoc(lu,(*(*info).anim)*1.0)
  rec[0]=*(*info).anim*1.0
  close,lu
  free_lun,lu
  aspect=my/(mx*0.9999)
  ximovie,assoc_file,mx,my,/float,nframes=nt, $;  ximovie,assoc_file,100,100,/float,nframes=100, $
          aspect=aspect, $      ;imin=min(wd), imax=max(wd)
          magnification=magnification,imin=min(*(*info).anim)*1.0,imax=max(*(*info).anim)*1.0
end

; display header in text window
pro xmhd_hdrdisp,event

  widget_control, event.top ,get_uvalue = info
  ; open text widget window to dump bytes in
  hdr_widget = widget_base(title = '.idl file contents',  $
                group_leader = (*info).tlb,/row)

  closefield = widget_base(hdr_widget,/column)
  closebutton = widget_button(closefield, value = 'Close', $
                              event_pro = 'xmhd_hdrdisp_destroy')

  idlparams=(*(*info).data)->getidlparams()
  ysz = 60.*n_elements((idlparams))
  disp_base = widget_base(hdr_widget,/column)
  disp_field = widget_text(disp_base, group_leader = (*info).tlb, /scroll, $
  xs=100, ysize = 50)

  widget_control, disp_field, set_value = idlparams,/append
  widget_control, hdr_widget, /realize

  xmanager, 'Display header contents', hdr_widget, /no_block, $
             group_leader = (*info).tlb
end

; close header display
pro xmhd_hdrdisp_destroy, event
 widget_control, event.top,/destroy
end

; close xmhd
pro xmhd_destroy, event

  widget_control, event.top,/destroy
end

;++++++++++++++++++++++
; options of maneging the window size and cleaning
pro xmhd_cleanup, tlb

  widget_control, tlb, get_uvalue = info
  wdelete, (*info).mainpixid
  ptr_free, info
end

;++++++++++++++++++ tab file++++++++++++++++++++++

;save as .jpeg, .jpg, .png, .ps
pro xmhd_writeimage,event

  thisfile=dialog_pickfile(file='idl.jpg',$
   filter=[['*.jpg;*.jpeg','*.png','.ps'],['jpeg','png', 'ps']],$
  /overwrite_prompt,title='valid formats: .jpeg, .jpg, .png, .ps')
  if thisfile eq '' then return
  widget_control,event.top,get_uvalue=info
  wset,(*info).wid
  ;if strpos(thisfile,'.bmp') ne -1 then write_bmp,thisfile, tvrd(true=1)
  if strpos(thisfile,'.png') ne -1 then write_png,thisfile, tvrd(true=1)
  if strpos(thisfile,'.jpeg') ne -1 then write_jpeg,thisfile,tvrd(true=1),true=1,quality=90
  if strpos(thisfile,'.jpg') ne -1 then write_jpeg,thisfile,tvrd(true=1),true=1,quality=90
  ;if strpos(thisfile,'.tiff') ne -1 then write_tiff,thisfile, tvrd(true=1)
  ;if strpos(thisfile,'.tif') ne -1 then write_tiff,thisfile, tvrd(true=1)
  ;if strpos(thisfile,'.gif') ne -1 then write_gif,thisfile, tvrd()
  if strpos(thisfile,'.ps') ne -1 then begin
     widget_control,event.top,get_uvalue=info
     thisdevice=!d.name
     set_plot,'ps'              ;,/copy
     xpsize=13.5
     calc_xysize,xpsize,xpsize/((*info).aspect*0.99),xsize,ysize
     xm=!x.margin
     ym=!x.margin
    !x.margin=[8,1]
    !y.margin=[4,7]
     device,file=thisfile,_extra=keywords,bits_per_pixel=8,/color,xsize=xsize,ysize=ysize,/encapsulated
     pseudoevent={widget_button,id:(*info).action, $
     top:event.top, handler:0l, select:1}
     widget_control,event.top,set_uvalue=info
     xmhd_draw,pseudoevent
     device,/close_file
     set_plot,thisdevice
     !x.margin = xm
     !y.margin = ym
  endif
end

;++++++++++++++++++ tab options++++++++++++++++++++++
; select color table
pro xmhd_colors, event

  widget_control, event.top, get_uvalue=info
  thisevent = tag_names(event, /structure_name)
  case thisevent of
  'WIDGET_BUTTON': begin
      xcolors, ncolors = (*info).ncolors, bottom = (*info).bottom, $
        title = 'xmhd colors (' + strtrim((*info).wid, 2) + ')', $
        group_leader = event.top, notifyid = [event.id, event.top]
      endcase
  'XCOLORS_LOAD': begin
      (*info).r = event.r((*info).bottom:(*info).ncolors-1 + (*info).bottom)
      (*info).g = event.g((*info).bottom:(*info).ncolors-1 + (*info).bottom)
      (*info).b = event.b((*info).bottom:(*info).ncolors-1 + (*info).bottom)
      if !d.n_colors gt 256 then begin
        pseudoevent={widget_button,id:(*info).action, $
          top:event.top, handler:0l, select:1}
        ;widget_control, (*info).action, send_event=pseudoevent
        xmhd_draw,pseudoevent
      endif
    endcase
  endcase
  widget_control, event.top, set_uvalue = info

end

; ----------- colour setup--------------------
; option window for colour setup
pro xmhd_setcolors, event

  widget_control, event.top, get_uvalue = info

  base = widget_base(/row, title='Layout setup',uvalue=event.top)
  inforef=widget_base(base,uvalue=info,uname='inforef')
  wbase = widget_base(base,  column=2, frame=2, /base_align_left)

  top = widget_base(wbase, column=1,event_pro='chosetop',/sensitive)
  topc=cw_field(top,   title='Top index color   ',/frame,/all_events, $
                 value=(*info).topcolor,uname='chosetop',xsize=3)

  bot = widget_base(wbase, column=1,event_pro='chosebottom',/sensitive)
  bottomc=cw_field(bot,title='Bottom index color',/frame,/all_events, $
                 value=(*info).bottomcolor,uname='chosebottom',xsize=3)

  charsize = widget_base(wbase, column=1,event_pro='charsize',/sensitive)
  charsizec=cw_field(charsize,title='Charsize         ',/frame,/all_events, $
                 value=!p.charsize,uname='charsize',xsize=4)

  charthick = widget_base(wbase, column=1,event_pro='charthick',/sensitive)
  charthickc=cw_field(charthick,title='Charthick        ',/frame,/all_events, $
                 value=!p.charthick,uname='charthick',xsize=4)

  background = widget_base(wbase, column=1,event_pro='background',/sensitive)
  backgroundc=cw_field(background,title='Background     ',/frame,/all_events, $
                 value=!p.background,uname='background',xsize=6)

  color = widget_base(wbase, column=1,event_pro='color',/sensitive)
  colorc=cw_field(color,title='Color       ',/frame,/all_events, $
                 value=!p.color,uname='color',xsize=9)

  thick = widget_base(wbase, column=1,event_pro='thick',/sensitive)
  thickc=cw_field(thick,title='Thick  ',/frame,/all_events, $
                 value=!p.thick,uname='thick',xsize=5)

  xthick = widget_base(wbase, column=1,event_pro='xthick',/sensitive)
  xthickc=cw_field(xthick,title='Xthick ',/frame,/all_events, $
                 value=!x.thick,uname='xthick',xsize=5)

  ythick = widget_base(wbase, column=1,event_pro='ythick',/sensitive)
  ythickc=cw_field(ythick,title='Ythick ',/frame,/all_events, $
                 value=!y.thick,uname='ythick',xsize=5)

  xticks = widget_base(wbase, column=1,event_pro='xticks',/sensitive)
  xticksc=cw_field(xticks,title='Xticks ',/frame,/all_events, $
                 value=!x.ticks,uname='xticks',xsize=5)

  yticks = widget_base(wbase, column=1,event_pro='yticks',/sensitive)
  yticksc=cw_field(yticks,title='Yticks ',/frame,/all_events, $
                 value=!y.ticks,uname='yticks',xsize=5)

  ticklen = widget_base(wbase, column=1,event_pro='ticklen',/sensitive)
  ticklenc=cw_field(ticklen,title='Ticklen',/frame,/all_events, $
                 value=!p.ticklen,uname='ticklen',xsize=5)

  widget_control, base, /realize

  colorok=widget_button(base,value='Ok',event_pro='xmhd_colorok')
  colorreset=widget_button(base,value='Reset',event_pro='xmhd_colorreset')

  xmanager,'Colours setup', base, /just_reg, /no_block, group_leader = group
end

; chose the highest index color value
pro chosetop, event
  inforef=widget_info(event.top, find_by_uname='inforef')
  widget_control, inforef, get_uvalue = info
  (*info).topcolor = event.value
  widget_control, inforef, set_uvalue = info
end

; chose the lowest index color value
pro chosebottom, event
  inforef=widget_info(event.top, find_by_uname='inforef')
  widget_control, inforef, get_uvalue = info
  (*info).bottomcolor = event.value
  widget_control, inforef, set_uvalue = info
end

; chose the lowest index color value
pro thick, event
  !p.thick = event.value
end

pro charsize, event
  !p.charsize = event.value
end

pro charthick, event
  !p.charthick = event.value
end

pro background, event
  !p.background = event.value
end

pro color, event
  !p.color = event.value
end

pro xthick, event
  !x.thick = event.value
end

pro ythick, event
  !y.thick = event.value
end

pro xticks, event
  !x.ticks = event.value
end

pro yticks, event
  !y.ticks = event.value
end

pro ticklen, event
  !p.ticklen = event.value
end

; reset layout setup window
pro xmhd_colorreset, event
  inforef=widget_info(event.top, find_by_uname='inforef')
  widget_control, inforef, get_uvalue = info
  widget_control, event.top,get_uvalue=top
  (*info).topcolor = 255
  (*info).bottomcolor = 0
  !p.thick = 0.0
  !p.charsize = 0.0
  !p.charthick = 0.0
  !p.background = 0
  !p.color = 16777215
  !x.thick = 0.0
  !y.thick = 0.0
  !x.ticks = 0
  !y.ticks = 0
  !p.ticklen = 0.02
  pseudoevent={widget_button,id:(*info).action, $
               top:top, handler:0l, select:1}
  xmhd_draw,pseudoevent
  widget_control, event.top,/destroy
end

; close colour setup window
pro xmhd_colorok, event

  inforef=widget_info(event.top, find_by_uname='inforef')
  widget_control, inforef, get_uvalue = info
  widget_control, event.top,get_uvalue=top

  pseudoevent={widget_button,id:(*info).action, $
               top:top, handler:0l, select:1}
  xmhd_draw,pseudoevent
  widget_control, event.top,/destroy
end

; ----------- plot hdr params--------------------
; reads the params field, plots or print
pro readhdrvar,event

  widget_control, event.top,get_uvalue=top
  widget_control, top,get_uvalue=info
  result=execute('var=float(*(*info).data->get'+event.value[0]+'())')
  if result eq 0 then begin
     print, "variable "+event.value[0]+" not found"
     return
  endif
  (*(*info).hdrvar).var = var
  (*(*info).hdrvar).name = event.value
  uvref=widget_info(event.top, find_by_uname='hdrvar')
  widget_control,uvref,set_uvalue=info
  widget_control, (*(*info).hdrvar).wid, get_value = wid
  itsnap = (*(*info).data->getisnaps())
  time=(*(*info).hdr)->gett()
  wset,wid
  if n_elements(itsnap) eq 1 then begin
     mens1=(*(*info).hdrvar).name+' = '+strtrim((*(*info).hdrvar).var,2)
     mens2='time: '+strtrim(time,2)
     erase,0
     xyouts,300,220.0,mens1,/device, charsize=2,charthick=1.5, alignment=0.5
     xyouts,300,180.0,mens2,/device, charsize=2,charthick=1.5, alignment=0.5
  endif else begin

     vary=(*(*info).hdrvar).var
     if strpos((*info).rtitle,'[hs]') ne -1 then begin
        xtitle=strmid((*info).rtitle,0,1)+' [s]'
        times=time*100
     endif else begin
        xtitle=(*info).rtitle
        times=time
     endelse
     plot,times,vary,xtitle=xtitle,ytitle=(*(*info).hdrvar).name, $
     yrange=[min(vary), max(vary)], ys=1, xs=1, $
     charsize=2.0, thick=1.5,charthick=1.5
  endelse
end

; define substructure for params plots
pro xmhd_hdr_define,struct,hdrvar=hdrvar,info=info,name=name,wid=wid

  nt=n_elements((*(*info).data->getisnaps()))
  if n_elements(var)  eq 0  then var=fltarr(nt)
  if n_elements(name)  eq 0  then name='nu1'
  if n_elements(wid)  eq 0  then wid=0
  struct={var:var, $
          wid:wid, $
          name:name}
end

; window of the plot hdr params
pro xmhd_hdrplot,event

  widget_control, event.top,get_uvalue=info

  xmhd_hdr_define,hdrstr,info=info
  itsnap = (*(*info).data->getisnaps())
  *(*info).hdrvar=hdrstr

  base = widget_base(/row, title='HDR PLOT',uvalue=event.top)
  col1 = widget_base(base,  column=2, frame=10)
  chosebase=widget_base(col1,column=1, /align_center,event_pro='readhdrvar',/sensitive)

  nname=cw_field(chosebase,title='Choose a variable',/frame,/string, $
                   value=hdrstr.name,/return_events,xsize=4, /column, uname='hdrvar')

  plotbase = widget_base(col1, column=1,/frame)
  pcid = widget_draw(plotbase,retain=2,xsize=600,ysize=400,uname='hdrvar')
  (*(*info).hdrvar).wid=pcid

  widget_control, base, /realize
  xmanager,'HDR PLOT', base, /just_reg, /no_block, group_leader = group
end

; ----------- Line setup--------------------
; define structure for lines
pro xmhd_field_define,struct,swt=swt,data=data,xax=xax,yax=yax,itsnap=itsnap,rpos=rpos,info=info,linedir=linedir, redo=redo, $
                      name=name,ct=ct,xr=xr,yr=yr,nseeds=nseeds,ldirect=ldirect,wfield=wfield,readvar=readvar,seedl=seedl

if n_elements(swt) eq 0 then swt=0
if n_elements(redo) eq 0 then redo=1
if n_elements(name) eq 0 then name='b'
if n_elements(readvar) eq 0 then readvar=' '
if n_elements(data) eq 0 then data=0
if n_elements(xax) eq 0 then xax='X'
if n_elements(yax) eq 0 then yax='Y'
if n_elements(itsnap) eq 0 then itsnap=0
if n_elements(wfield) eq 0 then wfield=0
if n_elements(ldirect) eq 0 then ldirect=0
if n_elements(nseeds) eq 0 then nseeds=49
if n_elements(ct) eq 0 then ct=255
 if n_elements(xr) eq 0 then begin
     x=*(*info).xscale
     xr=[0,n_elements(x)-1]
  endif
  if n_elements(yr) eq 0 then begin
     y=*(*info).yscale
     yr=[0,n_elements(y)-1]
  endif
if n_elements(rpos) eq 0 then rpos=0
if n_elements(seedl) eq 0 then seedl=0
if n_elements(linedir) eq 0 then linedir=0
struct={swt: swt, $
        redo: redo, $
        name:name, $
        readvar:readvar, $
        data:data, $
        xax:xax, $
        yax:yax, $
        ct:ct, $
        xr:xr, $
        yr:yr, $
        ldirect:ldirect, $
        nseeds:nseeds, $
        itsnap:itsnap, $
        wfield:wfield, $
        seedl:seedl, $
        linedir:linedir, $
        rpos: rpos}
end

; default values for lines
pro xmhd_flinedefault,event

  widget_control, event.top,get_uvalue=top
  widget_control, top,get_uvalue=info
  pref=widget_info(event.top, find_by_uname='regionref')
  widget_control,pref,get_uvalue=fc
  (*info).fc=ptr_new(fc)
  x=*(*info).xscale
  y=*(*info).yscale
  xmhd_field_define,struct, info=info,wfield=(*(*info).fc).wfield,readvar=(*(*info).fc).readvar
  widget_control, widget_info(event.top, find_by_uname='lname'),set_value=struct.name
  widget_control, widget_info(event.top, find_by_uname='nseeds'),set_value=struct.nseeds
  widget_control, widget_info(event.top, find_by_uname='lct'),set_value=struct.ct
  widget_control, widget_info(event.top, find_by_uname='linedirect'),set_value=struct.ldirect
  widget_control, widget_info(event.top, find_by_uname='xxpminslider'),set_value=min(x(struct.xr))
  widget_control, widget_info(event.top, find_by_uname='xpminslider'),set_value=struct.xr[0]
  widget_control, widget_info(event.top, find_by_uname='xxpmaxslider'),set_value=max(x(struct.xr))
  widget_control, widget_info(event.top, find_by_uname='xpmaxslider'),set_value=struct.xr[1]
  widget_control, widget_info(event.top, find_by_uname='yypminslider'),set_value=min(y(struct.yr))
  widget_control, widget_info(event.top, find_by_uname='ypminslider'),set_value=struct.yr[0]
  widget_control, widget_info(event.top, find_by_uname='yypmaxslider'),set_value=max(y(struct.yr))
  widget_control, widget_info(event.top, find_by_uname='ypmaxslider'),set_value=struct.yr[1]
  fc=struct
  widget_control,pref,set_uvalue=fc
end

;name of the variable for lines
pro lname,event

  widget_control, event.top,get_uvalue=top
  widget_control, top,get_uvalue=info
  fcref=widget_info(event.top, find_by_uname='regionref')
  widget_control,fcref,get_uvalue=fc, sensitive=1
  fc.name=event.value
  fc.redo=1
  widget_control,fcref,set_uvalue=fc, sensitive=1
end

; number of seed points for lines
pro lseeds, event
  widget_control, event.top,get_uvalue=top
  widget_control, top,get_uvalue=info
  fcref=widget_info(event.top, find_by_uname='regionref')
  widget_control, fcref,get_uvalue=fc
  fc.nseeds=event.value
  fc.redo=1
  widget_control,fcref,set_uvalue=fc
end

; Color value for lines
pro lct, event

  widget_control, event.top,get_uvalue=top
  widget_control, top,get_uvalue=info
  fcref=widget_info(event.top, find_by_uname='regionref')
  widget_control, fcref,get_uvalue=fc
  fc.ct=event.value
  widget_control,fcref,set_uvalue=fc
end

; Button selection for direction of the lines
function linedirect,event

  widget_control, event.top,get_uvalue=top
  btref=widget_info(event.top, find_by_uname='regionref')
  widget_control,btref,get_uvalue=fc, sensitive=1
  fc.ldirect= fc.ldirect+1 mod 2

  widget_control,btref,set_uvalue=fc , sensitive=1
  return,0
end

; save the lines setup
pro xmhd_flineok,event

  widget_control, event.top,get_uvalue=top
  widget_control, top,get_uvalue=info
  btref=widget_info(event.top, find_by_uname='regionref')
  widget_control,btref,get_uvalue=fc
  fc.readvar=(*(*info).fc).readvar
  (*info).fc=ptr_new(fc)
  widget_control, top,set_uvalue=info
  widget_control, event.top,/destroy
end

;draws the lines setup
pro xmhd_flinedraw,event

  widget_control, event.top,get_uvalue=top
  widget_control, top,get_uvalue=info
  btref=widget_info(event.top, find_by_uname='regionref')
  widget_control,btref,get_uvalue=fc
  fc.readvar=(*(*info).fc).readvar
  (*info).fc=ptr_new(fc)
  (*(*info).fc).swt = 1
  fc.swt=1
  widget_control, top,set_uvalue=info
  fc.wfield=(*(*info).fc).wfield
  widget_control, (*(*info).fc).wfield, set_value=1
  widget_control,btref,set_uvalue=fc
  pseudoevent={widget_button,id:(*info).action, $
               top:top, handler:0l, select:1}
  xmhd_draw, pseudoevent
end

; selects min/max X-axis in Indices for region overplot for contours, vectors and
; lines.
pro xmhd_xptabi, event

  uvref=widget_info(event.top, find_by_uname='regionref')
  widget_control,uvref,get_uvalue=uv
  widget_control,event.id,get_uvalue=uvalue,get_value=value
  case uvalue of
      'xpminslider' : xmhd_xpminslider, event
      'xpmaxslider' : xmhd_xpmaxslider, event
   endcase
end

; selects min X-axis in Indices for region overplot for contours, vectors and
; lines.
pro xmhd_xpminslider, event

  widget_control, event.top,get_uvalue=top
  widget_control, top,get_uvalue=info
  uvref=widget_info(event.top, find_by_uname='regionref')
  widget_control,uvref,get_uvalue=uv
  uv.xr[0]=event.value
  x=*(*info).xscale
  uv.redo = 1
  xxpminslider=widget_info(event.top, find_by_uname='xxpminslider')
  widget_control, xxpminslider,set_value=x(uv.xr[0])
  widget_control,uvref,set_uvalue=uv
end

; selects max X-axis in Indices for region overplot for contours, vectors and
; lines.
pro xmhd_xpmaxslider, event

  widget_control, event.top,get_uvalue=top
  widget_control, top,get_uvalue=info
  uvref=widget_info(event.top, find_by_uname='regionref')
  widget_control,uvref,get_uvalue=uv
  uv.xr[1]=event.value
  uv.redo = 1
  x=*(*info).xscale
  xxpmaxslider=widget_info(event.top, find_by_uname='xxpmaxslider')
  widget_control, xxpmaxslider,set_value=x(uv.xr[1])
  widget_control,uvref,set_uvalue=uv
end

; selects min/max X-axis in coords for region overplot for contours, vectors and
; lines.
pro xmhd_xptabc, event

  uvref=widget_info(event.top, find_by_uname='regionref')
  widget_control,uvref,get_uvalue=uv
  widget_control,event.id,get_uvalue=uvalue,get_value=value
  case uvalue of
      'xxpminslider' : xmhd_xxpminslider, event
      'xxpmaxslider' : xmhd_xxpmaxslider, event
   endcase
end

; selects min X-axis in coords for region overplot for contours, vectors and
; lines.
pro xmhd_xxpminslider, event

  widget_control, event.top,get_uvalue=top
  widget_control, top,get_uvalue=info
  uvref=widget_info(event.top, find_by_uname='regionref')
  widget_control,uvref,get_uvalue=uv
  x=*(*info).xscale
  minx=min(abs(x-event.value),min_subscript)
  uv.xr[0]=min_subscript
  uv.redo = 1
  xpminslider=widget_info(event.top, find_by_uname='xpminslider')
  widget_control, xpminslider,set_value=min_subscript
  widget_control,uvref,set_uvalue=uv
end

; selects max X-axis in coords for region overplot for contours, vectors and
; lines.
pro xmhd_xxpmaxslider, event

  widget_control, event.top,get_uvalue=top
  widget_control, top,get_uvalue=info
  uvref=widget_info(event.top, find_by_uname='regionref')
  widget_control,uvref,get_uvalue=uv, sensitive=1
  x=*(*info).xscale
  minx=min(abs(x-event.value),min_subscript)
  uv.xr[1]=min_subscript
  uv.redo = 1
  xpmaxslider=widget_info(event.top, find_by_uname='xpmaxslider')
  widget_control, xpmaxslider,set_value=min_subscript
  widget_control,uvref,set_uvalue=uv
end

; selects min/max Y-axis in Indices for region overplot for contours, vectors and
; lines.
pro xmhd_yptabi, event

  widget_control, event.top,get_uvalue=top
  widget_control, top,get_uvalue=info
  uvref=widget_info(event.top, find_by_uname='regionref')
  widget_control,uvref,get_uvalue=uv
  widget_control,event.id,get_uvalue=uvalue,get_value=value
  case uvalue of
      'ypminslider' : xmhd_ypminslider, event
      'ypmaxslider' : xmhd_ypmaxslider, event
   endcase
end

; selects min Y-axis in indices for region overplot for contours, vectors and
; lines.
pro xmhd_ypminslider, event

  widget_control, event.top,get_uvalue=top
  widget_control, top,get_uvalue=info
  uvref=widget_info(event.top, find_by_uname='regionref')
  widget_control,uvref,get_uvalue=uv
  uv.yr[0]=event.value
  y=*(*info).yscale
  uv.redo = 1
  yypminslider=widget_info(event.top, find_by_uname='yypminslider')
  widget_control, yypminslider,set_value=y(uv.yr[0])
  widget_control,uvref,set_uvalue=uv
end

; selects max Y-axis in indices for region overplot for contours, vectors and
; lines.
pro xmhd_ypmaxslider, event

  widget_control, event.top,get_uvalue=top
  widget_control, top,get_uvalue=info
  uvref=widget_info(event.top, find_by_uname='regionref')
  widget_control,uvref,get_uvalue=uv
  uv.yr[1]=event.value
  y=*(*info).yscale
  uv.redo = 1
  yypmaxslider=widget_info(event.top, find_by_uname='yypmaxslider')
  widget_control, yypmaxslider,set_value=y(uv.yr[1])
  widget_control,uvref,set_uvalue=uv
end

; selects min/max Y-axis in coord for region overplot for contours, vectors and
; lines.
pro xmhd_yptabc, event

  widget_control, event.top,get_uvalue=top
  widget_control, top,get_uvalue=info
  uvref=widget_info(event.top, find_by_uname='regionref')
  widget_control,uvref,get_uvalue=bt
  widget_control,event.id,get_uvalue=uvalue,get_value=value
  case uvalue of
      'yypminslider' : xmhd_yypminslider, event
      'yypmaxslider' : xmhd_yypmaxslider, event
   endcase
end

; selects min Y-axis in coord for region overplot for contours, vectors and
; lines.
pro xmhd_yypminslider, event

  widget_control, event.top,get_uvalue=top
  widget_control, top,get_uvalue=info
  uvref=widget_info(event.top, find_by_uname='regionref')
  widget_control,uvref,get_uvalue=uv, sensitive=1
  y=*(*info).yscale
  minx=min(abs(y-event.value),min_subscript)
  uv.yr[0]=min_subscript
  uv.redo = 1
  ypminslider=widget_info(event.top, find_by_uname='ypminslider')
  widget_control, ypminslider,set_value=min_subscript
  widget_control,uvref,set_uvalue=uv
end

; selects max Y-axis in coord for region overplot for contours, vectors and
; lines.
pro xmhd_yypmaxslider, event

  widget_control, event.top,get_uvalue=top
  widget_control, top,get_uvalue=info
  uvref=widget_info(event.top, find_by_uname='regionref')
  widget_control,uvref,get_uvalue=uv
  y=*(*info).yscale
  minx=min(abs(y-event.value),min_subscript)
  uv.yr[1]=min_subscript
  uv.redo = 1
  ypmaxslider=widget_info(event.top, find_by_uname='ypmaxslider')
  widget_control, ypmaxslider,set_value=min_subscript
  widget_control,uvref,set_uvalue=uv
end

; calculates the lines once the variable is selected or overying lines is chosen
pro xmhd_calcfield,info,xax,yax,zax

  itsnap = (*(*info).data->getisnaps())
  if n_elements(itsnap) gt 1 then begin
     itsnap=itsnap[(*info).rpos]
  endif
  if itsnap ne (*(*info).fc).itsnap or xax ne (*(*info).fc).xax or yax ne (*(*info).fc).yax or (*info).rpos ne (*(*info).fc).rpos or  $
     (*(*info).fc).redo eq 1 then begin
     (*(*info).fc).itsnap = itsnap
     (*(*info).fc).xax = xax
     (*(*info).fc).yax = yax
     (*(*info).fc).rpos = (*info).rpos
     mx=*(*info).data->getmx()
     my=*(*info).data->getmy()
     mz=*(*info).data->getmz()
     if mx gt 1 and my gt 1 and mz gt 1 then dim=3 else dim=2
     seeds=(*(*info).fc).nseeds
     seeds=round(sqrt(seeds))^2
     (*(*info).fc).nseeds= seeds
     r0=fltarr(dim,seeds)
     case dim of
        3: begin
           x=(*(*info).hdr->getx())
           y=(*(*info).hdr->gety())
           z=(*(*info).hdr->getz())
           case strupcase(strmid(xax,0,1)) of
              'X': r0(0,*) = (indgen(seeds)/sqrt(seeds))*(x((*(*info).fc).xr(1))-x((*(*info).fc).xr(0)))/sqrt(seeds) + x((*(*info).fc).xr(0))
              'Y': r0(1,*) = (indgen(seeds)/sqrt(seeds))*(y((*(*info).fc).xr(1))-y((*(*info).fc).xr(0)))/sqrt(seeds) + y((*(*info).fc).xr(0))
              'Z': r0(2,*) = (indgen(seeds)/sqrt(seeds))*(z((*(*info).fc).xr(1))-z((*(*info).fc).xr(0)))/sqrt(seeds) + z((*(*info).fc).xr(0)) - (*(*info).hdr->getz())[0]
           endcase
           case strupcase(strmid(yax,0,1)) of
              'X': for i=0,sqrt(seeds)-1 do r0(0,i*sqrt(seeds):(i+1)*sqrt(seeds)-1) =  $
                 findgen(sqrt(seeds))*(x((*(*info).fc).yr(1))-x((*(*info).fc).yr(0)))/sqrt(seeds) + x((*(*info).fc).yr(0))
              'Y': for i=0,sqrt(seeds)-1 do r0(1,i*sqrt(seeds):(i+1)*sqrt(seeds)-1) =  $
                 findgen(sqrt(seeds))*(y((*(*info).fc).yr(1))-y((*(*info).fc).yr(0)))/sqrt(seeds) + y((*(*info).fc).yr(0))
              'Z': for i=0,sqrt(seeds)-1 do r0(2,i*sqrt(seeds):(i+1)*sqrt(seeds)-1) = $
                 findgen(sqrt(seeds))*(z((*(*info).fc).yr(1))-z((*(*info).fc).yr(0)))/sqrt(seeds) + z((*(*info).fc).yr(0)) - (*(*info).hdr->getz())[0]
           endcase
           case strupcase(strmid(zax,0,1)) of
              'X': r0(0,*) = findgen(seeds)*0 + (*(*info).hdr->getx())[(*info).rpos]
              'Y': r0(1,*) = findgen(seeds)*0 + (*(*info).hdr->gety())[(*info).rpos]
              'Z': r0(2,*) = findgen(seeds)*0 + (*(*info).hdr->getz())[(*info).rpos] - (*(*info).hdr->getz())[0]
              'T': begin
                 xyaxistype = strmid(xax,0,1)+strmid(yax,0,1)
                 if strmatch(xyaxistype,'YZ',/fold_case) eq 1 then begin
                    r0(0,*) = findgen(seeds)*0 + (*(*info).hdr->getx())[*(*info).data->getislice()]
                 endif
                 if strmatch(xyaxistype,'XZ',/fold_case) eq 1 then begin
                    r0(1,*) = findgen(seeds)*0 + (*(*info).hdr->gety())[*(*info).data->getislice()]
                 endif
                 if strmatch(xyaxistype,'XY',/fold_case) eq 1 then begin
                    r0(2,*) = findgen(seeds)*0 + (*(*info).hdr->getz())[*(*info).data->getislice()] - (*(*info).hdr->getz())[0]
                 endif
              end
           endcase
        end
        2: begin
           x=(*(*info).hdr->getx())
           y=(*(*info).hdr->gety())
           z=(*(*info).hdr->getz())
           r0(0,*) = (indgen(seeds)/sqrt(seeds))*(x((*(*info).fc).xr(1))-x((*(*info).fc).xr(0)))/sqrt(seeds) + x((*(*info).fc).xr(0))
           if strupcase(strmid(yax,0,1)) eq 'Z' then begin
              for i=0,sqrt(seeds)-1 do r0(1,i*sqrt(seeds):(i+1)*sqrt(seeds)-1) = $
                 findgen(sqrt(seeds))*(z((*(*info).fc).yr(1))-z((*(*info).fc).yr(0)))/sqrt(seeds) + z((*(*info).fc).yr(0)) - (*(*info).hdr->getz())[0];+zmin
           endif else begin
              for i=0,sqrt(seeds)-1 do r0(1,i*sqrt(seeds):(i+1)*sqrt(seeds)-1) = $
                 findgen(sqrt(seeds))*(y((*(*info).fc).yr(1))-y((*(*info).fc).yr(0)))/sqrt(seeds) + y((*(*info).fc).yr(0))
           endelse
        end
     endcase
     br_bfieldline,*(*info).data->getparamsfile(),itsnap,r1,r2,nr1,nr2,r0=r0, $
                   swap=(*info).swap, vname=(*(*info).fc).name,linedir=linedir;,zorig=210
     (*(*info).fc).redo=0
     xmhd_field_define,field,swt=(*(*info).fc).swt,data=[[[r1]],[[r2]]],xax=xax,yax=yax, seedl=r0, $
                       itsnap=itsnap,rpos=(*info).rpos,info=info,linedir=linedir,ldirect=(*(*info).fc).ldirect,  $
                       wfield=(*(*info).fc).wfield,xr=(*(*info).fc).xr,yr=(*(*info).fc).yr, $
                       name=(*(*info).fc).name,ct=(*(*info).fc).ct,nseeds=(*(*info).fc).nseeds,redo=(*(*info).fc).redo
     (*info).fc=ptr_new(field)
  endif
end

; Window option for Lines setup
pro xmhd_flines,event


  widget_control, event.top,get_uvalue=info

  xmhd_field_define,fc,swt=(*(*info).fc).swt,data=(*(*info).fc).data,xax=(*(*info).fc).xax,seedl=(*(*info).fc).seedl, $
                    yax=(*(*info).fc).yax,itsnap=(*(*info).fc).itsnap,rpos=(*(*info).fc).rpos, $
                    info=info,name=(*(*info).fc).name,ct=(*(*info).fc).ct,xr=(*(*info).fc).xr,$
                    yr=(*(*info).fc).yr,nseeds=(*(*info).fc).nseeds,ldirect=(*(*info).fc).ldirect, $
                    wfield=(*(*info).fc).wfield,linedir=(*(*info).fc).linedir,redo=(*(*info).fc).redo

  x=*(*info).xscale
  y=*(*info).yscale
  xax=*(*info).aux->getxtitle()
  yax=*(*info).aux->getytitle()
;---------------------------------------------------------------------------------------------
  base = widget_base(/row, title='Lines setup',uvalue=event.top)
  wbase = widget_base(base,  column=2, frame=10)


; col 1
  col1=widget_base(wbase,  frame=1, row=5, /base_align_center)
  chosebase=widget_base(col1,column=1, /align_center,/sensitive, event_pro='lname')
  varname=cw_field(chosebase,title='Choose a variable',/frame,/string, uname='lname', $
                   value=(*(*info).fc).name,/return_events, xsize=3,/all_events)

  seedbase = widget_base(col1, column=1,event_pro='lseeds',/sensitive)
  nseed=cw_field(seedbase,title=' Seed number    ',/frame, uname='nseeds',/all_events, $
                   value=(*(*info).fc).nseeds, /integer,xsize=4)

  lastbut=widget_base(col1, column=1,/align_center)
  ctbut=widget_base(lastbut,/align_left,event_pro='lct',/sensitive)
  lct=cw_field(ctbut,title=' Colour index    ',xsize=4,value=(*(*info).fc).ct,/frame, $
                 uname='lct',/all_events)

  opbase=widget_base(col1, row=1)
  opnames=['Line direction']
  option_menu = cw_bgroup(opbase, opnames, /return_index, uname='linedirect',$
                            /nonexclusive,set_value =(*(*info).fc).ldirect , $
                            event_func = 'linedirect',row=1)


  opbut=widget_base(col1,/align_right,column=4)
  draw=widget_button(opbut,value='Draw',event_pro='xmhd_flinedraw')
  ok=widget_button(opbut,value='Ok',event_pro='xmhd_flineok')
  def=widget_button(opbut,value='Default',event_pro='xmhd_flinedefault')
  cancel=widget_button(opbut,value='Quit',event_pro='xmhd_destroy')


; col 2
  col2=widget_base(wbase,  frame=1, row=4)
  label=widget_label(col2, value='                 Region of the seeds', /align_center)


  ; x
  xtab=widget_tab(col2, event_pro='xmhd_xtab')

  ;tab 1 -> x coordinates
  xrsliderbase = widget_base(xtab, /row,/align_center, title='Coord', event_pro='xmhd_xptabc')
  xmin=min(x) & xmax=max(x)
  addtitle=strmid(xax,strpos(xax,'['),strlen(xax)-1)
  xxminslider = cw_fslider(xrsliderbase,/edit,format='(f8.3)',minimum=xmin,maximum=xmax,$
                       title='      X min '+addtitle, scroll=1,  uvalue='xxpminslider',value=min(x(fc.xr)), uname='xxpminslider')
  xxmaxslider = cw_fslider(xrsliderbase,/edit,format='(f8.3)',minimum=xmin,maximum=xmax,$
                       title='      X max '+addtitle, scroll=1,  uvalue='xxpmaxslider', value=max(x(fc.xr)), uname='xxpmaxslider')

  ;tab 2 -> x index
  xrsliderbase = widget_base(xtab, /row,/align_center, title='Index', event_pro='xmhd_xptabi')
  nr = n_elements(x)
  xrmin=0 & xrmax=nr-1
  xminslider = cw_fslider(xrsliderbase,/edit,format='(i5)',minimum=xrmin,maximum=xrmax,$
                       title='     X min', scroll=1, uvalue='xpminslider',value=fc.xr[0],uname='xpminslider')
  xmaxslider = cw_fslider(xrsliderbase,/edit,format='(i5)',minimum=xrmin,maximum=xrmax,$
                       title='     X max', scroll=1, uvalue='xpmaxslider', value=fc.xr[1],uname='xpmaxslider')
  ; y
  ytab=widget_tab(col2, event_pro='xmhd_ytab')

  ;tab 1 -> y coordinates
  yrsliderbase = widget_base(ytab, /row, /align_center, title='Coord', event_pro='xmhd_yptabc')
  ymin=min(y) & ymax=max(y)

  addtitle=strmid(yax,strpos(yax,'['),strlen(yax)-1)
  yyminslider = cw_fslider(yrsliderbase,/edit,format='(f8.3)',minimum=ymin,maximum=ymax,$
                       title='      Y min '+addtitle, scroll=1, uvalue='yypminslider',value=min(y(fc.yr)), uname='yypminslider')
  yymaxslider = cw_fslider(yrsliderbase,/edit,format='(f8.3)',minimum=ymin,maximum=ymax,$
                       title='      Y max '+addtitle, scroll=1, uvalue='yypmaxslider', value=max(y(fc.yr)), uname='yypmaxslider')
  ;tab 2 -> y index
  yrsliderbase = widget_base(ytab, /row, /align_center, title='Index', event_pro='xmhd_yptabi')
  nr = n_elements(y)
  yrmin=0 & yrmax=nr-1
  yminslider = cw_fslider(yrsliderbase,/edit,format='(i5)',minimum=yrmin,maximum=yrmax,$
                       title='      Y min', scroll=1, uvalue='ypminslider',value=fc.yr[0], uname= 'ypminslider')
  ymaxslider = cw_fslider(yrsliderbase,/edit,format='(i5)',minimum=yrmin,maximum=yrmax,$
                       title='      Y max ', scroll=1, uvalue='ypmaxslider', value=fc.yr[1], uname='ypmaxslider' )

  btref=widget_base(col2,uvalue=fc,uname='regionref')
  widget_control, base, /realize
  xmanager,'lines setup', base, /just_reg, /no_block, group_leader = group
end

; ----------- Vector setup--------------------
; define structure for contours
pro  xmhd_vector_define,uv,swt=swt, datax=datax, datay=datay, readvar=readvar, $
                        name=name, nseeds=nseeds, method=method, $
                        length=length, head=head, thick=thick, maxsq=maxsq, $
                        ct=ct, xr=xr,yr=yr, info=info, wvectfield=wvectfield,redo=redo

nx=(*info).ccd_xsz
ny=(*info).ccd_ysz
nt=n_elements((*(*info).data->getisnaps()))
nraster=(*info).nraster

if n_elements(swt)    eq 0  then swt=0
if n_elements(redo)    eq 0  then redo=0
if n_elements(datax)  eq 0  then datax=fltarr(nx,ny,nraster)
if n_elements(datay)  eq 0  then datay=fltarr(nx,ny,nraster)
if n_elements(name)   eq 0  then name='u'
if n_elements(readvar)eq 0  then readvar=''
if n_elements(nseeds) eq 0  then nseeds=10
if n_elements(method) eq 0  then method=1
if (*(*info).data)->getnofits() then begin
if n_elements(length) eq 0  then length=1.0
endif else begin
if n_elements(length) eq 0  then length=0.0001
endelse
if n_elements(head)   eq 0  then head=5.
if n_elements(thick)  eq 0  then thick=1.0
if n_elements(maxsq)  eq 0  then maxsq=1.0
if n_elements(ct)     eq 0  then ct=255
if n_elements(wvectfield) eq 0  then wvectfield=0
if n_elements(xr) eq 0 then xr=[0,nx-1]
if n_elements(yr) eq 0 then yr=[0,ny-1]

uv={swt: swt, $
    redo: redo, $
    name:name, $
    readvar:readvar, $
    nseeds:nseeds, $
    method:method,$
    length:length, $
    head:head, $
    thick:thick, $
    maxsq:maxsq, $
    ct:ct, $
    xr:xr, $
    yr:yr, $
    wvectfield:wvectfield, $
    datax:datax,$
    datay:datay}

;if (*info).vcalc eq 1 then xmhd_vectordraw, info=info, uv=uv
;(*info).vcalc=(*info).vcalc+1
end

; Default values for vectors
pro xmhd_vectdefault,event

  widget_control, event.top,get_uvalue=top
  widget_control, top,get_uvalue=info
  uvref=widget_info(event.top, find_by_uname='regionref')
  widget_control,uvref,get_uvalue=uv
  (*info).uv=ptr_new(uv)
  x=*(*info).xscale
  y=*(*info).yscale
  xmhd_vector_define,struct, info=info, datax=uv.datax,datay=uv.datay,wvectfield=(*(*info).uv).wvectfield,readvar=(*(*info).uv).readvar
  widget_control, widget_info(event.top, find_by_uname='nname'),set_value=struct.name
  widget_control, widget_info(event.top, find_by_uname='nseeds'),set_value=struct.nseeds
  widget_control, widget_info(event.top, find_by_uname='nmethod'),set_value=struct.method
  widget_control, widget_info(event.top, find_by_uname='nlength'),set_value=struct.length
  widget_control, widget_info(event.top, find_by_uname='nhead'),set_value=struct.head
  widget_control, widget_info(event.top, find_by_uname='nthick'),set_value=struct.thick
  widget_control, widget_info(event.top, find_by_uname='nmaxsq'),set_value=struct.maxsq
  widget_control, widget_info(event.top, find_by_uname='ncolor'),set_value=struct.ct
  widget_control, widget_info(event.top, find_by_uname='xxpminslider'),set_value=min(x(struct.xr))
  widget_control, widget_info(event.top, find_by_uname='xpminslider'),set_value=struct.xr[0]
  widget_control, widget_info(event.top, find_by_uname='xxpmaxslider'),set_value=max(x(struct.xr))
  widget_control, widget_info(event.top, find_by_uname='xpmaxslider'),set_value=struct.xr[1]
  widget_control, widget_info(event.top, find_by_uname='yypminslider'),set_value=min(y(struct.yr))
  widget_control, widget_info(event.top, find_by_uname='ypminslider'),set_value=struct.yr[0]
  widget_control, widget_info(event.top, find_by_uname='yypmaxslider'),set_value=max(y(struct.yr))
  widget_control, widget_info(event.top, find_by_uname='ypmaxslider'),set_value=struct.yr[1]
  uv=struct
  widget_control,uvref,set_uvalue=uv
end

; selects the variable name for vectors
pro vname,event

  widget_control, event.top,get_uvalue=top
  widget_control, top,get_uvalue=info
  uvref=widget_info(event.top, find_by_uname='regionref')
  widget_control,uvref,get_uvalue=uv, sensitive=1
  uv.name=event.value
  widget_control,uvref,set_uvalue=uv, sensitive=1
end


; selects number of vectors (in each axis)
pro vseeds, event

  widget_control, event.top,get_uvalue=top
  widget_control, top,get_uvalue=info
  uvref=widget_info(event.top, find_by_uname='regionref')
  widget_control, uvref,get_uvalue=uv
  uv.nseeds=event.value
  widget_control,uvref,set_uvalue=uv
end

; selects between random or uniform grid
function vmethod, event
  widget_control, event.top,get_uvalue=top
  widget_control, top,get_uvalue=info
  uvref=widget_info(event.top, find_by_uname='regionref')
  widget_control, uvref,get_uvalue=uv
  uv.method=event.value
  widget_control,uvref,set_uvalue=uv
  return,0
end

; selects the lenght of the arrows
pro vlength, event
  widget_control, event.top,get_uvalue=top
  widget_control, top,get_uvalue=info
  uvref=widget_info(event.top, find_by_uname='regionref')
  widget_control, uvref,get_uvalue=uv
  uv.length=event.value
  widget_control,uvref,set_uvalue=uv
end

; selects the sice of the head of the arrows
pro vhead, event

  widget_control, event.top,get_uvalue=top
  widget_control, top,get_uvalue=info
  uvref=widget_info(event.top, find_by_uname='regionref')
  widget_control, uvref,get_uvalue=uv
  uv.head=event.value
  widget_control,uvref,set_uvalue=uv

end

; selects the thickness of the vectors
pro vthick, event

  widget_control, event.top,get_uvalue=top
  widget_control, top,get_uvalue=info
  uvref=widget_info(event.top, find_by_uname='regionref')
  widget_control, uvref,get_uvalue=uv
  uv.thick=event.value
  widget_control,uvref,set_uvalue=uv
end

; selects Maxsq for vectors?
pro vmaxsq, event

  widget_control, event.top,get_uvalue=top
  widget_control, top,get_uvalue=info
  uvref=widget_info(event.top, find_by_uname='regionref')
  widget_control, uvref,get_uvalue=uv
  uv.maxsq=event.value
  widget_control,uvref,set_uvalue=uv
end

; selects the color for vectors
pro vct, event

  widget_control, event.top,get_uvalue=top
  widget_control, top,get_uvalue=info
  uvref=widget_info(event.top, find_by_uname='regionref')
  widget_control, uvref,get_uvalue=uv
  uv.ct=event.value
  widget_control,uvref,set_uvalue=uv
end

;save selected options
pro xmhd_vectok,event

  widget_control, event.top,get_uvalue=top
  widget_control, top,get_uvalue=info
  uvref=widget_info(event.top, find_by_uname='regionref')
  widget_control,uvref,get_uvalue=uv
  if (*(*info).uv).swt  eq 1 then xmhd_vectordraw, info=info, uv=uv else uv.readvar=(*(*info).uv).readvar
  (*info).uv=ptr_new(uv)
  widget_control, top,set_uvalue=info
  widget_control, event.top, /destroy
end

;draws the vectors with the selected options
pro xmhd_vectdraw,event

  widget_control, event.top,get_uvalue=top
  widget_control, top,get_uvalue=info
  uvref=widget_info(event.top,find_by_uname='regionref')
  widget_control,uvref,get_uvalue=uv
  uv.readvar=(*(*info).uv).readvar
  uv.swt = 1
  (*(*info).uv).swt = 1
  widget_control,uvref,set_uvalue=uv, sensitive=1
  widget_control, (*(*info).uv).wvectfield, set_value=1
  xmhd_vectordraw, info=info, uv=uv
  (*info).uv=ptr_new(uv)
  widget_control, top,set_uvalue=info
  pseudoevent={widget_button,id:(*info).action, $
               top:top, handler:0l, select:1}
  xmhd_draw, pseudoevent
end

;quits window setup for vectors
pro xmhd_vectquit,event

  widget_control, event.top,get_uvalue=top
  widget_control, top,get_uvalue=info
  widget_control, event.top, /destroy
end

; calculates the vector variables
pro xmhd_vectordraw, info=info, uv=uv
;---------------------------------------------------------------------------------------

  if (*(*info).uv).swt eq 1 then begin
    xax=*(*info).aux->getxtitle()
    yax=*(*info).aux->getytitle()
    sax=*(*info).aux->getrastertitle()

    itsnap = (*(*info).data->getisnaps())
    nt=n_elements(itsnap)

    swap=(*info).swap

    case (*info).mode of
       0: begin
          addx=strmid(xax,0,1)
          addy=strmid(yax,0,1)
       end
       1: begin
          addx=strmid(yax,0,1)
          addy=strmid(sax,0,1)
       end
       2: begin
          addx=strmid(xax,0,1)
          addy=strmid(sax,0,1)
       end
    endcase

    if (strupcase(addx) eq 'T') or (strupcase(addy) eq 'T') then begin
       print,'Vectors cannot plot when an axis is time'
       uv.swt=0
       (*info).uv=ptr_new(uv)
    endif else begin

       posux=strpos(uv.readvar,uv.name+addx)
       posuy=strpos(uv.readvar,uv.name+addy)

       dd = obj_new('br_data',(*info).snapname)

       if posux eq -1 then begin
          ux=reform(dd->getvar(uv.name+addx,itsnap,swap=swap,nofits=(*(*info).data)->getnofits(),rootfitsname=(*(*info).data)->getrootfitsname()))
       endif else begin
          if posux eq 0 then begin
             ux=(*(*info).uv).datax
          endif else begin
             ux=(*(*info).uv).datay
          endelse
       endelse

       if posuy eq -1 then begin
          uy=reform(dd->getvar(uv.name+addy,itsnap,swap=swap,nofits=(*(*info).data)->getnofits(),rootfitsname=(*(*info).data)->getrootfitsname()))
       endif else begin
          if posuy eq 0 then begin
             uy=(*(*info).uv).datax
          endif else begin
             uy=(*(*info).uv).datay
          endelse
       endelse

       uv.readvar = uv.name+addx+uv.name+addy
       obj_destroy,dd
       dim=size(ux)
       wh=where(dim gt 1)
;          if nt eq 1 then begin
;             ux=reform(ux,dim(wh(1)), dim(wh(2)))
;             uz=reform(uz,dim(wh(1)), dim(wh(2)))
;          endif
       dim=size(ux)

       if dim[0] eq 3 then begin
          if strupcase(strmid(xax,0,1)) ne 'T' and strupcase(strmid(yax,0,1)) ne 'T' and  $
             strupcase(strmid(sax,0,1)) ne 'T'  then begin
             nelx=(*(*info).data)->getmx()
             nely=(*(*info).data)->getmy()
             z=(*(*info).data)->getz()
             nelz=n_elements(z)
             maxz=max(z) & minz=min(z)
             newz=minz+(maxz-minz)/(nelz-1)*findgen(nelz)

             for iy=0,nely-1 do begin
                for ix=0, nelx-1 do begin
                   ux(ix,iy,*) = interpol( ux(ix,iy,*), z, newz  )
                   uy(ix,iy,*) = interpol( uy(ix,iy,*), z, newz  )
                endfor
             endfor
          endif else begin
             nelx=dim[1] & nt=dim[3]
             z=(*(*info).data)->getz()
             nelz=n_elements(z)
             maxz=max(z) & minz=min(z)
             newz=minz+(maxz-minz)/(nelz-1)*findgen(nelz)
             for it=0,nt-1 do begin
                for ix=0, nelx-1 do begin
                   ux(ix,*,it) = interpol( ux(ix,*,it), z, newz  )
                   uy(ix,*,it) = interpol( uy(ix,*,it), z, newz  )
                endfor
             endfor
          endelse
       endif else begin
          if strupcase(strmid(yax,0,1)) eq 'Z' then begin
             nelx=dim[1]
             z=(*(*info).data)->getz()
             nelz=n_elements(z)
             maxz=max(z) & minz=min(z)
             newz=minz+(maxz-minz)/(nelz-1)*findgen(nelz)
             for ix=0, nelx-1 do begin
                ux(ix,*) = interpol( ux(ix,*), z, newz  )
                uy(ix,*) = interpol( uy(ix,*), z, newz  )
             endfor
          endif
       endelse

       uv.datax=ux
       uv.datay=uy
       (*info).uv=ptr_new(uv)
    endelse
 endif
 end

; options window for vectors
pro xmhd_vect,event

  widget_control, event.top,get_uvalue=info

  xmhd_vector_define,uv,swt=(*(*info).uv).swt,datax=(*(*info).uv).datax, datay=(*(*info).uv).datay,$
                        name=(*(*info).uv).name, nseeds=(*(*info).uv).nseeds, method=(*(*info).uv).method,$
                        length=(*(*info).uv).length, wvectfield=(*(*info).uv).wvectfield, readvar=(*(*info).uv).readvar, $
                        thick=(*(*info).uv).thick, maxsq=(*(*info).uv).maxsq,head=(*(*info).uv).head, $
                        ct=(*(*info).uv).ct, xr=(*(*info).uv).xr,yr=(*(*info).uv).yr,info=info

  x=*(*info).xscale
  y=*(*info).yscale
  xax=*(*info).aux->getxtitle()
  yax=*(*info).aux->getytitle()

;---------------------------------------------------------------------------------------------
  base = widget_base(/row, title='Vector setup',uvalue=event.top)
  wbase = widget_base(base,  column=2, frame=10)


; col 1
  realcol1=widget_base(wbase,   row=3, /base_align_center)
  label=widget_label(realcol1, value='       ', /align_center)
  col1=widget_base(realcol1,  column=2, /base_align_center, frame=1)
  chosebase=widget_base(col1,column=1, /align_center,event_pro='vname',/sensitive)
  nname=cw_field(chosebase,title='Choose a var',/frame,/string, $
                   value=(*(*info).uv).name,/all_events,xsize=4, /column, uname='nname')

  seedbase = widget_base(col1, event_pro='vseeds',/sensitive, /align_center)
  nseeds=cw_field(seedbase,title='Seed number',/frame,$
                 value=(*(*info).uv).nseeds,/all_events,xsize=4, /column, /integer, uname='nseeds')


  opbase=widget_base(col1, row=1)
  opnames=['Random', 'Grid']
  nmethod = cw_bgroup(opbase, opnames, /return_index, $
                            /exclusive,set_value =(*(*info).uv).method , $
                            event_func = 'vmethod',row=1, uname='nmethod')


  subcol1= widget_base(col1, column=2, /align_center)
  headbase = widget_base(subcol1,event_pro='vhead',/sensitive, /align_center)
  nhead=cw_field(headbase,title=' Head ',/frame,/floating,$
                 value=(*(*info).uv).head,/all_events,xsize=4, /column, uname='nhead')

  lengthbase = widget_base(subcol1,event_pro='vlength',/sensitive, /align_center)
  nlength=cw_field(lengthbase,title='Length',/frame,/floating,$
                 value=(*(*info).uv).length,/all_events,xsize=7, /column, uname='nlength')

  thickbase = widget_base(subcol1,event_pro='vthick',/sensitive, /align_center)
  nthick=cw_field(thickbase,title='Thick ',/frame,/floating, $
                 value=(*(*info).uv).thick,/all_events,xsize=4, /column, uname='nthick')


  maxsqbase = widget_base(subcol1, event_pro='vmaxsq',/sensitive, /align_center)
  nmaxsq=cw_field(maxsqbase,title='Maxsq ',/frame,/floating,$
                 value=(*(*info).uv).maxsq,/all_events,xsize=4, /column, uname='nmaxsq')



  colorbase = widget_base(col1, event_pro='vct',/sensitive, /align_center)
  ncolor=cw_field(colorbase,title='Colour index',/frame,value=(*(*info).uv).ct,/all_events,xsize=4, $
                  uname='ncolor', /integer)


  opbut=widget_base(realcol1, row=1,align_center=10)
  empty=widget_label(opbut, value='       ')
  draw=widget_button(opbut,value='Draw',event_pro='xmhd_vectdraw')
  ok=widget_button(opbut,value='Ok',event_pro='xmhd_vectok')
  def=widget_button(opbut,value='Default',event_pro='xmhd_vectdefault')
  cancel=widget_button(opbut,value='Quit',event_pro='xmhd_vectquit')

; col 2
  col2=widget_base(wbase,  row=4, frame=1)
  label=widget_label(col2, value='                 Region of the seeds', /align_center)

  ; x
  xtab=widget_tab(col2, event_pro='xmhd_xtab')

  ;tab 1 -> x coordinates
  xrsliderbase = widget_base(xtab, /row,/align_center, title='Coord', event_pro='xmhd_xptabc')
  xmin=min(x) & xmax=max(x)
  addtitle=strmid(xax,strpos(xax,'['),strlen(xax)-1)
  xxminslider = cw_fslider(xrsliderbase,/edit,format='(f8.3)',minimum=xmin,maximum=xmax,$
                       title='      X min '+addtitle, scroll=1, uvalue='xxpminslider',value=min(x(uv.xr)), uname='xxpminslider')
  xxmaxslider = cw_fslider(xrsliderbase,/edit,format='(f8.3)',minimum=xmin,maximum=xmax,$
                       title='      X max '+addtitle, scroll=1, uvalue='xxpmaxslider', value=max(x(uv.xr)), uname='xxpmaxslider')
  ;tab 2 -> x index
  xrsliderbase = widget_base(xtab, /row,/align_center, title='Index', event_pro='xmhd_xptabi')
  nr = n_elements(x)
  xrmin=0 & xrmax=nr-1
  xminslider = cw_fslider(xrsliderbase,/edit,format='(i5)',minimum=xrmin,maximum=xrmax,$
                       title='     X min', scroll=1, uvalue='xpminslider',value=uv.xr[0],uname='xpminslider')
  xmaxslider = cw_fslider(xrsliderbase,/edit,format='(i5)',minimum=xrmin,maximum=xrmax,$
                       title='     X max', scroll=1, uvalue='xpmaxslider', value=uv.xr[1],uname='xpmaxslider')
  ; y
  ytab=widget_tab(col2, event_pro='xmhd_ytab')

  ;tab 1 -> y coordinates
  yrsliderbase = widget_base(ytab, /row, /align_center, title='Coord', event_pro='xmhd_yptabc')
  ymin=min(y) & ymax=max(y)


  addtitle=strmid(yax,strpos(yax,'['),strlen(yax)-1)
  yyminslider = cw_fslider(yrsliderbase,/edit,format='(f8.3)',minimum=ymin,maximum=ymax,$
                       title='      Y min '+addtitle, scroll=1, uvalue='yypminslider',value=min(y(uv.yr)), uname='yypminslider')
  yymaxslider = cw_fslider(yrsliderbase,/edit,format='(f8.3)',minimum=ymin,maximum=ymax,$
                       title='      Y max '+addtitle, scroll=1, uvalue='yypmaxslider', value=max(y(uv.yr)), uname='yypmaxslider')
  ;tab 2 -> y index
  yrsliderbase = widget_base(ytab, /row, /align_center, title='Index', event_pro='xmhd_yptabi')
  nr = n_elements(y)
  yrmin=0 & yrmax=nr-1
  yminslider = cw_fslider(yrsliderbase,/edit,format='(i5)',minimum=yrmin,maximum=yrmax,$
                       title='      Y min', scroll=1, uvalue='ypminslider',value=uv.yr[0], uname= 'ypminslider')
  ymaxslider = cw_fslider(yrsliderbase,/edit,format='(i5)',minimum=yrmin,maximum=yrmax,$
                       title='      Y max ', scroll=1, uvalue='ypmaxslider', value=uv.yr[1], uname='ypmaxslider' )

  uvref=widget_base(col2,uvalue=uv,uname='regionref')
  widget_control, base, /realize
  xmanager,'Vector setup', base, /just_reg, /no_block, group_leader = group

end

; ----------- Contour setup--------------------
; define structure for contours
pro xmhd_contour_define,struct,swt=swt,data=data,name=name, thickv=thickv, ir=ir,info=info, redo=redo, $
                         ct=ct,xr=xr,yr=yr,label=label,nlevels=nlevels,wcont=wcont,readvar=readvar

  x=*(*info).xscale
  y=*(*info).yscale
  nt=n_elements((*(*info).data->getisnaps()))
  nraster=(*(*info).data->getnraster())

  if n_elements(swt) eq 0 then swt=0
  if n_elements(redo) eq 0 then redo=0
  if n_elements(data) eq 0 then data=fltarr(n_elements(x),n_elements(y),nraster)
  if n_elements(name) eq 0 then name=''
  if n_elements(readvar) eq 0 then readvar=' '
  if n_elements(ct) eq 0 then ct=255
  if n_elements(label) eq 0 then label=0
  if n_elements(wcont) eq 0 then wcont=0
  if n_elements(nlevels) eq 0 then nlevels=5
  if n_elements(thickv) eq 0 then thickv=1.0
  if n_elements(xr) eq 0 then xr=[0,n_elements(x)-1]
  if n_elements(yr) eq 0 then yr=[0,n_elements(y)-1]
  if n_elements(ir) eq 0 then ir=[min(data)*1.0,max(data)*1.0]
  if ir[0] eq ir[1] then ir=[0.0,1.0]

struct={swt: swt, $
        redo: redo, $
        name:name, $
        readvar:readvar, $
        label:label, $
        ct:ct, $
        xr:xr, $
        yr:yr, $
        ir:ir, $
        thickv:thickv, $
        nlevels:nlevels, $
        wcont:wcont, $
        data:data}
end

; default values for contours
pro xmhd_contourdefault,event

  widget_control, event.top,get_uvalue=top
  widget_control, top,get_uvalue=info
  pref=widget_info(event.top, find_by_uname='regionref')
  widget_control,pref,get_uvalue=bt
  (*info).bt=ptr_new(bt)
  x=*(*info).xscale
  y=*(*info).yscale
  xmhd_contour_define,struct, info=info,wcont=(*(*info).bt).wcont,readvar=(*(*info).bt).readvar,data=(*(*info).bt).data,name=(*(*info).bt).name
  widget_control, widget_info(event.top, find_by_uname='cname'),set_value=struct.name
  widget_control, widget_info(event.top, find_by_uname='label'),set_value=struct.label
  widget_control, widget_info(event.top, find_by_uname='cct'),set_value=struct.ct
  widget_control, widget_info(event.top, find_by_uname='icminslider'),set_value=struct.ir[0]
  widget_control, widget_info(event.top, find_by_uname='icmaxslider'),set_value=struct.ir[1]
  widget_control, widget_info(event.top, find_by_uname='cthickv'),set_value=struct.thickv
  widget_control, widget_info(event.top, find_by_uname='cnlevels'),set_value=struct.nlevels
  widget_control, widget_info(event.top, find_by_uname='xxpminslider'),set_value=min(x(struct.xr))
  widget_control, widget_info(event.top, find_by_uname='xpminslider'),set_value=struct.xr[0]
  widget_control, widget_info(event.top, find_by_uname='xxpmaxslider'),set_value=max(x(struct.xr))
  widget_control, widget_info(event.top, find_by_uname='xpmaxslider'),set_value=struct.xr[1]
  widget_control, widget_info(event.top, find_by_uname='yypminslider'),set_value=min(y(struct.yr))
  widget_control, widget_info(event.top, find_by_uname='ypminslider'),set_value=struct.yr[0]
  widget_control, widget_info(event.top, find_by_uname='yypmaxslider'),set_value=max(y(struct.yr))
  widget_control, widget_info(event.top, find_by_uname='ypmaxslider'),set_value=struct.yr[1]
  bt=struct
  widget_control,pref,set_uvalue=bt
end

; chose the variable for contours
pro cname,event

  widget_control, event.top,get_uvalue=top
  widget_control, top,get_uvalue=info
  btref=widget_info(event.top, find_by_uname='regionref')
  widget_control, btref,get_uvalue=bt,sensitive=1
  if event.value ne bt.name then begin
     bt.name=event.value
     dd = obj_new('br_data',(*info).snapname)
     (*info).bt=ptr_new(bt)
     xmhd_calcbeta,info
     bt.data=(*(*info).bt).data
  endif
  xmin=min(bt.data)  &  xmax=max(bt.data)
;  (*(*info).bt).ir[0] = xmin
;  (*(*info).bt).ir[1] = xmax
  bt.ir[0] = xmin & bt.ir[1] = xmax
  icminslider=widget_info(event.top, find_by_uname='icminslider')
  widget_control, icminslider, set_value=xmin
  icmaxslider=widget_info(event.top, find_by_uname='icmaxslider')
  widget_control, icmaxslider, set_value=xmax
  widget_control,btref,set_uvalue=bt, sensitive=1
end

; calculates the variable contour once selected the variable or select
; the option of contours
pro xmhd_calcbeta,info
  if (*(*info).bt).name ne (*(*info).bt).readvar then begin
     itsnap = (*(*info).data->getisnaps())
     mode=*(*info).data->getmode()
     varname=(*(*info).bt).name

     case mode of
        'XYZ': begin
           dd = obj_new('br_data',(*info).snapname)
           beta=dd->getvar(varname,itsnap,swap=(*info).swap,nofits=(*(*info).data)->getnofits(),rootfitsname=(*(*info).data)->getrootfitsname())
           obj_destroy,dd
        end
        'XZ': begin
           dd = obj_new('br_data',(*info).snapname)
           iy=*(*info).aux->getspos()
           beta=dd->getvar(varname,itsnap,iy=iy,swap=(*info).swap,nofits=(*(*info).data)->getnofits(),rootfitsname=(*(*info).data)->getrootfitsname())
           obj_destroy,dd
        end
        'YZ': begin
           dd = obj_new('br_data',(*info).snapname)
           ix=*(*info).aux->getspos()
           beta=dd->getvar(varname,itsnap,ix=ix,swap=(*info).swap,nofits=(*(*info).data)->getnofits(),rootfitsname=(*(*info).data)->getrootfitsname())
           obj_destroy,dd
        end
        'XY': begin
           dd = obj_new('br_data',(*info).snapname)
           iz=*(*info).aux->getspos()
           beta=dd->getvar(varname,itsnap,iz=iz,swap=(*info).swap,nofits=(*(*info).data)->getnofits(),rootfitsname=(*(*info).data)->getrootfitsname())
           obj_destroy,dd
        end
        else:
     endcase
                                ; create a pointer to store beta
     xmhd_contour_define,bt,swt=(*(*info).bt).swt,data=beta,name=(*(*info).bt).name, thickv=(*(*info).bt).thickv, $
                         info=info,ct=(*(*info).bt).ct,xr=(*(*info).bt).xr,yr=(*(*info).bt).yr,$
                         label=(*(*info).bt).label,nlevels=(*(*info).bt).nlevels,ir=(*(*info).bt).ir, $
                         wcont=(*(*info).bt).wcont,readvar=(*(*info).bt).name
     (*info).bt=ptr_new(bt)
  endif

end

; chose the value of the thick contour
pro cthicv,event

  widget_control, event.top,get_uvalue=top
  btref=widget_info(event.top, find_by_uname='regionref')
  widget_control,btref,get_uvalue=bt, sensitive=1
  bt.thickv=event.value
  widget_control,btref,set_uvalue=bt, sensitive=1

end

;chose the number levels for the thin contours
pro cnlevels,event

  widget_control, event.top,get_uvalue=top
  btref=widget_info(event.top, find_by_uname='regionref')
  widget_control,btref,get_uvalue=bt, sensitive=1
  bt.nlevels=event.value
  widget_control,btref,set_uvalue=bt, sensitive=1
end

;chose color for contours
pro cct,event
  widget_control, event.top,get_uvalue=top
  btref=widget_info(event.top, find_by_uname='regionref')
  widget_control,btref,get_uvalue=bt, sensitive=1
  bt.ct=max([0,min([255,event.value])])
  widget_control,btref,set_uvalue=bt , sensitive=1

end

; selects the maximum and minimum values for the range where the thin
; contours are selected
pro xmhd_icontour, event

  widget_control, event.top,get_uvalue=top
  widget_control, top,get_uvalue=info
  btrefi=widget_info(event.top, find_by_uname='regionref')
  widget_control, btrefi,get_uvalue=bt
  widget_control,event.id,get_uvalue=uvalue,get_value=value,sensitive=1
  case uvalue of
      'icminslider' : begin
         bt.ir[0]=event.value
         widget_control,btrefi,set_uvalue=bt,sensitive=1
      end
      'icmaxslider' : begin
         bt.ir[1]=event.value
         widget_control,btrefi,set_uvalue=bt,sensitive=1
      end
   endcase
end

; adds lables to the contours
pro clabels,event

  widget_control, event.top,get_uvalue=top
  btref=widget_info(event.top, find_by_uname='regionref')
  widget_control,btref,get_uvalue=bt, sensitive=1
  bt.label= (bt.label+1) mod 2
  widget_control,btref,set_uvalue=bt , sensitive=1
end

;save the setup for contours
pro xmhd_contourok,event

  widget_control, event.top,get_uvalue=top
  widget_control, top,get_uvalue=info
  btref=widget_info(event.top, find_by_uname='regionref')
  widget_control,btref,get_uvalue=bt
  bt.readvar=(*(*info).bt).readvar
  (*info).bt=ptr_new(bt)
  widget_control, top,set_uvalue=info
  widget_control, event.top,/destroy
end

;draws contours with the selected options
pro xmhd_contourdraw,event

  widget_control, event.top,get_uvalue=top
  widget_control, top,get_uvalue=info
  btref=widget_info(event.top, find_by_uname='regionref')
  widget_control,btref,get_uvalue=bt
  bt.readvar=(*(*info).bt).readvar
  (*info).bt=ptr_new(bt)
  (*(*info).bt).swt=1
  if (*(*info).bt).name eq '' then begin
     (*(*info).bt).name='beta'
     bt.name='beta'
     xmhd_calcbeta,info
     cname=widget_info(event.top, find_by_uname='cname')
     widget_control, cname,set_value='beta'
     bt=*(*info).bt
  endif
  widget_control, top,set_uvalue=info

  widget_control,btref,set_uvalue=bt
  widget_control, (*(*info).bt).wcont, set_value=1
  pseudoevent={widget_button,id:(*info).action, $
                 top:top, handler:0l, select:1}
  xmhd_draw, pseudoevent
end


;setup options for contours
pro xmhd_contours,event

  widget_control, event.top,get_uvalue=info
  xmhd_contour_define,bt,swt=(*(*info).bt).swt,data=(*(*info).bt).data,name=(*(*info).bt).name, $
                      thickv=(*(*info).bt).thickv,info=info,ct=(*(*info).bt).ct, xr=(*(*info).bt).xr, $
                      yr=(*(*info).bt).yr,label=(*(*info).bt).label,nlevels=(*(*info).bt).nlevels, $
                      wcont=(*(*info).bt).wcont,readvar=(*(*info).bt).readvar

  x=*(*info).xscale
  y=*(*info).yscale
  xax=*(*info).aux->getxtitle()
  yax=*(*info).aux->getytitle()

;---------------------------------------------------------------------------------------------
  base = widget_base(/row, title='Contours setup',uvalue=event.top)
  wbase = widget_base(base,  column=2, frame=10)

; col 1
  col1=widget_base(wbase,  frame=1, row=7, /base_align_center)
  chosebase=widget_base(col1,column=1, /align_center,event_pro='cname',/sensitive)
  cname=cw_field(chosebase,title='Choose a variable',/frame,/string, $
                 value=(*(*info).bt).name,uname='cname',/return_event)

  thickbase = widget_base(col1, column=1,event_pro='cthicv',/sensitive)
  tvalue=cw_field(thickbase,title='Thick line value ',value=(*(*info).bt).thickv,/frame,$
                  uname='cthickv',/floating,/all_events)

  subcol1= widget_base(col1, column=2, /align_center)
  thinbase=widget_base(subcol1, column=1,event_pro='cnlevels',/sensitive)
  nlevels=cw_field(thinbase, title='Thin lines levels',value=(*(*info).bt).nlevels, $
                   /frame,xsize=3,/column,/all_events,uname='cnlevels')
  ctbut=widget_base(subcol1,/align_left,event_pro='cct')
  nseed=cw_field(ctbut,title='Colour index',xsize=3,value=bt.ct,/column, $
                 /frame,/all_events,uname='cct')

  opbase=widget_base(col1, column=1)
  label=widget_label(opbase, value='Intensity range',/align_center)

    ;tab 1 -> x coordinates
  xrsliderbase = widget_base(col1, /row,/align_center, title='Value', event_pro='xmhd_icontour',/frame,/sensitive)
  bt=*(*info).bt
  cname=bt.name
;  (*info).bt=ptr_new(bt)
  if cname eq '' then begin
     xmin=0 & xmax=1
     bt.ir=[xmin,xmax]
  endif else begin
     if min((*(*info).bt).data) eq max((*(*info).bt).data) and bt.ir[0] eq bt.ir[1] then begin
        xmhd_calcbeta,info
        beta=(*(*info).bt).data
        xmin=min(beta) & xmax=max(beta)
        bt.ir=[xmin,xmax]
     endif
  endelse

  addtitle=strmid(xax,strpos(xax,'['),strlen(xax)-1)
  xxminslider = cw_field(xrsliderbase, title='Min val=', value=bt.ir[0],/floating,/all_events, uname='icminslider', $
                         /frame,uvalue='icminslider',xsize=12)
  xxmaxslider = cw_field(xrsliderbase, title='Max val=', value=bt.ir[1],/floating,/all_events, uname='icmaxslider',$
                         /frame,uvalue='icmaxslider',xsize=12)
  btrefi=widget_base(col1,uvalue=bt,uname='regionref')
  opbut=widget_base(col1,/align_right,column=4)

  draw=widget_button(opbut,value='Draw',event_pro='xmhd_contourdraw')
  ok=widget_button(opbut,value='Ok',event_pro='xmhd_contourok')
  def=widget_button(opbut,value='Default',event_pro='xmhd_contourdefault')
  cancel=widget_button(opbut,value='Quit',event_pro='xmhd_destroy')

 ; falta por introducir en info.bt la salida de los resultados de los sliders
; col 2
  col2=widget_base(wbase,  frame=1, row=5)
  label=widget_label(col2, value='                 Region of the seeds', /align_center)
  ; x
  xtab=widget_tab(col2, event_pro='xmhd_xtab')

  ;tab 1 -> x coordinates
  xrsliderbase = widget_base(xtab, /row,/align_center, title='Coord', event_pro='xmhd_xptabc')
  xmin=min(x) & xmax=max(x)
  addtitle=strmid(xax,strpos(xax,'['),strlen(xax)-1)
  xxminslider = cw_fslider(xrsliderbase,/edit,format='(f8.3)',minimum=xmin,maximum=xmax,$
                       title='      X min '+addtitle, scroll=1, uvalue='xxpminslider',value=min(x(bt.xr)),uname='xxpminslider')
  xxmaxslider = cw_fslider(xrsliderbase,/edit,format='(f8.3)',minimum=xmin,maximum=xmax,$
                       title='      X max '+addtitle, scroll=1, uvalue='xxpmaxslider', value=max(x(bt.xr)), uname='xxpmaxslider')
  ;tab 2 -> x index
  xrsliderbase = widget_base(xtab, /row,/align_center, title='Index', event_pro='xmhd_xptabi')
  nr = n_elements(x)
  xrmin=0 & xrmax=nr-1
  xminslider = cw_fslider(xrsliderbase,/edit,format='(i5)',minimum=xrmin,maximum=xrmax,$
                       title='     X min', scroll=1, uvalue='xpminslider',value=bt.xr[0],uname='xpminslider')
  xmaxslider = cw_fslider(xrsliderbase,/edit,format='(i5)',minimum=xrmin,maximum=xrmax,$
                       title='     X max', scroll=1, uvalue='xpmaxslider', value=bt.xr[1],uname='xpmaxslider')
  ; y
  ytab=widget_tab(col2, event_pro='xmhd_ytab')

  ;tab 1 -> y coordinates
  yrsliderbase = widget_base(ytab, /row, /align_center, title='Coord', event_pro='xmhd_yptabc')
  ymin=min(y) & ymax=max(y)

  addtitle=strmid(yax,strpos(yax,'['),strlen(yax)-1)
  yyminslider = cw_fslider(yrsliderbase,/edit,format='(f8.3)',minimum=ymin,maximum=ymax,$
                       title='      Y min '+addtitle, scroll=1,value=min(y(bt.yr)), uvalue='yypminslider', uname='yypminslider')
  yymaxslider = cw_fslider(yrsliderbase,/edit,format='(f8.3)',minimum=ymin,maximum=ymax,$
                       title='      Y max '+addtitle, scroll=1, uvalue='yypmaxslider', value=max(y(bt.yr)), uname='yypmaxslider')

  ;tab 2 -> y index
  yrsliderbase = widget_base(ytab, /row, /align_center, title='Index', event_pro='xmhd_yptabi')
  nr = n_elements(y)
  yrmin=0 & yrmax=nr-1
  yminslider = cw_fslider(yrsliderbase,/edit,format='(i5)',minimum=yrmin,maximum=yrmax,$
                       title='      Y min', scroll=1, uvalue='ypminslider', value=bt.yr[0], uname='ypminslider')
  ymaxslider = cw_fslider(yrsliderbase,/edit,format='(i5)',minimum=yrmin,maximum=yrmax,$
                       title='      Y max ', scroll=1, uvalue='ypmaxslider', value=bt.yr[1], uname='ypmaxslider')

  btref=widget_base(col2,uvalue=bt,uname='regionref')

  opbase=widget_base(col2, column=1)
  label=widget_label(opbase, value='  Draw method', /align_center)
  opnames=['Labels']
  option_menu = cw_bgroup(opbase, opnames, /return_index, $
                            /nonexclusive, set_value = bt.label, $
                            event_func = 'clabels',row=1,uname='label')

  widget_control, base, /realize
  widget_control, event.top, set_uvalue = info
  xmanager,'Contours setup', base, /just_reg, /no_block, group_leader = group
end


;;; not used?
;select line
pro xmhd_lineselect, event
  widget_control, event.top, get_uvalue=info
  thisevent=tag_names(event,/structure_name)
  case thisevent of
    'widget_droplist': begin
      (*info).line = event.index
      end
      else:
  endcase
  ;display selected line
  pseudoevent={widget_button,id:(*info).action, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  xmhd_draw,pseudoevent
end

;;; not used?
; get the value of the selected line from the line list:
function xmhd_pickline_pick, event
  widget_control, event.top, get_uvalue = info
  (*info).line = event.value
  return, 0
end

;;; not used?
; close line selection widget
pro xmhd_pickline_destroy, event
  widget_control, event.top, get_uvalue = info
 if (*info).messenger eq (*info).animenu then begin
    xmhd_anim, event
  endif
  widget_control, event.top,/destroy
end

;;; not used?
; controls the animation event: if only one line,
; start animation, if several line, then pop up
; line selection window first.
pro xmhd_control_anim,event
  widget_control, event.top, get_uvalue = info
  if (*info).nwin lt 2 then begin
    xmhd_anim, event
  endif else begin
    (*info).messenger = (*info).animenu
    xmhd_pickline, event
  endelse
end


; protect colors not used?
pro xmhd_protect_colors,event

  widget_control, event.top, get_uvalue = info
  tvlct, (*info).r, (*info).g, (*info).b, (*info).bottom
end

; resize main window
pro xmhd_resize, event

  widget_control, event.top ,get_uvalue = info

  if (*info).noresize eq 0 then begin
     if  (*info).d_ysz ne event.y - (*info).menu_ysz > 1 then begin
        (*info).d_ysz = event.y - (*info).menu_ysz > 1
        if (*info).d_xsz ne (*info).d_ysz*(*info).aspect then begin
           (*info).d_xsz = (*info).d_ysz*(*info).aspect
           p_xsz=((*info).d_xsz+(*info).cb_xsz)/2
           widget_control, (*info).pcid, draw_xsize = p_xsz, $
                           draw_ysize = p_xsz*0.7, xsize = p_xsz, $
                           ysize = p_xsz*0.7
           widget_control, (*info).prid, draw_xsize = p_xsz, $
                           draw_ysize = p_xsz*0.7, xsize = p_xsz, $
                           ysize = p_xsz*0.7
        endif
        widget_control, (*info).drawid, draw_xsize = (*info).d_xsz, $
                        draw_ysize = (*info).d_ysz, xsize = (*info).d_xsz, $
                        ysize = (*info).d_ysz
        widget_control, (*info).colorbarid, draw_ysize = (*info).d_ysz
     endif
  endif

  pseudoevent={widget_button,id:0l, $
    top:event.top,handler:0l,select:1}
  xmhd_draw,pseudoevent
end

pro xmhd_manual, event

  tvlct,red,green,blue,/get
  loadct,0
  base = widget_base(title='Manual',uvalue=event.top)
  wbase = widget_base(base, frame=1,/COLUMN, X_SCROLL_SIZE=800, Y_SCROLL_SIZE=700)
;  winb = widget_base(wind1, frame=1,/COLUMN)
  widget_control,wbase,/realize
  mantexfile=getenv('BIFROST')+path_sep()+'IDL'+path_sep()+'ssw'+path_sep()+'doc/itn34_quicktoolfits.tex'
  openr,lur,mantexfile,/get_lun
  mantex=''
  mantextot=''
  count=-1
  sectnum=0
  subsect=0
  while (not eof(lur)) do begin
     readf,lur,mantex
     if count eq 0 then begin
       if strmid(mantex,0,9) eq '\section{' then begin
          sectnum=sectnum+1
          mantex='  '+strtrim(string(sectnum),2)+'  '+strmid(mantex,9,strpos(mantex,'}')-9)
       endif
;       text1 = widget_text(wbase,value=mantex, /editable,/scroll,/wrap,scr_xsize=800, scr_ysize=700)
       text1 = widget_text(wbase,value=mantex, /editable,/wrap,/scroll, scr_xsize=800, scr_ysize=500)
       widget_control, wbase, /realize
       count=count+1
    endif else begin
       if count le 10 then begin
       if strpos(mantex, '\section{Dependences}') eq -1 then begin
          if strpos(mantex, '\section{') ne -1 then begin
             sectnum=sectnum+1
             mantex='  '+strtrim(string(sectnum),2)+'  '+strmid(mantex,9,strpos(mantex,'}')-9)
          endif
          if strpos(mantex, '\subsection{') ne -1 then begin
             subsect=subsect+1
             mantex='  '+strtrim(string(sectnum),2)+'.'+strtrim(string(subsect),2)+'  '+strmid(mantex,12,strpos(mantex,'}')-12)
          endif
          if (strpos(mantex,'\begin{figure}') ne -1) or (strpos(mantex,'\begin{wrapfigure}') ne -1) then begin
             count=3
          endif
          if (strpos(mantex,'\begin{tabular}') ne -1) or (strpos(mantex,'\begin{eqnarray}') ne -1) or $
          (strpos(mantex,'\end{eqnarray}') ne -1) or (strpos(mantex,'\begin{itemize}') ne -1) or $
          ( strpos(mantex,'\end{itemize}') ne -1) or strpos(mantex,'\clearpage') ne -1 or $
           (strpos(mantex,'\end{tabular}') ne -1) then begin
             count=2
          endif
          if count eq 1 then begin
             remcom=['&&','\$','\\nonumber','\\_','\\&','\\item','\\\\']
             repcom=[''  ,'' ,''         ,'_' ,'&' ,'- ',''   ]
             for ic=0,n_elements(remcom)-1 do begin
                mantex=strsplit(mantex,remcom(ic),/extract,/regex)
                if n_elements(mantex) ge 2 then begin
                   mantex=strjoin(mantex,repcom(ic))
                endif
             endfor
             widget_control,text1,set_value=mantex,/append
             widget_control, wbase, /realize
          endif
          if count ge 3 then begin
             if strpos(mantex,'\includegraphics') ne -1 then begin
                figure=strmid(mantex,strpos(mantex,'{')+1,strpos(mantex,'.pdf')-strpos(mantex,'{'))+'jpg'
                read_jpeg,getenv('BIFROST')+path_sep()+'IDL'+path_sep()+'ssw'+path_sep()+'doc/'+figure,image,/true
                pict1 = widget_draw(wbase,retain=2,xsize=800,ysize=700)
                if (size(image))[0] eq 3 then begin
                   br_plot_image,reform(image[0,*,*]),thicklen=0,color=0
                endif else begin
                   br_plot_image,image,thicklen=0,color=0
                endelse
                widget_control, wbase, /realize
             endif
             if count eq 4 then begin
               if strpos(mantex,'\end{wrapfigure}') ne -1 then count=1
               if count ne 1 then begin
                  remcom=['&&','\$','\\nonumber','\\_','\\&','\\item','\\\\']
                  repcom=[''  ,'' ,''         ,'_' ,'&' ,'- ',''   ]
                  for ic=0,n_elements(remcom)-1 do begin
                     mantex=strsplit(mantex,remcom(ic),/extract,/regex)
                     if n_elements(mantex) ge 2 then begin
                        mantex=strjoin(mantex,repcom(ic))
                     endif
                  endfor
                  widget_control,text1,set_value=mantex,/append
                  widget_control, wbase, /realize
               endif
            endif
             if strpos(mantex,'\caption') ne -1 then begin
                remcom=['&&','\$','\\nonumber','\\_','\\&','\\item','\\\\']
                repcom=[''  ,'' ,''         ,'_' ,'&' ,'- ',''   ]
                for ic=0,n_elements(remcom)-1 do begin
                   mantex=strsplit(mantex,remcom(ic),/extract,/regex)
                   if n_elements(mantex) ge 2 then begin
                      mantex=strjoin(mantex,repcom(ic))
                   endif
                  endfor
                text1 = widget_text(wbase,value=mantex, /editable,/wrap,/scroll, scr_xsize=800, scr_ysize=500)
                widget_control, wbase, /realize
                count=4
             endif
          endif
          if (strpos(mantex,'\end{figure}') ne -1) or (strpos(mantex,'\end{tabular}') ne -1) or $
          (strpos(mantex,'\end{wrapfigure}') ne -1) or (strpos(mantex,'\begin{eqnarray}') ne -1 ) or $
          (strpos(mantex,'\begin{itemize}') ne -1) or (strpos(mantex,'\end{itemize}') ne -1) or $
          (strpos(mantex,'\clearpage') ne -1) or (strpos(mantex,'\begin{tabular}') ne -1) or $
          (strpos(mantex,'\end{eqnarray}') ne -1 ) then begin
             count=1
          endif
       endif else begin
          count=11
       endelse
    endif
    endelse
    if strmid(mantex,0,16) eq '\tableofcontents' then count=0
  endwhile
  widget_control, base, /realize
  xmanager,'About', base, /just_reg, /no_block, group_leader = group
  tvlct,red,green,blue
end

pro xmhd_about, event

  base = widget_base(title='About',uvalue=event.top)
  wbase = widget_base(base,  row=3, frame=1)
  text1 = widget_label(wbase,value='Name: br_xmhd.pro')
  text2 = widget_label(wbase,value='Authors: Juan Martinez-Sykora, Viggo H. Hansteen, Daniel Elias Nobrega Siverio, Helen Kim')
  text4 = widget_label(wbase,value='E-mail: jumasy1980@gmail.com')
  widget_control, base, /realize
  xmanager,'About', base, /just_reg, /no_block, group_leader = group
end

FUNCTION Clone_obj, Object=object

   IF( NOT Keyword_Set(object) ) THEN object = self
   obj = object
   filename = 'clone.sav'
   Save, obj, Filename=filename
   obj = 0 ; This is the trick to get the restore to 'clone' what it saved
   Restore, filename
   File_Delete, filename, /Quiet

   RETURN, obj
END

;--------------------------------------------------------------------------------------------------------------------------
;
;                                                           br_xmhd
;
;--------------------------------------------------------------------------------------------------------------------------

pro br_xmhd, data_red, group_leader=group,ncolors=ncolors,new=new,old=old,inputfield=inputfield, $
             swap=swap,diffobj=diffobj,fixnz=fixnz,noresize=noresize

  if Keyword_Set(diffobj) then begin
     data=clone_obj(object=data_red)
  endif else begin
     data=data_red
  endelse
;--------------------------------------------------------------------------------------------------------------------------
;                                                     initial parameters
;--------------------------------------------------------------------------------------------------------------------------
  if n_params() lt 1 then begin
     message,'br_xmhd, data, group_leader=group_leader, inputfield=inputfield, ncolors=ncolors,',/cont
     message,'              /new,/old,/swap,/diffobj,fixnz=fixnz,/noresize',/cont
     return
  endif

; optional parameters
  if n_elements(snapname)  eq 0 then begin
     if data->getnofits() eq 0 then begin
        snapname=data->getparamsfile()
     endif else begin
        snapname = data->getsnapname()
     endelse
  endif
  if n_elements(ncolors)   eq 0 then ncolors = (!d.n_colors < 256)
  if n_elements(drawcolor) eq 0 then drawcolor=!p.color
  if n_elements(noresize) eq 0 then noresize=0
; pick up local copies of hdr and aux objects
  if n_elements(old) eq 0 then old=0
  if old then new=0 else new=1
  if old then hdr=data->gethdr() else hdr=data
  aux=data->getaux()

; drawing window size in relation to screen
  if n_elements(scfac) eq 0 then scfac=0.5
  screensize=get_screen_size()
  sz=screensize*0.4

  nwin=data->getnwin()           ; number of variables (lines) plotted
                                 ; (usually one here)
  nraster = data->getnraster()   ; number of raster positions
  ntime = data-> getntime()      ; number of time steps
  ccd_sz=data->getccd_sz()       ; size of drawing area
  detector = fltarr(ccd_sz[0],ccd_sz[1])

; fill detector with viewed variable at time step (raster position) 0
  intmax=0.
  intmin=0.
  if (size(detector))[1] gt 1 then begin
      for i=0,nwin-1 do begin
          data->getwin, 0, wd, pos
          wd=reform(wd)
          intmax=max(wd)
          intmin=min(wd)
          detector(pos[0]:pos[0]+pos[1]-1,pos[2]:pos[2]+pos[3]-1)=wd(*, *, 0)
      endfor
  endif

; find original axes type 'x','y','z', or 't' set up scales
  xax=aux->getxtitle()
  yax=aux->getytitle()
  rax=aux->getxrastertitle()
  case strupcase(strmid(xax,0,1)) of
    'X': x = hdr->getx()
    'Y': x = hdr->gety()
    'Z': x = hdr->getz()
    'T': x = hdr->gett()
    else:
  endcase
  case strupcase(strmid(yax,0,1)) of
    'X': y = hdr->getx()
    'Y': y = hdr->gety()
    'Z': y = hdr->getz()
    'T': y = hdr->gett()
    else:
  endcase
  case strupcase(strmid(rax,0,1)) of
    'X': r = hdr->getx()
    'Y': r = hdr->gety()
    'Z': r = hdr->getz()
    'T': r = hdr->gett()
    else:
  endcase

  if n_elements(x) gt 1 then begin
; initialize size of draw window (ccd display):
     mag=min([sz[0]/ccd_sz[0],sz[1]/ccd_sz[1]])
     d_xsz = mag*ccd_sz[0]
     if old then d_ysz = d_xsz/data->getaspect() $
     else d_ysz=d_xsz/aux->getaspect()
     aspect = d_xsz/d_ysz
     window, /pixmap, /free, xsize = d_xsz, ysize = d_ysz
     pixid = !d.window
  endif else begin
     d_xsz = sz[0]
     d_ysz = sz[1]
     aspect=1
     xx=x
     yy=y
     tr=0
     window, /pixmap, /free, xsize = d_xsz, ysize = d_ysz
     pixid=!d.window
  endelse
;--------------------------------------------------------------------------------------------------------------------------
; initialization finished, now lay out widgets
; first the base widget for the menus:
  xwt = aux-> gettitle() + '   ' + hdr-> getidlpar()    ; xmhd window title
  tlb = widget_base(/row, title=xwt, tlb_size_events = 1, mbar=menubar)
  lcol = widget_base(tlb, /frame, /column)              ;left column.
  rcol = widget_base(tlb, /frame, /column)              ;right column.

; create pulldown menus on the base widget menubar
; left "file" menu

  filemenu=widget_button(menubar,value='File ',/menu, uvalue='file')
  savemenu=widget_button(filemenu, value='   Save as       ', event_pro='xmhd_writeimage', $
                         /align_left, accelerator="Ctrl+s")
  exitmenu=widget_button(filemenu, value='   Close         ', event_pro='xmhd_destroy', $
                         /align_left, accelerator="Ctrl+q")

  optmenu=widget_button(menubar,value='Options', uvalue='options')
  coltab=widget_button(optmenu, value='   Colour table', event_pro='xmhd_colors',$
                                 /align_left, accelerator="Ctrl+t")
  colsettab=widget_button(optmenu, value='   Layout setup', event_pro='xmhd_setcolors',$
                                 /align_left, accelerator="Ctrl+b")
  hdrplot = widget_button(optmenu, value='   Plot hdr params', event_pro='xmhd_hdrplot',$
                                 /align_left, accelerator="Ctrl+h")
  flines = widget_button(optmenu, value='   Line setup', event_pro='xmhd_flines',$
                                 /align_left, accelerator="Ctrl+l")
  vvects = widget_button(optmenu, value='   Vector setup', event_pro='xmhd_vect',$
                                 /align_left, accelerator="Ctrl+v")
  cconts = widget_button(optmenu, value='   Contour setup', event_pro='xmhd_contours',$
                                 /align_left, accelerator="Ctrl+c")

  helmenu=widget_button(menubar,value='Help', uvalue='help')
  manual = widget_button(helmenu, value='   Manual', event_pro='xmhd_manual',$
                                 /align_left, accelerator="Ctrl+m")
  about = widget_button(helmenu, value='   About', event_pro='xmhd_about',$
                                 /align_left, accelerator="Ctrl+a")
; now the display window base, the plotting canvas and the colorbar canvas:
  displaybase = widget_base(rcol, /row)
  drawid=widget_draw(displaybase, retain = 2,xsize = d_xsz, ysize = d_ysz, $
                     /button_events, event_pro='xmhd_zoom')


; color bar to the right of display window:
  cb_xsz = 104                         ; xsize of color bar draw widget
  white=!d.n_colors-1
  colorbarid = widget_draw(displaybase,retain=2,xsize=cb_xsz,ysize=d_ysz)

; two plot windows for showing row and column plots

  plotbase = widget_base(rcol, column=2,/frame)
  p_xsz=(d_xsz+cb_xsz)/2

  pcid = widget_draw(plotbase,retain=2,xsize=p_xsz,ysize=p_xsz*0.7)
  prid = widget_draw(plotbase,retain=2,xsize=p_xsz,ysize=p_xsz*0.7)

;-----------------------------------------------------------------------------------------------
; sliders for the two plots

  pcsliderbase = widget_base(rcol,column=2, /align_center)
  nr = n_elements(y)
  pcmin=0 & pcmax=nr-1

  tab=widget_tab(pcsliderbase, event_pro='xmhd_pctab', /align_left)
  box_base1 = widget_base(tab,column=1,title='Index', event_pro='xmhd_fclider')
  fclider = cw_fslider(box_base1,/edit,format='(i5)',minimum=pcmin,maximum=pcmax,$
                       title='     Plot row nr', scroll=1,uvalue='fclider')
  ymin=min(y) & ymax=max(y)
  box_base1 = widget_base(tab,column=1,title='Coord', event_pro='xmhd_ffclider')
  ffclider = cw_fslider(box_base1,/edit,format='(f8.3)',minimum=ymin,maximum=ymax,$
                        title='     Plot row nr', scroll=1,uvalue='ffclider')

  nr = n_elements(x)
  prmin=0 & prmax=nr-1

  tab=widget_tab(pcsliderbase, event_pro='xmhd_prtab')
  box_base1 = widget_base(tab, column=1, title='Index', event_pro='xmhd_frlider')
  frlider = cw_fslider(box_base1,/edit,format='(i5)',minimum=prmin,maximum=prmax,$
                      title='     Plot col nr', scroll=1)

  xmin=min(x) & xmax=max(x)
  box_base1 = widget_base(tab, column=2,/frame, title='Coord', event_pro='xmhd_ffrlider')
  ffrlider = cw_fslider(box_base1,/edit,format='(f8.3)',minimum=xmin,maximum=xmax,$
                       title='     Plot col nr', scroll=1)

; exclusive radiobuttons for controlling action in draw window
  dwoption = widget_base(lcol, /column,  /align_center)
  dwoption_title = widget_label(dwoption, value = 'WINDOW ACTION')
  dwoption_names = [' Zoom  ',' Aver. x', ' Aver. y', 'Value']
  dwoption_menu = cw_bgroup(dwoption, dwoption_names, /return_index, $
                            /exclusive, set_value = 0, $
                            event_func = 'xmhd_dwoption',row=1)

; drop list menu for controlling which axis is plotted against which
  modenames=(aux->getmenu()).mode
  if nraster gt 1 or ntime gt 1 then begin
    modebutton=widget_droplist(lcol,value=modenames[0:(aux->getmenu()).nmode-1], $
                         title='Display mode',event_pro='xmhd_mode',/align_center)
  endif else begin
    modebutton=widget_droplist(lcol,value=modenames[0], $
                         title='Display mode',event_pro='xmhd_mode',/align_center)
  endelse

; slider that controls which raster nr is displayed
  sliderbase = widget_base(lcol, /column,/align_center)
  rslider = -1
  if nraster gt 1 or ntime gt 1 then begin
    nr = nraster > ntime
    title = 'Image nr'
    rslider = widget_slider(sliderbase, xsize=120, minimum=0, maximum=nr-1, $
                            value=0, $
                            event_pro='xmhd_rslider',scroll=1)
  endif
  info_base=widget_base(lcol,/row,/align_center)
  info_txt=' '+aux->getxrastertitle()+'  '+strtrim(string(0.0,format='(f7.3)'),2)
  info_rslider = widget_label(info_base, value=info_txt)
  case aux->getmode() of
  'xz': spos=(hdr->gety())[aux->getspos()]
  'yz': spos=(hdr->getx())[aux->getspos()]
  'xy': spos=(hdr->getz())[aux->getspos()]
  'xyz': spos=(hdr->gett());[aux->getspos()]
  endcase
  info_txt=' '+aux->getstitle()+' '+strtrim(string(spos,format='(f7.3)'),2)
  info_sslider = widget_label(info_base, value=info_txt, ysize=20)

;-----------------------------------------------------------------------------------------------
; xrange sliders

  xtab=widget_tab(lcol, event_pro='xmhd_xtab')

  ;tab 1 -> x index
  xrsliderbase = widget_base(xtab, /row,/align_center, title='Index', event_pro='xmhd_xtabi')
  nr = n_elements(x)
  xrmin=0 & xrmax=nr-1
  xminslider = cw_fslider(xrsliderbase,/edit,format='(i5)',minimum=xrmin,maximum=xrmax,$
                       title='      X range min', scroll=1, uvalue='xminslider')
  xmaxslider = cw_fslider(xrsliderbase,/edit,format='(i5)',minimum=xrmin,maximum=xrmax,$
                       title='      X range max', scroll=1, uvalue='xmaxslider')

  ;tab 2 -> x coordinates
  xrsliderbase = widget_base(xtab, /row,/align_center, title='Coord', event_pro='xmhd_xtabc')
  xmin=min(x) & xmax=max(x)

  addtitle=strmid(xax,strpos(xax,'['),strlen(xax)-1)

  xxminslider = cw_fslider(xrsliderbase,/edit,format='(f8.3)',minimum=xmin,maximum=xmax,$
                       title='   X range min '+addtitle, scroll=1, uvalue='xxminslider')
  xxmaxslider = cw_fslider(xrsliderbase,/edit,format='(f8.3)',minimum=xmin,maximum=xmax,$
                       title='   X range max '+addtitle, scroll=1, uvalue='xxmaxslider', value=xmax)

;-----------------------------------------------------------------------------------------------
; yrange sliders

  ytab=widget_tab(lcol, event_pro='xmhd_ytab')

  ;tab 1 -> y index
  yrsliderbase = widget_base(ytab, /row, /align_center, title='Index', event_pro='xmhd_ytabi')
  nr = n_elements(y)
  yrmin=0 & yrmax=nr-1
  yminslider = cw_fslider(yrsliderbase,/edit,format='(i5)',minimum=yrmin,maximum=yrmax,$
                       title='       Y range min', scroll=1, uvalue='yminslider')
  ymaxslider = cw_fslider(yrsliderbase,/edit,format='(i5)',minimum=yrmin,maximum=yrmax,$
                       title='       Y range max', scroll=1, uvalue='ymaxslider')

  ;tab 2 -> y coordinatres
  yrsliderbase = widget_base(ytab, /row, /align_center, title='Coord', event_pro='xmhd_ytabc')
  ymin=min(y) & ymax=max(y)

  addtitle=strmid(yax,strpos(yax,'['),strlen(yax)-1)
  yyminslider = cw_fslider(yrsliderbase,/edit,format='(f8.3)',minimum=ymin,maximum=ymax,$
                       title='   Y range min '+addtitle, scroll=1, uvalue='yyminslider')
  yymaxslider = cw_fslider(yrsliderbase,/edit,format='(f8.3)',minimum=ymin,maximum=ymax,$
                       title='   Y range max '+addtitle, scroll=1, uvalue='yymaxslider', value=ymax)

;-----------------------------------------------------------------------------------------------
; irange sliders

  itab=widget_tab(lcol, event_pro='xmhd_itab')

  ;tab 1 -> i index
  irsliderbase = widget_base(itab, /row,/align_center, title='Index', event_pro='xmhd_itabi')
  nr = 256
  irmin=0 & irmax=nr-1
  iminslider = cw_fslider(irsliderbase,/edit,format='(i5)',minimum=irmin,maximum=irmax,$
                       title='      I range min', scroll=1, uvalue='iminslider')
  imaxslider = cw_fslider(irsliderbase,/edit,format='(i5)',minimum=irmin,maximum=irmax,$
                       title='      I range max', scroll=1, uvalue='imaxslider', value=irmax)

  ;tab 2 -> i values
  irsliderbase = widget_base(itab, /row, /align_center, title='Value', event_pro='xmhd_itabc')
  imin=min(reform(data->getvar(line))) & imax=max(reform(data->getvar(line)))
  iiminslider = cw_fslider(irsliderbase,/edit,format='(e11.3)',minimum=imin,maximum=imax,$
                       title='      I range min', scroll=1, uvalue='iiminslider')
  iimaxslider = cw_fslider(irsliderbase,/edit,format='(e11.3)',minimum=imin,maximum=imax,$
                       title='      I range max', scroll=1, uvalue='iimaxslider', value=imax)

;-----------------------------------------------------------------------------------------------
; more options

  iroption = widget_base(lcol, row=2,  /align_center, frame=3)
  lgoption_names = ['Log']
  lgoption_menu = cw_bgroup(iroption, lgoption_names, /return_index, $
                            /nonexclusive, set_value = 0, $
                            event_func = 'xmhd_lgoption')
  absoption_names = ['Abs']
  absoption_menu = cw_bgroup(iroption, absoption_names, /return_index, $
                            /nonexclusive, set_value = 0, $
                            event_func = 'xmhd_absoption')
  iroption_names = ['Auto scl']
  iroption_menu = cw_bgroup(iroption, iroption_names, /return_index, $
                            /nonexclusive, set_value = 0, $
                            event_func = 'xmhd_iroption')
  iroption_names = ['10^']
  iroption_menu = cw_bgroup(iroption, iroption_names, /return_index, $
                            /nonexclusive, set_value = 0, $
                            event_func = 'xmhd_expoption')
  iroption_names = ['nosq']
  iroption_menu = cw_bgroup(iroption, iroption_names, /return_index, $
                            /nonexclusive, set_value = 0, $
                            event_func = 'xmhd_nosqoption')

  magfield_names = ['Lines ']
  magfield_menu = cw_bgroup(iroption, magfield_names, /return_index, $
                            /nonexclusive, set_value = 0, $
                            event_func = 'xmhd_magfield',uname='wfield')
  vectfield_names = ['Vect. ']
  vectfield_menu = cw_bgroup(iroption, vectfield_names, /return_index, $
                            /nonexclusive, set_value = 0, $
                            event_func = 'xmhd_vectfield',uname='wvectfield')
  betacontour_names = ['Cont. ']
  betacontour_menu = cw_bgroup(iroption, betacontour_names, /return_index, $
                               /nonexclusive, set_value = 0, $
                               event_func = 'xmhd_betacontour',uname='wcont')
  flip_names = ['Flip z']
  flip_menu = cw_bgroup(iroption, flip_names, /return_index, $
                        /nonexclusive, set_value = 0, $
                        event_func = 'xmhd_flip')

;------------------------------------------------------------------------------------------------
; exclusive buttons to start "column" or "row" plots

  pxoption = widget_base(lcol, /column,frame=3)
  pxoption_title = widget_label(pxoption, value = 'Line plots')
  pxoption_names = ['1st dim plot','2nd dim plot', '3rd dim plot']
  pxoption_menu = cw_bgroup(pxoption, pxoption_names, /return_index, $
                            /nonexclusive, set_value = 0, $
                            event_func = 'xmhd_pixplot',row=1)



;------------------------------------------------------------------------------------------------
; finally a button to close down the xmhd session

  closefield = widget_base(lcol, /row, /align_center)
  closebutton = widget_button(closefield, value = 'Close', $
                              event_pro = 'xmhd_destroy')

  animbutton = widget_button(closefield, value = 'Animation', event_pro = 'xmhd_animsel');tvrd')

  idlbutton = widget_button(closefield, value = 'Params', event_pro = 'xmhd_hdrdisp')
  if n_elements(swap) eq 0 then swap=0

;------------------------------------------------------------------------------------------------
; field showing the value of the selected cursor
  cursorwindow = widget_base(lcol, /column,frame=3)
  cursortitle = widget_label(cursorwindow, value = 'CURSOR INFO')
  valcurs=widget_label(cursorwindow, /dynamic_resize, $
                 value='      Value =      ',uname='valcurs')
  cursorlab = widget_base(cursorwindow, /row,frame=2)
  xcurs=widget_label(cursorlab, $
                 value='      x [] = ;      ',uname='xcursor')
  ycurs=widget_label(cursorlab, $
                 value='      y [] =        ',uname='ycursor')

;------------------------------------------------------------------------------------------------

; with widgets laid out, realize the main window:
  widget_control, tlb, /realize, tlb_get_size = tlb_sz
; define size of widget and the menu column
  tlb_xsz = tlb_sz[0]  ; xsize of whole widget in pixels
  tlb_ysz = tlb_sz[1]  ; ysize of whole widget in pixels
  menu_ysz = tlb_ysz - d_ysz
  menu_xsz = tlb_xsz - d_xsz
; get window id of display window
  widget_control, drawid, get_value = wid
  wset, wid
; get and save color table
  tvlct, r, g, b, /get
  bottom = 0
  if (!d.n_colors le 256) then begin
    r = r[bottom:ncolors-1+bottom]
    g = g[bottom:ncolors-1+bottom]
    b = b[bottom:ncolors-1+bottom]
  endif
; set up default display mode ('time' or 'space'):
  dispmode = aux-> getdispmode()
; set up the grid resolution of the display

if n_elements(fixnz) eq 0 then fixnz=3000

; define the info structure, used send information around
  info = {detector:detector, $
          drawimage:ptr_new(), $
          anim:ptr_new(), $
          xscale:ptr_new(x), $
          yscale:ptr_new(y), $
          data:ptr_new(data), $
          hdr:ptr_new(hdr), $
          aux:ptr_new(aux), $
          snapname:snapname, $
          fc:ptr_new(obj_new()), $ ; for field lines
          bt:ptr_new(obj_new()), $ ; for contours
          hdrvar:ptr_new(obj_new()), $ ; for contours
          uv:ptr_new(obj_new()), $ ; for vectors
          ccd_xsz:ccd_sz[0], $
          ccd_ysz:ccd_sz[1], $
          nwin:nwin, $
          nraster:nraster, $
          ntime:ntime,  $
          swap:swap, $
          noresize:noresize, $
          rpos:0, $
          line:0, $
          fixnz:fixnz, $
          screensize:screensize, $
          d_xsz:d_xsz, $
          d_ysz:d_ysz, $
          tlb_xsz:tlb_xsz       ,$
          tlb_ysz:tlb_ysz       ,$
          menu_ysz:menu_ysz     ,$
          menu_xsz:menu_xsz, $
          keep_aspect:1, $
          aspect:aspect, $
          x_scroll_size:d_xsz, $
          y_scroll_size:d_ysz, $
          cb_xsz:cb_xsz, $
          tlb:tlb, $
          lcol:lcol, $
          rcol:rcol,  $
          r:r, g:g, b:b, $
          bottom:bottom, $
          ncolors:ncolors, $
          sx:0, $
          sy:0, $
          mode:dispmode, $
          messenger:0, $
          action:modebutton, $
          dwoption:0, $
          dwoption_menu:dwoption_menu, $
          cloption:0, $
          nosqoption:0, $
          pxoption:0, $
          pxoption_menu:pxoption_menu, $
          drawid:drawid, $
          colorbarid:colorbarid, $
          topcolor:255, $
          bottomcolor:0, $
          pcid:pcid, $
          prid:prid, $
          rslider:rslider, $
          xminslider:xminslider, $
          xmaxslider:xmaxslider, $
          yminslider:yminslider, $
          ymaxslider:ymaxslider, $
          iminslider:iminslider, $
          imaxslider:imaxslider, $
          xxminslider:xxminslider, $
          xxmaxslider:xxmaxslider, $
          yyminslider:yyminslider, $
          yymaxslider:yymaxslider, $
          iiminslider:iiminslider, $
          iimaxslider:iimaxslider, $
          info_rslider:info_rslider,$
          pciy:pcmin, $
          prix:prmin, $
          rtitle:aux->getxrastertitle(),$
          wdlist:0, $
          drawcolor:drawcolor, $
          mainpixid:pixid, $
          pixid:pixid, $
          title:' ', $
          xtitle:' ', $
          ytitle:' ', $
          xrmin:xrmin,$
          xrmax:xrmax,$
          yrmin:yrmin,$
          yrmax:yrmax,$
          irmin:irmin,$
          irmax:irmax,$
          ivmin:imin,$
          ivmax:imax,$
          iroption:0,$
          lgoption:0,$
          expoption:0,$
          absoption:0,$
          flip:0,$
          intmin:intmin,$
          intmax:intmax,$
          intminold:intmin,$
          intmaxold:intmax,$
          xx:ptr_new(xx),$
          yy:ptr_new(yy),$
          tr:ptr_new(tr),$
          uneven:0,$
          wid:wid,$
          fclider:fclider,$
          ffclider:ffclider,$
          frlider:frlider,$
          ffrlider:ffrlider,$
          ircoord:0,$
          ircoordrange:[imin,imax],$
          vcalc:0}
  info = ptr_new(info, /no_copy)
  xmhd_contour_define,beta,info=info,wcont=betacontour_menu
  if(n_elements(beta) ne 0)then (*info).bt=ptr_new(beta)

  xmhd_hdr_define,hdrvar,info=info
  if(n_elements(hdrvar) ne 0)then (*info).hdrvar=ptr_new(hdrvar)

  xmhd_vector_define,vel,info=info,wvectfield=vectfield_menu
  if(n_elements(vel) ne 0)then (*info).uv=ptr_new(vel)

  xmhd_field_define,field,info=info,wfield=magfield_menu
  if(n_elements(field) ne 0)then (*info).fc=ptr_new(field)
  if n_elements(inputfield) ne 0 then begin
     xmhd_field_define,field,swt=3,data=inputfield,xax=xax,yax=yax,itsnap=isnap,rpos=rpos,info=info,wfield=vectfield_menu
     (*info).fc=ptr_new(field)
  endif
; set user value of tlb widget to be the info ptr
  widget_control, tlb, /realize
  widget_control, tlb, set_uvalue = info
; create pseudoevent and send this event to xmhd_draw,
; in order to draw the image
  pseudoevent={widget_button,id:(*info).action, top:tlb, handler:0l, select:1}
  widget_control, (*info).action, send_event = pseudoevent
;  pseudoevent={widget_button,id:0l, top:tlb, handler:0l, select:1}
;  xdraw,pseudoevent
; start the xmanager

  xmanager, 'ql', tlb, /no_block,  event_handler = 'xmhd_resize', $
            group_leader = group, cleanup = 'xmhd_cleanup'

end
