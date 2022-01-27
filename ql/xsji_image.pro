;+
; NAME:
;       XSJI_IMAGE
;
; PURPOSE:
;       XSJI_IMAGE plots an image of an (IRIS or other) slit jaw image
;       and optionally overplots the positions of the slit during a 
;       raster.
;
; CATEGORY:
;       IRIS Data analysis SW
;
; CALLING SEQUENCE:
;       
; INPUTS: 
;       d - object containing image and methods for finding the 
;           location of the slit during a raster.
;
; KEYWORD PARAMETERS: 
;       slit - turn on display of slit positions
;
; OUTPUTS:
;       
; CALLS:
;       
; COMMON BLOCKS:
;       
; PROCEDURE:
;       
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;       16-Apr-2013: Viggo Hansteen
; 
; $Id: xsji_image.pro,v 1.28 2014/09/22 16:46:55 viggoh Exp $
;-
; save plot as postscript file
pro xsji_image_ps,event
  thisfile=dialog_pickfile(/write,file='xsji_image.ps')
  if thisfile eq '' then return
  widget_control,event.top,get_uvalue=info
  thisdevice=!d.name
  set_plot,'ps',/copy
  device,file=thisfile,_extra=keywords,/inches,bits_per_pixel=8,/color
  pseudoevent={widget_button,id:(*info).action, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  xsji_image_draw,pseudoevent
  device,/close_file
  set_plot,thisdevice
end

; save as jpeg file
pro xsji_image_jpeg,event
  thisfile=dialog_pickfile(/write,file='pixelplot.jpg')
  if thisfile eq '' then return
  widget_control,event.top,get_uvalue=info
  wset,(*info).wid
  snapshot=tvrd()
  tvlct,r,g,b,/get
  s=size(snapshot)
  image24=bytarr(3,s[1],s[2])
  image24(0,*,*)=r(snapshot)
  image24(1,*,*)=g(snapshot)
  image24(2,*,*)=b(snapshot)
  write_jpeg,thisfile,image24,true=1,quality=75
end

; change exposure number
pro xsji_image_expslider,event
  widget_control, event.top, get_uvalue = info
  (*info).expnr=event.value-1
  pseudoevent={widget_button,id:0L, $
    top:(*info).tlb, handler:0l, select:1}
  xsji_image_draw, pseudoevent
end

; turn on/off raster visualization
pro xsji_image_show_raster,event
  widget_control, event.top, get_uvalue = info
  (*info).show_raster=event.select
  pseudoevent={widget_button,id:0L, $
    top:(*info).tlb, handler:0l, select:1}
  xsji_image_draw, pseudoevent
end

; turn on/off log scaling
pro xsji_image_log,event
  widget_control, event.top, get_uvalue = info
  (*info).log=event.select
  pseudoevent={widget_button,id:0L, $
    top:(*info).tlb, handler:0l, select:1}
  xsji_image_draw, pseudoevent
end

; turn on/off log scaling
pro xsji_image_ximovie,event
  widget_control, event.top, get_uvalue = info
  widget_control,/hourglass
  sji=iris_sji(*(*info).data->getfilename_sji((*info).lwin))
  sji->ximovie,(*info).lwin,group_leader=(*info).tlb
end

; draw line profile
pro xsji_image_draw, event
  widget_control, event.top, get_uvalue = info
  wset,(*info).wid
  lwin=(*info).lwin
;
  widget_control,(*info).timetext,set_value=((*info).time)[(*info).expnr]
  xtitle='Solar X [arcsec]'
  ytitle='Solar Y [arcsec]'
  angle=round(*(*info).data->getinfo('SAT_ROT',lwin,/sji))
  if angle lt 0 then angle=360+angle
  case angle of
    0  : rot=0
    90 : rot=3
    180: rot=2
    270: rot=1
    else: begin 
      rot=0
      xtitle='Instrument X [arcsec]'
      ytitle='Instrument Y [arcsec]'
    endcase
  endcase
  sji_im=(*(*info).data)->descale_array( $
     rotate(((*(*info).data)->getsji(lwin,/noload))[*,*,(*info).expnr],rot))
  xw=(*(*info).data)->getxw_sji(lwin)
  yw=(*(*info).data)->getyw_sji(lwin)
  if sji_im[0] eq -1 then return
;  if (*info).show_raster then begin
;    fac = lwin gt 1 ? 1.7:4.0
;    xys=(*(*info).data)->find_slitpos(sjiwin=lwin,iexp=(*info).expnr)
;    for i=0,(*(*info).data)->getnraster(0)-1 do begin
;       sji_im[round(xys.xs[i]):round(xys.xs[i])+1,0:yw-1] = $
;         sji_im[round(xys.xs[i]):round(xys.xs[i])+1,0:yw-1]/fac
;    endfor
;  endif
  hdr=(*(*info).data)->gethdr_sji(lwin)
;  xscale=fxpar(hdr,'CDELT1')*indgen((size(sji_im))[1])+(*(*info).data)->getxcen_sji(lwin)
;  yscale=fxpar(hdr,'CDELT2')*indgen((size(sji_im))[2])+(*(*info).data)->getycen_sji(lwin)
  xscale=(*(*info).data)->xscale_sji(lwin)
  yscale=(*(*info).data)->yscale_sji(lwin)
  ((*(*info).data)->getaux())->loadct,'int+red'
  im_min=(*(*info).data)->datamin_sji(lwin)
  if (*info).log then begin 
    mplot_image,alog10(iris_histo_opt(sji_im,0.005,missing=*(*info).data->missing())>1.), $
      xscale,yscale,yticklen=0.02/(*info).aspect, $
      /bgblack,xtitle=xtitle,ytitle=ytitle, $
      top=254,color=254
  endif else begin
    mplot_image,(iris_histo_opt(sji_im>im_min,0.005,missing=*(*info).data->missing())),xscale,yscale, $
      /bgblack,xtitle=xtitle,ytitle=ytitle,yticklen=0.02/(*info).aspect, $
      top=254,color=254
  endelse
  if (*info).show_raster then begin
    xpos=*(*info).data->getxpos(sjiwin=lwin,sjiexpnr=(*(*info).data->locsji(lwin))[0]);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ypos=*(*info).data->getypos()
    angle=round(*(*info).data->getinfo('SAT_ROT',lwin,/sji))
    if angle < 0 then angle=360+angle
    for i=0,(*(*info).data)->getnraster(0)-1 do begin
      case angle of
        0  : plots,[xpos[i],xpos[i]],[ypos[0],ypos[n_elements(ypos)-1]],/data,color=255
        90 : plots,[xpos[0],xpos[n_elements(xpos)-1]],[ypos[i],ypos[i]],/data,color=255
        180: plots,[xpos[i],xpos[i]],[ypos[0],ypos[n_elements(ypos)-1]],/data,color=255
        270: plots,[xpos[0],xpos[n_elements(xpos)-1]],[ypos[i],ypos[i]],/data,color=255
        else: begin 
          plots,[xpos[i],xpos[i]],[ypos[0],ypos[n_elements(ypos)-1]],/data,color=255
        endcase
      endcase
    endfor
  endif
  ((*(*info).data)->getaux())->loadct,'int'
end

;resize main window
pro xsji_image_resize, event
  widget_control, event.top ,get_uvalue = info
  (*info).d_xsz = (event.x - (*info).menu_xsz) > 0
  (*info).d_ysz = (event.y - (*info).menu_ysz) > 0
  widget_control, (*info).drawid, draw_xsize = (*info).d_xsz, $
                   draw_ysize = (*info).d_ysz, xsize = (*info).d_xsz<((get_screen_size())[0])*0.75, $
                   ysize = (*info).d_ysz

  pseudoevent={widget_button,id:0L, $
    top:(*info).tlb, handler:0l, select:1}
  xsji_image_draw, pseudoevent

end

pro xsji_image_destroy, event
  widget_control, event.top, /destroy
end

pro xsji_image_cleanup, tlb
  widget_control, tlb, get_uvalue = info
  ptr_free, (*info).data
  ptr_free, info
end

pro xsji_image, data,lwin, groupl = groupl, slit=slit, $
             xtitle = xtitle, ytitle = ytitle, title = title

  if n_params() lt 2 then begin
    message,'xsji_image,data,lwin, slit=slit,xtitle=xtitle,ytitle=ytitle,groupl= groupl',/info
    return
  endif
  if n_elements(groupl) eq 0 then groupl=-1

  if n_elements(xtitle) eq 0 then xtitle = 'pixel'
  if n_elements(ytitle) eq 0 then ytitle = 'pixel'

  if groupl ne -1 then begin
    tlb = widget_base(title = 'SJI Image: '+data->getfilename_sji(lwin), mbar = menubar, $
                      tlb_size_events = 1, $
                          group_leader = groupl,/row,xoffset=100,yoffset=100)
  endif else begin
    tlb = widget_base(title = 'SJI Image: '+data->getfilename_sji(lwin), mbar = menubar, $
                      tlb_size_events = 1,/row,xoffset=100,yoffset=100)
  endelse
  
  lcol = widget_base(tlb, /frame, /column)
  rcol = widget_base(tlb, /column)

  displaybase = widget_base(rcol, /row)
  
  xscale=data->xscale_sji(lwin)
  yscale=data->yscale_sji(lwin)
  aspect=(max(xscale)-min(xscale))/(max(yscale)-min(yscale))
  xysz=(data->getaux())->getdrawsize('standard',aspect=aspect)
  calc_xysize,xysz[0],xysz[1],d_xsz,d_ysz

  drawid=widget_draw(displaybase, retain = 2, $
                     xsize = d_xsz < ((get_screen_size())[0])*0.75, x_scroll_size = d_xsz , $
                     ysize = d_ysz, y_scroll_size = d_ysz, $
                     /button_events, event_pro='xsji_image_draw')
;
  filemenu=widget_button(menubar, value='File',/menu, uvalue='file')
  savemenu=widget_button(filemenu, value='Save as', /menu)
  psmenu=widget_button(savemenu, value='Postscript', event_pro = 'xsji_image_ps')
  jpgmenu=widget_button(savemenu, value='JPG', event_pro = 'xsji_image_jpeg')
;
  exitmenu=widget_button(filemenu, value='Close', event_pro='xsji_image_destroy')
;
  sliderbase = widget_base(lcol,/col)
  if data->getnexp_sji(lwin) gt 1 then begin
    expslider = widget_slider(sliderbase, xsize = 90, $
                              minimum = 1, maximum = data->getnexp_sji(lwin), $
                              title = 'Exp.# ', $
                              value = 1, $
                              event_pro = 'xsji_image_expslider')
  endif
;
  rasterfield = widget_base(lcol,/column,/nonexclusive)
  rasterbutton = widget_button(rasterfield, value = 'show raster position', $
                              event_pro = 'xsji_image_show_raster')

  logfield = widget_base(lcol,/column,/nonexclusive)
  logbutton = widget_button(logfield, value = 'log(image)', $
                              event_pro = 'xsji_image_log')

  time=anytim2utc(anytim2tai(data->getinfo('STARTOBS',lwin,/sji))+data->gettime_sji(lwin), $
    /ccsds,/truncate)
  timebase=widget_base(lcol,/col)
  timetext=widget_label(timebase,value=time[0])

  ximoviefield = widget_base(lcol,/column)
  ximoviebutton = widget_button(ximoviefield, value = 'ximovie of SJI series', $
                              event_pro = 'xsji_image_ximovie')

  closefield = widget_base(lcol,/column)
  closebutton = widget_button(closefield, value = 'Close', $
                              event_pro = 'xsji_image_destroy')

    ; realize main window:
  widget_control, tlb, /realize, tlb_get_size = tlb_sz

  ; get window id of display window
  widget_control, drawid, get_value = wid
  wset, wid

  ; set automatic y-scaling as default:
  tlb_xsz = tlb_sz[0]  ; xsize of whole widget in pixels
  tlb_ysz = tlb_sz[1]  ; ysize of whole widget in pixels
  menu_xsz = tlb_xsz - d_xsz
  menu_ysz = tlb_ysz - d_ysz

  ; define the info structure, used to send information around
  info = {tlb:tlb               ,$
          data:ptr_new(data)    ,$
          lwin:lwin             ,$
          drawid:drawid         ,$
          xtitle:xtitle         ,$
          ytitle:ytitle         ,$
          timetext:timetext     ,$
          time:time             ,$
          d_xsz:d_xsz           ,$
          d_ysz:d_ysz           ,$
          expnr:0               ,$
          log:0                 ,$
          aspect:aspect         ,$
          show_raster:0         ,$
          tlb_xsz:tlb_xsz       ,$
          tlb_ysz:tlb_ysz       ,$
          menu_xsz:menu_xsz     ,$
          menu_ysz:menu_ysz     ,$
          wid:wid}
  info = ptr_new(info, /no_copy)

  ; set user value of tlb widget to be the info ptr
  widget_control, tlb, set_uvalue = info
  ; create pseudoevent and send this event to xdisplay_draw,
  ; in order to draw the image
  pseudoevent={widget_base,id:0l, $
               top:tlb, handler:0l, x:tlb_xsz, y:tlb_ysz}
  xsji_image_resize, pseudoevent
  if groupl ne -1 then begin
    xmanager, 'xsji_image', tlb, group_leader = groupl, $
             event_handler = 'xsji_image_resize', cleanup = 'xsji_image_cleanup',/no_block
  endif else begin
    xmanager, 'xsji_image', tlb, $
             event_handler = 'xsji_image_resize', cleanup = 'xsji_image_cleanup',/no_block
  endelse
end
