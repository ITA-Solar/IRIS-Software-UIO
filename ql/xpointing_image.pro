;+
; NAME:
;       XSDO_IMAGE
;
; PURPOSE:
;       XSDO_IMAGE plots an image of the Sun along with an outline of
;       the IRIS field of view.
;
; CATEGORY:
;       IRIS Data analysis SW
;
; CALLING SEQUENCE:
;       
; INPUTS: 
;       d - structure containing solar image, IRIS field of view
;           localtion and size and auxilary information.
;
; KEYWORD PARAMETERS: 
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
;       17-Jun-2013: Viggo Hansteen
; 
; $Id: xpointing_image.pro,v 1.3 2013/10/18 16:09:44 viggoh Exp $
;-
; save plot as postscript file
pro xpointing_image_ps,event
  thisfile=dialog_pickfile(/write,file='xpointing_image.ps')
  if thisfile eq '' then return
  widget_control,event.top,get_uvalue=info
  thisdevice=!d.name
  set_plot,'ps',/copy
  device,file=thisfile,_extra=keywords,/inches,bits_per_pixel=8,/color
  pseudoevent={widget_button,id:(*info).action, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  xpointing_image_draw,pseudoevent
  device,/close_file
  set_plot,thisdevice
end

; save as jpeg file
pro xpointing_image_jpeg,event
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
pro xpointing_image_slider,event
  widget_control, event.top, get_uvalue = info
  (*info).imnr=event.value-1
  pseudoevent={widget_button,id:0L, $
    top:(*info).tlb, handler:0l, select:1}
  xpointing_image_draw, pseudoevent
end

; draw image
pro xpointing_image_draw, event
  widget_control, event.top, get_uvalue = info
  wset,(*info).wid
  dum=*((*info).d.im)[(*info).imnr]
  plot_image,dum,true=1,pos=[0,0,1,1],xstyle=5,ystyle=5
       plots,[(*info).coord.x1,(*info).coord.x2, $
              (*info).coord.x3,(*info).coord.x4,(*info).coord.x1],$
             [(*info).coord.y1,(*info).coord.y2, $
              (*info).coord.y3,(*info).coord.y4,(*info).coord.y1],/data
end

;resize main window
pro xpointing_image_resize, event
  widget_control, event.top ,get_uvalue = info
  (*info).d_xsz = (event.x - (*info).menu_xsz) > 0
  (*info).d_ysz = (event.y - (*info).menu_ysz) > 0
  (*info).d_xsz = max([(*info).d_xsz,(*info).d_ysz])
  (*info).d_ysz = max([(*info).d_xsz,(*info).d_ysz])
  widget_control, (*info).drawid, draw_xsize = (*info).d_xsz, $
                   draw_ysize = (*info).d_ysz, xsize = (*info).d_xsz, $
                   ysize = (*info).d_ysz
  pseudoevent={widget_button,id:0L, $
    top:(*info).tlb, handler:0l, select:1}
  xpointing_image_draw, pseudoevent
end

pro xpointing_image_destroy, event
  widget_control, event.top, /destroy
end

pro xpointing_image_cleanup, tlb
  widget_control, tlb, get_uvalue = info
  ptr_free, info
end

pro xpointing_image,d,coord, groupl = groupl

  if n_params() lt 1 then begin
    message,'xpointing_image,d, groupl= groupl',/info
    return
  endif
  if n_elements(p) eq 0 then p=-1
  if n_elements(groupl) eq 0 then groupl=-1

  if groupl ne -1 then begin
    tlb = widget_base(title = 'Pointing Image Tool', mbar = menubar, $
                      tlb_size_events = 1, $
                          group_leader = groupl,/row,xoffset=100,yoffset=100)
  endif else begin
    tlb = widget_base(title = 'Pointing Image Tool', mbar = menubar, $
                      tlb_size_events = 1,/row,xoffset=100,yoffset=100)
  endelse
  
  lcol = widget_base(tlb, /frame, /column)
  rcol = widget_base(tlb, /column)

  displaybase = widget_base(rcol, /row)

  d_xsz = 500
  d_ysz = 500

  drawid=widget_draw(displaybase, retain = 2, xsize = d_xsz, ysize = d_ysz)
;
  filemenu=widget_button(menubar, value='File',/menu, uvalue='file')
  savemenu=widget_button(filemenu, value='Save as', /menu)
  psmenu=widget_button(savemenu, value='Postscript', event_pro = 'xpointing_image_ps')
  jpgmenu=widget_button(savemenu, value='JPG', event_pro = 'xpointing_image_jpeg')
;
  exitmenu=widget_button(filemenu, value='Close', event_pro='xpointing_image_destroy')
;
  sliderbase = widget_base(lcol,/col)
  if d.nr gt 1 then begin
    imslider = widget_slider(sliderbase, xsize = 90, $
                              minimum = 1, maximum = d.nr, $
                              title = 'Image # ', $
                              value = 1, $
                              event_pro = 'xpointing_image_slider')
  endif

  closefield = widget_base(lcol,/column)
  closebutton = widget_button(closefield, value = 'Close', $
                              event_pro = 'xpointing_image_destroy')

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
  info = {tlb:tlb, $
          d:d, $ 
          coord:coord, $ 
          drawid:drawid, $
          d_xsz:d_xsz           ,$
          d_ysz:d_ysz           ,$           
          imnr:0                ,$
          tlb_xsz:tlb_xsz       ,$
          tlb_ysz:tlb_ysz       ,$
          menu_xsz:menu_xsz     ,$
          menu_ysz:menu_ysz     ,$
          wid:wid}
  info = ptr_new(info, /no_copy)

  ; set user value of tlb widget to be the info ptr
  widget_control, tlb, set_uvalue = info
  ; create pseudoevent and send this event to xpointing_image_resize,
  ; in order to draw the image
  pseudoevent={widget_base,id:0l, $
               top:tlb, handler:0l, x:tlb_xsz, y:tlb_ysz}
  xpointing_image_resize, pseudoevent
  if groupl ne -1 then begin
    xmanager, 'xpointing_image', tlb, group_leader = groupl, $
             event_handler = 'xpointing_image_resize', cleanup = 'xpointing_image_cleanup',/no_block
  endif else begin
    xmanager, 'xpointing_image', tlb, $
             event_handler = 'xpointing_image_resize', cleanup = 'xpointing_image_cleanup',/no_block
  endelse
end
