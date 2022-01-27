;+
; NAME:
;       XMOMENT
;
; PURPOSE:
;       XMOMENT plots mean line profile, and has interactive mouse
;       functions to mark where the line is (start/stop pixels) and
;       where the continuum is. This information is used by the
;       eis_moment__moment program to calulate moments of the line
;       profile. 
;
; CATEGORY:
;       Hansteen/Wikstol Data analysis SW
;
; CALLING SEQUENCE:
;       
;
; INPUTS:
;       
;
; KEYWORD PARAMETERS:
;       
;
; OUTPUTS:
;       
;
; CALLS:
;       
;
; COMMON BLOCKS:
;       
;
; PROCEDURE:
;       
;
; RESTRICTIONS:
;
;
; MODIFICATION HISTORY:
;       23-Feb-2004: Oivind Wikstol.
;        4-Dec-2007: A. Gardini      - Pointers freed.
;        3-Jan-2008: A. Gardini      - Slider corrected.
;       24-Jan-2008: A. Gardini	     - Slider bug corrected.
;        8-Jul-2008: A. Gardini	     - Slider modified.
;       11-Jul-2008: A. Gardini	     - Set xwindow.
;-
; save plot as postscript file
pro xmoment_gauss_ps,event
  thisfile=dialog_pickfile(/write,file='pixelplot.ps')
  if thisfile eq '' then return
  widget_control,event.top,get_uvalue=info
;  keywords=pswindow()
  thisdevice=!d.name
  set_plot,'ps',/copy
  device,file=thisfile,_extra=keywords,/inches,bits_per_pixel=8,/color
  pseudoevent={widget_button,id:(*info).action, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  xmoment_gauss_draw,pseudoevent
  device,/close_file
  set_plot,thisdevice
end

; save as jpeg file
pro xmoment_gauss_jpeg,event
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

; draw line profile
pro xmoment_gauss_draw, event
  widget_control, event.top, get_uvalue = info
  wset,(*info).wid
  xtitle = (*info).xtitle
  ytitle = (*info).ytitle
  mspec = (*info).mspec
  lambda = (*info).lambda
  xr=[lambda[(*info).start], lambda[(*info).stop]]
; ystart = !y.crange[0]
; ystop = !y.crange[1]
  ystart = min(mspec)
  ystop = max(mspec)
  yr=[ystart,ystop]
  plot, lambda, mspec, xrange = xr, yrange = yr, $
         xtitle = xtitle, ytitle = ytitle, /ynoz, xsty = 1, ysty = 1
  oplot,[(*info).wlen_ref,(*info).wlen_ref],yr,line=2

  l1 = lambda[(*info).lpix[0]]
  l2 = lambda[(*info).lpix[1]]
  c1 = lambda[(*info).cpix[0]]
  c2 = lambda[(*info).cpix[1]]

;  l1 = (*info).lpix[0] + (*info).slidmin
;  l2 = (*info).lpix[1] + (*info).slidmin
;  c1 = (*info).cpix[0] + (*info).slidmin
;  c2 = (*info).cpix[1] + (*info).slidmin

  y1 = ystart
  y2 = max(mspec)
  x1 = max([l1,lambda[(*info).start]])
  x2 = min([l2,lambda[(*info).stop]])
  if (x2 gt x1) then polyfill, [x1, x2, x2, x1, x1], [y1, y1, y2, y2, y1], /line_fill, orient = 135, color = 255
  
  y1 = ystart
  y2 = 0.3*(ystop-ystart)+ystart
  x1 = max([c1,lambda[(*info).start]])
  x2 = min([c2,lambda[(*info).stop]])
  if (x2 gt x1) then polyfill, [x1, x2, x2, x1, x1], [y1, y1, y2, y2, y1], /line_fill, orient = 45

;  Plots, [l1, l1], [ystart, ystop]
;  plots, [l2, l2], [ystart, ystop]
;  plots, [c1, c1], [ystart, ystop]
;  plots, [c2, c2], [ystart, ystop]
end

function xmoment_gauss_wlen, event
  widget_control, event.top,get_uvalue=info
  (*info).wlen_ref=event.value
  (*(*info).data)->setline_wvl,(*info).iwin,(*info).wlen_ref
  pseudoevent={widget_button,id:0l, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  xmoment_gauss_draw,pseudoevent
  return,0
end

pro xmoment_gauss_startslider, event
  widget_control, event.top,get_uvalue=info
  (*info).start=event.value
  if (*info).start ge (*info).stop then begin
    if (*info).start eq (*info).slidmax then begin
      (*info).start = (*info).slidmax-1 
      widget_control,(*info).startslider,set_value=(*info).start 
    endif 
    (*info).stop = (*info).start +1
    widget_control,(*info).stopslider,set_value=(*info).stop 
;   ok = dialog_message('Start must be lt stop')
;   return
  endif
  x1 = (*info).start - (*info).slidmin
  x2 = ((*info).stop - (*info).slidmin) > x1
  xscale = indgen(x2 - x1 +1)+x1
  *(*info).xscale = interpol(xscale, (*info).xps)
;  *(*info).xscale = interpol((*info).lambda[x1:x2], (*info).xps)

  pseudoevent={widget_button,id:0l, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  xmoment_gauss_draw,pseudoevent
end

pro xmoment_gauss_stopslider, event
  widget_control, event.top,get_uvalue=info
  (*info).stop=event.value
  if (*info).stop le (*info).start then begin
    if (*info).stop eq (*info).slidmin then begin
      (*info).stop = (*info).slidmin +1
      widget_control,(*info).stopslider,set_value=(*info).stop 
    endif
    (*info).start = (*info).stop -1 
    widget_control,(*info).startslider,set_value=(*info).start 
;   ok = dialog_message('Stop must be gt to start')
;   return
  endif
  x1 = (*info).start - (*info).slidmin
  x2 = ((*info).stop - (*info).slidmin) > x1
  xscale = indgen(x2 - x1 +1)+x1
  *(*info).xscale = interpol(xscale, (*info).xps)

;  *(*info).xscale = interpol((*info).lambda[x1:x2], (*info).xps)

  pseudoevent={widget_button,id:0l, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  xmoment_gauss_draw,pseudoevent
end

pro xmoment_gauss_line_startslider, event
  widget_control, event.top,get_uvalue=info
  (*info).lpix[0] = event.value;-(*info).slidmin AG
  if (*info).lpix[0] gt (*info).lpix[1] then begin
    (*info).lpix[1] = (*info).lpix[0]
;   (*info).lpix = (*info).lpix[sort((*info).lpix)] old AG
    widget_control,(*info).line_stopslider,set_value=(*info).lpix[1] 
  endif
  (*(*info).data)-> setline_px, (*info).iwin, (*info).lpix
  pseudoevent={widget_button,id:0l, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  xmoment_gauss_draw,pseudoevent
end

pro xmoment_gauss_line_stopslider, event
  widget_control, event.top,get_uvalue=info
  (*info).lpix[1]=event.value
  if (*info).lpix[1] lt (*info).lpix[0] then begin
    (*info).lpix[0] = (*info).lpix[1]
;   (*info).lpix = (*info).lpix[sort((*info).lpix)] old AG
    widget_control,(*info).line_startslider,set_value=(*info).lpix[0] 
  endif 
  (*(*info).data)-> setline_px, (*info).iwin, (*info).lpix
  pseudoevent={widget_button,id:0l, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  xmoment_gauss_draw,pseudoevent
end

pro xmoment_gauss_cont_startslider, event
  widget_control, event.top,get_uvalue=info
  (*info).cpix[0]=event.value
  if (*info).cpix[0] gt (*info).cpix[1] then begin
    (*info).cpix[1] = (*info).cpix[0]
;   (*info).cpix = (*info).cpix[sort((*info).cpix)] old AG
    widget_control,(*info).cont_stopslider,set_value=(*info).cpix[1] 
  endif
  (*(*info).data)-> setcont_px, (*info).iwin, (*info).cpix
  pseudoevent={widget_button,id:0l, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  xmoment_gauss_draw,pseudoevent
end

pro xmoment_gauss_cont_stopslider, event
  widget_control, event.top,get_uvalue=info
  (*info).cpix[1]=event.value
  if (*info).cpix[1] lt (*info).cpix[0] then begin
    (*info).cpix[0] = (*info).cpix[1]
;   (*info).cpix = (*info).cpix[sort((*info).cpix)]
    widget_control,(*info).cont_startslider,set_value=(*info).cpix[0] 
  endif
  (*(*info).data)-> setcont_px, (*info).iwin, (*info).cpix
  pseudoevent={widget_button,id:0l, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  xmoment_gauss_draw,pseudoevent
end

; define start/stop pixels for line(s) and continuum
pro xmoment_gauss_setpix, event
  widget_control, event.top, get_uvalue = info
  if event.type gt 2 then return
  events=['down','up','motion']
  thisevent=events[event.type]
 
  case thisevent of 
  'down':begin
         end
  'up':begin
         xmax = (size(*(*info).xscale))[1]
         xp =   (event.x - (*info).xs) > 0
         xnew = fix((*(*info).xscale)[xp] - (*info).slidmin)
         if (*info).setlpix then begin
           if (*info).startpix then (*info).lpix[0] = xnew
           if (*info).stoppix then (*info).lpix[1] = xnew
           if (*info).lpix[1] lt (*info).lpix[0] then $
              (*info).lpix[1] = (*info).lpix[0] + 7 < (*info).stop
           ; put pixels into data object fields
           (*info).lpix = (*info).lpix[sort((*info).lpix)]
           (*(*info).data)-> setline_px, (*info).iwin, (*info).lpix
         endif else if (*info).setcpix then begin
           if (*info).startpix then (*info).cpix[0] = xnew
           if (*info).stoppix then (*info).cpix[1] = xnew
           if (*info).cpix[1] lt (*info).cpix[0] then $
              (*info).cpix[1] = (*info).cpix[0] + 7 < (*info).stop
           (*info).cpix = (*info).cpix[sort((*info).cpix)]
           ; put pixels into data object fields
           (*(*info).data)-> setcont_px, (*info).iwin, (*info).cpix
         endif
         ; switch between start and stop
         if (*info).startpix eq 1 then begin
         (*info).startpix = 0
         (*info).stoppix = 1
          endif else begin
          (*info).startpix = 1
          (*info).stoppix = 0
         endelse
       end
  'motion':begin
          end
  endcase

  ; plot new coordinates in draw widget:
  pseudoevent={widget_button,id:0L, $
    top:event.top, handler:0l, select:1}
  xmoment_gauss_draw, pseudoevent
  
end

; button to set line pixel or continuum pixel definition:
function xmoment_gauss_pix_select, event
  widget_control, event.top, get_uvalue = info
  case event.value of
    0: begin
        (*info).setlpix = 1
        (*info).setcpix = 0
      end
    1: begin
         (*info).setlpix = 0
         (*info).setcpix = 1
       end
  endcase
  return, 1
end

;resize main window
pro xmoment_gauss_resize, event
  widget_control, event.top ,get_uvalue = info
  (*info).d_xsz = (event.x - (*info).menu_xsz) > 0
  (*info).d_ysz = (event.y - (*info).menu_ysz) > 0
;  (*info).d_ysz = event.y - (*info).menu_ysz
  widget_control, (*info).drawid, draw_xsize = (*info).d_xsz, $
                   draw_ysize = (*info).d_ysz, xsize = (*info).d_xsz, $
                   ysize = (*info).d_ysz

  (*info).xs = !x.margin[0]*!d.x_ch_size
  (*info).xps = (*info).d_xsz - total(!x.margin)*!d.x_ch_size  ; x-overhead pixels
  ptr_free, (*info).xscale
  (*info).xscale = ptr_new((*info).xps)

  x1 = (*info).start - (*info).slidmin
  x2 = ((*info).stop - (*info).slidmin) > x1
  xscale = indgen(x2 - x1 +1) + x1
*(*info).xscale = interpol(xscale, (*info).xps)
;  *(*info).xscale = interpol((*info).lambda, (*info).xps)

  pseudoevent={widget_button,id:0L, $
    top:(*info).tlb, handler:0l, select:1}
  xmoment_gauss_draw, pseudoevent

end

pro xmoment_gauss_destroy, event
       widget_control, event.top, /destroy
end

pro xmoment_gauss_cleanup, tlb
  widget_control, tlb, get_uvalue = info
  ptr_free, (*info).xscale
  ptr_free, (*info).data
  ptr_free, info
end

pro xmoment_gauss, data, mspec, iwin, lambda, groupl = groupl, $
             xtitle = xtitle, ytitle = ytitle

  if n_params() lt 4 then begin
    print, 'xmoment_gauss, mspec, lambda, lpix, cpix, groupl = groupl, $'
    print, 'xtitle = xtitle, ytitle = ytitle'
    return
  endif

  if n_elements(xtitle) eq 0 then xtitle = 'Wavelength'
  if n_elements(ytitle) eq 0 then ytitle = 'Intensity'

  nlam = (size(lambda))[1]
  nspec = (size(mspec))[1]
;  start = lambda[0]
  start = 0
  stop = n_elements(lambda) - 1
;  stop = lambda[(size(lambda))[1]-1]
  if nlam ne nspec then begin
    print, 'xmoment_gauss: Wavelength and intensity arrays must be of same length'
           return
  endif

  ; define line pixels and continum pixels (start value)
;  lpix = [0,0]
;  cpix = [0,0]
  wd_def=(data->getwd_def())[iwin]
  lpix=wd_def.line_px
  cpix=wd_def.cont_px
  if groupl ne -1 then begin
  tlb = widget_base(title = 'Moments Prep Tool', mbar = menubar, $
                    tlb_size_events = 1, $
                    group_leader = groupl,/row,xoffset=50,yoffset=50)
  endif else begin
  tlb = widget_base(title = 'Moments Prep Tool', mbar = menubar, $
                    tlb_size_events = 1,/row,xoffset=50,yoffset=50)
  endelse
  
  lcol = widget_base(tlb, /frame, /column)
  rcol = widget_base(tlb, /column)

  displaybase = widget_base(rcol, /row)

  d_xsz = 500
  d_ysz = 400

  drawid=widget_draw(displaybase, retain = 2, xsize = d_xsz, ysize = d_ysz, $
                     /button_events, event_pro = 'xmoment_gauss_setpix')
  filemenu=widget_button(menubar, value='File',/menu, uvalue='file')
  savemenu=widget_button(filemenu, value='Save as', /menu)
  psmenu=widget_button(savemenu, value='Postscript', event_pro = 'xmoment_gauss_ps')
  jpgmenu=widget_button(savemenu, value='JPG', event_pro = 'xmoment_gauss_jpeg')

  exitmenu=widget_button(filemenu, value='Close', event_pro='xmoment_gauss_destroy')
;
;  ; Select line or continuum pixels
;  pixbase = widget_base(lcol, /column, /frame)
;  ; data source label field
;  pix_label_base = widget_base(pixbase, /row)
;  title = 'Select Line or Continuum Pixels'    ; label string
;  pix_label = widget_label(pix_label_base, value=title)
;  pix_id = ['Set Line Pixels', 'Set Continuum Pixels']
;  pixselect = cw_bgroup(pix_label_base, /exclusive, set_value = 0, $
;                         pix_id, event_func = 'xmoment_gauss_pix_select')

  starttitle = 'Plot Region [X]'
  stoptitle = 'Plot Region [X]'
  
  regionlabel = widget_label(lcol, value='Choose plot region')

  startslider = widget_slider(lcol, xsize=120, $
                          minimum=start, maximum=stop, $
                          title=starttitle, $
                          value=start, event_pro = 'xmoment_gauss_startslider', $
                          /drag)
  stopslider = widget_slider(lcol, xsize=120, $
                          minimum=start, maximum=stop, $
                          title=stoptitle, $
                          value=stop, $
                          event_pro = 'xmoment_gauss_stopslider', /drag)

  linelabel = widget_label(lcol, value='Define Line')
  wlen_ref=data->getline_wvl(iwin,wscale='AA')
  line_wlenslider = cw_fslider(lcol,/edit,format='(f7.2)',/frame, $
      maximum=lambda[stop],minimum=lambda[start], $
      title='Reference wavelength (def = '+string(wlen_ref,format='(f7.2)')+')', $
      value=wlen_ref,event_func='xmoment_gauss_wlen',/drag)

  line_starttitle = 'Line Start [X]'
  line_stoptitle = 'Line Stop [X]'
  line_startslider = widget_slider(lcol, xsize=120, $
                          minimum=start, maximum=stop, $
                          title=line_starttitle, $
                          value=lpix[0], event_pro = 'xmoment_gauss_line_startslider', $
                          /drag)
  line_stopslider = widget_slider(lcol, xsize=120, $
                          minimum=start, maximum=stop, $
                          title=line_stoptitle, $
                          value=lpix[1], $
                          event_pro = 'xmoment_gauss_line_stopslider', /drag)

  linelabel = widget_label(lcol, value='Define Continuum')
  cont_starttitle = 'Continuum Start [X]'
  cont_stoptitle = 'Continuum Stop [X]'
  cont_startslider = widget_slider(lcol, xsize=120, $
                          minimum=start, maximum=stop, $
                          title=cont_starttitle, $
                          value=cpix[0], event_pro = 'xmoment_gauss_cont_startslider', $
                          /drag)
  cont_stopslider = widget_slider(lcol, xsize=120, $
                          minimum=start, maximum=stop, $
                          title=cont_stoptitle, $
                          value=cpix[1], $
                          event_pro = 'xmoment_gauss_cont_stopslider', /drag)

  closefield = widget_base(lcol,/column)
  closebutton = widget_button(closefield, value = 'Finished', $
                              event_pro = 'xmoment_gauss_destroy')

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
          data:ptr_new(data), $
          iwin:iwin, $
          drawid:drawid, $
          mspec:mspec, $
          lambda:lambda, $
          startslider : startslider, $
          stopslider : stopslider, $
          line_startslider : line_startslider, $
          line_stopslider : line_stopslider, $
          cont_startslider : cont_startslider, $
          cont_stopslider : cont_stopslider, $
          start:start, $
          stop:stop, $
          slidmin:start, $
          slidmax:stop, $
          wlen_ref:wlen_ref, $
          lpix:lpix, $
          cpix: cpix, $
          setlpix:1, $
          setcpix:0, $
          xtitle:xtitle, $
          ytitle:ytitle, $
          d_xsz:d_xsz           ,$
          d_ysz:d_ysz           ,$
          tlb_xsz:tlb_xsz       ,$
          tlb_ysz:tlb_ysz       ,$
          menu_xsz:menu_xsz     ,$
          menu_ysz:menu_ysz, $
          xscale:ptr_new(), $
          startpix:1, $
          stoppix:0, $
          xps:0, $
          xs:0, $
          action:drawid, $
          wid:wid}
  info = ptr_new(info, /no_copy)

  ; set user value of tlb widget to be the info ptr
  widget_control, tlb, set_uvalue = info
  ; create pseudoevent and send this event to xdisplay_draw,
  ; in order to draw the image
; widget_control, line_stopslider, set_value=stop ; 2-Jan-2008
  pseudoevent={widget_base,id:0l, $
               top:tlb, handler:0l, x:tlb_xsz, y:tlb_ysz}
  xmoment_gauss_resize, pseudoevent
;  pseudoevent={widget_button,id:0L, $
;    top:tlb, handler:0l, select:1}
;  xmoment_gauss_draw, pseudoevent
  if groupl ne -1 then begin
  xmanager, 'xmoment_gauss', tlb, group_leader = groupl, $
             event_handler = 'xmoment_gauss_resize', cleanup = 'xmoment_gauss_cleanup'
  endif else begin
  xmanager, 'xmoment_gauss', tlb, $
             event_handler = 'xmoment_gauss_resize', cleanup = 'xmoment_gauss_cleanup'
  endelse
end
