;+
; NAME:
;       IRIS_XIMOVIE
;
; PURPOSE:
;      
;       IRIS_XIMOVIE provides a widget interface to run and control 
;       images displayed as a movie. The images must be written 
;       as an assoc file.
;                                                                  
;
; CATEGORY:
;       Hansteen/Wikstol Data analysis SW
;
; CALLING SEQUENCE:
;       iris_ximovie, file
;
; INPUTS:
;       pos: assoc file
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;       A widget that shows the movie with buttons to control it, and 
;       various options to save movie/images and to select color table. 
;
; CALLS:
;       
;
; COMMON BLOCKS:
;       
;
; PROCEDURE:
;
; RESTRICTIONS:
;
;
; MODIFICATION HISTORY:
;       Version 1.0, Jan-2004: Oivind Wikstol. 
;                  3-Dec-2007: Alessandro Gardini - Pointers freed.
; $Id: iris_ximovie.pro,v 1.26 2014/02/13 09:28:52 mawiesma Exp $
;-
; save snapshot as postscript file
pro iris_ximovie_ps,event
  widget_control, event.top,get_uvalue=info

  if(*info).run eq 1 then begin
    message = 'Please pause movie to select snapshot image frame'
    ok = dialog_message(message, dialog_parent = (*info).top)
    return
  endif

  thisfile=dialog_pickfile(/write,file='snapshot.ps')
  if thisfile eq '' then return
;  keywords=pswindow()
  thisdevice=!d.name
  set_plot,'ps',/copy
  device,file=thisfile,_extra=keywords,/inches,bits_per_pixel=8,/color
  pseudoevent={widget_button,id:(*info).action, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  iris_ximovie_draw,pseudoevent
  device,/close_file
  set_plot,thisdevice
end

; save as jpeg file
pro iris_ximovie_jpeg,event
  widget_control,event.top,get_uvalue=info
  if(*info).run eq 1 then begin
    message = 'Please pause movie to select snapshot image frame'
    ok = dialog_message(message, dialog_parent = (*info).top)
    return
  endif

  thisfile=dialog_pickfile(/write,file='idl.jpg')
  if thisfile eq '' then return
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

; save all frames as jpeg files
pro iris_ximovie_jpeg_all,event
  widget_control,event.top,get_uvalue=info

  thisfile=dialog_pickfile(/write,file='idl')
  if thisfile eq '' then return

  for i = (*info).first, (*info).last do begin
    (*info).frame_nr = i
    pseudoevent={widget_button,id:(*info).action, $
                  top:event.top, handler:0l, select:1}
    widget_control,event.top,set_uvalue=info
    iris_ximovie_draw,pseudoevent
  
    snapshot=tvrd()
    tvlct,r,g,b,/get
    s=size(snapshot)
    image24=bytarr(3,s[1],s[2])
    image24(0,*,*)=r(snapshot)
    image24(1,*,*)=g(snapshot)
    image24(2,*,*)=b(snapshot)
    write_jpeg,thisfile+string3(i)+'.jpg',image24,true=1,quality=75
    print, 'Saving frame # '+ strtrim(string(i), 2)
  endfor
end

; save as gif file
pro iris_ximovie_gif,event
  widget_control,event.top,get_uvalue=info
  if(*info).run eq 1 then begin
    message = 'Please pause movie to select snapshot image frame'
    ok = dialog_message(message, dialog_parent = (*info).top)
    return
  endif
  tvlct,r,g,b,/get
  thisfile=dialog_pickfile(/write,file='idl.gif')
  if thisfile eq '' then return
  ; write image
  if (*info).mag eq 1.0 then begin
    write_gif, thisfile, bytscl((*info).image,max=(*info).imax,min=(*info).imin), r, g, b
  endif else begin
    case (*info).use_rebin of
      1: write_gif, thisfile, bytscl(rebin((*info).image, (*info).d_xsz, (*info).d_ysz),max=(*info).imax,min=(*info).imin), $
                    r, g, b 
      0: write_gif, thisfile, bytscl(congrid((*info).image, (*info).d_xsz, (*info).d_ysz),max=(*info).imax,min=(*info).imin), $ 
                    r, g, b
    endcase
  endelse
end

; save all frames as gif files
pro iris_ximovie_gif_all,event
  widget_control,event.top,get_uvalue=info

  thisfile=dialog_pickfile(/write,file='idl')
  if thisfile eq '' then return

  for i = (*info).first, (*info).last do begin
    (*info).frame_nr = i
    pseudoevent={widget_button,id:(*info).action, $
                  top:event.top, handler:0l, select:1}
    widget_control,event.top,set_uvalue=info
    iris_ximovie_draw,pseudoevent
    tvlct,r,g,b,/get
  ; write image
  if (*info).mag eq 1.0 then begin
    write_gif, thisfile+string3(i)+'.gif', bytscl(*(*info).image,max=(*info).imax,min=(*info).imin), r, g, b
  endif else begin
    case (*info).use_rebin of
      1: write_gif, thisfile+string3(i)+'.gif' , bytscl(rebin((*info).image, (*info).d_xsz, (*info).d_ysz), $
                                                        max=(*info).imax,min=(*info).imin), r, g, b
      0: write_gif, thisfile+string3(i)+'.gif' , bytscl(congrid((*info).image, (*info).d_xsz, (*info).d_ysz), $
                                                        max=(*info).imax,min=(*info).imin), r, g, b
    endcase
  endelse
    print, 'Saving frame # '+ strtrim(string(i), 2)
  endfor
end

; save as mpeg movie
pro iris_ximovie_mpeg,event
  widget_control,event.top,get_uvalue=info

  ; dialog box to select file name
  thisfile=dialog_pickfile(/write,file='idl.mpg')

; set dimensions of the mpeg fil (xsize, ysize). Use window size.
  xsize = (*info).d_xsz
  ysize = (*info).d_ysz
  ; initialize mpg object
  quality = 100
  mpegid = mpeg_open([xsize, ysize], quality = quality)
  bscale=(*info).scaling1[0]
  bzero=(*info).scaling1[1]
  bscale2=(*info).scaling2[0]
  bzero2=(*info).scaling2[1]
  for i = (*info).first, (*info).last do begin
    frame_nr = i
    if((*info).twostream) then begin
      ; display only images from stream 1, only from stream2 or from both
      if ((*info).stream1) then begin
        image = (bscale*((*(*info).aimg)[frame_nr])+bzero)^(*info).gamma
        imin = (*info).imin
        imax = (*info).imax
      if (*info).imean ne 0 then image = image*(*info).imean/mean(image)
      endif else if ((*info).stream2) then begin
        image = (bscale2*((*(*info).aimg2)[frame_nr])+bzero2)^(*info).gamma2
        imin = (*info).i2min
        imax = (*info).i2max
        if (*info).i2mean ne 0 then image = image*(*info).i2mean/mean(image)
      endif else begin   ; this is the "blink" option
        if((*info).mix)then begin
          (*info).mix = 0
          (*info).fr_incr = 0
          image = (bscale*((*(*info).aimg)[frame_nr])+bzero)^(*info).gamma
          imin = (*info).imin
          imax = (*info).imax
        if (*info).imean ne 0 then image = image*(*info).imean/mean(image)
        endif else begin
          (*info).mix = 1
          image = (bscale2*((*(*info).aimg2)[frame_nr])+bzero2)^(*info).gamma2
          imin = (*info).i2min
          imax = (*info).i2max
        if (*info).i2mean ne 0 then image = image*(*info).i2mean/mean(image)
        endelse
      endelse
    endif else begin
      image = (bscale*((*(*info).aimg)[frame_nr])+bzero)^(*info).gamma
      imin = (*info).imin
      imax = (*info).imax
      if (*info).imean ne 0 then image = image*(*info).imean/mean(image)
    endelse
    image = image[(*info).startx:(*info).stopx, (*info).starty:(*info).stopy]

    ; bytscl image if it's not a byte array
    if not (*info).byt then $
      image = bytscl(image, min = imin, max = imax)
    ; store image at the specified frame index in mpeg movie
    print, 'storing frame # '+string(i)
    mpeg_put, mpegid, image = image, frame = i
  endfor
  ; save and close
  mpeg_save, mpegid, filename = thisfile
  mpeg_close, mpegid
end

pro iris_ximovie_frameslider, event
  widget_control, event.top,get_uvalue=info
  (*info).frame_nr=event.value
  pseudoevent={widget_button,id:(*info).action, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  iris_ximovie_draw,pseudoevent
end

; slider to change zoom position - X
pro iris_ximovie_xscroll_slider, event
  widget_control, event.top, get_uvalue = info
  nx = (*info).stopx-(*info).startx
  (*info).startx = event.value
  (*info).stopx = event.value + nx
  pseudoevent={widget_button,id:(*info).action, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  iris_ximovie_draw,pseudoevent

  pseudoevent={widget_button,id:0, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  iris_ximovie_boxdraw,pseudoevent
end

; slider to change zoom position - Y
pro iris_ximovie_yscroll_slider, event
  widget_control, event.top, get_uvalue = info
  ny = (*info).stopy-(*info).starty
  (*info).starty = event.value
  (*info).stopy = event.value + ny
  pseudoevent={widget_button,id:(*info).action, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  iris_ximovie_draw,pseudoevent

  pseudoevent={widget_button,id:0, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top, set_uvalue=info
  iris_ximovie_boxdraw,pseudoevent
end

; draw box in top level draw window when zoom position is changed
pro iris_ximovie_boxdraw, event
  widget_control, event.top, get_uvalue = info
  wset, (*info).topwid
  ; get size of parent image
  img = tvrd()
  sz = size(img)
  xsz = sz[1]
  ysz = sz[2]
  if xsz ne (*info).x_org_size or ysz ne (*info).y_org_size then begin
    xmag = xsz/float((*info).x_org_size)
    ymag = ysz/float((*info).y_org_size)
  endif else begin
    xmag = 1.
    ymag = 1.
  endelse
 
  bscale=(*info).scaling1[0]
  bzero=(*info).scaling1[1]
  if(xmag ne 1 or ymag ne 1) then begin
    img = (bscale*(congrid((*(*info).aimg)[(*info).frame_nr], xsz, ysz))+bzero)^(*info).gamma
    startx = fix((*info).startx*xmag)
    stopx = fix((*info).stopx*xmag)
    starty = fix((*info).starty*ymag)
    stopy = fix((*info).stopy*ymag)
  endif else begin 
    img = (bscale*((*(*info).aimg)[(*info).frame_nr])+bzero)^(*info).gamma
    startx = (*info).startx
    starty = (*info).starty
    stopx = (*info).stopx
    stopy = (*info).stopy
  endelse
  tv, bytscl(img)
;  tv, (*info).image, (*info).startx, (*info).starty

;         device,copy=[0,0,(*info).d_xsz,(*info).d_ysz,0,0,(*info).pixid]
;         plots,[sx,sx,dx,dx,sx],[sy,dy,dy,sy,sy],/device, $
;            color=!p.color
;         device,copy=[0,0,(*info).d_xsz,(*info).d_ysz,0,0,(*info).pixid]
  plots, [startx, startx, stopx, stopx, startx], $
         [starty, stopy, stopy, starty, starty], $
         color = !p.color, /device
;  wdelete, (*info).pixid
  wset, (*info).wid

end

pro iris_ximovie_framespeed, event
  widget_control, event.top,get_uvalue=info
  (*info).frame_speed=event.value
;  pseudoevent={widget_button,id:(*info).action, $
;    top:event.top, handler:0l, select:1}
;  widget_control,event.top,set_uvalue=info
;  iris_ximovie_draw,pseudoevent
end

pro iris_ximovie_frameincr, event
  widget_control, event.top, get_uvalue = info
  (*info).frame_incr = event.value
  return
end

pro iris_ximovie_incr, event
  widget_control, event.top, get_uvalue = info
  if(*info).fr_incr then begin
    (*info).frame_nr = (*info).frame_nr + (*info).frame_incr
    if (*info).frame_nr gt (*info).last then $
      (*info).frame_nr = (*info).frame_nr - (*info).last - 1
  endif
  pseudoevent={widget_button,id:(*info).action, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  iris_ximovie_draw,pseudoevent
  return
end

pro iris_ximovie_decr, event
  widget_control, event.top, get_uvalue = info
  if(*info).fr_incr then begin
    (*info).frame_nr = (*info).frame_nr - (*info).frame_incr
    if (*info).frame_nr lt (*info).first then $
      (*info).frame_nr = (*info).last
  endif
  pseudoevent={widget_button,id:(*info).action, $
    top:event.top, handler:0l, select:1}
  widget_control,event.top,set_uvalue=info
  iris_ximovie_draw,pseudoevent
  return
end

pro iris_ximovie_reset_key_frame,event
  forward_function iris_ximovie_gamma
  widget_control, event.top, get_uvalue = info
  if (*info).twostream then begin
    if (*info).stream1 then begin
      (*info).key_frame=(*info).frame_nr
      pseudoevent={widget_slider,id:(*info).action, $
        top:event.top, handler:0l, value:(*info).gamma, drag:1}
    endif else begin
      (*info).key_frame2=(*info).frame_nr
      pseudoevent={widget_slider,id:(*info).action, $
      top:event.top, handler:0l, value:(*info).gamma2, drag:1}
   endelse
  endif else begin
    (*info).key_frame=(*info).frame_nr
    pseudoevent={widget_slider,id:(*info).action, $
      top:event.top, handler:0l, value:(*info).gamma, drag:1}
  endelse  
  dum=iris_ximovie_gamma(pseudoevent)
  return
end

function iris_ximovie_gamma, event
  widget_control, event.top,get_uvalue=info
  bscale=(*info).scaling1[0]
  bzero=(*info).scaling1[1]
  bscale2=(*info).scaling2[0]
  bzero2=(*info).scaling2[1]
  if ((*info).stream1) then begin
    (*info).gamma=event.value
    image=bscale*((*(*info).aimg)[(*info).key_frame])+bzero
    (*info).imin=min(iris_histo_opt(image, $
                            (*info).histo_lim,missing=(*info).missing)>0.)^(*info).gamma
    (*info).imax=max(iris_histo_opt(image, $
                            (*info).histo_lim,missing=(*info).missing)>0.)^(*info).gamma
    pseudoevent={widget_button,id:(*info).action, $
      top:event.top, handler:0l, select:1}
    widget_control,event.top,set_uvalue=info
    iris_ximovie_draw,pseudoevent
  endif 
  if ((*info).stream2) then begin
    (*info).gamma2=event.value
    image2=bscale*((*(*info).aimg2)[(*info).key_frame2])+bzero
    (*info).i2min=min(iris_histo_opt(image2, $
                            (*info).histo_lim2,missing=(*info).missing)>0.)^(*info).gamma2
    (*info).i2max=max(iris_histo_opt(image2, $
                            (*info).histo_lim2,missing=(*info).missing)>0.)^(*info).gamma2
    pseudoevent={widget_button,id:(*info).action, $
      top:event.top, handler:0l, select:1}
    widget_control,event.top,set_uvalue=info
    iris_ximovie_draw,pseudoevent
   endif
   return,0
end

function iris_ximovie_histoopt, event
  widget_control, event.top,get_uvalue=info
  bscale=(*info).scaling1[0]
  bzero=(*info).scaling1[1]
  bscale2=(*info).scaling2[0]
  bzero2=(*info).scaling2[1]
  if ((*info).stream1) then begin
    (*info).histo_lim=10.0^(event.value)
    image=bscale*((*(*info).aimg)[(*info).key_frame])+bzero
    (*info).imin=min(iris_histo_opt(image, $
                            (*info).histo_lim,missing=(*info).missing)>0.)^(*info).gamma
    (*info).imax=max(iris_histo_opt(image, $
                            (*info).histo_lim,missing=(*info).missing)>0.)^(*info).gamma
    pseudoevent={widget_button,id:(*info).action, $
      top:event.top, handler:0l, select:1}
    widget_control,event.top,set_uvalue=info
    iris_ximovie_draw,pseudoevent
  endif 
  if ((*info).stream2) then begin
    (*info).histo_lim2=10.0^(event.value)
    image2=bscale*((*(*info).aimg2)[(*info).key_frame2])+bzero
    (*info).i2min=min(iris_histo_opt(image2, $
                            (*info).histo_lim2,missing=(*info).missing)>0.)^(*info).gamma2
    (*info).i2max=max(iris_histo_opt(image2, $
                            (*info).histo_lim2,missing=(*info).missing)>0.)^(*info).gamma2
    pseudoevent={widget_button,id:(*info).action, $
      top:event.top, handler:0l, select:1}
    widget_control,event.top,set_uvalue=info
    iris_ximovie_draw,pseudoevent
  endif
  return,0
end


; play in forward mode
pro iris_ximovie_play_fwd, event
  widget_control, event.top, get_uvalue = info
  (*info).run = 1
  (*info).playmode = 'forward'
  widget_control, (*info).play_fwd_button, set_value = (*info).bmp_buttons.play_blk 
  widget_control, (*info).play_rev_button, set_value = (*info).bmp_buttons.play_rev 
  widget_control, (*info).pause_button, set_value = (*info).bmp_buttons.pause 
  widget_control, (*info).play_cycle_button, set_value = (*info).bmp_buttons.cycle 
  widget_control, (*info).bg_base, timer = 0.0
  return
end

; play in reverse mode
pro iris_ximovie_play_rev, event
  widget_control, event.top, get_uvalue = info
  (*info).run = 1
  (*info).playmode = 'reverse'
  widget_control, (*info).play_fwd_button, set_value = (*info).bmp_buttons.play 
  widget_control, (*info).play_rev_button, set_value = (*info).bmp_buttons.play_rev_blk 
  widget_control, (*info).pause_button, set_value = (*info).bmp_buttons.pause 
  widget_control, (*info).play_cycle_button, set_value = (*info).bmp_buttons.cycle 
  widget_control, (*info).bg_base, timer = 0.0
  return
end

; play in cycle mode
pro iris_ximovie_play_cycle, event
  widget_control, event.top, get_uvalue = info
  (*info).run = 1
  (*info).playmode = 'cycle'
  widget_control, (*info).play_fwd_button, set_value = (*info).bmp_buttons.play 
  widget_control, (*info).play_rev_button, set_value = (*info).bmp_buttons.play_rev 
  widget_control, (*info).pause_button, set_value = (*info).bmp_buttons.pause 
  widget_control, (*info).play_cycle_button, set_value = (*info).bmp_buttons.cycle_blk 
  widget_control, (*info).bg_base, timer = 0.0
  return
end

pro iris_ximovie_pause, event
  widget_control, event.top, get_uvalue = info
  (*info).run = 0
  (*info).playmode = 'pause'
  widget_control, (*info).play_fwd_button, set_value = (*info).bmp_buttons.play 
  widget_control, (*info).play_rev_button, set_value = (*info).bmp_buttons.play_rev 
  widget_control, (*info).pause_button, set_value = (*info).bmp_buttons.pause_blk 
  widget_control, (*info).play_cycle_button, set_value = (*info).bmp_buttons.cycle
  return
end

; start blink option
pro iris_ximovie_blink_start, event
  widget_control, event.top, get_uvalue = info
  (*info).blink = 1
  widget_control, (*info).bg_base2, timer = 0.0
end

; stop blinking
pro iris_ximovie_blink_stop, event
  widget_control, event.top, get_uvalue = info
  (*info).blink = 0
end

;set stream 1
pro iris_ximovie_stream1, event
  widget_control, event.top, get_uvalue = info
  (*info).stream1 = 1
  (*info).stream2 = 0
  widget_control, (*info).gamma_slider,set_value=(*info).gamma
  widget_control, (*info).histoopt_slider,set_value=alog10((*info).histo_lim)
  return
end
;set stream2 
pro iris_ximovie_stream2, event
  widget_control, event.top, get_uvalue = info
  (*info).stream1 = 0
  (*info).stream2 = 1
  widget_control, (*info).gamma_slider,set_value=(*info).gamma2
  widget_control, (*info).histoopt_slider,set_value=alog10((*info).histo_lim2)
  return
end
;set blink
pro iris_ximovie_mix, event
  widget_control, event.top, get_uvalue = info
  (*info).stream1 = 0
  (*info).stream2 = 0
  return
end

; restrict frame range
pro iris_ximovie_setframes, event
  widget_control, event.top, get_uvalue = info

  setframes_tlb = widget_base(title = 'Select Frames', group_leader = (*info).tlb,/row)
  (*info).start_frame_id = cw_field(setframes_tlb, title = 'Set start frame #', /integer, $
                         /column, value = 0)
  (*info).stop_frame_id = cw_field(setframes_tlb, title = 'Set stop frame #', /integer, $
                        /column, value = (*info).nframes-1)
  closefield = widget_base(setframes_tlb,/column)
  closebutton = widget_button(closefield, value = 'OK', event_pro = 'iris_ximovie_setframes_destroy')

  widget_control, setframes_tlb, set_uvalue = info
  widget_control, setframes_tlb, /realize
  xmanager, 'Set Frame Range', setframes_tlb, $
             /no_block, group_leader = (*info).tlb

end

; destroy setframes widget
pro iris_ximovie_setframes_destroy, event
  widget_control, event.top, get_uvalue = info
  widget_control, (*info).start_frame_id, get_value = first

  if (first lt 0) then begin
    ok = dialog_message('Can`t start at negativ frame value. Using 0', /information)
    first = 0
  endif

  if first gt ((*info).nframes - 1) then begin
    msg = 'Start frame must be lt ' +strtrim(string((*info).nframes-1), 2)
    ok = dialog_message(msg, /information)
    first = (*info).nframes - 2
  endif

  (*info).first = first
  (*info).frame_nr = (*info).first

  widget_control, (*info).stop_frame_id, get_value = last
  if (last gt (*info).nframes) then begin
    msg = 'Max can not be more than number of frames ( ' +strtrim(string((*info).nframes-1), 2)+')'
    ok = dialog_message(msg, /information)
    last = (*info).nframes-1
  endif

  if (last le first) then begin
    msg = 'Stop frame must be gt start frame. Using ' +strtrim(string(first), 2)
    ok = dialog_message(msg, /information)
    last = (*info).nframes-1
  endif

  (*info).last = last < (*info).nframes

  widget_control, (*info).frameslider, set_slider_min = (*info).first
  widget_control, (*info).frameslider, set_slider_max = (*info).last
  widget_control, event.top,/destroy
end

; select color table
pro iris_ximovie_colors, event
  widget_control, event.top, get_uvalue=info
  thisevent = tag_names(event, /structure_name)
  case thisevent of
  'WIDGET_BUTTON': begin
      xcolors, ncolors = (*info).ncolors, bottom = (*info).bottom, $
        title = 'xwhisker colors (' + strtrim((*info).wid, 2) + ')', $
        group_leader = event.top, notifyid = [event.id, event.top]
      endcase
  'XCOLORS_LOAD': begin
      (*info).r = event.r((*info).bottom:(*info).ncolors-1 + (*info).bottom)
      (*info).g = event.g((*info).bottom:(*info).ncolors-1 + (*info).bottom)
      (*info).b = event.b((*info).bottom:(*info).ncolors-1 + (*info).bottom)
      if !d.n_colors gt 256 then begin
        pseudoevent={widget_button,id:0L, $
          top:event.top, handler:0l, select:1}
        iris_ximovie_draw, pseudoevent
      endif
    endcase
  endcase
  widget_control, event.top, set_uvalue = info
end

pro iris_ximovie_blink, event
  widget_control, event.top, get_uvalue = info
  ; stop blink if pauses and if movie runs
  if(*info).run eq 1 or (*info).blink eq 0 then return  
  bscale=(*info).scaling1[0]
  bzero=(*info).scaling1[1]
  bscale2=(*info).scaling2[0]
  bzero2=(*info).scaling2[1]
  if (*info).twostream then begin
    image1 = (bscale*((*(*info).aimg)[(*info).frame_nr])+bzero)^(*info).gamma
    image2 = (bscale2*((*(*info).aimg2)[(*info).frame_nr])+bzero2)^(*info).gamma2
    imin = (*info).imin
    imax = (*info).imax
    i2min = (*info).i2min
    i2max = (*info).i2max
    if (*info).imean ne 0 then begin
      imean = mean(image1)
      image1 = image1*(*info).imean/imean
    endif
    if (*info).i2mean ne 0 then begin
      i2mean = mean(image2)
      image2 = image2*(*info).i2mean/i2mean
    endif
    if not (*info).byt then begin
      image1 = bytscl(image1, min = imin, max = imax)
      image2 = bytscl(image2, min = i2min, max = i2max)
    endif
    frame1 = (*info).frame_nr
    frame2 = frame1
  endif else begin
    frame1 = (*info).frame_nr
    frame2 = (*info).frame_nr + (*info).frame_incr
    if frame2 gt (*info).last then frame2 = frame2 - (*info).last
    image1 = (bscale*((*(*info).aimg)[frame1])+bzero)^(*info).gamma
    image2 = (bscale*((*(*info).aimg)[frame2])+bzero)^(*info).gamma
    imin = (*info).imin
    imax = (*info).imax
    if (*info).imean ne 0 then begin
      imean = mean(image1)
      image1 = image1*(*info).imean/imean
      imean2 = mean(image2)
      image2 = image2*(*info).imean/imean2
    endif
    if not(*info).byt then begin
      image1 = bytscl(image1, min = imin, max = imax)
      image2 = bytscl(image2, min = imin, max = imax)
    endif
  endelse
  ; determine which of the images to plot:
  if((*info).im1)then begin
    (*info).im1 = 0
    (*info).im2 = 1
    image = image1
    frame = frame1
  endif else begin
    (*info).im1 = 1
    (*info).im2 = 0
    image = image2
    frame = frame2
  endelse
  image = image[(*info).startx:(*info).stopx, (*info).starty:(*info).stopy]
;  (*info).image = image
  ; display image
  if (*info).mag eq 1.0 then begin
    tv, image
  endif else begin
    case (*info).use_rebin of
      1: tv, rebin(image, (*info).d_xsz, (*info).d_ysz)
      0: tv, congrid(image, (*info).d_xsz, (*info).d_ysz)
    endcase
  endelse

  ; update frame nr. slider
  widget_control, (*info).frameslider, set_value = frame

  ; keep blinking
  widget_control,(*info).bg_base2, timer = 1./(*info).frame_speed
  return
end

pro iris_ximovie_draw, event
  widget_control, event.top, get_uvalue = info
  if (*info).run eq 1 then return    ; movie must be paused
  if !d.name ne 'PS' then  wset, (*info).wid

  (*info).fr_incr = 1
  ; set frame number
;  (*info).frame_nr = (*info).frame_nr + (*info).frame_incr
;  if (*info).frame_nr gt (*info).last then (*info).frame_nr = (*info).frame_nr - (*info).last - 1 

  bscale=(*info).scaling1[0]
  bzero=(*info).scaling1[1]
  bscale2=(*info).scaling2[0]
  bzero2=(*info).scaling2[1]

  if((*info).twostream) then begin
    ; display only images from stream 1, only from stream2 or from both
    if ((*info).stream1) then begin
      image = (bscale*((*(*info).aimg)[(*info).frame_nr])+bzero)^(*info).gamma
      imin = (*info).imin
      imax = (*info).imax
      if (*info).imean ne 0 then image = image*(*info).imean/mean(image)
      widget_control, (*info).gamma_slider,set_value=(*info).gamma
      widget_control, (*info).histoopt_slider,set_value=alog10((*info).histo_lim)
    endif else if ((*info).stream2) then begin
      image = (bscale2*((*(*info).aimg2)[(*info).frame_nr])+bzero2)^(*info).gamma2
      imin = (*info).i2min
      imax = (*info).i2max
      if (*info).i2mean ne 0 then image = image*(*info).i2mean/mean(image)
      widget_control, (*info).gamma_slider,set_value=(*info).gamma2
      widget_control, (*info).histoopt_slider,set_value=alog10((*info).histo_lim2)
    endif else begin   ; this is the "blink" option
      if((*info).mix)then begin
        (*info).mix = 0
        (*info).fr_incr = 0
        image = (bscale*((*(*info).aimg)[(*info).frame_nr])+bzero)^(*info).gamma
        imin = (*info).imin
        imax = (*info).imax
      if (*info).imean ne 0 then image = image*(*info).imean/mean(image)
      endif else begin
        (*info).mix = 1
        image = (bscale2*((*(*info).aimg2)[(*info).frame_nr])+bzero2)^(*info).gamma2
        imin = (*info).i2min
        imax = (*info).i2max
      if (*info).i2mean ne 0 then image = image*(*info).i2mean/mean(image)
      endelse
    endelse
  endif else begin
    image = (bscale*((*(*info).aimg)[(*info).frame_nr])+bzero)^(*info).gamma
    imin = (*info).imin
    imax = (*info).imax
    if (*info).imean ne 0 then image = image*(*info).imean/mean(image)
  endelse

  if (*info).frameslider ne -1 then widget_control, (*info).frameslider, set_value = (*info).frame_nr
  image = (image[(*info).startx:(*info).stopx, (*info).starty:(*info).stopy])
  if ptr_valid((*info).image) then ptr_free,(*info).image
  (*info).image = ptr_new(image)
  ; bytscl image if it's not a byte array
  if not (*info).byt then $
    *(*info).image = bytscl(*(*info).image, min = imin, max = imax)
  ; display image
  if (*info).mag eq 1.0 then begin
    tv, *(*info).image
  endif else begin
    case (*info).use_rebin of
      1: tv, rebin(*(*info).image, (*info).d_xsz, (*info).d_ysz)
      0: tv, congrid(*(*info).image, (*info).d_xsz, (*info).d_ysz)
    endcase
  endelse
;
  if (*info).timetext ne -1 then begin
    widget_control,(*info).timetext,set_value=((*info).time)[(*info).frame_nr]
    if (*info).clock then begin
      ch_air=1.
      xmargin_normal=!d.x_ch_size/float(!d.x_size)*ch_air
      ymargin_normal=!d.y_ch_size/float(!d.y_size)*ch_air
      clock_size=0.07
      clock,anytim2utc(((*info).time)[(*info).frame_nr],/ccsds,/time_only,/truncate), $
        pos=[xmargin_normal,ymargin_normal],size=clock_size
    endif
  endif

  return
end

pro iris_ximovie_bck, event
  widget_control, event.top, get_uvalue = info
  if (*info).run eq 0 then return    ; movie has been paused
  if !d.name ne 'PS' then  wset, (*info).wid
  
  (*info).fr_incr = 1
  ; set frame number
  case (*info).playmode of 'forward': begin
      (*info).frame_nr = (*info).frame_nr + (*info).frame_incr
      if (*info).frame_nr gt (*info).last then $
       (*info).frame_nr = ((*info).frame_nr - (*info).last - 1) > (*info).first
    end
    'reverse': begin
      (*info).frame_nr = (*info).frame_nr - (*info).frame_incr
      if (*info).frame_nr lt (*info).first then $
        (*info).frame_nr = ((*info).frame_nr + (*info).last + 1) <  (*info).last
    end
    'cycle': begin     
      (*info).frame_nr = (*info).frame_nr + (*info).cycle*(*info).frame_incr
      if (*info).frame_nr gt (*info).last then begin
        (*info).cycle = -1
        (*info).frame_nr = (*info).frame_nr + (*info).cycle*(*info).frame_incr
      endif
      if (*info).frame_nr lt (*info).first then begin
        (*info).cycle = 1
        (*info).frame_nr = (*info).frame_nr + (*info).cycle*(*info).frame_incr
      endif
    end
  endcase
        
  bscale=(*info).scaling1[0]
  bzero=(*info).scaling1[1]
  bscale2=(*info).scaling2[0]
  bzero2=(*info).scaling2[1]

  if((*info).twostream) then begin
    ; display only images from stream 1, only from stream2 or from both
    if ((*info).stream1) then begin
      image = (bscale*((*(*info).aimg)[(*info).frame_nr])+bzero)^(*info).gamma
      imin = (*info).imin
      imax = (*info).imax
      if (*info).imean ne 0 then image = image*(*info).imean/mean(image)
      widget_control, (*info).gamma_slider,set_value=(*info).gamma
      widget_control, (*info).histoopt_slider,set_value=alog10((*info).histo_lim)
    endif else if ((*info).stream2) then begin
      image = (bscale2*((*(*info).aimg2)[(*info).frame_nr])+bzero2)^(*info).gamma2
      imin = (*info).i2min
      imax = (*info).i2max
      if (*info).i2mean ne 0 then image = image*(*info).i2mean/mean(image)
      widget_control, (*info).gamma_slider,set_value=(*info).gamma2
      widget_control, (*info).histoopt_slider,set_value=alog10((*info).histo_lim2)
    endif else begin   ; this is the "blink" option
      if ((*info).mix) then begin
        (*info).mix = 0
        (*info).fr_incr = 0
        image = ((*(*info).aimg)[(*info).frame_nr])^(*info).gamma
        imin = (*info).imin
        imax = (*info).imax
      if (*info).imean ne 0 then image = image*(*info).imean/mean(image)
      endif else begin
        (*info).mix = 1
        image = ((*(*info).aimg2)[(*info).frame_nr])^(*info).gamma2
        imin = (*info).i2min
        imax = (*info).i2max
      if (*info).i2mean ne 0 then image = image*(*info).i2mean/mean(image)
      endelse
    endelse
  endif else begin
    image = (bscale*((*(*info).aimg)[(*info).frame_nr])+bzero)^(*info).gamma
    imin = (*info).imin
    imax = (*info).imax
    if (*info).imean ne 0 then image = image*(*info).imean/mean(image)
  endelse
  *(*info).image = (image[(*info).startx:(*info).stopx, (*info).starty:(*info).stopy])
  ; if display window is differetn from image size:
  ; bytscl image if it's not a byte array
  if not (*info).byt then $
    *(*info).image = bytscl(*(*info).image, min = imin, max = imax)
  ; display image
  if (*info).mag eq 1.0 then begin
    tv, (*(*info).image)
  endif else begin
  case (*info).use_rebin of
    1: tv, (rebin(*(*info).image, (*info).d_xsz, (*info).d_ysz))
    0: tv, (congrid(*(*info).image, (*info).d_xsz, (*info).d_ysz))
  endcase

;    if (*info).use_rebin then begin
;      tv, rebin(*(*info).image, (*info).d_xsz, (*info).d_ysz)
;    endif else begin
;      tv, congrid(*(*info).image, (*info).d_xsz, (*info).d_ysz)
;    endelse
  endelse
 
  ; update slider:
  widget_control, (*info).frameslider, set_value = (*info).frame_nr
;
  if (*info).timetext ne -1 then begin
    widget_control,(*info).timetext,set_value=((*info).time)[(*info).frame_nr]
    if (*info).clock then begin
      ch_air=1.
      xmargin_normal=!d.x_ch_size/float(!d.x_size)*ch_air
      ymargin_normal=!d.y_ch_size/float(!d.y_size)*ch_air
      clock_size=0.07
      clock,anytim2utc(((*info).time)[(*info).frame_nr],/ccsds,/time_only,/truncate), $
        pos=[xmargin_normal,ymargin_normal],size=clock_size
    endif
  endif

  ;  generate timer event
  widget_control,(*info).bg_base, timer = 1./(*info).frame_speed
end  

pro iris_ximovie_zoom, event
  widget_control,event.top,get_uvalue=info
  wset, (*info).wid
  if event.type gt 2 then return
  events=['down','up','motion']
  thisevent=events[event.type]
  if(*info).run eq 1 then return
  window, /pixmap, /free, xsize = (*info).d_xsz, ysize = (*info).d_ysz
  if (*info).mag eq 1.0 then begin
    tv, *(*info).image
  endif else begin
    case (*info).use_rebin of
      1: tv, rebin(*(*info).image, (*info).d_xsz, (*info).d_ysz)
      0: tv, congrid(*(*info).image, (*info).d_xsz, (*info).d_ysz)
    endcase
           ;/erase, /nointerp
  endelse
  (*info).pixid = !d.window

  case thisevent of
    'down': begin
    ;  turn motion events on2
    ;  set static corner
    widget_control,(*info).drawid,draw_motion_events=1
    (*info).sx=event.x
    (*info).sy=event.y
  endcase
    'up': begin
      ;  erase last box
      ;  turn motion events off
       device,copy=[0,0,(*info).d_xsz,(*info).d_ysz,0,0, $
       (*info).pixid]
       widget_control,(*info).drawid,draw_motion_events=0
       sx=(*info).sx
       sy=(*info).sy
       dx=event.x
       dy=event.y
       sx = (sx < (*info).d_xsz - 1) > 0
       sy = (sy < (*info).d_ysz - 1) > 0
       dx = (dx < (*info).d_xsz - 1) > 0
       dy = (dy < (*info).d_ysz - 1) > 0
       image = *(*info).image
;       image=image[sx<dx:sx>dx,sy<dy:sy>dy]
       xscale = *(*info).xscale
       yscale = *(*info).yscale
       xscale = xscale[sx<dx:sx>dx]
       yscale = yscale[sy<dy:sy>dy]
       nx = n_elements(xscale)-1
       ny = n_elements(yscale)-1
       image = image[xscale[0]:xscale[nx], yscale[0]:yscale[ny]]
       sz=size(image)
       mind = min(sz[0:2])
       startx = (*info).startx
       starty = (*info).starty
       pos = [xscale[0]+startx, xscale[nx]+startx, yscale[0]+starty, yscale[ny]+starty]
       ;pos=[sx<dx,sx>dx,sy<dy,sy>dy]
      if (*info).imean ne 0 then inorm = 1 else i2norm = 0
      if (*info).i2mean ne 0 then i2norm = 1 else i2norm = 0
       if mind ge 2 then begin
        if((*info).twostream)then begin
          if (*info).flt then begin
             iris_ximovie, (*info).afile, (*info).x_org_size, (*info).y_org_size,  $
                      offset = (*info).offset, $
                      afile2 = (*info).afile2, pos = pos, /float, start_im = (*info).frame_nr, $
                      group_leader = event.top, imin = (*info).imin, imax = (*info).imax, $
                      i2min = (*info).i2min, i2max = (*info).i2max, inorm = inorm, $
                      i2norm = i2norm,nframes=(*info).nframes, $
                      topwid = (*info).wid, swap_endian = (*info).swap_endian, $
                      scaling1=(*info).scaling1,scaling2=(*info).scaling2, $
                      time=(*info).time,clock=(*info).clock
           endif else if (*info).int then begin
             iris_ximovie, (*info).afile, (*info).x_org_size, (*info).y_org_size, $
                      offset = (*info).offset, $
                      afile2 = (*info).afile2, pos = pos, /int, start_im = (*info).frame_nr, $
                      group_leader = event.top, imin = (*info).imin, imax = (*info).imax, $
                      i2min = (*info).i2min, i2max = (*info).i2max, inorm = inorm, $
                      i2norm = i2norm,nframes=(*info).nframes, $
                      topwid = (*info).wid, swap_endian = (*info).swap_endian, $
                      scaling1=(*info).scaling1,scaling2=(*info).scaling2, $
                      time=(*info).time,clock=(*info).clock
           endif else begin 
             iris_ximovie, (*info).afile, (*info).x_org_size, (*info).y_org_size, $
                      offset = (*info).offset, $
                      afile2 = (*info).afile2, pos = pos, start_im = (*info).frame_nr, $
                      group_leader = event.top, imin = (*info).imin, imax = (*info).imax, $
                      i2min = (*info).i2min, i2max = (*info).i2max, inorm = inorm, $
                      i2norm = i2norm,nframes=(*info).nframes, $
                      topwid = (*info).wid, $
                      scaling1=(*info).scaling1,scaling2=(*info).scaling2, $
                      time=(*info).time,clock=(*info).clock
           endelse
       endif else begin
         if (*info).flt then begin
           iris_ximovie, (*info).afile, (*info).x_org_size, (*info).y_org_size, $
                    offset = (*info).offset, $ 
                    pos = pos, /float, topwid = (*info).wid, $
                    group_leader = event.top, imin = (*info).imin, imax = (*info).imax, $
                    inorm = inorm, i2norm = i2norm, start_im = (*info).frame_nr, $
                    swap_endian = (*info).swap_endian,nframes=(*info).nframes, $
                    scaling1=(*info).scaling1,scaling2=(*info).scaling2, $
                      time=(*info).time,clock=(*info).clock
         endif else if (*info).int then begin
           iris_ximovie, (*info).afile, (*info).x_org_size, (*info).y_org_size, $
                    offset = (*info).offset, $
                    pos = pos, /int, topwid = (*info).wid, $
                    group_leader = event.top, imin = (*info).imin, imax = (*info).imax, $
                    inorm = inorm, i2norm = i2norm, start_im = (*info).frame_nr, $
                    swap_endian = (*info).swap_endian,nframes=(*info).nframes, $
                    scaling1=(*info).scaling1,scaling2=(*info).scaling2, $
                      time=(*info).time,clock=(*info).clock
         endif else begin 
           iris_ximovie, (*info).afile, (*info).x_org_size, (*info).y_org_size, pos = pos, $
                    offset = (*info).offset, topwid = (*info).wid, $
                    group_leader = event.top, imin = (*info).imin, imax = (*info).imax, $
                    inorm = inorm, i2norm = i2norm, start_im = (*info).frame_nr, $
                    nframes=(*info).nframes, $
                    scaling1=(*info).scaling1,scaling2=(*info).scaling2, $
                      time=(*info).time,clock=(*info).clock
         endelse
       endelse
     endif
   endcase
       'motion':  begin
       ;  erase previous box
       ;  draw new box
         dx=event.x
         dy=event.y
         sx=(*info).sx
         sy=(*info).sy
         wset,(*info).wid
         device,copy=[0,0,(*info).d_xsz,(*info).d_ysz,0,0,(*info).pixid]
         plots,[sx,sx,dx,dx,sx],[sy,dy,dy,sy,sy],/device, $
            color=!p.color
       endcase
    endcase
  wdelete, (*info).pixid
end

;resize main window
pro iris_ximovie_resize, event
  widget_control, event.top ,get_uvalue = info 
  (*info).mag = (*info).mag*event.x/(*info).tlb_xsz
  frac = float(1./(*info).mag) - fix(1./(*info).mag)
  if frac ne 0 then (*info).use_rebin = 0
  if (*info).keep_aspect ne 0 then begin
    (*info).d_xsz = event.x - (*info).menu_xsz > 1
    (*info).d_ysz = (*info).d_xsz*(*info).aspect
  endif else begin
    (*info).d_xsz = event.x - (*info).menu_xsz > 1 
    (*info).d_ysz = event.y - (*info).menu_ysz > 1
  endelse
  ptr_free, (*info).xscale
  ptr_free, (*info).yscale
  (*info).xscale = ptr_new((*info).d_xsz)
  (*info).yscale = ptr_new((*info).d_ysz)
  *(*info).xscale = interpol(indgen((*info).xsize), (*info).d_xsz)
  *(*info).yscale = interpol(indgen((*info).ysize), (*info).d_ysz)
  widget_control, (*info).drawid, draw_xsize = (*info).d_xsz, $ 
                   draw_ysize = (*info).d_ysz, xsize = (*info).d_xsz, $ 
                   ysize = (*info).d_ysz 
  ; create pseudoevent and send this event to xdisplay_draw, 
  ; in order to draw the image
  pseudoevent={widget_button,id:(*info).action, $
               top:(*info).tlb, handler:0l, select:1}
  iris_ximovie_draw, pseudoevent
end 

; to keep or not to keep aspect ratio when resizing:
pro iris_ximovie_aspect, event
  widget_control, event.top, get_uvalue = info
  (*info).keep_aspect = event.select
end

pro iris_ximovie_destroy, event
    widget_control, event.top, /destroy
end

pro iris_ximovie_cleanup, tlb
  widget_control, tlb, get_uvalue = info
  wdelete, (*info).mainpixid
  free_lun, (*info).alu
  if((*info).twostream) then free_lun, (*info).alu2
  if (*info).fdelete then begin
    file_delete, (*info).afile
    if (*info).afile2 ne 0 then file_delete, (*info).afile2
  endif
  if ptr_valid((*info).aimg) then ptr_free, (*info).aimg
  if ptr_valid((*info).aimg2) then ptr_free, (*info).aimg2
  if ptr_valid((*info).image) then ptr_free, (*info).image
  ptr_free, (*info).xscale
  ptr_free, (*info).yscale
  ptr_free, info
end

pro iris_ximovie, afile, xsize, ysize, afile2 = afile2, offset = offset, pos = pos, $
             nframes = nframes, topwid = topwid, title = title, $
             scaling1=scaling1, scaling2=scaling2, $
             first_im = first_im, last_im = last_im, start_im = start_im, $
             imin = imin, imax = imax, inorm = inorm, $
             i2min = i2min, i2max = i2max, i2norm = i2norm, float = float, int = int, double = double, $
             magnification = magnification, aspect = aspect, group_leader = group_leader, $
             ncolors = ncolors, fdelete = fdelete,  swap_endian = swap_endian, $
             time=time, clock=clock, missing=missing, type=type

  if n_params() lt 1 then begin
    print, 'iris_ximovie, file, xsize, ysize, afile2=afile2, offset=offset, pos=pos, $'
    print,  'nframes = nframes, first_im=first_im, title=title,scaling=scaling, $'
    print, 'last_im=last_im, imin = imin, imax = imax, inorm=inorm, i2norm=i2norm,float = float, $'
    print, 'int = int, magnification=magnification, aspect=aspect,group_leader = group_leaderl,ncolors=ncolors,$'
    print, 'swap_endian=swap_endian, time=time,clock=clock,missing=missing,type=type'
    return
  endif

  if n_elements(title) eq 0 then title = 'Ximovie: Image movie tool' else title='Ximovie: '+title
  if n_elements(magnification) eq 0 then mag = 1.0 else mag = magnification
  if n_elements(topwid) eq 0 then topwid = 0
  if keyword_set(first_im) then first = first_im else first = 0
  if n_elements(start_im) eq 0 then start_im = first
  if keyword_set(fdelete) then fdelete = 1 else fdelete = 0
  if n_elements(ncolors) eq 0 then ncolors = !d.table_size  ; used to be (!d.n_colors < 256)
  if n_elements(afile2) ne 0 then twostream = 1 else twostream = 0
  if n_elements(offset) eq 0 then offset = 0
  if keyword_set(swap_endian) then swap_endian = 1 else swap_endian = 0
  if n_elements(missing) eq 0 then missing=-9999999.
  if n_elements(scaling1) eq 0 then scaling1=[1.,0.]
  if n_elements(scaling2) eq 0 then scaling2=[1.,0.]

  ; open assoc file
  openr, alu, afile, /get_lun, swap_endian = swap_endian
  if(twostream) then openr, alu2, afile2, /get_lun, swap_endian = swap_endian else alu2 = 0
  status = fstat(alu)
 
  if n_elements(pos) eq 0 then begin
    startx = 0
    starty = 0
    stopx = xsize-1
    stopy = ysize-1
    subimage = 0
  endif else begin
    startx = fix(pos[0])
    stopx = fix(pos[1])
    starty = fix(pos[2])
    stopy = fix(pos[3])
    subimage = 1
  endelse


  ; initialize first image and plot image scale 
  byt = 0
  flt = 0
  integ = 0
  dbl = 0
  imean = 0
  i2mean = 0
  if n_elements(type) eq 0 then begin
    type = 'byte'
    if keyword_set(double) then type =  'double' 
    if keyword_set(float) then type = 'float'
    if keyword_set(int) then type = 'int'
    if keyword_set(byte) then type =  'byte'
  endif
  case type of
   'double' : begin
    dbl = 1
    aimage = assoc(alu, dblarr(xsize, ysize), offset)
    if(twostream) then aimage2 = assoc(alu2, dblarr(xsize, ysize), offset)
    if n_elements(nframes) eq 0 then nframes = status.size/xsize/ysize/8
    if keyword_set(last_im) then last = last_im else last = nframes-1
    image = aimage[0]
    image = image[startx:stopx, starty:stopy]
    if keyword_set(imin)then imin = imin else imin = min(image)
    if keyword_set(imax)then imax = imax else imax = max(image)
    if keyword_set(inorm) then imean = mean(image)
;    image = bytscl(image, min = imin, max = imax)
    end 
   'float' : begin
    flt = 1
    aimage = assoc(alu, fltarr(xsize, ysize), offset)
    if(twostream) then aimage2 = assoc(alu2, fltarr(xsize, ysize), offset)
    if n_elements(nframes) eq 0 then nframes = status.size/xsize/ysize/4
    if keyword_set(last_im) then last = last_im else last = nframes-1
    image = aimage[0]
    image = image[startx:stopx, starty:stopy]
    if keyword_set(imin)then imin = imin else imin = min(image)
    if keyword_set(imax)then imax = imax else imax = max(image)
    if keyword_set(inorm) then imean = mean(image)
;    image = bytscl(image, min = imin, max = imax)
    end 
    'int': begin
    integ = 1
    aimage = assoc(alu, intarr(xsize, ysize), offset)
    if(twostream) then aimage2 = assoc(alu2, intarr(xsize, ysize), offset)
    if n_elements(nframes) eq 0 then nframes = status.size/xsize/ysize/2
    if keyword_set(last_im) then last = last_im else last = nframes-1
    image = aimage[0]
    image = image[startx:stopx, starty:stopy]
    if keyword_set(imin)then imin = imin else imin = min(image)
    if keyword_set(imax)then imax = imax else imax = max(image)
    if keyword_set(inorm) then imean = mean(image)
;    image = bytscl(image, min = imin, max = imax)
    end 
    'byte': begin
    byt = 1
    aimage = assoc(alu, bytarr(xsize, ysize), offset)
    if(twostream) then aimage2 = assoc(alu2, bytarr(xsize, ysize), offset)
    if n_elements(nframes) eq 0 then nframes = status.size/xsize/ysize
    if keyword_set(last_im) then last = last_im else last = nframes-1
    imin = 0
    imax = 255
    image = aimage[0]
    image = image[startx:stopx, starty:stopy]
    end
  endcase
  image=scaling1[0]*image+scaling1[1]
  aimg = ptr_new(aimage)
  if(twostream) then begin
    image2 = aimage2[0]
    image2 = image2[startx:stopx, starty:stopy]
    if keyword_set(i2min)then i2min = i2min else i2min = min(image2)
    if keyword_set(i2max)then i2max = i2max else i2max = max(image2)
    if keyword_set(i2norm) then i2mean = mean(image2)
    image2 = bytscl(image2, min = i2min, max = i2max)
  endif else begin
    aimage2 = 0
    afile2 = 0
    image2 = 0
    i2min = 0
    i2max = 0
  endelse
  image2=scaling2[0]*image2+scaling2[1]
  aimg2 = ptr_new(aimage2)
  ; set parameters controlling the direction and speed of the movie
  direction = 1
  frame_incr = 1
  frame_speed = 100
  frame_nr = start_im
  org_sz = 1
  sz = size(image)
  d_xsz = sz[1]*mag      ; draw window xsize 
  d_ysz = sz[2]*mag      ; draw window ysize 
  ; check if desired x and y size is integer number of original size. 
                                ; If that's the case, use rebin in
                                ; stead of congrid (if xsize and ysize
                                ; are even numbers):
  frac = float(1./mag) - fix(1./mag)
  use_rebin = 0
  if frac eq 0 then begin
    if (xsize mod 2 or ysize mod 2) ne 1 then use_rebin = 1
  endif
  if keyword_set(aspect) then begin
    d_ysz = aspect*d_xsz
    mag = aspect
    use_rebin = 0
  endif else begin
    aspect = float(d_ysz/d_xsz)
 endelse
    x_org_size = xsize
    y_org_size = ysize
  ; if this is a zoomed image (pos ne 0) then resize x and y:
  if n_elements(pos) ne 0 then begin
    xsize = stopx - startx + 1
    ysize = stopy - starty + 1
  endif
  ; initialize size of draw window (ccd display):
  window, /pixmap, /free, xsize = xsize, ysize = ysize
  tv, bytscl(image, top = ncolors)
  pixid = !d.window

  xscale = interpol(indgen(xsize), d_xsz)
  yscale = interpol(indgen(ysize), d_ysz)

  ; get bitmap buttons for play/pause buttons:
  bmp_buttons = get_bmp_buttons()
  tlb = widget_base(title = title , mbar = menubar, $
                    tlb_size_events = 1, $
                          group_leader = group_leader,/row)
  lcol = widget_base(tlb, /frame, /column, xsize = 250)      ;left column.
  rcol = widget_base(tlb, /column)      ;right column.

  displaybase = widget_base(rcol, /row) 
  drawid=widget_draw(displaybase, retain = 2, xsize = d_xsz, ysize = d_ysz, $
                     /button_events, event_pro = 'iris_ximovie_zoom')

  filemenu=widget_button(menubar, value='File',/menu, uvalue='file')
  savemenu=widget_button(filemenu, value='Save as', /menu)
  psmenu=widget_button(savemenu, value='Postscript', event_pro = 'iris_ximovie_ps')
  gifmenu=widget_button(savemenu, value='GIF', /menu)
  gif1menu=widget_button(gifmenu, value='Snapshot', event_pro = 'iris_ximovie_gif')
  gif2menu=widget_button(gifmenu, value='All frames', event_pro = 'iris_ximovie_gif_all')  
  jpgmenu=widget_button(savemenu, value='JPG', /menu)
  jpg1menu=widget_button(jpgmenu, value='Snapshot', event_pro = 'iris_ximovie_jpeg')
  jpg2menu=widget_button(jpgmenu, value='All frames', event_pro = 'iris_ximovie_jpeg_all')  
  mpgmenu=widget_button(savemenu, value='MPEG', event_pro = 'iris_ximovie_mpeg')
  exitmenu=widget_button(filemenu, value='Close', event_pro='iris_ximovie_destroy')
  optmenu = widget_button(menubar, value = 'Options', /menu)
  frame_menu = widget_button(optmenu, value = 'Restrict frame range', event_pro = 'iris_ximovie_setframes')
  colmenu = widget_button(optmenu, value = 'Colour Table', event_pro = 'iris_ximovie_colors')

  anim_field = widget_base(lcol, /column, /frame)
  alabel = widget_label(anim_field, value = 'Animation Control')
  button_area = widget_base(anim_field, /row)
  play_fwd_button = widget_button(button_area, value = bmp_buttons.play, event_pro = 'iris_ximovie_play_fwd')
  pause_button = widget_button(button_area, value = bmp_buttons.pause_blk, /bitmap, event_pro = 'iris_ximovie_pause')
  play_rev_button = widget_button(button_area, value = bmp_buttons.play_rev, event_pro = 'iris_ximovie_play_rev')
  play_cycle_button = widget_button(button_area, value = bmp_buttons.cycle, event_pro = 'iris_ximovie_play_cycle')

  
  frameslider=-1
  framespeed_slider=-1
  blink_button=-1
  if last gt 0 then begin
  framenr_title = 'Frame number'
  frameslider = widget_slider(anim_field, xsize=200, $
                          minimum=first, maximum=last, $
                          title=framenr_title, $
                          value=first, event_pro = 'iris_ximovie_frameslider', /drag)

  framespeed_title = 'Animation Speed [frames/sec]'
  framespeed_slider = widget_slider(anim_field, xsize=200, $
                          minimum=1, maximum=100, $
                          title=framespeed_title, $
                          value=frame_speed, event_pro = 'iris_ximovie_framespeed', /drag)

  ; controls that appear only if two data sources
  if(twostream) then begin
    twostream_area = widget_base(anim_field, /row)
    stream1_button = widget_button(twostream_area, value = 'Stream 1', $
                             event_pro = 'iris_ximovie_stream1')
    stream2_button = widget_button(twostream_area, value = 'Stream 2', $
                             event_pro = 'iris_ximovie_stream2')
    mix_button = widget_button(twostream_area, value = 'Blink', $
                             event_pro = 'iris_ximovie_mix')
  endif
  stream1=1

  ; frame incremention slider
  frame_field = widget_base(lcol, /frame, /column)
  if last gt 1 then begin
    frame_incr_slider = widget_slider(frame_field, xsize = 200, minimum = 1, $
                                   maximum = last, title = 'Frame increment', $
                                   value = 1, event_pro = 'iris_ximovie_frameincr')
  endif

  ; Controls for showing single frames
  draw_field = widget_base(lcol, /frame, /column)
  drawlabel = widget_label(draw_field, value = 'Single image control')
  button_field = widget_base(draw_field, /row)
  decr_button = widget_button(button_field, value = '-', event_pro = 'iris_ximovie_decr')
  incr_button = widget_button(button_field, value = '+', event_pro = 'iris_ximovie_incr')
  blink_button = widget_button(button_field, value = 'Blink', event_pro = 'iris_ximovie_blink_start')
  blink_pause_button = widget_button(button_field, value = 'Pause', event_pro = 'iris_ximovie_blink_stop')
  
endif

  if(subimage) then begin
    ; slider bars to move around in full image when subimage is displayed:
    nx = stopx-startx
    ny = stopy-starty
    xstart_slider = widget_slider(draw_field, xsize = 200, minimum = 0, $
                                     maximum = x_org_size-nx-1, title = 'Change zoom position - X', $
                                     value = startx, event_pro = 'iris_ximovie_xscroll_slider', /drag)
    ystart_slider = widget_slider(draw_field, xsize = 200, minimum = 0, $
                                     maximum = y_org_size-ny-1, title = 'Change zoom position - Y', $
                                     value = starty, event_pro = 'iris_ximovie_yscroll_slider', /drag)
  endif
  
  ; Control window size etc.:
  wcontrol_field = widget_base(lcol, /frame, /column)
  abf = widget_base(wcontrol_field, /column, /nonexclusive)
  aspect_button = widget_button(abf, value = 'Keep Aspect ratio when resizing', $
                               event_pro = 'iris_ximovie_aspect')

  ; set aspect ratio button
   widget_control, aspect_button, set_button = 1
  
  ; find key frame
   key_frame=-1
   repeat key_frame=key_frame+1 until $
      max(scaling1[0]*((*aimg)[key_frame])+scaling1[1],/nan) gt imax/2. or key_frame eq last
   if key_frame gt last then begin
     message,'No good data found',/info
     return
  end

  ; control of gamma and histo_opt
   histofield=widget_base(lcol,/row)
   gamma=1.0
   gamma_slider = cw_fslider(lcol,/edit,format='(f6.2)',/frame, $
      maximum=3.0,minimum=0.1,value=gamma, $
      title='Gamma Correction',xsize=80, $
      event_func='iris_ximovie_gamma',/drag)

   histo_lim=10.0^(-3.)
   histoopt_slider = cw_fslider(lcol,/edit,format='(f6.2)',/frame, $
      maximum=-1.,minimum=-6.0,value=alog10(histo_lim), $
      title='log(HistoOpt Value)',xsize=80, $
      event_func='iris_ximovie_histoopt',/drag)
   image=scaling1[0]*((*aimg)[key_frame])+scaling1[1]
   imin=min(iris_histo_opt(image,histo_lim,missing=missing)>0.)^gamma
   imax=max(iris_histo_opt(image,histo_lim,missing=missing)>0.)^gamma

   key_frame2=-1
   gamma2=1.0
   histo_lim2=10.0^(-3.)
   if twostream then begin
     repeat key_frame2=key_frame2+1 until $
       max(scaling2[0]*((*aimg2)[key_frame2])+scaling2[1],/nan) ne i2max/2. or key_frame2 eq last
     if key_frame2 gt last then begin
       message,'No good data found for stream2',/info
       return
     end
     image2=scaling2[0]*((*aimg2)[key_frame])+scaling2[1]
     i2min=min(iris_histo_opt(image2,histo_lim2,missing=missing)>0.)^gamma2
     i2max=max(iris_histo_opt(image2,histo_lim2,missing=missing)>0.)^gamma2
   endif

; reset key_frame

  reset_keyfield = widget_base(lcol,/column)
  reset_keybutton = widget_button(reset_keyfield, value = 'Reset key frame', $
                              event_pro = 'iris_ximovie_reset_key_frame')
  
  ; time widget if keyword set

  if n_elements(time) eq 0 then begin
    time=-1
    timetext=-1
  endif
  if datatype(time) eq 'STR' then begin
    timebase=widget_base(lcol,/col)
    timetext=widget_label(timebase,value=time[0])
  endif else timetext=-1                       
  if n_elements(clock) eq 0 then clock=0

  ; Close iris_ximovie button:
  closefield = widget_base(lcol,/column)
  closebutton = widget_button(closefield, value = 'Close', $
                              event_pro = 'iris_ximovie_destroy')

    ; realize main window:
  widget_control, tlb, /realize, tlb_get_size = tlb_sz
  ; set up background
  bg_base = widget_base(tlb, event_pro = 'iris_ximovie_bck')
  bg_base2 = widget_base(tlb, event_pro = 'iris_ximovie_blink')
  ; get window id of display window
  widget_control, drawid, get_value = wid
  wset, wid

  tlb_xsz = tlb_sz[0]  ; xsize of whole widget in pixels 
  tlb_ysz = tlb_sz[1]  ; ysize of whole widget in pixels 
  menu_ysz = tlb_ysz - d_ysz 
  menu_xsz = tlb_xsz - d_xsz
  ;get and save color table
  tvlct, r, g, b, /get
  bottom = 0
  if (!d.n_colors le 256) then begin
    r = r[bottom:ncolors-1+bottom]
    g = g[bottom:ncolors-1+bottom]
    b = b[bottom:ncolors-1+bottom]
  endif
  if n_elements(groupl_leader) ne 0 then groupl = groupl_leader else groupl = tlb
  ; define the info structure, used send information around
  info = {alu:alu, $
          alu2:alu2, $
          afile:afile, $
          afile2:afile2, $
          offset:offset, $
          aimg:aimg, $
          aimg2:aimg2, $
          image:ptr_new(), $
          key_frame:key_frame, $
          key_frame2:key_frame2, $
          xsize:xsize, $
          ysize:ysize, $
          x_org_size:x_org_size, $
          y_org_size:y_org_size, $
          use_rebin:use_rebin, $
          startx:startx, $
          starty:starty, $
          stopx:stopx, $
          stopy:stopy, $
          xscale:ptr_new(), $
          yscale:ptr_new(), $
          tlb:tlb, $
          fdelete:fdelete, $
          drawid:drawid, $
          bg_base:bg_base, $
          bg_base2:bg_base2, $
          im1:1, $
          im2:0, $
          blink_button:blink_button, $
          d_xsz:d_xsz           ,$
          d_ysz:d_ysz           ,$
          tlb_xsz:tlb_xsz       ,$
          tlb_ysz:tlb_ysz       ,$        
          menu_ysz:menu_ysz     ,$
          menu_xsz:menu_xsz, $
          keep_aspect:1, $
          aspect:aspect, $
          r:r, g:g, b:b, $
          frame_nr:frame_nr, $
          direction:direction, $
          frame_incr:frame_incr, $
          frame_speed:frame_speed, $
          mag:mag, $
          ncolors:ncolors, $
          bottom:bottom, $
          playmode:'pause', $
          cycle:1, $
          first:first, $
          last:last, $
          nframes:nframes, $
          start_frame_id:0L, $
          stop_frame_id:0L, $
          gamma:gamma, $
          gamma2:gamma2, $
          histo_lim:histo_lim, $
          histo_lim2:histo_lim2, $
          gamma_slider:gamma_slider, $
          histoopt_slider:histoopt_slider, $
          missing:missing, $
          scaling1:scaling1, $
          scaling2:scaling2, $
          run:0, $
          blink:0, $
          imin:imin, $
          imax:imax, $
          i2min:i2min, $
          i2max:i2max, $
          imean:imean, $
          i2mean:i2mean, $
          stream1:stream1, $
          stream2:0, $
          mix:1, $
          twostream:twostream, $
          swap_endian:swap_endian, $
          fr_incr:1, $
          byt:byt, $
          flt:flt, $
          int:integ, $
          dbl:dbl, $
          action:drawid, $
          wid:wid, $
          topwid:topwid, $
          groupl:groupl, $
          sx:0, $
          sy:0, $
          mainpixid:pixid, $
          pixid:pixid, $
          frameslider:frameslider, $
          time:time, $
          timetext:timetext, $
          clock:clock, $
          play_fwd_button:play_fwd_button, $
          play_rev_button:play_rev_button, $
          play_cycle_button:play_cycle_button, $
          pause_button:pause_button, $
          bmp_buttons:bmp_buttons, $
          framespeed_slider:framespeed_slider}
  info = ptr_new(info, /no_copy)
  (*info).xscale = ptr_new(xscale)
  (*info).yscale = ptr_new(yscale)
  *(*info).xscale = xscale
  *(*info).yscale = yscale
  ; set user value of tlb widget to be the info ptr
  widget_control, tlb, set_uvalue = info
  widget_control, bg_base, set_uvalue = info

  ; create pseudoevent and send this event to xdisplay_draw, 
  ; in order to draw the image
  pseudoevent={widget_button,id:(*info).action, $
               top:tlb, handler:0l, select:1}
  iris_ximovie_draw, pseudoevent

  xmanager, 'iris_ximovie', tlb, /no_block, group_leader = group, $
            event_handler = 'iris_ximovie_resize', cleanup = 'iris_ximovie_cleanup'
end





