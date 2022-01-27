;+
; NAME:
;       IRIS_ALIGN_LIMB
;
; PURPOSE:
;       iris_align_limb aligns a series of SJI images (l2), if they have been taken at the limb
;
; CATEGORY:
;       IRIS sji analysis SW
;
; CALLING SEQUENCE:
;       sji = iris_align_limb(sjifile)
;
; INPUTS:
;       file - SJI l2 file or a iris_sji object
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;       returns the aligned sji-object, or applies the alignment directly on the sji object in the input
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
;       6-Feb-2014: Martin Wiesmann
;
; $Id: iris_align_limb.pro,v 1.17 2014/03/27 09:29:48 mawiesma Exp $
;-
; save plot as postscript file
pro iris_align_limb_ps,event
  thisfile=dialog_pickfile(/write,file='iris_align_limb.ps')
  if thisfile eq '' then return
  widget_control,event.top,get_uvalue=info
  thisdevice=!d.name
  set_plot,'ps',/copy
  device,file=thisfile,_extra=keywords,/inches,bits_per_pixel=8,/color
  pseudoevent={widget_button,id:info.action, $
    top:event.top, handler:0l, select:1}
  iris_align_limb_draw,pseudoevent
  device,/close_file
  set_plot,thisdevice
end

; save as jpeg file
pro iris_align_limb_jpeg,event
  thisfile=dialog_pickfile(/write,file='pixelplot.jpg')
  if thisfile eq '' then return
  widget_control,event.top,get_uvalue=info
  wset,info.wid
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
pro iris_align_limb_expslider,event
  widget_control, event.top, get_uvalue = info, /no_copy
  info.expnr=event.value-1
  if info.step3 then widget_control, info.shiftfield, set_value=(*info.limb_dxy)[0,info.expnr]
  pseudoevent={widget_button,id:0L, $
    top:info.tlb, handler:0l, select:1}
  widget_control, event.top, set_uvalue = info, /no_copy
  iris_align_limb_draw, pseudoevent
end

; turn on/off log scaling
pro iris_align_limb_log,event
  widget_control, event.top, get_uvalue = info, /no_copy
  info.log=event.select
  pseudoevent={widget_button,id:0L, $
    top:info.tlb, handler:0l, select:1}
  widget_control, event.top, set_uvalue = info, /no_copy
  iris_align_limb_draw, pseudoevent
end

; show sji as movie
pro iris_align_limb_ximovie,event
  widget_control, event.top, get_uvalue = info, /no_copy
  widget_control,/hourglass
  *info.sji->ximovie,info.lwin,group_leader=info.tlb
  widget_control, event.top, set_uvalue = info, /no_copy
end

; draw image anew
pro iris_align_limb_draw, event
  widget_control, event.top, get_uvalue = info, /no_copy
  wset,info.wid
  lwin=info.lwin
  
  drawing=1
  ;  if info.step1 then begin
  ;    tagnames = tag_names(event)
  ;    ind = where(tagnames eq 'PRESS',c)
  ;    if c gt 0 then begin
  ;      if event.press then begin
  ;        info.alignbox[[0,2]] = event.x
  ;        info.alignbox[[1,3]] = event.y
  ;        info.boxdrawing=1
  ;      endif else if event.release then begin
  ;        info.alignbox[2] = event.x
  ;        info.alignbox[3] = event.y
  ;        info.boxdrawing=0
  ;      endif else begin
  ;        if info.boxdrawing then begin
  ;          info.alignbox[2] = event.x
  ;          info.alignbox[3] = event.y
  ;        endif else drawing=0
  ;      endelse
  ;    endif
  ;    ;drawind a box is not that easy
  ;    ;alternative way
  ;    widget_control, info.leftfield, get_value=temp
  ;    info.alignbox[0]=temp
  ;    widget_control, info.bottomfield, get_value=temp
  ;    info.alignbox[1]=temp
  ;    widget_control, info.rightfield, get_value=temp
  ;    info.alignbox[2]=temp
  ;    widget_control, info.topfield, get_value=temp
  ;    info.alignbox[3]=temp
  ;  endif else if info.step2 then begin
  ;    widget_control, info.xshiftfield, get_value=temp
  ;    info.rshift[0]=temp;*(*info.sji->getresx())
  ;    widget_control, info.yshiftfield, get_value=temp
  ;    info.rshift[1]=temp;*(*info.sji->getresy())
  ;    widget_control, info.radiusfield, get_value=temp
  ;    info.radjust=temp
  ;    widget_control, info.radresult, set_value='= '+string(info.rsol+info.radjust, format='(f7.2)')
  ;    info.rsoltemp=info.rsol+info.radjust
  ;  endif
  
  if drawing then begin
    widget_control,info.timetext,set_value=(info.time)[info.expnr]
    xtitle='Solar X [arcsec]'
    ytitle='Solar Y [arcsec]'
    angle=round(*info.sji->getinfo('SAT_ROT'))
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
    sji_im=rotate(((*info.sji)->getvar())[*,*,info.expnr],rot)
    xw=(*info.sji)->getxw(lwin)
    yw=(*info.sji)->getyw(lwin)
    if sji_im[0] eq -1 then return
    hdr=(*info.sji)->gethdr()
    xscale=(*info.sji)->xscale(lwin)
    yscale=(*info.sji)->yscale(lwin)
    ((*info.sji)->getaux())->loadct,'int+red'
    im_min=(*info.sji)->datamin(lwin)
    if info.log then begin
      mplot_image,alog10(iris_histo_opt(sji_im,0.005,missing=*info.sji->missing())>1.), $
        xscale,yscale,yticklen=0.02/info.aspect, $
        /bgblack,xtitle=xtitle,ytitle=ytitle, $
        top=254,color=254
    endif else begin
      mplot_image,(iris_histo_opt(sji_im>im_min,0.005,missing=*info.sji->missing())),$
        xscale,yscale,yticklen=0.02/info.aspect, $
        /bgblack,xtitle=xtitle,ytitle=ytitle, $
        top=254,color=254
    endelse
    if info.step1 && total(info.alignbox) gt 0 then begin
      oplot, xscale[info.alignbox[[0,0,2,2,0]]],yscale[info.alignbox[[1,3,3,1,1]]],thick=1,color=255,line=2
    endif else if info.step2 ||info.step2b || info.step3 then begin
      npt=10000
      phi=findgen(npt)/(npt-1.)*2*!pi
      xrad = info.rsoltemp*sin(phi) + info.rshift[0]
      yrad = info.rsoltemp*cos(phi) + info.rshift[1]
      oplot,xrad,yrad,thick=1,color=255,line=2
      if info.step2 ||info.step2b then begin
        xrad = (info.rsoltemp+info.alignbounds[0])*sin(phi) + info.rshift[0]
        yrad = (info.rsoltemp+info.alignbounds[0])*cos(phi) + info.rshift[1]
        oplot,xrad,yrad,thick=1,color=255,line=1
        xrad = (info.rsoltemp+info.alignbounds[1])*sin(phi) + info.rshift[0]
        yrad = (info.rsoltemp+info.alignbounds[1])*cos(phi) + info.rshift[1]
        oplot,xrad,yrad,thick=1,color=255,line=1
      endif
    endif
    ((*info.sji)->getaux())->loadct,'int'
  endif ;if drawing
  widget_control, event.top, set_uvalue = info, /no_copy
end

;resize main window
pro iris_align_limb_resize, event
  widget_control, event.top ,get_uvalue = info, /No_copy
  if (event.id ne info.leftfield) && (event.id ne info.rightfield) $
    && (event.id ne info.bottomfield) && (event.id ne info.topfield) $
    && (event.id ne info.xshiftfield) && (event.id ne info.yshiftfield) $
    && (event.id ne info.radiusfield) then begin
    info.d_xsz = (event.x - info.menu_xsz) > 0
    info.d_ysz = (event.y - info.menu_ysz) > 0
    widget_control, info.drawid, draw_xsize = info.d_xsz, $
      draw_ysize = info.d_ysz, xsize = info.d_xsz<((get_screen_size())[0])*0.75, $
      ysize = info.d_ysz
  endif
  pseudoevent={widget_button,id:0L, $
    top:info.tlb, handler:0l, select:1}
  widget_control, event.top ,set_uvalue = info, /No_copy
  iris_align_limb_draw, pseudoevent
end

pro iris_align_limb_destroy, event
  widget_control, event.top, /destroy
end

pro iris_align_limb_cleanup, tlb
  widget_control, tlb, get_uvalue = info, /No_copy
  info.result = info.sji
  ptr_free, info.sji
end

pro iris_align_step1fields, event
  widget_control, event.top ,get_uvalue = info, /No_copy
  widget_control, info.leftfield, get_value=temp
  info.alignbox[0]=temp
  widget_control, info.bottomfield, get_value=temp
  info.alignbox[1]=temp
  widget_control, info.rightfield, get_value=temp
  info.alignbox[2]=temp
  widget_control, info.topfield, get_value=temp
  info.alignbox[3]=temp
  pseudoevent={id:0L, top:info.tlb, handler:0l, select:1}
  widget_control, event.top ,set_uvalue = info, /No_copy
  iris_align_limb_draw, pseudoevent
end

pro iris_align_step2fields, event
  widget_control, event.top ,get_uvalue = info, /No_copy
  widget_control, info.xshiftfield, get_value=temp
  info.rshift[0]=temp
  widget_control, info.yshiftfield, get_value=temp
  info.rshift[1]=temp
  widget_control, info.radiusfield, get_value=temp
  info.radjust=temp
  info.rsoltemp=info.rsol+info.radjust
  widget_control, info.radresult, set_value='= '+string(info.rsoltemp, format='(f7.2)')
  widget_control, info.minboundfield, get_value=temp
  info.alignbounds[0]=temp
  widget_control, info.maxboundfield, get_value=temp
  info.alignbounds[1]=temp
  widget_control, info.stepfield, get_value=temp
  info.alignsteps=temp
  pseudoevent={id:0L, top:info.tlb, handler:0l, select:1}
  widget_control, event.top ,set_uvalue = info, /No_copy
  iris_align_limb_draw, pseudoevent
end

pro iris_align_step3fields, event
  widget_control, event.top ,get_uvalue = info, /No_copy
  widget_control, /hourglass
  case event.id of
    info.shiftfield: begin
      widget_control, info.shiftfield, get_value=temp
      diff = temp - (*info.limb_dxy)[0,info.expnr]
      (*info.limb_dxy)[0,info.expnr]=temp
      *info.sji->align_finetune, info.lwin, info.expnr, [diff,0]
    end
    info.smoothbutton: begin
      widget_control, info.smoothwidthfield, get_value=width
      temp = *info.limb_dxy
      temp[0,*] = smooth((*info.limb_dxy)[0,*], width)
      diff = temp - (*info.limb_dxy)
      *info.sji->shift, info.lwin, diff
      (*info.limb_dxy)=temp
      widget_control, info.shiftfield, set_value=temp[info.expnr]
    end
    info.savebutton: begin
      sji=*info.sji
      save,sji,filename='limbaligned_sji_object.sav'
    end
    info.realignbutton: BEGIN
      *info.sji->realign, info.lwin
    end
    else:
  endcase
  wset, info.wid_limb_dxy
  plot, (*info.limb_dxy)[0,*], ytitle='X-shift',xrange=[0,N_ELEMENTS((*info.limb_dxy)[0,*])-1],xstyle=1
  pseudoevent={id:0l, top:info.tlb, handler:0l}
  widget_control, event.top ,set_uvalue = info, /No_copy
  iris_align_limb_draw, pseudoevent
end

pro iris_align_limb_step1, event
  widget_control, event.top ,get_uvalue = info, /No_copy
  widget_control, /hourglass
  widget_control, info.step1field, sensitive=0
  *info.sji->align,info.lwin,dxy=dxy,region=info.alignbox
  widget_control, info.step2field, sensitive=1
  info.step1=0
  info.step2=1
  
  ;dxy=*info.sji->get_alignshift()
  n=n_ELEMENTS(dxy[0,*])
  base = widget_base(/column)
  w1 = widget_draw(base,xsize=600,ysize=300)
  w2 = widget_draw(base,xsize=600,ysize=300)
  w3 = widget_draw(base,xsize=600,ysize=300)
  widget_control,base,/realize
  widget_control, w1, get_value = wid
  wset, wid
  plot, dxy[0,*], ytitle='X-shift',xrange=[0,n],xstyle=1
  widget_control, w2, get_value = wid
  wset, wid
  plot, dxy[1,*], ytitle='Y-shift',xrange=[0,n],xstyle=1
  widget_control, w3, get_value = wid
  wset, wid
  plot, dxy[0,*], dxy[1,*], xtitle='X-shift', ytitle='Y-shift', psym=2
  
  pseudoevent={widget_base,id:0l, $
    top:info.tlb, handler:0l, x:info.tlb_xsz, y:info.tlb_ysz}
  widget_control, event.top ,set_uvalue = info, /No_copy
  iris_align_limb_draw, pseudoevent
end

pro iris_align_limb_step2, event
  widget_control, event.top ,get_uvalue = info
  widget_control, /hourglass
  widget_control, info.step2field, sensitive=0
  wset,info.wid
  lwin=info.lwin
  info.step1=0
  info.step2=0
  info.step2b=1
  
  npt=10000
  phi=findgen(npt)/(npt-1.)*2*!pi
  nrad=info.alignsteps
  dr = findgen(nrad) * (info.alignbounds[1]-info.alignbounds[0])/(nrad-1.) + info.alignbounds[0]
  rad=info.rsol+info.radjust+dr
  xrad=(info.rsol+info.radjust)*sin(phi) + info.rshift[0]
  yrad=(info.rsol+info.radjust)*cos(phi) + info.rshift[1]
  xscale=(*info.sji)->xscale(lwin)
  yscale=(*info.sji)->yscale(lwin)
  line=where(xrad le max(xscale) and xrad gt min(xscale) and $
    yrad le max(yscale) and yrad gt min(yscale))
  nline=n_elements(line)
  nt=*info.sji->getnexp()
  ldata=fltarr(nline,nrad,nt)
  
  sz=size((*info.sji)->getvar())
  ((*info.sji)->getaux())->loadct,'int'
  
  base=widget_base(/column)
  w1=widget_draw(base,xsize=600,ysize=300)
  w2=widget_draw(base,xsize=600,ysize=300)
  widget_control,base,/realize
  widget_control, w1, get_value = wid
  wset, wid
  
  ;  ymax=0
  for it=0,nt-1 do begin
    print,it,nt-1
    for ir=0,nrad-1 do begin
      ;    print,'rad =',string(rad[ir],format='(f6.1)'),' ir/(nrad-1) ', $
      ;      strtrim(string(ir),2),'/',strtrim(string(nrad-1),2)
      ;info.rsoltemp=rad[ir]
      ;    widget_control, event.top ,set_uvalue = info
      ;    pseudoevent={id:0l, top:info.tlb, handler:0l}
      ;    iris_align_limb_draw, pseudoevent
      xrad=rad[ir]*sin(phi[line]) + info.rshift[0]
      yrad=rad[ir]*cos(phi[line]) + info.rshift[1]
      xint=(xrad-min(xscale))/(max(xscale)-min(xscale))*sz[1]
      yint=(yrad-min(yscale))/(max(yscale)-min(yscale))*sz[2]
      ;var=interpolate(((*info.sji)->getvar())[*,*,0],xint,yint)
      ;    if max(var) gt ymax then ymax=max(var)
      ;    plot,yrad,var,xr=[min(yrad),max(yrad)],xst=1,yr=[0,ymax]
      ;    for it=0,nt-1 do begin
      ldata[*,ir,it]=interpolate(((*info.sji)->getvar())[*,*,it],xint,yint)
    endfor
    pih,rotate(ldata[*,*,it],3)
  endfor
  
  rint=rad/(info.rsol+info.radjust)
  lphi=fltarr(nrad,nt)
  dr0=fltarr(nt)
  for it=0,nt-1 do begin
    for ir=0,nrad-1 do begin
      lphi[ir,it]=trapez(phi[line],ldata[*,ir,it]*rint[ir])
    endfor
    lphi[*,it]=lphi[*,it]/(max(phi[line])-min(phi[line]))
    lphimax=max(lphi[*,it],rint0) & print,it,' r[rint0]',rad[rint0],' rint0 ',rint0
    if it eq 0 then info.rsoltemp=rad[rint0]
    dr0[it]=(rad[rint0]-info.rsoltemp)/(*info.sji)->getresx()
  endfor
  pih,lphi
  
  dr=fltarr(2,nt)
  dr[0,*]=-dr0
  
  info.limb_dxy = ptr_new(dr)
  
  widget_control, w2, get_value = wid
  wset, wid
  plot, dr[0,*], ytitle='X-shift',xrange=[0,nt-1],xstyle=1
  info.wid_limb_dxy=wid
  
  *info.sji->shift, lwin, dr
  
  widget_control, info.step3field, sensitive=1
  info.step2b=0
  info.step3=1
  
  pseudoevent={id:0l, top:info.tlb, handler:0l}
  widget_control, event.top ,set_uvalue = info, /No_copy
  iris_align_limb_draw, pseudoevent
end

function iris_align_limb, file

  if n_params() lt 1 then begin
    message,'iris_align_limb, file',/info
    ;return
    file = '/mn/xsan/d2/iris/data/level2/2013/12/10/20131210_023548_3800259453/iris_l2_20131210_023548_3800259453_SJI_1400_t000.fits'
    file = '~/iris/data/level2/iris_l2_20131210_023548_3800259453_SJI_1400_t000.fits'
  ;file = '~/iris/data/level2/iris_l2_20131211_075749_3813513603_SJI_1400_t000.fits'
  ;file = '~/iris/data/level2/iris_l2_20140114_170436_3823257483_SJI_1400_t000.fits'
  endif
  
  intype = size(file, /type)
  if intype eq 7 then begin
    sji = iris_sji(file)
  endif else if intype eq 11 then begin
    sji = file
  endif else begin
    message, 'incorrect input type',/info
  endelse
  
  lwin=where(sji->getread_sji(),c)
  if c ne 1 then begin
    message,'problem reading file',/info
    return,-1
  endif
  lwin=lwin[0]
  
  tlb = widget_base(title = 'SJI Image: '+sji->getfilename(lwin), mbar = menubar, $
    tlb_size_events = 1,/row,xoffset=100,yoffset=100)
    
    
  lcol = widget_base(tlb, /frame, /column)
  rcol = widget_base(tlb, /column)
  
  displaybase = widget_base(rcol, /row)
  
  xscale=sji->xscale(lwin)
  yscale=sji->yscale(lwin)
  aspect=(max(xscale)-min(xscale))/(max(yscale)-min(yscale))
  xysz=(sji->getaux())->getdrawsize('standard',aspect=aspect)
  calc_xysize,xysz[0],xysz[1],d_xsz,d_ysz
  
  drawid=widget_draw(displaybase, retain = 2, $
    xsize = d_xsz < ((get_screen_size())[0])*0.75, x_scroll_size = d_xsz , $
    ysize = d_ysz, y_scroll_size = d_ysz, $
    ;/button_events, /motion_events, $
    event_pro='iris_align_limb_draw')
  ;
  filemenu=widget_button(menubar, value='File',/menu, uvalue='file')
  savemenu=widget_button(filemenu, value='Save as', /menu)
  psmenu=widget_button(savemenu, value='Postscript', event_pro = 'iris_align_limb_ps')
  jpgmenu=widget_button(savemenu, value='JPG', event_pro = 'iris_align_limb_jpeg')
  ;
  exitmenu=widget_button(filemenu, value='Close', event_pro='iris_align_limb_destroy')
  ;
  sliderbase = widget_base(lcol,/col)
  if sji->getnexp(lwin) gt 1 then begin
    expslider = widget_slider(sliderbase, xsize = 90, $
      minimum = 1, maximum = sji->getnexp(lwin), $
      title = 'Exp.# ', $
      value = 1, $
      event_pro = 'iris_align_limb_expslider')
  endif
  ;
  time=anytim2utc(anytim2tai(sji->getinfo('STARTOBS'))+sji->gettime(lwin), $
    /ccsds,/truncate)
  timebase=widget_base(lcol,/col)
  timetext=widget_label(timebase,value=time[0])
  
  logfield = widget_base(lcol,/column,/nonexclusive)
  logbutton = widget_button(logfield, value = 'log(image)', $
    event_pro = 'iris_align_limb_log')
    
  ximoviefield = widget_base(lcol,/column)
  ximoviebutton = widget_button(ximoviefield, value = 'ximovie of SJI series', $
    event_pro = 'iris_align_limb_ximovie')
    
  step1field = widget_base(lcol, /column, /frame, event_pro='iris_align_step1fields')
  label = widget_label(step1field, value='STEP 1')
  label = widget_label(step1field, value='Choose a region for cross correlation alignment')
  ;label = widget_label(step1field, value='Click corners of rectangle in image')
  xbase = widget_base(step1field, /row)
  ybase = widget_base(step1field, /row)
  alignbox=[30,40,100,100];280,290]
  leftfield = cw_field(xbase, title='Left  :', value=alignbox[0], /integer, /all_events, xsize=5)
  bottomfield = cw_field(ybase, title='Bottom:', value=alignbox[1], /integer, /all_events, xsize=5)
  rightfield = cw_field(xbase, title='Right:', value=alignbox[2], /integer, /all_events, xsize=5)
  topfield = cw_field(ybase, title='Top  :', value=alignbox[3], /integer, /all_events, xsize=5)
  alignbutton = widget_button(step1field, value='Run alignment', event_pro='iris_align_limb_step1')
  
  
  step2field = widget_base(lcol, /column, /frame, sensitive=0, event_pro='iris_align_step2fields')
  label = widget_label(step2field, value='STEP 2')
  label = widget_label(step2field, value='Fine-adjust the solar limb')
  xbase = widget_base(step2field, /row)
  ybase = widget_base(step2field, /row)
  rshift=[-5.0, -12.0]
  xshiftfield = cw_field(xbase, title='Center: X :', value=rshift[0], /floating, /all_events, xsize=7)
  yshiftfield = cw_field(xbase, title='Y :', value=rshift[1], /floating, /all_events, xsize=7)
  rsol = get_rb0p(time[0],/radius,/quiet)
  radjust=10.2
  rsoltemp = rsol+radjust
  radiusfield = cw_field(ybase, title='Radius: '+string(rsol, format='(f7.2)')+' +', value=radjust, /floating, /all_events, xsize=6)
  radresult = widget_label(ybase, value='= '+string(rsoltemp, format='(f7.2)'))
  boundary = widget_base(step2field, /row)
  alignbounds = [-10.0, 10.0]
  alignsteps = 100
  minboundfield = cw_field(boundary, title='min', value=alignbounds[0], /floating, /all_events, xsize=6)
  maxboundfield = cw_field(boundary, title='max', value=alignbounds[1], /floating, /all_events, xsize=6)
  stepfield = cw_field(boundary, title='steps', value=alignsteps, /integer, /all_events, xsize=6)
  align2button = widget_button(step2field, value='Run limb alignment', event_pro='iris_align_limb_step2')
  
  
  step3field = widget_base(lcol, /column, /frame, sensitive=0, event_pro='iris_align_step3fields')
  label = widget_label(step3field, value='STEP 3')
  label = widget_label(step3field, value='Fine-adjust the shift values')
  smoothbase = widget_base(step3field, /row)
  smoothbutton = widget_button(smoothbase, value='Smooth line')
  smoothwidth = 7
  smoothwidthfield = cw_field(smoothbase, title='using width:', value=smoothwidth, /integer, xsize=5)
  shiftfield = cw_field(step3field, title='Shift :', value=0.0, /floating, /all_events, xsize=7)
  realignbutton = widget_button(step3field, value='realign original')
  label = widget_label(step3field, value='Realignment recommended after shifting single exposures')
  savebutton = widget_button(step3field, value='save object')
  
  
  closefield = widget_base(lcol,/column)
  closebutton = widget_button(closefield, value = 'Close', $
    event_pro = 'iris_align_limb_destroy')
    
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
  
  result = ptr_new(sji)
  
  ; define the info structure, used to send information around
  info = {tlb:tlb               ,$
    sji:ptr_new(sji)    ,$
    result:result,    $
    lwin:lwin             ,$
    drawid:drawid         ,$
    xtitle:'pixel'         ,$
    ytitle:'pixel'         ,$
    timetext:timetext     ,$
    time:time             ,$
    d_xsz:d_xsz           ,$
    d_ysz:d_ysz           ,$
    expnr:0               ,$
    log:0                 ,$
    leftfield:leftfield   ,$
    bottomfield:bottomfield   ,$
    rightfield:rightfield   ,$
    topfield:topfield   ,$
    alignbox:alignbox    ,$
    boxdrawing:0B         ,$
    step1field:step1field ,$
    step1:1B              ,$
    step2field:step2field ,$
    step2:0B              ,$
    step2b:0B              ,$
    step3:0B              ,$
    rsol:rsol             ,$
    rsoltemp:rsoltemp     ,$
    xshiftfield:xshiftfield,$
    yshiftfield:yshiftfield,$
    radiusfield:radiusfield,$
    radresult:radresult  ,$
    rshift:rshift      ,$
    radjust:radjust           ,$
    alignbounds:alignbounds ,$
    alignsteps:alignsteps ,$
    minboundfield:minboundfield ,$
    maxboundfield:maxboundfield ,$
    stepfield:stepfield ,$
    limb_dxy:ptr_new() ,$
    wid_limb_dxy:0 ,$
    step3field:step3field ,$
    shiftfield:shiftfield ,$
    smoothwidthfield:smoothwidthfield, $
    smoothbutton:smoothbutton, $
    realignbutton:realignbutton, $
    savebutton:savebutton ,$
    aspect:aspect         ,$
    show_raster:0         ,$
    tlb_xsz:tlb_xsz       ,$
    tlb_ysz:tlb_ysz       ,$
    menu_xsz:menu_xsz     ,$
    menu_ysz:menu_ysz     ,$
    wid:wid}
    
  ; set user value of tlb widget to be the info ptr
  widget_control, tlb, set_uvalue = info, /no_copy
  ; create pseudoevent and send this event to xdisplay_draw,
  ; in order to draw the image
  pseudoevent={widget_base,id:0l, $
    top:tlb, handler:0l, x:tlb_xsz, y:tlb_ysz}
  iris_align_limb_resize, pseudoevent
  xmanager, 'iris_align_limb', tlb, $
    event_handler = 'iris_align_limb_resize', cleanup = 'iris_align_limb_cleanup'

  res = *result
  ptr_free, result
  return, res
end
