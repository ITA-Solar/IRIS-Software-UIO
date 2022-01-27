pro IRISsim_showCCDregions, CRSList, CRSind3, imagefac, imfuv, imnuv, imsji, MainWindow, CCDWindow

  ; $Id: irissim_showccdregions.pro,v 1.5 2013/09/05 12:24:19 mawiesma Exp $  ;

  bold=2*imagefac
  titlesize=2.5*imagefac
  titleposy=titlesize*!d.y_ch_size/548
  yfac = 1096.0/548.0
  
  ;draw the window
  CCDWindow = WIDGET_BASE(/COLUMN, group_leader=MainWindow, XOffset=0, YOffset=0, title='CRS views on CCD', /TLB_KILL_REQUEST_EVENTS)
  drawFUV = WIDGET_DRAW(CCDWindow, xsize=2072*imagefac, ysize=548*imagefac)
  
  base2 = WIDGET_BASE(CCDWindow, /ROW)
  drawSJI = WIDGET_DRAW(base2, xsize=1036*imagefac, ysize=548*imagefac)
  drawNUV = WIDGET_DRAW(base2, xsize=1036*imagefac, ysize=548*imagefac)
  
  widget_control, CCDWindow, /realize
  widget_control, CCDWindow, set_UValue=MainWindow
  xmanager, 'IRISsim_showXML', CCDWindow, /just_reg, /no_block
  
  widget_control, drawFUV, get_Value=wid
  wset, wid
  tv, imfuv, true=1
  ind=CRSind3[2]
  if ind ge 0 then begin
    for reg=0,CRSList[ind].SubRegions-1 do begin
      coords = IRISsim_flipcoords( (*(CRSList[ind]).StartCol)[reg], (*(CRSList[ind]).EndCol)[reg], $
        (*(CRSList[ind]).StartRow)[reg], (*(CRSList[ind]).EndRow)[reg], 1, 1, /fuv, /startatzero )
      left = coords.tsc / 2.0 * imagefac
      if left lt 1.0 then left=1.0
      right = coords.tec / 2.0 * imagefac
      if right gt 2070.0*imagefac then right=2070.0*imagefac
      bottom = coords.tsr / yfac * imagefac
      if bottom lt 1.0 then bottom=1.0
      top = coords.ter / yfac * imagefac
      if top gt 546.0*imagefac then top=546.0*imagefac
      plots, [left, left], [bottom, top], color=150, thick=2, /device
      plots, right, top, color=150, thick=2, /continue, /device
      plots, right, bottom, color=150, thick=2, /continue, /device
      plots, left, bottom, color=150, thick=2, /continue, /device
    endfor
    xyouts, 0.5, 1-1.7*titleposy, CRSList[ind].ID, color=0, /normal, charsize=titlesize, charthick=bold, alignment=0.5
  endif
  
  
  widget_control, drawNUV, get_Value=wid
  wset, wid
  tv, imnuv, true=1
  ind=CRSind3[1]
  if ind ge 0 then begin
    for reg=0,CRSList[ind].SubRegions-1 do begin
      coords = IRISsim_flipcoords( (*(CRSList[ind]).StartCol)[reg], (*(CRSList[ind]).EndCol)[reg], $
        (*(CRSList[ind]).StartRow)[reg], (*(CRSList[ind]).EndRow)[reg], 1, 1, /nuv, /startatzero )
      left = coords.tsc / 2.0 * imagefac
      if left lt 1.0 then left=1.0
      right = coords.tec / 2.0 * imagefac
      if right gt 1034.0*imagefac then right=1034.0*imagefac
      bottom = coords.tsr / yfac * imagefac
      if bottom lt 1.0 then bottom=1.0
      top = coords.ter / yfac * imagefac
      if top gt 546.0*imagefac then top=546.0*imagefac
      plots, [left, left], [bottom, top], color=150, thick=2, /device
      plots, right, top, color=150, thick=2, /continue, /device
      plots, right, bottom, color=150, thick=2, /continue, /device
      plots, left, bottom, color=150, thick=2, /continue, /device
    endfor
    xyouts, 0.5, 1-1.7*titleposy, CRSList[ind].ID, color=0, /normal, charsize=titlesize, charthick=bold, alignment=0.5
  endif
  
  widget_control, drawSJI, get_Value=wid
  wset, wid
  tv, imsji, true=1
  ind=CRSind3[0]
  if ind ge 0 then begin
    for reg=0,CRSList[ind].SubRegions-1 do begin
      left = ((*(CRSList[ind]).StartRow)[reg]-1) / 2.0 * imagefac
      if left lt 1.0 then left=1.0
      right = ((*(CRSList[ind]).EndRow)[reg]-1) / 2.0 * imagefac
      if right gt 1034.0*imagefac then right=1034.0*imagefac
      bottom = ((*(CRSList[ind]).StartCol)[reg]-1) / yfac * imagefac
      if bottom lt 1.0 then bottom=1.0
      top = ((*(CRSList[ind]).EndCol)[reg]-1) / yfac * imagefac
      if top gt 546.0*imagefac then top=546.0*imagefac
      plots, [left, left], [bottom, top], color=150, thick=2, /device
      plots, right, top, color=150, thick=2, /continue, /device
      plots, right, bottom, color=150, thick=2, /continue, /device
      plots, left, bottom, color=150, thick=2, /continue, /device
    endfor
    xyouts, 0.5, 1-1.7*titleposy, CRSList[ind].ID, color=255, /normal, charsize=titlesize, charthick=bold, alignment=0.5
  endif
  
end
