;
;+
; NAME:
;       IRIS_AIA_MANUALALIGN
;
; PURPOSE:
;       iris_aia_manualalign is a widget tool that allows the manual alignment
;       of IRIS and AIA data. The WCS values will be changed in the AIA data set
;
; CATEGORY:
;       IRIS Data analysis
;
; CALLING SEQUENCE:
;       iris_aia_manualalign, OBSid[, irisdir=irisdir, aiadir=aiadir]
;
; INPUTS:
;       OBSid: e.g. '20140611_073631_3820256197', ignored if both irisdir and aiadir are defined
;
; KEYWORD PARAMETERS:
;       irisdir: The path to the level 2 IRIS data (optional)
;                if not given, the program tries to find it using the OBSid
;                first it looks in getenv('IRIS_DATA')+'/level2/'
;                if not found it looks in IRISsim_constants->get_data_path_uio_l2();UIO
;                if not found it looks in IRISsim_constants->get_data_path_lmsal_l2();LMSAL
;       aiadir: The path to the level 2 AIA data (optional)
;                if not given, the program tries to find it using the OBSid
;                (At UIO we decided to save the AIA files in the IRIS level 3 branch for convenience)
;                first it looks in getenv('IRIS_DATA')+'/level3/'
;                if not found it looks in IRISsim_constants->get_data_path_uio_l3();UIO
;                if not found it looks in IRISsim_constants->get_data_path_lmsal_l3();LMSAL
;
; OUTPUTS:
;       When desired changes are applied the header keywords and
;       auxiliary extension of AIA fits files are changed according to given values
;
; CALLS:
;
; COMMON BLOCKS:
;
; PROCEDURE:
;       The procedure opens a widget tool, which displays one exposure of the IRIS data with the
;       corresponding exposure of AIA data. left image blinks the two images, right images shows
;       IRIS data in red and AIA data in blue. One can choose the wavelength filter for IRIS and AIA
;       data separately, as well as the exposure index. If exposure index of IRIS is changed the exposure
;       index of AIA is adjusted to the AIA exposure closest in time, and vice versa.
;       With the 3 sliders, one can shift the IRIS image in x and y, and rotate it.
;       These shifts are applied to the WCS value of the AIA dataset when clicking the corresponding button.
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;       8-Oct-2014: Martin Wiesmann
;
; $Id: 2024-03-20 14:43 CET $
;
;-

function iris_aia_manualalign_scaleimage, image, cutoff
  if N_ELEMENTS(cutoff) eq 0 then cutoff=0.01
  im = histo_opt(image,cutoff)
  minim = min(im, max=maxim)
  diffim = maxim-minim
  im = 255 * (im-minim)/diffim
  return, im
end




pro iris_aia_manualalign_getimages, diris, daia, auxiris, auxaia, expind, iwiniris, iwinaia, shiftrot0, $
  datairis, dataaia, maxsize, $
  aia=aia, $
  expindother=expindother, orimageiris=orimageiris, orimageaia=orimageaia, $
  timeiris=timeiris, timeaia=timeaia
  
  if keyword_set(aia) then begin
    expindaia = expind
    timediff = min(abs((*auxiris).time-(*auxaia).time[expindaia]), expindiris)
    expindother = expindiris
  endif else begin
    expindiris = expind
    timediff = min(abs((*auxaia).time-(*auxiris).time[expindiris]), expindaia)
    expindother = expindaia
  endelse
  startobs = (*diris)->getinfo('STARTOBS')
  timeiris = anytim2utc(IRISsim_addtime(startobs, (*auxiris).time[expindiris], /seconds),/ccsds)
  timeaia = anytim2utc(IRISsim_addtime(startobs, (*auxaia).time[expindaia], /seconds),/ccsds)
  
  orimageiris = reform((*datairis)[*,*,expindiris])
  orimageaia = reform((*dataaia)[*,*,expindaia])
  satdiff = (*diris)->getinfo('SAT_ROT') - ((*daia)->getinfo('SAT_ROT') - shiftrot0)
  ind = where(total(orimageiris,2) gt (size(orimageiris))[2] * (-200), count)
  if count gt 1 then begin
    x1 = ind[0]
    x2 = ind[N_ELEMENTS(ind)-1]
    ind = where(total(orimageiris,1) gt (size(orimageiris))[1] * (-200))
    y1 = ind[0]
    y2 = ind[N_ELEMENTS(ind)-1]
    orimageiris = orimageiris[x1:x2,y1:y2]
    if x2-x1+1 gt maxsize[0] || y2-y1+1 gt maxsize[1] then begin
      if x2-x1+1 gt maxsize[0] then fact = float(maxsize[0]) / float(x2-x1+1)
      if fact*(y2-y1+1) gt maxsize[1] then fact = float(maxsize[1]) / float(y2-y1+1)
      xs = floor(fact * (x2-x1+1))
      ys = floor(fact * (y2-y1+1))
      orimageiris = congrid(orimageiris, xs, ys, cubic=-0.5)
    endif else begin
      xs = x2-x1+1
      ys = y2-y1+1
    endelse
    
    crpix1iris = (*diris)->getinfo('CRPIX1')
    crpix2iris = (*diris)->getinfo('CRPIX2')
    crpix1aia = (*daia)->getinfo('CRPIX1')
    crpix2aia = (*daia)->getinfo('CRPIX2')
    x1aia = floor(crpix1aia - ((*auxaia).xcen[expindaia] - ((*auxiris).xcen[expindiris] - (crpix1iris-x1)*(*diris)->getresx(iwiniris))) / (*daia)->getresx(iwinaia))
    x2aia = ceil(crpix1aia - ((*auxaia).xcen[expindaia] - ((*auxiris).xcen[expindiris] - (crpix1iris-x2)*(*diris)->getresx(iwiniris))) / (*daia)->getresx(iwinaia))
    y1aia = floor(crpix2aia - ((*auxaia).ycen[expindaia] - ((*auxiris).ycen[expindiris] - (crpix2iris-y1)*(*diris)->getresy(iwiniris))) / (*daia)->getresy(iwinaia))
    y2aia = ceil(crpix2aia - ((*auxaia).ycen[expindaia] - ((*auxiris).ycen[expindiris] - (crpix2iris-y2)*(*diris)->getresy(iwiniris))) / (*daia)->getresy(iwinaia))
    orimageaia = congrid(orimageaia[x1aia:x2aia,y1aia:y2aia], xs, ys, cubic=-0.5)
    orimageaia = rot(orimageaia, satdiff, cubic=-0.5)
  endif else begin
    orimageiris = fltarr(3,3)
    orimageaia = fltarr(3,3)
  endelse
end




pro iris_aia_manualalign_event, event
  widget_control, event.top, get_uvalue=info, /no_copy
  destroyed=0
  
  IF (TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_TIMER') THEN BEGIN
    if info.blinkon then begin
      if info.blinkiris then begin
        im = *info.imageaia
        info.blinkiris = 0
      endif else begin
        im = *info.imageiris
        info.blinkiris = 1
      endelse
      wset, info.winblink
      erase
      tv, im
      WIDGET_CONTROL, event.TOP, TIMER=info.blinktime
    endif
  ENDIF else begin
  
  
    case event.id of
      info.ui_blinkstart: begin
        if info.blinkon then begin
          info.blinkon=0
          widget_control, event.id, set_value='START'
        endif else begin
          info.blinkon=1
          widget_control, event.id, set_value='STOP'
          WIDGET_CONTROL, event.TOP, TIMER=info.blinktime
        endelse
      end
      
      info.ui_blinkslow: if info.blinktime lt 10 then info.blinktime = info.blinktime / 0.75
      info.ui_blinkfast: if info.blinktime gt 0.2 then info.blinktime = info.blinktime * 0.75
      
      info.ui_shiftx: begin
        widget_control, info.ui_rot, get_value=rota
        *info.imageiris = rot(*info.orimageiris, rota, cubic=-0.5)
        widget_control, info.ui_shifty, get_value=shifty
        pcoeff = [event.value*(-1),0,1,0]
        qcoeff = [shifty*(-1),1,0,0]
        *info.imageiris = iris_aia_manualalign_scaleimage(INF_POLY_2D(*info.imageiris,pcoeff,qcoeff,2,cubic=-0.5))
        wset, info.winblink
        erase
        tv, *info.imageiris
        im=fltarr((size(*info.imageiris))[1],(size(*info.imageiris))[2],3)
        im[*,*,0]=*info.imageiris
        im[*,*,2]=*info.imageaia
        wset,info.winoverlay
        erase
        tv,im,true=3
      end
      
      info.ui_shifty: begin
        widget_control, info.ui_rot, get_value=rota
        *info.imageiris = rot(*info.orimageiris, rota, cubic=-0.5)
        widget_control, info.ui_shiftx, get_value=shiftx
        pcoeff = [shiftx*(-1),0,1,0]
        qcoeff = [event.value*(-1),1,0,0]
        *info.imageiris = iris_aia_manualalign_scaleimage(INF_POLY_2D(*info.imageiris,pcoeff,qcoeff,2,cubic=-0.5))
        wset, info.winblink
        erase
        tv, *info.imageiris
        im=fltarr((size(*info.imageiris))[1],(size(*info.imageiris))[2],3)
        im[*,*,0]=*info.imageiris
        im[*,*,2]=*info.imageaia
        wset,info.winoverlay
        erase
        tv,im,true=3
      end
      
      info.ui_rot: begin
        *info.imageiris = rot(*info.orimageiris, event.value, cubic=-0.5)
        widget_control, info.ui_shiftx, get_value=shiftx
        widget_control, info.ui_shifty, get_value=shifty
        pcoeff = [shiftx*(-1),0,1,0]
        qcoeff = [shifty*(-1),1,0,0]
        *info.imageiris = iris_aia_manualalign_scaleimage(INF_POLY_2D(*info.imageiris,pcoeff,qcoeff,2,cubic=-0.5))
        wset, info.winblink
        erase
        tv, *info.imageiris
        im=fltarr((size(*info.imageiris))[1],(size(*info.imageiris))[2],3)
        im[*,*,0]=*info.imageiris
        im[*,*,2]=*info.imageaia
        wset,info.winoverlay
        erase
        tv,im,true=3
      end
      
      info.ui_autoadjust: begin
        widget_control, info.ui_shiftx, get_value=shiftx
        info.shiftxold = shiftx
        widget_control, info.ui_shifty, get_value=shifty
        info.shiftyold = shifty
        widget_control, info.ui_rot, get_value=rota
        info.shiftrotold = rota
        rotalign, *info.orimageaia, *info.orimageiris, shiftx, shifty, rota
        widget_control, info.ui_rot, set_value=rota
        *info.imageiris = rot(*info.orimageiris, rota, cubic=-0.5)
        widget_control, info.ui_shiftx, set_value=shiftx
        widget_control, info.ui_shifty, set_value=shifty
        pcoeff = [shiftx*(-1),0,1,0]
        qcoeff = [shifty*(-1),1,0,0]
        *info.imageiris = iris_aia_manualalign_scaleimage(INF_POLY_2D(*info.imageiris,pcoeff,qcoeff,2,cubic=-0.5))
        wset, info.winblink
        erase
        tv, *info.imageiris
        im=fltarr((size(*info.imageiris))[1],(size(*info.imageiris))[2],3)
        im[*,*,0]=*info.imageiris
        im[*,*,2]=*info.imageaia
        wset,info.winoverlay
        erase
        tv,im,true=3
      end
      
      info.ui_undoautoadjust: begin
        widget_control, info.ui_shiftx, set_value=info.shiftxold
        widget_control, info.ui_shifty, set_value=info.shiftyold
        widget_control, info.ui_rot, set_value=info.shiftrotold
        *info.imageiris = rot(*info.orimageiris, info.shiftrotold, cubic=-0.5)
        pcoeff = [info.shiftxold*(-1),0,1,0]
        qcoeff = [info.shiftyold*(-1),1,0,0]
        *info.imageiris = iris_aia_manualalign_scaleimage(INF_POLY_2D(*info.imageiris,pcoeff,qcoeff,2,cubic=-0.5))
        wset, info.winblink
        erase
        tv, *info.imageiris
        im=fltarr((size(*info.imageiris))[1],(size(*info.imageiris))[2],3)
        im[*,*,0]=*info.imageiris
        im[*,*,2]=*info.imageaia
        wset,info.winoverlay
        erase
        tv,im,true=3
      end
      
      info.ui_expiris: begin
        info.expindiris = event.value
        iris_aia_manualalign_getimages, ptr_new(info.diris), ptr_new(info.daia), info.auxiris, info.auxaia, info.expindiris, info.iwiniris, info.iwinaia, info.shiftrot0, $
          info.datairis, info.dataaia, info.maxsize, $
          expindother=expindother, orimageiris=orimageiris, orimageaia=orimageaia, $
          timeiris=timeiris, timeaia=timeaia
        info.expindaia = expindother
        widget_control, info.ui_expaia, set_value=info.expindaia
        widget_control, info.l_timeiris, set_value='Time: '+timeiris
        widget_control, info.l_xceniris, set_value='XCEN : '+string((*info.auxiris).xcen[info.expindiris], format='(f7.2)')
        widget_control, info.l_yceniris, set_value='YCEN : '+string((*info.auxiris).ycen[info.expindiris], format='(f7.2)')
        widget_control, info.l_timeaia, set_value='Time: '+timeaia
        widget_control, info.l_xcenaia, set_value='XCEN : '+string((*info.auxaia).xcen[info.expindaia], format='(f7.2)')
        widget_control, info.l_ycenaia, set_value='YCEN : '+string((*info.auxaia).ycen[info.expindaia], format='(f7.2)')
        *info.orimageiris = orimageiris
        *info.orimageaia = orimageaia
        widget_control, info.ui_rot, get_value=rota
        *info.imageiris = rot(*info.orimageiris, rota, cubic=-0.5)
        widget_control, info.ui_shiftx, get_value=shiftx
        widget_control, info.ui_shifty, get_value=shifty
        pcoeff = [shiftx*(-1),0,1,0]
        qcoeff = [shifty*(-1),1,0,0]
        *info.imageiris = iris_aia_manualalign_scaleimage(INF_POLY_2D(*info.imageiris,pcoeff,qcoeff,2,cubic=-0.5))
        *info.imageaia = iris_aia_manualalign_scaleimage(*info.orimageaia)
        wset, info.winblink
        erase
        tv, *info.imageiris
        im=fltarr((size(*info.imageiris))[1],(size(*info.imageiris))[2],3)
        im[*,*,0]=*info.imageiris
        im[*,*,2]=*info.imageaia
        wset,info.winoverlay
        erase
        tv,im,true=3
      end
      
      info.ui_expaia: begin
        info.expindaia = event.value
        iris_aia_manualalign_getimages, ptr_new(info.diris), ptr_new(info.daia), info.auxiris, info.auxaia, info.expindaia, info.iwiniris, info.iwinaia, info.shiftrot0, $
          info.datairis, info.dataaia, info.maxsize, /aia, $
          expindother=expindother, orimageiris=orimageiris, orimageaia=orimageaia, $
          timeiris=timeiris, timeaia=timeaia
        info.expindiris = expindother
        widget_control, info.ui_expiris, set_value=info.expindiris
        widget_control, info.l_timeiris, set_value='Time: '+timeiris
        widget_control, info.l_xceniris, set_value='XCEN : '+string((*info.auxiris).xcen[info.expindiris], format='(f7.2)')
        widget_control, info.l_yceniris, set_value='YCEN : '+string((*info.auxiris).ycen[info.expindiris], format='(f7.2)')
        widget_control, info.l_timeaia, set_value='Time: '+timeaia
        widget_control, info.l_xcenaia, set_value='XCEN : '+string((*info.auxaia).xcen[info.expindaia], format='(f7.2)')
        widget_control, info.l_ycenaia, set_value='YCEN : '+string((*info.auxaia).ycen[info.expindaia], format='(f7.2)')
        *info.orimageiris = orimageiris
        *info.orimageaia = orimageaia
        widget_control, info.ui_rot, get_value=rota
        *info.imageiris = rot(*info.orimageiris, rota, cubic=-0.5)
        widget_control, info.ui_shiftx, get_value=shiftx
        widget_control, info.ui_shifty, get_value=shifty
        pcoeff = [shiftx*(-1),0,1,0]
        qcoeff = [shifty*(-1),1,0,0]
        *info.imageiris = iris_aia_manualalign_scaleimage(INF_POLY_2D(*info.imageiris,pcoeff,qcoeff,2,cubic=-0.5))
        *info.imageaia = iris_aia_manualalign_scaleimage(*info.orimageaia)
        wset, info.winblink
        erase
        tv, *info.imageiris
        im=fltarr((size(*info.imageiris))[1],(size(*info.imageiris))[2],3)
        im[*,*,0]=*info.imageiris
        im[*,*,2]=*info.imageaia
        wset,info.winoverlay
        erase
        tv,im,true=3
      end
      
      info.ui_filteriris: begin
        info.fileindiris = event.index
        info.diris = iris_sji(info.firis[info.fileindiris])
        ind = info.diris->getread_sji()
        info.iwiniris = (where(ind eq 1))[0]
        ptr_free, info.auxiris
        info.auxiris = ptr_new(info.diris->sji_info(info.iwiniris))
        widget_control, info.ui_expiris, set_slider_max=info.diris->getnexp()-1
        
        iris_aia_manualalign_getimages, ptr_new(info.diris), ptr_new(info.daia), info.auxiris, info.auxaia, info.expindaia, info.iwiniris, info.iwinaia, info.shiftrot0, $
          info.datairis, info.dataaia, info.maxsize, /aia, $
          expindother=expindother, orimageiris=orimageiris, orimageaia=orimageaia, $
          timeiris=timeiris, timeaia=timeaia
        info.expindiris = expindother
        widget_control, info.ui_expiris, set_value=info.expindiris
        widget_control, info.l_timeiris, set_value='Time: '+timeiris
        widget_control, info.l_xceniris, set_value='XCEN : '+string((*info.auxiris).xcen[info.expindiris], format='(f7.2)')
        widget_control, info.l_yceniris, set_value='YCEN : '+string((*info.auxiris).ycen[info.expindiris], format='(f7.2)')
        widget_control, info.l_timeaia, set_value='Time: '+timeaia
        widget_control, info.l_xcenaia, set_value='XCEN : '+string((*info.auxaia).xcen[info.expindaia], format='(f7.2)')
        widget_control, info.l_ycenaia, set_value='YCEN : '+string((*info.auxaia).ycen[info.expindaia], format='(f7.2)')
        ptr_free, info.orimageiris
        info.orimageiris = ptr_new(orimageiris)
        *info.orimageaia = orimageaia
        widget_control, info.ui_rot, get_value=rota
        ptr_free, info.imageiris
        info.imageiris = ptr_new(rot(*info.orimageiris, rota, cubic=-0.5))
        widget_control, info.ui_shiftx, get_value=shiftx
        widget_control, info.ui_shifty, get_value=shifty
        pcoeff = [shiftx*(-1),0,1,0]
        qcoeff = [shifty*(-1),1,0,0]
        *info.imageiris = iris_aia_manualalign_scaleimage(INF_POLY_2D(*info.imageiris,pcoeff,qcoeff,2,cubic=-0.5))
        *info.imageaia = iris_aia_manualalign_scaleimage(*info.orimageaia)
        wset, info.winblink
        erase
        tv, *info.imageiris
        im=fltarr((size(*info.imageiris))[1],(size(*info.imageiris))[2],3)
        im[*,*,0]=*info.imageiris
        im[*,*,2]=*info.imageaia
        wset,info.winoverlay
        erase
        tv,im,true=3
      end
      
      info.ui_filteraia: begin
        info.fileindaia = event.index
        info.daia = iris_sji(info.faia[info.fileindaia])
        ind = info.daia->getread_sji()
        info.iwinaia = (where(ind eq 1))[0]
        ptr_free, info.auxaia
        info.auxaia = ptr_new(info.daia->sji_info(info.iwinaia))
        widget_control, info.ui_expaia, set_slider_max=info.daia->getnexp()-1
        
        iris_aia_manualalign_getimages, ptr_new(info.diris), ptr_new(info.daia), info.auxiris, info.auxaia, info.expindiris, info.iwiniris, info.iwinaia, info.shiftrot0, $
          info.datairis, info.dataaia, info.maxsize, $
          expindother=expindother, orimageiris=orimageiris, orimageaia=orimageaia, $
          timeiris=timeiris, timeaia=timeaia
        info.expindaia = expindother
        widget_control, info.ui_expaia, set_value=info.expindaia
        widget_control, info.l_timeiris, set_value='Time: '+timeiris
        widget_control, info.l_xceniris, set_value='XCEN : '+string((*info.auxiris).xcen[info.expindiris], format='(f7.2)')
        widget_control, info.l_yceniris, set_value='YCEN : '+string((*info.auxiris).ycen[info.expindiris], format='(f7.2)')
        widget_control, info.l_timeaia, set_value='Time: '+timeaia
        widget_control, info.l_xcenaia, set_value='XCEN : '+string((*info.auxaia).xcen[info.expindaia], format='(f7.2)')
        widget_control, info.l_ycenaia, set_value='YCEN : '+string((*info.auxaia).ycen[info.expindaia], format='(f7.2)')
        *info.orimageiris = orimageiris
        ptr_free, info.orimageaia
        info.orimageaia = ptr_new(orimageaia)
        widget_control, info.ui_rot, get_value=rota
        *info.imageiris = rot(*info.orimageiris, rota, cubic=-0.5)
        widget_control, info.ui_shiftx, get_value=shiftx
        widget_control, info.ui_shifty, get_value=shifty
        pcoeff = [shiftx*(-1),0,1,0]
        qcoeff = [shifty*(-1),1,0,0]
        *info.imageiris = iris_aia_manualalign_scaleimage(INF_POLY_2D(*info.imageiris,pcoeff,qcoeff,2,cubic=-0.5))
        ptr_free, info.imageaia
        info.imageaia = ptr_new(iris_aia_manualalign_scaleimage(*info.orimageaia))
        wset, info.winblink
        erase
        tv, *info.imageiris
        im=fltarr((size(*info.imageiris))[1],(size(*info.imageiris))[2],3)
        im[*,*,0]=*info.imageiris
        im[*,*,2]=*info.imageaia
        wset,info.winoverlay
        erase
        tv,im,true=3
      end
      
      info.ui_apply: begin
        widget_control, info.ui_rot, get_value=rota0
        widget_control, info.ui_shiftx, get_value=shiftx
        shiftx = (-shiftx) * info.diris->getresx(info.iwiniris)
        widget_control, info.ui_shifty, get_value=shifty
        shifty = (-shifty) * info.diris->getresy(info.iwiniris)
        print,shiftx,shifty
        for ifile=0,N_ELEMENTS(info.faia)-1 do begin
          data=readfits(info.faia[ifile], header)
          print, gt_tagval(header,'pc1_1')
          print, gt_tagval(header,'pc1_2')
          print, gt_tagval(header,'pc2_1')
          print, gt_tagval(header,'pc2_2')
          print, gt_tagval(header,'pc3_1')
          print, gt_tagval(header,'pc3_2')
          print, gt_tagval(header,'sat_rot')
          a0 = gt_tagval(header,'sat_rot')
          shiftrot0 = (gt_tagval(header,'shiftrot', missing=0.0))[0]
          shiftx0 = (gt_tagval(header,'shiftx', missing=0.0))[0]
          shifty0 = (gt_tagval(header,'shifty', missing=0.0))[0]
          rota=a0-rota0-shiftrot0
          print,rota
          sxaddpar, header, 'crota2', rota
          sxdelpar, header, 'sat_rot'
          sxdelpar,header,'pc1_1'
          sxdelpar,header,'pc1_2'
          sxdelpar,header,'pc2_1'
          sxdelpar,header,'pc2_2'
          sxdelpar,header,'pc3_1'
          sxdelpar,header,'pc3_2'
          wcsaia = fitshead2wcs(header)
          print, wcsaia.pc[0,0]
          print, wcsaia.pc[0,1]
          print, wcsaia.pc[1,0]
          print, wcsaia.pc[1,1]
          sxaddpar, header, 'sat_rot', rota
          sxdelpar, header, 'crota2'
          sxaddpar, header, 'shiftrot', rota0*(-1)
          sxaddpar, header, 'shiftx', shiftx
          sxaddpar, header, 'shifty', shifty
          sxaddpar,header,'pc1_1',wcsaia.pc[0,0]
          sxaddpar,header,'pc1_2',wcsaia.pc[0,1]
          sxaddpar,header,'pc2_1',wcsaia.pc[1,0]
          sxaddpar,header,'pc2_2',wcsaia.pc[1,1]
          sxaddpar,header,'pc3_1',0.0
          sxaddpar,header,'pc3_2',0.0
          
          dataux1 = readfits(info.faia[ifile], haux1, ext=1)
          haux11 = fitshead2struct(haux1)
          dataux1[haux11.xcenix,*] = dataux1[haux11.xcenix,*] + (shiftx-shiftx0)
          dataux1[haux11.ycenix,*] = dataux1[haux11.ycenix,*] + (shifty-shifty0)
          sxaddpar,header,'xcen',dataux1[haux11.xcenix, N_ELEMENTS(dataux1[haux11.xcenix,*])/2]
          sxaddpar,header,'ycen',dataux1[haux11.ycenix, N_ELEMENTS(dataux1[haux11.ycenix,*])/2]
          dataux1[haux11.pc1_1ix,*] = wcsaia.pc[0,0]
          dataux1[haux11.pc1_2ix,*] = wcsaia.pc[0,1]
          dataux1[haux11.pc2_1ix,*] = wcsaia.pc[1,0]
          dataux1[haux11.pc2_2ix,*] = wcsaia.pc[1,1]
          
          dataux2 = readfits(info.faia[ifile], haux2, ext=2)

          writefits, info.faia[ifile], data, header
          writefits, info.faia[ifile], dataux1, haux1, /append
          writefits, info.faia[ifile], dataux2, haux2, /append
        endfor
        print,'saved all files'
      end
      
      info.ui_undoall: begin
        for ifile=0,N_ELEMENTS(info.faia)-1 do begin
          data=readfits(info.faia[ifile], header)
          print, gt_tagval(header,'pc1_1')
          print, gt_tagval(header,'pc1_2')
          print, gt_tagval(header,'pc2_1')
          print, gt_tagval(header,'pc2_2')
          print, gt_tagval(header,'pc3_1')
          print, gt_tagval(header,'pc3_2')
          print, gt_tagval(header,'sat_rot')
          a0 = gt_tagval(header,'sat_rot')
          shiftrot0 = (gt_tagval(header,'shiftrot', missing=0.0))[0]
          shiftx0 = (gt_tagval(header,'shiftx', missing=0.0))[0]
          shifty0 = (gt_tagval(header,'shifty', missing=0.0))[0]
          rota=a0-shiftrot0
          print,rota
          sxaddpar, header, 'crota2', rota
          sxdelpar, header, 'sat_rot'
          sxdelpar,header,'pc1_1'
          sxdelpar,header,'pc1_2'
          sxdelpar,header,'pc2_1'
          sxdelpar,header,'pc2_2'
          sxdelpar,header,'pc3_1'
          sxdelpar,header,'pc3_2'
          wcsaia = fitshead2wcs(header)
          print, wcsaia.pc[0,0]
          print, wcsaia.pc[0,1]
          print, wcsaia.pc[1,0]
          print, wcsaia.pc[1,1]
          sxaddpar, header, 'sat_rot', rota
          sxdelpar, header, 'crota2'
          sxdelpar, header, 'shiftrot'
          sxdelpar, header, 'shiftx'
          sxdelpar, header, 'shifty'
          sxaddpar,header,'pc1_1',wcsaia.pc[0,0]
          sxaddpar,header,'pc1_2',wcsaia.pc[0,1]
          sxaddpar,header,'pc2_1',wcsaia.pc[1,0]
          sxaddpar,header,'pc2_2',wcsaia.pc[1,1]
          sxaddpar,header,'pc3_1',0.0
          sxaddpar,header,'pc3_2',0.0
          
          dataux1 = readfits(info.faia[ifile], haux1, ext=1)
          haux11 = fitshead2struct(haux1)
          dataux1[haux11.xcenix,*] = dataux1[haux11.xcenix,*] - shiftx0
          dataux1[haux11.ycenix,*] = dataux1[haux11.ycenix,*] - shifty0
          sxaddpar,header,'xcen',dataux1[haux11.xcenix, N_ELEMENTS(dataux1[haux11.xcenix,*])/2]
          sxaddpar,header,'ycen',dataux1[haux11.ycenix, N_ELEMENTS(dataux1[haux11.ycenix,*])/2]
          dataux1[haux11.pc1_1ix,*] = wcsaia.pc[0,0]
          dataux1[haux11.pc1_2ix,*] = wcsaia.pc[0,1]
          dataux1[haux11.pc2_1ix,*] = wcsaia.pc[1,0]
          dataux1[haux11.pc2_2ix,*] = wcsaia.pc[1,1]
          
          dataux2 = readfits(info.faia[ifile], haux2, ext=2)
          
          writefits, info.faia[ifile], data, header
          writefits, info.faia[ifile], dataux1, haux1, /append
          writefits, info.faia[ifile], dataux2, haux2, /append
        endfor
        print,'saved all files'
        widget_control, info.ui_rot, set_value=0
        widget_control, info.ui_shiftx, set_value=0
        widget_control, info.ui_shifty, set_value=0
        *info.imageiris = iris_aia_manualalign_scaleimage(*info.orimageiris)
        wset, info.winblink
        erase
        tv, *info.imageiris
        im=fltarr((size(*info.imageiris))[1],(size(*info.imageiris))[2],3)
        im[*,*,0]=*info.imageiris
        im[*,*,2]=*info.imageaia
        wset,info.winoverlay
        erase
        tv,im,true=3
      end
      
      else:
    endcase
    
  endelse
  
  if ~destroyed then widget_control, event.top, set_uvalue=info, /no_copy
end





; Main program
pro iris_aia_manualalign, OBSid, irisdir=irisdir, aiadir=aiadir

  constants = obj_new('IRISsim_constants')

  if N_PARAMS() eq 0 then begin
    if ~keyword_set(irisdir) || ~keyword_set(aiadir) then begin
      print, 'usage: iris_aia_manualalign, OBSid[, irisdir=irisdir, aiadir=aiadir]'
      print, 'need either OBSid and optionally irisdir or aiadir'
      print, 'OR irisdir and aiadir'
      return
    endif
  endif else begin
    if ~keyword_set(irisdir) then begin
      rootl2=getenv('IRIS_DATA')+'/level2/'
      if ~file_test(rootl2, /directory) then begin
        rootl2=constants->get_data_path_uio_l2();UIO
        if ~file_test(rootl2, /directory) then begin
          rootl2=constants->get_data_path_lmsal_l2();LMSAL
          if ~file_test(rootl2, /directory) then begin
            print, 'could not find root directory to iris level 2 data'
            return
          endif
        endif
      endif
      yyyy=strmid(OBSid,0,4)
      mm=strmid(OBSid,4,2)
      dd=strmid(OBSid,6,2)
      irisdir=rootl2+yyyy+'/'+mm+'/'+dd+'/'+OBSid+'/'
    endif
    if ~keyword_set(aiadir) then begin
      rootl3=getenv('IRIS_DATA')+'/level3/'
      if ~file_test(rootl3, /directory) then begin
        rootl3=constants->get_data_path_uio_l3()
        if ~file_test(rootl3, /directory) then begin
          rootl3=constants->get_data_path_lmsal_l3()
          if ~file_test(rootl3, /directory) then begin
            print, 'could not find root directory to iris level 3 data'
            return
          endif
        endif
      endif
      yyyy=strmid(OBSid,0,4)
      mm=strmid(OBSid,4,2)
      dd=strmid(OBSid,6,2)
      aiadir=rootl3+yyyy+'/'+mm+'/'+dd+'/'+OBSid+'/'
    endif
  endelse
  
  

  
  
  
  
  firis = file_search(irisdir+'iris_l2_*_SJI_*.fits', count=nfiris)
  if nfiris eq 0 then begin
    print, 'did not find any iris SJI l2 files in'
    print, irisdir
    return
  endif
  faia = file_search(aiadir+'aia_l2_*.fits', count=nfaia)
  if nfaia eq 0 then begin
    print, 'did not find any aia l2 files in'
    print, aiadir
    return
  endif
  
  help,firis,faia
  print,firis
  print,faia
  
  mreadfits_header, firis, hiris, only_tags='tdesc1'
  filtersiris = hiris.tdesc1
  ind = where(strmatch(filtersiris,'*1400*') eq 1, count)
  if count eq 0 then ind = where(strmatch(filtersiris,'*1300*') eq 1, count)
  if count eq 0 then ind = 0
  fileindiris = ind[0]
  diris = iris_sji(firis[fileindiris])
  expindiris = 0
  
  mreadfits_header, faia, haia, only_tags='tdesc1'
  filtersaia = haia.tdesc1
  ind = where(strmatch(filtersaia,'*1600*') eq 1, count)
  if count eq 0 then ind = where(strmatch(filtersaia,'*1700*') eq 1, count)
  if count eq 0 then ind = 0
  fileindaia = ind[0]
  daia = iris_sji(faia[fileindaia])
  
  ind = diris->getread_sji()
  iwiniris = (where(ind eq 1))[0]
  ind = daia->getread_sji()
  iwinaia = (where(ind eq 1))[0]
  auxiris = diris->sji_info(iwiniris)
  auxaia = daia->sji_info(iwinaia)
  
  ;check whether a change has been applied already
  shiftrot0 = daia->getinfo('SHIFTROT', iwinaia, missing=0.0, found=found)
  if found then shiftrot = (-shiftrot0) else shiftrot = 0.6
  shiftx0 = daia->getinfo('SHIFTX', iwinaia, missing=0.0, found=found)
  if found then shiftx = (-shiftx0) / diris->getresx(iwiniris) else shiftx=0.0
  shifty0 = daia->getinfo('SHIFTY', iwinaia, missing=0.0, found=found)
  if found then shifty = (-shifty0) / diris->getresy(iwiniris) else shifty=0.0
  auxaia.xcen = auxaia.xcen - shiftx0
  auxaia.ycen = auxaia.ycen - shifty0
  
  datairis = diris->getvar(iwiniris)
  dataaia = daia->getvar(iwinaia)


  screen = get_screen_size()
  maxsize = [(screen[0]-200)/2, screen[1]-400]
  
  iris_aia_manualalign_getimages, ptr_new(diris), ptr_new(daia), ptr_new(auxiris), ptr_new(auxaia), expindiris, iwiniris, iwinaia, shiftrot0, $
    ptr_new(datairis), ptr_new(dataaia), maxsize, $
    expindother=expindother, orimageiris=orimageiris, orimageaia=orimageaia, $
    timeiris=timeiris, timeaia=timeaia
  expindaia = expindother
  imageaia = iris_aia_manualalign_scaleimage(orimageaia)
  imageiris = rot(orimageiris, shiftrot, cubic=-0.5)
  pcoeff = [shiftx*(-1),0,1,0]
  qcoeff = [shifty*(-1),1,0,0]
  imageiris = iris_aia_manualalign_scaleimage(INF_POLY_2D(imageiris,pcoeff,qcoeff,2,cubic=-0.5))
  
  
  
  
  ; MainWindow ; Base-Widget
  MainWindow = WIDGET_BASE(/Column, title='Align IRIS and AIA data', XOffset=50, YOffset=20)
  
  Base20 = WIDGET_BASE(MainWindow, /Row, frame=1)
  
  Base10 = WIDGET_BASE(Base20, /Column, frame=1)
  label = WIDGET_LABEL(Base10, value='IRIS dir :  '+irisdir, /align_left)
  label = WIDGET_LABEL(Base10, value='AIA dir  :  '+aiadir, /align_left)
  label = WIDGET_LABEL(Base10, value='OBS ID   :  '+diris->getinfo('OBSID'), /align_left)
  label = WIDGET_LABEL(Base10, value='Start    :  '+diris->getinfo('STARTOBS'), /align_left)
  label = WIDGET_LABEL(Base10, value='End      :  '+diris->getinfo('ENDOBS'), /align_left)
  
  Base22 = WIDGET_BASE(Base20, /Column, frame=1)
  label = WIDGET_LABEL(Base22, value='IRIS')
  ui_filteriris = WIDGET_COMBOBOX(Base22, value=filtersiris)
  widget_control, ui_filteriris, set_combobox_select=fileindiris
  ui_expiris = WIDGET_SLIDER(Base22, title='Exposure', minimum=0, maximum=diris->getnexp()-1, value=expindiris)
  l_timeiris = WIDGET_LABEL(Base22, value='Time: '+timeiris, /align_left)
  Base23 = WIDGET_BASE(Base22, /Row)
  l_xceniris = WIDGET_LABEL(Base23, value='XCEN : '+string(auxiris.xcen[expindiris], format='(f7.2)'), /align_left)
  l_fovxiris = WIDGET_LABEL(Base23, value='   FOV X : '+string(diris->getfovx(iwiniris), format='(f7.2)'), /align_left)
  Base24 = WIDGET_BASE(Base22, /Row)
  l_yceniris = WIDGET_LABEL(Base24, value='YCEN : '+string(auxiris.ycen[expindiris], format='(f7.2)'), /align_left)
  l_fovyiris = WIDGET_LABEL(Base24, value='   FOV Y : '+string(diris->getfovy(iwiniris), format='(f7.2)'), /align_left)
  l_rotiris = WIDGET_LABEL(Base22, value='Rotation : '+string(diris->getinfo('SAT_ROT'), format='(f7.2)'), /align_left)
  
  Base26 = WIDGET_BASE(Base20, /Column, frame=1)
  label = WIDGET_LABEL(Base26, value='AIA')
  ui_filteraia = WIDGET_COMBOBOX(Base26, value=filtersaia)
  widget_control, ui_filteraia, set_combobox_select=fileindaia
  ui_expaia = WIDGET_SLIDER(Base26, title='Exposure', minimum=0, maximum=daia->getnexp()-1, value=expindaia)
  l_timeaia = WIDGET_LABEL(Base26, value='Time: '+timeaia, /align_left)
  Base27 = WIDGET_BASE(Base26, /Row)
  l_xcenaia = WIDGET_LABEL(Base27, value='XCEN : '+string(auxaia.xcen[expindaia], format='(f7.2)'), /align_left)
  l_fovxaia = WIDGET_LABEL(Base27, value='   FOV X : '+string(daia->getfovx(iwinaia), format='(f7.2)'), /align_left)
  Base28 = WIDGET_BASE(Base26, /Row)
  l_ycenaia = WIDGET_LABEL(Base28, value='YCEN : '+string(auxaia.ycen[expindaia], format='(f7.2)'))
  l_fovyaia = WIDGET_LABEL(Base28, value='   FOV Y : '+string(daia->getfovy(iwinaia), format='(f7.2)'), /align_left)
  l_rotaia = WIDGET_LABEL(Base26, value='Rotation : '+string(daia->getinfo('SAT_ROT')+shiftrot, format='(f7.2)'), /align_left)

  
  Base30 = WIDGET_BASE(MainWindow, /Column, frame=1)
  Base30a = WIDGET_BASE(Base30, /Row)
  ui_shiftx = CW_FSLIDER(Base30a, minimum=-50, maximum=50, scroll=0.5, /edit, title='X-Shift', xsize=400, value=shiftx)
  ui_shifty = CW_FSLIDER(Base30a, minimum=-50, maximum=50, scroll=0.5, /edit, title='Y-Shift', xsize=400, value=shifty)
  ui_rot = CW_FSLIDER(Base30a, minimum=-20, maximum=20, scroll=0.5, /edit, title='Rotation', xsize=400, value=shiftrot)
  ui_autoadjust = WIDGET_BUTTON(Base30a, value='Align automatically')
  ui_undoautoadjust = WIDGET_BUTTON(Base30a, value='Undo Auto-Alignment')
  Base31 = WIDGET_BASE(Base30, /Row)
  Base32 = WIDGET_BASE(Base31, /Column, frame=1)
  blink = widget_draw(base32, xsize=(size(imageiris))[1], ysize=(size(imageiris))[2], retain=2, frame=1)
  Base33 = WIDGET_BASE(base32, /row)
  ui_blinkstart = WIDGET_BUTTON(base33, value='START')
  ui_blinkslow = WIDGET_BUTTON(base33, value='SLOWER')
  ui_blinkfast = WIDGET_BUTTON(base33, value='FASTER')
  
  Base36 = WIDGET_BASE(Base31, /Column, frame=1)
  overlay = widget_draw(base36, xsize=(size(imageiris))[1], ysize=(size(imageiris))[2], retain=2, frame=1)
  
  ui_apply = widget_button(MainWindow, value='Apply Transformations to AIA data sets')
  ui_undoall = widget_button(MainWindow, value='Undo all Transformations on AIA data sets')

  
  widget_control, MainWindow, /realize
  xmanager, 'iris_aia_manualalign', MainWindow, /no_block
  
  widget_control, blink, get_value=winblink
  widget_control, overlay, get_value=winoverlay
  
  
  info = { $
    firis:firis, $
    fileindiris:fileindiris, $
    diris:diris, $
    expindiris:expindiris, $
    iwiniris:iwiniris, $
    auxiris:ptr_new(auxiris), $
    datairis:ptr_new(datairis), $
    faia:faia, $
    fileindaia:fileindaia, $
    daia:daia, $
    expindaia:expindaia, $
    iwinaia:iwinaia, $
    auxaia:ptr_new(auxaia), $
    dataaia:ptr_new(dataaia), $
    maxsize:maxsize, $
    ui_filteriris:ui_filteriris, $
    ui_expiris:ui_expiris, $
    l_timeiris:l_timeiris, $
    l_xceniris:l_xceniris, $
    l_yceniris:l_yceniris, $
    l_fovxiris:l_fovxiris, $
    l_fovyiris:l_fovyiris, $
    l_rotiris:l_rotiris, $
    ui_filteraia:ui_filteraia, $
    ui_expaia:ui_expaia, $
    l_timeaia:l_timeaia, $
    l_xcenaia:l_xcenaia, $
    l_ycenaia:l_ycenaia, $
    l_fovxaia:l_fovxaia, $
    l_fovyaia:l_fovyaia, $
    l_rotaia:l_rotaia, $
    ui_shiftx:ui_shiftx, $
    ui_shifty:ui_shifty, $
    ui_rot:ui_rot, $
    shiftrot0:shiftrot0, $
    shiftxold:shiftx, $
    shiftyold:shifty, $
    shiftrotold:shiftrot, $
    ui_autoadjust:ui_autoadjust, $
    ui_undoautoadjust:ui_undoautoadjust, $
    ui_blinkstart:ui_blinkstart, $
    ui_blinkslow:ui_blinkslow, $
    ui_blinkfast:ui_blinkfast, $
    blinktime:0.75, $
    blinkon:0b, $
    blinkiris:1b, $
    winblink:winblink, $
    winoverlay:winoverlay, $
    orimageiris:ptr_new(orimageiris), $
    orimageaia:ptr_new(orimageaia), $
    imageiris:ptr_new(imageiris), $
    imageaia:ptr_new(imageaia), $
    ui_apply:ui_apply, $
    ui_undoall:ui_undoall}
    
  widget_control, MainWindow, set_uvalue=info, /no_copy
  
  wset, winblink
  tvscl,imageiris
  
  im=fltarr((size(imageiris))[1],(size(imageiris))[2],3)
  im[*,*,0]=imageiris
  im[*,*,2]=imageaia
  wset,winoverlay
  tv,im,true=3
end
