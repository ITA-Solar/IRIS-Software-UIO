pro IRISsim_paramsave, default, info

; $Id: irissim_paramsave.pro,v 1.4 2013/05/05 14:59:43 mawiesma Exp $  ;

  ;widget_control, info.textOBSfile, get_value=OBSfile
  widget_control, info.textFRMdir, get_value=FRMdir
  widget_control, info.textFDBdir, get_value=FDBdir
  widget_control, info.textCRSdir, get_value=CRSdir
  widget_control, info.textfuvdir, get_value=fuvdir
  widget_control, info.textfuvfile, get_value=fuvfile
  widget_control, info.textnuvdir, get_value=nuvdir
  widget_control, info.textnuvfile, get_value=nuvfile
  widget_control, info.textdtime, get_value=dtime
  widget_control, info.textdtimend, get_value=dtimend
  widget_control, info.bgoutput1, get_value=output1
  widget_control, info.bgoutput2, get_value=output2
  widget_control, info.textx, get_value=maxWINsizeX
  widget_control, info.texty, get_value=maxWINsizeY
  widget_control, info.textdest, get_value=dest
  widget_control, info.textqtloc, get_value=qtloc
  widget_control, info.textovsji, get_value=ovsji
  widget_control, info.textovtype, get_value=ovtype
  widget_control, info.textthresov, get_value=thresov
  widget_control, info.textthressji, get_value=thressji
  widget_control, info.textthresspec, get_value=thresspec
;  widget_control, info.bgdebug, get_value=debug
;  widget_control, info.textdebug, get_value=debugnr
;  widget_control, info.textdebugt, get_value=debugt
  
  maxWINsize=[maxWINsizeX[0], maxWINsizeY[0]]
  saveplot = output1[0]
  makemovie = output1[1]
  playmovie = output1[2]
  savefitsfiles = output2[0]
  copyxml = output2[1]
  logcsv = output2[2]
  logtxt = output2[3]
  
  params = { $
    version:3, $
    ;OBSfile:OBSfile[0], $
    fuvdir:fuvdir[0], $
    fuvfile:fuvfile[0], $
    nuvdir:nuvdir[0], $
    nuvfile:nuvfile[0], $
    dtime:dtime[0], $
    dtimend:dtimend[0], $
    dest:dest[0], $
    qtloc:qtloc[0], $
    ovsji:ovsji[0], $
    ovtype:ovtype[0], $
    maxWINsize:maxWINsize, $
    thresov:thresov[0], $
    thressji:thressji[0], $
    thresspec:thresspec[0], $
;    debug:debug[0], $
;    debugnr:debugnr[0], $
;    debugt:debugt[0], $
    savefitsfiles:savefitsfiles, $
    saveplot:saveplot, $
    makemovie:makemovie, $
    playmovie:playmovie, $
    copyxml:copyxml, $
    logcsv:logcsv, $
    logtxt:logtxt, $
    FRMdir:FRMdir[0], $
    FDBdir:FDBdir[0], $
    CRSdir:CRSdir[0] $
    }
    
    
  if default then begin
    file = IRISsim_appReadme() + '/irissim_defaultparam.sav'
    save, params, filename=file
  endif else begin
    file = dialog_pickfile(/write, default_extension='sav')
    if file ne '' then save, params, filename=file
  endelse
  print, 'file saved'
end
