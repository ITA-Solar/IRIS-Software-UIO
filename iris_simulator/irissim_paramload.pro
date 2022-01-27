pro IRISsim_paramload, default, info

  ; $Id: irissim_paramload.pro,v 1.4 2013/05/05 14:59:43 mawiesma Exp $  ;

  if default then begin
    file = IRISsim_appReadme() + '/irissim_defaultparam.sav'
    if ~FILE_TEST(file) then file = programrootdir()+'irissim_defaultparam.sav'
    restore, filename=file
  endif else begin
    file = dialog_pickfile(/read, filter=['*sav', '*.*'])
    if file ne '' then restore, filename=file $
    else return
  endelse
  
  index = Where(Tag_Names(params) EQ 'VERSION', count)
  if count eq 0 then $
    ;version 1
    params = CREATE_STRUCT(params, 'VERSION', 1)

  if params.version le 2 then begin
    FRMdir=''
    FDBdir=''
    CRSdir=''
  endif
  if params.version eq 3 then begin
    FRMdir=params.FRMdir
    FDBdir=params.FDBdir
    CRSdir=params.CRSdir
  endif
    
    
  output1=intarr(3)
  output2=intarr(4)
  
  output1[0] = params.saveplot
  output1[1] = params.makemovie
  output1[2] = params.playmovie
  output2[0] = params.savefitsfiles
  output2[1] = params.copyxml
  output2[2] = params.logcsv
  output2[3] = params.logtxt
  
  cd, current=curdir
  curdir=curdir+'/'
  if params.fuvdir eq '' then params.fuvdir=curdir
  if params.nuvdir eq '' then params.nuvdir=curdir
  if params.dest eq '' then params.dest=curdir
  
  ;widget_control, info.textOBSfile, set_value=params.OBSfile
  widget_control, info.textFRMdir, set_value=FRMdir
  widget_control, info.textFDBdir, set_value=FDBdir
  widget_control, info.textCRSdir, set_value=CRSdir
  widget_control, info.textfuvdir, set_value=params.fuvdir
  widget_control, info.textfuvfile, set_value=params.fuvfile
  widget_control, info.textnuvdir, set_value=params.nuvdir
  widget_control, info.textnuvfile, set_value=params.nuvfile
  widget_control, info.textdtime, set_value=params.dtime
  widget_control, info.textdtimend, set_value=params.dtimend
  widget_control, info.bgoutput1, set_value=output1
  widget_control, info.bgoutput2, set_value=output2
  widget_control, info.textdest, set_value=params.dest
  widget_control, info.textqtloc, set_value=params.qtloc
  widget_control, info.textovsji, set_value=params.ovsji
  widget_control, info.textovtype, set_value=params.ovtype
  widget_control, info.textx, set_value=params.maxWINsize[0]
  widget_control, info.texty, set_value=params.maxWINsize[1]
  widget_control, info.textthresov, set_value=params.thresov
  widget_control, info.textthressji, set_value=params.thressji
  widget_control, info.textthresspec, set_value=params.thresspec
;  widget_control, info.bgdebug, set_value=params.debug
;  widget_control, info.textdebug, set_value=params.debugnr
;  widget_control, info.textdebugt, set_value=params.debugt
  
end
