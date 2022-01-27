function iris_sot_umodes, time0, time1, cat, info=info, $
   mcount=mcount , taglist=taglist, incat=incat, _extra=_extra, $
   display=display, nottest=notest , zbuffer=zbuffer,imapc=imapc, $
   interactive=interactive, files=files, where_mode=where_mode, url=url, $
   add_pointing=add_pointing, topn_modes=topn_modes, $
   iris_start=iris_start, iris_end=iris_end, iris_obsid=iris_obsid
;+
;   Name: sot_umodes
;
;   Purpose: return string array of uniq ssot "modes" 
;
;   Input Paramters:
;      time0,time1 - time range of interest (->sxi_cat)  
;      Optionally, if "time0" are catalog structures, use those verbatim
;
;   Output:
;      function returns string array of uniq "modes"
;      (if /FILES set, implies interactive and return file list instead)
;
;   Output Parameters
;      cat - catalog structures for input time range
;            (may be passed in via INCAT to optimize subsequent searches
;             over the same records; e.g. no reread of catalog)
;
;   Keyword Parameters
;      taglist (in) - optional user supplied taglist to define a mode
;                default='img_code,wavelnth,f1filter,f2filter,exptime'
;      incat (in) - vector of sxi structures - in lieu of times (no cat re-read)
;                   e.g. if you have read the catalog or fits 'index' outside 
;      mcount (out) - number of matches per mode; long(n_elements umodes)
;      info (out) - mode summary for All catalog records 
;      _extra - unspecified keywords -> sot_cat.pro
;      interactive (switch) - if set, present menu selection
;                 (see ssw_uniq_modes.pro header for calling context)
;      files (switch) - if set & interactive return implied file list instead 
;                      of mode list  (implies /INTERACTIVE)
;      url (switch) - if set, return implied urls (implies /FILES)
;      display (switch) - if set, display cadence graphic 
;      zbuffer (switch) - implies display, but -> Zbuffer
;      topn_modes - only return/display the "popular" modes, where 
;                   popular is the top TOPN_MODES
;      no_window (switch) - Don't create window when DISPLAY (assume user win) 
;      iris_start - Start of the IRIS OBS
;      iris_end - End of the IRIS OBS
;      iris_obsid - IRIS OBS ID
;
;   Calling Sequence:
;      IDL>modes=sot_umodes(time0,time1,mcount=mc, info=info)
;      IDL>modes=sot_umodes(time0,time1,mcount=mc, /interactive)
;      IDL>modes=sot_umodes(incat=catalog,/display) ; user supplied catalog instead 
;
;   Calling Example w/display (show cadence plot per mode)
;   IDL> sot_cat,'15-feb-2007','17-feb-2007',catalog
;   IDL> um=sot_umodes(catalog,/display)
;
;   History:
;      31-oct-2006 - S.L.Freeland - from 'sxi_modes' but
;                    now via ssw_uniq_modes.pro
;       6-nov-2006 - S.L.Freeland - add some doc & interactive keyword
;       7-nov-2006 - S.L.Freeland - add /FILES keyword&function
;       6-apr-2007 - S.L.FReeland - add /ADD_POINTING keyword&function
;      26-jun-2007 - S.L.Freeland - allow catalog input via positional parameter
;      15-nov-2007 - S.L.Freeland - add TOPN_MODES & NO_WINDOW
;       9-nov-2015 - M.Wiesmann - copied from sot_umodes
;                   added iris_start, iris_end and iris_obsid, and displays those as well
;  
;    Side Effects:
;       If /DISPLAY is set, will clobber existing color table
;
;    Note: if /DISPLAY is set and output plot is mostly black, then try:
;       IDL> device,decompose=0
;       IDL> um=sot_umodes(...,/display) ; retry
;
; $Id: iris_sot_umodes.pro,v 1.3 2015/12/02 14:28:44 mawiesma Exp $  ;
;-
;
zbuffer=keyword_set(zbuffer)
display=keyword_set(display) or zbuffer
files=keyword_set(files) or keyword_set(url)
interactive=files or keyword_set(interactive)

case 1 of 
   data_chk(incat,/struct): cat = incat ; user supplied
   data_chk(time0,/struct): cat=time0   ; cat via positional param
   n_params() ge 2: begin 
      sot_cat,time0,time1,cat,  _extra=_extra
      if n_elements(cat) lt 2 then begin 
         box_message,'No records in your time range'
         return,-1 ; !!! Early Exit
      endif 
   endcase 
   else: begin 
       box_message,'Require input time range...
      return, -1
   endcase
endcase

if n_elements(taglist) eq 0 then taglist=$
   'obs_type,wave,naxis1,naxis2'

if keyword_set(add_pointing) then begin 
   car=ssw_index2carr(cat,/add,/fast,/round)
   cat=rep_tag_value(cat,cat.car_lon/10,'car_lon') ; allow some slop...
   taglist=taglist+',car_lon,hel_lat'
endif

umodes=ssw_uniq_modes(cat,taglist,mcount=mcount,info=info, $
   interactive=interactive, where_mode=where_mode)
nmodes=n_elements(umodes)
if keyword_set(topn_modes) then begin 
   order=sort(mcount)
   topn=last_nelem(order,topn_modes)
   umodes=umodes(topn)
   mcount=mcount(topn)
   nmodes=n_elements(umodes)
endif

if keyword_set(display) then begin
   if 1-keyword_set(no_window) then $
       wdef,xx,1024,600, zbuffer=zbuffer   ; 1280,900 - todo autosize to NMODES 
   linecolors
   nsmodes=n_elements(umodes)
   carr=[4,2,9,5,7]
   sarr=['??','SP','CT','FG']
   first=strmid(strtrim(umodes,2),0,2)
   tzero=reltime(cat(0).date_obs,days=-.5,out='ccsds')
   utplot,[tzero,last_nelem(cat.date_obs)],[0,nsmodes],$
      /nodata,/xstyle,ystyle=5, back=11, $
      title='Hinode SOT Modes '+ $
      'MODE=uniq(' + taglist + ')
   for i=0,nsmodes-1 do begin 
      ss=ssw_uniq_modes(cat,taglist,where_mode=umodes(i))
       color=carr((where(first(i) eq sarr))+1)
       if strpos(umodes(i),'TF') ne -1 then color=4
       evt_grid,cat(ss).date_obs,ticklen=.01,tickpos=.1+(i*.03), $ ;.013
          color=color
       evt_grid,cat(ss(0)).date_obs,/no_blank,ticklen=.000001,labpos=.1+(i*.03), $
          label=strcompress(umodes(i)), labcolor=color, $
          labsize=([.8,.7])(zbuffer) ,$
          align=1,/noarrow, imap=strpos(umodes(i),'CT ref') ne -1, $
          imagemap_coord=imapc
   endfor
   if keyword_set(iris_start) && keyword_set(iris_end) then begin
      if ~keyword_set(iris_obsid) then iris_obsid=''
      color=3
       evt_grid,[iris_start],ticklen=0.022+nsmodes*.03,tickpos=0.1+nsmodes*.015, $ ;.013
          color=color
       evt_grid,[iris_end],ticklen=0.022+nsmodes*.03,tickpos=0.1+nsmodes*.015, $ ;.013
          color=color
       evt_grid,iris_start,/no_blank,ticklen=.000001,labpos=.1+(nsmodes*.03), $
          label='IRIS OBS '+iris_obsid, labcolor=color, $
          labsize=([.8,.7])(zbuffer) ,$
          align=1,/noarrow
   endif
endif

if files then begin  ; user wants FILELIST, not UMODES returned...
  umodes=sot_cat2files(cat(umodes),url=url)
endif
   
return,umodes
end 
