;+
; NAME:
;	BR_GETSNAPIND
;
;
; PURPOSE:
;	Get indices of snap files for given idlparam root
;
;
; CATEGORY:
;	Bifrost
;
;
; CALLING SEQUENCE:
;	BR_GETSNAPIND,idlparam,snaps ,/idl
;
;
; INPUTS:
;	idlparam - root name of snap files
;
; KEYWORD PARAMETERS:
;	idl      - use .idl files instead of .snap files
;
; OUTPUTS:
;	snaps - indices of snap files
;
;
; RESTRICTIONS:
;	will not include the isnap=0 file (idlparam.idl file)
;
;
; PROCEDURE:
;	uses file_search
;
;
; MODIFICATION HISTORY:
; $Id$
;-
pro br_getsnapind,idlparam,snaps ,idl=idl

if(n_params() lt 2) then begin
  message,'syntax: br_getsnapind,idlparam,snaps ,/idl',/info
  return
endif

f1=file_search(idlparam+'_[0-9][0-9][0-9].idl',count=count1)
if(count1 gt 0) then ff=f1
f2=file_search(idlparam+'_[0-9][0-9][0-9][0-9].idl',count=count2)
if(count2 gt 0) then begin
  if(count1 gt 0) then ff=[ff,f2] else ff=f2
endif
ntime=count1+count2
if(ntime gt 0) then begin
  k0=strlen(idlparam)+1  ; new naming convention
endif else begin
  f1=file_search(idlparam+'[0-9][0-9][0-9].idl',count=count1)
  if(count1 gt 0) then ff=f1
  f2=file_search(idlparam+'[0-9][0-9][0-9][0-9].idl',count=count2)
  if(count2 gt 0) then begin
    if(count1 gt 0) then ff=[ff,f2] else ff=f2
  endif
  ntime=count1+count2
  if(ntime eq 0) then begin
    message,'no .idl files found',/info
    return
  endif
  k0=strlen(idlparam)
endelse

if(keyword_set(idl)) then begin
  snaps=intarr(ntime)
  for i=0,ntime-1 do begin
    snaps[i]=fix(strmid(ff[i],k0,strlen(ff[i])-k0))
  endfor
endif else begin
  snaps=indgen(ntime)
  j=0
  for i=0,ntime-1 do begin
    snap_nr=fix(strmid(ff[i],k0,strlen(ff[i])-k0))
    dum=file_search(idlparam+'_'+br_string3(snap_nr)+'.snap',count=count)
    if(count eq 0) then dum=file_search(idlparam+br_string3(snap_nr)+'.snap',count=count)
    if(count eq 1) then begin
      snaps[j]=snap_nr
      j=j+1
    endif
  endfor
  snaps=snaps[0:j-1]
endelse

end
