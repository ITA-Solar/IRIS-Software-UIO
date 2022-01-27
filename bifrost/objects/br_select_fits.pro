;+
; NAME:
;	BR_SELECT_FITS
;
;
; PURPOSE:
;	select one fits file from directory
;
;
; CATEGORY:
;	Bifrost
;
;
; CALLING SEQUENCE:
;	BR_SELECT_FITS,fits
;
;
; OUTPUTS:
;	fits - root name
;
;
; PROCEDURE:
;	searches through directory for *_[0-9][0-9][0-9].fits files and takes * as
;	root name. If several matches, prompts the user to choose one
;	of them
;
;
; MODIFICATION HISTORY:
; $Id$
;-
pro br_select_fits,fits

if(n_params() lt 1) then begin
  message,'syntax: br_select_fits,fits',/info
  return
endif

f=file_search('*[0-9][0-9][0-9].fits',count=nfiles) ; find all .fits files
if(nfiles eq 0) then begin                         ; no files found, return
  message,'no fits files found',/info
  fits=''
  return
endif

nroots=1
ic=strpos(f[0],'.fits')-3
if(strmid(f[0],ic-2,1) eq '_') then begin
   fits=strmid(f[0],0,ic-2) 
endif else begin
   if(strmid(f[0],ic-1,1) eq '_') then fits=strmid(f[0],0,ic-1) else fits=strmid(f[0],0,ic)
endelse

iv=strpos(fits,'_',/reverse_search) 
if(iv ne -1) then begin
   fits=strmid(f[0],0,iv) 
endif 

roots=[fits]
for i=1,nfiles-1 do begin                          ; find unique root names
  ic=strpos(f[i],'.fits')-3

  if(strmid(f[i],ic-2,1) eq '_') then begin
     fits=strmid(f[i],0,ic-2) 
  endif else begin
     if(strmid(f[i],ic-1,1) eq '_') then fits=strmid(f[i],0,ic-1) else fits=strmid(f[i],0,ic)
  endelse

  iv=strpos(fits,'_',/reverse_search) 
  if(iv ne -1) then begin
     fits=strmid(f[i],0,iv) 
  endif 

  if(fits ne roots[nroots-1]) then begin       ; roots are sorted, only one test
    nroots=nroots+1
    roots=[roots,fits]
  endif
endfor

case nroots of $
  1: begin
       fits=roots[0]
     end
else: begin
       for i=0,nroots-1 do begin
         print,i,' ',roots[i]
       endfor
       read,'give fits number: ',i
       fits=roots[i]
     end
endcase

end
