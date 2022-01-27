;+
; NAME:
;       IRIS_SCALE
;
; PURPOSE:
;       Read an IRIS floating-point FITS file and write a scaled
;       integer FITS FILE
;
; CATEGORY:
;       IRIS
;
; CALLING SEQUENCE:
;       IRIS_SCALE,filein,fileout ,/silent,/toeof
;
; INPUTS:
;       filein  - input file name containing floating-point IRIS data
;
; KEYWORD PARAMETERS:
;       toeof   - read to EOF instead of finding the number of
;                 extensions from file header
;       silent  - do not print version
;
; OUTPUTS:
;       fileout - output file name 
;
; SIDE EFFECTS:
;       if /toeof: readfits prints "EOF encountered attempting to read extension xx"
;
; MODIFICATION HISTORY:
;       1.1  2013/10/03 matsc   First version
;       1.2  2013/10/03 matsc   checks file type to avoid EOF message,
;                               old behaviour with /toeof
;-
pro iris_scale,filein,fileout ,silent=silent,toeof=toeof

verstring="$Id: iris_scale.pro,v 1.2 2013/10/03 21:42:48 matsc Exp $"
if(~keyword_set(silent)) then begin
  ic=strpos(verstring,'pro,v')+6
  message,'version '+strmid(verstring,ic,strlen(verstring)-ic-6),/info
endif
if(n_params() lt 2) then begin
  message,'Syntax: iris_scale,filein,fileout ,/silent,/toeof',/info
  return
endif

; check inputs

if(n_elements(filein) eq 0) or (n_elements(fileout) eq 0) then begin
  message,'both filein and fileout has to be given',/info
  message,'Syntax: iris_scale,filein,fileout ,/silent,/toeof',/info
  return
endif
if(filein eq fileout) then begin
  message,'filein and fileout cannot be identical',/info
  return
endif
dum=file_search(filein,count=count)
if(count ne 1) then begin
  message,'filein does not exist: '+filein,/info
  return
endif

bzero=7992
bscale=0.25
error=0
iext=0
next=1
while(error eq 0) and (iext le next) do begin
  data=readfits(filein,hdr,ext=iext,/silent)    ; read data
  if(iext eq 0) then begin
    if(fxpar(hdr,'NAXIS') eq 0) then begin      ; special case for raster level2 files, data in extensions
      next_s=fxpar(hdr,'NWIN')                   ; number of extensions with data to be scaled
      next=next_s+2                              ; total number of extensions
    endif else begin 
      next_s=0
      case fxpar(hdr,'LVL_NUM') of
        2.0 : next=2 
        3.0 : next=3
        else: next=0
      endcase
    endelse
    if(keyword_set(toeof)) then next=1000
  endif
  if(n_elements(data) eq 1) then begin          ; check if EOF
    if(data eq (-1)) then begin
      if(iext gt 0) then error=1
    endif
  endif
  if(error eq 0) then begin                     ; not EOF                    
    if(size(data,/tname) eq 'FLOAT') and (iext le next_s) then begin   ; scale data
      sxaddpar,hdr,'BZERO',bzero,' True_value = BZERO + BSCALE*Array_value',after='BTYPE'
      sxaddpar,hdr,'BSCALE',bscale,' True_value = BZERO + BSCALE*Array_value',after='BZERO',format="f4.2"
      sxaddpar,hdr,'BITPIX',16,' Number of bits per data pixel'
      missing = where(finite(data) ne 1,count)
      clipdata = (-199) > data < (16383-200)
      intdata = fix(round(( clipdata + 200) * 4-32768))
      if(count gt 0) then intdata[missing] = -32768
      writefits,fileout,intdata,hdr,append=(iext gt 0)
    endif else begin                            ; do not scale data
      writefits,fileout,data,hdr,append=(iext gt 0)
    endelse
  endif
  iext=iext+1
endwhile

end
