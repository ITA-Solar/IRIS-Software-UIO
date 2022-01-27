function iris_shift,file,dxy, quiet=quiet, noncumulative=noncumulative
;+
;   iris_shift,file,dxy ,/quiet, /noncumulative
;
;            shifts a cube with dxy
;            find dxy array by calling mfg_align
;            output is cube with _align appended
; 
;   based on matsc's mfg_shift used for Hinode filtergrams
;   
;   18.2.2014 Martin Wiesmann: Added keyword noncumulative
;           by default dxy is assumed to be cumulative, setting this keyword will
;           interpret dxy as non-cumulative
;   $Id: iris_shift.pro,v 1.5 2014/02/18 13:18:31 mawiesma Exp $ 
;-
if(n_params() lt 2) then begin
  message,'im_align=iris_shift(file,dxy ,/quiet)',/info
  return,-1
endif

s=iris_sji(file)
file_align='iris_align_'+anytim2cal(s->getdate_obs(),/date,form=8)+'_'+ $
                         anytim2cal(s->getdate_obs(),/time,form=8)+'.sav'
if n_elements(dxy) eq 0 then begin
  if (file_info(file_align)).exists then restore,file_align else begin
    message,'no dxy array given and no dxy save set found, returning',/info
    return,-1
  endelse
endif
nt=s->getnexp()
siz=size(dxy)
if(siz[0] ne 2) or (siz[1] ne 2) or (siz[2] ne nt) then begin
  message,'dxy has wrong dimension, should be [2,'+strtrim(nt,2)+']',/info
  return,-1
endif

if keyword_set(noncumulative) then begin
  dx=reform(dxy[0,*])
  dy=reform(dxy[1,*])
endif else begin
  dx=total(reform(dxy[0,*]),/cum)
  dy=total(reform(dxy[1,*]),/cum)
endelse

; read first image
i=0
im2=(s->getvar()) ; read image
im=im2[*,*,i]
siz=size(im)
if(siz[3] eq 1) then begin
  im=float(im)   ; make image float before shift
  im2[*,*,i]=byte(iris_shiftf(im,dx[i],dy[i])+0.5)  ; add 0.5 to get correct roundoff
endif else if(siz[3] eq 2) then begin
  im=float(im)   ; make image float before shift
  im2[*,*,i]=fix(iris_shiftf(im,dx[i],dy[i])+0.5)  ; add 0.5 to get correct roundoff
endif else begin
  im2[*,*,i]=iris_shiftf(im,dx[i],dy[i])
endelse

if(not keyword_set(quiet)) then mtimer,/start
for i=1,nt-1 do begin
  im=im2[*,*,i]    ; read image
  if(siz[3] eq 1) then begin
    im=float(im)   ; make image float before shift
    im2[*,*,i]=byte(iris_shiftf(im,dx[i],dy[i])+0.5)  ; add 0.5 to get correct roundoff
  endif else if(siz[3] eq 2) then begin
    im=float(im)   ; make image float before shift
    im2[*,*,i]=fix(iris_shiftf(im,dx[i],dy[i])+0.5)  ; add 0.5 to get correct roundoff
  endif else begin
    im2[*,*,i]=iris_shiftf(im,dx[i],dy[i])
  endelse
  if(not keyword_set(quiet)) then mtimer,'iris_shift',i,nt,/remaining
endfor
if(not keyword_set(quiet)) then mtimer,/stop
obj_destroy,s
return,im2

end
