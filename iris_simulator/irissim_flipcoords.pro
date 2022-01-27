;if calculating forwards, i.e crs->t (backwards not set) then
;assumption is that input are real crs coords, i.e. start at 1
;one of the keywords (fuv, nuv, sjifuv, sjinuv) has to be set
;
;backwards calculation: crs_sr is actually tsr and so on...
;input can be both, starting at 1 or zero (indicated by startatzero keyword)
;output will have the same format
;summing_only: the coords will not be flipped, only multiplied by summing (only for backwards)


FUNCTION IRISsim_flipcoords, crs_scIN, crs_ecIN, crs_srIN, crs_erIN, $
    sumsptrlIN, sumspatIN, $
    fuv=fuv, nuv=nuv, sjifuv=sjifuv, sjinuv=sjinuv, $
    startatzero=startatzero, backwards=backwards, summing_only=summing_only
    
  ; $Id: irissim_flipcoords.pro,v 1.5 2013/09/11 11:01:35 mawiesma Exp $  ;
    
  if N_PARAMS() lt 6 then begin
    if N_PARAMS() lt 4 then begin
      print, 'need more input (at least the 4 crs coords)'
      return, -1
    endif else begin
      if N_PARAMS() eq 5 then begin
        print, 'no summing for spatial direction received, assuming 1'
        sumspatIN=1
      endif else begin
        print, 'no summing received, assuming 1'
        sumsptrlIN=1
        sumspatIN=1
      endelse
    endelse
  endif
  
  crs_sr = fix(crs_srIN)
  crs_er = fix(crs_erIN)
  crs_sc = fix(crs_scIN)
  crs_ec = fix(crs_ecIN)
  sumsptrl = fix(sumsptrlIN)
  sumspat = fix(sumspatIN)
  
  addone = ~keyword_set(startatzero)
  
  if ~keyword_set(backwards) then begin
    if ~keyword_set(summing_only) then begin
    
      ;forwards all transformations
      if keyword_set(fuv) then begin
        tsc = (crs_sr - 1) / sumsptrl + addone
        tec = (crs_er - 1) / sumsptrl + addone
        tsr = (1096 - crs_ec) / sumspat + addone
        ter = (1096 - crs_sc) / sumspat + addone
      endif else if keyword_set(nuv) then begin
        tsc = (4144 - crs_er) / sumsptrl + addone
        tec = (4144 - crs_sr) / sumsptrl + addone
        tsr = (crs_sc - 1) / sumspat + addone
        ter = (crs_ec - 1) / sumspat + addone
      endif else if keyword_set(sjifuv) then begin
        tsc = (2072 - crs_er) / sumsptrl + addone
        tec = (2072 - crs_sr) / sumsptrl + addone
        tsr = (crs_sc - 1) / sumspat + addone
        ter = (crs_ec - 1) / sumspat + addone
      endif else if keyword_set(sjinuv) then begin
        tsc = (crs_sr - 1) / sumsptrl + addone
        tec = (crs_er - 1) / sumsptrl + addone
        tsr = (crs_sc - 1) / sumspat + addone
        ter = (crs_ec - 1) / sumspat + addone
      endif else begin
        print, 'no keyword is set, returning data without change'
        tsc = crs_sc
        tec = crs_ec
        tsr = crs_sr
        ter = crs_er
      endelse
      
    endif else begin
    
    ;forwards, summing only
      tsc = (crs_sc - addone) / sumsptrl + addone
      tec = (crs_ec + (~addone)) / sumsptrl - (~addone)
      tsr = (crs_sr - addone) / sumspat + addone
      ter = (crs_er + (~addone)) / sumspat - (~addone)    
    endelse
    
  endif else begin
    ;calculate the values back again
    if keyword_set(summing_only) then begin
      tsc = (crs_sc - addone) * sumsptrl + addone
      tec = (crs_ec + (~addone)) * sumsptrl - (~addone)
      tsr = (crs_sr - addone) * sumspat + addone
      ter = (crs_er + (~addone)) * sumspat - (~addone)
    endif else begin
      print, 'not yet implemented... is there a need for this?'
      print, 'returning data without change'
      tsc = crs_sc
      tec = crs_ec
      tsr = crs_sr
      ter = crs_er
    endelse
  endelse
  
  result = {tsr:tsr, ter:ter, tsc:tsc, tec:tec}
  
  return, result
END
