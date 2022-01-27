function fitspointer,filename,header,exten_no=exten_no,silent=silent
;
;+
; NAME:
;	FITSPOINTER
;
; PURPOSE:
;	Finds starting position of data block in extension number exten_no in fits files 
;
; CATEGORY:
;       CRISPEX support software
;	
; CALLING SEQUENCE:
;	FITSPOINTER,filename,header,exten_no=exten_no,silent=silent
;
; INPUTS:
;	filename - fits file name 
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;       exten_no - extension number to be read/found; default = 0
;       silent - if no on screen messages desired
;
; OUTPUTS:
;       function gives byte starting position of data extension in file
;
; OPTIONAL OUTPUTS:
;        header - header for extension number exten_no
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURES USED:
;       Functions:   SXPAR()
;       Procedures:  READFITS
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;   v1.0 20-Sep-2012 Viggo Hansteen - first version
;   $Id: fitspointer.pro,v 1.1 2013/09/27 07:29:16 matsc Exp $
;-
;
  if n_params() lt 1 then begin
    message,'fitspointer,filename ,header,exten_no=exten_no,silent=silent',/cont
    return,-1
  endif
    
  doheader=arg_present(header)
  if n_elements(exten_no) eq 0 then exten_no=0L  
  silent=keyword_set(silent)

  openr, unit, filename, ERROR=error,/get_lun
  if error NE 0 then begin
    message,/con,' ERROR - Unable to locate file ' + filename
    return, -1
  endif

;  Handle Unix or Fpack compressed files which will be opened via a pipe using
;  the SPAWN command. 
;  ViggoH 19092012: This piece of code can be uncommented at a later date if we want to
;  include this capability...

  ;; if unixZ then begin
  ;;               free_lun, unit
  ;;               spawn, 'gzip -cd '+filename, unit=unit                 
  ;;               gzip = 1b
  ;; endif else if fcompress then begin 
  ;;               free_lun, unit
  ;;               spawn,'funpack -S ' + filename, unit=unit,/sh
  ;;               if eof(unit) then begin 
  ;;                 message,'Error spawning FPACK decompression',/CON
  ;;                 free_lun,unit
  ;;                 return,-1
  ;;                endif    
  ;;       endif   
  ;; endelse

  hbuf=36
  datapointer=0L

  for ext = 0L, exten_no do begin
               
;  Read the next header, and get the number of bytes taken up by the data.

    block = string(replicate(32b,80,36))
    w = [-1]
    header = strarr(36)
    headerblock = 0L
    i = 0L      

    while w[0] EQ -1 do begin
          
      if EOF(unit) then begin 
            message,/con, $
               'EOF encountered attempting to read extension ' + strtrim(ext,2)
            free_lun,unit
            return,-1
      endif

      readu, unit, block
      headerblock = headerblock + 1
      w = where(strlen(block) NE 80, Nbad)
      if (Nbad GT 0) then begin
           message,'Warning-Invalid characters in header',/INF,NoPrint=Silent
           block[w] = string(replicate(32b, 80))
      endif
      w = where(strcmp(block,'END     ',8), Nend)
      if (headerblock EQ 1) || ((ext EQ exten_no) && (doheader)) then begin
        if Nend GT 0 then  begin
          if headerblock EQ 1 then header = block[0:w[0]]   $
          else header = [header[0:i-1],block[0:w[0]]]
       endif else begin
         header[i] = block
         i = i+36
         if i mod hbuf EQ 0 then header = [header,strarr(hbuf)]
       endelse
      endif
      datapointer=datapointer+2880L
    endwhile

    if (ext EQ 0 ) then $
      if strmid( header[0], 0, 8)  NE 'SIMPLE  ' then begin
        message,/CON, $
           'ERROR - Header does not contain required SIMPLE keyword'
      if ~unitsupplied then free_lun, unit
      return, -1
    endif
             
; Get parameters that determine size of data region.
                
    bitpix =  sxpar(header,'BITPIX')
    byte_elem = abs(bitpix)/8               ;Bytes per element
    naxis  = sxpar(header,'NAXIS')
    gcount = sxpar(header,'GCOUNT') > 1
    pcount = sxpar(header,'PCOUNT')
                
    if naxis GT 0 then begin 
      dims = sxpar( header,'NAXIS*')           ;Read dimensions
      ndata = product(dims,/integer)
    endif else ndata = 0
                
    nbytes = byte_elem * gcount * (pcount + ndata)

;  Move to the next extension header in the file.   Use MRD_SKIP to skip with
;  fastest available method (POINT_LUN or readu) for different file
;  types (regular, compressed, Unix pipe, socket) 

    if ext LT exten_no then begin
      nrec = long64((nbytes + 2879) / 2880)
      if nrec GT 0 then mrd_skip, unit, nrec*2880L
      datapointer=datapointer+nrec*2880L    
   endif
  endfor

  case BITPIX of 
           8:   IDL_type = 1          ; Byte
          16:   IDL_type = 2          ; Integer*2
          32:   IDL_type = 3          ; Integer*4
          64:   IDL_type = 14         ; Integer*8
         -32:   IDL_type = 4          ; Real*4
         -64:   IDL_type = 5          ; Real*8
        else:   begin
                message,/CON, 'ERROR - Illegal value of BITPIX (= ' +  $
                strtrim(bitpix,2) + ') in FITS header'
                if ~unitsupplied then free_lun,unit
                return, -1
                end
  endcase     
 
  if nbytes EQ 0 then begin
    if ~SILENT then message, $
         "FITS header has NAXIS or NAXISi = 0,  no data array read",/CON
    free_lun, unit
    return,-1
  endif

  if exten_no GT 0 then begin
    xtension = strtrim( sxpar( header, 'XTENSION' , Count = N_ext),2)
    if N_ext EQ 0 then message, /INF, NoPRINT = Silent, $
          'WARNING - Header missing XTENSION keyword'
  endif 

  if ~SILENT then begin   ;Print size of array being read

     if exten_no GT 0 then message, $
              'Reading FITS extension of type ' + xtension, /INF  
     if N_elements(dims) EQ 1 then $
         st = 'Now reading ' + strtrim(dims,2) + ' element vector' else $                 
         st = 'Now reading ' + strjoin(strtrim(dims,2),' by ') + ' array'
         if (exten_no GT 0) && (pcount GT 0) then st = st + ' + heap area'
         message,/INF,st   
  endif
;
  return,datapointer  
;
end
