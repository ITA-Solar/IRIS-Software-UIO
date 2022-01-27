FUNCTION IRIS_FITSPOINTER, filename, header, EXTEN_NO=exten_no, SILENT=silent
;	Finds starting position of data block in extension number exten_no in fits files 
; Based on earlier FITSPOINTER.PRO, modification history:
;   v1.0 20-Sep-2012 Viggo Hansteen - first version

  IF N_PARAMS() LT 1 THEN BEGIN
    MESSAGE,'fitspointer,filename ,header,exten_no=exten_no,silent=silent',/CONT
    RETURN,-1
  ENDIF
    
  doheader=ARG_PRESENT(header)
  IF N_ELEMENTS(exten_no) EQ 0 THEN exten_no=0L  
  silent=KEYWORD_SET(silent)

  OPENR, unit, filename, ERROR=error,/GET_LUN
  IF error NE 0 then begin
    MESSAGE,/CON,' ERROR - Unable to locate file ' + filename
    RETURN, -1
  ENDIF

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

  FOR ext = 0L, exten_no DO BEGIN
               
;  Read the next header, and get the number of bytes taken up by the data.

    block = STRING(REPLICATE(32b,80,36))
    w = [-1]
    header = STRARR(36)
    headerblock = 0L
    i = 0L      

    WHILE w[0] EQ -1 DO BEGIN
          
      IF EOF(unit) THEN BEGIN 
            MESSAGE,/CON, $
               'EOF encountered attempting to read extension ' + STRTRIM(ext,2)
            FREE_LUN,unit
            RETURN,-1
      ENDIF

      READU, unit, block
      headerblock = headerblock + 1
      w = WHERE(STRLEN(block) NE 80, Nbad)
      IF (Nbad GT 0) THEN BEGIN
           MESSAGE,'Warning-Invalid characters in header',/INF,NoPrint=Silent
           block[w] = STRING(REPLICATE(32b, 80))
      ENDIF
      w = where(strcmp(block,'END     ',8), Nend)
      IF (headerblock EQ 1) || ((ext EQ exten_no) && (doheader)) THEN BEGIN
        IF Nend GT 0 THEN  BEGIN
          IF headerblock EQ 1 THEN header = block[0:w[0]]   $
          ELSE header = [header[0:i-1],block[0:w[0]]]
       ENDIF ELSE BEGIN
         header[i] = block
         i = i+36
         IF i MOD hbuf EQ 0 THEN header = [header,strarr(hbuf)]
       ENDELSE
      ENDIF
      datapointer=datapointer+2880L
    ENDWHILE

    IF (ext EQ 0 ) THEN $
      IF STRMID( header[0], 0, 8)  NE 'SIMPLE  ' THEN BEGIN
        MESSAGE,/CON, $
           'ERROR - Header does not contain required SIMPLE keyword'
      IF ~unitsupplied THEN FREE_LUN, unit
      RETURN, -1
    ENDIF
             
; Get parameters that determine size of data region.
                
    bitpix =  SXPAR(header,'BITPIX')
    byte_elem = ABS(bitpix)/8               ;Bytes per element
    naxis  = SXPAR(header,'NAXIS')
    gcount = SXPAR(header,'GCOUNT') > 1
    pcount = SXPAR(header,'PCOUNT')
                
    IF naxis GT 0 THEN BEGIN 
      dims = SXPAR( header,'NAXIS*')           ;Read dimensions
      ndata = PRODUCT(dims,/integer)
    ENDIF ELSE ndata = 0
                
    nbytes = byte_elem * gcount * (pcount + ndata)

;  Move to the next extension header in the file.   Use MRD_SKIP to skip with
;  fastest available method (POINT_LUN or readu) for different file
;  types (regular, compressed, Unix pipe, socket) 

    IF ext LT exten_no THEN BEGIN
      nrec = LONG64((nbytes + 2879) / 2880)
      IF nrec GT 0 THEN mrd_skip, unit, nrec*2880L
      datapointer=datapointer+nrec*2880L    
   ENDIF
  ENDFOR

  CASE BITPIX OF 
           8:   IDL_type = 1          ; Byte
          16:   IDL_type = 2          ; Integer*2
          32:   IDL_type = 3          ; Integer*4
          64:   IDL_type = 14         ; Integer*8
         -32:   IDL_type = 4          ; Real*4
         -64:   IDL_type = 5          ; Real*8
        ELSE:   BEGIN
                MESSAGE,/CON, 'ERROR - Illegal value of BITPIX (= ' +  $
                STRTRIM(bitpix,2) + ') in FITS header'
                IF ~unitsupplied THEN FREE_LUN,unit
                RETURN, -1
                END
  ENDCASE     
 
  IF nbytes EQ 0 THEN BEGIN
    IF ~SILENT THEN MESSAGE, $
         "FITS header has NAXIS or NAXISi = 0,  no data array read",/CON
    FREE_LUN, unit
    RETURN,-1
  ENDIF

  IF exten_no GT 0 THEN BEGIN
    xtension = STRTRIM( SXPAR( header, 'XTENSION' , Count = N_ext),2)
    IF N_ext EQ 0 THEN MESSAGE, /INF, NoPRINT = Silent, $
          'WARNING - Header missing XTENSION keyword'
  ENDIF 

  IF ~SILENT THEN BEGIN   ;Print size of array being read
    IF exten_no GT 0 THEN MESSAGE, $
             'Reading FITS extension of type ' + xtension, /INF  
    IF N_elements(dims) EQ 1 THEN $
        st = 'Now reading ' + STRTRIM(dims,2) + ' element vector' ELSE $                 
        st = 'Now reading ' + STRJOIN(STRTRIM(dims,2),' by ') + ' array'
        IF (exten_no GT 0) && (pcount GT 0) THEN st = st + ' + heap area'
        MESSAGE,/INF,st   
  ENDIF
  FREE_LUN, unit
  RETURN,datapointer  
END
