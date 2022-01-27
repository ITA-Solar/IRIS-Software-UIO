function iris_find_winpos,lu,exten, silent=silent,ext_no=ext_no,errmsg=errmsg

;+
; NAME:
;     IRIS_FIND_WINPOS
; PURPOSE:
;     Return location of window data in fits file
;
; CALLING SEQUENCE:
;     pos=IRIS_FIND_WINPOS(lu,ext, /silent)
;     pos=IRIS_FIND_WINPOS(lu, extname, /silent,ext_no=,errmsg= )
;
; INPUT PARAMETERS:
;     lu      = An open unit descriptor for a FITS data stream.
;     exten   = Number of extensions to skip.
;                              or
;             Scalar string giving extension name (in the EXTNAME keyword)         
; OPTIONAL INPUT PARAMETER:
;     /SILENT - If set, then any messages about invalid characters in the 
;               FITS file are suppressed.
; OPTIONAL OUTPUT PARAMETER:
;       ERRMSG  = If this keyword is present, then any error messages will be
;                 returned to the user in this parameter rather than
;                 depending on the MESSAGE routine in IDL.  If no errors are
;                 encountered, then a null string is returned.
;
; RETURNS:
;     location (in bytes) of all windows
;    -1 if an error is encountered.
;
; COMMON BLOCKS:
;      None.
; SIDE EFFECTS:
;      Repositions the file pointer.
; PROCEDURE:
;      Each FITS header is read in and parsed, and the file pointer is moved
;      to where the next FITS extension header until the desired
;      extension is reached.
; PROCEDURE CALLS:
;      FXPAR(), MRD_HREAD, MRD_SKIP
; MODIFICATION HISTORY:
;      Extracted from FXMOVE 31-Aug-2013 by V. Hansteen
;  $Id: iris_find_winpos.pro,v 1.2 2013/09/03 10:05:31 viggoh Exp $
;-
         DO_NAME = SIZE( EXTEN,/TNAME) EQ 'STRING'
	 PRINT_ERROR = NOT ARG_PRESENT(ERRMSG)
         ERRMSG = ''
         IF DO_NAME THEN BEGIN 
	              FIRSTBLOCK = 0
		      EXT_NO = 9999
		      ENAME = STRTRIM( STRUPCASE(EXTEN), 2 )
		      ON_IOERROR, ALLOW_PLUN
		      POINT_LUN, -LU, DUM
		      ON_IOERROR, NULL
         ENDIF ELSE BEGIN 
	              FIRSTBLOCK = 1
		      EXT_NO = EXTEN
	ENDELSE 	            
		
        offset=long64(0)
        pointer=lon64arr(ext_no+1)
        FOR I = 0, EXT_NO DO BEGIN
               
;
;  Read the next header, and get the number of bytes taken up by the data.
; 

                IF EOF(LU) THEN BEGIN 
		    IF DO_NAME THEN ERRMSG = $
	'Extension name ' + ename + ' not found in FITS file' ELSE ERRMSG = $	    
	'EOF encountered while moving to specified extension'
	        if PRINT_ERROR then message,errmsg	
		RETURN, -1
		ENDIF
           
                ; Can't use FXHREAD to read from pipe, since it uses
                ; POINT_LUN.  So we read this in ourselves using mrd_hread

                MRD_HREAD, LU, HEADER, STATUS, SILENT = Silent, $
		    ERRMSG = ERRMSG
                IF STATUS LT 0 THEN BEGIN 
		    IF PRINTERROR THEN MESSAGE,ERRMSG
		    RETURN, -1
		ENDIF    
                
                nmax=n_elements(header)
                endline=nmax-1
                nblock=endline*80/2880+1
                pointer[i]=nblock*2880+offset

                ; Get parameters that determine size of data
                ; region.
                IF DO_NAME THEN IF I GT 1 THEN BEGIN
		       EXTNAME = STRTRIM(FXPAR(HEADER,'EXTNAME',COUNT=N_name),2)
			 if N_NAME GT 0 THEN $
			  IF ENAME EQ STRUPCASE(EXTNAME) THEN BEGIN
			        EXT_NO= I-1
				BLOCK = 1 + ((N_ELEMENTS(HEADER)-1)/36)
				POINT_LUN, -LU, CURR_POSS
				POINT_LUN, LU, CURR_POSS - BLOCK*2880 
			        BREAK
			ENDIF	
		ENDIF	         
                BITPIX = FXPAR(HEADER,'BITPIX')
                NAXIS  = FXPAR(HEADER,'NAXIS')
                GCOUNT = FXPAR(HEADER,'GCOUNT') 
                IF GCOUNT EQ 0 THEN GCOUNT = 1
                PCOUNT = FXPAR(HEADER,'PCOUNT')
                
                IF NAXIS GT 0 THEN BEGIN 
                        DIMS = FXPAR(HEADER,'NAXIS*')           ;Read dimensions
			NDATA = PRODUCT(DIMS,/INTEGER) 
                ENDIF ELSE NDATA = 0
                
                NBYTES = LONG64(ABS(BITPIX) / 8) * GCOUNT * (PCOUNT + NDATA)
;
;  Move to the next extension header in the file.
;
                NREC = (NBYTES + 2879) / 2880
                
                offset=long64(nrec)*2880
                MRD_SKIP, LU, offset
                offset=offset+pointer[i]

        ENDFOR
	        
        RETURN, pointer
ALLOW_PLUN:
        ERRMSG =  $
	'Extension name cannot be specified unless POINT_LUN access is available'
	if PRINT_ERROR then message,errmsg	
	RETURN, -1        
END
