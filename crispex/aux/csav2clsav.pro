;---------------------------------------------------------------------------------------------------------
;+
; NAME:
;      	CSAV2CLSAV
;
; PURPOSE:
;     	Conversion of CRISPEX output *.CSAV files into CRISPEX input/output *.CLSAV files
;
; CATEGORY:
;      	CRISPEX auxiliary routine
;
; CALLING SEQUENCE:
;	CSAV2CLSAV, INPUTFILENAME, NX, NY
;
; INPUTS:
;	INPUTFILENAME	= One (or an array of) *.CSAV file(s).
;	NX		= The number of pixels in the x-direction.
;	NY		= The number of pixels in the y-direction.
;
; KEYWORDS:
;	None.
;
; OUTPUTS:
;	For each input *.CSAV file one *.CLSAV file.
;
; COMMON BLOCKS:
;     	None.
;
; SIDE EFFECTS:
;     	None.
;
; RESTRICTIONS:
;     	None.
;
; PROCEDURE:
;	Calling the routine with:
;		
;	CSAV2CLSAV, 'halpha_11Jun2008_2011Apr12_140515.csav', 925, 948
;
;	Will return one file: 'halpha_11Jun2008_2011Apr12_140515.clsav', which can be read in
;	by CRISPEX again.
;	
;	One may also supply an array of input files:
;		
;	CSAV2CLSAV, ['halpha_11Jun2008_2011Apr12_140515.csav','halpha_11Jun2008_2011Apr12_140759.csav'], 925, 948
;
; MODIFICATION HISTORY:
;	15 Dec 2009 GV:	Initial routine
;	12 Apr 2011 GV: Extended program info, small revisions
;
; AUTHOR:
;	Gregal Vissers (g.j.m.vissers@astro.uio.no)
;	@ Institute of Theoretical Astrophysics, University of Oslo
;-
;---------------------------------------------------------------------------------------------------------

PRO CSAV2CLSAV, inputfilename, nx, ny
  
;==================== VERSION AND REVISION NUMBER
  ; Version 1.0 (rev 1) == version 1.0.0
	base_version_number = '1.0'

  ; Get revision number from CVS $Id
  id_string = '; $Id: csav2clsav.pro,v 1.2 2014/04/10 08:02:01 gregal Exp $'
  split_id_string = STRSPLIT(id_string[0],' ',/EXTRACT)
  cvs_idn = split_id_string[3]
  cvs_rev = (STRSPLIT(cvs_idn,'.',/EXTRACT))[1]
  cvs_msg = STRJOIN(split_id_string[3:6],' ')
  
  ; Assumption: CVS committed revision number will always be 1.x, with x
  ; increasing linearly
  revnr = 2+FIX(cvs_rev)-2     ; rev_nr=2, cvs_rev=2 when implemented

  ; Change rev_nr and cvs_rev below whenever changing base_versions_number!
  subvnr = 2 + (FIX(cvs_rev)-2) - 1  ; rev_nr=2, cvs_rev=2 when implemented
  ; Convert revision and version numbers to strings
  revision_number = STRTRIM(revnr,2)   
  version_number = base_version_number +'.'+ STRTRIM(subvnr,2)
  vnr_msg = version_number+' (r'+revision_number+'; '+cvs_msg+')'

;==================== PROGRAM-INFO ON CALL W/O PARAMS
	IF (N_PARAMS() NE 3) THEN BEGIN
    MESSAGE,'Version '+vnr_msg, /INFO
		MESSAGE,'CSAV2CLSAV, Inputfilename, Nx, Ny',/INFO
		RETURN
	ENDIF

	FOR i=0,N_ELEMENTS(inputfilename)-1 DO BEGIN
		RESTORE, inputfilename[i]
		xp = x_coords
		yp = y_coords
		SPLINE_P,xp,yp,xr,yr,INTERVAL=1
		w_lpts = WHERE((xr GE 0) AND (xr LE nx-1) AND (yr GE 0) AND (yr LE ny-1), nw_lpts)
	
		x_coords = xp
		y_coords = yp
		x_loop_pts = xr
		y_loop_pts = yr
		w_loop_pts = w_lpts
		
		tsav_set = (N_ELEMENTS(t_saved) EQ 1) 
		cvers_set = (N_ELEMENTS(crispex_version) EQ 2) 
    IF ((N_ELEMENTS(ngaps) NE 1) AND (nw_lpts NE 0)) THEN BEGIN
      result = CRISPEX_ARRAY_GET_GAPS(w_loop_pts, N_ELEMENTS(x_loop_pts))
      ngaps = result.ngaps
      databounds = result.databounds
      wdatabounds = result.wdatabounds
    ENDIF
	
		dirfilename = FILE_DIRNAME(inputfilename[i],/MARK_DIRECTORY)
		splitfilename = STRMID(inputfilename[i],STRLEN(dirfilename),STRLEN(inputfilename[i]))
		outputfilename = STRMID(splitfilename,0,STRPOS(splitfilename,'.',/REVERSE_SEARCH))+'.clsav'
		IF (tsav_set AND cvers_set) THEN $
      SAVE, crispex_version, spect_pos, x_coords, y_coords, x_loop_pts, $
        y_loop_pts, w_loop_pts, t_saved, ngaps, databounds, wdatabounds, $
        FILENAME = dirfilename+outputfilename $
    ELSE IF tsav_set THEN $
			SAVE, spect_pos, x_coords, y_coords, x_loop_pts, y_loop_pts, w_loop_pts, $
        t_saved, ngaps, databounds, wdatabounds, $
        FILENAME = dirfilename+outputfilename $
    ELSE IF cvers_set THEN $
      SAVE, crispex_version, spect_pos, x_coords, y_coords, x_loop_pts, $
        y_loop_pts, w_loop_pts, ngaps, databounds, wdatabounds, $
        FILENAME = dirfilename+outputfilename $
    ELSE $
      SAVE, spect_pos, x_coords, y_coords, x_loop_pts, y_loop_pts, w_loop_pts, $
        ngaps, databounds, wdatabounds, FILENAME = dirfilename+outputfilename
		PRINT,'Written: '+dirfilename+outputfilename
	ENDFOR
END
