;---------------------------------------------------------------------------------------------------------
;+
; NAME:
;      	SPECTFILEUPDATE
;
; PURPOSE:
;     	Conversion of CRISPEX input spectral save file from pre-"version 1.6" format to version 1.6 (or later) format
;
; CATEGORY:
;      	CRISPEX auxiliary updating routine
;
; CALLING SEQUENCE:
;	SPECTFILEUPDATE, INPUTFILENAME, XTITLE=xtitle, YTITLE=ytitle
;
; INPUTS:
;	INPUTFILENAME	= One (or an array of) spectral files
;
; KEYWORDS:
;	XTITLE		= One (or an array of) string(s) containing the x-title label(s) for plots.
;			  Number of elements must be equal to that of INPUTFILENAME, otherwise the first element
;			  will be replicated to create an array of the same length as INPUTFILENAME.
;	YTITLE		= One (or an array of) string(s) containing the y-title label(s) for plots
;			  Number of elements must be equal to that of INPUTFILENAME, otherwise the first element
;			  will be replicated to create an array of the same length as INPUTFILENAME.
;
; OUTPUTS:
;	For each input spectral file one output spectral file with three variables:
;		NORM_SPECT	= Normalised spectrum
;		NORM_FACTOR	= Normalisation factor used to produce NORM_SPECT
;		SPECT_POS	= Spectral wavelength positions in any desired unit
;		XTITLE		= x-title label for plots. Default is empty string.
;		YTITLE		= y-title label for plots. Default is empty string.
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
;		SPECTFILEUPDATE, 'halpha_11Jun2008.spectfile'
;
;	Will return one file: 'halpha_11Jun2008.updated.spectfile', which can be read in
;	by CRISPEX again.
;	
;	One may also supply an array of input files:
;		
;		SPECTFILEUPDATE, ['halpha_11Jun2008.spectfile','ca8542_28Jun2010.spectfile']
;
;	In addition, the x- and/or y-title labels for the CRISPEX in-program plots can now also be provided
;	through the spectral save file:
;	
;		SPECTFILEUPDATE, 'halpha_11Jun2008.spectfile', XTITLE='Wavelength [nm]'
;	
;
; MODIFICATION HISTORY:
;	22 Apr 2010 GV:	Initial version
;	25 Apr 2010 GV: Added XTITLE and YTITLE keywords
;
; AUTHOR:
;	Gregal Vissers (g.j.m.vissers@astro.uio.no)
;	@ Institute of Theoretical Astrophysics, University of Oslo
;-
;---------------------------------------------------------------------------------------------------------

PRO SPECTFILEUPDATE, inputfilename, XTITLE=xtitle, YTITLE=ytitle

	IF (N_PARAMS() NE 1) THEN BEGIN
		PRINT,'SPECTFILEUPDATE, INPUTFILENAME, XTITLE=xtitle, YTITLE=ytitle'
		RETURN
	ENDIF

	ninput = N_ELEMENTS(inputfilename)
	IF (N_ELEMENTS(XTITLE) EQ 0) THEN xtitle = ''
	IF (N_ELEMENTS(YTITLE) EQ 0) THEN ytitle = ''
	IF ((N_ELEMENTS(XTITLE) NE ninput) AND (N_ELEMENTS(XTITLE) GT 0)) THEN updated_xtitle = REPLICATE(xtitle[0],ninput) ELSE updated_xtitle = xtitle
	IF ((N_ELEMENTS(YTITLE) NE ninput) AND (N_ELEMENTS(YTITLE) GT 0)) THEN updated_ytitle = REPLICATE(ytitle[0],ninput) ELSE updated_ytitle = ytitle

	FOR i=0,ninput-1 DO BEGIN
		RESTORE, inputfilename[i]
		norm_spect = spec
		norm_factor = 1/mn
		spect_pos = ll
		IF (N_ELEMENTS(XTITLE_LABEL) NE 1) THEN xtitle_label = updated_xtitle[i]
		IF (N_ELEMENTS(YTITLE_LABEL) NE 1) THEN ytitle_label = updated_ytitle[i]
		
		dirfilename = FILE_DIRNAME(inputfilename[i],/MARK_DIRECTORY)
		IF (dirfilename EQ './') THEN pos = 0 ELSE pos = STRLEN(dirfilename)
		splitfilename = STRMID(inputfilename[i],pos,STRLEN(inputfilename[i]))
		extension = STRMID(splitfilename,STRPOS(splitfilename,'.',/REVERSE_SEARCH),STRLEN(splitfilename))
		outputfilename = STRMID(splitfilename,0,STRPOS(splitfilename,'.',/REVERSE_SEARCH))+'.updated'+extension
		SAVE, spect_pos, norm_spect, norm_factor, xtitle_label, ytitle_label, FILENAME = dirfilename+outputfilename
		PRINT,'Written: '+dirfilename+outputfilename
	ENDFOR
END
