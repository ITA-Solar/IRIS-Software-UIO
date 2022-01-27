;-------------------------------------------------------------------------------
;+
; NAME:
;      	TANAT: Time slice ANAlysis Tool
;
; PURPOSE:
;	Analyse the output timeslices from CRISPEX in order to
;	determine velocities.
;
; CATEGORY:
;      	Data analysis
;
; CALLING SEQUENCE:
;	TANAT, filename, LINE_CENTER=line_center, ASECPIX=asecpix, DT=dt
;
; INPUTS:
;	filename	= filename (or string array of filenemes) of the timeslice(s)
;
; KEYWORDS:
;	LINE_CENTER	= Not set: linecentre is determined from the data or from SPECTFILE.
;			  Integer scalar: linecentre is set to position specified by LINE_CENTER.
;			  2-element array (format: [WAVELENGTH, DELTA_LAMBDA]): linecentre is
;				determined from the data or from SPECTFILE and set to WAVELENGTH. 
;				The distance in wavelength between the linepositions is specified 
;				by DELTA LAMBDA.
;			  3-element array (format: [Integer scalar, WAVELENGTH, DELTA_LAMBDA]):
;				combination of the two above, as linecentre is specified by the
;				integer scalar and set to WAVELENGTH with tickmarks at DELTA_LAMBDA
;				distances from eachother.
;	ASECPIX		= Specifies the arcseconds per pixel. Defaults to current SST default of 0.0592.
;	DT		= Specifies the elapsed time in seconds per time step. Defaults to 1.
;
; OUTPUTS:
;	Measurements can be saved to a file, specified by the text in the filename input text box in
;	the upper right part of the Set up tab. Each measurement is written on a single line,
;	containing: the filename (i.e. of the saved timeslice, a *csav file), the spectral position at
;	which the measurement is taken, a measurement ID, the first spatial coordinate, the second
;	spatial coordinate, the actual distance in pixels between them, the first temporal coordinate,
;	the second temporal coordinate, the difference in temporal coordinates, the absolute speed
;	(in km/s) and an optional flag.
;	For fitted measurements the order is slightly altered: after the measurement ID follow first
;	the three spatial coordinates, the three temporal coordinates, the accelaration (in m/s^2) and 
;	finally an optional flag.
;
; COMMON BLOCKS:
;     	None.
;
; SIDE EFFECTS:
;     	None.
;
; RESTRICTIONS:
;     	Input must be in CRISPEX timeslice output format, i.e. certain
;	variable naming in the savefile is assumed.
;
; PROCEDURE:
;	Simple call (i.e. without any keywords) will yield a window containing
;	controls, the timeslice itself and a plot of the detailed spectrum. Spectral position as well
;	as conversion (i.e. arcseconds per pixel and seconds per time step) can be modified. Browsing
;	with the cursor over the timeslice will show the current frame number and pixel along the slice,
;	as well as the detailed spectrum at the current pixel (if such information is available of course). 
;
;	Left-clicking with the mouse will store and lock the current cursor position. Subsequently a
;	straight line will be drawn from the locked position to the then current cursor position enabling
;	the determination of the speed of a (constant velocity) feature. Re-clicking with the left mouse
;	button at a different location will relocate the locked position. Clicking with the middle mouse
;	button will fix the other end of the line. Right clicking will release the cursor lock.
;
; MODIFICATION HISTORY:
;	27 Mar 2009 GV:	setup of original version
;	30 Mar 2009 GV: release of beta version						(v0.9)
;	06 Apr 2009 GV: corrected errors in read-in and implemented calculation of	(v0.9.1)
;			correct distances
;	14 Apr 2009 GV: adapted to accomodate change in CRISPEX output save file	(v0.9.2)
;	22 May 2009 GV: implemented save measurement to file option			(v0.9.3)
;	24 Aug 2009 GV: implemented visualisation options (scaling and combination	(v0.9.4)
;			of spectral positions) and distributed options to tabs
;	22 Aug 2011 GV: extensive makeover, implemented parabolic fit, overlay of 	(v1.0)
;			measurements, option to open other files in-program, fixed
;			several display and saving bugs
;
; AUTHOR:
;	Gregal Vissers (g.j.m.vissers@astro.uio.no)
;	@ Institute for Theoretical Astrophysics, University of Oslo
;-
;-------------------------------------------------------------

;==================== TANAT FUNCTIONS
;------------------------- SCALING FUNCTION
FUNCTION TANAT_SCALING_CONTRAST, minimum_init, maximum_init, $
  minimum_perc, maximum_perc
  minimum_init = DOUBLE(minimum_init)
  maximum_init = DOUBLE(maximum_init)
  range = maximum_init-minimum_init
  minmax = [minimum_init+range*FLOAT(minimum_perc)/100.,$
            minimum_init+range*FLOAT(maximum_perc)/100.]
  RETURN, minmax
END

;------------------------- COORDINATE TRANSFORM FUNCTIONS
FUNCTION TANAT_TRANSFORM_DATA2DEVICE, info, LX_IN=lx_in, T_IN=t_in, MAIN=main, $
  INVERSE=inverse
  IF KEYWORD_SET(MAIN) THEN BEGIN
    winlx = (*(*info).winsizes).windowx
    wint = (*(*info).winsizes).windowy
    IF (*(*info).dispswitch).gap_consider THEN $
      d_nlx =  (*(*info).dataparams).nlxpts $
    ELSE $
      d_nlx = (*(*info).dataparams).nlx
    d_nt = (*(*info).dataparams).d_nt
    tpos = (*(*info).dataparams).t_low
  ENDIF 
  lx_out = -1
  t_out = -1
  IF ~KEYWORD_SET(INVERSE) THEN BEGIN
    ; Data -> Device: add 0.5 pixel offset to center cursor on pixel, instead of
    ; on IDL default lower left corner
    IF (N_ELEMENTS(LX_IN) NE 0) THEN lx_out = (lx_in+0.5) * winlx / FLOAT(d_nlx)
    IF (N_ELEMENTS(T_IN) NE 0) THEN t_out = (t_in+0.5 - tpos) * wint / FLOAT(d_nt)
  ENDIF ELSE BEGIN
    ; Device -> Data
    IF (N_ELEMENTS(LX_IN) NE 0) THEN lx_out = lx_in * (d_nlx) / winlx 
    IF (N_ELEMENTS(T_IN) NE 0) THEN t_out = t_in * (d_nt) / wint + tpos
  ENDELSE
  result = {lx:lx_out, t:t_out}
  RETURN, result
END


;------------------------- WIDGET FUNCTION
FUNCTION TANAT_WIDGET_DIVIDER, base
  divider_base = WIDGET_BASE(base, /FRAME, /YSIZE)
  divider_labl = WIDGET_LABEL(divider_base, VALUE=' ')
  RETURN, divider_base
END



;================================================================================= ABOUT WINDOW PROCEDURES
PRO TANAT_ABOUT_WINDOW, event 
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	abouttlb = WIDGET_BASE(TITLE = 'TANAT: ABOUT', GROUP_LEADER = (*(*info).winids).root, TLB_FRAME_ATTR = 1, /TLB_KILL_REQUEST_EVENTS, TLB_SIZE_EVENTS = 0)
	disp = WIDGET_BASE(abouttlb, /COLUMN)
	aboutdrawid = WIDGET_DRAW(disp, XSIZE = (*(*info).winsizes).aboutwinx, YSIZE = (*(*info).winsizes).aboutwiny, RETAIN = 2)
	WIDGET_CONTROL, abouttlb, /REALIZE, TLB_SET_XOFFSET=(*(*info).winsizes).aboutwinxoffset, TLB_SET_YOFFSET=(*(*info).winsizes).aboutwinyoffset
	WIDGET_CONTROL, aboutdrawid, GET_VALUE = aboutwid
	TANAT_UPDATE_STARTUP_FEEDBACK, (*(*info).feedbparams).startup_im, (*(*info).feedbparams).xout, (*(*info).feedbparams).yout, $
		['Running TANAT version '+(*(*info).versioninfo).version_number+' ('+(*(*info).versioninfo).revision_number+')','',$
		'Developed by: Gregal Vissers', $
		'               Institute of Theoretical Astrophysics,',$
		'               University of Oslo',$
		'               2009-2014']
	WIDGET_CONTROL, aboutdrawid, EVENT_PRO = 'TANAT_ABOUT_CURSOR', /SENSITIVE, /DRAW_MOTION_EVENTS, /TRACKING_EVENTS, /DRAW_BUTTON_EVENTS
	WIDGET_CONTROL, abouttlb, SET_UVALUE = info
	XMANAGER, 'TANAT', abouttlb,/NO_BLOCK
	(*(*info).winids).abouttlb = abouttlb
END

PRO TANAT_ABOUT_CURSOR, event
; Handles cursor actions on the about window
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_DRAW' THEN BEGIN
		CASE event.TYPE OF
		0:	CASE event.PRESS OF
			1:	BEGIN	; left mouse button press
					WIDGET_CONTROL, (*(*info).winids).abouttlb, /DESTROY
					(*(*info).winids).abouttlb = 0
				END
			ELSE: BREAK
			ENDCASE
		ELSE: RETURN
		ENDCASE
	ENDIF
END

PRO TANAT_ABOUT_WINDOW_CLOSE, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	WIDGET_CONTROL, (*(*info).winids).abouttlb, /DESTROY
END

;================================================================================= CALCULATE PROCEDURES
PRO TANAT_CALCULATE_DELTA, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	(*(*info).measparams).delta_lx = FLOAT((*(*info).dataparams).lx) - FLOAT((*(*info).curs).lxlock)
	(*(*info).measparams).delta_t = FLOAT((*(*info).dataparams).t) - FLOAT((*(*info).curs).tlock)
	IF (ABS((*(*info).measparams).delta_lx) GT 0.) THEN BEGIN
		IF ((*(*info).measparams).delta_lx GT 0.) THEN BEGIN
			lower = (*(*info).curs).lxlock
			upper = lower + (*(*info).measparams).delta_lx - 1
		ENDIF ELSE BEGIN
			upper = (*(*info).curs).lxlock
			lower = upper + (*(*info).measparams).delta_lx + 1
		ENDELSE
		(*(*info).measparams).act_delta_lx = FLOAT(TOTAL((*(*(*info).dataparams).lxdist)[lower:upper])) 
	ENDIF ELSE (*(*info).measparams).act_delta_lx = 0.
	time = FLOAT((*(*info).measparams).delta_t) * FLOAT((*(*info).measparams).secondststep)
	distance = 149597870. * TAN(FLOAT((*(*info).measparams).act_delta_lx) / 3600. * !DTOR)
	IF ((time EQ 0.) OR ((time EQ 0.) AND (distance EQ 0.))) THEN BEGIN
		WIDGET_CONTROL, (*(*info).ctrlsmeas).speed_text, SET_VALUE = 'UNDEF'
	ENDIF ELSE BEGIN
		(*(*info).measparams).speed = FLOAT(distance) / ABS(FLOAT(time))
		WIDGET_CONTROL, (*(*info).ctrlsmeas).speed_text, SET_VALUE = STRTRIM((*(*info).measparams).speed, 2)
	ENDELSE

	WIDGET_CONTROL, (*(*info).ctrlsmeas).delta_lx_text, SET_VALUE = STRTRIM((*(*info).measparams).act_delta_lx,2)
	WIDGET_CONTROL, (*(*info).ctrlsmeas).delta_t_text, SET_VALUE = STRTRIM(ABS((*(*info).measparams).delta_t),2)
	WIDGET_CONTROL, (*(*info).ctrlsmeas).lx_params_text, SET_VALUE = '('+STRTRIM(FIX((*(*info).curs).lxlock),2)+','+STRTRIM(FIX((*(*info).dataparams).lx),2)+')'
	WIDGET_CONTROL, (*(*info).ctrlsmeas).t_params_text, SET_VALUE = '('+STRTRIM(FIX((*(*info).curs).tlock),2)+','+STRTRIM(FIX((*(*info).dataparams).t),2)+')'
END

;================================================================================= CLOSING AND PROGRAM EXIT PROCEDURES
PRO TANAT_CLEANUP, base
	WIDGET_CONTROL, base, GET_UVALUE = info
	PTR_FREE, info
END

PRO TANAT_CLOSE, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info, /DESTROY
	PTR_FREE, info
END

PRO TANAT_CLOSE_EVENT_WINDOW, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	IF (event.TOP EQ (*(*info).winids).errtlb) THEN (*(*info).winids).errtlb = 0
	IF (event.TOP EQ (*(*info).winids).savnamenulltlb) THEN (*(*info).winids).savnamenulltlb = 0
	IF (event.TOP EQ (*(*info).winids).savnamedoubletlb) THEN (*(*info).winids).savnamedoubletlb = 0
	WIDGET_CONTROL, event.TOP, /DESTROY
END

;================================================================================= CURSOR PROCEDURES
PRO TANAT_CURSOR, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_TRACKING' THEN BEGIN
		IF event.ENTER THEN BEGIN
			WIDGET_CONTROL, event.HANDLER, get_value = wid
			WSET, wid
			ci = UINTARR(16) & cim = ci & cim[8] = 1
			DEVICE, CURSOR_IMAGE = ci, CURSOR_MASK = cim, CURSOR_XY = [8,8]
		ENDIF ELSE BEGIN
			IF (((*(*info).measparams).parabolic_fit OR $
          (*(*info).measparams).series) AND $
          ((*(*info).measparams).np GE 1)) THEN BEGIN
				*(*(*info).measparams).lx_array = $
          (*(*(*info).measparams).lx_array)[0:(*(*info).measparams).np-1]
				*(*(*info).measparams).t_array = $
          (*(*(*info).measparams).t_array)[0:(*(*info).measparams).np-1]
				*(*(*info).measparams).slx_array = $
          (*(*(*info).measparams).slx_array)[0:(*(*info).measparams).np-1]
				*(*(*info).measparams).st_array = $
          (*(*(*info).measparams).st_array)[0:(*(*info).measparams).np-1]
				IF (*(*info).measparams).parabolic_fit THEN TANAT_PARABOLIC_FIT, event
				IF ((*(*info).measparams).series AND $
            (*(*info).dispswitch).series_feedback) THEN BEGIN
					IF ((*(*info).measparams).np GE 2) THEN $
            TANAT_SERIES_PATH, event $
           ELSE BEGIN
						*(*(*info).measparams).sxr = *(*(*info).measparams).slx_array
						*(*(*info).measparams).syr = *(*(*info).measparams).st_array
					ENDELSE
				ENDIF
				TANAT_DRAW, event
			ENDIF 
			DEVICE, /CURSOR_CROSSHAIR
		ENDELSE
		IF (((*(*info).feedbparams).verbosity)[3] EQ 1) THEN $
      TANAT_VERBOSE_GET, event, [event.ENTER], $
        labels=['WIDGET_TRACKING: event.Enter']
	ENDIF ELSE IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_DRAW' THEN BEGIN
		IF (((*(*info).feedbparams).verbosity)[3] EQ 1) THEN $
      TANAT_VERBOSE_GET, event, [event.TYPE,event.PRESS], $
        labels=['WIDGET_DRAW: event.Type','WIDGET_DRAW: event.Press']
    lxt = TANAT_TRANSFORM_DATA2DEVICE(info, LX_IN=event.X, $
      T_IN=event.Y, /MAIN, /INVERSE)
    IF (*(*info).dispswitch).gap_consider THEN BEGIN
      lowsub = INDGEN((*(*info).dataparams).ngaps)*2
      uppsub = lowsub+1
      (*(*info).dispswitch).curs_out_of_range = $
        ABS( TOTAL((lxt.lx GE (*(*(*info).dataparams).databounds)[lowsub]) AND $
              (lxt.lx LE (*(*(*info).dataparams).databounds)[uppsub]))-1) 
    ENDIF
		CASE event.TYPE OF
		0:	CASE event.PRESS OF	; pressed a mouse button
			1:	BEGIN	; left mouse button --> lock cursor to first (and subsequent) position(s)
					(*(*info).curs).lockset = 1
					IF (((*(*info).measparams).parabolic_fit EQ 0) AND $
            ((*(*info).measparams).series EQ 0)) THEN BEGIN
						WIDGET_CONTROL, (*(*info).ctrlsmeas).delta_lx_label, SENSITIVE = 1
						WIDGET_CONTROL, (*(*info).ctrlsmeas).delta_t_label, SENSITIVE = 1
						WIDGET_CONTROL, (*(*info).ctrlsmeas).delta_lx_text, SENSITIVE = 1
						WIDGET_CONTROL, (*(*info).ctrlsmeas).delta_t_text, SENSITIVE = 1
						WIDGET_CONTROL, (*(*info).ctrlsmeas).speed_label, SENSITIVE = 1
						WIDGET_CONTROL, (*(*info).ctrlsmeas).speed_text, SENSITIVE = 1
						WIDGET_CONTROL, (*(*info).ctrlsmeas).lx_params_label, SENSITIVE = 1
						WIDGET_CONTROL, (*(*info).ctrlsmeas).lx_params_text, SENSITIVE = 1
						WIDGET_CONTROL, (*(*info).ctrlsmeas).t_params_label, SENSITIVE = 1
						WIDGET_CONTROL, (*(*info).ctrlsmeas).t_params_text, SENSITIVE = 1
					ENDIF
					(*(*info).curs).slxlock = event.X
					(*(*info).curs).stlock = event.Y
          (*(*info).curs).lxlock = lxt.lx
          (*(*info).curs).tlock = lxt.t
					(*(*info).dataparams).lx = (*(*info).curs).lxlock
					(*(*info).dataparams).t = (*(*info).curs).tlock
					TANAT_CURSOR_SET_COORDSLIDERS, 0, 0, event
					IF ((*(*info).measparams).parabolic_fit OR $
              (*(*info).measparams).series) THEN BEGIN
						(*(*info).measparams).np += 1
						IF ((*(*info).measparams).np LT 2) THEN BEGIN
							*(*(*info).measparams).lx_array = (*(*info).curs).lxlock
							*(*(*info).measparams).t_array = (*(*info).curs).tlock
							*(*(*info).measparams).slx_array = (*(*info).curs).slxlock
							*(*(*info).measparams).st_array = (*(*info).curs).stlock
						ENDIF ELSE BEGIN
							*(*(*info).measparams).lx_array = $
                [*(*(*info).measparams).lx_array, (*(*info).curs).lxlock]
							*(*(*info).measparams).t_array = $
                [*(*(*info).measparams).t_array, (*(*info).curs).tlock]
							*(*(*info).measparams).slx_array = $
                [*(*(*info).measparams).slx_array, (*(*info).curs).slxlock]
							*(*(*info).measparams).st_array = $
                [*(*(*info).measparams).st_array, (*(*info).curs).stlock]
						ENDELSE
						WIDGET_CONTROL, (*(*info).ctrlsmeas).rem_but, $
              SENSITIVE = (((*(*info).measparams).np GE 2) AND $
                            (*(*info).measparams).parabolic_fit)
						WIDGET_CONTROL, (*(*info).ctrlsmeas).acc_label, $
              SENSITIVE = (((*(*info).measparams).np GE 2) AND $
                            (*(*info).measparams).parabolic_fit)
						WIDGET_CONTROL, (*(*info).ctrlsmeas).rem_series_but, $
              SENSITIVE = (((*(*info).measparams).np GE 3) AND $
                            (*(*info).measparams).series)
						WIDGET_CONTROL, (*(*info).ctrlsmeas).save_series_but, $
              SENSITIVE = (((*(*info).measparams).np GE 3) AND $
                            (*(*info).measparams).series)
						IF ((*(*info).measparams).series AND $
               ((*(*info).measparams).np GE 2) AND $
                (*(*info).dispswitch).series_feedback) THEN TANAT_SERIES_PATH, event
					ENDIF
				END
			2:	BEGIN	; middle mouse button --> lock cursor to last position
					IF (*(*info).curs).lockset THEN BEGIN
						(*(*info).curs).lockset = 2
						(*(*info).curs).slx = event.X
						(*(*info).curs).st = event.Y
            (*(*info).dataparams).lx = lxt.lx
            (*(*info).dataparams).t = lxt.t
						IF ((*(*info).measparams).parabolic_fit OR $
                (*(*info).measparams).series) THEN BEGIN
							(*(*info).measparams).np += 1
							*(*(*info).measparams).lx_array = $
                [*(*(*info).measparams).lx_array, (*(*info).dataparams).lx]
							*(*(*info).measparams).t_array = $
                [*(*(*info).measparams).t_array, (*(*info).dataparams).t]
							*(*(*info).measparams).slx_array = $
                [*(*(*info).measparams).slx_array, (*(*info).curs).slx]
							*(*(*info).measparams).st_array = $
                [*(*(*info).measparams).st_array, (*(*info).curs).st]
							WIDGET_CONTROL, (*(*info).ctrlsmeas).save_button, SENSITIVE = ((*(*info).measparams).parabolic_fit AND ((*(*info).measparams).np EQ 3))
						ENDIF ELSE WIDGET_CONTROL, (*(*info).ctrlsmeas).save_button, /SENSITIVE
						WIDGET_CONTROL, (*(*info).ctrlsmeas).flag_label, /SENSITIVE
						WIDGET_CONTROL, (*(*info).ctrlsmeas).flag_text, /SENSITIVE
						TANAT_CURSOR_SET_COORDSLIDERS, 0, 0, event
						IF (*(*info).measparams).parabolic_fit THEN TANAT_PARABOLIC_FIT, event
						IF ((*(*info).measparams).series AND (*(*info).dispswitch).series_feedback) THEN TANAT_SERIES_PATH, event
					ENDIF ELSE RETURN
				END
			4:	BEGIN	; right mouse button --> unlock cursor
					(*(*info).curs).lockset = 0
					TANAT_CURSOR_SET_COORDSLIDERS, 1, 1, event
					TANAT_RESET_OUTPUTS, event
				END
			ELSE: BREAK
			ENDCASE
		2:	BEGIN	; movement of mouse
				IF ((*(*info).curs).lockset NE 2) THEN BEGIN
					(*(*info).curs).slx = event.X
					(*(*info).curs).st = event.Y
          lxt = TANAT_TRANSFORM_DATA2DEVICE(info, LX_IN=(*(*info).curs).slx, $
            T_IN=(*(*info).curs).st, /MAIN, /INVERSE)
          (*(*info).dataparams).lx = lxt.lx
          (*(*info).dataparams).t = lxt.t
					IF ((*(*info).curs).lockset EQ 1) THEN BEGIN
						TANAT_CURSOR_SET_COORDSLIDERS, 0, 0, event
						IF (((*(*info).measparams).parabolic_fit EQ 0) AND $
                ((*(*info).measparams).series EQ 0)) THEN $
              TANAT_CALCULATE_DELTA, event
						IF (((*(*info).measparams).parabolic_fit OR $
                 (*(*info).measparams).series) AND $
                ((*(*info).measparams).np GE 1)) THEN BEGIN
							*(*(*info).measparams).lx_array = $
                [(*(*(*info).measparams).lx_array)[$
                0:(*(*info).measparams).np-1], (*(*info).dataparams).lx]
							*(*(*info).measparams).t_array = $
                [(*(*(*info).measparams).t_array)[$
                0:(*(*info).measparams).np-1], (*(*info).dataparams).t]
							*(*(*info).measparams).slx_array = $
                [(*(*(*info).measparams).slx_array)[$
                0:(*(*info).measparams).np-1], (*(*info).curs).slx]
							*(*(*info).measparams).st_array = $
                [(*(*(*info).measparams).st_array)[$
                0:(*(*info).measparams).np-1], (*(*info).curs).st]
							IF (*(*info).measparams).parabolic_fit THEN TANAT_PARABOLIC_FIT, event
							IF ((*(*info).measparams).series AND $
                (*(*info).dispswitch).series_feedback) THEN TANAT_SERIES_PATH, event
						ENDIF
					ENDIF ELSE BEGIN
						TANAT_CURSOR_SET_COORDSLIDERS, 1, 1, event
					ENDELSE
				ENDIF ELSE RETURN
			END
		ELSE: RETURN
		ENDCASE
	ENDIF
	TANAT_DRAW, event
END

PRO TANAT_CURSOR_SET_COORDSLIDERS, xsensitive, ysensitive, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	WIDGET_CONTROL, (*(*info).ctrlsmeas).lx_slider, SET_VALUE = (*(*info).dataparams).lx, SENSITIVE = xsensitive
	WIDGET_CONTROL, (*(*info).ctrlsmeas).t_slider, SET_VALUE = (*(*info).dataparams).t, SENSITIVE = ysensitive
END
 
;================================================================================= BINARY CONVERSION FUNCTIONS
FUNCTION TANAT_DEC2BIN, decimal_number
; Handles the conversion of decimal to binary number
	IF (decimal_number NE 0) THEN BEGIN
		coarse_p = ALOG10(DOUBLE(decimal_number))/ALOG10(2.D)
		p = FLOOR(coarse_p)
		firstpass = 1
		i=0
		binary_array = INTARR(FLOOR(coarse_p)+1)
		b = REVERSE((2^FINDGEN(FLOOR(coarse_p)+1)))
		WHILE (p GE 0) DO BEGIN
			IF firstpass THEN BEGIN
				IF (2^p LE decimal_number) THEN BEGIN
					binary_array[i] = 1
					decimal_number = decimal_number - 2^p
				ENDIF ELSE binary_array[i] = 0
			ENDIF ELSE BEGIN
				IF (2^p LE decimal_number) THEN BEGIN
					binary_array[i] = 1
					decimal_number = decimal_number - 2^p
				ENDIF ELSE binary_array[i] = 0
				firstpass = 0
			ENDELSE
			p = p - 1
			i = i + 1
		ENDWHILE
		binary_array = REVERSE(binary_array)
		IF (N_ELEMENTS(binary_array) LT 5) THEN binary_array = [binary_array, REPLICATE(0,5-N_ELEMENTS(binary_array))]
	ENDIF ELSE binary_array = [0,0,0,0,0]
	RETURN, binary_array
END

;================================================================================= DRAW PROCEDURES
PRO TANAT_DRAW, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	TANAT_DRAW_SLICE, event
	IF (*(*info).dispswitch).ref THEN TANAT_DRAW_SLICE, event, /REFERENCE
	TANAT_DRAW_LS, event
END

PRO TANAT_DRAW_SLICE, event, REFERENCE=reference
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
  IF ~KEYWORD_SET(REFERENCE) THEN BEGIN
	  WSET, (*(*info).winids).wid
    imrefscaling = 0
    selected_data = *(*(*info).data).loopslice
    ngaps = (*(*info).dataparams).ngaps
    databounds = *(*(*info).dataparams).databounds
    wdatabounds = *(*(*info).dataparams).wdatabounds
    empty_slice = *(*(*info).data).empty_slice
    gap_consider = (*(*info).dispswitch).gap_consider
  ENDIF ELSE BEGIN
	  WSET, (*(*info).winids).refwid
    imrefscaling = 1
    selected_data = *(*(*info).data).refloopslice
    ngaps = (*(*info).dataparams).ngaps_ref
    databounds = *(*(*info).dataparams).databounds_ref
    wdatabounds = *(*(*info).dataparams).wdatabounds_ref
    empty_slice = *(*(*info).data).ref_empty_slice
    gap_consider = (*(*info).dispswitch).ref_gap_consider
  ENDELSE
  IF ((*(*info).scaling).gamma[imrefscaling] NE 1.) THEN BEGIN
    wherelt0 = WHERE(selected_data LT 0, count)
    IF (count LE 0) THEN $
      selected_data = $
        (TEMPORARY(selected_data))^(*(*info).scaling).gamma[imrefscaling] $
    ELSE BEGIN
      selected_data = $
        (TEMPORARY(ABS(selected_data)))^(*(*info).scaling).gamma[imrefscaling]
      selected_data[wherelt0] *= -1
    ENDELSE
  ENDIF
  minimum = MIN(selected_data, MAX=maximum, /NAN)
;  IF ((*(*(*info).scaling).imagescale)[sel] EQ 2) THEN BEGIN
;    selected_data = IRIS_HISTO_OPT(TEMPORARY(selected_data), $
;      (*(*info).scaling).histo_opt_val[scale_idx], MISSING=-32768, /SILENT)
;    minimum = MIN(selected_data, MAX=maximum, /NAN)
;  ENDIF
  minmax = TANAT_SCALING_CONTRAST(minimum,maximum,$
    (*(*info).scaling).minimum[imrefscaling],(*(*info).scaling).maximum[imrefscaling])
  ; Handle gaps in the data
  IF gap_consider THEN BEGIN
    dispslice_data = selected_data
    ; Arbitrary, but consistent, minimum value such that the gaps are really
    ; black compared to the data values
    min_dispslice_data = $
      -(1+(MIN(dispslice_data) LT 0)*2) * ABS(MIN(dispslice_data))
    IF (empty_slice[0,0] NE min_dispslice_data) THEN $
      REPLICATE_INPLACE, empty_slice, min_dispslice_data
    dispslice = empty_slice[*, $
      (*(*info).dataparams).t_low:(*(*info).dataparams).t_upp]
    FOR i=0,N_ELEMENTS(databounds)/2-1 DO $
      dispslice[wdatabounds[2*i]:wdatabounds[2*i+1], *] = $
        dispslice_data[databounds[2*i]:databounds[2*i+1],*]
  ENDIF ELSE $
    dispslice = selected_data
	finalimage = BYTSCL(dispslice, MIN = minmax[0], MAX = minmax[1], /NAN) 
  TV, CONGRID(finalimage,(*(*info).winsizes).windowx,$
    (*(*info).winsizes).windowy,INTERP=(*(*info).dispswitch).smoothed)
	TANAT_DRAW_CURSCROSS_PLOT, event
	TANAT_DRAW_OVERLAYS, event
END

PRO TANAT_DRAW_OVERLAYS, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	IF (*(*info).dispswitch).overlay_measurements THEN BEGIN
		FOR k=0,999 DO BEGIN
			IF ((*(*(*info).overlays).lxbpoint)[k] NE $
          (*(*(*info).overlays).lxepoint)[k]) AND $
         ((*(*(*info).overlays).tbpoint)[k] NE $
          (*(*(*info).overlays).tepoint)[k]) THEN BEGIN
          slxtb = TANAT_TRANSFORM_DATA2DEVICE(info,$
            LX_IN=(*(*(*info).overlays).lxbpoint)[k], $
            T_IN=(*(*(*info).overlays).tbpoint)[k], /MAIN)
          slxte = TANAT_TRANSFORM_DATA2DEVICE(info,$
            LX_IN=(*(*(*info).overlays).lxepoint)[k], $
            T_IN=(*(*(*info).overlays).tepoint)[k], /MAIN)
        PLOTS, [slxtb.lx, slxtb.t], [slxte.lx, slxte.t], /DEVICE
        PLOTS, [slxtb.lx, slxtb.t], [slxte.lx, slxte.t], /DEVICE, PSYM=6
        PLOTS, [slxtb.lx, slxtb.t], [slxte.lx, slxte.t], /DEVICE, PSYM=7
        ; Get coordinates for xyout of measurement ID
				IF ((*(*(*info).overlays).lxepoint)[k] GT $
            (*(*(*info).overlays).lxbpoint)[k]) THEN BEGIN
					upp_x = slxte.lx  
					low_x = slxtb.lx 
				ENDIF ELSE BEGIN
					upp_x = slxtb.lx
					low_x = slxte.lx
        ENDELSE
				IF ((*(*(*info).overlays).tepoint)[k] GT $
            (*(*(*info).overlays).tbpoint)[k]) THEN BEGIN
          upp_t = slxte.t
          low_t = slxtb.t
        ENDIF ELSE BEGIN
          upp_t = slxtb.t
          low_t = slxte.t
        ENDELSE
				XYOUTS, (upp_x-low_x)/2.+low_x, (upp_t-low_t)/2.+low_t+5, $
          (*(*(*info).measparams).meas_id)[k], /DEVICE
			ENDIF
		ENDFOR
	ENDIF
END

PRO TANAT_DRAW_LS, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	IF (*(*info).dispswitch).slab_set THEN BEGIN
		WSET, (*(*info).winids).ls_drawid
		PLOT, *(*(*info).dataparams).lps, *(*(*info).dataparams).spec, $
      POS=[(*(*info).plotpos).lsx0,(*(*info).plotpos).lsy0,$
      (*(*info).plotpos).lsx1,(*(*info).plotpos).lsy1],/NORM, CHARSIZE=1,YS=1,$
			YR=[(*(*info).plotaxes).ls_low_y,(*(*info).plotaxes).ls_upp_y],$
      XSTYLE = (*(*info).dispswitch).v_dop_set * 8 + 1, $
      XTITLE = (*(*info).plotaxes).spxtitle, YTITLE = 'Scaled intensity', $
			BACKGROUND = (*(*info).plotparams).bgplotcol,$
      COLOR = (*(*info).plotparams).plotcol, /NODATA
		IF ( (*(*info).dispswitch).v_dop_set EQ 1) THEN BEGIN
			AXIS, XAXIS=1, XRANGE = [((*(*info).plotaxes).v_dop)[0], $
        ((*(*info).plotaxes).v_dop)[(*(*info).dataparams).lp_last]], $
        XSTYLE=1, XTITLE = 'Doppler velocity [km/s]', $
        COLOR = (*(*info).plotparams).plotcol
			XYOUTS,(*(*(*info).dataparams).lps)[FLOOR((*(*info).dataparams).lp_last-6/23.*(*(*info).dataparams).lp_last)],0.9*(*(*info).plotaxes).ls_yrange + (*(*info).plotaxes).ls_low_y, $
				STRTRIM(STRING((*(*info).plotaxes).v_dop[(*(*info).dataparams).lp],FORMAT='(3(F9.2,x))'),2)+' km/s', COLOR = (*(*info).plotparams).plotcol
		ENDIF
		IF ((*(*info).dispswitch).singlepos EQ 0) THEN BEGIN
			PLOTS, [1,1] * (*(*(*info).dataparams).lps)[(*(*info).dataparams).low_low_val],[(*(*info).plotaxes).ls_low_y,(*(*info).plotaxes).ls_upp_y], COLOR = (*(*info).plotparams).plotcol, LINESTYLE = 5
			PLOTS, [1,1] * (*(*(*info).dataparams).lps)[(*(*info).dataparams).upp_upp_val],[(*(*info).plotaxes).ls_low_y,(*(*info).plotaxes).ls_upp_y], COLOR = (*(*info).plotparams).plotcol, LINESTYLE = 5
			IF (*(*info).dispswitch).multpos THEN BEGIN
				POLYFILL,[(*(*(*info).dataparams).lps)[(*(*info).dataparams).low_low_val],(*(*(*info).dataparams).lps)[(*(*info).dataparams).upp_low_val],$
					(*(*(*info).dataparams).lps)[(*(*info).dataparams).upp_low_val],(*(*(*info).dataparams).lps)[(*(*info).dataparams).low_low_val]], [(*(*info).plotaxes).ls_low_y, (*(*info).plotaxes).ls_low_y, $
					(*(*info).plotaxes).ls_upp_y, (*(*info).plotaxes).ls_upp_y], COLOR = 200
				POLYFILL,[(*(*(*info).dataparams).lps)[(*(*info).dataparams).upp_upp_val],(*(*(*info).dataparams).lps)[(*(*info).dataparams).low_upp_val],$
					(*(*(*info).dataparams).lps)[(*(*info).dataparams).low_upp_val],(*(*(*info).dataparams).lps)[(*(*info).dataparams).upp_upp_val]], [(*(*info).plotaxes).ls_low_y, (*(*info).plotaxes).ls_low_y, $
					(*(*info).plotaxes).ls_upp_y, (*(*info).plotaxes).ls_upp_y], COLOR = 200
				PLOTS, [1,1] * (*(*(*info).dataparams).lps)[(*(*info).dataparams).low_upp_val],[(*(*info).plotaxes).ls_low_y,(*(*info).plotaxes).ls_upp_y], COLOR = (*(*info).plotparams).plotcol, LINESTYLE = 5
				PLOTS, [1,1] * (*(*(*info).dataparams).lps)[(*(*info).dataparams).upp_low_val],[(*(*info).plotaxes).ls_low_y,(*(*info).plotaxes).ls_upp_y], COLOR = (*(*info).plotparams).plotcol, LINESTYLE = 5
			ENDIF
		ENDIF
		OPLOT, (*(*(*info).dataparams).lps),*(*(*info).dataparams).spec,$
      COLOR = (*(*info).plotparams).plotcol
    IF ((*(*info).dispswitch).curs_out_of_range EQ 0) THEN BEGIN
  		ssp = (  *(*(*info).data).loopdata)[FIX((*(*info).dataparams).lx),FIX((*(*info).dataparams).t),*]/REPLICATE((*(*info).dataparams).ms,(*(*info).dataparams).nlp)
  		OPLOT, (*(*(*info).dataparams).lps)+(*(*info).dataparams).lp_first, ssp, LINE=2, COLOR = (*(*info).plotparams).plotcol
  		IF (*(*info).dispswitch).subtract THEN BEGIN
  			OPLOT, (*(*(*info).dataparams).lps), *(*(*info).dataparams).spec-ssp, COLOR = (*(*info).plotparams).plotcol
  			OPLOT, (*(*(*info).dataparams).lps), *(*(*info).dataparams).spec-ssp, PSYM = 4, COLOR = (*(*info).plotparams).plotcol
  		ENDIF
    ENDIF ELSE $
      XYOUTS, (!X.CRANGE[1]-!X.CRANGE[0])/2.+!X.CRANGE, $
        (!Y.CRANGE[1]-!Y.CRANGE[0])/2.+!Y.CRANGE, $
        'No data available for!Cselected pixel position', $
        COLOR=(*(*info).plotparams).plotcol, ALIGN=0.5, CHARSIZE=1.2, /DATA
		IF ((*(*info).plotaxes).ls_low_y LT 0.) THEN PLOTS, [((*(*(*info).dataparams).lps))[0],((*(*(*info).dataparams).lps))[(*(*info).dataparams).nlp-1]],[0.,0.], COLOR = (*(*info).plotparams).plotcol
		PLOTS, [1,1] * (*(*(*info).dataparams).lps)[(*(*info).dataparams).lp],[(*(*info).plotaxes).ls_low_y,(*(*info).plotaxes).ls_upp_y], COLOR = (*(*info).plotparams).plotcol
	ENDIF 
END

PRO TANAT_DRAW_CURSCROSS_PLOT, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	PLOTS, (*(*info).curs).slx,(*(*info).curs).st, /DEVICE, COLOR = !P.COLOR, PSYM = 1
	IF ((*(*info).curs).lockset GE 1) THEN BEGIN
		IF ((*(*info).measparams).parabolic_fit OR (*(*info).measparams).series) THEN BEGIN
			IF (*(*info).measparams).parabolic_fit THEN BEGIN
				IF ((*(*info).measparams).np LT 3) THEN PLOTS, *(*(*info).measparams).slx_array, *(*(*info).measparams).st_array, /DEVICE, COLOR = !P.COLOR ELSE $
					PLOTS, *(*(*info).measparams).x_vals, *(*(*info).measparams).y_vals, /DEVICE, COLOR = !P.COLOR
			ENDIF
			PLOTS, *(*(*info).measparams).slx_array, *(*(*info).measparams).st_array, /DEVICE, COLOR = !P.COLOR, PSYM = 1
			PLOTS, *(*(*info).measparams).slx_array, *(*(*info).measparams).st_array, /DEVICE, COLOR = !P.COLOR, PSYM = 4
			IF ((*(*info).measparams).series AND (*(*info).dispswitch).series_feedback) THEN PLOTS,*(*(*info).measparams).sxr,*(*(*info).measparams).syr, /DEVICE, COLOR = !P.COLOR
		ENDIF ELSE BEGIN
			PLOTS, (*(*info).curs).slxlock, (*(*info).curs).stlock, /DEVICE, COLOR = !P.COLOR, PSYM = 1
			PLOTS, (*(*info).curs).slxlock, (*(*info).curs).stlock, /DEVICE, COLOR = !P.COLOR, PSYM = 4
			PLOTS, [(*(*info).curs).slx,(*(*info).curs).slxlock],[(*(*info).curs).st,(*(*info).curs).stlock], /DEVICE, COLOR = !P.COLOR
			IF ((*(*info).curs).lockset EQ 2) THEN PLOTS, (*(*info).curs).slx, (*(*info).curs).st, /DEVICE, COLOR = !P.COLOR, PSYM = 4
		ENDELSE
	ENDIF
END

PRO TANAT_DRAW_OVERLAY_SAVED_MEASUREMENTS, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	(*(*info).dispswitch).overlay_measurements = event.SELECT
	IF (*(*info).dispswitch).overlay_measurements THEN BEGIN
		IF (FILE_INFO((*(*info).measparams).savefilename)).EXISTS THEN BEGIN
			nlines = FILE_LINES((*(*info).measparams).savefilename)
			datarr = STRARR(1,nlines)
			OPENR,unit1,(*(*info).measparams).savefilename,/GET_LUN
			READF,unit1,datarr
			FREE_LUN,unit1
			k=0
			cutfilename = STRMID((*(*info).dataparams).filename,$
        STRPOS((*(*info).dataparams).filename,'/')+1,$
        STRLEN((*(*info).dataparams).filename))
			FOR i=0,nlines-1 DO BEGIN
				lastline_1 = STRSPLIT(datarr(0,i), ' ', /EXTRACT)
				lastline_2 = STRSPLIT(datarr(0,i), '	', /EXTRACT)
				IF (SIZE(lastline_1, /DIMENSIONS) GT 1) THEN $
          splitline = lastline_1 $
        ELSE IF (SIZE(lastline_2, /DIMENSIONS) GT 1) THEN $
          splitline = lastline_2
				IF ((splitline[0] EQ (*(*info).dataparams).filename) OR $
            (splitline[0] EQ cutfilename)) THEN BEGIN
					(*(*(*info).overlays).lxbpoint)[k] = (FLOAT(splitline[3]))
					(*(*(*info).overlays).lxepoint)[k] = (FLOAT(splitline[4]))
					(*(*(*info).overlays).tbpoint)[k] = (FLOAT(splitline[6]))
					(*(*(*info).overlays).tepoint)[k] = (FLOAT(splitline[7]))
					(*(*(*info).measparams).meas_id)[k] = (STRSPLIT(splitline[2],'.',/EXTRACT))[1]
					k = k+1
				ENDIF
			ENDFOR
		ENDIF ELSE BEGIN
			CD,CURRENT=curpath
			TANAT_WINDOW_OK, event, 'TANAT: ERROR!','TANAT could not overlay saved measurements as '+(*(*info).measparams).savefilename,' does not exist in the current working directory','('+curpath+PATH_SEP()+').',$
				OK_EVENT='TANAT_CLOSE_EVENT_WINDOW', BASE=tlb
			(*(*info).winids).errtlb = tlb
			WIDGET_CONTROL, (*(*info).ctrlsmeas).overlay_button, SET_BUTTON = 0
		ENDELSE
	ENDIF
	TANAT_DRAW, event
END

;================================================================================= TAB EVENT PROCEDURES
PRO TANAT_EVENT, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
END

;==================== FILE OPENING PROCEDURES
PRO TANAT_FILE_OPEN, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	new_filename = DIALOG_PICKFILE(TITLE='TANAT: Choose a CSAV file', /READ, /MUST_EXIST, FILTER='*.csav',/FIX_FILTER)
	IF (STRCOMPRESS(new_filename) NE '') THEN BEGIN
		WIDGET_CONTROL,/HOURGLASS
    TANAT_FILE_RESTORE, new_filename, SPECTRUM=spectrum, MS=ms,$
      SPECT_POS_SAV=spect_pos, SPECT_POS_LOW=spect_pos_low, $
      SPECT_POS_UPP=spect_pos_upp, $
      X_LOOP_PTS=x_loop_pts, Y_LOOP_PTS=y_loop_pts,$
      NLX=nlx, NT=nt, NLP=nlp, LOOP_DATA=loop_data, SLAB_SET=slab_set,$
      W_FIRST=w_first, W_LAST=w_last, W_SET=w_set, $
      NGAPS=ngaps, DATABOUNDS=databounds, WDATABOUNDS=wdatabounds
		*(*(*info).dataparams).spec = spectrum		&	(*(*info).dataparams).ms = ms
		(*(*info).dataparams).nlx = nlx			&	(*(*info).dataparams).nt = nt
		(*(*info).dataparams).nlp = nlp			&	(*(*info).dispswitch).slab_set = slab_set
		*(*(*info).data).loopdata = loop_data		&	*(*(*info).data).loopslice = FLTARR(nlx,nt)
		(*(*info).dataparams).filename = new_filename	
    ; Failsafe against older save files where spect_pos(_low/upp) were saved
    ; differently in CRISPEX
    IF ((spect_pos NE spect_pos_low) AND (spect_pos_low EQ spect_pos_upp)) THEN $
      (*(*info).dataparams).lp = spect_pos_low $
    ELSE $
      (*(*info).dataparams).lp = spect_pos - spect_pos_low
;    (*(*info).dataparams).lp = spect_pos - spect_pos_low
		(*(*info).dataparams).w_first = w_first		&	(*(*info).dataparams).w_last = w_last
		(*(*info).dataparams).w_set = w_set
		TANAT_SET_SPECTPARAMS, spectrum, nlp, spect_pos, LINE_CENTER=line_center,$
      LPS=lps, LC=lc, SPXTITLE=spxtitle, V_DOP_VALS=v_dop_vals,$
      V_DOP_SET=v_dop_set
		*(*(*info).dataparams).lps = lps		&	(*(*info).dataparams).lc = lc
		(*(*info).plotaxes).spxtitle = spxtitle		&	(*(*info).plotaxes).v_dop = v_dop_vals
		(*(*info).dispswitch).v_dop_set = v_dop_set	
    (*(*info).dataparams).lp_first = 0
		(*(*info).dataparams).lp_last = spect_pos_upp - spect_pos_low
  	TANAT_SET_TIMESLICE_PARAMS, nlx, nt, x_loop_pts, y_loop_pts, $
      (*(*info).measparams).arcsecpix, LXDIST=lxdist, CUMUL_LXDIST=cumul_lxdist,$
      LX_FIRST=lx_first, LX_LAST=lx_last, T_FIRST=t_first, T_LAST=t_last, $
      NLXPTS=nlxpts
    (*(*info).dispswitch).gap_consider = $
      ((ngaps GE 1) AND (*(*info).dispswitch).ref)
		*(*(*info).dataparams).lxdist = lxdist		&	(*(*info).dataparams).lx = FLOAT(lx_first)
    *(*(*info).dataparams).cumul_lxdist = cumul_lxdist
    (*(*info).dataparams).nlxpts = nlxpts
    (*(*info).dataparams).lx_first = lx_first
    IF (*(*info).dispswitch).gap_consider THEN $
      (*(*info).dataparams).lx_last = lx_last $
    ELSE $
      (*(*info).dataparams).lx_last = nlx-1
		(*(*info).dataparams).t = FLOAT(t_first)	&	(*(*info).curs).slx = FLOAT(lx_first)
		(*(*info).dataparams).t_low = FLOAT(t_first)	&	(*(*info).dataparams).t_upp = FLOAT(t_last)
		(*(*info).dataparams).t_first = FLOAT(t_first)	&	(*(*info).dataparams).t_last = FLOAT(t_last)
		(*(*info).curs).st = FLOAT(t_first)		&	(*(*info).curs).lxlock = FLOAT(lx_first)
		(*(*info).curs).tlock = FLOAT(t_first)		&	(*(*info).curs).slxlock	= FLOAT(lx_first)
		(*(*info).curs).stlock = FLOAT(t_first)
    (*(*info).dataparams).ngaps = ngaps
    (*(*info).dataparams).databounds = PTR_NEW(databounds)
    (*(*info).dataparams).wdatabounds = PTR_NEW(wdatabounds)
    (*(*info).data).empty_slice = $
      PTR_NEW(MAKE_ARRAY(N_ELEMENTS(x_loop_pts),nt,TYPE=(SIZE(loop_data, /TYPE))))
		TANAT_SET_CONTROLS, event
		TANAT_T_RANGE, event
		IF (slab_set NE 1) THEN BEGIN
			WSET, (*(*info).winids).ls_drawid
			TV,CONGRID(REPLICATE(200,10,10),(*(*info).winsizes).lswinx,(*(*info).winsizes).lswiny)
			XYOUTS,(*(*info).winsizes).lswinx/2.,(*(*info).winsizes).lswiny/2.,'Could not display spectral information as!Conly one spectral position is available.', COLOR = 0, ALIGNMENT = 0.5, /DEVICE
		ENDIF
		TANAT_SET_SAVEFILENAME, event
	ENDIF
END

PRO TANAT_FILE_RESTORE, filename, SPECTRUM=spectrum, MS=ms, $
  SPECT_POS_SAV=spect_pos_sav, SPECT_POS_LOW=spect_pos_low, SPECT_POS_UPP=spect_pos_upp,$
  X_LOOP_PTS=x_loop_pts, Y_LOOP_PTS=y_loop_pts, NLX=nlx,$
  NT=nt, NLP=nlp, LOOP_DATA=loop_data, SLAB_SET=slab_set, W_FIRST=w_first,$
  W_LAST=w_last, W_SET=w_set, NGAPS=ngaps, DATABOUNDS=databounds, $
  WDATABOUNDS=wdatabounds
	RESTORE, filename
	slab_set = 0
	IF (N_ELEMENTS(loop_slab) GT 0) THEN BEGIN			; Old CRISPEX versions failsafe
		slab_set = ((SIZE(loop_slab))[0] GE 3) 
		w_set = ((SIZE(loop_slab))[0] EQ 4)
		loop_data = loop_slab
	ENDIF ELSE IF (N_ELEMENTS(loop_slice) GT 0) THEN BEGIN
		slab_set = ((SIZE(loop_slice))[0] GE 3)
		w_set = ((SIZE(loop_slice))[0] EQ 4)
		loop_data = loop_slice
	ENDIF
	IF slab_set THEN BEGIN
		spectrum = average_spectrum
		ms	= scaling_factor
		nlp	= (SIZE(spectrum))[1]
	ENDIF ELSE BEGIN
		spectrum= 0
		ms	= 1
		nlp 	= 1
	ENDELSE
  spect_pos_sav = spect_pos
  ; Failsafe against old csav files
  IF (N_ELEMENTS(SPECT_POS_LOW) NE 1) OR $
      (N_ELEMENTS(SPECT_POS_UPP) NE 1) THEN BEGIN
    spect_pos_low = 0
    spect_pos_upp = nlp-1
  ENDIF
	nlx = loop_size
	nt = (SIZE(loop_data))[2]
	w_first = 0
	IF w_set THEN w_last = (SIZE(loop_data))[3]-1 ELSE w_last = 1
  ; Handle gaps in data, failsafe against older csav files
  IF (N_ELEMENTS(ngaps) NE 1) THEN BEGIN
    ngaps = 0
    databounds = 0
    wdatabounds = 0
  ENDIF 
END

;================================================================================= DETAILED SPECTRUM PROCEDURES
PRO TANAT_LS_LOW, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	WIDGET_CONTROL, (*(*info).ctrlsgen).lower_y_text, GET_VALUE = textvalue
	(*(*info).plotaxes).ls_low_y = FLOAT(textvalue[0])
	TANAT_LS_RANGE, event
END

PRO TANAT_LS_UPP, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	WIDGET_CONTROL, (*(*info).ctrlsgen).upper_y_text, GET_VALUE = textvalue
	(*(*info).plotaxes).ls_upp_y = FLOAT(textvalue[0])
	TANAT_LS_RANGE, event
END

PRO TANAT_LS_RANGE, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	(*(*info).plotaxes).ls_yrange = (*(*info).plotaxes).ls_upp_y - (*(*info).plotaxes).ls_low_y
	TANAT_DRAW, event
END

PRO TANAT_LS_SUBTRACT, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	(*(*info).dispswitch).subtract = event.SELECT
	TANAT_DRAW, event
END

;================================================================================= PARABOLIC FIT PROCEDURES
PRO TANAT_PARABOLIC_FIT, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	degree = ((*(*info).measparams).np-1) < 2
	degree = degree > 0
	s_result = POLY_FIT(FLOAT(*(*(*info).measparams).st_array), FLOAT(*(*(*info).measparams).slx_array), degree)
	act_t_array = *(*(*info).measparams).t_array * (*(*info).measparams).secondststep
  tmp_act_lx_array = INTERPOLATE(*(*(*info).dataparams).cumul_lxdist,$
    *(*(*info).measparams).lx_array)
	act_lx_array =  149597870. * TAN(FLOAT(tmp_act_lx_array) / 3600. * !DTOR)
	act_s_result = POLY_FIT(act_t_array,act_lx_array, degree)
	IF ((*(*info).measparams).np GT 2) THEN BEGIN	
		IF ((*(*(*info).measparams).slx_array)[(*(*info).measparams).np-1] LT (*(*(*info).measparams).slx_array)[0]) THEN sign = -1. ELSE sign = 1.
		*(*(*info).measparams).y_vals = FINDGEN(1000)/1001 * ( (*(*(*info).measparams).st_array)[(*(*info).measparams).np-1] - (*(*(*info).measparams).st_array)[0] ) + (*(*(*info).measparams).st_array)[0]
		*(*(*info).measparams).x_vals = s_result[2] * (FLOAT(*(*(*info).measparams).y_vals))^2 + s_result[1] * FLOAT(*(*(*info).measparams).y_vals) + s_result[0]
		(*(*info).measparams).acceleration = 2000. * act_s_result[2] * sign
		WIDGET_CONTROL, (*(*info).ctrlsmeas).acc_text, SET_VALUE = STRTRIM((*(*info).measparams).acceleration,2), /SENSITIVE
	ENDIF
END

PRO TANAT_PARABOLIC_FIT_SET, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	(*(*info).measparams).parabolic_fit = event.SELECT
	WIDGET_CONTROL,(*(*info).ctrlsmeas).set_series_but, SENSITIVE = ABS(event.SELECT-1)
	WIDGET_CONTROL,(*(*info).ctrlsmeas).fdb_series_but, SENSITIVE = ABS(event.SELECT-1)
	IF ((*(*info).curs).lockset GT 0) THEN TANAT_RESET_OUTPUTS, event
	IF ((*(*info).measparams).parabolic_fit EQ 0) THEN BEGIN
		(*(*info).measparams).np = 0
		*(*(*info).measparams).lx_array = 0.
		*(*(*info).measparams).t_array = 0.
		*(*(*info).measparams).slx_array = 0.
		*(*(*info).measparams).st_array = 0.
		*(*(*info).measparams).x_vals = 0.
		*(*(*info).measparams).y_vals = 0.
		WIDGET_CONTROL, (*(*info).ctrlsmeas).rem_but, SENSITIVE = 0
		WIDGET_CONTROL, (*(*info).ctrlsmeas).acc_label, SENSITIVE = 0
		WIDGET_CONTROL, (*(*info).ctrlsmeas).acc_text, SET_VALUE = '0', SENSITIVE = 0
		(*(*info).curs).lockset = 0
		TANAT_DRAW, event
	ENDIF
END

PRO TANAT_REMOVE_POINT, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	(*(*info).measparams).np -= 1
	*(*(*info).measparams).lx_array = (*(*(*info).measparams).lx_array)[0:(*(*info).measparams).np-1]
	*(*(*info).measparams).t_array = (*(*(*info).measparams).t_array)[0:(*(*info).measparams).np-1]
	*(*(*info).measparams).slx_array = (*(*(*info).measparams).slx_array)[0:(*(*info).measparams).np-1]
	*(*(*info).measparams).st_array = (*(*(*info).measparams).st_array)[0:(*(*info).measparams).np-1]
	IF ((*(*info).measparams).np LE 2) THEN BEGIN
		IF (*(*info).measparams).parabolic_fit THEN BEGIN
			WIDGET_CONTROL, (*(*info).ctrlsmeas).rem_but, SENSITIVE = 0
			WIDGET_CONTROL, (*(*info).ctrlsmeas).acc_label, SENSITIVE = 0
			WIDGET_CONTROL, (*(*info).ctrlsmeas).acc_text, SET_VALUE = '0', SENSITIVE = 0
		ENDIF ELSE IF (*(*info).measparams).series THEN BEGIN
			WIDGET_CONTROL, (*(*info).ctrlsmeas).rem_series_but, SENSITIVE = 0
			WIDGET_CONTROL, (*(*info).ctrlsmeas).save_series_but, SENSITIVE = 0
		ENDIF
	ENDIF
	IF (*(*info).measparams).parabolic_fit THEN BEGIN
		WIDGET_CONTROL, (*(*info).ctrlsmeas).save_button, SENSITIVE = ((*(*info).measparams).np EQ 3) 
		TANAT_PARABOLIC_FIT, event
	ENDIF
	IF ((*(*info).measparams).series AND (*(*info).dispswitch).series_feedback) THEN TANAT_SERIES_PATH, event
	TANAT_DRAW, event
END

;================================================================================= PLAYBACK PROCEDURES
PRO TANAT_PB_BG, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	CASE (*(*info).pbparams).spmode OF
		1	: BEGIN
				(*(*info).dataparams).lp += (*(*info).pbparams).spdirection * (*(*info).pbparams).lp_step
				(*(*info).pbparams).spdirection *= -1
				IF ((*(*info).dataparams).lp GT (*(*info).dataparams).lp_last) THEN (*(*info).dataparams).lp -= (*(*info).dataparams).nlp ELSE $
					IF ((*(*info).dataparams).lp LT (*(*info).dataparams).lp_first) THEN (*(*info).dataparams).lp += (*(*info).dataparams).nlp
				WIDGET_CONTROL,(*(*info).ctrlsmeas).lp_slider, SET_VALUE = (*(*info).dataparams).lp
			  END
		ELSE: RETURN
	ENDCASE 
	WIDGET_CONTROL, (*(*info).pbparams).bg, TIMER = 1. / (*(*info).pbparams).lp_speed
	TANAT_UPDATE_LP, event
	TANAT_DRAW, event
END

PRO TANAT_PB_SPECTBLINK, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	(*(*info).pbparams).spmode = event.SELECT
	IF (*(*info).pbparams).spmode THEN BEGIN
		(*(*info).pbparams).spmode = 1
		(*(*info).pbparams).spdirection = 1
		WIDGET_CONTROL, (*(*info).pbparams).bg, TIMER = 0.0
	ENDIF
END

;================================================================================= COMBINED SPECTRAL POSITIONS PROCEDURES
PRO TANAT_POS_SINGLE, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	(*(*info).dispswitch).singlepos = event.SELECT
	WIDGET_CONTROL, (*(*info).ctrlsmeas).lp_slider, $
    SENSITIVE = (*(*info).dispswitch).singlepos
	WIDGET_CONTROL, (*(*info).ctrlsmeas).lp_speed_slider, $
    SENSITIVE = (*(*info).dispswitch).singlepos
	WIDGET_CONTROL, (*(*info).ctrlsmeas).lp_blink_slider, $
    SENSITIVE=((*(*info).dispswitch).singlepos AND $
              ((*(*info).dataparams).nlp GE 3))
	WIDGET_CONTROL, (*(*info).ctrlsmeas).lp_blink_button, $
    SENSITIVE = (*(*info).dispswitch).singlepos
	TANAT_UPDATE_LP, event
	TANAT_DRAW, event
END

PRO TANAT_POS_DOUBLE, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	(*(*info).dispswitch).doublepos = event.SELECT
	WIDGET_CONTROL, (*(*info).ctrlsview).low_low_slider, $
    SENSITIVE = ((*(*info).dispswitch).doublepos AND $
      ((*(*info).dataparams).nlp GE 3))
	WIDGET_CONTROL, (*(*info).ctrlsview).upp_upp_slider, $
    SENSITIVE = ((*(*info).dispswitch).doublepos AND $
      ((*(*info).dataparams).nlp GE 3))
	TANAT_UPDATE_LP, event
	TANAT_DRAW, event
END

PRO TANAT_POS_MULT, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	(*(*info).dispswitch).multpos = event.SELECT
	IF ((*(*info).dataparams).low_low_val GE (*(*info).dataparams).upp_low_val) THEN (*(*info).dataparams).upp_low_val = (*(*info).dataparams).low_low_val + 1
	IF ((*(*info).dataparams).upp_upp_val LE (*(*info).dataparams).low_upp_val) THEN (*(*info).dataparams).low_upp_val = (*(*info).dataparams).upp_upp_val - 1
	WIDGET_CONTROL, (*(*info).ctrlsview).low_low_slider, SET_SLIDER_MAX = (*(*info).dataparams).nlp-4, SENSITIVE = (*(*info).dispswitch).multpos
	WIDGET_CONTROL, (*(*info).ctrlsview).low_upp_slider, SENSITIVE = (*(*info).dispswitch).multpos, SET_VALUE = (*(*info).dataparams).upp_low_val
	WIDGET_CONTROL, (*(*info).ctrlsview).upp_low_slider, SENSITIVE = (*(*info).dispswitch).multpos, SET_VALUE = (*(*info).dataparams).low_upp_val
	WIDGET_CONTROL, (*(*info).ctrlsview).upp_upp_slider, SET_SLIDER_MIN = 3, SENSITIVE = (*(*info).dispswitch).multpos
	TANAT_UPDATE_LP, event
	TANAT_DRAW, event
END

;================================================================================= BMP PROCEDURES
FUNCTION TANAT_READ_BMP, filename, srcdir
; Handles the reading of (button) BMP files
	bmp_dummy = READ_BMP(srcdir+filename)  
	bmp_dummy = TRANSPOSE(bmp_dummy, [1,2,0])
	RETURN, bmp_dummy
END

;================================================================================= RESET PROCEDURES
PRO TANAT_RESET_OUTPUTS, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	WIDGET_CONTROL, (*(*info).ctrlsmeas).delta_lx_label, SENSITIVE = 0
	WIDGET_CONTROL, (*(*info).ctrlsmeas).delta_t_label, SENSITIVE = 0
	WIDGET_CONTROL, (*(*info).ctrlsmeas).speed_label,  SENSITIVE = 0
	WIDGET_CONTROL, (*(*info).ctrlsmeas).delta_lx_text, SET_VALUE = '0', SENSITIVE = 0
	WIDGET_CONTROL, (*(*info).ctrlsmeas).delta_t_text, SET_VALUE = '0', SENSITIVE = 0
	WIDGET_CONTROL, (*(*info).ctrlsmeas).speed_text, SET_VALUE = '0', SENSITIVE = 0
	WIDGET_CONTROL, (*(*info).ctrlsmeas).lx_params_label, SENSITIVE = 0
	WIDGET_CONTROL, (*(*info).ctrlsmeas).lx_params_text, SET_VALUE = '(0,0)', SENSITIVE = 0
	WIDGET_CONTROL, (*(*info).ctrlsmeas).t_params_label, SENSITIVE = 0
	WIDGET_CONTROL, (*(*info).ctrlsmeas).t_params_text, SET_VALUE = '(0,0)', SENSITIVE = 0
	WIDGET_CONTROL, (*(*info).ctrlsmeas).flag_label, SENSITIVE = 0
	WIDGET_CONTROL, (*(*info).ctrlsmeas).flag_text, SET_VALUE = '0', SENSITIVE = 0
	WIDGET_CONTROL, (*(*info).ctrlsmeas).save_button, SENSITIVE = 0
	WIDGET_CONTROL, (*(*info).ctrlsmeas).acc_label, SENSITIVE = 0
	WIDGET_CONTROL, (*(*info).ctrlsmeas).acc_text, SET_VALUE = '0', SENSITIVE = 0
	WIDGET_CONTROL, (*(*info).ctrlsmeas).rem_but, SENSITIVE = 0
	(*(*info).measparams).flag = 0
	(*(*info).measparams).np = 0
	*(*(*info).measparams).lx_array = 0.
	*(*(*info).measparams).t_array = 0.
	*(*(*info).measparams).slx_array = 0.
	*(*(*info).measparams).st_array = 0.
END

;================================================================================= SAVE MEASUREMENT PROCEDURES
PRO TANAT_SAVE_MEASUREMENT, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	savefiles = FILE_SEARCH((*(*info).measparams).savefilename, COUNT = savefilecount)
	IF savefilecount THEN BEGIN
		nlines = FILE_LINES((*(*info).measparams).savefilename)
		datarr = STRARR(1,nlines)
		OPENR,unit1,(*(*info).measparams).savefilename,/GET_LUN
		READF,unit1,datarr
		FREE_LUN,unit1
		lastline_1 = STRSPLIT(datarr(0,nlines-1), ' ', /EXTRACT)
		lastline_2 = STRSPLIT(datarr(0,nlines-1), '	', /EXTRACT)
		IF (SIZE(lastline_1, /DIMENSIONS) GT 1) THEN lastline = lastline_1 ELSE IF (SIZE(lastline_2, /DIMENSIONS) GT 1) THEN lastline = lastline_2
		lst_id = lastline[2]
		lst_id_parts = STRSPLIT(lst_id,'.',/EXTRACT)
		IF ((*(*info).dataparams).filename EQ lastline[0]) THEN BEGIN
			new_id = '	'+STRING(FLOAT(lst_id)+0.001, FORMAT = '(3(F9.3,x))')
		ENDIF ELSE BEGIN
			prev_main_id = lst_id_parts[0]
			new_id = '	'+STRING(FLOAT(prev_main_id)+1, FORMAT = '(3(F9.3,x))')
		ENDELSE
		OPENU, unit2,(*(*info).measparams).savefilename, WIDTH = 360, /GET_LUN, /APPEND
		IF (*(*info).measparams).parabolic_fit THEN $
			PRINTF, unit2, (*(*info).dataparams).filename, FIX((*(*info).dataparams).lp), new_id, (*(*(*info).measparams).lx_array)[0], (*(*(*info).measparams).lx_array)[1], (*(*(*info).measparams).lx_array)[2], $
			(*(*(*info).measparams).t_array)[0], (*(*(*info).measparams).t_array)[1], (*(*(*info).measparams).t_array)[2], (*(*info).measparams).acceleration, (*(*info).measparams).flag $
		ELSE PRINTF, unit2, (*(*info).dataparams).filename, FIX((*(*info).dataparams).lp), new_id, (*(*info).curs).lxlock, (*(*info).dataparams).lx, (*(*info).measparams).act_delta_lx, (*(*info).curs).tlock, $
			(*(*info).dataparams).t, ABS((*(*info).measparams).delta_t), (*(*info).measparams).speed, (*(*info).measparams).flag
		FREE_LUN,unit2
	ENDIF ELSE BEGIN
		new_id = '	'+STRING(0.0, FORMAT = '(3(F9.3,x))')
		OPENW, unit3, (*(*info).measparams).savefilename, WIDTH = 360, /GET_LUN
		PRINTF, unit3, '#	Filename				lp	id	lx_start		lx_end	delta_lx	t_start	t_end	delta_t	'+$
				'v_abs [km/s]	flag'
		IF (*(*info).measparams).parabolic_fit THEN $
			PRINTF, unit3, (*(*info).dataparams).filename, FIX((*(*info).dataparams).lp), new_id, (*(*(*info).measparams).lx_array)[0], (*(*(*info).measparams).lx_array)[1], (*(*(*info).measparams).lx_array)[2], $
			(*(*(*info).measparams).t_array)[0], (*(*(*info).measparams).t_array)[1], (*(*(*info).measparams).t_array)[2], (*(*info).measparams).acceleration, (*(*info).measparams).flag $
		ELSE PRINTF, unit3, (*(*info).dataparams).filename, FIX((*(*info).dataparams).lp), new_id, (*(*info).curs).lxlock, (*(*info).dataparams).lx, (*(*info).measparams).act_delta_lx, (*(*info).curs).tlock, $
			(*(*info).dataparams).t, ABS((*(*info).measparams).delta_t), (*(*info).measparams).speed, (*(*info).measparams).flag
		FREE_LUN, unit3
	ENDELSE
	PRINT, 'Measurement saved to "'+STRTRIM((*(*info).measparams).savefilename,2)+'" with ID: '+STRTRIM(new_id,2)+', flagged '+STRTRIM((*(*info).measparams).flag,2)
	IF (*(*info).dispswitch).overlay_measurements THEN TANAT_DRAW_OVERLAY_SAVED_MEASUREMENTS, event
END

PRO TANAT_SAVE_MEASUREMENT_DEFINE_FLAG, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	WIDGET_CONTROL, (*(*info).ctrlsmeas).flag_text, GET_VALUE = textvalue
	(*(*info).measparams).flag = FIX(textvalue[0])
END

;==================== SCALING PROCEDURES
PRO TANAT_SCALING_SELECT_DATA, event
; Handles the selection of scaling 
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
  ;index = {0,1} -> Main, reference
  (*(*info).scaling).imrefscaling = event.INDEX
	TANAT_SCALING_SET_SLIDERS_BUTTONS, event
END

PRO TANAT_SCALING_SET_SLIDERS_BUTTONS, event
; Handles the selection of scaling 
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
  showarr = [1,(*(*info).dispswitch).ref]
  WIDGET_CONTROL, (*(*info).ctrlsview).scale_reset_button, $
    SENSITIVE=showarr[(*(*info).scaling).imrefscaling]
  WIDGET_CONTROL, (*(*info).ctrlsview).scalemin_slider,$
    SET_VALUE=(*(*info).scaling).minimum[(*(*info).scaling).imrefscaling], $
    SENSITIVE=showarr[(*(*info).scaling).imrefscaling]
  WIDGET_CONTROL, (*(*info).ctrlsview).scalemax_slider,$
    SET_VALUE=(*(*info).scaling).maximum[(*(*info).scaling).imrefscaling], $
    SENSITIVE=showarr[(*(*info).scaling).imrefscaling]
  gamma_val = (*(*info).scaling).gamma[(*(*info).scaling).imrefscaling]
  gamma_slider_val = 500 * (ALOG10(gamma_val) + 1)
  WIDGET_CONTROL, (*(*info).ctrlsview).gamma_slider, SET_VALUE=gamma_slider_val, $
    SENSITIVE=showarr[(*(*info).scaling).imrefscaling]
  WIDGET_CONTROL, (*(*info).ctrlsview).gamma_label, $
    SET_VALUE=STRING(gamma_val,FORMAT='(F6.3)')
END

PRO TANAT_SCALING_GAMMA_SLIDER, event
; Handles events from the minimum scaling value slider
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
  (*(*info).scaling).gamma[(*(*info).scaling).imrefscaling] = $
    10.^((FLOAT(event.VALUE)/500.) - 1.)
  WIDGET_CONTROL, (*(*info).ctrlsview).gamma_label, $
    SET_VALUE=STRING((*(*info).scaling).gamma[(*(*info).scaling).imrefscaling],$
    FORMAT='(F6.3)')
  TANAT_DRAW_SLICE, event, REFERENCE=(*(*info).scaling).imrefscaling
END

PRO TANAT_SCALING_SLIDER_MIN, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
  (*(*info).scaling).minimum[(*(*info).scaling).imrefscaling] = event.VALUE
  IF ((*(*info).scaling).minimum[(*(*info).scaling).imrefscaling] GE $
      (*(*info).scaling).maximum[(*(*info).scaling).imrefscaling]) THEN BEGIN
    (*(*info).scaling).maximum[(*(*info).scaling).imrefscaling] = $
      (*(*info).scaling).minimum[(*(*info).scaling).imrefscaling] + 1
    TANAT_SCALING_SET_SLIDERS_BUTTONS, event
  ENDIF
  TANAT_DRAW_SLICE, event, REFERENCE=(*(*info).scaling).imrefscaling
END

PRO TANAT_SCALING_SLIDER_MAX, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
  (*(*info).scaling).maximum[(*(*info).scaling).imrefscaling] = event.VALUE
  IF ((*(*info).scaling).maximum[(*(*info).scaling).imrefscaling] LE $
      (*(*info).scaling).minimum[(*(*info).scaling).imrefscaling]) THEN BEGIN
    (*(*info).scaling).minimum[(*(*info).scaling).imrefscaling] = $
      (*(*info).scaling).maximum[(*(*info).scaling).imrefscaling] - 1
    TANAT_SCALING_SET_SLIDERS_BUTTONS, event
  ENDIF
  TANAT_DRAW_SLICE, event, REFERENCE=(*(*info).scaling).imrefscaling
END

PRO TANAT_SCALING_RESET_DEFAULTS, event
; Handles events from the minimum scaling value slider
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
  ; Reset contrast value
  (*(*info).scaling).minimum[(*(*info).scaling).imrefscaling] = 0
  (*(*info).scaling).maximum[(*(*info).scaling).imrefscaling] = 100
  (*(*info).scaling).gamma[(*(*info).scaling).imrefscaling] = 1.
  TANAT_SCALING_SET_SLIDERS_BUTTONS, event
  TANAT_DRAW_SLICE, event, REFERENCE=(*(*info).scaling).imrefscaling
END 


;================================================================================= GET SERIES OF POINTS PROCEDURES
PRO TANAT_SERIES_PATH, event
; Gets the actual loop path from spline interpolation
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	np_local = (SIZE(*(*(*info).measparams).lx_array))[1] 
	IF (np_local GE 2) THEN BEGIN
		IF ((*(*(*info).measparams).lx_array)[np_local-1] EQ (*(*(*info).measparams).lx_array)[np_local-2]) AND ((*(*(*info).measparams).t_array)[np_local-1] EQ (*(*(*info).measparams).t_array)[np_local-2]) THEN RETURN
	ENDIF
	SPLINE_P,*(*(*info).measparams).slx_array,*(*(*info).measparams).st_array,xr,yr,INTERVAL=1
	*(*(*info).measparams).sxr = xr 
	*(*(*info).measparams).syr = yr
END

PRO TANAT_SERIES_FEEDBACK, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	(*(*info).dispswitch).series_feedback = event.SELECT
	TANAT_DRAW, event
END

PRO TANAT_SERIES_SAVE, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	monthstrarr = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
	curt = SYSTIME()
	tstr = STRSPLIT(curt,/EXTRACT)
	sstr = STRSPLIT(tstr[3],':',/EXTRACT)
	tstr[2] = STRING(tstr[2],FORMAT='(I02)')								; date
	date_order = [4,1,2] 											; defsaveid = 0 > YYYYMMMDD_hhmmss, ex: 2011Apr12_114545
	saveid = tstr[date_order[0]]+tstr[date_order[1]]+tstr[date_order[2]]+'_'+sstr[0]+sstr[1]+sstr[2]
	infilename = (STRSPLIT((*(*info).dataparams).filename,PATH_SEP(),/EXTRACT))[N_ELEMENTS(STRSPLIT((*(*info).dataparams).filename,PATH_SEP(),/EXTRACT))-1]
	basefstr = STRMID(infilename,0,STRPOS(infilename,'.',/REVERSE_SEARCH))
	savefilename = basefstr+'_ptseries_'+saveid+'.tsav'
	OPENW, unit, savefilename, WIDTH = 360, /GET_LUN
	PRINTF, unit, '#	np'
	PRINTF, unit, '#	x-coord		y-coord'
	PRINTF, unit, (*(*info).measparams).np
	FOR i=0,(*(*info).measparams).np-1 DO BEGIN
		PRINTF,unit,(*(*(*info).measparams).lx_array)[i],(*(*(*info).measparams).t_array)[i]
	ENDFOR
	FREE_LUN, unit
	PRINT,'Series of points saved to: '+savefilename
	WIDGET_CONTROL, (*(*info).ctrlsmeas).save_series_but, SENSITIVE = 0
END


PRO TANAT_SERIES_SET, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	(*(*info).measparams).series = event.SELECT
	WIDGET_CONTROL,(*(*info).ctrlsmeas).set_fit_but, SENSITIVE = ABS(event.SELECT-1)
	IF ((*(*info).curs).lockset GT 0) THEN TANAT_RESET_OUTPUTS, event
	IF ((*(*info).measparams).series EQ 0) THEN BEGIN
		(*(*info).measparams).np = 0
		*(*(*info).measparams).lx_array = 0.
		*(*(*info).measparams).t_array = 0.
		*(*(*info).measparams).slx_array = 0.
		*(*(*info).measparams).st_array = 0.
		*(*(*info).measparams).sxr = 0.
		*(*(*info).measparams).syr = 0.
		WIDGET_CONTROL, (*(*info).ctrlsmeas).rem_series_but, SENSITIVE = 0
		WIDGET_CONTROL, (*(*info).ctrlsmeas).save_series_but, SENSITIVE = 0
		(*(*info).curs).lockset = 0
		TANAT_DRAW, event
	ENDIF
END

;================================================================================= SET PROCEDURES
PRO TANAT_SET_ARCSEC_X, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	WIDGET_CONTROL, (*(*info).ctrlssetup).dx_text, GET_VALUE = textvalue
	(*(*info).measparams).arcsecpix[0] = FLOAT(textvalue[0])
	IF ((*(*info).curs).lockset GE 1) THEN TANAT_CALCULATE_DELTA, event
END

PRO TANAT_SET_ARCSEC_Y, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	WIDGET_CONTROL, (*(*info).ctrlssetup).dy_text, GET_VALUE = textvalue
	(*(*info).measparams).arcsecpix[1] = FLOAT(textvalue[0])
	IF ((*(*info).curs).lockset GE 1) THEN TANAT_CALCULATE_DELTA, event
END

PRO TANAT_SET_SECONDS, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	WIDGET_CONTROL, (*(*info).ctrlssetup).seconds_text, GET_VALUE = textvalue
	(*(*info).measparams).secondststep = FLOAT(textvalue[0])
	IF ((*(*info).curs).lockset GE 1) THEN TANAT_CALCULATE_DELTA, event
END

PRO TANAT_SET_SAVEFILENAME, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	WIDGET_CONTROL, (*(*info).ctrlssetup).savefile_text, GET_VALUE = textvalue
	(*(*info).measparams).savefilename = textvalue[0]
	savefiles = FILE_SEARCH((*(*info).measparams).savefilename, COUNT = savefilecount)
	compressedfilename = STRCOMPRESS((*(*info).measparams).savefilename, /REMOVE_ALL)
	IF (compressedfilename NE (*(*info).measparams).savefilename) OR ((*(*info).measparams).savefilename EQ '') THEN BEGIN 
		TANAT_WINDOW_OK, event, 'TANAT: ERROR!','Invalid filename. Please enter a filename of','at least one character and without any white spaces.', OK_EVENT='TANAT_CLOSE_EVENT_WINDOW', BASE=tlb
		(*(*info).winids).savnamenulltlb = tlb
	ENDIF ELSE IF savefilecount THEN BEGIN
		TANAT_WINDOW_OK, event,'TANAT: WARNING!', 'The file already exists. New measurements', 'will be added to the existing file.', OK_EVENT='TANAT_CLOSE_EVENT_WINDOW', BASE=tlb
		(*(*info).winids).savnamedoubletlb = tlb
	ENDIF
END

PRO TANAT_SET_CONTROLS, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	; First set consequences of slab_set = {0,1}
	IF (*(*info).dispswitch).slab_set THEN BEGIN
		(*(*info).dataparams).low_low_val = 0				&	(*(*info).dataparams).upp_low_val = 1
		(*(*info).dataparams).low_upp_val = (*(*info).dataparams).nlp-2	&	(*(*info).dataparams).upp_upp_val = (*(*info).dataparams).nlp-1
		upp_low_slider_min = 1						&	upp_upp_slider_min = 2
		lp_slider_max = (*(*info).dataparams).lp_last
	ENDIF ELSE BEGIN
		(*(*info).dataparams).low_low_val = 0				& 	(*(*info).dataparams).upp_low_val = 0
		(*(*info).dataparams).low_upp_val = 1				&	(*(*info).dataparams).upp_upp_val = 1
		upp_low_slider_min = 0						&	upp_upp_slider_min = 0
		lp_slider_max = (*(*info).dataparams).lp
	ENDELSE
	(*(*info).dispswitch).overlay_measurements = 0
	(*(*info).dispswitch).smoothed = 0
	; Set controls on tab 1 (Measurements)
	WIDGET_CONTROL, (*(*info).ctrlsmeas).lp_slider, SET_SLIDER_MIN = (*(*info).dataparams).lp_first, SET_SLIDER_MAX = lp_slider_max, SET_VALUE = (*(*info).dataparams).lp, SENSITIVE = (*(*info).dispswitch).slab_set
	WIDGET_CONTROL, (*(*info).ctrlsmeas).lp_speed_slider, SET_VALUE = 10, SENSITIVE = (*(*info).dispswitch).slab_set
	WIDGET_CONTROL, (*(*info).ctrlsmeas).lx_slider, SET_SLIDER_MIN = (*(*info).dataparams).lx_first, SET_SLIDER_MAX = (*(*info).dataparams).lx_last, SET_VALUE = (*(*info).dataparams).lx_first
  WIDGET_CONTROL, (*(*info).ctrlsmeas).lp_blink_slider, $
    SET_SLIDER_MIN=(*(*info).dataparams).lp_first+1, $
    SET_SLIDER_MAX=(*(*info).dataparams).lp_last, $
    SET_VALUE=(*(*info).dataparams).lp_first+1, $
    SENSITIVE=((*(*info).dispswitch).slab_set AND $
              ((*(*info).dataparams).nlp GE 3))
	WIDGET_CONTROL, (*(*info).ctrlsmeas).lp_blink_button, SET_BUTTON = 0, SENSITIVE = (*(*info).dispswitch).slab_set
	WIDGET_CONTROL, (*(*info).ctrlsmeas).t_slider, SET_SLIDER_MIN = (*(*info).dataparams).t_first, SET_SLIDER_MAX = (*(*info).dataparams).t_last, SET_VALUE = (*(*info).dataparams).t_first
	WIDGET_CONTROL, (*(*info).ctrlsmeas).overlay_button, SET_BUTTON = (*(*info).dispswitch).overlay_measurements
	; Set controls on tab 2 (View)
	WIDGET_CONTROL, (*(*info).ctrlsview).lower_t_text, SET_VALUE = STRTRIM((*(*info).dataparams).t_first,2)
	WIDGET_CONTROL, (*(*info).ctrlsview).upper_t_text, SET_VALUE = STRTRIM((*(*info).dataparams).t_last,2)
	WIDGET_CONTROL, (*(*info).ctrlsview).reset_trange_but, SENSITIVE = 0
  (*(*info).scaling).minimum[0] = 0
  (*(*info).scaling).maximum[0] = 100
  (*(*info).scaling).gamma[0] = 1
  TANAT_SCALING_SET_SLIDERS_BUTTONS, event
  ; Set controls on tab (Spectral)
	WIDGET_CONTROL, (*(*info).ctrlsview).single_pos, /SET_BUTTON, SENSITIVE = (*(*info).dispswitch).slab_set
	WIDGET_CONTROL, (*(*info).ctrlsview).double_pos, SENSITIVE = (*(*info).dispswitch).slab_set
	WIDGET_CONTROL, (*(*info).ctrlsview).mult_pos, $
    SENSITIVE=((*(*info).dispswitch).slab_set AND $
                ((*(*info).dataparams).nlp GE 3))
	(*(*info).dispswitch).singlepos = 1
	(*(*info).dispswitch).doublepos = 0
	(*(*info).dispswitch).multpos = 0
	WIDGET_CONTROL, (*(*info).ctrlsview).low_low_slider, SET_SLIDER_MIN = (*(*info).dataparams).low_low_val, SET_SLIDER_MAX = (*(*info).dataparams).nlp-2, SET_VALUE = (*(*info).dataparams).low_low_val, SENSITIVE = 0
	WIDGET_CONTROL, (*(*info).ctrlsview).low_upp_slider, SET_SLIDER_MIN = (*(*info).dataparams).upp_low_val, SET_SLIDER_MAX = (*(*info).dataparams).nlp-3, SET_VALUE = (*(*info).dataparams).upp_low_val, SENSITIVE = 0
	WIDGET_CONTROL, (*(*info).ctrlsview).upp_low_slider, SET_SLIDER_MIN = upp_low_slider_min, SET_SLIDER_MAX = (*(*info).dataparams).low_upp_val, SET_VALUE = (*(*info).dataparams).low_upp_val, SENSITIVE = 0
	WIDGET_CONTROL, (*(*info).ctrlsview).upp_upp_slider, SET_SLIDER_MIN = upp_upp_slider_min, SET_SLIDER_MAX = (*(*info).dataparams).upp_upp_val, SET_VALUE = (*(*info).dataparams).upp_upp_val, SENSITIVE = 0
	WIDGET_CONTROL, (*(*info).ctrlsview).non_smooth_button, SET_BUTTON = ABS((*(*info).dispswitch).smoothed-1)
	WIDGET_CONTROL, (*(*info).ctrlsview).smooth_button, SET_BUTTON = (*(*info).dispswitch).smoothed
	; Set general controls
	WIDGET_CONTROL, (*(*info).ctrlsgen).subtract_but, SET_BUTTON = 0, SENSITIVE = (*(*info).dispswitch).slab_set
	WIDGET_CONTROL, (*(*info).ctrlsgen).lower_y_label, SENSITIVE = (*(*info).dispswitch).slab_set
	WIDGET_CONTROL, (*(*info).ctrlsgen).lower_y_text, SET_VALUE = '0', SENSITIVE = (*(*info).dispswitch).slab_set
	WIDGET_CONTROL, (*(*info).ctrlsgen).upper_y_label, SENSITIVE = (*(*info).dispswitch).slab_set
	WIDGET_CONTROL, (*(*info).ctrlsgen).upper_y_text, SET_VALUE = '1', SENSITIVE = (*(*info).dispswitch).slab_set
	WIDGET_CONTROL, (*(*info).ctrlsgen).det_spect, SENSITIVE = (*(*info).dispswitch).slab_set
  WIDGET_CONTROL, (*(*info).ctrlsgen).filetext, $
    SET_VALUE=FILE_BASENAME((*(*info).dataparams).filename)
	; Set to tab 3
	WIDGET_CONTROL, (*(*info).winids).tab_tlb, SET_TAB_CURRENT = 3
END

PRO TANAT_SET_SPECTPARAMS, spectrum, nlp, spect_pos, LINE_CENTER=line_center, $
  LPS=lps, LC=lc, SPXTITLE=spxtitle, V_DOP_VALS=v_dop_vals, V_DOP_SET=v_dop_set
  lps = 0
	spxtitle = ''
	v_dop_vals = 0
	v_dop_set = 0
	IF (nlp GT 1) THEN BEGIN
		c_speed	= 2.99792458D5						; Speed of light indeed in km/s
		lps	= FINDGEN(nlp)
		IF (N_ELEMENTS(LINE_CENTER) EQ 0) THEN BEGIN			; If the LINE_CENTER keyword is not set:
			lc	= ( WHERE( spectrum EQ MIN(spectrum) ) )[0]	; autodetermine linecentre value from spectrum
			lps	= ( lps - lps[FIX(lc)] ) 				; reset scale to have lps=0 at lc
			spxtitle= 'Spectral position'
		ENDIF ELSE IF (N_ELEMENTS(LINE_CENTER) EQ 1) THEN BEGIN		; else, if the position is supplied
			lc 	= line_center					; use that given value
			IF (lc GE nlp) OR (lc LT 0) THEN BEGIN			; Check whether 0 LE lc LT nlp
				PRINT,'ERROR: Linecentre index value '+STRTRIM(FIX(lc),2)+' falls outside of allowed range [0,'+STRTRIM(nlp-1,2)+']!'
				RETURN
			ENDIF
			lps	= ( lps - lps[FIX(lc)] ) 				; reset scale to have lps=0 at lc
			spxtitle= 'Spectral position'
		ENDIF ELSE IF (N_ELEMENTS(LINE_CENTER) EQ 2) THEN BEGIN		; else, if also the wavelength is supplied
			lc	 = ( WHERE( spectrum EQ MIN(spectrum) ) )[0]	; autodetermine linecentre value from spectrum
			lambda_c= line_center[0]				; get the linecentre wavelength
			dlambda	= line_center[1]				; and get delta lambda per lineposition
			lps	= ( lps - lps[FIX(lc)] ) 				; reset scale to have lps=0 at lc
			lps	= lps*dlambda + lambda_c				; reset scale to wavelength scale
			spxtitle= 'Wavelength'					; and adapt the xtitle accordingly
			v_dop_set = 1
		ENDIF ELSE BEGIN						; else, if linecentre and wavelength supplied
			lc 	= line_center[0]				; get the linecentre position
			IF (lc GE nlp) OR (lc LT 0) THEN BEGIN			; Check whether 0 LE lc LT nlp
				PRINT,'ERROR: Linecentre index value '+STRTRIM(FIX(lc),2)+' falls outside of allowed range [0,'+STRTRIM(nlp-1,2)+']!'
				RETURN
			ENDIF
			lambda_c= line_center[1]				; and the linecentre wavelength
			dlambda	= line_center[2]				; and get delta(wavelength) per lineposition
			lps	= ( lps - lps[FIX(lc)] ) 				; reset scale to have lps=0 at lc
			lps	= lps*dlambda + lambda_c				; reset scale to wavelength scale
			spxtitle= 'Wavelength'					; and adapt the xtitle accordingly
			v_dop_set = 1
		ENDELSE
	
		IF v_dop_set THEN v_dop_vals = c_speed*(lps/lps[FIX(lc)]-1)		; array with Doppler velocities in km/s
	ENDIF ELSE $
    ; If nlp=1 then set parameters accordingly
		lc = 0
END

PRO TANAT_SET_TIMESLICE_PARAMS, nlx, nt, x_loop_pts, y_loop_pts, arcsecpix, $
  LXDIST=lxdist, CUMUL_LXDIST=cumul_lxdist, LX_FIRST=lx_first, $
  LX_LAST=lx_last, T_FIRST=t_first, T_LAST=t_last, NLXPTS=nlxpts
  nlxpts = N_ELEMENTS(x_loop_pts)
	lxdist = DBLARR(nlxpts)
	cumul_lxdist = DBLARR(nlxpts)
	IF (TOTAL(x_loop_pts) EQ 0) AND (TOTAL(y_loop_pts) EQ 0) THEN BEGIN
    nlxpts = nlx
    lxdist = REPLICATE(1.,nlxpts) 
  ENDIF ELSE BEGIN
		FOR i=0,nlxpts-2 DO BEGIN
			x_up = x_loop_pts[i+1]
			x_dn = x_loop_pts[i]
			y_up = y_loop_pts[i+1]
			y_dn = y_loop_pts[i]
			lxdist[i] = SQRT( $
        ((x_up - x_dn)*arcsecpix[0])^2 + $
        ((y_up - y_dn)*arcsecpix[1])^2 )
      IF (i EQ 0) THEN $
        cumul_lxdist[i] = lxdist[i] $
      ELSE $
        cumul_lxdist[i] = lxdist[i] + cumul_lxdist[i-1]
		ENDFOR
		lxdist[nlxpts-1] = MEAN(lxdist[0:nlxpts-2])
    cumul_lxdist[nlxpts-1] = lxdist[nlxpts-1] + cumul_lxdist[nlxpts-2]
	ENDELSE
	lx_first	= 0						; Set number of first x-coordinate
	lx_last		= nlxpts-1						; Set number of last x-coordinate
	t_first		= 0						; Set number of first frame		
	t_last		= nt-1						; Set number of last frame
END

;==================== SLIDER PROCEDURES
PRO TANAT_SLIDER_LOW_LOW, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	(*(*info).dataparams).low_low_val = event.VALUE
	IF (*(*info).dispswitch).doublepos THEN BEGIN
		IF ((*(*info).dataparams).low_low_val GE (*(*info).dataparams).upp_upp_val) THEN (*(*info).dataparams).upp_upp_val = (*(*info).dataparams).low_low_val + 1
		WIDGET_CONTROL, (*(*info).ctrlsview).upp_upp_slider, SET_VALUE = (*(*info).dataparams).upp_upp_val
	ENDIF ELSE IF (*(*info).dispswitch).multpos THEN BEGIN
		IF ((*(*info).dataparams).low_low_val GE (*(*info).dataparams).upp_low_val) THEN (*(*info).dataparams).upp_low_val = (*(*info).dataparams).low_low_val + 1
		IF ((*(*info).dataparams).upp_low_val GT (*(*info).dataparams).low_upp_val) THEN (*(*info).dataparams).low_upp_val = (*(*info).dataparams).low_upp_val + 1
		IF ((*(*info).dataparams).low_upp_val GE (*(*info).dataparams).upp_upp_val) THEN (*(*info).dataparams).upp_upp_val = (*(*info).dataparams).low_upp_val + 1
		WIDGET_CONTROL, (*(*info).ctrlsview).low_upp_slider, SET_VALUE = (*(*info).dataparams).upp_low_val
		WIDGET_CONTROL, (*(*info).ctrlsview).upp_low_slider, SET_VALUE = (*(*info).dataparams).low_upp_val
		WIDGET_CONTROL, (*(*info).ctrlsview).upp_upp_slider, SET_VALUE = (*(*info).dataparams).upp_upp_val
	ENDIF
	TANAT_UPDATE_LP, event
	TANAT_DRAW, event
END

PRO TANAT_SLIDER_LOW_UPP, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	(*(*info).dataparams).upp_low_val = event.VALUE
	IF ((*(*info).dataparams).upp_low_val GT (*(*info).dataparams).low_upp_val) THEN (*(*info).dataparams).low_upp_val = (*(*info).dataparams).low_upp_val + 1
	IF ((*(*info).dataparams).low_upp_val GE (*(*info).dataparams).upp_upp_val) THEN (*(*info).dataparams).upp_upp_val = (*(*info).dataparams).low_upp_val + 1
	IF ((*(*info).dataparams).upp_low_val LE (*(*info).dataparams).low_low_val) THEN (*(*info).dataparams).low_low_val = (*(*info).dataparams).upp_low_val - 1
	WIDGET_CONTROL, (*(*info).ctrlsview).low_low_slider, SET_VALUE = (*(*info).dataparams).low_low_val
	WIDGET_CONTROL, (*(*info).ctrlsview).upp_upp_slider, SET_VALUE = (*(*info).dataparams).upp_upp_val
	WIDGET_CONTROL, (*(*info).ctrlsview).upp_low_slider, SET_VALUE = (*(*info).dataparams).low_upp_val
	TANAT_UPDATE_LP, event
	TANAT_DRAW, event
END

PRO TANAT_SLIDER_REFLP, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	(*(*info).dataparams).reflp = event.VALUE
	TANAT_UPDATE_LP, event
	TANAT_DRAW, event
END

PRO TANAT_SLIDER_REFLP_LOCK, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	(*(*info).dispswitch).reflp_lock = event.SELECT
	(*(*info).dataparams).reflp = (*(*info).dataparams).lp
	WIDGET_CONTROL, (*(*info).ctrlsmeas).reflp_slider, SENSITIVE = ABS((*(*info).dispswitch).reflp_lock-1), SET_VALUE = (*(*info).dataparams).reflp
	TANAT_UPDATE_LP, event
	TANAT_DRAW, event
END

PRO TANAT_SLIDER_LP, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	(*(*info).dataparams).lp = event.VALUE
	IF (*(*info).dispswitch).reflp_lock THEN BEGIN
		(*(*info).dataparams).reflp = (*(*info).dataparams).lp
		WIDGET_CONTROL, (*(*info).ctrlsmeas).reflp_slider, SET_VALUE = (*(*info).dataparams).reflp
	ENDIF
	TANAT_UPDATE_LP, event
	TANAT_DRAW, event
END

PRO TANAT_SLIDER_LP_INCR, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	(*(*info).dataparams).lp += (*(*info).pbparams).lp_step
	IF ((*(*info).dataparams).lp GT (*(*info).dataparams).lp_last) THEN (*(*info).dataparams).lp = (*(*info).dataparams).lp_first
	WIDGET_CONTROL,(*(*info).ctrlsmeas).lp_slider, SET_VALUE = (*(*info).dataparams).lp
	TANAT_UPDATE_LP, event
	TANAT_DRAW, event
END

PRO TANAT_SLIDER_LP_DECR, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	(*(*info).dataparams).lp -= (*(*info).pbparams).lp_step
	IF ((*(*info).dataparams).lp LT (*(*info).dataparams).lp_first) THEN (*(*info).dataparams).lp = (*(*info).dataparams).lp_last
	WIDGET_CONTROL,(*(*info).ctrlsmeas).lp_slider, SET_VALUE = (*(*info).dataparams).lp
	TANAT_UPDATE_LP, event
	TANAT_DRAW, event
END

PRO TANAT_SLIDER_LX, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	(*(*info).dataparams).lx = event.VALUE
  IF (*(*info).dispswitch).gap_consider THEN BEGIN
    lowsub = INDGEN((*(*info).dataparams).ngaps)*2
    uppsub = lowsub+1
    (*(*info).dispswitch).curs_out_of_range = $
      ABS( TOTAL(((*(*info).dataparams).lx GE $
                (*(*(*info).dataparams).databounds)[lowsub]) AND $
            ((*(*info).dataparams).lx LE $
             (*(*(*info).dataparams).databounds)[uppsub]))-1) 
  ENDIF
  slxt = TANAT_TRANSFORM_DATA2DEVICE(info, LX_IN=(*(*info).dataparams).lx, $
    /MAIN)
  (*(*info).curs).slx = slxt.lx
	TANAT_DRAW, event
END

PRO TANAT_SLIDER_SPECTSTEP, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	(*(*info).pbparams).lp_step = event.VALUE
END

PRO TANAT_SLIDER_SPEED, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	(*(*info).pbparams).lp_speed = event.VALUE
END

PRO TANAT_SLIDER_T, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	(*(*info).dataparams).t = event.VALUE
  slxt = TANAT_TRANSFORM_DATA2DEVICE(info, T_IN=(*(*info).dataparams).t, $
    /MAIN)
  (*(*info).curs).st = slxt.t
	TANAT_DRAW, event
END

PRO TANAT_SLIDER_W, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	(*(*info).dataparams).w = event.VALUE
	TANAT_UPDATE_LP, event
	TANAT_DRAW, event
END

PRO TANAT_SLIDER_UPP_LOW, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	(*(*info).dataparams).low_upp_val = event.VALUE
	IF ((*(*info).dataparams).low_upp_val LT (*(*info).dataparams).upp_low_val) THEN (*(*info).dataparams).upp_low_val = (*(*info).dataparams).upp_low_val - 1
	IF ((*(*info).dataparams).upp_low_val LE (*(*info).dataparams).low_low_val) THEN (*(*info).dataparams).low_low_val = (*(*info).dataparams).upp_low_val - 1
	IF ((*(*info).dataparams).low_upp_val GE (*(*info).dataparams).upp_upp_val) THEN (*(*info).dataparams).upp_upp_val = (*(*info).dataparams).low_upp_val + 1
	WIDGET_CONTROL, (*(*info).ctrlsview).low_upp_slider, SET_VALUE = (*(*info).dataparams).upp_low_val
	WIDGET_CONTROL, (*(*info).ctrlsview).low_low_slider, SET_VALUE = (*(*info).dataparams).low_low_val
	WIDGET_CONTROL, (*(*info).ctrlsview).upp_upp_slider, SET_VALUE = (*(*info).dataparams).upp_upp_val
	TANAT_UPDATE_LP, event
	TANAT_DRAW, event
END

PRO TANAT_SLIDER_UPP_UPP, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	(*(*info).dataparams).upp_upp_val = event.VALUE
	IF (*(*info).dispswitch).doublepos THEN BEGIN
		IF ((*(*info).dataparams).upp_upp_val LE (*(*info).dataparams).low_low_val) THEN (*(*info).dataparams).low_low_val = (*(*info).dataparams).upp_upp_val - 1
		WIDGET_CONTROL, (*(*info).ctrlsview).low_low_slider, SET_VALUE = (*(*info).dataparams).low_low_val
	ENDIF ELSE IF (*(*info).dispswitch).multpos THEN BEGIN
		IF ((*(*info).dataparams).upp_upp_val LE (*(*info).dataparams).low_upp_val) THEN (*(*info).dataparams).low_upp_val = (*(*info).dataparams).upp_upp_val - 1
		IF ((*(*info).dataparams).low_upp_val LT (*(*info).dataparams).upp_low_val) THEN (*(*info).dataparams).upp_low_val = (*(*info).dataparams).upp_low_val - 1
		IF ((*(*info).dataparams).upp_low_val LE (*(*info).dataparams).low_low_val) THEN (*(*info).dataparams).low_low_val = (*(*info).dataparams).upp_low_val - 1
		WIDGET_CONTROL, (*(*info).ctrlsview).upp_low_slider, SET_VALUE = (*(*info).dataparams).low_upp_val
		WIDGET_CONTROL, (*(*info).ctrlsview).low_upp_slider, SET_VALUE = (*(*info).dataparams).upp_low_val
		WIDGET_CONTROL, (*(*info).ctrlsview).low_low_slider, SET_VALUE = (*(*info).dataparams).low_low_val
	ENDIF
	TANAT_UPDATE_LP, event
	TANAT_DRAW, event
END
		
;==================== DISPLAY PROCEDURES
PRO TANAT_SMOOTH_VIEW, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	(*(*info).dispswitch).smoothed = event.SELECT
	TANAT_DRAW, event
END

;==================== DISPLAY T PROCEDURES
PRO TANAT_T_LOW, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	WIDGET_CONTROL, (*(*info).ctrlsview).lower_t_text, GET_VALUE = textvalue
	(*(*info).dataparams).t_low = FLOAT(textvalue[0])
	IF ((*(*info).dataparams).t_low GE (*(*info).dataparams).t_upp) THEN BEGIN
		(*(*info).dataparams).t_low = (*(*info).dataparams).t_upp - 1
		WIDGET_CONTROL, (*(*info).ctrlsview).lower_t_text, SET_VALUE = STRTRIM((*(*info).dataparams).t_low,2)
	ENDIF
	IF ((*(*info).dataparams).t_low LT (*(*info).dataparams).t_first) THEN BEGIN
		(*(*info).dataparams).t_low = (*(*info).dataparams).t_first
		WIDGET_CONTROL, (*(*info).ctrlsview).lower_t_text, SET_VALUE = STRTRIM((*(*info).dataparams).t_low,2)
	ENDIF
	TANAT_T_RANGE, event
END

PRO TANAT_T_UPP, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	WIDGET_CONTROL, (*(*info).ctrlsview).upper_t_text, GET_VALUE = textvalue
	(*(*info).dataparams).t_upp = FLOAT(textvalue[0])
	IF ((*(*info).dataparams).t_upp LE (*(*info).dataparams).t_low) THEN BEGIN
		(*(*info).dataparams).t_upp = (*(*info).dataparams).t_low + 1
		WIDGET_CONTROL, (*(*info).ctrlsview).upper_t_text, SET_VALUE = STRTRIM((*(*info).dataparams).t_upp,2)
	ENDIF
	IF ((*(*info).dataparams).t_upp GT (*(*info).dataparams).t_last) THEN BEGIN
		(*(*info).dataparams).t_upp = (*(*info).dataparams).t_last
		WIDGET_CONTROL, (*(*info).ctrlsview).upper_t_text, SET_VALUE = STRTRIM((*(*info).dataparams).t_upp,2)
	ENDIF
	TANAT_T_RANGE, event
END

PRO TANAT_T_RANGE, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	(*(*info).dataparams).t_range = $
    (*(*info).dataparams).t_upp - (*(*info).dataparams).t_low
  WIDGET_CONTROL, (*(*info).ctrlsview).reset_trange_but, $
    SENSITIVE=((*(*info).dataparams).t_range+1 NE (*(*info).dataparams).nt)
	WIDGET_CONTROL, (*(*info).ctrlsmeas).t_slider, $
    SET_SLIDER_MIN = (*(*info).dataparams).t_low
	WIDGET_CONTROL, (*(*info).ctrlsmeas).t_slider, $
    SET_SLIDER_MAX = (*(*info).dataparams).t_upp
	(*(*info).dataparams).d_nt = (*(*info).dataparams).t_range+1
  slxt = TANAT_TRANSFORM_DATA2DEVICE(info, $
    T_IN=[(*(*info).dataparams).t, (*(*info).curs).tlock], $
    /MAIN)
  (*(*info).curs).st = slxt.t[0]
  (*(*info).curs).stlock = slxt.t[1]
	IF (((*(*info).measparams).parabolic_fit OR $
      (*(*info).measparams).series) AND $
      ((*(*info).measparams).np GE 1)) THEN BEGIN
    slxtarray = TANAT_TRANSFORM_DATA2DEVICE(info, $
      T_IN=*(*(*info).measparams).t_array, /MAIN)
    *(*(*info).measparams).st_array = slxtarray.t
  ENDIF
	TANAT_UPDATE_LP, event
	IF ((*(*info).curs).lockset GE 1) THEN TANAT_CALCULATE_DELTA, event
	TANAT_DRAW, event
END

PRO TANAT_T_RANGE_RESET, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	(*(*info).dataparams).t_upp = (*(*info).dataparams).t_last
	(*(*info).dataparams).t_low = (*(*info).dataparams).t_first
	WIDGET_CONTROL, (*(*info).ctrlsview).upper_t_text, SET_VALUE = STRTRIM((*(*info).dataparams).t_upp,2)
	WIDGET_CONTROL, (*(*info).ctrlsview).lower_t_text, SET_VALUE = STRTRIM((*(*info).dataparams).t_low,2)
	WIDGET_CONTROL, (*(*info).ctrlsview).reset_trange_but, SENSITIVE = 0
	TANAT_T_RANGE, event
END

;================================================================================= UPDATE PROCEDURES
PRO TANAT_UPDATE_LP, event
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	IF (*(*info).dispswitch).singlepos THEN BEGIN
		IF (*(*info).dispswitch).slab_set THEN BEGIN
			IF (*(*info).dataparams).w_set THEN *(*(*info).data).loopslice = REFORM((*(*(*info).data).loopdata)[*,(*(*info).dataparams).t_low:(*(*info).dataparams).t_upp,$
				(*(*info).dataparams).lp-(*(*info).dataparams).lp_first, (*(*info).dataparams).w]) ELSE $
				*(*(*info).data).loopslice = REFORM((*(*(*info).data).loopdata)[*,(*(*info).dataparams).t_low:(*(*info).dataparams).t_upp,(*(*info).dataparams).lp-(*(*info).dataparams).lp_first])
		ENDIF ELSE BEGIN
			IF (*(*info).dataparams).w_set THEN *(*(*info).data).loopslice = (*(*(*info).data).loopdata)[*,(*(*info).dataparams).t_low:(*(*info).dataparams).t_upp,(*(*info).dataparams).w] ELSE $
				*(*(*info).data).loopslice = (*(*(*info).data).loopdata)[*,(*(*info).dataparams).t_low:(*(*info).dataparams).t_upp]
		ENDELSE
		IF (*(*info).dispswitch).ref THEN BEGIN
			IF (*(*info).dispswitch).ref_slab_set THEN $
				*(*(*info).data).refloopslice = REFORM((*(*(*info).data).refloopdata)[*,(*(*info).dataparams).t_low:(*(*info).dataparams).t_upp,(*(*info).dataparams).reflp-(*(*info).dataparams).reflp_first]) ELSE $
				*(*(*info).data).refloopslice = (*(*(*info).data).refloopdata)[*,(*(*info).dataparams).t_low:(*(*info).dataparams).t_upp]
		ENDIF
	ENDIF ELSE IF (*(*info).dispswitch).doublepos THEN BEGIN
		IF (*(*info).dispswitch).slab_set THEN BEGIN
			IF (*(*info).dataparams).w_set THEN BEGIN
				loopslice_1 = ((*(*(*info).data).loopdata)[*,(*(*info).dataparams).t_low:(*(*info).dataparams).t_upp,(*(*info).dataparams).low_low_val-(*(*info).dataparams).lp_first,(*(*info).dataparams).w]) 
				loopslice_2 = ((*(*(*info).data).loopdata)[*,(*(*info).dataparams).t_low:(*(*info).dataparams).t_upp,(*(*info).dataparams).upp_upp_val-(*(*info).dataparams).lp_first,(*(*info).dataparams).w])
			ENDIF ELSE BEGIN
				loopslice_1 = ((*(*(*info).data).loopdata)[*,(*(*info).dataparams).t_low:(*(*info).dataparams).t_upp,(*(*info).dataparams).low_low_val-(*(*info).dataparams).lp_first]) 
				loopslice_2 = ((*(*(*info).data).loopdata)[*,(*(*info).dataparams).t_low:(*(*info).dataparams).t_upp,(*(*info).dataparams).upp_upp_val-(*(*info).dataparams).lp_first])
			ENDELSE
			*(*(*info).data).loopslice = (loopslice_1 + loopslice_2)/2.
		ENDIF
	ENDIF ELSE IF (*(*info).dispswitch).multpos THEN BEGIN
		IF (*(*info).dataparams).w_set THEN BEGIN
			loopslice_1 = TOTAL(((*(*(*info).data).loopdata)[*,(*(*info).dataparams).t_low:(*(*info).dataparams).t_upp,((*(*info).dataparams).low_low_val-$
				(*(*info).dataparams).lp_first):((*(*info).dataparams).upp_low_val-(*(*info).dataparams).lp_first),(*(*info).dataparams).w]),3) / $
				((*(*info).dataparams).upp_low_val - (*(*info).dataparams).low_low_val + 1)
			loopslice_2 = TOTAL(((*(*(*info).data).loopdata)[*,(*(*info).dataparams).t_low:(*(*info).dataparams).t_upp,((*(*info).dataparams).low_upp_val-$
				(*(*info).dataparams).lp_first):((*(*info).dataparams).upp_upp_val-(*(*info).dataparams).lp_first),(*(*info).dataparams).w]),3) / $
				((*(*info).dataparams).upp_upp_val - (*(*info).dataparams).low_upp_val + 1)
		ENDIF ELSE BEGIN
			loopslice_1 = TOTAL(((*(*(*info).data).loopdata)[*,(*(*info).dataparams).t_low:(*(*info).dataparams).t_upp,((*(*info).dataparams).low_low_val-$
				(*(*info).dataparams).lp_first):((*(*info).dataparams).upp_low_val-(*(*info).dataparams).lp_first)]),3) / ((*(*info).dataparams).upp_low_val - (*(*info).dataparams).low_low_val + 1)
			loopslice_2 = TOTAL(((*(*(*info).data).loopdata)[*,(*(*info).dataparams).t_low:(*(*info).dataparams).t_upp,((*(*info).dataparams).low_upp_val-$
				(*(*info).dataparams).lp_first):((*(*info).dataparams).upp_upp_val-(*(*info).dataparams).lp_first)]),3) / ((*(*info).dataparams).upp_upp_val - (*(*info).dataparams).low_upp_val + 1)
		ENDELSE
		*(*(*info).data).loopslice = (loopslice_1 + loopslice_2)/2.
	ENDIF
;	TANAT_SCALING_RANGE, event
END

PRO TANAT_UPDATE_STARTUP_FEEDBACK, bgim, xout, yout, feedback_text
	LOADCT,3,/SILENT
	TVSCL,bgim
	LOADCT,0,/SILENT
	FOR i=0,N_ELEMENTS(feedback_text)-1 DO XYOUTS, xout[i], yout[i], feedback_text[i], COLOR=255, /DEVICE, CHARSIZE=1.125
END

;================================================================================= VERBOSE PROCEDURES
PRO TANAT_VERBOSE_GET, event, variables, labels=labels
; Displays feedback of variables structuredly 
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	nvars = N_ELEMENTS(variables)
	IF ((*(*info).feedbparams).last_routine_count NE 0) THEN PRINT,''
	IF (N_ELEMENTS(labels) EQ nvars) THEN BEGIN
		maxchar = MAX(STRLEN(labels))
		FOR i=0,nvars-1 DO BEGIN
			IF (STRLEN(labels[i]) NE maxchar) THEN whitespace = STRJOIN(REPLICATE(' ',(maxchar-STRLEN(labels[i])))) ELSE whitespace = ''
			PRINT,STRJOIN(REPLICATE('  ',SCOPE_LEVEL()-2))+'> '+labels[i]+whitespace+': '+STRTRIM(variables[i],2)
		ENDFOR
	ENDIF ELSE BEGIN
		FOR i=0,nvars-1 DO HELP, variables[i]
	ENDELSE
END

PRO TANAT_VERBOSE_GET_ROUTINE, event, rname, IGNORE_LAST=ignore_last
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	prespace = STRJOIN(REPLICATE('  ',SCOPE_LEVEL()-2))
  rname = (SCOPE_TRACEBACK(/STRUCTURE))[N_ELEMENTS(SCOPE_TRACEBACK(/STRUCTURE))-2].ROUTINE
	IF KEYWORD_SET(IGNORE_LAST) THEN (*(*info).feedbparams).last_routine = ''
	IF ((rname NE (*(*info).feedbparams).last_routine) AND $
    ((*(*info).feedbparams).last_routine_count GT 0)) THEN PRINT,''
	IF (rname EQ (*(*info).feedbparams).last_routine) THEN $
    (*(*info).feedbparams).last_routine_count += 1 $
  ELSE $
    (*(*info).feedbparams).last_routine_count = 0
	IF ((*(*info).feedbparams).last_routine_count GT 0) THEN $
    rcount = ' x '+STRTRIM((*(*info).feedbparams).last_routine_count,2)+'.' $
  ELSE $
    rcount = '.'
	IF (rname NE (*(*info).feedbparams).last_routine) THEN $
    PRINT,prespace+'TANAT RUN: Called '+rname+'.' $
  ELSE $
		WRITEU,-1,STRING(FORMAT='(%"\r'+prespace+'TANAT RUN: Called ",a'+$
      STRTRIM(STRLEN(rname),2)+',a'+STRTRIM(STRLEN(rcount),2)+')',rname,rcount) 
	(*(*info).feedbparams).last_routine = rname
END
;================================================================================= WINDOW PROCEDURES
PRO TANAT_WINDOW, xsize, ysize, leader, title, base, wid, xoffset, yoffset, DRAWID = drawid, DRAWBASE =disp, XSCROLL = xscrollsize, YSCROLL = yscrollsize, SCROLL = scroll
	base = WIDGET_BASE(TITLE = STRTRIM(title), GROUP_LEADER = leader, TLB_FRAME_ATTR = 1, /TLB_KILL_REQUEST_EVENTS)
	disp = WIDGET_BASE(base, /COLUMN)
	drawid = WIDGET_DRAW(disp, XSIZE = xsize, YSIZE = ysize, RETAIN = 2)
	WIDGET_CONTROL, base, /REALIZE, TLB_SET_XOFFSET=xoffset, TLB_SET_YOFFSET=yoffset
	WIDGET_CONTROL, drawid, GET_VALUE = wid
END

PRO TANAT_WINDOW_OK, event, title, message1, message2, message3, message4, OK_EVENT=ok_event, BASE=base, BLOCK=block
	WIDGET_CONTROL, event.TOP, GET_UVALUE = info
	IF (TOTAL(((*(*info).feedbparams).verbosity)[2:3]) GE 1) THEN $
    TANAT_VERBOSE_GET_ROUTINE, event
	base = WIDGET_BASE(TITLE = title, GROUP_LEADER = (*(*info).winids).root, TLB_FRAME_ATTR = 1, /TLB_KILL_REQUEST_EVENTS)
	disp = WIDGET_BASE(base, /COLUMN)
	message_base = WIDGET_BASE(disp, /COLUMN)
	text_label1 = WIDGET_LABEL(message_base, VALUE = message1)
	IF (N_ELEMENTS(message2) GT 0) THEN text_label2 = WIDGET_LABEL(message_base, VALUE = message2)
	IF (N_ELEMENTS(message3) GT 0) THEN text_label3 = WIDGET_LABEL(message_base, VALUE = message3)
	IF (N_ELEMENTS(message4) GT 0) THEN text_label4 = WIDGET_LABEL(message_base, VALUE = message4)
	button_base = WIDGET_BASE(disp,/ROW,/ALIGN_CENTER)
	ok_but = WIDGET_BUTTON(button_base, VALUE = 'OK' , EVENT_PRO = ok_event)
	WIDGET_CONTROL, base, /REALIZE, TLB_SET_XOFFSET = 500, TLB_SET_YOFFSET = 500
	WIDGET_CONTROL, base, SET_UVALUE = info
	IF (N_ELEMENTS(BLOCK) NE 1) THEN block = 0
	XMANAGER, 'TANAT', base, NO_BLOCK=ABS(block-1)
END

;===============================================================================
;================================== MAIN PROGRAM CODE ==========================
;===============================================================================
PRO TANAT,$							; call program
	filename,$						; name of file containing time slab
	LINE_CENTER=line_center, $				; line center keyword with spectral information
	ASECPIX=asecpix, $					; spatial resolution in arcseconds per pixel
	DT=dt, $						; time step
	VERBOSE=verbose

;==================== VERSION AND REVISION NUMBER
  ; Version 1.0 (rev 58) == version 1.0.0
	base_version_number = '1.0'

  ; Get revision number from CVS $Id
  id_string = '; $Id: tanat.pro,v 1.15 2014/04/25 10:06:55 gregal Exp $'
  split_id_string = STRSPLIT(id_string[0],' ',/EXTRACT)
  cvs_idn = split_id_string[3]
  cvs_rev = (STRSPLIT(cvs_idn,'.',/EXTRACT))[1]
  cvs_msg = STRJOIN(split_id_string[3:6],' ')
  ; Assumption: CVS committed revision number will always be 1.x, with x
  ; increasing linearly
  revnr = 63+FIX(cvs_rev)-6     ; rev_nr=63, cvs_rev=6 when implemented

  ; Change rev_nr and cvs_rev below whenever changing base_versions_number!
  subvnr = 63 + (FIX(cvs_rev)-6) - 58  ; rev_nr=63, cvs_rev=6 when implemented
  ; Convert revision and version numbers to strings
  revision_number = STRTRIM(revnr,2)   
  version_number = base_version_number +'.'+ STRTRIM(subvnr,2)
  vnr_msg = version_number+' (r'+revision_number+'; '+cvs_msg+')'
	
;==================== PROGRAM-INFO ON CALL W/O PARAMS
	IF N_PARAMS() LT 1 THEN BEGIN
    MESSAGE,'Version '+vnr_msg, /INFO
		MESSAGE,'TANAT, filename, LINE_CENTER=line_center, ASECPIX=asecpix, '+$
      'DT=dt, VERBOSE=verbose', /INFO
		RETURN
	ENDIF

;==================== PROGRAM VERBOSITY CHECK
	IF (N_ELEMENTS(VERBOSE) NE 1) THEN BEGIN			
		IF (N_ELEMENTS(VERBOSE) GT 1) THEN $
      PRINT,'ERROR: The VERBOSE keyword may only be set to a single integer '+$
            'number. Reverting to default verbosity level 0.' 
    verbose = 0
		verbosity = [0,0,0,0,0]
	ENDIF ELSE BEGIN
		verbose >= 0	&	verbose <= 26
	ENDELSE
	verbosity = TANAT_DEC2BIN(verbose)


;================================================================================= TANAT DIRECTORY CHECK
	file_tanat		= (ROUTINE_INFO('TANAT',/SOURCE)).PATH
	dir_aux 		= FILE_DIRNAME(file_tanat,/MARK_DIRECTORY)
	dir_resources		= STRMID(dir_aux,0,STRPOS(STRMID(dir_aux,0,STRLEN(dir_aux)-1),'/',/REVERSE_SEARCH)+1)+'resources'+PATH_SEP()

;================================================================================= START-UP WINDOW
	screensize 	= GET_SCREEN_SIZE()											; Get the user screensize
	x_screen_mid	= screensize[0]/2.
	y_screen_mid	= screensize[1]/2.
	startup_im 	= REBIN(REFORM(TOTAL((TANAT_READ_BMP('tanat_startup.bmp',dir_resources))[*,*,1:2],3)),400,300)
	startup_nx 	= (SIZE(startup_im))[1]
	startup_ny 	= (SIZE(startup_im))[2]
	startup_xpos 	= FIX(x_screen_mid-startup_nx/2.)
	startup_ypos 	= FIX(y_screen_mid-startup_ny/2.)
	xout 		= REPLICATE(24,9)
	yout 		= REPLICATE(FIX(startup_ny/2.5)+10,9)-INDGEN(9)*15

;========================================================================= READ-IN OF FILE(S)
	nfiles = N_ELEMENTS(filename)
	ref = 0
	IF (nfiles LE 2) THEN BEGIN
      TANAT_FILE_RESTORE, filename[0], SPECTRUM=spectrum, MS=ms,$
        SPECT_POS_SAV=spect_pos, SPECT_POS_LOW=spect_pos_low,$
        SPECT_POS_UPP=spect_pos_upp, X_LOOP_PTS=x_loop_pts, Y_LOOP_PTS=y_loop_pts,$
        NLX=nlx, NT=nt, NLP=nlp, LOOP_DATA=loop_data, SLAB_SET=slab_set,$
        W_FIRST=w_first, W_LAST=w_last, W_SET=w_set, $
        NGAPS=ngaps, DATABOUNDS=databounds, WDATABOUNDS=wdatabounds
        empty_slice = MAKE_ARRAY(N_ELEMENTS(x_loop_pts),nt,$
          TYPE=(SIZE(loop_data, /TYPE)))
			IF (nfiles EQ 2) THEN BEGIN
        TANAT_FILE_RESTORE, filename[1], SPECTRUM=refspectrum, MS=refms,$
          SPECT_POS_SAV=ref_spect_pos, SPECT_POS_LOW=ref_spect_pos_low,$
          SPECT_POS_UPP=ref_spect_pos_upp, X_LOOP_PTS=ref_x_loop_pts,$
          Y_LOOP_PTS=ref_y_loop_pts, NLX=refnlx, NT=refnt, NLP=refnlp, $
          LOOP_DATA=ref_loop_data, SLAB_SET=ref_slab_set, W_FIRST=w_first,$
          W_LAST=w_last, W_SET=w_set, $
          NGAPS=ngaps_ref, DATABOUNDS=databounds_ref, $
          WDATABOUNDS=wdatabounds_ref
          ref_empty_slice = MAKE_ARRAY(N_ELEMENTS(ref_x_loop_pts),refnt,$
            TYPE=(SIZE(ref_loop_data, /TYPE)))
        reffilename = STRMID(filename[1], STRPOS(filename[1],'/',/REVERSE_SEARCH)+1,STRLEN(filename[1])-1)		; Failsafe to remove leading /
				IF ((refnlx NE nlx) OR (refnt NE nt)) THEN BEGIN
					PRINT,'ERROR: Dimensions of the reference file (['+STRTRIM(refnlx,2)+','+STRTRIM(refnt,2)+']) are not compatible with those of the main file '+$
						'(['+STRTRIM(nlx,2)+','+STRTRIM(nt,2)+'])!'
					RETURN
				ENDIF ELSE ref = 1
			ENDIF ELSE BEGIN
        reffilename = ''
				ref_slab_set = 0
				ref_loop_data = 0
				refnlx = 0
				refnt = 0
				refnlp = 0
        ngaps_ref = 0
        databounds_ref = 0
        wdatabounds_ref = 0
        ref_empty_slice = 0
			ENDELSE
      gap_consider = ((ngaps GE 1) AND ref)
      ref_gap_consider = (ngaps_ref GE 1)
			filename = STRMID(filename[0], STRPOS(filename[0],'/',/REVERSE_SEARCH)+1,STRLEN(filename[0])-1)		; Failsafe to remove leading /
	ENDIF ELSE BEGIN
		PRINT,'ERROR: You may only supply up to two files for read-in!'
		RETURN
	ENDELSE

	IF (N_ELEMENTS(type) GT 0) THEN BEGIN
		IF (type EQ 1) THEN PRINT,'WARNING: You are currently analysing data from an approximated loop slice!'
	ENDIF

;==================== SETTING START-UP OPTIONS 
;-------------------- INITIAL SPECTRAL PARAMETERS
  TANAT_SET_SPECTPARAMS, spectrum, nlp, spect_pos, LINE_CENTER=line_center,$
    LPS=lps, LC=lc, SPXTITLE=spxtitle, V_DOP_VALS=v_dop_vals, $
    V_DOP_SET=v_dop_set
  ; Failsafe against older save files where spect_pos(_low/upp) were saved
  ; differently in CRISPEX
  IF ((spect_pos NE spect_pos_low) AND (spect_pos_low EQ spect_pos_upp)) THEN $
    lp_start = spect_pos_low $
  ELSE $
    lp_start = spect_pos - spect_pos_low
  lp_first = 0
  lp_last = spect_pos_upp - spect_pos_low
	IF (nfiles EQ 2) THEN BEGIN
    TANAT_SET_SPECTPARAMS, refspectrum, refnlp, ref_spect_pos, $
      LINE_CENTER=ref_line_center, LPS=reflps, LC=reflc, SPXTITLE=refspxtitle,$
      V_DOP_VALS=ref_v_dop_vals, V_DOP_SET=ref_v_dop_set
		reflp_start = ref_spect_pos - ref_spect_pos_low
    reflp_first = 0
		reflp_last = ref_spect_pos_upp - ref_spect_pos_low
		eqnlps = (refnlp EQ nlp)
	ENDIF ELSE BEGIN
		eqnlps = 0
		reflp_start = 0
    reflp_first = 0
		reflp_last = 1
	ENDELSE

;-------------------- INITIAL TIMESLICE PARAMETERS
	IF (N_ELEMENTS(ASECPIX) GE 1) THEN BEGIN
    IF (N_ELEMENTS(ASECPIX) EQ 1) THEN $
      arcsecpix = REPLICATE(asecpix,2) $
    ELSE $
      arcsecpix = asecpix 
  ENDIF ELSE arcsecpix = REPLICATE(0.0592,2)
	TANAT_SET_TIMESLICE_PARAMS, nlx, nt, x_loop_pts, y_loop_pts, arcsecpix, $
    LXDIST=lxdist, CUMUL_LXDIST=cumul_lxdist, LX_FIRST=lx_first, $
    LX_LAST=lx_last, T_FIRST=t_first, T_LAST=t_last, NLXPTS=nlxpts
  IF (gap_consider EQ 0) THEN lx_last = nlx-1
	IF (N_ELEMENTS(DT) EQ 1) THEN secondststep = dt ELSE secondststep = 1.

;-------------------- WINDOW SIZES (CHANGE ONLY NUMERICAL VALUES!)
	windowx		= 0.2 * screensize[0]				; Set maximum x-extent of spectral win(s)
	windowy		= 0.85 * screensize[1]				; Set maximum y-extent of spectral win(s)
	lswinx 		= 0.25 * screensize[0]				; Set maximum x-extent of loc spec win
	lswiny 		= 0.2 * screensize[0]				; Set maximum y-extent of loc spec win

	lsx0		= 0.1						; x0 coordinate of the detailed spectrum plot
	lsy0		= 0.1						; y0 coordinate of the plot
	lsx1		= 0.95						; x1 coordinate of the plot
	IF (v_dop_set EQ 1) THEN lsy1 = 0.9 ELSE lsy1 = 0.95		; y1 coordinate of the plot

	spx0		= 0.2						; x0 coordinate of the temporal spectrum plot
	spy0		= 0.1						; y0 coordinate of the plot
	spx1		= 0.9						; x1 coordinate of the plot
	spy1		= 0.95						; y1 coordinate of the plot
	xplspw		= spx1 - spx0					; x-extent of the plot
	yplspw		= spy1 - spy0					; y-extent of the plot

	ntreb		= yplspw * windowy				; actual nt rebinning factor
	spwiny		= ntreb / yplspw				; extent and rebinning factor
	nlxreb		= xplspw * windowx 				; actual nl rebinning factor
	spwinx		= nlxreb / xplspw				; determine new window x- and y-size from plot

	xdelta		= 20						; Extra xoffset for positioning of windows
	ydelta		= 40						; Extra yoffset for positioning of windows

	ls_low_y	= 0.
	ls_upp_y	= 1.
	ls_yrange	= ls_upp_y - ls_low_y

	loopdata = PTR_NEW(loop_data, /NO_COPY)
	loopslice = PTR_NEW(FLTARR(nlx,nt))
	IF ref THEN BEGIN
		refloopdata = PTR_NEW(ref_loop_data, /NO_COPY)
		refloopslice = PTR_NEW(FLTARR(refnlx,refnt))
	ENDIF ELSE BEGIN
		refloopdata = 0
		refloopslice = 0
	ENDELSE

	lxbpoint = PTR_NEW(FLTARR(1000))
	lxepoint = PTR_NEW(FLTARR(1000))
	tbpoint = PTR_NEW(FLTARR(1000))
	tepoint = PTR_NEW(FLTARR(1000))
	meas_id = PTR_NEW(STRARR(1000))
	lx_array = PTR_NEW(FLTARR(30))
	t_array = PTR_NEW(FLTARR(30))
	slx_array = PTR_NEW(FLTARR(30))
	st_array = PTR_NEW(FLTARR(30))
	sxr = PTR_NEW(FLTARR(30))
	syr = PTR_NEW(FLTARR(30))
	x_vals = PTR_NEW(FLTARR(1000))
	y_vals = PTR_NEW(FLTARR(1000))

  gamma_val = [1.,1.]

;========================================================================= SETTING UP WIDGET
;--------------------------------------------------------------------------------- INITIALISE CONTROL PANEL
  control_panel	      = WIDGET_BASE(TITLE = 'TANAT: Timeslice Analysis Tool', $
                          TLB_FRAME_ATTR=1, /COLUMN, KILL_NOTIFY='TANAT_CLEANUP',$
                          MBAR = menubar)
  filemenu	          = WIDGET_BUTTON(menubar, VALUE='File', /MENU, $
                          UVALUE='file')
	about		            = WIDGET_BUTTON(filemenu, VALUE='About', $
                          EVENT_PRO='TANAT_ABOUT_WINDOW', ACCELERATOR='Ctrl+A')
	open_file	          = WIDGET_BUTTON(filemenu, VALUE='Open...', $
                          EVENT_PRO='TANAT_FILE_OPEN', /SEPARATOR, $
                          ACCELERATOR='Ctrl+O')
	exitmenu	          = WIDGET_BUTTON(filemenu, VALUE='Quit', $
                          EVENT_PRO='TANAT_CLOSE', /SEPARATOR, $
                          ACCELERATOR='Ctrl+Q')

  shortcutmenu		    = WIDGET_BUTTON(menubar, VALUE = 'Control shortcuts',$
                          /MENU, UVALUE = 'shortcut')
  sh_spectralmenu 	  = WIDGET_BUTTON(shortcutmenu, VALUE = 'Spectral options',$
                          /MENU, UVALUE = 'spectral')
  sh_lp_incr_button 	= WIDGET_BUTTON(sh_spectralmenu, $
                          VALUE='Spectral position +', $
                          EVENT_PRO='TANAT_SLIDER_LP_INCR', ACCELERATOR = 'Shift+S')
  sh_lp_decr_button 	= WIDGET_BUTTON(sh_spectralmenu, $
                          VALUE='Spectral position -', $
                          EVENT_PRO = 'TANAT_SLIDER_LP_DECR', ACCELERATOR = 'Shift+A')

  main_base           = WIDGET_BASE(control_panel, /ROW)
	buttons_base	      = WIDGET_BASE(main_base, /COLUMN)
  tab_width           = 470
  pad                 = 3
	tab_tlb 	          = WIDGET_TAB(buttons_base, LOCATION=location,$
                          XSIZE=tab_width+2*pad)

  ; ==================== Define and order tabs ====================
	measure_tab	        = WIDGET_BASE(tab_tlb, TITLE = 'Measurements',/COLUMN,$
                          XSIZE=tab_width)
	spectral_tab	      = WIDGET_BASE(tab_tlb, TITLE = 'Spectral',/COLUMN,$
                          XSIZE=tab_width)
	view_tab	          = WIDGET_BASE(tab_tlb, TITLE = 'View',/COLUMN,$
                          XSIZE=tab_width)
	setup_tab	          = WIDGET_BASE(tab_tlb, TITLE='Set up',/COLUMN,$
                          XSIZE=tab_width)
	WIDGET_CONTROL, tab_tlb, SET_TAB_CURRENT = 3

  ; ==================== Always visible controls ====================
  detspect_frame	    = WIDGET_BASE(buttons_base, /FRAME, /COLUMN)
	detspect_buts	      = WIDGET_BASE(detspect_frame, /ROW, /NONEXCLUSIVE)
	subtract_but	      = WIDGET_BUTTON(detspect_buts, VALUE='Subtract average',$
                          EVENT_PRO='TANAT_LS_SUBTRACT', TOOLTIP='Subtract '+$
                          'detailed spectrum from average spectrum', $
                          SENSITIVE=slab_set)
	detspect_range	    = WIDGET_BASE(detspect_frame, /ROW)
	lower_y_label	      = WIDGET_LABEL(detspect_range, VALUE='Lower y-value:',$
                          /ALIGN_LEFT, SENSITIVE=slab_set)
	lower_y_text	      = WIDGET_TEXT(detspect_range, VALUE='0',  /EDITABLE,$
                          XSIZE=5, EVENT_PRO='TANAT_LS_LOW', SENSITIVE=slab_set)
	upper_y_label	      = WIDGET_LABEL(detspect_range, VALUE='Upper y-value:',$
                          /ALIGN_LEFT, SENSITIVE=slab_set)
	upper_y_text	      = WIDGET_TEXT(detspect_range, VALUE='1',  /EDITABLE,$
                          XSIZE=5, EVENT_PRO='TANAT_LS_UPP', SENSITIVE=slab_set)
	detsp_drawbase	    = WIDGET_BASE(detspect_frame, /COLUMN)
	det_spect	          = WIDGET_DRAW(detsp_drawbase, XSIZE=lswinx, YSIZE=lswiny,$
                          SENSITIVE=slab_set)
  
  ; ==================== Measurement Tab ====================
	sliders		          = WIDGET_BASE(measure_tab, /ROW)
	pos_sliders	        = WIDGET_BASE(sliders, /GRID_LAYOUT, COLUMN=2)
  lp_slid		          = WIDGET_SLIDER(pos_sliders, TITLE = 'Spectral position',$
                          MIN=lp_first, MAX=lp_last>(lp_first+1), $
                          VALUE=lp_start, $
                          EVENT_PRO='TANAT_SLIDER_LP', /DRAG, $
                          SENSITIVE=slab_set,$
                          XSIZE=FLOOR((tab_width-4*pad)/2.))
  lx_slider	          = WIDGET_SLIDER(pos_sliders, $
                          TITLE='Pixel position along the loop', MIN=lx_first,$
                          MAX=lx_last, VALUE=lx_first, $
                          EVENT_PRO='TANAT_SLIDER_LX', /DRAG)
  t_slider	          = WIDGET_SLIDER(pos_sliders, TITLE='Frame number',$
                          MIN=t_first, MAX=t_last, VALUE=t_first, $
                          EVENT_PRO='TANAT_SLIDER_T', /DRAG)
  w_slider	          = WIDGET_SLIDER(pos_sliders, TITLE='Width position', $
                          MIN=w_first, MAX=w_last, VALUE=w_start, $
                          EVENT_PRO='TANAT_SLIDER_W', /DRAG, SENSITIVE=w_set)
  ref_lp_slid	        = WIDGET_SLIDER(pos_sliders, $
                          TITLE='Reference spectral position', $
                          MIN=reflp_first, MAX=reflp_last>(reflp_first+1), $
                          VALUE=reflp_start,$
                          EVENT_PRO='TANAT_SLIDER_REFLP', /DRAG, $
                          SENSITIVE=(ABS(eqnlps-1) AND ref_slab_set))
	ref_lp_but_field    = WIDGET_BASE(pos_sliders, /ROW, /NONEXCLUSIVE)
	ref_lp_but	        = WIDGET_BUTTON(ref_lp_but_field, $
                          VALUE='Lock reference to main position',$
                          EVENT_PRO='TANAT_SLIDER_REFLP_LOCK', $
                          SENSITIVE=(eqnlps AND (refnlp GT 1)))
  WIDGET_CONTROL, ref_lp_but, SET_BUTTON = (eqnlps AND (refnlp GT 1))
  sliders_divider     = TANAT_WIDGET_DIVIDER(measure_tab)

	parameters	        = WIDGET_BASE(measure_tab, /ROW)
	speed		            = WIDGET_BASE(parameters, /COLUMN)
	delta_lx	          = WIDGET_BASE(speed, /ROW)
	delta_t		          = WIDGET_BASE(speed, /ROW)
	delta_speed	        = WIDGET_BASE(speed, /ROW)
	acc		              = WIDGET_BASE(speed, /ROW)
	delta_lx_label	    = WIDGET_LABEL(delta_lx, $
                          VALUE='Travelled distance [pixel]:', /ALIGN_LEFT,$
                          SENSITIVE=0)
  delta_lx_text	      = WIDGET_LABEL(delta_lx, VALUE='0', SENSITIVE=0, $
                          /DYNAMIC_RESIZE)
  delta_t_label	      = WIDGET_LABEL(delta_t, VALUE='Elapsed time [frame]:',$
                          /ALIGN_LEFT, SENSITIVE=0)
  delta_t_text	      = WIDGET_LABEL(delta_t, VALUE='0',  SENSITIVE=0,$
                          /DYNAMIC_RESIZE)
  speed_label	        = WIDGET_LABEL(delta_speed, $
                          VALUE='Absolute speed [km/s]:', /ALIGN_LEFT, $
                          SENSITIVE=0)
  speed_text	        = WIDGET_LABEL(delta_speed, VALUE='0', SENSITIVE=0,$
                          /DYNAMIC_RESIZE)
  acc_label	          = WIDGET_LABEL(acc, VALUE='Acceleration [m/s^2]:',$
                          /ALIGN_LEFT, SENSITIVE=0)
  acc_text	          = WIDGET_LABEL(acc, VALUE='0', SENSITIVe=0,$
                          /DYNAMIC_RESIZE)

	params_and_save     = WIDGET_BASE(parameters, /COLUMN)
	lx_params	          = WIDGET_BASE(params_and_save, /ROW)
	t_params	          = WIDGET_BASE(params_and_save, /ROW)
	save_params	        = WIDGET_BASE(params_and_save, /ROW)
	lx_params_label	    = WIDGET_LABEL(lx_params, VALUE='Position coordinates:',$
                          /ALIGN_LEFT, SENSITIVE=0)
  lx_params_text	    = WIDGET_LABEL(lx_params, VALUE='(0,0)', SENSITIVE=0,$
                          /DYNAMIC_RESIZE)
  t_params_label	    = WIDGET_LABEL(t_params, VALUE='Time coordinates:',$
                          /ALIGN_LEFT, SENSITIVE=0)
	t_params_text	      = WIDGET_LABEL(t_params, VALUE='(0,0)', SENSITIVE=0, $
                          /DYNAMIC_RESIZE)
	flag_label	        = WIDGET_LABEL(save_params, VALUE='Flag:', SENSITIVE=0, $
                          /ALIGN_LEFT)
	flag_text	          = WIDGET_TEXT(save_params, VALUE='0', SENSITIVE=0,$
                          XSIZE=3, /EDITABLE, $
                          EVENT_PRO='TANAT_SAVE_MEASUREMENT_DEFINE_FLAG')
	save_button	        = WIDGET_BUTTON(save_params, VALUE='Save measurement',$
                          EVENT_PRO='TANAT_SAVE_MEASUREMENT', SENSITIVE=0)

	parabolic_fit	      = WIDGET_BASE(measure_tab, /ROW)
	fit_but_base	      = WIDGET_BASE(parabolic_fit, /ROW, /NONEXCLUSIVE)
	set_fit_but	        = WIDGET_BUTTON(fit_but_base, $
                          VALUE='Store points for parabolic fit', $
                          EVENT_PRO='TANAT_PARABOLIC_FIT_SET')
	rem_but		          = WIDGET_BUTTON(parabolic_fit, VALUE='Remove last point',$
                          EVENT_PRO='TANAT_REMOVE_POINT', SENSITIVE=0)
  sliders_divider1    = TANAT_WIDGET_DIVIDER(measure_tab)
	
	series_points	      = WIDGET_BASE(measure_tab, /ROW)
	series_but_base	    = WIDGET_BASE(series_points, /ROW, /NONEXCLUSIVE)
	set_series_but	    = WIDGET_BUTTON(series_but_base, $
                          VALUE='Store series of points', $
                          EVENT_PRO='TANAT_SERIES_SET')
	fdb_series_but	    = WIDGET_BUTTON(series_but_base, VALUE='Feedback',$
                          EVENT_PRO='TANAT_SERIES_FEEDBACK')
	rem_series_but	    = WIDGET_BUTTON(series_points, VALUE='Remove last point',$
                          EVENT_PRO='TANAT_REMOVE_POINT', SENSITIVE=0)
	save_series_but	    = WIDGET_BUTTON(series_points, VALUE='Save series',$
                          EVENT_PRO='TANAT_SERIES_SAVE', SENSITIVE=0)
	WIDGET_CONTROL, fdb_series_but, /SET_BUTTON
  sliders_divider2    = TANAT_WIDGET_DIVIDER(measure_tab)

	overlay		          = WIDGET_BASE(measure_tab, /ROW, /NONEXCLUSIVE)
	overlay_but	        = WIDGET_BUTTON(overlay, VALUE='Overlay saved '+$
                          'measurements for this timeslice', $
                          EVENT_PRO='TANAT_DRAW_OVERLAY_SAVED_MEASUREMENTS')

  ; ==================== View Tab ====================
	t_range_field	      = WIDGET_BASE(view_tab, /ROW)
	lower_t_label	      = WIDGET_LABEL(t_range_field, VALUE='Lower t-value:', $
                          /ALIGN_LEFT)
	lower_t_text	      = WIDGET_TEXT(t_range_field, VALUE=STRTRIM(t_first,2),$
                          /EDITABLE, XSIZE=5, EVENT_PRO='TANAT_T_LOW')
	upper_t_label	      = WIDGET_LABEL(t_range_field, VALUE='Upper t-value:', $
                          /ALIGN_LEFT)
	upper_t_text	      = WIDGET_TEXT(t_range_field, VALUE=STRTRIM(t_last,2),$
                          /EDITABLE, XSIZE=5, EVENT_PRO='TANAT_T_UPP')
	reset_trange_but    = WIDGET_BUTTON(t_range_field, $
                          VALUE='Reset temporal boundaries', $
                          EVENT_PRO='TANAT_T_RANGE_RESET', SENSITIVE=0)
  view_divider1       = TANAT_WIDGET_DIVIDER(view_tab)

	scale_base	        = WIDGET_BASE(view_tab, /COLUMN)
  scaling_base        = WIDGET_BASE(scale_base, /ROW)
  scaling_cbox        = WIDGET_COMBOBOX(scaling_base, $
                          VALUE=['Main data','Reference data'], $
                          EVENT_PRO='TANAT_SCALING_SELECT_DATA')
  scale_reset_but     = WIDGET_BUTTON(scaling_base, VALUE='Reset', $
                          EVENT_PRO='TANAT_SCALING_RESET_DEFAULTS')
  scaleslid_base      = WIDGET_BASE(scale_base, /GRID_LAYOUT, COLUMN=2)
  scalemin_slider     = WIDGET_SLIDER(scaleslid_base, TITLE='Minimum', VALUE=0,$
                          MIN=0, MAX=99, EVENT_PRO='TANAT_SCALING_SLIDER_MIN',$
                          /DRAG, XSIZE=FLOOR((tab_width-4*pad)/2.))
  scalemax_slider     = WIDGET_SLIDER(scaleslid_base, TITLE='Maximum', VALUE=100,$
                          MIN=1, MAX=100, EVENT_PRO='TANAT_SCALING_SLIDER_MAX',$
                          /DRAG)
  gamma_label         = WIDGET_LABEL(scale_base, VALUE=STRING(gamma_val[0], $
                          FORMAT='(F6.3)'), /ALIGN_CENTER,XSIZE=250)
  gamma_slider        = WIDGET_SLIDER(scale_base, TITLE='Gamma', MIN=0, MAX=1000, $
                          VALUE=500*(ALOG10(gamma_val[0])+1), $
                          EVENT_PRO='TANAT_SCALING_GAMMA_SLIDER', $
                          /SUPPRESS, /DRAG)
  view_divider2       = TANAT_WIDGET_DIVIDER(view_tab)

	smooth_buttons	    = WIDGET_BASE(view_tab, /ROW, /EXCLUSIVE)
	non_smooth_but	    = WIDGET_BUTTON(smooth_buttons, VALUE='Non-smoothed view')
	smooth_but	        = WIDGET_BUTTON(smooth_buttons, VALUE='Smoothed view', $
                          EVENT_PRO='TANAT_SMOOTH_VIEW')
	WIDGET_CONTROL, non_smooth_but, /SET_BUTTON
	
  ; ==================== Spectral Tab ====================
	spect_base 	        = WIDGET_BASE(spectral_tab, /COLUMN)
	pos_buts_1	        = WIDGET_BASE(spect_base, /COLUMN, /EXCLUSIVE)
	single_pos	        = WIDGET_BUTTON(pos_buts_1, $
                          VALUE='Single spectral position', $
                          EVENT_PRO='TANAT_POS_SINGLE', SENSITIVE=slab_set)
	WIDGET_CONTROL, single_pos, /SET_BUTTON
	double_pos	        = WIDGET_BUTTON(pos_buts_1, $
                          VALUE='Two combined spectral positions', $
                          EVENT_PRO='TANAT_POS_DOUBLE', SENSITIVE=slab_set)
	mult_pos	          = WIDGET_BUTTON(pos_buts_1, $
                          VALUE='More combined spectral positions', $
                          EVENT_PRO='TANAT_POS_MULT', $
                          SENSITIVE=(slab_set AND (nlp GE 3)))

	lower_sliders	      = WIDGET_BASE(spect_base, /GRID_LAYOUT, COLUMN=2)
	low_low_slider	    = WIDGET_SLIDER(lower_sliders, $
                          TITLE='Lower lower spectral position', VALUE=0, $
                          MIN=0, MAX=(nlp-3)>1, SENSITIVE=0, $
                          EVENT_PRO='TANAT_SLIDER_LOW_LOW', /DRAG, $
                          XSIZE=FLOOR((tab_width-4*pad)/2.))
	low_upp_slider	    = WIDGET_SLIDER(lower_sliders, $
                          TITLE='Upper lower spectral position', VALUE=1, $
                          MIN=1, MAX=(nlp-2)>2, SENSITIVE=0, $
                          EVENT_PRO='TANAT_SLIDER_LOW_UPP', /DRAG)
	upper_sliders	      = WIDGET_BASE(spect_base, /GRID_LAYOUT, COLUMN=2)
	upp_low_slider	    = WIDGET_SLIDER(upper_sliders, $
                          TITLE='Lower upper spectral position', VALUE=(nlp-2)>1, $
                          MIN=1, MAX=(nlp-2)>2, SENSITIVE=0, $
                          EVENT_PRO='TANAT_SLIDER_UPP_LOW', /DRAG,$
                          XSIZE=FLOOR((tab_width-4*pad)/2.))
	upp_upp_slider	    = WIDGET_SLIDER(upper_sliders, $
                          TITLE='Upper upper spectral position', $
                          VALUE=1+(nlp GE 3), $
                          MIN=1+(nlp GE 3), MAX=(nlp-1)>3, SENSITIVE=0, $
                          EVENT_PRO='TANAT_SLIDER_UPP_UPP', /DRAG)
  spectral_divider1   = TANAT_WIDGET_DIVIDER(spectral_tab)
	
	blink_sliders	      = WIDGET_BASE(spectral_tab,/GRID_LAYOUT, COLUMN=2)
	lp_speed_slid	      = WIDGET_SLIDER(blink_sliders, $
                          TITLE='Animation speed [blink/s]', MIN=1, MAX=100, $
                          VALUE=10, EVENT_PRO='TANAT_SLIDER_SPEED', /DRAG,$
                          SENSITIVE=slab_set,$
                          XSIZE=FLOOR((tab_width-4*pad)/2.))
  lp_blink_slid	      = WIDGET_SLIDER(blink_sliders, $
                          TITLE='Spectral increment',$
                          MIN=1, MAX=lp_last>2, VALUE=1, $
                          EVENT_PRO='TANAT_SLIDER_SPECTSTEP',$
                          /DRAG, SENSITIVE=(slab_set AND (nlp GE 3)))
  lp_blink_field	    = WIDGET_BASE(spectral_tab, /ROW,/NONEXCLUSIVE)
  lp_blink_but	      = WIDGET_BUTTON(lp_blink_field, $
                          VALUE='Blink between spectral positions', $
                          EVENT_PRO='TANAT_PB_SPECTBLINK', SENSITIVE=slab_set)

  ; ==================== Set-up Tab ====================
	settings	          = WIDGET_BASE(setup_tab, /ROW)
	dimensions	        = WIDGET_BASE(settings ,/COLUMN)
	arcsec_field	      = WIDGET_BASE(dimensions, /ROW)
	arcsec_label	      = WIDGET_LABEL(arcsec_field, $
                          VALUE='Arcseconds per pixel:', /ALIGN_LEFT)
	dx_text   	        = WIDGET_TEXT(arcsec_field, VALUE=STRTRIM(arcsecpix[0],2),$
                          /EDITABLE, XSIZE=7, EVENT_PRO='TANAT_SET_ARCSEC_X')
	x_label		          = WIDGET_LABEL(arcsec_field, VALUE = 'X', /ALIGN_CENTER)
	dy_text   	        = WIDGET_TEXT(arcsec_field, VALUE=STRTRIM(arcsecpix[1],2),$
                          /EDITABLE, XSIZE=7, EVENT_PRO='TANAT_SET_ARCSEC_Y')
	seconds_field	      = WIDGET_BASE(dimensions, /ROW)
	seconds_label	      = WIDGET_LABEL(seconds_field, $
                          VALUE='Seconds per timestep:', /ALIGN_LEFT)
	seconds_text	      = WIDGET_TEXT(seconds_field, $
                          VALUE=STRTRIM(secondststep,2), /EDITABLE, XSIZE=5, $
                          EVENT_PRO='TANAT_SET_SECONDS')
  setup_divider1      = TANAT_WIDGET_DIVIDER(setup_tab)
	savefile	          = WIDGET_BASE(setup_tab, /COLUMN)
	savefile_label	    = WIDGET_LABEL(savefile, /ALIGN_LEFT, $
                          VALUE='Measurements saved to file:')
	savefile_text	      = WIDGET_TEXT(savefile, VALUE='tanat_measurements.dat',$
                          /EDITABLE, XSIZE=50, EVENT_PRO='TANAT_SET_SAVEFILENAME')

	
  ; Draw base(s)
  draw_base	= WIDGET_BASE(main_base, /ROW)
	timeslice	= WIDGET_DRAW(draw_base, XSIZE = windowx, YSIZE = windowy)
	IF ref THEN reftimeslice = WIDGET_DRAW(draw_base, XSIZE = windowx, YSIZE = windowy)

  ; File name(s)
  filebase        = WIDGET_BASE(control_panel, /ROW, /FRAME)
  label_base      = WIDGET_BASE(filebase, /COLUMN)
  filelabel_val   = 'File:'
  IF (nfiles EQ 2) THEN $
    filelabel_val = 'Main '+STRLOWCASE(filelabel_val)
  filelabel       = WIDGET_LABEL(label_base, VALUE=filelabel_val, /ALIGN_LEFT)
  IF (nfiles EQ 2) THEN $
    reffilelabel  = WIDGET_LABEL(label_base, VALUE='Reference file:', /ALIGN_LEFT)
  filen_base      = WIDGET_BASE(filebase, /COLUMN)
  filetext        = WIDGET_LABEL(filen_base, VALUE=FILE_BASENAME(filename), $
                      /ALIGN_RIGHT, /DYNAMIC_RESIZE)
  IF (nfiles EQ 2) THEN $
    reffiletext   = WIDGET_LABEL(filen_base, VALUE=FILE_BASENAME(reffilename), $
                      /ALIGN_RIGHT, /DYNAMIC_RESIZE) $
  ELSE $
    reffiletext   = 0

	WIDGET_CONTROL, control_panel, /REALIZE
	bg = WIDGET_BASE(control_panel, EVENT_PRO = 'TANAT_PB_BG')

	WIDGET_CONTROL, timeslice, GET_VALUE = drawid
	IF ref THEN WIDGET_CONTROL, reftimeslice, GET_VALUE = refdrawid ELSE refdrawid = 0
	WIDGET_CONTROL, det_spect, GET_VALUE = ls_drawid
	WIDGET_CONTROL, timeslice, EVENT_PRO = 'TANAT_CURSOR', /SENSITIVE, /DRAW_MOTION_EVENTS, /TRACKING_EVENTS,/DRAW_BUTTON_EVENTS

;--------------------------------------------------------------------------------- MEASUREMENT TAB CONTROLS
	ctrlsgen = { $
		subtract_but:subtract_but, lower_y_label:lower_y_label, lower_y_text:lower_y_text, upper_y_label:upper_y_label, $
		upper_y_text:upper_y_text, det_spect:det_spect, $
    filetext:filetext, reffiletext:reffiletext $
	}
;--------------------------------------------------------------------------------- MEASUREMENT TAB CONTROLS
	ctrlsmeas = { $
		lp_slider:lp_slid, lx_slider:lx_slider, t_slider:t_slider, reflp_slider:ref_lp_slid, $
		lp_speed_slider:lp_speed_slid, lp_blink_slider:lp_blink_slid, lp_blink_button:lp_blink_but, $
		delta_lx_label:delta_lx_label, delta_t_label:delta_t_label, delta_lx_text:delta_lx_text, delta_t_text:delta_t_text, $
		speed_label:speed_label, speed_text:speed_text, acc_label:acc_label, acc_text:acc_text, $
		lx_params_text:lx_params_text, t_params_text:t_params_text, lx_params_label:lx_params_label, t_params_label:t_params_label, $
		flag_label:flag_label, flag_text:flag_text, save_button:save_button, set_fit_but:set_fit_but, rem_but:rem_but, overlay_button:overlay_but, $
		set_series_but:set_series_but, rem_series_but:rem_series_but, save_series_but:save_series_but, fdb_series_but:fdb_series_but $
	}
;--------------------------------------------------------------------------------- SETUP CONTROLS
	ctrlssetup = { $
		dx_text:dx_text, dy_text:dy_text, seconds_text:seconds_text, $
		savefile_text:savefile_text $
	}
;--------------------------------------------------------------------------------- VIEW TAB CONTROLS
	ctrlsview = { $
		lower_t_text:lower_t_text, upper_t_text:upper_t_text, reset_trange_but:reset_trange_but, $
		scalemin_slider:scalemin_slider, scalemax_slider:scalemax_slider,$
    gamma_slider:gamma_slider, gamma_label:gamma_label, $
    scale_reset_button:scale_reset_but, $
		single_pos:single_pos, double_pos:double_pos, mult_pos:mult_pos, $
		low_low_slider:low_low_slider, low_upp_slider:low_upp_slider, upp_low_slider:upp_low_slider, upp_upp_slider:upp_upp_slider, $
		non_smooth_button:non_smooth_but, smooth_button:smooth_but $
	}
;--------------------------------------------------------------------------------- CURSOR
	curs = { $
		lxlock:FLOAT(lx_first), tlock:FLOAT(t_first), slxlock:FLOAT(lx_first), stlock:FLOAT(t_first), $
		slx:FLOAT(lx_first), st:FLOAT(t_first), lockset:0 $
	}
;--------------------------------------------------------------------------------- DATA 
	data = { $
		loopdata:loopdata, loopslice:loopslice, refloopdata:refloopdata, $
    refloopslice:refloopslice, empty_slice:PTR_NEW(empty_slice),$
    ref_empty_slice:PTR_NEW(ref_empty_slice)  $
	}
;--------------------------------------------------------------------------------- DATA PARAMETERS
	dataparams = { $
		filename:filename, reffilename:reffilename, spec:PTR_NEW(spectrum), $
    lps:PTR_NEW(lps), ms:ms, nlx:nlx, nlxpts:nlxpts, nt:nt, nlp:nlp, $
		lxdist:PTR_NEW(lxdist), cumul_lxdist:PTR_NEW(cumul_lxdist), $
    lx:FLOAT(lx_first), t:FLOAT(t_first), lx_first:lx_first, lx_last:lx_last, $
		t_first:t_first, t_last:t_last, lp:lp_start, lc:lc, lp_first:lp_first, lp_last:lp_last, $
		t_low:t_first, t_upp:t_last, t_range:nt, d_nt:nt, low_low_val:0,$
    upp_low_val:1, low_upp_val:(nlp-2)>1, upp_upp_val:(1+(nlp GE 3)), $
		refnlp:refnlp, reflp:reflp_start, reflp_first:reflp_first, $
    reflp_last:reflp_last, w_first:w_first, w_last:w_last, w_set:w_set, $
    w:w_first, ngaps:ngaps, databounds:PTR_NEW(databounds), $
    wdatabounds:PTR_NEW(wdatabounds), $
    ngaps_ref:ngaps_ref, databounds_ref:PTR_NEW(databounds_ref), $
    wdatabounds_ref:PTR_NEW(wdatabounds_ref) $
	}
;--------------------------------------------------------------------------------- DISPLAY SWITCHES
	dispswitch = { $
		overlay_measurements:0, singlepos:1, doublepos:0, multpos:0, subtract:0, $
		man_scale:0, smoothed:0, v_dop_set:v_dop_set, slab_set:slab_set, $
		ref:ref, ref_slab_set:ref_slab_set, reflp_lock:eqnlps, series_feedback:1, $
    gap_consider:gap_consider, ref_gap_consider:ref_gap_consider, $
    curs_out_of_range:0 $
	}
;--------------------------------------------------------------------------------- FEEDBACK PARAMS
	feedbparams = { $
		xout:xout, yout:yout, startup_im:startup_im, verbosity:verbosity, last_routine:'', last_routine_count:0 $
	}
;--------------------------------------------------------------------------------- SAVING MEASUREMENT PARAMETERS
	measparams = { $
		lx_array:lx_array, t_array:t_array, slx_array:slx_array, st_array:st_array, sxr:sxr, syr:syr, np:0, speed:0., delta_lx:0, act_delta_lx:0., delta_t:0, $
		acceleration:0., parabolic_fit:0, series:0, x_vals:x_vals, y_vals:y_vals, meas_id:meas_id, secondststep:secondststep, arcsecpix:arcsecpix, $
		savefilename:'tanat_measurements.dat', flag:0 $
	}
;--------------------------------------------------------------------------------- OVERLAY PARAMETERS
	overlays = { $
		lxbpoint:lxbpoint, lxepoint:lxepoint, tbpoint:tbpoint, tepoint:tepoint $
	}
;--------------------------------------------------------------------------------- PLAYBACK PARAMS
	pbparams = { $
		spmode:0, spdirection:1, lp_step:1, lp_speed:10, bg:bg $
	}
;--------------------------------------------------------------------------------- PLOTAXES
	plotaxes = { $
		ls_low_y:ls_low_y, ls_upp_y:ls_upp_y, ls_yrange:ls_yrange, v_dop:v_dop_vals, $
		spxtitle:spxtitle, spwinx:spwinx $
	}
;--------------------------------------------------------------------------------- PLOT PARAMETERS
	plotparams = { $
		plotcol:0, bgplotcol:!P.COLOR $
	}
;--------------------------------------------------------------------------------- PLOT POSITION
	plotpos = { $
		lsx0:lsx0, lsy0:lsy0, lsx1:lsx1, lsy1:lsy1 $
	}
;--------------------------------------------------------------------------------- SCALING PARAMS
	scaling = { $
		min_val:PTR_NEW([0.,0.]), max_val:PTR_NEW([100.,100.]), $
    range:PTR_NEW([100.,100.]),minimum:[0.,0.], maximum:[100.,100.],$
		imrefscaling:0, gamma:gamma_val $
	}
;--------------------------------------------------------------------------------- VERSION INFO
	versioninfo = { $
		version_number:version_number, revision_number:revision_number $				
	}
;--------------------------------------------------------------------------------- WINDOW IDs
	winids = { $
		root:control_panel, wid:drawid, refwid:refdrawid, ls_drawid:ls_drawid, $
		abouttlb:0, savnamenulltlb:0, savnamedoubletlb:0, errtlb:0, tab_tlb:tab_tlb $
	}
;--------------------------------------------------------------------------------- WINDOW SIZES
	winsizes = { $
		aboutwinx:startup_nx, aboutwiny:startup_ny, aboutwinxoffset:startup_xpos, aboutwinyoffset:startup_ypos, $
		lswinx:lswinx, lswiny:lswiny, windowx:windowx, windowy:windowy $
	}
;--------------------------------------------------------------------------------- DEFINE INFO POINTER
	info = { $
		ctrlsgen:PTR_NEW(ctrlsgen,/NO_COPY),$
		ctrlsmeas:PTR_NEW(ctrlsmeas,/NO_COPY),$
		ctrlssetup:PTR_NEW(ctrlssetup,/NO_COPY),$
		ctrlsview:PTR_NEW(ctrlsview,/NO_COPY),$
		curs:PTR_NEW(curs,/NO_COPY),$
		data:PTR_NEW(data,/NO_COPY), $
		dataparams:PTR_NEW(dataparams,/NO_COPY), $
		dispswitch:PTR_NEW(dispswitch,/NO_COPY),$
		feedbparams:PTR_NEW(feedbparams,/NO_COPY),$
		measparams:PTR_NEW(measparams,/NO_COPY), $
		overlays:PTR_NEW(overlays,/NO_COPY), $
		pbparams:PTR_NEW(pbparams,/NO_COPY),$
		plotaxes:PTR_NEW(plotaxes,/NO_COPY),$
		plotparams:PTR_NEW(plotparams,/NO_COPY),$
		plotpos:PTR_NEW(plotpos,/NO_COPY),$
		scaling:PTR_NEW(scaling,/NO_COPY),$
		versioninfo:PTR_NEW(versioninfo,/NO_COPY), $
		winids:PTR_NEW(winids,/NO_COPY), $
		winsizes:PTR_NEW(winsizes,/NO_COPY) $
	}
		
	info = PTR_NEW(info, /NO_COPY)
	WIDGET_CONTROL, control_panel, SET_UVALUE = info

	pseudoevent = { WIDGET_BUTTON, id:control_panel, top:control_panel, handler:0L, select:1 }

	IF (*(*info).dispswitch).slab_set THEN TANAT_UPDATE_LP, pseudoevent ;ELSE BEGIN
;		WIDGET_CONTROL, (*(*info).ctrlsmeas).lp_slider, SENSITIVE = 0
;		WIDGET_CONTROL, (*(*info).ctrlsmeas).lp_speed_slider, SENSITIVE = 0
;		WIDGET_CONTROL, (*(*info).ctrlsmeas).lp_blink_slider, SENSITIVE = 0
;		WIDGET_CONTROL, (*(*info).ctrlsmeas).lp_blink_button, SENSITIVE = 0
;	ENDELSE
	TANAT_UPDATE_LP, pseudoevent
;	IF ref THEN BEGIN
;		(*(*info).scaling).imrefscaling = 1
;		TANAT_SCALING_RANGE, pseudoevent
;		TANAT_DRAW_REF, pseudoevent
;	ENDIF
;	(*(*info).scaling).imrefscaling = 0
;	TANAT_SCALING_RANGE, pseudoevent
;	TANAT_DRAW_MAIN, pseudoevent
;	TANAT_DRAW_LS, pseudoevent
	TANAT_DRAW, pseudoevent
	IF (slab_set NE 1) THEN BEGIN
		WSET, (*(*info).winids).ls_drawid
		TV,CONGRID(REPLICATE(200,10,10),(*(*info).winsizes).lswinx,(*(*info).winsizes).lswiny)
		XYOUTS,(*(*info).winsizes).lswinx/2.,(*(*info).winsizes).lswiny/2.,'Could not display spectral information as!Conly one spectral position is available.', COLOR = 0, ALIGNMENT = 0.5, /DEVICE
	ENDIF
	TANAT_SET_SAVEFILENAME, pseudoevent

;--------------------------------------------------------------------------------- START MANAGING
	XMANAGER, 'TANAT', control_panel, /NO_BLOCK
END
