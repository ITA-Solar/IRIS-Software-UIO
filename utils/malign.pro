FUNCTION MALIGN,A,B ,plot=plot
;+
; NAME:
;	MALIGN
;
; PURPOSE:
;	Compute the shift image B has to be given to match image A.
;
; CALLING SEQUENCE:
;	Result = MALIGN(A,B)
;
; INPUTS:
;	A = reference image.
;
;	B = image to be aligned.
;
; OUTPUTS:
;	Result = Shift in X,Y to give image B to match A.
;
; SIDE EFFECTS:
;	None.
;
; COMMON BLOCKS:
;	None.
;
; RESTRICTIONS:
;	IF dimensions of images are not a power of 2, algorithm can be
;	slow.
;
; PROCEDURE:
;	It uses the properties of the Fourier transform to compute the
;	cross correlation between the two images.
;
; MODIFICATION HISTORY:
;	Written by Roberto Luis Molowny Horas, July 1992.
;
;-
;
ON_ERROR,2

	sa = SIZE(a)
	sb = SIZE(b)
	IF sa(0) NE 2 THEN MESSAGE,'Image must be 2-D'
	IF sa(1) NE sb(1) OR sa(2) NE sb(2) THEN $
		MESSAGE,'Images must have same dimensions'

	cc = SHIFT(FLOAT(FFT(FFT(a,-1)*$		;Cross correlation.
		CONJ(FFT(b,-1)),1)),sa(1)/2,sa(2)/2)

	if(n_elements(plot) ne 0) then begin
	  plot_image,cc,-sa(1)/2+indgen(sa(1)),-sa(2)/2+indgen(sa(2)),/norm
	endif

	xy = MAXLOC(cc)					;Finding the maximum.

	IF xy(0) EQ 0 OR xy(0) EQ sa(1)-1 OR xy(1) EQ 0 OR xy(1) EQ sa(2)-1 $
		THEN BEGIN
			PRINT,' >>>> Shift too large! '
			x = 0 & y = 0			;Outside image.
	ENDIF ELSE BEGIN
		cc = cc(xy(0)-1:xy(0)+1,xy(1)-1:xy(1)+1);Maximum in centre.
		FIVEPOINT,cc,x,y
		x = xy(0) - sa(1)/2 + x			;Centering.
		y = xy(1) - sa(2)/2 + y
	ENDELSE


	RETURN,[x,y]
	END

