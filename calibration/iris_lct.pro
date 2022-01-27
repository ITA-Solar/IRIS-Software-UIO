pro IRIS_LCT, input, $
	r, g, b, $
	noload = noload, aialike = aialike, test = test
	
;+
; procedure to load 'standard' color tables for IRIS images and spectra
;
; INPUT:	Can be either a level 2 header (structure array), or a string specifying
;			the img_path whose color table should be loaded.
;
; OUTPUTS: 	r, g, b : byte vectors to set the color table via TVLCT
;
; KEYWORDS: /noload: if not set, then the color table is loaded. Set this if you 
;			just want the output vectors
;			/aialike: If set, use Sam's suggested IRIS->AIA color table mapping
;			/test : use some test color tables made in GIMP
;
; Paul Boerner        2014/12/11 spun off from AIA_LCT
;
;-

if N_ELEMENTS(input) eq 0 then begin
	PRINT,'IRIS_LCT: must pass in img_path or level 2 header in first parameter'
	RETURN
endif

if N_TAGS(input) gt 0 then begin
	htags = TAG_NAMES(input[0])
	l2imgpath = WHERE(htags eq 'TDESC1', hasl2)
	if hasl2 gt 0 then begin
		img_path = input[0].tdesc1
	endif else begin
		l1imgpath = WHERE(htags eq 'IMG_PATH', hasl1)
		if hasl1 gt 0 then begin
			img_path = input[0].img_path
		endif else begin
			PRINT, 'IRIS_LCT: No IMG_PATH or TDESC1 in input structure'
			RETURN
		endelse
	endelse
endif else begin
	img_path = input[0]
endelse

if KEYWORD_SET(test) then begin
	testfile = CONCAT_DIR(GET_LOGENV('$SSW'), 'iris/idl/lmsal/calibration/test_colors.sav')
	RESTORE, testfile
endif

; A few vectors with different shapes for color planes
c0=BYTE(FINDGEN(256))
c1=BYTE(SQRT(FINDGEN(256))*SQRT(255.))
c2=BYTE(FINDGEN(256)^2/255.)
c3=BYTE((c1+c2/2.)*255./(MAX(c1) + MAX(c2)/2.))
c4 = BYTARR(256)
c4[50:255] = FINDGEN(206)^2/165.
c5 = (FIX(c1) + FIX(c3))/2.

rr = BYTARR(256) + 255
rr[0:175] = FINDGEN(176)/175. * 255.
gg = BYTARR(256)
gg[100:255] = FINDGEN(156)/155. * 255.
bb = BYTARR(256)
bb[150:255] = FINDGEN(106)/105. * 255.
agg = BYTARR(256)
agg[120:255] = FINDGEN(136)/135. * 255.
abb = BYTARR(256)
abb[190:255] = FINDGEN(66)/65. * 255.

; allowed values of img_path
use_img_path = img_path[0]
img_paths = ['SJI_5000W', 'SJI_2832', 'SJI_2796', 'SJI_1600W', 'SJI_1400', 'SJI_1330', $
	'FUV', 'NUV', 'SJI_NUV']
index = WHERE(use_img_path eq img_paths, numfound)
if numfound eq 0 then use_img_path = STRMID(input[0].tdet1, 0, 3)

case use_img_path of
	'SJI_1330'	: begin  ; 1330
		case 1 of
			KEYWORD_SET(aialike) : begin
				r = c1
				g = c0
				b = c2
			end
			KEYWORD_SET(test) : begin
				r = grr[*,0]
				g = ggg[*,0]
				b = gbb[*,0]
			end
			else : begin
				r = rr
				g = gg
				b = bb
			end
		endcase
	end
	'SJI_1400'	: begin  ; 1400
		case 1 of
			KEYWORD_SET(aialike) : begin
				r = rr
				g = agg
				b = abb
			end
			KEYWORD_SET(test) : begin
				r = grr[*,1]
				g = ggg[*,1]
				b = gbb[*,1]
			end
			else : begin
				r = c5
				g = c2
				b = c4
			end
		endcase
	end
	'SJI_1600W'	: begin  ; 1600
		r = c1
		g = c0
		b = c0
	end
	'SJI_2796'	: begin  ; 2796
		case 1 of
			KEYWORD_SET(aialike) : begin
				r = rr
				g = c0
				b = abb
			end
			KEYWORD_SET(test) : begin
				r = grr[*,2]
				g = ggg[*,2]
				b = gbb[*,2]
			end
			else : begin
				r = c1
				g = c3
				b = c2
			end
		endcase
	end
	'SJI_2832'	: begin  ; 2832
		case 1 of
			KEYWORD_SET(aialike) : begin
				r = c3
				g = c3
				b = c2
			end
			KEYWORD_SET(test) : begin
				r = grr[*,3]
				g = ggg[*,3]
				b = gbb[*,3]
			end
			else : begin
				r = c0
				g = c0
				b = c2
			end
		endcase
	end
	'SJI_5000W'	: begin  ; 5000
		r = c1
		g = c1
		b = c0
	end
	'FUV'	: begin  ; FUV
		r = rr
		g = gg
		b = bb
	end
	'NUV'	: begin  ; NUV
		r = c1
		g = c3
		b = c2
	end
	'SJI_NUV'	: begin  ; NUV
		r = c0
		g = c0
		b = c0
	end
	else	: begin
		PRINT, 'IRIS_LCT : No color table defined for ', img_path[0]
		RETURN
	end
endcase

if not KEYWORD_SET(noload) then TVLCT, r, g, b

end
