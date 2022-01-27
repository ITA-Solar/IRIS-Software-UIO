PRO Clock, time, pos=pos, size=siz, fill=fill, dev=dev, thick=thick, col=col,bg=bg
;+
; NAME:
;       CLOCK
; PURPOSE:
;       Draw kind of an analog clock in the window
;       
; CALLING SEQUENCE:
;       Clock [, Time ]
;
; OPTIONAL INPUT PARAMETER:
;       Time: (string) Time that the clock should show. Format is 
;             HH:MM:SS
;             If omitted, the currect time (systime) is used.
;
; KEYWORDS:
;       POS  : Position of the clock IN NORMAL COORDINATES  (0-1)
;              Default is (0,0)
;
;       SIZE : Size of clock IN NORMAL COORDINATES  (0-1)
;              Default is 0.2 with a square shape. Elliptic shapes can
;              be created with size being a 2-element vector 
;              [xsize, ysize]
;
;       FILL : (Flag) Erase the area under the clock before drawing
;
;       DEV  : (Flag) Suplied position is in devive coordinates
;              instead of nrom
;
; OUTPUTS:
;       none
; COMMON BLOCKS:
;       none
; MODIFICATION HISTORY:
;       19-Jul-1994  P.Suetterlin, KIS
;       12-Mar-2001  Fixed error in y-pos computation
;-

IF n_params() LT 1 THEN time = strmid(systime(), 11, 8)
IF NOT keyword_set(pos) THEN pos = [0, 0]
IF NOT keyword_set(siz) THEN $
  IF keyword_set(dev) THEN siz = 0.2*!d.x_size ELSE siz = 0.2
  IF keyword_set(thick) eq 0 THEN thick = 3
IF keyword_set(dev) THEN BEGIN
    IF n_elements(siz) EQ 1 THEN BEGIN
        rd = convert_coord([pos(0), 0, siz], [pos(1), 0, 0], /dev, /to_norm)
        pos = rd(0:1, 0)
        siz = rd(0, 2)-rd(0, 1)
    ENDIF ELSE BEGIN
        rd = convert_coord([pos(0), 0, siz(0)], [0, pos(1), 0, siz(1)], $
                           /dev, /to_norm)
        pos = rd(0:1, 0)
        siz = [rd(0, 2)-rd(0, 1), rd(1, 2)-rd(1, 1)]
    ENDELSE
 ENDIF
if n_elements(col) eq 0 then col=255
if n_elements(bg) eq 0 then bg=0

IF n_elements(siz) EQ 1 THEN BEGIN
    rd = convert_coord([0, siz], [0, 0], /norm, /to_dev)
    rd = rd(0, 1)-rd(0, 0)
    rd = convert_coord([0, 0], [0, rd], /dev, /to_norm)
    rd = abs(rd(1, 1)-rd(1, 0))
    siz = [siz, rd]
ENDIF

h = fix(strmid(time, 0, 2)) MOD 12
m = fix(strmid(time, 3, 2))
s = fix(strmid(time, 6, 2))
;;m = fix(m+fix(strmid(time, 6, 2))/60.+0.5)

cx = pos(0)+siz(0)/2
cy = pos(1)+siz(1)/2

b = 0.075
ra = 1
ri = 0.9
rg1 = 0.85
rg2 = -0.2
x = [-b, rg1,  b, rg2, -b]
y = [ b, rg1, -b, rg2, b]

IF keyword_set(fill) THEN $
  polyfill, [pos(0), pos(0), pos(0)+siz(0), pos(0)+siz(0)], $
  [pos(1), pos(1)+siz(1), pos(1)+siz(1), pos(1)], /norm, col = bg


FOR i = 0, 330, 30 DO BEGIN
    phi = i*!Pi/180
    plots, [ri, ra]*cos(phi)*siz(0)/2+cx, $
           [ri, ra]*sin(phi)*siz(1)/2+cy, /norm, thick=thick,col=col
ENDFOR

phi = (h+m/60.)*3*!pi/18
sp = sin(phi) & cp = cos(phi)
polyfill, 0.7*x*[cp, sp, cp, sp, cp]*siz(0)/2+cx, $
          0.7*y*[sp, cp, sp, cp, sp]*siz(1)/2+cy, /norm,col=col


phi = (m*6.+s/10.) * !Pi/180
sp = sin(phi) & cp = cos(phi)
polyfill, x*[cp, sp, cp, sp, cp]*siz(0)/2+cx, $
          y*[sp, cp, sp, cp, sp]*siz(1)/2+cy, /norm,col=col

END

