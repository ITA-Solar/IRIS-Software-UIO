pro iris_colors_yk, r, g, b, contrast=contrast
;+
; NAME:
;       IRIS_COLORS_YK
; PURPOSE:
;       Generate a blue-white-red color table
; CALLING SEQUENCE:
;       iris_colors_yk, [r, g, b, contrast=contrast]
; INPUTS:
;       none
; Keywords:
;       contrast - contrast
; OUTPUTS:
;       none
; Opitional Outputs:
;       r, g, b - RGB values used in the color table
;
; $Id: iris_colors_yk.pro,v 1.1 2013/09/26 07:49:15 viggoh Exp $       
;-

if n_elements(contrast) eq 0 then power=1.5 else power=contrast

b = bytarr(256)
b[0:127] = byte(round(255.*(-abs((1.-findgen(128)/127.)^power)+1)))
b[128:*] = 255
r = reverse(b)
g = b
g[128:*] = r[128:*]

; I want the blue to belong to negative values and red to belong to positive values
r=reverse(r) & g=reverse(g) & b=reverse(b)

; Include black in the table (at the very bottom)
r(0)=0 & g(0)=0 & b(0)=0

tvlct, r,g,b

end
