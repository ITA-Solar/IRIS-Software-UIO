; docformat = 'rst'

;+
; Linear interpolation in many dimensions.
;
; Like the bilinear and trilinear interpolation of IDL's built-in
; INTERPOLATE function. Wrapper for ninterpolate by J.D. Smith to
; support interpolation for multiple points.
; 
; :Categories:
;
;    SST pipeline
; 
; 
; :Author:
; 
;    Mats Löfdahl, Institute for Solar Physics
; 
; 
; :Returns:
; 
;    An array with the interpolated values.
;
; :Params:
; 
;    P : in, type=fltarr
; 
;      The array of data values. The number of dimensions should match
;      the number of coordinate arrays.
;
;    x0 : in, type=fltarr
;
;       Array with coordinate values for the first coordinate.
; 
;    x1 : in, type=fltarr
;
;       Array with coordinate values for the second coordinate.
; 
;    x2 : in, type=fltarr
;
;       Array with coordinate values for the third coordinate.
; 
; :Keywords:
; 
;    mode : in, optional, type=string
;   
;       If 'idl', use IDL's built-in interpolate function. If
;       'mint', use minterpolate.
; 
; 
; :History:
; 
;    2017-09-29 : MGL. First version.
;    2017-12-19 : Gregal Vissers, added to CRISPEX distribution in aux/ninterpol 
; 
; 
; 
; 
;-
function crispex_interpolate_many, P, x0, x1, x2, x3, x4, x5, x6, mode = mode

  Ndims = size(P, /n_dim)
  Npoints = n_elements(x0) 

  if Ndims eq 0 or Npoints eq 0 then stop

  ;; No need for multiple dimensions 
  if Ndims eq 1 then return, interpolate(p, x0)

  if n_elements(mode) eq 0 then mode = ''
  
  ;; Check same number of points in all axes
  if Ndims eq 2 and n_elements(x1) ne Npoints then stop
  if Ndims eq 3 and n_elements(x2) ne Npoints then stop
  if Ndims eq 4 and n_elements(x3) ne Npoints then stop
  if Ndims eq 5 and n_elements(x4) ne Npoints then stop
  if Ndims eq 6 and n_elements(x5) ne Npoints then stop
  if Ndims eq 7 and n_elements(x6) ne Npoints then stop

  ;; Want the built-in IDL interpolate?
  if mode eq 'idl' then begin
     case Ndims of
        2 : return, interpolate(p, x0, x1)
        3 : return, interpolate(p, x0, x1, x2)
        else : stop
     endcase
  endif

  case Ndims of
     2 : points = [[x0],[x1]]
     3 : points = [[x0],[x1],[x2]]
     4 : points = [[x0],[x1],[x2],[x3]]
     5 : points = [[x0],[x1],[x2],[x3],[x4]]
     6 : points = [[x0],[x1],[x2],[x3],[x4],[x5]]
     7 : points = [[x0],[x1],[x2],[x3],[x4],[x5],[x6]]
     else : stop
  endcase

  result = fltarr(Npoints)
  if mode eq 'mint' then $
     for ipoint = 0, Npoints-1 do result[ipoint] = minterpolate(P, reform(points[ipoint, *])) $
  else $
     for ipoint = 0, Npoints-1 do result[ipoint] = crispex_ninterpolate(P, reform(points[ipoint, *])) 
     

  return, result
  
end

data2 = randomu(seed, 10, 10)
data3 = randomu(seed, 10, 10, 10)
data4 = randomu(seed, 10, 10, 10, 10)
data5 = randomu(seed, 10, 10, 10, 10, 10)
data6 = randomu(seed, 10, 10, 10, 10, 10, 10)

x0 = [.37, .45, .12]*10
x1 = [.81, .45, .22]*10
x2 = [.13, .45, .32]*10
x3 = [.22, .45, .42]*10
x4 = [.47, .45, .52]*10
x5 = [.73, .45, .62]*10

print, 'ninterpolate', crispex_interpolate_many(data2, x0, x1)
print, 'minterpolate', crispex_interpolate_many(data2, x0, x1, mode = 'mint')
print, ' interpolate', crispex_interpolate_many(data2, x0, x1, mode = 'idl')
print

print, 'ninterpolate', crispex_interpolate_many(data3, x0, x1, x2)
print, 'minterpolate', crispex_interpolate_many(data3, x0, x1, x2, mode = 'mint')
print, ' interpolate', crispex_interpolate_many(data3, x0, x1, x2, mode = 'idl')
print

print, 'ninterpolate', crispex_interpolate_many(data4, x0, x1, x2, x3)
print, 'minterpolate', crispex_interpolate_many(data4, x0, x1, x2, x3, mode = 'mint')
print

print, 'ninterpolate', crispex_interpolate_many(data5, x0, x1, x2, x3, x4)
print, 'minterpolate', crispex_interpolate_many(data5, x0, x1, x2, x3, x4, mode = 'mint')
print

print, 'ninterpolate', crispex_interpolate_many(data6, x0, x1, x2, x3, x4, x5)
print, 'minterpolate', crispex_interpolate_many(data6, x0, x1, x2, x3, x4, x5, mode = 'mint')
print

 
                
end            
