; NAME:
;       BR_DATA__LOAD_FITSOTHERS
;
; PURPOSE:
;       
; INPUTS:
;       varname = the variable name
;       xyz = the domain
;       isnap = the snapshot
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;       w =  output structure
; CALLS:
;       
; PROCEDURE:
;      loads variables which depends on magnetic field to the data
;      object from fits files.
;
; RESTRICTIONS:
;
;
; MODIFICATION HISTORY:
;
; $Id: br_data__load_fitsothers.pro,v 1.48 2013/09/26 23:01:57 juanms Exp
function br_data::load_fitsothers,varname,xyz,isnap,w

if varname eq ' ' then begin
   print,'From others:'
   print,'allows to calculate the emission for any EUV line that is included in table/lines/'
   print,'e.g. fe12_193_p calculates de emission for Fe XII 193 \AA'
   print,'mn: Mach number, i.e., mn = |u| / cs  '
   print,'man: Alfven Mach number, i.e., |u| / va, where va is the alfven speed'
   return,0
 endif

;   This calculates the emissivity of the EUV lines using br_iion
   if STRMATCH(varname, '[a-z]*[0-9]*_[0-9][0-9][0-9]*', /FOLD_CASE) eq 1 then begin
      self.varname=varname
      dd = obj_new('br_data',self.paramsfile)
      (dd->getaux())->setswap,(self->getaux())->getswap()
      br_iion,varname,dd,isnap,/highres
      w=reform(dd->getvar(),[-xyz.ix[0]+xyz.ix[1],-xyz.iy[0]+xyz.iy[1], $
                      -xyz.iz[0]+xyz.iz[1]]+1)
      return,1
   endif

; This functions loads Mach number and Mach alfvenic number
  p = where(varname eq ['mn','man'])
  if p[0] ne -1 then begin
     self.varname = varname
     dd = obj_new('br_data',self.paramsfile)
     modu = dd->getvar('modu',isnap,swap=(self->getaux())->getswap())
     case varname of
        ; Mach number: ratio of flow speed to sound speed
        'mn': begin
           cs = dd->getvar('cs',isnap,swap=(self->getaux())->getswap()) ; load sound speed
           w =modu/cs 
        end
        ; Alfven Mach number: ratio of flow speed to Alfven speed
        'man': begin
           tiny=1e-8
           va = dd->getvar('va',isnap,swap=(self->getaux())->getswap()) ; load Alfven speed
           w = modu/(va+tiny)
        end
     endcase
     ddxyz=dd->getxyz()
     obj_destroy,dd
     w=reform(w,[-ddxyz.ix[0]+ddxyz.ix[1],-ddxyz.iy[0]+ddxyz.iy[1], $ 
                 -ddxyz.iz[0]+ddxyz.iz[1]]+1)
     w=w[xyz.ix[0]:xyz.ix[1],xyz.iy[0]:xyz.iy[1],xyz.iz[0]:xyz.iz[1]]
     return,1
  endif

; This functions loads Non-log of the variables. 
  p = where(varname eq ['tg','r','ne','e','pg'])
  if p[0] ne -1 then begin
     self.varname = varname
     dd = obj_new('br_data',self.paramsfile)
     case varname of
        'tg': begin
           w = dd->getvar('lgtg',isnap,swap=(self->getaux())->getswap()) ; load temperature
        end
        'r': begin
           w = dd->getvar('lgr',isnap,swap=(self->getaux())->getswap()) ; load density
        end
        'ne': begin
           w = dd->getvar('lgne',isnap,swap=(self->getaux())->getswap()) ; load electron density
        end
        'pg': begin
           w = dd->getvar('lgp',isnap,swap=(self->getaux())->getswap()) ; load pressure
        end
        'e': begin
           w = dd->getvar('lge',isnap,swap=(self->getaux())->getswap()) ; load energy
        end
     endcase
     ddxyz=dd->getxyz()
     w = reform( 10^(w) ,[-ddxyz.ix[0]+ddxyz.ix[1],-ddxyz.iy[0]+ddxyz.iy[1], $
                          -ddxyz.iz[0]+ddxyz.iz[1]]+1)
     w=w[xyz.ix[0]:xyz.ix[1],xyz.iy[0]:xyz.iy[1],xyz.iz[0]:xyz.iz[1]] 
     obj_destroy,dd
     return,1
  endif

  return,0
end
