; NAME:
;       BR_DATA__LOAD_FITSPOYNTING
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
; This function loads the Poynting Flux, as found on page 12 of
; http://arxiv.org/abs/1205.3764 (Fang Fang et al. 2012)
;
; RESTRICTIONS:
;
;
; MODIFICATION HISTORY:
;
; $Id: br_data__load_fitspoynting.pro,v 1.48 2013/09/26 23:01:57 juanms
; Exp
function br_data::load_fitspoynting,varname,xyz,isnap,w

if varname eq ' ' then begin
    print,'From poynting:'
    print,'spx, spy, and spz: Totalpoynting flux, e.g., '
    print,'spx = -bx (uy by + uz bz) + ux (by^2 + bz^2)  '
    print,'spxfe, spyfe, and spzfe: poynting flux for "vertical" motion, e.g., '
    print,'spxfe = ux (by^2 + bz^2) '
    print,'spxw, spyw, and spzw: poynting flux for "vertical" motion, e.g., '
    print,'spxw =-bx (uy by + uz bz)  '
    return,0
 endif


  p=where(varname eq ['spx','spxw','spxfe','spy','spyw','spyfe', $
                      'spz','spzw','spzfe'])
  if p[0] ne -1 then begin
    self.varname=varname
    dd=obj_new('br_data',self.paramsfile)
    dd.rootfitsname=self->getrootfitsname()
    dd.nofits=self->getnofits()
    r = dd->getvar('r',isnap,swap=(self->getaux())->getswap()) ; reads r
    case varname of
       ; X-Component
       'spx': begin
          ux = dd->getvar('ux',isnap,swap=(self->getaux())->getswap()) ;reads ux
          uy = dd->getvar('uy',isnap,swap=(self->getaux())->getswap()) ;reads uy
          uz = dd->getvar('uz',isnap,swap=(self->getaux())->getswap()) ;reads uz
          bx = dd->getvar('bx',isnap,swap=(self->getaux())->getswap()) ;reads bx
          by = dd->getvar('by',isnap,swap=(self->getaux())->getswap()) ;reads by
          bz = dd->getvar('bz',isnap,swap=(self->getaux())->getswap()) ;reads bz
          w=-bx*(uy*by+uz*bz) + ux*(by^2+bz^2)
       end
       'spxfe': begin ; vertical component
          ux = dd->getvar('ux',isnap,swap=(self->getaux())->getswap()) ;reads ux
          by = dd->getvar('by',isnap,swap=(self->getaux())->getswap()) ;reads by
          bz = dd->getvar('bz',isnap,swap=(self->getaux())->getswap()) ;reads bz
          w=ux*(by^2+bz^2)
       end
       'spxw': begin ; horizontal component
          bx = dd->getvar('bx',isnap,swap=(self->getaux())->getswap()) ;reads bx
          by = dd->getvar('by',isnap,swap=(self->getaux())->getswap()) ;reads by
          bz = dd->getvar('bz',isnap,swap=(self->getaux())->getswap()) ;reads bz
          uy = dd->getvar('uy',isnap,swap=(self->getaux())->getswap()) ;reads uy
          uz = dd->getvar('uz',isnap,swap=(self->getaux())->getswap()) ;reads uz
          w=-bx*(uy*by+uz*bz)
       end
       ; Y-Component
       'spy': begin
          ux = dd->getvar('ux',isnap,swap=(self->getaux())->getswap()) ;reads ux
          uy = dd->getvar('uy',isnap,swap=(self->getaux())->getswap()) ;reads uy
          uz = dd->getvar('uz',isnap,swap=(self->getaux())->getswap()) ;reads uz
          bx = dd->getvar('bx',isnap,swap=(self->getaux())->getswap()) ;reads bx
          by = dd->getvar('by',isnap,swap=(self->getaux())->getswap()) ;reads by
          bz = dd->getvar('bz',isnap,swap=(self->getaux())->getswap()) ;reads bz
          w=-by*(ux*bx+uz*bz)+uy*(bx^2+bz^2)
       end
       'spyfe': begin
          uy = dd->getvar('uy',isnap,swap=(self->getaux())->getswap()) ;reads uy
          bx = dd->getvar('bx',isnap,swap=(self->getaux())->getswap()) ;reads bx
          bz = dd->getvar('bz',isnap,swap=(self->getaux())->getswap()) ;reads bz
          w=uy*(bx^2+bz^2)
       end
       'spyw': begin
          ux = dd->getvar('ux',isnap,swap=(self->getaux())->getswap()) ;reads ux
          uz = dd->getvar('uz',isnap,swap=(self->getaux())->getswap()) ;reads uz
          bx = dd->getvar('bx',isnap,swap=(self->getaux())->getswap()) ;reads bx
          by = dd->getvar('by',isnap,swap=(self->getaux())->getswap()) ;reads by
          bz = dd->getvar('bz',isnap,swap=(self->getaux())->getswap()) ;reads bz
          w=-by*(ux*bx+uz*bz)
       end
       ; Z-Component
       'spz': begin
          ux = dd->getvar('ux',isnap,swap=(self->getaux())->getswap()) ;reads ux
          uy = dd->getvar('uy',isnap,swap=(self->getaux())->getswap()) ;reads uy
          uz = dd->getvar('uz',isnap,swap=(self->getaux())->getswap()) ;reads uz
          bx = dd->getvar('bx',isnap,swap=(self->getaux())->getswap()) ;reads bx
          by = dd->getvar('by',isnap,swap=(self->getaux())->getswap()) ;reads by
          bz = dd->getvar('bz',isnap,swap=(self->getaux())->getswap()) ;reads bz
          w=-bz*(uy*by+ux*bx)+uz*(by^2+bx^2)
       end 
       'spzfe': begin
          uz = dd->getvar('pz',isnap,swap=(self->getaux())->getswap()) ;reads uy
          bx = dd->getvar('bx',isnap,swap=(self->getaux())->getswap()) ;reads bx
          by = dd->getvar('by',isnap,swap=(self->getaux())->getswap()) ;reads by
          w=uz*(by^2+bx^2)
       end 
       'spzw': begin
          ux = dd->getvar('ux',isnap,swap=(self->getaux())->getswap()) ;reads ux
          uy = dd->getvar('uy',isnap,swap=(self->getaux())->getswap()) ;reads uy
          bx = dd->getvar('bx',isnap,swap=(self->getaux())->getswap()) ;reads bx
          by = dd->getvar('by',isnap,swap=(self->getaux())->getswap()) ;reads by
          bz = dd->getvar('bz',isnap,swap=(self->getaux())->getswap()) ;reads bz
          w=-bz*(uy*by+ux*bx)
       end
    endcase
    ddxyz=dd->getxyz()
    obj_destroy,dd
    w=reform(w,[-ddxyz.ix[0]+ddxyz.ix[1],-ddxyz.iy[0]+ddxyz.iy[1], $ 
                -ddxyz.iz[0]+ddxyz.iz[1]]+1)
    w=w[xyz.ix[0]:xyz.ix[1],xyz.iy[0]:xyz.iy[1],xyz.iz[0]:xyz.iz[1]]
    return, 1
    bx=0 & by=0 & bz=0 & ux=0 & uy=0 & uz=0 
 endif
  return,0
end
