; NAME:
;       BR_DATA__LOAD_FITSBVAR
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
; $Id: br_data__load_FITSBVAR.pro,v 1.48 2013/09/26 23:01:57 juanms Exp
function br_data::load_fitsbvar,varname,xyz,isnap,w

 if varname eq ' ' then begin
    print,'from bvar:'
    print,'bh: horizontal B, i.e., sqrt(bx^2+by^2)'
    print,'beta: plasma beta, i.e., 8 !pi Pg / B^2'
    print,'bvx or bvy or bvz: absolute value of bx, by or bz' ; THIS CAN BE REMOVE???
    return,0
 endif

; This function loads variables that use the magnetic field
  p=where(varname eq ['bh','beta','va'])
  if p[0] ne -1 then begin
     self.varname = varname
     dd = obj_new('br_data',self.paramsfile)
     dd.rootfitsname=self->getrootfitsname()
     dd.nofits=self->getnofits()
     ; load bx
     bx=dd->getvar('bx',isnap,swap=(self->getaux())->getswap())
     ; load by
     by=dd->getvar('by',isnap,swap=(self->getaux())->getswap())
     ddxyz=dd->getxyz()
     ; define bx^2 and by^2
     bxsq = bx^2
     bysq = by^2
     w = bxsq + bysq
     case varname of
        ; Magnitude of magnetic field with bx, by
        'bh': begin
           w = sqrt(w)
        end
        ; Plasma beta: ratio of gas pressure to magnetic pressure
        'beta': begin
           bz=dd->getvar('bz',isnap,swap=(self->getaux())->getswap())
           bzsq = bz^2
           w = bzsq + w
           p = dd->getvar('pg',isnap,swap=(self->getaux())->getswap())
           w=p*2./w
        end
        ; Alfven speed
        'va': begin
           bz=dd->getvar('bz',isnap,swap=(self->getaux())->getswap())
           bzsq = bz^2
           w = bzsq + w
           buf=dd->getvar('r',isnap,swap=(self->getaux())->getswap())
           w=sqrt(w/buf)
        end
     endcase
     obj_destroy,dd
     w=reform(w,[-ddxyz.ix[0]+ddxyz.ix[1],-ddxyz.iy[0]+ddxyz.iy[1], $ 
                 -ddxyz.iz[0]+ddxyz.iz[1]]+1)
     w=w[xyz.ix[0]:xyz.ix[1],xyz.iy[0]:xyz.iy[1],xyz.iz[0]:xyz.iz[1]]
     return,1
  endif
  p=where(varname eq ['bvx','bvy','bvz'])
  if p[0] ne -1 then begin
    self.varname=varname
    dd = obj_new('br_data',self.paramsfile)
    dd.rootfitsname=self->getrootfitsname()
    dd.nofits=(self->gethdr())->getnofits()
    case varname of
       ; Absolute value of bx
       'bvx': begin
          buf=dd->getvar('bx',isnap,swap=(self->getaux())->getswap())
          w=abs(buf)
       end
       ; Absolute value of by
       'bvy': begin
          buf=dd->getvar('by',isnap,swap=(self->getaux())->getswap())
          w=abs(buf)
       end
       ; Absolute value of bz
       'bvz': begin
          buf=dd->getvar('by',isnap,swap=(self->getaux())->getswap())
          w=abs(buf)
       end
    endcase
    ddxyz=dd->getxyz()
    obj_destroy,dd
    w=reform(w,[-ddxyz.ix[0]+ddxyz.ix[1],-ddxyz.iy[0]+ddxyz.iy[1], $ 
                -ddxyz.iz[0]+ddxyz.iz[1]]+1)
    w=w[xyz.ix[0]:xyz.ix[1],xyz.iy[0]:xyz.iy[1],xyz.iz[0]:xyz.iz[1]]
    return,1
  endif
  return,0
end

           
