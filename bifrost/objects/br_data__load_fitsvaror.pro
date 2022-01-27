; NAME:
;       BR_DATA__LOAD_FITSVAROR
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
;      loads variables which depends on density in the data
;      object from fits files.
;
; RESTRICTIONS:
;
;
; MODIFICATION HISTORY:
;
; $Id: br_data__load_fitsvaror.pro,v 1.48 2013/09/26 23:01:57 juanms
; Exp
function br_data::load_fitsvaror,varname,xyz,isnap,w

 if varname eq ' ' then begin
    print,'from varor:'
    print,'px py or pz: each momentum component'
    print,'ee: internal energy, i.e., e / r'
    print,'s: adiabatic entropy, i.e., alog(p) - gamma*alog(r)'
    print,'cs: sound speed, i.e.,  sqrt(gamma pg / r) '
    return,0
 endif


  p=where(varname eq ['px','py','pz','ee','s','cs'])
  if p[0] ne -1 then begin
    self.varname=varname
    dd = obj_new('br_data',self.paramsfile)
    dd.rootfitsname=self->getrootfitsname()
    dd.nofits=self->getnofits()
    w=dd->getvar('r',isnap,swap=(self->getaux())->getswap())
    case varname of
       'px': begin
          w2=dd->getvar('ux',isnap,swap=(self->getaux())->getswap())
          w=w2*w
       end
       'py': begin
          w2=dd->getvar('uy',isnap,swap=(self->getaux())->getswap())
          w=w2*w 
       end
       'pz': begin
          w2=dd->getvar('uz',isnap,swap=(self->getaux())->getswap())
          w=w2*w 
       end
       'ee': begin
          w2=dd->getvar('e',isnap,swap=(self->getaux())->getswap())
          w=w2/w 
       end
       's': begin
          w2=dd->getvar('pg',isnap,swap=(self->getaux())->getswap())
          w=alog(w2)-1.667*alog(w) 
       end
       'cs': begin
          w2=dd->getvar('pg',isnap,swap=(self->getaux())->getswap())
          w=sqrt((self->getgamma())*w2/w )
       end
   endcase
    ddxyz=dd->getxyz()
    w=reform(w,[-ddxyz.ix[0]+ddxyz.ix[1],-ddxyz.iy[0]+ddxyz.iy[1], $ 
                -ddxyz.iz[0]+ddxyz.iz[1]]+1)
    w=w[xyz.ix[0]:xyz.ix[1],xyz.iy[0]:xyz.iy[1],xyz.iz[0]:xyz.iz[1]]
    return,1
 endif
 return,0
end
