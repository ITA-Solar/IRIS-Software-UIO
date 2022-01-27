; NAME:
;       BR_DATA__LOAD_FITSPART
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
;      loads variables which depends on "per particle", i.e., divided
;      by density,  to the data object from fits files.
;
; RESTRICTIONS:
;
;
; MODIFICATION HISTORY:
;
; $Id: br_data__load_fitspart.pro,v 1.48 2013/09/26 23:01:57 juanms
; Exp
function br_data::load_fitspart,varname,xyz,isnap,w

if varname eq ' ' then begin
    print,'From part:'
    print,'allows to calculate the each heating per particle only if it is saved in the aux file'
    print,' The way is written is adding only an r at the end of the name, e.g., qjouler = qjoule / r'
    return,0
 endif

  p=where(varname eq ['qjambr','qradr','qeadvr','qpdvr','qviscr','qediffr','qrdiffr','qspitzr','qjouler','qtotr', $
                      'qthinr', 'qgenradr', 'qhr', 'qcar', 'qcwrmr', 'qcheatr', 'qmgr', 'qdeltatr', 'qhmbfr',  $
                      'qbalmerr', 'qincradr', 'qthreshr' ])
  if p[0] ne -1 then begin
    dd = obj_new('br_data',self.paramsfile)
    dd.rootfitsname=self->getrootfitsname()
    dd.nofits=self->getnofits()
    rho= dd->getvar('r',isnap,swap=(self->getaux())->getswap())
    case varname of 
       'qjambr': begin
          w=dd->getvar('qjamb',isnap,swap=(self->getaux())->getswap())
          w = w/rho 
       end
       'qradr': begin
          w=dd->getvar('qrad',isnap,swap=(self->getaux())->getswap())
          w = w/rho 
       end
       'qeadvr': begin
          w=dd->getvar('qeadv',isnap,swap=(self->getaux())->getswap())
          w = w/rho 
       end
       'qpdvr': begin
          w=dd->getvar('qpdv',isnap,swap=(self->getaux())->getswap())
          w = w/rho 
       end
       'qviscr': begin
          w=dd->getvar('qvisc',isnap,swap=(self->getaux())->getswap())
          w = w/rho 
       end
       'qediffr': begin
          w=dd->getvar('qediff',isnap,swap=(self->getaux())->getswap())
          w = w/rho 
       end
       'qrdiffr': begin
          w=dd->getvar('qrdiff',isnap,swap=(self->getaux())->getswap())
          w = w/rho 
       end
       'qspitzr': begin
          w=dd->getvar('qspitz',isnap,swap=(self->getaux())->getswap())
          w = w/rho 
       end
       'qjouler': begin
          w=dd->getvar('qjoule',isnap,swap=(self->getaux())->getswap())
          w = w/rho 
       end
       'qtotr': begin
          w=dd->getvar('qtot',isnap,swap=(self->getaux())->getswap())
          w = w/rho 
       end
       'qthinr': begin
          w=dd->getvar('qthin',isnap,swap=(self->getaux())->getswap())
          w = w/rho 
       end
       'qgenradr': begin
          w=dd->getvar('qgenrad',isnap,swap=(self->getaux())->getswap())
          w = w/rho 
       end
       'qhr': begin
          w=dd->getvar('qh',isnap,swap=(self->getaux())->getswap())
          w = w/rho 
      end
       'qcar': begin
          w=dd->getvar('qca',isnap,swap=(self->getaux())->getswap())
          w = w/rho 
       end
       'qmgr': begin
          w=dd->getvar('qmg',isnap,swap=(self->getaux())->getswap())
          w = w/rho 
       end
       'qcwrmr': begin
          w=dd->getvar('qcwrm',isnap,swap=(self->getaux())->getswap())
          w = w/rho 
       end
       'qcheatr': begin
          w=dd->getvar('qcheat',isnap,swap=(self->getaux())->getswap())
          w = w/rho 
       end
       'qdeltatr': begin
          w=dd->getvar('qdeltat',isnap,swap=(self->getaux())->getswap())
          w = w/rho 
       end
       'qhmbfr': begin
          w=dd->getvar('qhmbf',isnap,swap=(self->getaux())->getswap())
          w = w/rho 
       end
       'qbalmerr': begin
          w=dd->getvar('qbalmer',isnap,swap=(self->getaux())->getswap())
          w = w/rho 
       end
       'qincradr': begin
          w=dd->getvar('qincrad',isnap,swap=(self->getaux())->getswap())
          w = w/rho 
       end
       'qthreshr': begin
          w=dd->getvar('qthresh',isnap,swap=(self->getaux())->getswap())
          w = w/rho 
       end
    endcase
    rho=0 
    ddxyz=dd->getxyz()
    w=reform(w,[-ddxyz.ix[0]+ddxyz.ix[1],-ddxyz.iy[0]+ddxyz.iy[1], $ 
                -ddxyz.iz[0]+ddxyz.iz[1]]+1)
    w=w[xyz.ix[0]:xyz.ix[1],xyz.iy[0]:xyz.iy[1],xyz.iz[0]:xyz.iz[1]]
    return,1
 endif
  return,0
end
