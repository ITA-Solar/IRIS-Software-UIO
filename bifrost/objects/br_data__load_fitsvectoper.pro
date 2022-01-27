; NAME:
;       BR_DATA__LOAD_FITSVECTOPER
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
; This function loads the variables that are calculated from vector
; operations 
;
; RESTRICTIONS:
;
;
; MODIFICATION HISTORY:
;
; $Id: br_data__load_fitsvectoper.pro,v 1.48 2013/09/26 23:01:57 juanms
; Exp
function br_data::load_fitsvectoper,varname,xyz,isnap,w
; This function includes vector operations


if varname eq ' ' then begin
    print,'From vectoper:'
    print,'allows to calculate operations'
    print,'Ending with 2 (except i2) is the square of the vector of the first letter: '
    print,' e.g., b2 = bx^2 + by^2 + bz^2'
    print,'Starting with mod is the module of the vector of the last letter: '
    print,' e.g., modb = sqrt(bx^2 + by^2 + bz^2)'
    print,'In between perp calculates the perpendicular component of the vector of the first '
    print,'letter in comparision to the last letter e.g., bperpu = magnetic field perpendicular'
    print,'to the velocity'
    print,'In between par calculates the parallel component of the vector of the first '
    print,'letter in comparision to the last letter e.g., bparu = magnetic field parallel'
    print,'to the velocity'
    print,'In between rot calculates the vector of the first letter x vector of the last letter '
    print,'e.g., brotu = B x u'
    return,0
 endif


; the square value the 3 components of the vector: i.e, res=(ux^2+uy^2+uz^2)
  if STRMATCH(varname, '*2', /FOLD_CASE) eq 1 and STRMATCH(varname, 'i*', /FOLD_CASE) eq 0 then begin
     dd = obj_new('br_data',self.paramsfile)
     dd.rootfitsname=self->getrootfitsname()
     dd.nofits=self->getnofits()
     varn=STRSPLIT(varname,'2', /EXTRACT,/regex)
     self.varname=varname
     varnew=varn[0]+'x'
     w = (dd->getvar(varnew,isnap,swap=(self->getaux())->getswap()))^2
     varnew=varn[0]+'y'
     w= w + (dd->getvar(varnew,isnap,swap=(self->getaux())->getswap()))^2
     varnew=varn[0]+'z'
     w= w + (dd->getvar(varnew,isnap,swap=(self->getaux())->getswap()))^2
     ddxyz=dd->getxyz()
     w=reform(w,[-ddxyz.ix[0]+ddxyz.ix[1],-ddxyz.iy[0]+ddxyz.iy[1], $ 
                 -ddxyz.iz[0]+ddxyz.iz[1]]+1)
     w=w[xyz.ix[0]:xyz.ix[1],xyz.iy[0]:xyz.iy[1],xyz.iz[0]:xyz.iz[1]]
     obj_destroy,dd
    return,1
 endif

; the module of a vector: i.e, res=sqrt(ux^2+uy^2+uz^2)
  if STRMATCH(varname, 'mod*', /FOLD_CASE) eq 1 then begin
     dd = obj_new('br_data',self.paramsfile)
     dd.rootfitsname=self->getrootfitsname()
     dd.nofits=self->getnofits()
     varn=STRSPLIT(varname,'mod', /EXTRACT,/regex)
     self.varname=varname
     varnew=varn[0]+'x'
     w = (dd->getvar(varnew,isnap,swap=(self->getaux())->getswap()))^2
     varnew=varn[0]+'y'
     w= w + (dd->getvar(varnew,isnap,swap=(self->getaux())->getswap()))^2
     varnew=varn[0]+'z'
     w= w + (dd->getvar(varnew,isnap,swap=(self->getaux())->getswap()))^2
     w=sqrt(w) 
     ddxyz=dd->getxyz()
     w=reform(w,[-ddxyz.ix[0]+ddxyz.ix[1],-ddxyz.iy[0]+ddxyz.iy[1], $ 
                 -ddxyz.iz[0]+ddxyz.iz[1]]+1)
     w=w[xyz.ix[0]:xyz.ix[1],xyz.iy[0]:xyz.iy[1],xyz.iz[0]:xyz.iz[1]]
     obj_destroy,dd
    return,1
 endif

; the perpendicular/parallel component of a vector respect to another
; vector: uperpb = velocity perpedicular to the magnetic field
   if STRMATCH(varname, '*perp*', /FOLD_CASE) eq 1 or STRMATCH(varname, '*par*', /FOLD_CASE) eq 1 then begin
      dd = obj_new('br_data',self.paramsfile)
      dd.rootfitsname=self->getrootfitsname()
      dd.nofits=self->getnofits()
      self.varname=varname
      if  STRMATCH(varname, '*perp*', /FOLD_CASE) eq 1 then varn=STRSPLIT(varname,'perp', /EXTRACT,/regex)
      if  STRMATCH(varname, '*par*', /FOLD_CASE) eq 1 then varn=STRSPLIT(varname,'par', /EXTRACT,/regex)
      if  STRMATCH(varname, '*pbpg', /FOLD_CASE) eq 0 then begin
         varnew=varn[1]+'x'
         bx=dd->getvar(varnew,isnap,swap=(self->getaux())->getswap())
         varnew=varn[1]+'y'
         by=dd->getvar(varnew,isnap,swap=(self->getaux())->getswap())
         varnew=varn[1]+'z'
         bz=dd->getvar(varnew,isnap,swap=(self->getaux())->getswap())
      endif else begin
         bx=dd->getvar('bx',isnap,swap=(self->getaux())->getswap())
         by=dd->getvar('by',isnap,swap=(self->getaux())->getswap())
         bz=dd->getvar('bz',isnap,swap=(self->getaux())->getswap())
      endelse      
      if STRMATCH(varname, '?par*', /FOLD_CASE) eq 1 then begin
         varn=STRSPLIT(varname,'par', /EXTRACT,/regex)
         varnew=varn[0]+'x'
         ux=dd->getvar(varnew[0],isnap,swap=(self->getaux())->getswap())
         w=(ux*bx)^2/(bx^2+by^2+bz^2)
         varnew=varn[0]+'y'
         uy=dd->getvar(varnew[0],isnap,swap=(self->getaux())->getswap())
         w=w+(uy*by)^2/(bx^2+by^2+bz^2)
         varnew=varn[0]+'z'
         uz=dd->getvar(varnew[0],isnap,swap=(self->getaux())->getswap())
         w=sqrt(w+(uz*bz)^2/(bx^2+by^2+bz^2))
         ddxyz=dd->getxyz()
         w=reform(w,[-ddxyz.ix[0]+ddxyz.ix[1],-ddxyz.iy[0]+ddxyz.iy[1], $ 
                     -ddxyz.iz[0]+ddxyz.iz[1]]+1)
         w=w[xyz.ix[0]:xyz.ix[1],xyz.iy[0]:xyz.iy[1],xyz.iz[0]:xyz.iz[1]]
      endif else begin
         varn=STRSPLIT(varname,'perp', /EXTRACT,/regex)
         varnew=varn[0]+'x'
         ux=dd->getvar(varnew[0],isnap,swap=(self->getaux())->getswap())
         uxpar=ux*bx/sqrt(bx^2+by^2+bz^2)
         varnew=varn[0]+'y'
         uy=dd->getvar(varnew[0],isnap,swap=(self->getaux())->getswap())
         uypar=uy*by/sqrt(bx^2+by^2+bz^2)
         varnew=varn[0]+'z'
         uz=dd->getvar(varnew[0],isnap,swap=(self->getaux())->getswap())
         uzpar=uz*bz/sqrt(bx^2+by^2+bz^2)
         uperx=ux-uxpar
         upery=uy-uypar
         uperz=uz-uzpar
         w = sqrt(uperx^2+upery^2+uperz^2)
         ddxyz=dd->getxyz()
         w=reform(w,[-ddxyz.ix[0]+ddxyz.ix[1],-ddxyz.iy[0]+ddxyz.iy[1], $ 
                     -ddxyz.iz[0]+ddxyz.iz[1]]+1)
         w=w[xyz.ix[0]:xyz.ix[1],xyz.iy[0]:xyz.iy[1],xyz.iz[0]:xyz.iz[1]]
      endelse
      varx=0 & vary=0 & varz=0
      ux=0 & uy=0 & uz=0
      bx=0 & by=0 & bz=0 & pg=0
      obj_destroy,dd
      return,1
   endif

; the rotational of two vectors
; urotbx = x component of (velocity x magnetic field)
   if STRMATCH(varname, '*rot*', /FOLD_CASE) eq 1 then begin
      dd = obj_new('br_data',self.paramsfile)
      dd.rootfitsname=self->getrootfitsname()
      dd.nofits=self->getnofits()
      varn=STRSPLIT(varname,'rot', /EXTRACT,/regex)
      self.varname=varname
      case strmid(varn[1],1,1) of
         'x': begin
            varnew=strmid(varn[1],0,1)+'y'
            by=dd->getvar(varnew,isnap,swap=(self->getaux())->getswap())
            varnew=strmid(varn[1],0,1)+'z'
            bz=dd->getvar(varnew,isnap,swap=(self->getaux())->getswap())
            varnew=varn[0]+'y'
            uy=dd->getvar(varnew[0],isnap,swap=(self->getaux())->getswap())
            varnew=varn[0]+'z'
            uz=dd->getvar(varnew[0],isnap,swap=(self->getaux())->getswap())
            w = uy*bz-uz*by
         end
         'y': begin
            varnew=strmid(varn[1],0,1)+'x'
            bx=dd->getvar(varnew,isnap,swap=(self->getaux())->getswap())
            varnew=strmid(varn[1],0,1)+'z'
            bz=dd->getvar(varnew,isnap,swap=(self->getaux())->getswap())
            varnew=varn[0]+'x'
            ux=dd->getvar(varnew[0],isnap,swap=(self->getaux())->getswap())
            varnew=varn[0]+'z'
            uz=dd->getvar(varnew[0],isnap,swap=(self->getaux())->getswap())
            w = uz*bx-ux*bz
         end
         'z': begin
            varnew=strmid(varn[1],0,1)+'x'
            bx=dd->getvar(varnew,isnap,swap=(self->getaux())->getswap())
            varnew=strmid(varn[1],0,1)+'y'
            by=dd->getvar(varnew,isnap,swap=(self->getaux())->getswap())
            varnew=varn[0]+'x'
            ux=dd->getvar(varnew[0],isnap,swap=(self->getaux())->getswap())
            varnew=varn[0]+'y'
            uy=dd->getvar(varnew[0],isnap,swap=(self->getaux())->getswap())
            w = ux*by-uy*bx
         end
         else : begin
            print,'WARNING: the variable must end with x, y or z: e.g., urotbx '
            exit
         end
      endcase
      ddxyz=dd->getxyz()
      w=reform(w,[-ddxyz.ix[0]+ddxyz.ix[1],-ddxyz.iy[0]+ddxyz.iy[1], $ 
                  -ddxyz.iz[0]+ddxyz.iz[1]]+1)
      w=w[xyz.ix[0]:xyz.ix[1],xyz.iy[0]:xyz.iy[1],xyz.iz[0]:xyz.iz[1]]
      ux=0 & uy=0 & uz=0
      bx=0 & by=0 & bz=0
      return,1
   endif
   return,0
end
