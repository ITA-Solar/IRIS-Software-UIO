;+
; NAME:
;       BR_HDR__DEFINE
;
; PURPOSE:
;
; INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
; CALLS:
;       
; PROCEDURE:
;       Defines the structure of header class objects.
;
; RESTRICTIONS:
;
;
; MODIFICATION HISTORY:
;
; $Id: br_hdr__define.pro,v 1.16 2015/06/29 15:01:57 viggoh Exp $
;-
function br_hdr::init,paramsfile,quiet=quiet,isnap=isnap,nofits=nofits
  self.title='BR Header'
  self->inithdr,paramsfile,isnap=isnap,quiet=quiet,nofits=nofits
  return,1
end

pro br_hdr::inithdr,paramsfile,isnap=isnap,quiet=quiet,nofits=nofits
  if n_elements(paramsfile) eq 0 then self.paramsfile='mhd' $
                                 else self.paramsfile=paramsfile
  self.meshfile=''
  if n_elements(quiet) eq 0 then quiet=0
  if not quiet then begin
    message,self->gettitle()+self->getver_no()+ $
            ' params file root: '+self.paramsfile,/info
  endif

  self.mvar=8
  self.snapvars=['r', 'px', 'py', 'pz', 'e', 'bx', 'by', 'bz']
  self.auxvars1dx = ['bx0t','by0t','xs_bcut','xe_bcut','ys_bcut','ye_bcut' ,'tbt','tbtmax','tbw']
  self.snappos=indgen(8)
  self.snapname=self.paramsfile
  self.do_hion=-1
  self.hionhasoldpops=-1
  self.hionatomfile='undefined'
  self.hionneetabfile='undefined'
  if n_elements(isnap) ne 0 then begin
     nofits=self->setnofits(nofits=nofits)
     self->readparams,isnap,nofits=self.nofits
     self->readmesh,nofits=self.nofits,isnap=isnap
  endif
end

pro br_hdr::cleanup
  ptr_free,self.auxvars
  ptr_free,self.auxpos
  ptr_free,self.idlparams
  ptr_free,self.xm
  ptr_free,self.xmdn
  ptr_free,self.dxidxup
  ptr_free,self.dxidxdn
  ptr_free,self.dx1d
  ptr_free,self.ym
  ptr_free,self.ymdn
  ptr_free,self.dyidyup
  ptr_free,self.dyidydn
  ptr_free,self.dy1d
  ptr_free,self.zm
  ptr_free,self.zmdn
  ptr_free,self.dzidzup
  ptr_free,self.dzidzdn
  ptr_free,self.dz1d
end

pro br_hdr::read,isnap,nofits=nofits,varname=varname
  if n_elements(isnap) eq 0 then begin
     message,'no snapshot number given; isnap set to -1',/info
     isnap=-1
     self.isnap=isnap
  endif
  if n_elements(nofits) eq 0  then nofits=self.nofits
  self->readparams,isnap,nofits=nofits
  self->readmesh,nofits=nofits,isnap=isnap
end

pro br_hdr::readparams,isnap,paramsfile=paramsfile, $
         nofits=nofits,filefits=filefits,verbose=verbose
  if n_elements(nofits) eq 0 then begin
     nofits=self->setnofits()
  endif else begin
     nofits=self->setnofits(nofits=nofits)
  endelse
  self.nofits=nofits
  if nofits eq 0 then begin
     self->readfitsparams,isnap,filefits=filefits
  endif else begin
     self->readidlparams,isnap,paramsfile=paramsfile,verbose=verbose
  endelse

end

pro br_hdr::readidlparams,isnap,paramsfile=paramsfile, $
         verbose=verbose
  if n_elements(paramsfile) eq 0 then paramsfile=self.paramsfile
  self->setparamsfile,paramsfile
  self.snapname=paramsfile
  if n_elements(isnap) eq 0 then begin
    message,'no snapshot number given; isnap set to -1',/info
    isnap=-1
 endif
  self.isnap=isnap
  if isnap lt 0 then begin
    self.scr=1
    if isnap eq -9999 then self.scr=0
  endif else self.scr=0
  fi=file_info(self->getfilename('.idl'))
  if not fi.exists then begin
    message,'NO such file '+self->getfilename('.idl')
  endif
  if keyword_set(verbose) then $
                        message,'reading '+self->getfilename('.idl'),/info
  openr,lu,self->getfilename('.idl'),/get_lun
  dum=' '
  maxlines = 1000
  idlparams = strarr(maxlines)
  i=0
  
  on_ioerror, done
  more:
    readf, lu, dum
    idlparams(i) = dum
    result = execute(idlparams(i))
    i = i+1
    goto, more
  done:
  if ptr_valid(self.idlparams) then ptr_free,self.idlparams
  self.idlparams=ptr_new(idlparams[0:i])
  self.mzb=mz
  if n_elements(mb) eq 0 then mb=5
  self.mb=mb
;
; units
  if n_elements(u_l) ne 0 then self.units.ul=u_l
  if n_elements(u_t) ne 0 then self.units.ut=u_t
  if n_elements(u_r) ne 0 then self.units.ur=u_r
  if n_elements(u_kr) ne 0 then self.units.kr=u_kr
  if n_elements(u_te) ne 0 then self.units.te=u_te
  if n_elements(u_tg) ne 0 then self.units.tg=u_tg
;
  if n_elements(boundarychk) eq 0 then boundarychk=0
  if boundarychk then self.mzb=mz+2*mb
;

  free_lun, lu
  if isnap ne self.isnap and self.isnap ge 0 then $
    message,'the required isnap '+strtrim(string(isnap),2)+' =/ isnap in '+ $
            self->getfilename('.idl'),/info
;
  if n_elements(meshfile) ne 0 then self.meshfile=strtrim(meshfile,2)
  if n_elements(snapname) ne 0 then self.snapname=strtrim(snapname,2)
  if n_elements(coltablefile) ne 0 then self.coltablefile=strtrim(coltablefile,2)
  if n_elements(dt) ne 0 then self.dt=dt else dt=1.e-30
  if n_elements(dtsnap) ne 0 then self.dtsnap=dtsnap else self.dtsnap=-1.e30
  if n_elements(mf_epf) ne 0 then self.mf_epf=mf_epf else self.mf_epf =0
  if n_elements(mf_nfluid) ne 0 then self.mf_nfluid=mf_nfluid else self.mf_nfluid =1
  if n_elements(mf_ifluid) ne 0 then self.mf_ifluid=mf_ifluid else self.mf_ifluid =1
  if n_elements(gamma) ne 0 then self.gamma=gamma else gamma=5./3.
  if n_elements(t) ne 0 then self.t=t else self.t=0.
  if n_elements(mx) ne 0 then self.mx=mx else self.mx=0
  if n_elements(my) ne 0 then self.my=my else self.my=0
  if n_elements(mz) ne 0 then self.mz=mz else self.mz=0
  if n_elements(nstepstart) ne 0 then self.nstepstart=long(nstepstart) $
  else self.nstepstart=0l
  if n_elements(dx) ne 0 then self.dx=dx else self.dx=0.
  if n_elements(dy) ne 0 then self.dy=dy else self.dy=0.
  if n_elements(dz) ne 0 then self.dz=dz else self.dz=0.
  if n_elements(dt) ne 0 then self.dt=dt else self.dt=0.
  if n_elements(coltablefile) ne 0 then self.coltablefile=strtrim(coltablefile,2) else self.coltablefile=' '
  if n_elements(teff) ne 0 then self.teff=teff else self.teff=0.
  if n_elements(nu1) ne 0 then self.nu1=nu1 else self.nu1=0.
  if n_elements(nu2) ne 0 then self.nu2=nu2 else self.nu2=0.
  if n_elements(nu3) ne 0 then self.nu3=nu3 else self.nu3=0.
  if n_elements(nu_r_xy) ne 0 then self.nu_r_xy=nu_r_xy else self.nu_r_xy=0.
  if n_elements(nu_r) ne 0 then self.nu_r=nu_r else self.nu_r=0.
  if n_elements(nu_ee) ne 0 then self.nu_ee=nu_ee else self.nu_ee=0.
  if n_elements(eta3) ne 0 then self.eta3=eta3 else self.eta3=0.
  if n_elements(ca_max) ne 0 then self.ca_max=ca_max else self.ca_max=0.
  if n_elements(rbot) ne 0 then self.rbot=rbot else self.rbot=0.
  if n_elements(ebot) ne 0 then self.ebot=ebot else self.ebot=0.
  if n_elements(bx0) ne 0 then self.bx0=bx0 else self.bx0=0.
  if n_elements(by0) ne 0 then self.by0=by0 else self.by0=0.
  if n_elements(strtb) ne 0 then self.strtb=strtb else self.strtb=' '
  if n_elements(nu_rm) ne 0 then self.nu_rm=nu_rm else self.nu_rm=0.
  if n_elements(zotb) ne 0 then self.zotb=zotb else self.zotb=0.

; cork parameters
  if n_elements(ck_nrcorks) ne 0 then self.ck_nrcorks=ck_nrcorks else self.ck_nrcorks=0LL
  if n_elements(ck_placement) ne 0 then self.ck_placement=ck_placement else self.ck_placement='undefined'
  if n_elements(ck_inject) ne 0 then self.ck_inject=ck_inject else self.ck_inject=-1
  if n_elements(ck_highest_id) ne 0 then self.ck_highest_id=ck_highest_id else self.ck_highest_id=0LL
  if n_elements(ck_relocate) ne 0 then self.ck_relocate=ck_relocate else self.ck_relocate=-1
  if n_elements(ck_aux) ne 0 then begin
; ck_aux keyword present in IDL file
     ck_aux=strsplit(ck_aux,' ',/extract)
     if n_elements(ck_aux) ne 0 then begin
;ck_aux string is not empty
;        self.ck_auxvars=ptr_new(strarr(n_elements(ck_aux))) 
        self.ck_auxvars=ptr_new(ck_aux)
     endif else begin
; ck_aux string present but empty
        self.auxpos=ptr_new(0)
     endelse
  endif else begin
; ck_aux string not present in IDL file
     ck_auxvars=ptr_new(0)
  endelse

  if n_elements(boundarychk) ne 0 then self.boundarychk=boundarychk

  aux=strsplit(aux,' ',/extract)
  if n_elements(aux) ne 0 then self.auxpos=ptr_new(intarr(n_elements(aux))) $
                          else self.auxpos=ptr_new(0)
  self.auxvars=ptr_new(aux) 

  if n_elements(tabinputfile) ne 0 then self.tabinputfile=strtrim(tabinputfile,2)
  if n_elements(do_hion) ne 0 then self.do_hion=do_hion else self.do_hion=0
  if n_elements(hionhasoldpops) ne 0 then self.hionhasoldpops=hionhasoldpops else self.hionhasoldpops=0.
  if n_elements(hionatomfile) ne 0 then self.hionatomfile=hionatomfile else self.hionatomfile=0.
  if n_elements(hionneetabfile) ne 0 then self.hionneetabfile=hionneetabfile else self.hionneetabfile=0.

  ; heliumpaprams
  if n_elements(do_helium) ne 0 then self.do_helium=do_helium else self.do_helium=0
  if n_elements(hehasoldpops) ne 0 then self.hehasoldpops=hehasoldpops else self.hehasoldpops=0
  if n_elements(heliumatomfile) ne 0 then self.heliumatomfile=heliumatomfile else self.heliumatomfile='undefined'
  if n_elements(heliumeuvfile) ne 0 then self.heliumeuvfile=heliumeuvfile else self.heliumeuvfile='undefined'


  ;--------ooe--------
  if n_elements(atomfl) ne 0 then self.atomfl=atomfl else self.atomfl=''
  ;-------------------




end


pro br_hdr::readfitsparams,isnap,filefits=filefits, $
         verbose=verbose
forward_function readfits


  if n_elements(isnap) eq 0 then begin
    message,'no snapshot number given; isnap set to 1',/info
    isnap=1
 endif
  self.isnap=isnap
  if n_elements(filefits) eq 0 then begin
     fitsname=self->getfitsfilename('.fits',isnap=isnap)
  endif else begin
     fitsname = filefits+'_'+strtrim(br_string3(isnap),2)+'.fits'
  endelse

  fi=file_info(fitsname[0])
  if not fi.exists then begin
    message,'NO such file '+fitsname,/cont
    return
  endif
  if keyword_set(verbose) then $
                        message,'reading '+fitsname[0],/info

  data   = readfits(fitsname[0],header,/silen)
  nlines = n_elements(header)
  n0lines= where(header eq 'COMMENT Variables from .idl file:                                               ')
  nflines= where(header eq 'COMMENT Non-uniform z-coordinate                                                ')
  idlparams=strarr(nlines[0]-n0lines[0])
  for il = n0lines[0], nflines[0] -1 do begin
     poseq = strpos(header[il],'=')
     if poseq ge 0 then begin
        poslashrev  = strpos(header[il],'/',/reverse_search)
        poslash     = strpos(header[il],'/')
        varname     = strmid(header[il],poslashrev+1)
        varvalue    = strmid(header[il],poseq,poslash-poseq)
        if where(strtrim(varname) eq '') then begin
           varname     = strmid(header[il],0,poseq)
        endif
        idlparams(il-n0lines[0]) = varname+varvalue
        result      = execute(varname+varvalue)
     endif
  endfor

  if ptr_valid(self.idlparams) then ptr_free,self.idlparams
  self.idlparams=ptr_new(idlparams)
  self.mzb=mz
  if n_elements(mb) eq 0 then mb=5
  self.mb=mb
;
; units
  if n_elements(u_l) ne 0 then self.units.ul=u_l
  if n_elements(u_t) ne 0 then self.units.ut=u_t
  if n_elements(u_r) ne 0 then self.units.ur=u_r
  if n_elements(u_kr) ne 0 then self.units.kr=u_kr
  if n_elements(u_te) ne 0 then self.units.te=u_te
  if n_elements(u_tg) ne 0 then self.units.tg=u_tg
;
  if n_elements(boundarychk) eq 0 then boundarychk=0
  if boundarychk then self.mzb=mz+2*mb
;

  if isnap ne self.isnap and self.isnap ge 0 then $
    message,'the required isnap '+strtrim(string(isnap),2)+' =/ isnap in '+ $
            self->getfilename('.idl'),/info
;
  if n_elements(meshfile) ne 0 then self.meshfile=strtrim(meshfile,2)
  if n_elements(snapname) ne 0 then self.snapname=strtrim(snapname,2)
  if n_elements(dt) ne 0 then self.dt=dt else dt=1.e-30
  if n_elements(gamma) ne 0 then self.gamma=gamma else gamma=5./3.
  if n_elements(t) ne 0 then self.t=t else self.t=0.
  if n_elements(mx) ne 0 then self.mx=mx else self.mx=0
  if n_elements(my) ne 0 then self.my=my else self.my=0
  if n_elements(mz) ne 0 then self.mz=mz else self.mz=0
  if n_elements(nstepstart) ne 0 then self.nstepstart=long(nstepstart) $
  else self.nstepstart=0l
  if n_elements(dx) ne 0 then self.dx=dx else self.dx=0.
  if n_elements(dy) ne 0 then self.dy=dy else self.dy=0.
  if n_elements(dz) ne 0 then self.dz=dz else self.dz=0.
  if n_elements(dt) ne 0 then self.dt=dt else self.dt=0.
  if n_elements(coltablefile) ne 0 then self.coltablefile=strtrim(coltablefile,2) else self.coltablefile=' '
  if n_elements(teff) ne 0 then self.teff=teff else self.teff=0.
  if n_elements(nu1) ne 0 then self.nu1=nu1 else self.nu1=0.
  if n_elements(nu2) ne 0 then self.nu2=nu2 else self.nu2=0.
  if n_elements(nu3) ne 0 then self.nu3=nu3 else self.nu3=0.
  if n_elements(nu_r_xy) ne 0 then self.nu_r_xy=nu_r_xy else self.nu_r_xy=0.
  if n_elements(nu_r) ne 0 then self.nu_r=nu_r else self.nu_r=0.
  if n_elements(nu_ee) ne 0 then self.nu_ee=nu_ee else self.nu_ee=0.
  if n_elements(eta3) ne 0 then self.eta3=eta3 else self.eta3=0.
  if n_elements(ca_max) ne 0 then self.ca_max=ca_max else self.ca_max=0.
  if n_elements(rbot) ne 0 then self.rbot=rbot else self.rbot=0.
  if n_elements(ebot) ne 0 then self.ebot=ebot else self.ebot=0.
  if n_elements(bx0) ne 0 then self.bx0=bx0 else self.bx0=0.
  if n_elements(by0) ne 0 then self.by0=by0 else self.by0=0.
  if n_elements(strtb) ne 0 then self.strtb=strtb else self.strtb=' '
  if n_elements(nu_rm) ne 0 then self.nu_rm=nu_rm else self.nu_rm=0.
  if n_elements(zotb) ne 0 then self.zotb=zotb else self.zotb=0.
  if n_elements(boundarychk) ne 0 then self.boundarychk=boundarychk

  if n_elements(ck_nrcorks) ne 0 then self.ck_nrcorks=ck_nrcorks else self.ck_nrcorks=0LL
  if n_elements(ck_placement) ne 0 then self.ck_placement=ck_placement else self.ck_placement='undefined'
  if n_elements(ck_inject) ne 0 then self.ck_inject=ck_inject else self.ck_inject=-1
  if n_elements(ck_highest_id) ne 0 then self.ck_highest_id=ck_highest_id else self.ck_highest_id=0LL
  if n_elements(ck_relocate) ne 0 then self.ck_relocate=ck_relocate else self.ck_relocate=-1

  aux=strsplit(aux,' ',/extract)
  if n_elements(aux) ne 0 then self.auxpos=ptr_new(intarr(n_elements(aux))) $
                          else self.auxpos=ptr_new(0)
  self.auxvars=ptr_new(aux) 

  if n_elements(tabinputfile) ne 0 then self.tabinputfile=strtrim(tabinputfile,2)
  if n_elements(do_hion) ne 0 then self.do_hion=do_hion else self.do_hion=0
  if n_elements(hionhasoldpops) ne 0 then self.hionhasoldpops=hionhasoldpops else self.hionhasoldpops=0.
  if n_elements(hionatomfile) ne 0 then self.hionatomfile=hionatomfile else self.hionatomfile=0.
  if n_elements(hionneetabfile) ne 0 then self.hionneetabfile=hionneetabfile else self.hionneetabfile=0.

  ;--------ooe--------
  if n_elements(atomfl) ne 0 then self.atomfl=atomfl else self.atomfl=''
  ;-------------------
end

pro br_hdr::mesh

;*
;*  Equidistant mesh calculation [xyz]m and therefore unity Jacobian
;*  d[xyz]id[xyz]dn and d[xyz]id[xyz]up. d[xyz] not set by cread.inc but
;*  defined by specified box sizes s[xyz] in this case.
;*

  nofits=self.nofits
  if nofits eq 1 then message,'Creating a uniform grid with dx, dy, dz :'+ $
                            string(self.dx,format='(1x,f4.2)')+ $
                            string(self.dy,format='(1x,f4.2)')+ $
                            string(self.dz,format='(1x,f4.2)'),/info
  if self.dx eq 0.0 then self.dx=1.
  if self.dy eq 0.0 then self.dy=1.
  if self.dz eq 0.0 then self.dz=1.
; x axis
  mx=self.mx
  self.xm=ptr_new(findgen(mx)*self.dx)
  self.xmdn=ptr_new(*(self.xm)-0.5*self.dx)
  self.dxidxup=ptr_new(fltarr(mx)+1./self.dx)
  self.dxidxdn=ptr_new(fltarr(mx)+1./self.dx)
  self.dx1d=ptr_new(fltarr(mx)+self.dx)
; y axis
  my=self.my
  self.ym=ptr_new(findgen(my)*self.dy)
  self.ymdn=ptr_new(*(self.ym)-0.5*self.dy)
  self.dyidyup=ptr_new(fltarr(my)+1./self.dy)
  self.dyidydn=ptr_new(fltarr(my)+1./self.dy)
  self.dy1d=ptr_new(fltarr(my)+self.dy)
; z axis
  mz=self.mz
  self.zm=ptr_new(findgen(mz)*self.dz)
  self.zmdn=ptr_new(*(self.zm)-0.5*self.dz)
  self.dzidzup=ptr_new(fltarr(mz)+1./self.dz)
  self.dzidzdn=ptr_new(fltarr(mz)+1./self.dz)
  self.dz1d=ptr_new(fltarr(mz)+self.dz)
;
  init_stagger,self.mx,self.my,self.mz,(*self.dx1d)[0],(*self.dy1d)[0],(*self.dz1d)[0]
end

pro br_hdr::readmesh,nofits=nofits,isnap=isnap

  if n_elements(nofits) eq 0 then nofits=self.nofits

  if nofits eq 1 then begin
     self->readdatamesh
  endif else begin
     isnap=self.isnap
     self->readfitsmesh,isnap=isnap
  endelse

end

pro br_hdr::readfitsmesh,isnap=isnap
 
  self->readparams,isnap,nofits=self.nofits
  self->mesh
  fitsname=self->getfitsfilename('.fits',isnap=isnap)
  z = readfits(fitsname[0],header,ext=1,/silen)
  self.zm   = ptr_new(z)
  self.zmdn = ptr_new(z)
  
  dz = z - shift(z,-1)
  dz(n_elements(dz)-1) = dz(n_elements(dz)-2)
  self.dzidzup = ptr_new(dz)
  self.dzidzdn = ptr_new(dz)
  self.dz1d    = ptr_new(dz)
  
end                             


pro br_hdr::readdatamesh,meshfile=meshfile
  if n_elements(meshfile) eq 0 then meshfile=self.meshfile
  if not (file_info(meshfile)).exists then begin
    message,'no such file '+meshfile,/info
    self->mesh
    zh=([*self.zm,*self.zmdn])[sort([*self.zm,*self.zmdn])]
    if self.mz gt 1 then init_stagger,self.mx,self.my,self.mz,(*self.dx1d)[0],(*self.dy1d)[0],zh
    return
 endif
  message,'reading '+meshfile,/info
  openr,lu,meshfile,/get_lun
; read x axis
  mx=0 & my=0 & mz=0
  readf,lu,mx 
  self.mx=mx
  self.xm=ptr_new(fltarr(mx))
  self.xmdn=ptr_new(fltarr(mx))
  self.dxidxup=ptr_new(fltarr(mx))
  self.dxidxdn=ptr_new(fltarr(mx))
  self.dx1d=ptr_new(fltarr(mx))
  readf,lu,*self.xm,*self.xmdn,*self.dxidxup,*self.dxidxdn
  if mx gt 1 then begin
    *self.dx1d=shift(*self.xm,-1)-*self.xm
    (*self.dx1d)[mx-1]=(*self.xm)[mx-1]-(*self.xm)[mx-2]
  endif else begin
    *self.dx1d=1.0
  endelse
; read y axis
  readf,lu,my 
  self.my=my
  self.ym=ptr_new(fltarr(my))
  self.ymdn=ptr_new(fltarr(my))
  self.dyidyup=ptr_new(fltarr(my))
  self.dyidydn=ptr_new(fltarr(my))
  self.dy1d=ptr_new(fltarr(my))
  readf,lu,*self.ym,*self.ymdn,*self.dyidyup,*self.dyidydn
  if my gt 1 then begin
    *self.dy1d=shift(*self.ym,-1)-*self.ym
    (*self.dy1d)[my-1]=(*self.ym)[my-1]-(*self.ym)[my-2]
  endif else begin
    *self.dy1d=1.0
  endelse
; read z axis
  readf,lu,mz 
  self.mz=mz
  if self.mzb eq 0 then self.mzb=mz
  self.zm=ptr_new(fltarr(mz))
  self.zmdn=ptr_new(fltarr(mz))
  self.dzidzup=ptr_new(fltarr(mz))
  self.dzidzdn=ptr_new(fltarr(mz))
  self.dz1d=ptr_new(fltarr(mz))
  readf,lu,*self.zm,*self.zmdn,*self.dzidzup,*self.dzidzdn
  if mz gt 1 then begin
     *self.dz1d=shift(*self.zm,-1)-*self.zm
     (*self.dz1d)[mz-1]=(*self.zm)[mz-1]-(*self.zm)[mz-2]
  endif else begin
    *self.dz1d=1.0
    self.mz=0
  endelse
  free_lun,lu
; prepare the use of the stagger functions - assuming that 
;  if mz gt 1 then begin
     zh=([*self.zm,*self.zmdn])[sort([*self.zm,*self.zmdn])]
;  endif else begin
;     zh=[1.0,0.5];([*self.zm,*self.zmdn])[sort([*self.zm,*self.zmdn])]
;  endelse
  init_stagger,self.mx,self.my,self.mz,(*self.dx1d)[0],(*self.dy1d)[0],zh
end                             

pro br_hdr::display
  nlines=n_elements(self->getidlparams())
  for i=0,nlines-1 do begin
    print,(self->getidlparams())[i]
  endfor
end

pro br_hdr::writeidlparams,idlparam,isnap=isnap
  if n_params() eq 0 then idlparam=self->getfilename('.idl')
  ic=strpos(idlparam,'.idl')
  if ic eq -1 then begin
     isnapex=stregex(idlparam, '[0123456789][0123456789][0123456789]')
     if isnapex eq -1 then begin
        if n_elements(isnap) eq 0 then isnap=string3(self->getisnap())
        file=idlparam+'_'+isnap+'.idl' 
     endif else begin 
        file=idlparam+'.idl' 
     endelse
  endif else begin
     file=idlparam
  endelse
  openw,lu,file,/get
  nlines=n_elements(self->getidlparams())
  for i=0,nlines-1 do begin
    printf,lu,(self->getidlparams())[i]
  endfor
  close,lu
  free_lun,lu
end

function br_hdr::getidlparams
  return,*self.idlparams
end

pro br_hdr::setidlparams,idlparams
  *self.idlparams=idlparams
end

pro br_hdr::setisnap,isnap
  self.isnap=isnap
end

pro br_hdr::changeidlparam,param,value,after=after
  if n_elements(after) eq 0 then after=''
  nlines=n_elements(self->getidlparams())
  line=0
  par=strupcase(strtrim(param,2))
  done=0
  repeat begin
    fullpar=(strsplit((self->getidlparams())[line],'=',/extract))[0]
    idlpar=strupcase(strtrim(fullpar,2))
    if(par eq idlpar)then begin
      pvalue=value
      if size(value,/type) eq 7 then pvalue='"'+string(value)+'"'
      (*self.idlparams)[line]=fullpar+'= '+string(pvalue)
      done=1
    endif
    line=line+1
  endrep until done or (line eq nlines)
  if not done and after ne '' then begin
    line=0
    after=strupcase(strtrim(after,2))
    repeat begin
      fullpar=(strsplit((self->getidlparams())[line],'=',/extract))[0]
      idlpar=strupcase(strtrim(fullpar,2))
      newline=string(replicate(32b,15-strlen(param)))+param+' = '
      if idlpar eq after then begin
        newline=newline+string(value)
        idlparams=(self->getidlparams())
        if line lt nlines-1 then begin
          idlparams=[idlparams[0:line],newline,idlparams[line+1:*]]
        endif else idlparams=[idlparams[0:line],newline]
        self->setidlparams,idlparams
        done=1
      endif
      line=line+1
    endrep until done or (line eq nlines)
  endif
  if not done then begin
    message,'could not find parameter/after '+parameter+'/'+after,/info
  endif
end

function br_hdr::gettitle
  return, self.title
end

pro br_hdr::settitle,title
  self.title=title
end

function br_hdr::getver_no
  return, '$Revision: 1.16 $'
end

function br_hdr::getmx
  return,self.mx
end

function br_hdr::getmy
  return,self.my
end

function br_hdr::getmz
  return,self.mz
end

function br_hdr::getmb
  return,self.mb
end

function br_hdr::getnstepstart
  return,self.nstepstart
end

function br_hdr::getmzb
  return,self.mzb
end

pro br_hdr::setmzb,mzb
  self.mzb=mzb
end

function br_hdr::getmvar
  return,self.mvar
end

function br_hdr::getxm
  if ptr_valid(self.xm) then begin
    return,*self.xm
 endif else begin
    message,'no xm defined in object',/info
    return,fltarr(1)-1.0
 endelse
end

function br_hdr::getx,i
  if n_elements(i) eq 0 then return,self->getxm() else return,(self->getxm())[i]
end

function br_hdr::getxmdn
  if ptr_valid(self.xmdn) then begin
    return,*self.xmdn
 endif else begin
    message,'no xmdn defined in object',/info
    return,fltarr(1)-1.0
 endelse
end

function br_hdr::getdxidxup
  if ptr_valid(self.dxidxup) then begin
    return,*self.dxidxup
 endif else begin
    message,'no dxidxup defined in object',/info
    return,fltarr(1)-1.0
 endelse
end

function br_hdr::getdxidxdn
  if ptr_valid(self.dxidxdn) then begin
    return,*self.dxidxdn
 endif else begin
    message,'no dxidxdn defined in object',/info
    return,fltarr(1)-1.0
 endelse
end

function br_hdr::getdx1d
  if ptr_valid(self.dx1d) then begin
    return,*self.dx1d
 endif else begin
    message,'no dx1d defined in object',/info
    return,fltarr(1)-1.0
 endelse
end

function br_hdr::getdx,i
  if n_elements(i) eq 0 then i=0
  return,(self->getdx1d())[i]
end

function br_hdr::getym
  if ptr_valid(self.ym) then begin
    return,*self.ym
 endif else begin
    message,'no ym defined in object',/info
    return,fltarr(1)-1.0
 endelse
end

function br_hdr::gety,i
  if n_elements(i) eq 0 then return,self->getym() else return,(self->getym())[i]
end

function br_hdr::getymdn
  if ptr_valid(self.ymdn) then begin
    return,*self.ymdn
 endif else begin
    message,'no ymdn defined in object',/info
    return,fltarr(1)-1.0
 endelse
end

function br_hdr::getdyidyup
  if ptr_valid(self.dyidyup) then begin
    return,*self.dyidyup
 endif else begin
    message,'no dyidyup defined in object',/info
    return,fltarr(1)-1.0
 endelse
end

function br_hdr::getdyidydn
  if ptr_valid(self.dyidydn) then begin
    return,*self.dyidydn
 endif else begin
    message,'no dyidydn defined in object',/info
    return,fltarr(1)-1.0
 endelse
end

function br_hdr::getdy1d
  if ptr_valid(self.dy1d) then begin
    return,*self.dy1d
 endif else begin
    message,'no dy1d defined in object',/info
    return,fltarr(1)-1.0
 endelse
end

function br_hdr::getdy,i
  if n_elements(i) eq 0 then i=0
  return,(self->getdy1d())[i]
end

function br_hdr::getzm
  if ptr_valid(self.zm) then begin
    if self.mz eq self.mzb then begin
      return,*self.zm
   endif else begin
      mb=self.mb
      mz=self.mz
      ztop=fltarr(mb)
      zbot=fltarr(mb)
      for j=1,mb do begin
        zbot[mb-j]= (*self.zm)[0]-j*((*self.zm)[1]-(*self.zm)[0])
        ztop[j-1] =(*self.zm)[mz-1]+j*((*self.zm)[mz-1]-(*self.zm)[mz-2])
      endfor
      return,[zbot,*self.zm,ztop]
    endelse
 endif else begin
    message,'no zm defined in object',/info
    return,fltarr(1)-1.0
 endelse
end

function br_hdr::getz,i
  if n_elements(i) eq 0 then return,self->getzm() else return,(self->getzm())[i]
end

function br_hdr::getzh
  if ptr_valid(self.zm) then begin
    return,([self->getzm(),self->getzmdn()])[sort([self->getzm(),self->getzmdn()])]
  endif else begin
    message,'no zh defined in object',/info
    return,fltarr(1)-1.0
  endelse
end

function br_hdr::getzmdn
  if ptr_valid(self.zmdn) then begin
    if self.mz eq self.mzb then begin
      return,*self.zmdn
    endif else begin
      mb=self.mb
      mz=self.mz
      ztop=fltarr(mb)
      zbot=fltarr(mb)
      for j=1,mb do begin
        zbot[mb-j]= (*self.zmdn)[0]-j*((*self.zmdn)[1]-(*self.zmdn)[0])
        ztop[j-1] =(*self.zmdn)[mz-1]+j*((*self.zmdn)[mz-1]-(*self.zmdn)[mz-2])
      endfor
      return,[zbot,*self.zmdn,ztop]
    endelse
 endif else begin
    message,'no zmdn defined in object',/info
    return,fltarr(1)-1.0
 endelse
end

function br_hdr::getdzidzup
  if ptr_valid(self.dzidzup) then begin
    if self.mz eq self.mzb then begin
      return,*self.dzidzup
    endif else begin
      mb=self.mb
      mz=self.mz
      ztop=fltarr(mb)+(*self.dzidzup)[mz-1]
      zbot=fltarr(mb)+(*self.dzidzup)[0]
      return,[zbot,*self.dzidzup,ztop]
    endelse
 endif else begin
    message,'no dzidzup defined in object',/info
    return,fltarr(1)-1.0
 endelse
end

function br_hdr::getdzidzdn
  if ptr_valid(self.dzidzdn) then begin
    if self.mz eq self.mzb then begin
      return,*self.dzidzdn
    endif else begin
      mb=self.mb
      mz=self.mz
      ztop=fltarr(mb)+(*self.dzidzdn)[mz-1]
      zbot=fltarr(mb)+(*self.dzidzdn)[0]
      return,[zbot,*self.dzidzdn,ztop]
    endelse
 endif else begin
    message,'no dzidzdn defined in object',/info
    return,fltarr(1)-1.0
 endelse
end

function br_hdr::getdz1d
  if ptr_valid(self.dz1d) then begin
    if self.mz eq self.mzb then begin
      return,*self.dz1d
    endif else begin
      mb=self.mb
      mz=self.mz
      ztop=fltarr(mb)+(*self.dz1d)[mz-1]
      zbot=fltarr(mb)+(*self.dz1d)[0]
      return,[zbot,*self.dz1d,ztop]
    endelse
 endif else begin
    message,'no dz1d defined in object',/info
    return,fltarr(1)-1.0
 endelse
end
 
function br_hdr::getdz,i
  if n_elements(i) eq 0 then i=0
  return,(self->getdz1d())[i]
end

function br_hdr::getparamsfile
  return,self.paramsfile
end

function br_hdr::getidlpar
  return,self->getparamsfile()
end

pro br_hdr::setparamsfile,paramsfile
  self.paramsfile=paramsfile
end

pro br_hdr::setfitsfile,fitsname
  self.fitsname=fitsname
end

function br_hdr::getfilename,filetype,isnap=isnap,mf_ifluid=mf_ifluid
  if n_elements(filetype) eq 0 then begin
    message,'no filetype given'
  endif
  if n_elements(isnap) eq 0 then isnap=self->getisnap()
  if n_elements(mf_nfluid) eq 0 then mf_nfluid=self->getmf_nfluid()
  if mf_nfluid ge 2 then begin
     if filetype eq '.snap' or filetype eq '.snap.scr' or filetype eq '.snap.panic' then begin
        if n_elements(mf_ifluid) eq 0 then mf_ifluid=self->setmf_ifluid(mf_ifluid=mf_ifluid)
        mfsnapname='_mf_'+strtrim(string(mf_ifluid),2)+'_'
     endif else begin
        mfsnapname=''
     endelse
  endif else begin
     mfsnapname=''
  endelse
  if self->getscr() then return,self->getsnapname()+mfsnapname+filetype+'.scr'
  if filetype eq '.panic' then return,self->getsnapname()+mfsnapname+filetype
  if isnap eq 0 then begin
    file=self->getsnapname()+mfsnapname+'_000'+filetype              ; new standard
    dum=file_search(file,count=count)
    if(count eq 0) then file=self->getsnapname()+mfsnapname+filetype ; old standard without _000
    return,file
  endif else begin
    if mfsnapname eq '' then mfsnapname='_'
    file=self->getsnapname()+mfsnapname+br_string3(abs(isnap))+filetype+''           ; new standard
    dum=file_search(file,count=count)
    if mfsnapname eq '_' then mfsnapname=''
    if(count eq 0) then file=self->getsnapname()+mfsnapname+br_string3(abs(isnap))+filetype+'' ; old standard without '_'
    return,file
  endelse
end

function br_hdr::getsnapname
  return,self.snapname
end

function br_hdr::getcoltablefile
  return,self.coltablefile
end

function br_hdr::getfitsfilename,filetype,isnap=isnap
  if n_elements(filetype) eq 0 then begin
     message,'no filetype given'
  endif
  if n_elements(isnap) eq 0 then isnap=self->getisnap()

  if isnap eq 0 then begin
    file=file_search(self.paramsfile+'*_000'+filetype,count=count)        ; new standard
    if(count eq 0) then begin
       message,'no file called ' + self.paramsfile+'*_000'+filetype
    endif
    return,file
  endif else begin
    file=file_search(self.paramsfile+'*_'+br_string3(abs(isnap))+filetype,count=count)           ; new standard
    if(count eq 0) then begin
       message,'no file called ' + self.paramsfile+'*_'+br_string3(abs(isnap))+filetype
    endif
    return,file
  endelse
end

function br_hdr::getsnapvars
  return,self.snapvars
end

function br_hdr::getparamsfile
  return,self.paramsfile
end


function br_hdr::getmeshfile
  return,self.meshfile
end

function br_hdr::getsnappos,p
  return, self.snappos[p]
end

function br_hdr::getauxvars
  return,*self.auxvars
end

function br_hdr::getnofits,nofits=nofits
  return, self.nofits
end

function br_hdr::setnofits,nofits=nofits
  if n_elements(nofits) ne 0 then begin
     if nofits eq 1 then begin
        if (file_search(self.paramsfile+'*.idl'))[0] eq '' then begin
           message,'No idlparams files, trying with fits',/info
           self.nofits=0
        endif else begin
           self.nofits=nofits
        endelse
     endif else begin
        if (file_search(self.paramsfile+'*.fits'))[0] eq '' then begin
           self.nofits=1
        endif else begin
           self.nofits=nofits
        endelse
     endelse
  endif else begin
     if (file_search(self.paramsfile+'*.fits'))[0] eq '' then begin
        self.nofits=1
     endif else begin
        self.nofits=0
     endelse
  endelse
  return, self.nofits
end

function br_hdr::getrootfitsname
  return, self.rootfitsname
end

function br_hdr::getauxvars1dx
  return,self.auxvars1dx
end

function br_hdr::getauxpos,p
  return,*self.auxpos[p]
end

function br_hdr::getisnap
  return,self.isnap
end

function br_hdr::getscr
  return,self.scr
end

function br_hdr::getdt
  return,self.dt
end

;; function br_hdr::getdx
;;   return,self.dx
;; end

;; function br_hdr::getdy
;;   return,self.dy
;; end

;; function br_hdr::getdz
;;   return,self.dz
;; end

function br_hdr::gettsnap
  return,self.t
end

function br_hdr::getmf_nfluid
  mf_nfluid=self.mf_nfluid
  if mf_nfluid lt 1 then mf_nfluid=1
  return,mf_nfluid
end

function br_hdr::getmf_epf
  return,self.mf_epf
end

function br_hdr::getdtsnap
  return,self.dtsnap
end

function br_hdr::getcorkparams

  corkparams=create_struct( $
             'ncorks',self.ck_nrcorks, $
             'placement',self.ck_placement, $
             'inject',self.ck_inject, $
             'highest_id',self.ck_highest_id, $
             'relocate',self.ck_relocate,$
             'aux',self.ck_auxvars)
  
  return,corkparams
end

function br_hdr::getck_nrcorks
  return,self.ck_nrcorks
end

function br_hdr::getck_placement
  return,self.ck_placement
end

function br_hdr::getck_inject
  return,self.ck_inject
end

function br_hdr::getck_highest_id
  return,self.ck_highest_id
end

function br_hdr::getck_relocate
  return,self.ck_relocate
end

function br_hdr::getboundarychk
  return,self.boundarychk
end

function br_hdr::getgamma
  return,self.gamma
end

function br_hdr::getunits
  self.units.uu=self.units.ul/self.units.ut
  self.units.up=self.units.ur*self.units.uu^2
  self.units.uee=self.units.uu^2
  self.units.ue=self.units.ur*self.units.uu^2
  self.units.mu=0.63
  self.units.bk=1.380658d-16
  self.units.mh=1.6726219d-24
  self.units.mm=self.units.mu*self.units.mh
  self.units.ub=self.units.uu*sqrt(4.*!pi*self.units.ur)
  self.units.re = 8.31e7
  self.units.cl = 2.998e10               ; cm/s
  self.units.qe = 4.80321d-10
  self.units.me = 9.10953d-28
  return,self.units
end

function br_hdr::gettabinputfile
  return,self.tabinputfile
end

pro br_hdr::readtabparam
  if self->gettabinputfile() eq '' then begin
    message,'no tabparam file name given',/info
    message,'loading scratch header to find file name',/info
    self->readparams
  endif
  (self->getaux())->readtabparam,self->gettabinputfile()
end

function br_hdr::gettabgrph
  if (self->getaux())->tabparamload() eq 0 then self->readtabparam
  return,(self->getaux())->gettabgrph()
end

function br_hdr::gettabelements
  if (self->getaux())->tabparamload() eq 0 then self->readtabparam
  return,(self->getaux())->gettabelements()
end

function br_hdr::gettababund
  if (self->getaux())->tabparamload() eq 0 then self->readtabparam
  return,(self->getaux())->gettababund()
end

function br_hdr::getdo_hion
  return,self.do_hion
end

function br_hdr::gethionhasoldpops
  return,self.hionhasoldpops
end

function br_hdr::gethionatomfile
  return,self.hionatomfile
end

function br_hdr::gethionneetabfile
  return,self.hionneetabfile
end

;-------ooe-------------
function br_hdr::getatomfl
  return,self.atomfl
end
;-----------------------

pro br_hdr__define
  struct={br_hdr,title:' ', $
                  ver_no:' ', $
                  meshfile:'mesh.dat', $
                  paramsfile:'mhd', $
                  snapname:'mhd', $
                  fitsname:' ', $
                  nofits:0, $
                  rootfitsname:' ', $
                  mf_nfluid : 1, $
                  mf_ifluid : 1, $
                  mf_epf : 0, $
                  snapvars: ['r', 'px', 'py', 'pz', 'e', 'bx', 'by', 'bz'], $
                  snappos: intarr(8), $
                  auxvars: ptr_new(), $
                  auxpos: ptr_new(), $
                  idlparams: ptr_new(), $
                  isnap:0, $
                  scr:0, $
                  t:0.0, $
                  dt:0.0, $
                  dtsnap:0.0,$
                  coltablefile:' ',$
                  gamma:0.0, $
                  dx:0.0, $
                  dy:0.0, $
                  dz:0.0, $
                  nu1: 0.0, $
                  nu2: 0.0, $
                  nu3: 0.0, $
                  nu_r_xy: 0.0, $
                  nu_r: 0.0, $
                  nu_ee: 0.0, $
                  eta3: 0.0, $
                  ca_max: 0.0, $
                  nu_rm: 0.0, $
                  teff:0.0, $
                  rbot:0.0, $
                  ebot:0.0, $
                  bx0:0.0, $
                  by0:0.0, $
                  strtb:'',$
                  zotb: 0.0, $
                  mx:0, $
                  my:0, $
                  mz:0, $
                  mb:0, $
                  mzb:0, $
                  nstepstart:0l,$
                  mvar:0, $
                  boundarychk:0, $
                  xm:ptr_new(), $
                  xmdn:ptr_new(), $
                  dxidxup:ptr_new(), $
                  dxidxdn:ptr_new(), $
                  dx1d:ptr_new(), $
                  ym:ptr_new(), $
                  ymdn:ptr_new(), $
                  dyidyup:ptr_new(), $
                  dyidydn:ptr_new(), $
                  dy1d:ptr_new(), $
                  zm:ptr_new(), $
                  zmdn:ptr_new(), $
                  dzidzup:ptr_new(), $
                  dzidzdn:ptr_new(), $
                  dz1d:ptr_new(), $
                  units:create_struct(name='units', $
                                  'ul',0.0, $
                                  'ut',0.0, $
                                  'ur',0.0, $
                                  'uu',0.0, $
                                  'up',0.0, $
                                  'uee',0.0, $
                                  'ue',0.0, $
                                  'mu',0.0, $
                                  'bk',0.0, $
                                  'mh',0.0, $
                                  'mm',0.0, $
                                  'me',0.0, $
                                  'kr',0.0, $
                                  'te',0.0, $
                                  're',0.0, $
                                  'cl',0.0, $
                                  'qe',0.0, $
                                  'ub',0.0, $
                                  'tg',0.0), $
                  auxvars1dx: ['bx0t','by0t','xs_bcut','xe_bcut','ys_bcut','ye_bcut' ,'tbt','tbtmax','tbw'], $
                  tabinputfile:'tabparam.in', $
                  do_hion:0, $
                  hionhasoldpops:0, $
                  hionatomfile:' ', $
                  hionneetabfile:' ', $
                  ;---helium-----
                  do_helium:0, $
                  hehasoldpops:0, $
                  heliumatomfile:' ', $
                  heliumeuvfile:' ', $
                  ;-----ooe-------
                  atomfl:' ', $
                  ;---corks-----
                  ck_nrcorks:0LL, $
                  ck_placement:' ', $
                  ck_inject:0, $
                  ck_highest_id: 0LL, $
                  ck_relocate: 0, $
                  ck_auxvars: ptr_new() $
                  }
end


