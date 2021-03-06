;
;+
; NAME:
;       BR_DATA__DEFINE
;
; PURPOSE:
;
; CATEGORY:
;       Hansteen/Wikstol Data analysis SW
;
; CALLING SEQUENCE:
;
;
; INPUTS:
;      None
;
; KEYWORD PARAMETERS:
;
;
; OUTPUTS:
;
; CALLS:
;
;
; COMMON BLOCKS:
;
;
; PROCEDURE:
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;
; $Id$
;
;-
function br_data::init,paramsfile,quiet=quiet,isnap=isnap
  self.title='BR Data'
  self.aux=ptr_new(obj_new('br_aux'))
  self.hdr=ptr_new(obj_new('br_hdr',paramsfile))
  self.allowed_mode=['X','Y','Z','XY','XZ','YZ','XYZ']
  self.nwin=1
  if n_elements(quiet) eq 0 then quiet=0
  if n_elements(paramsfile) eq 0 and not quiet then begin
    f=dialog_pickfile(filter=['*.idl*'])
    fbase=max(strsplit(f,path_sep()))
    paramsfile=strmid(strmid(f,fbase),0,stregex(strmid(f,fbase),('.idl')))
    if stregex(paramsfile,'([0-9]){3}$',/boolean) then $
       paramsfile=strmid(paramsfile,0,stregex(paramsfile,'([0-9]){3}$'))
  endif
; from br_hdr::init...
  self->heliuminit
  self->hioninit
  self->inithdr,paramsfile,isnap=isnap,quiet=quiet,nofits=self.nofits
  self.meshfile='mesh.dat'
;
  return,1
end

function br_data::readvardata,varname,isnap,nofits=nofits,found=found,mf_ifluid=mf_ifluid,zr=zr
forward_function readfits

  found=0
  if n_elements(nofits) eq 0 then nofits=self.nofits
  if n_elementS(zr) eq 0 then zr=-1
  if nofits eq 0 then begin
     fitsname=self.paramsfile+'_'+varname+'_'+strtrim(br_string3(isnap),2)+'.fits'
     fileit=file_info(fitsname)
     if fileit.exists then begin
        message,'reading variable '+ varname + ' from ' + fitsname,/info
        w=readfits(fitsname,header,/silen)
        found=1
     endif
  endif else begin
     p = where(varname eq self->getsnapvars())
     q = where(varname eq self->getauxvars())
     r = where(varname eq self->gethionvars())
     s = where(varname eq self->getheliumvars())
     if p[0] ne -1 or q[0] ne -1 or r[0] ne -1 or s[0] ne -1 then begin
        self.varname=varname
        lup = self->findvar(varname,isnap,mf_ifluid=mf_ifluid)
        if lup.lu ne -1 then begin
           if self->get1d() then begin
              case self->getmode() of
                 'X': buf = assoc(lup.lu,fltarr(self.mx))
                 'Y': buf = assoc(lup.lu,fltarr(self.my))
                 'Z': buf = assoc(lup.lu,fltarr(self.mzb))
              endcase
           w=buf[lup.p]
           endif else begin
              if self->get2d() then begin
                 case self->getmode() of
                    'XY': buf = assoc(lup.lu,fltarr(self.mx,self.my,1))
                    'XZ': buf = assoc(lup.lu,fltarr(self.mx,self.mzb,1))
                    'YZ': buf = assoc(lup.lu,fltarr(self.my,self.mzb,1))
                 endcase
                 w=buf[lup.p]
              endif else begin
                if zr lt 0 then begin
                  buf = assoc(lup.lu,fltarr(self.mx,self.my,self.mzb))
                  w=buf[lup.p]
                endif else begin
                  buf = assoc(lup.lu,fltarr(self.mx,self.my,1))
                  w=buf[lup.p*self.mzb+zr]
                endelse
              endelse
           endelse
           free_lun,lup.lu
           found=1
        endif
     endif
  endelse

  if found eq 0 then return,0 else return,w

end

pro br_data::load,varname,isnap,ix=ix,iy=iy,iz=iz,bin=bin,ionele=ionele, $
  swap=swap,nofits=nofits,rootfitsname=rootfitsname,mf_ifluid=mf_ifluid,ooe=ooe,krc=krc
  if n_params() ne 2 then begin
    message,'data->load,varname,isnap,ix=ix,iy=iy,iz=iz, $',/info
    message,'      swap=swap,nofits=nofits',/info
    return
  endif
  if size(isnap,/type) ne 2 then begin
    message,'isnap variable must be integer type!'
 endif

  if n_elements(nofits) eq 0 then nofits = self->getnofits()
  self.mf_nfluid=self->getmf_nfluid()
  if n_elements(mf_ifluid) eq 0 then mf_ifluid = self->setmf_ifluid(varname=varname)
  if n_elements(rootfitsname) eq 1 then self.rootfitsname = rootfitsname
  nofits=self->setnofits(nofits=nofits)
  self.nofits=nofits
  self->readpars,isnap,nofits=nofits
  mf_ifluid=self->setmf_ifluid(varname=varname,mf_ifluid=mf_ifluid)
  self->set2d,0
  self->set1d,0
;
; Check for 1D data and mode
  d1x=self->getauxvars1dx()
;  for i=1,n_elements(d1x)-1 do qlist=qlist(where(STREGEX(qlist,d1x[i], /FOLD_CASE) eq 0))
  if (where(d1x eq varname))[0] ne -1 then begin
    self->setmode,'X'
    self->set1d,1
  endif
;
; Check for 2D data and mode
  if stregex(varname,'(xy|xz|yz)',/fold) ne -1 then begin
    self->setmode,strmid(varname,stregex(varname,'(xy|xz|yz)',/fold),2)
    self->set2d,1
 endif
;
; else 3D data and mode
  if not self->get2d() and not self->get1d() then begin
     if n_elements(ix) eq 0 and $
        n_elements(iy) eq 0 and $
        n_elements(iz) eq 0 then iy=0
     if isnap[0] lt 0 then begin
        if self->getmy() le 1 then self->setmode,'xz' else self->setmode,'xyz'
     endif else begin
       if n_elements(ix) ne 0 then begin
         if self->getmy() le 1 then self->setmode,'z' else if self->getmz() le 1 then self->setmode,'y' else self->setmode,'yz'
         islice=ix
       endif
       if n_elements(iy) ne 0 then begin
        if self->getmx() le 1 then self->setmode,'z' else if self->getmz() le 1 then self->setmode,'x' else self->setmode,'xz'
        islice=iy
       endif
       if n_elements(iz) ne 0 then begin
         if self->getmx() le 1 then self->setmode,'y' else if self->getmy() le 1 then self->setmode,'x' else self->setmode,'xy'
         islice=iz
       endif
       if n_elements(isnap) eq 1 then begin
         if self->getmy() le 1 then self->setmode,'xz' else begin
           self->setmode,'xyz'
         endelse
       endif
     endelse
  endif
;
  if n_elements(islice) eq 0 then islice=0
  self.islice=islice
  if n_elements(swap) eq 0 then swap=0
  (self->getaux())->setswap,swap
  (self->getaux())->setspos,islice
  aux=self->getaux()
  if ptr_valid(self.w) then ptr_free,self.w
;
  for it=0,n_elements(isnap)-1 do begin
    if varname eq '' then goto,nodata
;
;  initialize data area
    if it eq 0 then begin
      self->readmesh
      xyz=self->getxyz()

      self.w=ptr_new(reform(fltarr(xyz.ix[1]-xyz.ix[0]+1, $
        xyz.iy[1]-xyz.iy[0]+1,xyz.iz[1]-xyz.iz[0]+1,xyz.it[1]-xyz.it[0]+1)))

      if min(self->getboundarychk()) ne max(self->getboundarychk()) then begin
         print, '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
         print, 'Warning some snapshots have the ghost zone and others do not:'
         print, 'removed boundaries!'
         print, '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
      endif
   endif
    if min(self->getboundarychk()) ne max(self->getboundarychk()) then begin
       if (self->getboundarychk())[it] ne 0 then begin
          if self.mzb eq self.mz then begin
             xyz.iz = xyz.iz + self.mb
             self.mzb = self.mz + self.mb*2
          endif
       endif else begin
          if self.mzb gt self.mz then begin
             self.mzb = self.mz
             xyz.iz = xyz.iz - self.mb
          endif
       endelse
    endif
    w = self->readvardata(varname,isnap[it],nofits=nofits,found=found,mf_ifluid=mf_ifluid)
    if found eq 1 then w = reform((w)[xyz.ix[0]:xyz.ix[1],xyz.iy[0]:xyz.iy[1],xyz.iz[0]:xyz.iz[1]])

    if nofits eq 0 then begin
       if not found then found=self->load_fitsbvar(varname,xyz,isnap[it],w)
       if not found then found=self->load_fitsvaror(varname,xyz,isnap[it],w)
       if not found then found=self->load_fitspoynting(varname,xyz,isnap[it],w)
       if not found then found=self->load_fitsothers(varname,xyz,isnap[it],w)
       if not found then found=self->load_fitspart(varname,xyz,isnap[it],w)
       if not found then found=self->load_fitsvectoper(varname,xyz,isnap[it],w)
    endif else begin
       if not found then found=self->load_lookup(varname,xyz,isnap[it],w,bin=bin,ionele=ionele)
       if not found then found=self->load_varor(varname,xyz,isnap[it],w)
       if not found then found=self->load_bvar(varname,xyz,isnap[it],w)
       if not found then found=self->load_jvar(varname,xyz,isnap[it],w)
       if not found then found=self->load_poynting(varname,xyz,isnap[it],w)
       if not found then found=self->load_forces(varname,xyz,isnap[it],w)
       if not found then found=self->load_others(varname,xyz,isnap[it],w,krc=krc,ooe=ooe,nofits=nofits)
       if not found then found=self->load_part(varname,xyz,isnap[it],w)
       if not found then found=self->load_work(varname,xyz,isnap[it],w)
       if not found then found=self->load_vectoper(varname,xyz,isnap[it],w)
       if not found then found=self->load_vorticityeq(varname,xyz,isnap[it],w)
       if not found then found=self->load_genohm(varname,xyz,isnap[it],w)
    endelse
;    if not found then found=self->loadooe(varname,isnap[it])
       if not found then begin  ; no variable found
       if varname ne ' ' then begin
          message,'VARIABLE "'+varname+'" not found',/info
          message,"run the following for help d->load,' ',it",/info
      endif
      return
    endif
;
      if self->getmode() eq 'XYZ' or xyz.it[1] eq 0 then begin
        (*self.w)=w
      endif else begin
            if (self.mx eq 1 and self.my eq 1) then begin
               (*self.w)[*,it]=reform(w)
            endif else begin
               if strmid(self->getmode(),0,1) ne 'X' and self.my eq 1 then begin
                  (*self.w)[*,it]=reform(w)
               endif else begin
                  if strmid(self->getmode(),1,2) eq '' then begin
                     (*self.w)[*,it]=reform(w)
                  endif else begin
                     (*self.w)[*,*,it]=reform(w)
                  endelse
               endelse
            endelse
         endelse
    nodata:
  endfor
  w=0
  case self->getmode() of
  'XYZ': begin
    xw = self.mx
    yw = self.my
    self.nraster=self.mzb
    aux->setxtitle,'x [Mm]'
    aux->setytitle,'y [Mm]'
    aux->setrastertitle,'z [Mm]'
    aux->setstitle,'t [hs]'
    aux->setmode,'xyz'
    if not (file_info(self.meshfile)).exists then begin
       dxw = self.dx
       dyw = self.dy
       x=findgen(xw)*dxw
       y=findgen(yw)*dyw
    endif else begin
       x=self->getxm()
       y=self->getym()
    endelse
         end
  'XY': begin
    xw = self.mx
    yw = self.my
    self.nraster=self->getnsnaps()
    aux->setxtitle,'x [Mm]'
    aux->setytitle,'y [Mm]'
    aux->setrastertitle,'t [hs]'
    aux->setstitle,'z [Mm]'
    aux->setmode,'xy'
    if not (file_info(self.meshfile)).exists then begin
       dxw = self.dx
       dyw = self.dy
       x=findgen(xw)*dxw
       y=findgen(yw)*dyw
    endif else begin
       x=self->getxm()
       y=self->getym()
    endelse
        end
  'XZ': begin
    xw = self.mx
    yw = self.mzb
    self.nraster=self->getnsnaps()
    aux->setxtitle,'x [Mm]'
    aux->setytitle,'z [Mm]'
    aux->setrastertitle,'t [hs]'
    aux->setstitle,'y [Mm]'
    aux->setmode,'xz'
    if not (file_info(self.meshfile)).exists then begin
       dxw = self.dx
       dyw = self.dz
       x=findgen(xw)*dxw
       y=findgen(yw)*dyw
    endif else begin
       x=self->getxm()
       y=self->getzm()
    endelse
        end
  'YZ': begin
    xw = self.my
    yw = self.mzb
    self.nraster=self->getnsnaps()
    aux->setxtitle,'y [Mm]'
    aux->setytitle,'z [Mm]'
    aux->setrastertitle,'t [hs]'
    aux->setstitle,'x [Mm]'
    aux->setmode,'yz'
    if not (file_info(self.meshfile)).exists then begin
       dxw = self.dy
       dyw = self.dz
       x=findgen(xw)*dxw
       y=findgen(yw)*dyw
    endif else begin
       x=self->getym()
       y=self->getzm()
    endelse
 end
  'X': begin
    xw = self.mx
    yw = self->getnsnaps()
    self.nraster=1
    aux->setxtitle,'x [Mm]'
    aux->setytitle,'t [hs]'
    aux->setrastertitle,''
    aux->setstitle,'x [Mm]'
    aux->setmode,'x'
    if not (file_info(self.meshfile)).exists then begin
       dxw = self.dx
       dyw = self.dt
       x=findgen(xw)*dxw
       y=findgen(yw)*dyw
    endif else begin
       x=self->getxm()
       y=self->gett()
    endelse
 end
  'Y': begin
    xw = self.my
    yw = self->getnsnaps()
    self.nraster=1
    aux->setxtitle,'y [Mm]'
    aux->setytitle,'t [hs]'
    aux->setrastertitle,''
    aux->setstitle,'y [Mm]'
    aux->setmode,'y'
    if not (file_info(self.meshfile)).exists then begin
       dxw = self.dy
       dyw = self.dt
       x=findgen(xw)*dxw
       y=findgen(yw)*dyw
    endif else begin
       x=self->getym()
       y=self->gett()
    endelse
 end
  'Z': begin
    xw = self.mz
    yw = self->getnsnaps()
    self.nraster=1
    aux->setxtitle,'z [Mm]'
    aux->setytitle,'t [hs]'
    aux->setrastertitle,''
    aux->setstitle,'z [Mm]'
    aux->setmode,'z'
    if not (file_info(self.meshfile)).exists then begin
       dxw = self.dz
       dyw = self.dt
       x=findgen(xw)*dxw
       y=findgen(yw)*dyw
    endif else begin
       x=self->getzm()
       y=self->gett()
    endelse
  end
  else:
  endcase
  if xw eq 1 then aux->setdispmode,0
  self.xs=ptr_new(0)
  self.xw=ptr_new(xw)
  self.ys=ptr_new(0)
  self.yw=ptr_new(yw)
  aux->setaspect,(max(x)-min(x))/(max(y)-min(y))
  self.ccd_sz = [(max(*(self.xs))+xw),max(*(self.ys))+yw]
end


pro br_data::loadslice,varname,isnap,ix=ix,iy=iy,iz=iz,bin=bin,ionele=ionele, $
  swap=swap,nofits=nofits,rootfitsname=rootfitsname,mf_ifluid=mf_ifluid
  if n_params() ne 2 then begin
    message,'data->loadslice,varname,isnap,ix=ix,iy=iy,iz=iz, $',/info
    message,'      swap=swap,nofits=nofits',/info
    return
  endif
  if size(isnap,/type) ne 2 then begin
    message,'isnap variable must be integer type!'
 endif

  if n_elements(nofits) eq 0 then nofits = self->getnofits()
  self.mf_nfluid=self->getmf_nfluid()
  if n_elements(mf_ifluid) eq 0 then mf_ifluid = self->setmf_ifluid(varname=varname)
  if n_elements(rootfitsname) eq 1 then self.rootfitsname = rootfitsname
  nofits=self->setnofits(nofits=nofits)
  self.nofits=nofits
  self->readpars,isnap,nofits=nofits
  mf_ifluid=self->setmf_ifluid(varname=varname,mf_ifluid=mf_ifluid)
  self->set2d,0
  self->set1d,0
;
; Check for 1D data and mode
  d1x=self->getauxvars1dx()
;  for i=1,n_elements(d1x)-1 do qlist=qlist(where(STREGEX(qlist,d1x[i], /FOLD_CASE) eq 0))
  if (where(d1x eq varname))[0] ne -1 then begin
    self->setmode,'X'
    self->set1d,1
  endif
;
; Check for 2D data and mode
  if stregex(varname,'(xy|xz|yz)',/fold) ne -1 then begin
    self->setmode,strmid(varname,stregex(varname,'(xy|xz|yz)',/fold),2)
    self->set2d,1
 endif
;
; else 3D data and mode
  if not self->get2d() and not self->get1d() then begin
      if n_elements(ix) ne 0 then begin
        if self->getmy() le 1 then self->setmode,'z' else if self->getmz() le 1 then self->setmode,'y' else self->setmode,'yz'
        islice=ix
      endif else begin
        ix=-1
      endelse
      if n_elements(iy) ne 0 then begin
       if self->getmx() le 1 then self->setmode,'z' else if self->getmz() le 1 then self->setmode,'x' else self->setmode,'xz'
       islice=iy
      endif else begin
        iy=-1
      endelse
      if n_elements(iz) ne 0 then begin
        if self->getmx() le 1 then self->setmode,'y' else if self->getmy() le 1 then self->setmode,'x' else self->setmode,'xy'
        islice=iz
      endif else begin
        iz=-1
      endelse
      if n_elements(isnap) eq 1 then begin
        if self->getmy() le 1 then self->setmode,'xz' else begin
          if iz ge 0 then begin
            self->setmode,'xy'
          endif else begin
            if ix ge 0 then begin
              self->setmode,'yz'
            endif else begin
              if iy ge 0 then begin
                self->setmode,'xz'
              endif else begin
                self->setmode,'xyz'
              endelse
            endelse
          endelse
        endelse
      endif else begin
        if ix eq -1 and iy eq -1 and iz eq -1 then begin
          iy=0
          if self->getmx() le 1 then self->setmode,'z' else if self->getmz() le 1 then self->setmode,'x' else self->setmode,'xz'
          islice=iy
        endif
      endelse
  endif
;
  if n_elements(islice) eq 0 then islice=0
  self.islice=islice
  if n_elements(swap) eq 0 then swap=0
  (self->getaux())->setswap,swap
  (self->getaux())->setspos,islice
  aux=self->getaux()
  if ptr_valid(self.w) then ptr_free,self.w
;
  for it=0,n_elements(isnap)-1 do begin
    if varname eq '' then goto,nodata2
;
;  initialize data area
    if it eq 0 then begin
      self->readmesh
      xyz=self->getxyz()
      if iz ge 0 and self->getmy() gt 1 and self->getmx() gt 1 and n_elements(isnap) eq 1 then xyz.iz=[0,0]
      self.w=ptr_new(reform(fltarr(xyz.ix[1]-xyz.ix[0]+1, $
        xyz.iy[1]-xyz.iy[0]+1,xyz.iz[1]-xyz.iz[0]+1,xyz.it[1]-xyz.it[0]+1)))

      if min(self->getboundarychk()) ne max(self->getboundarychk()) then begin
         print, '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
         print, 'Warning some snapshots have the ghost zone and others do not:'
         print, 'removed boundaries!'
         print, '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
      endif
   endif
    if min(self->getboundarychk()) ne max(self->getboundarychk()) then begin
       if (self->getboundarychk())[it] ne 0 then begin
          if self.mzb eq self.mz then begin
             xyz.iz = xyz.iz + self.mb
             self.mzb = self.mz + self.mb*2
          endif
       endif else begin
          if self.mzb gt self.mz then begin
             self.mzb = self.mz
             xyz.iz = xyz.iz - self.mb
          endif
       endelse
    endif
    w = self->readvardata(varname,isnap[it],nofits=nofits,found=found,mf_ifluid=mf_ifluid,zr=iz)
    if found eq 1 then w = reform((w)[xyz.ix[0]:xyz.ix[1],xyz.iy[0]:xyz.iy[1],xyz.iz[0]:xyz.iz[1]])

;    if not found then found=self->loadooe(varname,isnap[it])
    if not found then begin  ; no variable found
       if varname ne ' ' then begin
          message,'VARIABLE "'+varname+'" not found',/info
          message,"run the following for help d->loadslice,' ',it",/info
          message,"this function can read ONLY variable that are saved and does not allow to precompute the variables"
      endif
      return
    endif
;
      if self->getmode() eq 'XYZ' or xyz.it[1] eq 0 then begin
        (*self.w)=w
      endif else begin
            if (self.mx eq 1 and self.my eq 1) then begin
               (*self.w)[*,it]=reform(w)
            endif else begin
               if strmid(self->getmode(),0,1) ne 'X' and self.my eq 1 then begin
                  (*self.w)[*,it]=reform(w)
               endif else begin
                  if strmid(self->getmode(),1,2) eq '' then begin
                     (*self.w)[*,it]=reform(w)
                  endif else begin
                     (*self.w)[*,*,it]=reform(w)
                  endelse
               endelse
            endelse
         endelse
    nodata2:
  endfor
  w=0
  case self->getmode() of
  'XYZ': begin
    xw = self.mx
    yw = self.my
    self.nraster=self.mzb
    aux->setxtitle,'x [Mm]'
    aux->setytitle,'y [Mm]'
    aux->setrastertitle,'z [Mm]'
    aux->setstitle,'t [hs]'
    aux->setmode,'xyz'
    if not (file_info(self.meshfile)).exists then begin
       dxw = self.dx
       dyw = self.dy
       x=findgen(xw)*dxw
       y=findgen(yw)*dyw
    endif else begin
       x=self->getxm()
       y=self->getym()
    endelse
         end
  'XY': begin
    xw = self.mx
    yw = self.my
    self.nraster=self->getnsnaps()
    aux->setxtitle,'x [Mm]'
    aux->setytitle,'y [Mm]'
    aux->setrastertitle,'t [hs]'
    aux->setstitle,'z [Mm]'
    aux->setmode,'xy'
    if not (file_info(self.meshfile)).exists then begin
       dxw = self.dx
       dyw = self.dy
       x=findgen(xw)*dxw
       y=findgen(yw)*dyw
    endif else begin
       x=self->getxm()
       y=self->getym()
    endelse
        end
  'XZ': begin
    xw = self.mx
    yw = self.mzb
    self.nraster=self->getnsnaps()
    aux->setxtitle,'x [Mm]'
    aux->setytitle,'z [Mm]'
    aux->setrastertitle,'t [hs]'
    aux->setstitle,'y [Mm]'
    aux->setmode,'xz'
    if not (file_info(self.meshfile)).exists then begin
       dxw = self.dx
       dyw = self.dz
       x=findgen(xw)*dxw
       y=findgen(yw)*dyw
    endif else begin
       x=self->getxm()
       y=self->getzm()
    endelse
        end
  'YZ': begin
    xw = self.my
    yw = self.mzb
    self.nraster=self->getnsnaps()
    aux->setxtitle,'y [Mm]'
    aux->setytitle,'z [Mm]'
    aux->setrastertitle,'t [hs]'
    aux->setstitle,'x [Mm]'
    aux->setmode,'yz'
    if not (file_info(self.meshfile)).exists then begin
       dxw = self.dy
       dyw = self.dz
       x=findgen(xw)*dxw
       y=findgen(yw)*dyw
    endif else begin
       x=self->getym()
       y=self->getzm()
    endelse
 end
  'X': begin
    xw = self.mx
    yw = self->getnsnaps()
    self.nraster=1
    aux->setxtitle,'x [Mm]'
    aux->setytitle,'t [hs]'
    aux->setrastertitle,''
    aux->setstitle,'x [Mm]'
    aux->setmode,'x'
    if not (file_info(self.meshfile)).exists then begin
       dxw = self.dx
       dyw = self.dt
       x=findgen(xw)*dxw
       y=findgen(yw)*dyw
    endif else begin
       x=self->getxm()
       y=self->gett()
    endelse
 end
  'Y': begin
    xw = self.my
    yw = self->getnsnaps()
    self.nraster=1
    aux->setxtitle,'y [Mm]'
    aux->setytitle,'t [hs]'
    aux->setrastertitle,''
    aux->setstitle,'y [Mm]'
    aux->setmode,'y'
    if not (file_info(self.meshfile)).exists then begin
       dxw = self.dy
       dyw = self.dt
       x=findgen(xw)*dxw
       y=findgen(yw)*dyw
    endif else begin
       x=self->getym()
       y=self->gett()
    endelse
 end
  'Z': begin
    xw = self.mz
    yw = self->getnsnaps()
    self.nraster=1
    aux->setxtitle,'z [Mm]'
    aux->setytitle,'t [hs]'
    aux->setrastertitle,''
    aux->setstitle,'z [Mm]'
    aux->setmode,'z'
    if not (file_info(self.meshfile)).exists then begin
       dxw = self.dz
       dyw = self.dt
       x=findgen(xw)*dxw
       y=findgen(yw)*dyw
    endif else begin
       x=self->getzm()
       y=self->gett()
    endelse
        end
  else:
  endcase
  if xw eq 1 then aux->setdispmode,0
  self.xs=ptr_new(0)
  self.xw=ptr_new(xw)
  self.ys=ptr_new(0)
  self.yw=ptr_new(yw)
  aux->setaspect,(max(x)-min(x))/(max(y)-min(y))
  self.ccd_sz = [(max(*(self.xs))+xw),max(*(self.ys))+yw]
end

function br_data::getxyz
  case self.mode of
  'X': begin
     if self->getmz() gt 1 then begin
        return,{ix:[0,self.mx-1],iy:[0,0],iz:[self.islice,self.islice], $
                it:[0,self->getnsnaps()-1]}
     endif else begin
        return,{ix:[0,self.mx-1],iy:[self.islice,self.islice],iz:[0,0], $
                it:[0,self->getnsnaps()-1]}
     endelse
  end
  'Y': begin
     if self->getmz() gt 1 then begin
        return,{ix:[0,0],iy:[0,self.my-1],iz:[self.islice,self.islice], $
                it:[0,self->getnsnaps()-1]}
     endif else begin
        return,{ix:[self.islice,self.islice],iy:[0,self.my-1],iz:[0,0], $
                it:[0,self->getnsnaps()-1]}
     endelse
  end
  'Z': begin
     if self->getmy() gt 1 then begin
        return,{ix:[0,0],iy:[self.islice,self.islice],iz:[0,self.mz-1], $
                it:[0,self->getnsnaps()-1]}
     endif else begin
        return,{ix:[self.islice,self.islice],iy:[0,0],iz:[0,self.mz-1], $
                it:[0,self->getnsnaps()-1]}
     endelse
  end
  'XY': return,{ix:[0,self.mx-1],iy:[0,self.my-1],iz:[self.islice,self.islice], $
                it:[0,self->getnsnaps()-1]}
  'XZ': return,{ix:[0,self.mx-1],iy:[self.islice,self.islice],iz:[0,self.mzb-1], $
                it:[0,self->getnsnaps()-1]}
  'YZ': return,{ix:[self.islice,self.islice],iy:[0,self.my-1],iz:[0,self.mzb-1], $
                it:[0,self->getnsnaps()-1]}
  'XYZ': return,{ix:[0,self.mx-1],iy:[0,self.my-1],iz:[0,self.mzb-1], $
                it:[0,0]}
  else: begin
;    message,'Illegal mode '+self.getmode(),/info
    return,{ix:-1,iy:-1,iz:-1,it:-1}
        endelse
  endcase
end

function br_data::findvar,varname,isnap,mf_ifluid=mf_ifluid
  p=where(varname eq self->getsnapvars())
  q=where(varname eq self->getauxvars())
  r=where(varname eq self->gethionvars())
  s=where(varname eq self->getheliumvars())

  filetype=''
  extraname=''
  if p[0] ne -1 then begin
    filetype='.snap'
    if isnap eq -9999 then filetype='.panic'
  endif
  if q[0] ne -1 then begin
     filetype='.aux'
     p=q
  endif
  if q[0] ne -1 and self->getmode() eq 'X' then begin
     if  self->getmy() gt 1 or  self->getmz() then filetype='_X.aux'
  endif
  if q[0] ne -1 and self->getmode() eq 'Y' then begin
     if  self->getmx() gt 1 or  self->getmz() then filetype='_Y.aux'
  endif
  if q[0] ne -1 and self->getmode() eq 'Z' then begin
     if  self->getmx() gt 1 or  self->getmy() then filetype='_Z.aux'
  endif

  if filetype ne '' then begin
    if self->get2d() then filetype='_'+self->getmode()+filetype
    mf_ifluid=self->setmf_ifluid(varname=varname,mf_ifluid=mf_ifluid)
    mf_ifluid=self.mf_ifluid
    filename=self->getfilename(filetype,isnap=isnap,mf_ifluid=mf_ifluid)
    if not (file_info(filename)).exists then begin
       message,filename+' not found',/info
       return,{lu:-1,p:-1}
    endif
    message,'reading '+varname+' from '+filename+' isnap '+ $
             strtrim(isnap,2),/info
    openr,lu,filename,/get_lun,swap_endian=(self->getaux())->getswap()
    idvar=self->getsnappos(p)
    if idvar gt self.mvar and filetype eq '.snap' then begin
       message, 'no magnetic field in this data set!',/info
       return,{lu:lu,p:-1}
    endif
  endif else if (r[0] ne -1) then begin
     ; hion variable requested
    p=self->gethionsnappos(r)
    message,'reading '+varname+' from ' $
             +self->gethionsnapname(isnap=isnap)+' isnap '+ $
             strtrim(isnap,2),/info
    openr,lu,self->gethionsnapname(isnap=isnap),/get_lun, $
             swap_endian=(self->getaux())->getswap()
    return,{lu:lu,p:p}
  endif else if (s[0] ne -1) then begin
     ; helium variable requested
     p=self->getheliumsnappos(s)
     filename=self->getheliumsnapname(isnap=isnap)
     if not (file_info(filename)).exists then begin
        message,filename+' not found',/info
        return,{lu:-1,p:-1}
     endif
     message,'reading '+varname+' from '+filename+' isnap '+ $
             strtrim(isnap,2),/info
     openr,lu,filename,/get_lun,swap_endian=(self->getaux())->getswap()
     return,{lu:lu,p:p}
  endif else begin
     message,'no such variable '+varname,/info
     return,{lu:-1,p:-1}
  endelse
  return,{lu:lu,p:p}
end

pro br_data::save, file
end

function br_data::setmf_ifluid,varname=varname,mf_ifluid=mf_ifluid

  mf_epf=self->getmf_epf()

  if mf_epf eq 1 then varl=['r','px','py','pz','e'] else varl=['r','px','py','pz']

  if n_elements(mf_ifluid) eq 0 then mf_ifluid=1
  if mf_ifluid gt self.mf_nfluid or mf_ifluid lt 1 then begin
     message,'mf_ifluid does not match with the number of fluids' + strtrim(string(self.mf_nfluid),2)+', reset to 1',/info
     mf_ifluid=1
  endif
  if where(varl eq varname) ne -1 then begin
     self.mf_ifluid=mf_ifluid
  endif else begin
     self.mf_ifluid=1
  endelse
  return,self.mf_ifluid
end


function br_data::gethome_inst
  return, self.home_inst
end

pro br_data::setxw,xw
  self.xw=xw
end

function br_data::getxw
  return, self.xw
end

function br_data::getxs
  return, self.xs
end

function br_data::getys
  return, self.ys
end

function br_data::getyw
  return, self.yw
end


function br_data::getnexp
  return, self.nexp
end

pro br_data::setnexp,nexp
  self.nexp=nexp
end

function br_data::getnraster
  return, self.nraster
end

function br_data::getntime
  return, self.ntime
end

function br_data::getnwin
  return, self.nwin
end

pro br_data::setnwin, nwin
  self.nwin = nwin
end

function br_data::gettitle
  return, self.title
end

function br_data::getver_no
  return, '$Revision$'
end

function br_data::getcomment
  return, self.comment
end

pro br_data::setcomment, str
  self.comment = str
end

function br_data::getdatasource
  return, self.datasource
end

pro br_data::getwin,win,wd,pos          ; get window win, into wd, position pos on ccd
  wd=*self.w[win]
  pos=[*self.xs[win],*self.xw[win],*self.ys[win],*self.yw[win]]
  return
end

function br_data::setwid, win, wid
  self.wid[win] = wid
end

function br_data::rdvar,isnap,idvar,nofits=nofits
  self->read,isnap,idvar,nofits=nofits
  return,self->getvar(0)
end

function br_data::getvar,win,isnap,ix=ix,iy=iy,iz=iz, $   ; return window win
         swap=swap,nofits=nofits,rootfitsname=rootfitsname,bin=bin,ionele=ionele,mf_ifluid=mf_ifluid,krc=krc,ooe=ooe
  if n_elements(win) eq 0 then begin
    win=0 & iwin=0
 endif
  s=size(win)
  stype=s[s[0]+1]
  if stype eq 7 then begin ; string type
    self->load,win,isnap,ix=ix,iy=iy,iz=iz, $   ; return window win
          swap=swap,nofits=nofits,rootfitsname=rootfitsname,bin=bin,ionele=ionele,mf_ifluid=mf_ifluid,krc=krc,ooe=ooe
    iwin=0
 endif else iwin=win
  if ptr_valid(self.w[iwin]) then return,*self.w[iwin] else return,-1
end


function br_data::getvarslice,win,isnap,ix=ix,iy=iy,iz=iz, $   ; return window win
         swap=swap,nofits=nofits,rootfitsname=rootfitsname,bin=bin,ionele=ionele,mf_ifluid=mf_ifluid
  if n_elements(win) eq 0 then begin
    win=0 & iwin=0
 endif
  s=size(win)
  stype=s[s[0]+1]
  if stype eq 7 then begin ; string type
    self->loadslice,win,isnap,ix=ix,iy=iy,iz=iz, $   ; return window win
          swap=swap,nofits=nofits,rootfitsname=rootfitsname,bin=bin,ionele=ionele,mf_ifluid=mf_ifluid
    iwin=0
 endif else iwin=win
  if ptr_valid(self.w[iwin]) then return,*self.w[iwin] else return,-1
end


function br_data::getexp
  return,*self.exp
end

pro br_data::setupvar,w,win
  self.w[win]=ptr_new(w)
end

pro br_data::setvar,w,win
  if n_elements(win) eq 0 then win=0
  *self.w[win]=w
end

function br_data::gethdr
  return,*self.hdr
end

function br_data::getaux
  return,*self.aux
end

function br_data::getswap
  return,(*self.aux)->getswap()
end

function br_data::setswap,swap
  (*self.aux)->setswap,swap
end

pro br_data::setunit,unit
  self.unit=unit
end

function br_data::getvarname
  return,self.varname
end

pro br_data::setvarname,varname
  self.varname=varname
end

function br_data::getunit,string=string
  if n_elements(string) eq 0 then string=0
  if string then return,(self.unit)[0] else return,self.unit
end

pro br_data::readpars,isnap,nofits=nofits

  if ptr_valid(self.isnaps) then ptr_free,self.isnaps
  self.isnaps=ptr_new(isnap)
  if ptr_valid(self.tsnaps) then ptr_free,self.tsnaps
  self.tsnaps=ptr_new(fltarr(n_elements(isnap)))
  if ptr_valid(self.dts) then ptr_free,self.dts
  self.dts=ptr_new(fltarr(n_elements(isnap)))
;  if ptr_valid(self.coltablefiles) then ptr_free,self.coltablefiles
  self.coltablefiles=ptr_new(strarr(n_elements(isnap)))
  if ptr_valid(self.nstep) then ptr_free,self.nstep
  self.nstep=ptr_new(lonarr(n_elements(isnap)))
  if ptr_valid(self.teffs) then ptr_free,self.teffs
  self.teffs=ptr_new(fltarr(n_elements(isnap)))
  if ptr_valid(self.boundarychks) then ptr_free,self.boundarychks
  self.boundarychks=ptr_new(fltarr(n_elements(isnap)))
  if ptr_valid(self.nu1s) then ptr_free,self.nu1s
  self.nu1s=ptr_new(fltarr(n_elements(isnap)))
  if ptr_valid(self.nu2s) then ptr_free,self.nu2s
  self.nu2s=ptr_new(fltarr(n_elements(isnap)))
  if ptr_valid(self.nu3s) then ptr_free,self.nu3s
  self.nu3s=ptr_new(fltarr(n_elements(isnap)))
  if ptr_valid(self.nu_r_xys) then ptr_free,self.nu_r_xys
  self.nu_r_xys=ptr_new(fltarr(n_elements(isnap)))
  if ptr_valid(self.nu_rs) then ptr_free,self.nu_rs
  self.nu_rs=ptr_new(fltarr(n_elements(isnap)))
  if ptr_valid(self.nu_ees) then ptr_free,self.nu_ees
  self.nu_ees=ptr_new(fltarr(n_elements(isnap)))
  if ptr_valid(self.eta3s) then ptr_free,self.eta3s
  self.eta3s=ptr_new(fltarr(n_elements(isnap)))
  if ptr_valid(self.ca_maxs) then ptr_free,self.ca_maxs
  self.ca_maxs=ptr_new(fltarr(n_elements(isnap)))
  if ptr_valid(self.rbots) then ptr_free,self.rbots
  self.rbots=ptr_new(fltarr(n_elements(isnap)))
  if ptr_valid(self.ebots) then ptr_free,self.ebots
  self.ebots=ptr_new(fltarr(n_elements(isnap)))
  if ptr_valid(self.bx0s) then ptr_free,self.bx0s
  self.bx0s=ptr_new(fltarr(n_elements(isnap)))
  if ptr_valid(self.by0s) then ptr_free,self.by0s
  self.by0s=ptr_new(fltarr(n_elements(isnap)))
  if ptr_valid(self.strtbs) then ptr_free,self.strtbs
  self.strtbs=ptr_new(strarr(n_elements(isnap)))
  if ptr_valid(self.zotbs) then ptr_free,self.zotbs
  self.zotbs=ptr_new(fltarr(n_elements(isnap)))
;  if ptr_valid(self.coltablefile) then ptr_free,self.coltablefiles
;  self.coltablefiles=ptr_new(strarr(n_elements(isnap)))
  for it=0,n_elements(isnap)-1 do begin
    self->readparams,isnap[it],paramsfile=paramsfile,nofits=nofits
    (*self.tsnaps)[it]=self.t
    (*self.dts)[it]=self.dt
    (*self.coltablefiles)[it]=self.coltablefile
    (*self.nstep)[it]=self.nstepstart
    (*self.teffs)[it]=self.teff
    (*self.boundarychks)[it]=self.boundarychk
    (*self.nu1s)[it]=self.nu1
    (*self.nu2s)[it]=self.nu2
    (*self.nu3s)[it]=self.nu3
    (*self.nu_r_xys)[it]=self.nu_r_xy
    (*self.nu_rs)[it]=self.nu_r
    (*self.nu_ees)[it]=self.nu_ee
    (*self.eta3s)[it]=self.eta3
    (*self.ca_maxs)[it]=self.ca_max
    (*self.rbots)[it]=self.rbot
    (*self.ebots)[it]=self.ebot
    (*self.bx0s)[it]=self.bx0
    (*self.by0s)[it]=self.by0
    (*self.strtbs)[it]=self.strtb
    (*self.zotbs)[it]=self.zotb
;    (*self.coltablefiles)[it]=self.coltablefile
  endfor
end

pro br_data::setmode,mode
  mode=strupcase(strtrim(mode,2))
  if (where(mode eq self.allowed_mode))[0] eq -1 then begin
    message,mode+' is not allowed; use one of '+self.allowed_mode,/info
    return
  endif
  self.mode=mode
end

function br_data::getmode
  return,self.mode
end

pro br_data::setislice,islice
  self.islice=islice
end

function br_data::getislice
  return,self.islice
end

function br_data::getisnaps
  return,*self.isnaps
end

function br_data::getnstep
  return,*self.nstep
end

function br_data::getnsnaps
  if ptr_valid(self.isnaps) then return,n_elements(*self.isnaps) else return,-1
end

function br_data::getccd_sz
  return, self.ccd_sz
end

function br_data::gett,i
  if n_elements(i) eq 0 then return,*self.tsnaps else return,(*self.tsnaps)[i]
end

function br_data::getdt
  return,*self.dts
end

function br_data::getcoltablefile
  return,*self.coltablefiles
end

function br_data::getteff
  return,*self.teffs
end

function br_data::getnu1
  return,*self.nu1s
end

;function br_data::getcoltablefile
;  return,*self.coltablefiles
;end

function br_data::getboundarychk
  return,*self.boundarychks
end

function br_data::getnu2
  return,*self.nu2s
end

function br_data::getnu3
  return,*self.nu3s
end

function br_data::getnu_r_xy
  return,*self.nu_r_xys
end

function br_data::getnu_r
  return,*self.nu_rs
end

function br_data::getnu_ee
  return,*self.nu_ees
end

function br_data::geteta3
  return,*self.eta3s
end

function br_data::getca_max
  return,*self.ca_maxs
end

function br_data::getrbot
  return,*self.rbots
end

function br_data::getebot
  return,*self.ebots
end

function br_data::getbx0
  return,*self.bx0s
end

function br_data::getby0
  return,*self.by0s
end

function br_data::getstrtb
  return,*self.strtb
end

function br_data::getzotb
  return,*self.zotbs
end

function br_data::get2d
  return,self.twod
end

pro br_data::set2d,twod
  self.twod=twod
end

function br_data::get1d
  return,self.oned
end

pro br_data::set1d,oned
  self.oned=oned
end

function br_data::getauxvars,all=all
  if n_elements(all) eq 0 then all=0
  if all then return,*self.auxvars
  auxvar=*self.auxvars
  mode=self->getmode()
  d1x=self->getauxvars1dx()
  if self->get1d() then begin
     for i=0,n_elements(auxvar)-1 do begin
        if (where(STREGEX(d1x,auxvar[i], /FOLD_CASE) eq 0))[0] ne -1 then begin
           if n_elements(auxvar1) eq 0 then  auxvar1=auxvar(i) else auxvar1=[auxvar1,auxvar(i)]
        endif
     endfor
        return,auxvar1
  endif else begin
     auxvar=auxvar(where(STREGEX(auxvar,d1x[0], /FOLD_CASE) eq -1))
     for i=1,n_elements(d1x)-1 do auxvar=auxvar(where(STREGEX(auxvar,d1x[i], /FOLD_CASE) eq -1))
     if self->get2d() then begin
        return,auxvar[where(stregex(auxvar,mode,/bool,/fold))]
     endif else begin
        return,auxvar[where(stregex(auxvar,'(xy|xz|yz)',/bool,/fold) eq 0)]
     endelse
  endelse
end

pro br_data::cleanup
  for i=0,self.nwin-1 do ptr_free,self.w[i]
  ptr_free,self.exp
  ptr_free,self.xs
  ptr_free,self.xw
  ptr_free,self.ys
  ptr_free,self.yw
end

function br_data::hav,i,var=var
  if n_elements(var) eq 0 then var=self->getvar()
  sz=size(var)
  case sz[0] of
  2: if n_elements(i) eq 0 then return,total(var,1)/sz[1] $
                           else return,(total(var,1)/sz[1])[i]
  3: if n_elements(i) eq 0 then return,total(total(var,1),1)/sz[1]/sz[2] $
                           else return,(total(total(var,1),1))[i]/sz[1]/sz[2]
  endcase
end

function br_data::hstd,i,var=var
  if n_elements(var) eq 0 then var=self->getvar()
  sz=size(var)
  case sz[0] of
  2: if n_elements(i) eq 0 then begin
       hav=rebin(reform(total(var,1)/sz[1],1,sz[2]),sz[1],sz[2])
       return,sqrt(total((var-hav)^2,1)/sz[1])
     endif else begin
       hav=rebin(reform(total(var,1)/sz[1],1,sz[2]),sz[1],sz[2])
       return,(sqrt(total((var-hav)^2,1)/sz[1]))[i]
     endelse
  3: if n_elements(i) eq 0 then begin
       hav=rebin(reform(total(total(var,1),1)/sz[1]/sz[2],1,1,sz[3]),sz[1],sz[2],sz[3])
       return,sqrt(total(total((var-hav)^2,1),1)/sz[1]/sz[2])
     endif else begin
       hav=rebin(reform(total(total(var,1),1)/sz[1]/sz[2],1,1,sz[3]),sz[1],sz[2],sz[3])
       return,(sqrt(total(total((var-hav)^2,1),1)/sz[1]/sz[2]))[i]
     endelse
  endcase
end

function br_data::hmed,i,var=var
  if n_elements(var) eq 0 then var=self->getvar()
  sz=size(var)
  case sz[0] of
  2: if n_elements(i) eq 0 then return,median(var,1) $
                           else return,(median(var,1))[i]
  3: begin
       if n_elements(i) eq 0 then begin
         tmp=fltarr(sz[3])
         for j=0,sz[3]-1 do begin
           tmp[j]=median(var[*,*,j])
        endfor
        return,tmp
       endif else return,median(var[*,*,i])
     end
  endcase
end

function br_data::h1av,i
  sz=size(self->getvar())
  if n_elements(i) eq 0 then return,total(self->getvar(),1)/sz[1] $
                        else return,(total(self->getvar(),1)/sz[1])[i,*]
end

function br_data::hmax
  sz=size(self->getvar(0))
  dum=fltarr(sz[3])
  for k=0,sz[3]-1 do begin
    dum[k]=max((self->getvar())[*,*,k])
  endfor
  return,dum
end

function br_data::hmin
  sz=size(self->getvar())
  dum=fltarr(sz[3])
  for k=0,sz[3]-1 do begin
    dum[k]=min((self->getvar())[*,*,k])
  endfor
  return,dum
end

pro br_data::getfield,it,bx,by,bz
  nofits=self.nofits
  self->load,'bx',it,nofits=nofits
  bx=self->getvar()
  self->load,'by',it,nofits=nofits
  by=self->getvar()
  self->load,'bz',it,nofits=nofits
  bz=self->getvar()
end

pro br_data::help,var,_extra=ex
  help,self,/obj,_extra=ex
end

pro br_data__define
  struct={br_data, $
          comment:'', $
          home_inst:'', $
          datasource:'', $
          varname:'', $
          hdr:ptr_new(),$ ; This definition prevents the dangling pointer
          aux:ptr_new(),$ ; This definition prevents the dangling pointer
          nexp:0UL, $
          nraster:0, $
          ntime:0,  $
          nwin:0, $
          exp:ptr_new(), $
          isnaps:ptr_new(), $
          tsnaps:ptr_new(), $
          coltablefiles:ptr_new(),$
          dts:ptr_new(), $
          nstep:ptr_new(), $
          teffs:ptr_new(), $
          boundarychks:ptr_new(), $
          nu1s:ptr_new(), $
;          coltablefiles:ptr_new(), $
          nu2s:ptr_new(), $
          nu3s:ptr_new(), $
          nu_r_xys:ptr_new(), $
          nu_rs:ptr_new(), $
          nu_ees:ptr_new(), $
          eta3s:ptr_new(), $
          ca_maxs:ptr_new(), $
          rbots:ptr_new(), $
          ebots:ptr_new(), $
          bx0s:ptr_new(), $
          by0s:ptr_new(), $
          strtbs:ptr_new(), $
          zotbs:ptr_new(), $
          xs:ptr_new(),  $
          xw:ptr_new(),  $
          ys:ptr_new(),  $
          yw:ptr_new(),  $
          w:ptr_new(), $
          wid:ptr_new(), $
          ccd_sz:intarr(2), $
          unit:'', $
          mode:'', $
          allowed_mode:strarr(7), $
          islice:0, $
          twod:0, $
          oned:0, $
          ooeatomf:'', $ ;; This is for the ooe
          ooevars: 0 , $ ;; This is for the ooe
          inherits br_helium, $
          inherits br_hion,$
          inherits br_hdr $
         }
end
