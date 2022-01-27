;
;+
; NAME:
;       BR_BFIELDLINE
;
; PURPOSE:
;      Computes field lines base on randomly placing seeds in a
;      (constrained) region utilizing Chae's br_bline routines.
;
; CATEGORY:
;      Bifrost SW
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
;      br_bline routine
;      br_stretch
;
; COMMON BLOCKS:
;
; PROCEDURE:
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;
; $Id$
;-
;
pro br_bfieldline,idlparam,it,r1,r2,nr1,nr2,r0=r0,vname=vname, $
               zorig=zorig,nline=nline,swap=swap,enlarge=enlarge,mfield=mfield, $
               zst=zst,bxst=bxst,byst=byst,bzst=bzst,islice=islice,mode=mode,deltas=deltas, $
               zmax=zmax,linedir=linedir;,niter=niter

  if n_elements(zorig) eq 0 then zorig=116
  if n_elements(vname) eq 0 then vname='b'
  if n_elements(mfield) eq 0 then mfield=800
  if n_elements(nline) eq 0 then nline=100
  if n_elements(r0) ne 0 then if (size(r0))[0] eq 1 then nline=1 else nline=(size(r0))[2]
  if n_elements(deltas) eq 0 then deltas=[1e10,1e10]
  d=obj_new('br_data',idlparam)
  bx=d->getvar(vname+'x',it,swap=swap)
  by=d->getvar(vname+'y',it,swap=swap)
  bz=d->getvar(vname+'z',it,swap=swap)
  
  z=d->getz()
  if n_elements(zmax) eq 0 then zmax=abs(z[0]-z[n_elements(z)-1])
  zbot=z[0]
  z=(z-zbot)
  dx=d->getdx()
  dy=d->getdy()
  sz=size(bx)
  x=d->getx()
  y=d->gety()
  nx=d->getmx()
  ny=d->getmy()
  nz=d->getmz()
  br_stretch,bx,nx,ny,dx,z,bxst
  br_stretch,by,nx,ny,dx,z,byst
  br_stretch,bz,nx,ny,dx,z,bzst,zst=zst
  dzst=fltarr(2)
  dzst[0]=zst[1]-zst[0]
  dzst[1]=1.0
  if n_elements(r0) eq 0 then begin
     xx=x#(fltarr(ny)+1.)
     yy=(fltarr(nx)+1.)#y
     r0=fltarr(3,nline)
     if mode eq 'xz' or mode eq 'yz' then begin
        if mode eq 'xz' then begin
           if n_elements(islice) eq 0 then islice=ny/2
           b2=(bx[*,islice,zorig]^2+by[*,islice,zorig]^2+bz[*,islice,zorig]^2)
           ny=1
        endif
        if mode eq 'yz' then begin
           if n_elements(islice) eq 0 then islice=nx/2
           b2=(bx[islice,*,zorig]^2+by[islice,*,zorig]^2+bz[islice,*,zorig]^2)
           nx=1
        endif
        b2=b2/max(b2)
        rxy=randomu(seed,nx,ny)
        brxy=rxy*b2
        chosen=(sort(brxy))[n_elements(brxy)-nline:n_elements(brxy)-1]
        if mode eq 'xz' then begin
           r0[0,0:nline-1]=xx[chosen]
           r0[1,0:nline-1]=y[islice]
           r0[2,0:nline-1]=z[zorig]
        endif
        if mode eq 'yz' then begin
           r0[0,0:nline-1]=x[islice]
           r0[1,0:nline-1]=yy[chosen]
           r0[2,0:nline-1]=z[zorig]
        endif
     endif else begin        
        b2=(bx[*,*,zorig]^2+by[*,*,zorig]^2+bz[*,*,zorig]^2)
        b2=b2/max(b2)
        rxy=randomu(seed,nx,ny)
        brxy=rxy*b2
        chosen=(sort(brxy))[n_elements(brxy)-nline:n_elements(brxy)-1]    
        r0[0,0:nline-1]=xx[chosen]
        r0[1,0:nline-1]=yy[chosen]
        r0[2,0:nline-1]=z[zorig]
     endelse
  endif
  linedir=fltarr(3,nline)
  for il=0,nline-1 do begin
     if nx le 1 then begin
        ix=0
        iy=min(where(y ge r0(0,il)))
        iz=min(where(z ge r0(1,il)))
        linedir(0,il)=bx(ix,iy,iz)
        linedir(1,il)=by(ix,iy,iz)
        linedir(2,il)=bz(ix,iy,iz)
     endif else begin
        if ny le 1 then begin
           ix=min(where(x ge r0(0,il)))
           iy=0
           iz=min(where(z ge r0(1,il)))
           linedir(0,il)=bx(ix,iy,iz)
           linedir(1,il)=by(ix,iy,iz)
           linedir(2,il)=bz(ix,iy,iz)
        endif else begin
           if nz le 1 then begin
              ix=min(where(x ge r0(0,il)))
              iy=min(where(y ge r0(1,il)))
              iz=0
              linedir(0,il)=bx(ix,iy,iz)
              linedir(1,il)=by(ix,iy,iz)
              linedir(2,il)=bz(ix,iy,iz)
           endif else begin
              ix=min(where(x ge r0(0,il)))
              iy=min(where(y ge r0(1,il)))
              iz=min(where(z ge r0(2,il)))
              linedir(0,il)=bx(ix,iy,iz)
              linedir(1,il)=by(ix,iy,iz)
              linedir(2,il)=bz(ix,iy,iz)
           endelse
        endelse
     endelse
  endfor
  szr=size(r0)
  if szr[0] eq 2 then nline=szr[2] else nline=1
  nr1=intarr(nline) & nr2=nr1
  r1=fltarr(mfield,3,nline) & r2=r1
  for i=0,nline-1 do begin
     if i mod 100 eq 0 then message,'done '+string(float(i)/nline*100,format='(f4.1)')+'% of calculation',/info
     br_bline,bxst,byst,bzst,r0[*,i],ri1,dx=dx,dy=dy,dz=dzst, $
            ds=min([dx, dy, dzst[0]]),/xyperiodic,deltas=deltas[0],niter=mfield,zmax=zmax
     br_bline,bxst,byst,bzst,r0[*,i],ri2,dx=dx,dy=dy,dz=dzst, $
            ds=-min([dx, dy, dzst[0]]),/xyperiodic,deltas=deltas[1],niter=mfield,zmax=zmax
     nr1[i]=n_elements(ri1[*,0])
     nr2[i]=n_elements(ri2[*,0])
     r1[0:nr1[i]-1,*,i]=ri1
     r2[0:nr2[i]-1,*,i]=ri2
  endfor
  r1[*,2,*]=r1[*,2,*]+zbot
  r2[*,2,*]=r2[*,2,*]+zbot
  
  obj_destroy,d
end
