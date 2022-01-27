;+
; NAME:
;       BR_HELIUM__DEFINE
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
;       Defines the structure of helium class objects.
;
; RESTRICTIONS:
;
;
; MODIFICATION HISTORY:
;
; $Id: 
;-
pro br_helium::heliuminit
  self.heliumvars=['nhe1','nhe2','nhe3']
  self.heliumsnappos=indgen(3)

  self.do_helium = -1
  self.hehasoldpops =  -1
  heliumatomfile =  "undefined" 
  heliumeuvfile =   "undefined" 

end

function br_helium::getheliumvars
  return, self.heliumvars
end

function br_helium::getheliumsnappos,p
  return, self.heliumsnappos[p]
end

function br_helium::getnatomother
  return, 0.101473
end

function br_helium::hionpg,isnap
  sumh=self->getnatomother()
  grph=self->getgrph()
  units=self->getunits()
  npart=self->getvar('r',isnap)
  npart=npart*units.ur/grph
  nelec=self->getvar('hionne',isnap)
  tg=self->getvar('hiontg',isnap)
  nh2=self->getvar('nh2',isnap)
  
  ; helium included in  sumh
  npart=npart*(1.0 + sumh)-nh2 + nelec


  pg=1.3806580e-16*tg*npart

  
  delvar,tg,npart,nh2,nelec
  return,pg

end

function br_helium::getpmagnetic,isnap
  bx=self->getvar('bx',isnap)
  by=self->getvar('by',isnap)
  bz=self->getvar('bz',isnap)
  units=self->getunits()
  p=(bx^2+by^2+bz^2)*units.ub/8.0/3.1416
  return,p

end

function br_helium::fhe,isnap
  ; compute He(ion)/He(tot)

  nhe1=exp(double(self->getvar('nhe1',isnap)))
  nhe2=exp(double(self->getvar('nhe2',isnap)))
  nhe3=exp(double(self->getvar('nhe3',isnap)))
  nhet=(nhe1)+(nhe2)+(nhe3)

  a=size(nhe1)
  if a[0] eq 2 then begin
     f=dblarr(a[1],a[2],3)
     f[*,*,0]=nhe1/nhet
     f[*,*,1]=nhe2/nhet
     f[*,*,2]=nhe3/nhet
  endif else if a[0] eq 3 then begin
     f=fltarr(a[1],a[2],a[3],3)
     f[*,*,*,0]=nhe1/nhet
     f[*,*,*,1]=nhe2/nhet
     f[*,*,*,2]=nhe3/nhet
  endif

  return,f

  ;; ions=indgen(3)+1
  ;; if (n_elements(ion) eq 0) then begin
  ;;    message,'he_ion=self.fhe(ion,isnap)',/info
  ;;    return,-1
  ;; endif
  ;; if where(indgen(3)+1 eq ion) eq -1 then begin
  ;;    message,'ion must be 1,2 or 3.',/info
  ;;    return,-1
  ;; endif
  ;; fhe=self->getvar('nhe'+string(ion,format='(i1)'),isnap)
  ;; fhe=exp(fhe)/(self->hetot(isnap))
  ;; return,fhe
end

function br_helium::hetot,isnap
  nhe1=self->getvar('nhe1',isnap)
  nhe2=self->getvar('nhe2',isnap)
  nhe3=self->getvar('nhe3',isnap)
  return,exp(nhe1)+exp(nhe2)+exp(nhe3)
end

function br_helium::getheliumsnapname,isnap=isnap
  if n_elements(isnap) eq 0 then isnap=self->getisnap()
  if isnap le -1 then suffix='.helium.snap.scr'
  if isnap eq 0 then suffix='.helium.snap'
  if isnap ge 1 then suffix='.helium_'+br_string3(isnap)+ '.snap'
  name=self->getsnapname()+suffix
  dum=file_search(name,count=count)
  return,name
end

function br_helium::planckbin,bn,isnap
  ; compute the frequency integrated planckfunction
  tg=self->getvar('hiontg',isnap)

  lam0=911.7d
  lam1=753.143d

  nx=512
  ny=1
  nz=325

  fact1=!hplanck/!kboltzmann
  fact2=fact1*!clight*1.d8

  bint=dblarr(nx,nz)
  for k=0,nz-1 do begin
;     for j=0,ny-1 do begin
        for i=0,nx-1 do begin
           nu0=!clight*1.d8/lam0
           nu1=!clight*1.d8/lam1
           dnu=nu1-nu0

           uu=fact1*nu1/tg[i,k]
           ul=fact1*nu0/tg[i,k]
           x=dindgen(100)/99.d0
           x=ul+(uu-ul)*x

          
           
           y=x*x*x*exp(-x)
           iw=where(x lt 10.d,n,/null)
           if (n ne 0) then begin
              y[0:n-1]=x[0:n-1]^3.d0/(exp(x[0:n-1])-1.d0)
           endif
           xint=trapez(x,y)

           const=2*!hplanck/!clight/!clight*(!kboltzmann*tg[i,k]/!hplanck)^4.d0
           bint[i,k]=const*xint
;           stop
        end
;     end
  end
  return,bint

end

pro br_helium::read_binned_cemiss,tgr_euv,cemiss_euv
  file='cemisstab_helium.dat.binned'
  openr,lun,file,/get_lun
  ntgr_euv=0l
  nbin_euv=0l
  readu,lun,ntgr_euv,nbin_euv
  point_lun,lun,ntgr_euv*8*1
  tgr_euv=fltarr(ntgr_euv)
  readu,lun,tgr_euv
  tmp=fltarr(ntgr_euv)
  cemiss_euv=fltarr(ntgr_euv,nbin_euv)
  for i=0,nbin_euv-1 do begin
     point_lun,lun,ntgr_euv*8*(i+2)
     readu,lun,tmp
     cemiss_euv[*,i]=tmp
  endfor
  close,lun

end

pro br_helium::readheliumatom,isnap

if n_elements(isnap) eq 0 then isnap=10s
self->readheliumpars,isnap
fname=(*self.heliumatomfiles)[0]
openr,lu,fname,/get_lun


; read
t='#'
comment='#'
while strmid(strtrim(t,2),0,1) eq comment do readf,lu,t
atomid=strtrim(t,2)
t='#'
while strmid(strtrim(t,2),0,1) eq comment do readf,lu,t
words=strsplit(t,/extract)
abund=float(words[0])
awgt=float(words[1])
t='#'
while strmid(strtrim(t,2),0,1) eq comment do readf,lu,t
words=strsplit(t,/extract)
nk=fix(words[0])
nlin=fix(words[1])
ncnt=fix(words[2])
nfix=fix(words[3])


ev=dblarr(nk)
g=dblarr(nk)
label=strarr(nk)
ionstage=intarr(nk)
for i=0,nk-1 do begin
   t='#'
   while strmid(strtrim(t,2),0,1) eq comment do readf,lu,t
   w=strsplit(t,/extract) 
   ev[i]=double(w[0])
   g[i] =double(w[1])
   ionstage[i] = fix(w[-1])
   label[i] = (strsplit(t,"'",/extract))[1]
end
hc_ev =  0.00012398424
ev=ev*hc_ev
;
; now read the photoionziation cross sections:
print,'currently only reads correct if nbin=6. Must at some point setup with '
print,'reading of hydrogen file'
sigmahe=dblarr(6,2)
for icnt=0,1 do begin
   t='#'
   while strmid(strtrim(t,2),0,1) eq comment do readf,lu,t
   ; now t contains upper and lower level
   for i=0,5 do begin
      t='#'
      while strmid(strtrim(t,2),0,1) eq comment do readf,lu,t
      w=strsplit(t,/extract)
      sigmahe[i,icnt]=double(w[1])
   end
end


if ptr_valid(self.heatomdata) then ptr_free,self.heatomdata
self.heatomdata=ptr_new({name:atomid, abund:abund, awgt:awgt, nk:nk, nlin:nlin, ncnt:ncnt, nfix:nfix, $
                         ev:ev, g:g, label:label, sigmahe:sigmahe})






; reads recombination coeffs for the time being
t='*'
while strmid(strtrim(t,2),0,4) ne 'TEMP' do readf,lu,t
nt=0
readf,lu,nt
if ptr_valid(self.herec_t) then ptr_free,self.herec_t
self.herec_t=ptr_new(dblarr(nt))
if ptr_valid(self.herec_r) then ptr_free,self.herec_r
self.herec_r=ptr_new(dblarr(nt,2))

tmp=fltarr(nt)
readf,lu,tmp
(*self.herec_t)=tmp

for idum=0,1 do begin
   t='*'
   while strmid(strtrim(t,2),0,4) ne 'RECO' do readf,lu,t
   i=0
   j=0
   readf,lu,t
   words=strsplit(t,/extract,count=nw)
   i=fix(words[0])
   j=fix(words[1])
   if min([i,j]) eq 1 and min([i,j]) eq 1 then itr=0 else itr=1
   (*self.herec_r)[0:nw-3,itr]=double(words[2:nw-1])
   tmp=dblarr(nt-nw+2)
   readf,lu,tmp
   (*self.herec_r)[nw-2:-1,itr]=tmp   
end

close,/all

end

pro br_helium::opacity_euv,isnap,ibin,op
  
  self->get_sigmahe,isnap,sigmahe
  self->get_sigmah,isnap,sigmah

  nbin=n_elements(sigmah)
  n1=self->getvar('n1',isnap)
  nhe1=self->getvar('nhe1',isnap)
  nhe1=exp(nhe1)
  nhe2=self->getvar('nhe2',isnap)
  nhe2=exp(nhe2)
  
  op=n1*sigmah[ibin] + nhe1*sigmahe[ibin,0] + nhe2*sigmahe[ibin,1]

  delvar,n1,nhe1,nhe2

end


pro br_helium::get_sigmahe,isnap,sigmahe
  self->readheliumatom,isnap
  sigmahe=(*self.heatomdata).sigmahe
end



pro br_helium::effrec,isnap,effrec

  if n_elements(isnap) ne 1 then begin
     print,'Only one snap at the time'
     return
  end

  isnap=fix(isnap)
  tg=fltarr(self.mx,self.my,self.mz)
  nel=fltarr(self.mx,self.my,self.mz)

  tmp=self->getvar('hiontg',isnap)
  tg[*,*,*]=tmp
  tmp=self->getvar('hionne',isnap)
  nel[*,*,*]=tmp
  
  self->readheliumatom,isnap
  effrec=fltarr(self.mx,self.my,self.mz,2)
  for itr=0,1 do begin
     for k=0,self.mz-1 do begin
        for j=0,self.my-1 do begin
           effrec[*,j,k,itr]=interpol((*self.herec_r)[*,itr],(*self.herec_t),tg[*,j,k],/spline)
        end
     end
  end
  for i=0,1 do effrec[*,*,*,i]=effrec[*,*,*,i]*nel
  
  delvar,tmp,nel,tg
  effrec=reform(effrec)

end


pro br_helium::readheliumpars,isnap,paramsfile=paramsfile;,bdrchk=bdrchk
 
  if ptr_valid(self.isnaps) then ptr_free,self.isnaps
  self.isnaps=ptr_new(isnap)

  if ptr_valid(self.do_heliums) then ptr_free,self.do_heliums
  self.do_heliums=ptr_new(intarr(n_elements(isnap)))

  if ptr_valid(self.hehasoldpopss) then ptr_free,self.hehasoldpopss
  self.hehasoldpopss=ptr_new(intarr(n_elements(isnap)))

  if ptr_valid(self.heliumatomfiles) then ptr_free,self.heliumatomfiles
  self.heliumatomfiles=ptr_new(strarr(n_elements(isnap)))

  if ptr_valid(self.heliumeuvfiles) then ptr_free,self.heliumeuvfiles
  self.heliumeuvfiles=ptr_new(strarr(n_elements(isnap)))

  for it=0,n_elements(isnap)-1 do begin
     self->readparams,isnap[it],paramsfile=paramsfile;,bdrchk=bdrchk
     (*self.do_heliums)[it]=self.do_helium
     (*self.hehasoldpopss)[it]=self.hehasoldpops
     (*self.heliumatomfiles)[it]=self.heliumatomfile
     (*self.heliumeuvfiles)[it]=self.heliumeuvfile
  endfor
end



pro br_helium::cleanup
ptr_free,self.do_heliums
ptr_free,self.hehasoldpopss
ptr_free,self.heliumatomfiles
ptr_free,self.heliumeuvfiles
ptr_free,self.heatomdata
ptr_free,self.herec_t
ptr_free,self.herec_r

end

pro br_helium__define
  struct={br_helium,$
          do_heliums:ptr_new(), $
          hehasoldpopss:ptr_new(), $
          heliumatomfiles:ptr_new(), $
          heliumeuvfiles:ptr_new(), $
          heliumvars: ['nhe1','nhe2','nhe3'],$
          heliumsnappos: intarr(3), $
          heatomdata: ptr_new(), $
          herec_t: ptr_new(), $
          herec_r: ptr_new() $
         }
end
