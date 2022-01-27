;+
; NAME:
;       BR_HION__DEFINE
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
;       Defines the structure of hion class objects.
;
; RESTRICTIONS:
;
;
; MODIFICATION HISTORY:
;
; $Id: 
;-
pro br_hion::hioninit
  self.hionvars=['hionne','hiontg','n1','n2','n3','n4','n5','n6','nh2']
  self.hionsnappos=indgen(9)
  self.grph=2.380491D-24
end

function br_hion::getdohion
  if ptr_valid(self.dohions) then begin
     return, *self.dohions
  endif else begin
     message,'pointer not assigned',/informational
     return,-1
  endelse
end

function br_hion::gethionhasoldpops
  if ptr_valid(self.hionhasoldpopss) then begin
     return, *self.hionhasoldpopss
  endif else begin
     message,'pointer not assigned',/informational
     return,-1
  endelse
end

function br_hion::gethionatomfile
  if ptr_valid(self.hionatomfiles) then begin
     return, *self.hionatomfiles
  endif else begin
     message,'pointer not assigned',/informational
     return,'undefined'
  endelse
end

pro br_hion::getfion,it,fion
 ; if abs(self->getu_r()) lt 1e-20 then begin
 ;    self->readparams,it[0]
 ; endif

  self->load,'n6',it
  fion=self->getvar()
  fion=fion*self->getgrph()
  self->load,'r',it
  r=self->getvar()
  u=self->getunits()
  fion=fion/(r*u.ur)

end

pro br_hion::getntot,it,ntot
  if abs(self->getu_r()) lt 1e-20 then begin
     self->readparams,it[0]
  endif

  self->load,'r',it
  r=self->getvar()
  ntot=(r*self->getu_r())/self->getgrph()

end

pro br_hion::getnh2,it,nh2

  if abs(self->getu_r()) lt 1e-20 then begin
     self->readparams,it[0]
  endif
  self->load,'r',it
  nh2=self->getvar()*self->getu_r()/self->getgrph()
  self->load,'n1',it
  nh2=nh2-self->getvar()
  self->load,'n2',it
  nh2=nh2-self->getvar()
  self->load,'n3',it
  nh2=nh2-self->getvar()
  self->load,'n4',it
  nh2=nh2-self->getvar()
  self->load,'n5',it
  nh2=nh2-self->getvar()
  self->load,'n6',it
  nh2=nh2-self->getvar()

  nh2=0.5*nh2

end

function br_hion::gethionneetabfile
  if ptr_valid(self.hionneetabfiles) then begin
     return, *self.hionneetabfiles
  endif else begin
     message,'pointer not assigned',/informational
     return,'undefined'
  endelse
end

function br_hion::gethionvars
  return, self.hionvars
end

function br_hion::gethionsnappos,p
  return, self.hionsnappos[p]
end

function br_hion::gethionsnapname,isnap=isnap
  if n_elements(isnap) eq 0 then isnap=self->getisnap()
  if isnap le -1 then suffix='.hion.snap.scr'
  if isnap eq 0 then suffix='.hion.snap'
  if isnap ge 1 then suffix='.hion_'+br_string3(isnap)+ '.snap'
  name=self->getsnapname()+suffix
  dum=file_search(name,count=count)
  if(count eq 0) then begin
     suffix=br_string3(isnap)+ '.snap'
     name=self->getsnapname()+'.hion'+suffix ; old standard without '_'
  endif
  return,name
end

pro br_hion::readhionpars,isnap,paramsfile=paramsfile;,bdrchk=bdrchk
 
  if ptr_valid(self.isnaps) then ptr_free,self.isnaps
  self.isnaps=ptr_new(isnap)

  if ptr_valid(self.dohions) then ptr_free,self.dohions
  self.dohions=ptr_new(intarr(n_elements(isnap)))

  if ptr_valid(self.hionhasoldpopss) then ptr_free,self.hionhasoldpopss
  self.hionhasoldpopss=ptr_new(intarr(n_elements(isnap)))

  if ptr_valid(self.hionatomfiles) then ptr_free,self.hionatomfiles
  self.hionatomfiles=ptr_new(strarr(n_elements(isnap)))

  if ptr_valid(self.hionneetabfiles) then ptr_free,self.hionneetabfiles
  self.hionneetabfiles=ptr_new(strarr(n_elements(isnap)))

  for it=0,n_elements(isnap)-1 do begin
     self->readparams,isnap[it],paramsfile=paramsfile;,bdrchk=bdrchk
     (*self.dohions)[it]=self.do_hion
     (*self.hionhasoldpopss)[it]=self.hionhasoldpops
     (*self.hionatomfiles)[it]=self.hionatomfile
     (*self.hionneetabfiles)[it]=self.hionneetabfile
  endfor

end

function br_hion::getgrph
  return,self.grph
end

pro br_hion::coll_rat,isnap,col

  if n_elements(isnap) gt 1 then begin
     print,'only one snap at the time'
     return
  endif
  self->read_hion_atom,isnap
  tg=self->getvar('hiontg',isnap)
  nel=self->getvar('hionne',isnap)
  mx=self->getmx()
  my=self->getmy()
  mz=self->getmz()


  kk= 2.d0 * !PI * (!MELECTRON/!HPLANCK) * (!KBOLTZMANN/!HPLANCK)
  nesqrt=nel*sqrt(tg)
  t12=0.5d0/tg
  t2=tg*tg
  ct32=(kk*tg)^(-1.5d0)
  one_kT2 = 1.d0 / (!kboltzmann * t2)

  n_poly=n_elements((*self.hion_coldat)[*,0])
  plgt=dblarr(mx,my,mz,n_poly)
  plgt[*,*,*,0]=1.d0
  plgt[*,*,*,1]=alog(tg)
  for k=2,n_poly-1 do begin
     plgt[*,*,*,k]=plgt[*,*,*,k-1]*plgt[*,*,*,1]
  end
  
  ; make loop that set's up the col matrix.d
  col=fltarr(mx,my,mz,6,6)
  ncol=n_elements(*self.hion_icol)
  
  eerg=13.6*(1.0-1.0/(findgen(5)+1.0)^2)
  eerg=[eerg,13.6]*!ev_to_erg
  gw=2.0*(findgen(5)+1.0)^2.
  gw=[gw, 1.0]

  
  for i=0,ncol-1 do begin
     cc=(*self.hion_coldat)[0,i]*plgt[*,*,*,0]
     for j=1,n_poly-1 do begin
        cc=cc+(*self.hion_coldat)[j,i]*plgt[*,*,*,j]
     end
     cc=exp(cc)

     ii=(*self.hion_icol)[i]-1
     jj=(*self.hion_jcol)[i]-1

     expde_kt=exp(-(eerg[jj]-eerg[ii])/!kboltzmann/tg)

     if jj ne 5 then begin
        ; when bound-bound
        ; downward
        col[*,*,*,jj,ii]=cc*nesqrt*gw[ii]/gw[jj]
        ; upward
        col[*,*,*,ii,jj]=cc*nesqrt*expde_kt
     endif else begin
        ; bound-free
        ;upward
        col[*,*,*,ii,jj]=cc*nesqrt*expde_kt

        ; downward
        nin6=nel*gw[ii]/gw[jj]/2.d0*CT32/expde_kt
        col[*,*,*,jj,ii]=col[*,*,*,ii,jj]*nin6
     endelse
  

  end

  delvar,plgt
  

end


pro br_hion::read_hion_atom,isnap

if n_elements(isnap) eq 0 then isnap=200s
self->readhionpars,isnap
fname=(*self.hionatomfiles)[0]
openr,lu,fname,/get_lun

; read
t='#'
comment='#'
while strmid(strtrim(t,2),0,1) eq comment do readf,lu,t
atomid=(strsplit(t,"'",/extract))[0]

t='#'
while strmid(strtrim(t,2),0,1) eq comment do readf,lu,t
words=strsplit(t,/extract)
nk=fix(words[0])
nlin=fix(words[1])
ncnt=fix(words[2])

ev=dblarr(nk)
g=dblarr(nk)
label=strarr(nk)
ionstage=intarr(nk)

irad=intarr(nlin+ncnt)
jrad=intarr(nlin+ncnt)
fosc=dblarr(nlin+ncnt)
trad=dblarr(nlin+ncnt)
hval=dblarr(nlin+ncnt)
transtype=strarr(nlin+ncnt)
krad=intarr(nk,nk)

for i=0,nk-1 do begin
   t='#'
   while strmid(strtrim(t,2),0,1) eq comment do readf,lu,t
   w=strsplit(t,/extract) 
   ev[i]=double(w[0])
   g[i] =double(w[1])
   ionstage[i] = fix(w[-2])
   label[i] = (strsplit(t,"'",/extract))[1]
end

; read line data, 
for ilin=0,nlin+ncnt-1 do begin
   t='#'
   while strmid(strtrim(t,2),0,1) eq comment do readf,lu,t
   w=strsplit(t,/extract) 

   jj=fix(w[0])-1
   ii=fix(w[1])-1
   krad[ii,jj]=ilin
   krad[jj,ii]=ilin
   irad[ilin] = min([ii,jj])
   jrad[ilin] = max([ii,jj])
   fosc[ilin] = double(w[2])
   trad[ilin] = double(w[3])
   hval[ilin] = double(w[4])
   transtype[ilin] = w[5]
end


; now read lyman continuum data
t='#'
while strmid(strtrim(t,2),0,1) eq comment do readf,lu,t
nbin_euv=fix(t)
wl=dblarr(nbin_euv+1)
sigmah=dblarr(nbin_euv)
for ibin=0,nbin_euv do begin
   t='#'
   while strmid(strtrim(t,2),0,1) eq comment do readf,lu,t
   wl[ibin] = double(t)
end
for ibin=0,nbin_euv-1 do begin
   t='#'
   while strmid(strtrim(t,2),0,1) eq comment do readf,lu,t
   sigmah[ibin] = double((strsplit(t,/extract))[1])   
end

if ptr_valid(self.hatomdata) then ptr_free,self.hatomdata
self.hatomdata=ptr_new({name:atomid, nk:nk, nlin:nlin, ncnt:ncnt, $
                         ev:ev, g:g, label:label, krad:krad, irad:irad, $
                         jrad:jrad, fosc:fosc, trad:trad, hval:hval, transtype:transtype, $
                         nbin_euv:nbin_euv, wl:wl, sigmah:sigmah})


; ONLY READS COLLISIONAL DATA - FOR THE TIME BEING. 
; read collisional data
;read collisional data
t="#"
while strmid(t,0,1) eq '#' do readf,lu,t
ntemp_bb=fix(t)
;bb data temp
readf,lu,t
;bb data coeff
for i=1,10 do readf,lu,t
readf,lu,t
while (strmid(t,0,1) eq '#') do readf,lu,t
ntemp_bf =fix(t)
;bf data temp
readf,lu,t
;bf data coeff
for i=1,5 do readf,lu,t

;  read polynomial approx to collisions
readf,lu,t
while (strmid(t,0,1) eq '#') do readf,lu,t
n_poly=fix(t)
n_bb=10
n_bf=5

if ptr_valid(self.hion_icol) then ptr_free,self.hion_icol
self.hion_icol=ptr_new(intarr(n_bb+n_bf))
if ptr_valid(self.hion_jcol) then ptr_free,self.hion_jcol
self.hion_jcol=ptr_new(intarr(n_bb+n_bf))
if ptr_valid(self.hion_coldat) then ptr_free,self.hion_coldat
self.hion_coldat=ptr_new(dblarr(n_poly+1,n_bb+n_bf))

n_read=0
while n_read lt n_bb+n_bf do begin

   t='#' 
   while strmid(t,0,1) eq '#' do readf,lu,t
   words=strsplit(t,/extract)
   (*self.hion_icol)[n_read]=min(fix(words[0:1]))
   (*self.hion_jcol)[n_read]=max(fix(words[0:1]))

   ;cc[*,n_read]=double(words[2:2+n_poly])
   (*self.hion_coldat)[*,n_read]=double(words[2:2+n_poly])
   n_read=n_read+1

end

free_lun,lu

end

pro br_hion::get_sigmah,isnap,sigmah
  self->read_hion_atom,isnap
  sigmah=(*self.hatomdata).sigmah
end

pro br_hion::cleanup
ptr_free,self.dohions
ptr_free,self.hionhasoldpopss
ptr_free,self.hionatomfiles
ptr_free,self.hionneetabfiles
ptr_free,self.hion_coldat
ptr_free,self.hion_icol
ptr_free,self.hion_jcol
ptr_free,self.hatomdata
end

pro br_hion__define
  struct={br_hion,$
          grph:2.380491D-24, $        ;gram per hydrogen atom.
          dohions:ptr_new(), $
          hionhasoldpopss:ptr_new(), $
          hionatomfiles:ptr_new(), $
          hionneetabfiles:ptr_new(), $
          hion_coldat:ptr_new(), $
          hion_icol:ptr_new(), $
          hion_jcol:ptr_new(), $
          hionvars: ['hionne','hiontg','n1','n2','n3','n4','n5','n6','fion','nh2'],$
          hionsnappos: intarr(9),$
          hatomdata: ptr_new() $
         }
end
