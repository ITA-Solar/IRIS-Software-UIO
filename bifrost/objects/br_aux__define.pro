; NAME:
;       BR_AUX__DEFINE
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
;       Defines the structure of auxiliar objects.
;
; RESTRICTIONS:
;
;
; MODIFICATION HISTORY:
;
; $Id$
;-

function br_aux::init
  self.variablename=''
  self.xtitle='x [Mm]'
  self.rastertitle='Time [hs]'
  self.ytitle='z [Mm]'
  self.stitle='y [Mm]'
  self.menu.naction=3
  self.menu.action=['Zoom','Average x direction','Average y direction']
  self.menu.nmode=3
  self.menu.mode=['Axis 1 v 2','Axis 2 v 3','Axis 1 v 3']
  self.assoc_file='xmhd_assoc_file.tmp'
  if getenv('BIFROST') ne '' then begin
     self.data_dir=getenv('BIFROST')+path_sep()+'IDL'+path_sep()+'data'+path_sep()
  endif else begin
     self.data_dir='~'+path_sep()+'IDL'+path_sep()+'data'+path_sep()
     if not (file_info(self.data_dir)).exists then begin
        message,'creating folders in'+self.data_dir,/info
        spawn,'mkdir -p '+ self.data_dir+'lines'
        spawn,'mkdir '+ self.data_dir+'filters'
        spawn,'mkdir '+ self.data_dir+'ancillary'
        spawn,'mkdir '+ self.data_dir+'psf'
     endif
  endelse
  self->setallowed_lines
  self.tabinputfile='tabparam.in'
  self->readtabparam
  return,1
end

function br_aux::gettitle
  return,self.title
end

pro br_aux::setxtitle,xtitle
  self.xtitle=xtitle
end

function br_aux::getxtitle
  return,self.xtitle
end

pro br_aux::setrastertitle,rastertitle
  self.rastertitle=rastertitle
end

function br_aux::getrastertitle
  return,self.rastertitle
end

; for old times sake....
function br_aux::getxrastertitle
  return,self.rastertitle
end

pro br_aux::setytitle,ytitle
  self.ytitle=ytitle
end

function br_aux::getytitle
  return,self.ytitle
end

function br_aux::getvariablename
  return,self.variablename
end

function br_aux::getxytitle
  return,self.xytitle
end

pro br_aux::setrev,rev
  self.rev=rev
end

pro br_aux::setswap,swap
  self.swap=swap
end

function br_aux::getrev
  return,self.rev
end

function br_aux::getaspect
  return,self.aspect
end

pro br_aux::setaspect,aspect
  self.aspect=aspect
end

function br_aux::getswap
  return,self.swap
end

pro br_aux::setassoc_file,file
  self.assoc_file=file
end

function br_aux::getassoc_file
if not (file_info('.')).write then begin
  message,'Could not write assoc file in cwd, placing in ~',/info
  self.assoc_file='~/xmhd_assoc_file.tmp'
endif
return,self.assoc_file
end

function br_aux::getdata_dir
  return,self.data_dir
end

function br_aux::getlines_dir
  return,self.data_dir+'lines'+path_sep()
end

function br_aux::getfilters_dir
  return,self.data_dir+'filters'+path_sep()
end

function br_aux::getfilters,filter
  line_dir=self->getfilters_dir()
  if (file_info(line_dir+filter+'.sav')).exists then begin
    restore,line_dir+filter+'.sav'
  endif else begin
    restgen,struct=resp,file=line_dir+filter
  endelse
  return,resp
end

function br_aux::getpsf_dir
  return,self.data_dir+'psf'+path_sep()
end


pro br_aux::setdata_dir,data_dir
  self.data_dir=data_dir
  self->setallowed_lines
end

pro br_aux::setallowed_lines,data_dir
;
  if n_params() eq 0 then data_dir=self.data_dir else self.data_dir=data_dir
  f=file_search(self.data_dir+'lines','*.sav')
;  f=[file_search(self.data_dir+'lines','*.sav'),f]
  self.allowed_lines=ptr_new(strarr(n_elements(f)))
  for i=0,n_elements(*self.allowed_lines)-1 do begin
    s=strpos(f[i],path_sep(),/reverse_search)+1
    e=strpos(f[i],'.',/reverse_search)
    (*self.allowed_lines)[i]=strmid(f[i],s,e-s)
  endfor
end

function br_aux::getlines,ion,wvlr
  if n_params() lt 1 then begin
    message,' must give desired ion (e.g. "c"): lines=a->getlines(ion, wvlr)',/info
    return,['no ion given']
 endif
  if n_elements(wvlr) eq 0 then wvlr=[0.,1000000000.]
  dum=self->getallowed_lines()
  if max(stregex(dum,'^'+ion)) ne 0 then begin
    message,' no lines found for ion '+ion,/info
    return,['no lines found']
  endif
  dum=dum[where(stregex(dum,'^'+ion) eq 0)]
  for i=0,n_elements(dum)-1 do begin
    wvl=self->getwvl(dum[i])
    if wvl lt wvlr[0] or wvl gt wvlr[1] then dum[i]=''
  endfor
  for i=1,n_elements(dum)-1 do begin
    dumi=dum[i]
    if stregex(dumi,'_p$',/boolean) then begin
      dumi=strmid(dumi,0,stregex(dumi,'_p$'))
      if dumi eq dum[i-1] then dum[i-1]=''
    endif
  endfor
  dum=dum[where(dum ne '')]
  for i=0,n_elements(dum)-1 do print,i,'  ',$
    strupcase(self->getatom_name(dum[i]))+' '+ $
    self->getion_state(dum[i],/rom)+' '+ $
    string(self->getwvl(dum[i]),format='(f6.1)')
  return,dum
end

function br_aux::line_label,var
  return,(strupcase(self->getatom_name(var))+' '+ $
         self->getion_state(var,/rom))[0]
end

function br_aux::getallowed_lines
  return,*self.allowed_lines
end

pro br_aux::setmode,mode
  self.mode=mode
end

function br_aux::getmode
  return,self.mode
end

function br_aux::getmenu
  return,self.menu
end

function br_aux::getdispmode
  return, self.dispmode
end

pro br_aux::setstitle,title
  self.stitle=title
end

function br_aux::getstitle
  return,self.stitle
end

pro br_aux::setspos,pos
  self.spos=pos
end

function br_aux::getspos
  return,self.spos
end

pro br_aux::setdispmode,mode
  self.dispmode=mode
end

pro br_aux::loadct,ct,rgb=rgb,contrast=contrast
cct=ct
if datatype(ct) eq 'STR' then begin
  case cct of
  'int': cct=0
  'vel': cct=98
  'wid': cct=13
  'red': cct=3
  'green': cct=9
  'pink': cct=7
  'blue' : cct=1
  'int+red': cct=97
  'int+red+green': cct=96
  'int+red+green+blue': cct=95
  else: begin
    message,'no such ct '+cct+' defined, set to 0',/info
    cct=0
 end
endcase
endif
case cct of 
95: begin
  loadct,0,/silent
  tvlct,r,g,b,/get
  r[253]=0
  g[253]=0
  b[253]=255
  r[254]=0
  g[254]=255
  b[254]=0
  r[255]=255
  g[255]=0
  b[255]=0
  tvlct,r,g,b
   end
96: begin
  loadct,0,/silent
  tvlct,r,g,b,/get
  r[254]=0
  g[254]=255
  b[254]=0
  r[255]=255
  g[255]=0
  b[255]=0
  tvlct,r,g,b
   end
97: begin
  loadct,0,/silent
  tvlct,r,g,b,/get
  r[255]=255
  g[255]=0
  b[255]=0
  tvlct,r,g,b
   end
98: begin
; "Ada's" red and blue colorscale
   br_colors_yk,r,g,b, contrast=1.0
   end
99: begin
  datadir=self->getdata_dir()
  restore,datadir+'ancillary/bluegreyred.sav'
  tvlct,r,g,b 
   end
13: begin 
  loadct,13
  tvlct,r,g,b,/get
  flc=255
  r[flc]=255 & g[flc]=255 & b[flc]=255
  tvlct,r,g,b
  end
else: begin
  loadct,cct
  tvlct,r,g,b,/get
      end
endcase
rgb={r:r,g:g,b:b}
end

function br_aux::strtime,time
  ut=(self->getunits()).ut
  strtime=strtrim(string(fix((time)*ut/60./60.),format='(i2.2)'),2)+':' $
    +strtrim(string(fix((time)*ut/60. mod 60),format='(i2.2)'),2)+':' $
    +strtrim(string(fix((time)*ut mod 60.),format='(i2.2)'),2)
  return,strtime
end

function br_aux::getvartxt,cvar
  u=self->getunits()
  case cvar of
  'r': info={name:'!4q!3',ubf:u.ur,usi:1.0,unit:'!3g/cm!E3!N',unitsi:'!3kg/m!E3!D'}
  'ux': info={name:'!3u!Dx!N',ubf:u.ul/u.ut/1.e5,usi:u.ul/u.ut/1.e5,unit:'km/s',unitsi:'km/s'}
  'uy': info={name:'!3u!Dy!N',ubf:u.ul/u.ut/1.e5,usi:u.ul/u.ut/1.e5,unit:'km/s',unitsi:'km/s'}
  'uz': info={name:'!3u!Dz!N',ubf:u.ul/u.ut/1.e5,usi:u.ul/u.ut/1.e5,unit:'km/s',unitsi:'km/s'}
  'e': info={name:'!3e',ubf:u.e,usi:u.ue/1.e3,unit:'erg/cm!E3!N',unitsi:'J/m!E3!N'}
  'bx': info={name:'!3B!Dx!N',ubf:u.ub,usi:u.ub/1.e4,unit:'Gauss',unitsi:'T'}
  'by': info={name:'!3B!Dy!N',ubf:u.ub,usi:u.ub/1.e4,unit:'Gauss',unitsi:'T'}
  'bz': info={name:'!3B!Dz!N',ubf:u.ub,usi:u.ub/1.e4,unit:'Gauss',unitsi:'T'}
  'modb': info={name:'!3mod(B)',ubf:u.ub,usi:u.ub/1.e4,unit:'Gauss',unitsi:'T'}
  'b2': info={name:'!3B!E2!N/2',ubf:u.e/2.,usi:u.ue/2./1.e3,unit:'erg/cm!E3!N',unitsi:'J/m!E3!N'}
  'tg': info={name:'!3T!Dg!N',ubf:1.0,usi:1.0,unit:'K',unitsi:'K'}
  'p': info={name:'!3P!Dg!N',ubf:u.p,usi:u.up/1.e3,unit:'dyn/cm!E2!N',unitsi:'N/m!E2!N'}
  else: info={name:cvar,ubf:1.0,usi:1.0,unit:'',unitsi:''}
  endcase
  return,info
end

function br_aux::atom_names
  aname=['h','he', $
         'li','be','b','c','n','o','f','ne', $
         'na','mg','al','si','p','s','cl','ar', $
         'k','ca', $
             'sc','ti','v','cr','mn','fe','co','ni','cu','zn', $
                  'ga','ge','as','se','br','kr']
  return,aname
end

function br_aux::getatom_name,var,upcase=upcase
  if n_elements(upcase) eq 0 then upcase=0
  dum=(self->atom_names())[self->findatom(var)]
  if upcase then begin 
    atom=strupcase(strmid(dum,0,1))
    if strlen(dum) gt 1 then atom=atom+strmid(dum,1,1)
  endif else atom=dum
  return,atom
end

function br_aux::findatom,var
  aname=self->atom_names()
  varl=strlowcase(strtrim(var,2))
  pos=stregex(varl,'[0-9]')
  dum=varl
  index=intarr(n_elements(pos))
  for j=0,n_elements(pos)-1 do begin
    if pos[j] ne -1 then dum[j]=strmid(varl[j],0,pos[j]) else dum[j]=varl[j]
    index[j]=where(dum[j] eq aname)
  endfor
  return,index
end

function br_aux::getion_state,var,rom=rom
  varl=strlowcase(strtrim(var,2))
  varl=(strsplit(varl,'_',/extract))[0]
  pos=stregex(varl,'[0-9]')
  if pos ne -1 then dum=fix(strmid(varl,pos,2)) else begin
    message,'no number found in atom name '+var,/info
    return,-1
  endelse
  if keyword_set(rom) then dum=roman(dum)
  return,dum
end

function br_aux::abund,var,file=file,dir=dir,verbose=verbose
  if n_elements(verbose) eq 0 then verbose=0
  if n_elements(file) eq 0 then file='sun_photospheric_2007_grevesse.abund'
  if n_elements(dir) eq 0 then begin
     DEFSYSV,'!xuvtop',exists=i
     if i eq 0 then begin
        message,'xuvtop system variable for chianti is not defined',/info
        return,-1.0
     endif
     dir=!xuvtop+'/abundance'
  endif
  read_abund,dir+'/'+file,abund,ref
  if verbose and ref[0] ne '' then print,ref
  elem=self->findatom(var)
  if elem ne -1 then return,(abund[elem])[0] else begin
    message,'no element found, abundance set to -1.0',/info
    return,-1.0
  endelse
end

function br_aux::awgt,var
  awgt=[1.00794,4.002602, $
        6.941,9.012,10.811,12.0107,14.00674,15.9994,18.998,20.1797, $
        22.9897,24.3050,26.9815,28.0855,30.9737,32.066,35.4527,39.948, $
        39.0983,40.078, $
  44.9559,47.867,50.9415,51.9961,54.9380,55.845,58.9332,58.6934,63.546,65.39, $
          69.723,72.61,74.9216,78.96,79.904,83.80]
  elem=self->findatom(var)
  if max(elem) eq -1 then begin 
    message,'no element found, weight set to -1.0',/info
    return,-1.0
  endif
  return,reform(awgt[elem])
end

function br_aux::getion,cion
  line_dir=self->getlines_dir()
  if not((file_info(line_dir)).exists) then spawn,'mkdir -p '+line_dir
  if (file_info(line_dir+cion+'.sav')).exists then begin
    restore,line_dir+cion+'.sav'
  endif else begin
    br_chianti_build_goftne_nsens,cion+'.sav',path=line_dir;self->getdata_dir()+'lines/'
    restore,line_dir+cion+'.sav'
 endelse
  if n_elements(ion) eq 0 then return,{empty:-1} else return,ion
end

function br_aux::getabund,cion
  ion=self->getion(cion)
  return,ion.abund[0]
end

function br_aux::getwvl,cion
  ion=self->getion(cion)
  if where(tag_names(ion) eq 'EMPTY') eq 0 then return,ion.empty $
  else return,ion.lines[0].wvl
end

function br_aux::getny0,cion
  cc=3.e8
  aa2cm=1.e10
  ion=self->getion(cion)
  return,cc/ion.lines[0].wvl*aa2cm
end

function br_aux::gettmax,cion
  ion=self->getion(cion)
  return,10^(ion.lines[0].tmax)
end

function br_aux::getwtherm,cion
  u=self->getunits()
  tmax=self->gettmax(cion)
  awgt=self->awgt(cion)*u.mh
  return,sqrt(4.*alog(2)*(2.*u.bk*tmax/awgt))
end

function br_aux::getbk
  bk=1.38e-16
  return,bk
end

function br_aux::getamu
  amu=1.67e-24
  return,amu
end

pro br_aux::cleanup  
  ptr_free,self.allowed_lines
end

function br_aux::tabparamload
  if self.tabparam.nrhobin eq 0 then return,0 else return,1
end

pro br_aux::readtabparam,file
  if n_elements(file) eq 0 then file=self.tabinputfile $
  else self.tabinputfile=file
  if not (file_info(file)).exists then begin
    message,'tabparam file '+file+' not found, exiting',/info
    return
 endif
  message,'reading tabparam file '+file,/info
  openr,lu,file,/get_lun
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
  if n_elements(eostablefile) ne 0 then self.tabparam.eostablefile=eostablefile
  if n_elements(rhoeiradtablefile) ne 0 then self.tabparam.rhoeiradtablefile=rhoeiradtablefile
  if n_elements(netgradtablefile) ne 0 then self.tabparam.netgradtablefile=netgradtablefile
  if n_elements(nrhobin) ne 0 then self.tabparam.nrhobin=nrhobin
  if n_elements(rhomin) ne 0 then self.tabparam.rhomin=rhomin
  if n_elements(rhomax) ne 0 then self.tabparam.rhomax=rhomax
  if n_elements(neibin) ne 0 then self.tabparam.neibin=neibin
  if n_elements(eimin) ne 0 then self.tabparam.eimin=eimin
  if n_elements(eimax) ne 0 then self.tabparam.eimax=eimax
  if n_elements(lte) ne 0 then self.tabparam.lte=lte
  if n_elements(nradbins) ne 0 then self.tabparam.nradbins=nradbins
  if n_elements(cel) ne 0 then self.tabparam.cel=cel
  if n_elements(abund) ne 0 then self.tabparam.abund=abund
  if n_elements(aweight) ne 0 then self.tabparam.aweight=aweight
  free_lun, lu
end

function br_aux::gettabparam,file
  if n_elements(file) eq 0 and self->tabparamload() eq 0 then begin
    file='tabparam.in'
    message,'no tabparam file name given and file not read',/info
    message,'using default file '+file,/info
  endif
  if n_elements(file) ne 0 then self->readtabparam,file
  return,self.tabparam
end

function br_aux::grph
  return,2.27e-24
end

function br_aux::gettabgrph
  return,total((self->gettababund())*self->awgt(self->gettabelements()))*self->getamu()
end

function br_aux::gettababund
  abund=float(strsplit(self.tabparam.abund,' ',/extract))
  return,10^(abund-12.0)
end

function br_aux::gettabaweight
  aweight=float(strsplit(self.tabparam.aweight,' ',/extract))
  return,aweight
end

; function br_aux::get_ieq,temp,iz,ion
;  dir=concat_dir(!xuvtop,'ioneq')
;  ioneq_name=dir+'/chianti.ioneq' 
;  read_ioneq,ioneq_name,ioneq_logt,ioneq_frac,ioneq_ref
;  ieq=get_ieq(temp,iz,ion,ioneq_logt=ioneq_logt,ioneq_frac=ioneq_frac)
;  if ion le 2 then begin
;    ieq[where(temp lt 1.e4)]=0.95 ; specifically for ni 2
;    message,'Ionization fraction set to 0.95 for T < 10 000 K',/cont
;  endif
;  return,ieq
; end

function br_aux::gettabelements
  return,strsplit(self.tabparam.cel,' ',/extract)
end

function br_aux::getunits
  self.units.ul=1.e8
  self.units.ut=1.e2
  self.units.ur=1.e-7
  self.units.uu=self.units.ul/self.units.ut
  self.units.up=self.units.ur*self.units.uu^2
  self.units.uee=self.units.uu^2
  self.units.ue=self.units.ur*self.units.uu^2
  self.units.mu=0.63
  self.units.bk=self->getbk()
  self.units.mh=self->getamu()
  self.units.mm=self.units.mu*self.units.mh
  self.units.ub=self.units.uu*sqrt(4.*!pi*self.units.ur)
  self.units.re = 8.31e7
  self.units.cl = 2.998e10               ; cm/s
  self.units.qe = 4.80321d-10
  self.units.me = 9.10953d-28
  return,self.units
end

function br_aux::solar_flux,si=si,cgs=cgs
j2erg=1.e7
m2cm=1.e2
rsun=6.957e8
solar_flux=3.828e26/rsun^2/4./!pi
u=self.getunits()
if n_elements(si) ne 0 then return,solar_flux
if n_elements(cgs) ne 0 then return,solar_flux*j2erg/m2cm^2
return,solar_flux*j2erg/m2cm^2/u.ue/u.uu ; W/m2 J2erg
end

pro br_aux__define
  nitem=10
  struct={br_aux, $
          title:'', $
          xtitle:'', $
          ytitle:'', $
          stitle:'', $
          spos:0,$
          rastertitle:'', $
          variablename:'', $
          swap:0, $
          rev:0, $
          aspect:0.0, $
          assoc_file:'', $
          data_dir:'', $
          allowed_lines:ptr_new(), $
          mode:'', $
          dispmode:0, $
          menu:create_struct(name='menu', $
                                  'action',strarr(nitem), $
                                  'naction',0, $
                                  'xtitle',strarr(nitem), $
                                  'ytitle',strarr(nitem), $
                                  'mode',strarr(nitem), $
                                  'nmode',0, $
                                  'params',strarr(nitem), $
                                  'nparams',0), $
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
         tabinputfile:'',$
         tabparam:create_struct(name='tabparam', $
                                'eostablefile','', $
                                'rhoeiradtablefile','', $
                                'netgradtablefile','', $
                                'nrhobin',0, $
                                'rhomin',1.0, $
                                'rhomax',1.0, $
                                'neibin',0, $
                                'eimin',1.0, $
                                'eimax',1.0, $
                                'nradbins',0, $
                                'lte',0, $
                                'cel','', $
                                'abund','', $
                                'aweight','')}
end
