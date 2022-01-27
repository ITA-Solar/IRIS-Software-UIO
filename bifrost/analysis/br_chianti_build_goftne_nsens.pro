pro br_chianti_build_goftne_nsens,varname,wave=wave,goftne=goftne,ioneq_name=ioneq_name,abund_name=abund_name,path=path

;br_chianti_build_goftne_nsens,'fe12_187_p.sav',wave=[187.,187.999],goftne=goftne_fe12_187

if n_params() lt 1 then begin
   message,'br_chianti_build_goftne_nsens,varname,wave=wave,goftne=goftne,ioneq_name=ioneq_name,abund_name=abund_name',/info
   message,'The varname must have the following structure: ion name + ionization level + _ + wavelength in \AA',/info
   message,'e.g: fe12_187, it can also end with _p or _p.sav, e.g: fe12_187_p.sav, if not, it will be added. ',/info
   message,'if wave is not set, it will take one \AA range, eg:[187,187.999]',/info
   return
endif

varnamepre=varname
DEFSYSV,'!xuvtop',exists=i
if i eq 0 then begin
   message,'xuvtop system variable for chianti is not defined',/info
   return
endif
dbase_path=!xuvtop 
if n_elements(ioneq_name) eq 0 then ioneq_name = dbase_path + '/ioneq/chianti.ioneq' $
else ioneq_name = dbase_path + ioneq_name ;;; 'arnaud_raymond.ioneq''
;ioneq_name=dbase_path+'/ioneq/arnaud_raymond.ioneq'
;ioneq_name=dbase_path+'/ioneq/arnaud_raymond_ext.ioneq	'
;ioneq_name=dbase_path+'/ioneq/arnaud_rothenflug.ioneq'
;ioneq_name=dbase_path+'/ioneq/arnaud_rothenflug_ext.ioneq'
;ioneq_name=dbase_path+'/ioneq/bryans_etal_06.ioneq'
;ioneq_name=dbase_path+'/ioneq/bryans_etal_09.ioneq'
;ioneq_name=dbase_path+'/ioneq/shull_steenberg.ioneq'
;ioneq_name=dbase_path+'/ioneq/shull_steenberg_ext.ioneq'
;ioneq_name=dbase_path+'/ioneq/mazzotta_etal.ioneq'
;ioneq_name=dbase_path+'/ioneq/mazzotta_etal_9.ioneq'
;ioneq_name=dbase_path+'/ioneq/mazzotta_etal_ext.ioneq'

if n_elements(abund_name) eq 0 then abund_name = dbase_path + '/abundance/sun_photospheric.abund' $
else abund_name = dbase_path + abund_name;; coronal abundances in the temp resp.
;abund_name=dbase_path+'/abundance/sun_coronal.abund' ;; coronal abundances in the temp resp.

posl=stregex(strlowcase(varname),'[a-z]+',length=lenl)
posn=stregex(varname,'[0-9]+',length=lenn)
var=strmid(varname,posl,lenl)+'_'+strmid(varname,posn,lenn)
name=strmid(varname,posl,lenl)+strmid(varname,posn,lenn)

if n_elements(wave) ne 2 then begin
poss=stregex(varname,'_') 
waven=strmid(varname,poss,10)
posw=stregex(waven,'[0-9]+',length=lenw)
wave = [fix(strmid(waven,posw,lenw)),fix(strmid(waven,posw,lenw))+0.999]
endif

if stregex(varname,'.sav') eq -1 then begin
   if stregex(varname,'_p') eq -1 then varname=varname+'_p.sav' else varname=varname+'.sav' 
endif

openr,lun,ioneq_name,/get_lun
txt=''
readf,lun,txt
free_lun,lun
stra=strsplit(txt,/extract)
ndp=fix(stra[0])
ntg=5
press=fltarr(71)  &  press=10^(findgen(71)*0.1+12) 
goftne=fltarr(ndp,71) & goftnen=fltarr(ndp*ntg,71)

a=obj_new('br_aux')

for ii=0,n_elements(press)-1 do begin
   press_s=press[ii]
   nion=create_struct('ioneq_logt',-1)
   ch_synthetic,wave[0],wave[1],output=nion,sngl_ion=var,pressure=press[ii],/goft, $   
            ioneq_name=ioneq_name
   if n_tags(nion) le 1 then return
   if ii eq 0 then begin
      goftneline=fltarr(n_elements(nion.lines),ndp,71)
      goftnenline=fltarr(n_elements(nion.lines),ndp*ntg,71)
      line=create_struct(nion.lines[0],'goftne',fltarr(ndp*ntg,71))
      lines=replicate(line,n_elements(nion.lines))
      message,' # lines in ' + varname + ' = ' + strtrim(string(n_elements(nion.lines)),1),/info 
   endif
   for il=0,n_elements(nion.lines)-1 do begin
      goftneline[il,*,ii]=nion.lines[il].goft
      ;loc=where(reform(goftneline[il,*,ii]) ne 0.)
      loc=indgen(n_elements(goftneline[il,*,ii]))
      ;goftnenline(il,loc[0]*ntg:(max(loc)+1)*ntg-1,ii)=interpol(reform(alog10(goftneline(il,loc,ii))),indgen(n_elements(loc)), $
      ;                                                       findgen((n_elements(loc))*ntg)/(n_elements(loc)*ntg-1)*(n_elements(loc)-1),/spline) 
      goftnenline(il,loc[0]*ntg:(max(loc)+1)*ntg-1,ii)=interpol(reform(alog10(goftneline(il,loc,ii))),loc, $
                                                             findgen((n_elements(loc))*ntg)/(n_elements(loc)*ntg-1)*(n_elements(loc)-1),/spline) 
      if ii eq 0 then begin
         lines[il].iz=nion.lines[il].iz
         lines[il].ion=nion.lines[il].ion
         lines[il].ident=nion.lines[il].ident
         lines[il].ident_latex=nion.lines[il].ident_latex
         lines[il].snote=nion.lines[il].snote
         lines[il].lvl1=nion.lines[il].lvl1
         lines[il].lvl2=nion.lines[il].lvl2
         lines[il].tmax=nion.lines[il].tmax
         lines[il].wvl=nion.lines[il].wvl
         lines[il].flag=nion.lines[il].flag
         lines[il].goft=nion.lines[il].goft
      endif
      lines[il].goftne=goftnenline[il,*,*]
   endfor
   
   if nion.ioneq_logt[0] eq -1 then return 
   nlns=n_elements(nion.lines.wvl)
   if nlns eq 1 then goftne[*,ii]=(nion.lines.goft)[*,0]
   if nlns gt 1 then goftne[*,ii]=total(nion.lines.goft,2) 
   ;loc=where(reform(goftne[*,ii]) ne 0.)
   loc=indgen(n_elements((reform(goftne))[*,ii]))
   ;goftnen(loc[0]*ntg:(max(loc))*ntg-1,ii)=interpol(reform(alog10(goftne(loc,ii))),indgen(n_elements(loc)),findgen((n_elements(loc)-1)*ntg)/(n_elements(loc)*ntg)*n_elements(loc),/spline) 
   goftnen(loc[0]*ntg:(max(loc)+1)*ntg-1,ii)=interpol(reform(alog10(goftne(loc,ii))),loc,findgen((n_elements(loc))*ntg)/(n_elements(loc)*ntg-1)*(n_elements(loc)-1),/spline) 
   if ii eq 30 then nion_s=nion
endfor



for il=0,n_elements(nion.lines)-1 do begin
   goftne= lines[il].goftne*0
   goftne(where(goftnen lt 0.))=10^(lines[il].goftne(where(goftnen lt 0.)))
   goftne(where(goftne eq 1.0)) = 0.0
   lines[il].goftne=goftne
   lines[il].goftne[where(finite(lines[il].goftne,/NAN))]=0.0
   lines[il].goft[where(finite(lines[il].goft,/NAN))]=0.0
endfor

newion={lines:lines,IONEQ_LOGT:nion.ioneq_logt,IONEQ_NAME:nion.ioneq_name,IONEQ_REF:nion.ioneq_ref, $
        WVL_LIMITS:nion.wvl_limits,MODEL_FILE:nion.model_file,MODEL_NAME:nion.model_name,MODEL_NE:nion.model_name, $
        MODEL_PE:nion.model_pe,MODEL_TE:nion.model_te,WVL_UNITS:nion.wvl_units,INT_UNITS:nion.int_units, $
        ADD_PROTONS:nion.add_protons,DATE:nion.date,VERSION:nion.version,PHOTOEXCITATION:nion.photoexcitation}

goftne=goftnen*0
goftne(where(goftnen lt 0.))=10^(goftnen(where(goftnen lt 0.)))
goftne(where(goftne eq 1.0)) = 0.0
goftne[where(finite(goftne,/NAN))]=0.0

abund=a->abund(name)
if abund ne -1.0 and n_elements(nion) gt 0 then begin
   wvlname=strtrim(string(wave[0]),2)
   message,'computed line '+name+' '+wvlname+' abund '+string(abund),/info
   nion=newion
   nti=n_elements(nion.IONEQ_LOGT)
   deltat=(nion.IONEQ_LOGT)[1]-(nion.IONEQ_LOGT)[0]
   logtgr=findgen(nti*ntg)*deltat/ntg+4.
   ion = create_struct(nion,'abund',abund,'pressure',press,'GOFTNE',goftne,'logtgr',logtgr)
   if n_elements(path) eq 0 then path=''
   save,ion,file=path+varname
endif else begin
print,'Abundance does not exist or not tag',name
endelse

destroy,a
varname=varnamepre

end
