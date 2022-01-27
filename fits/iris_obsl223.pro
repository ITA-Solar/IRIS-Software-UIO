;+
; NAME:
;	IRIS_OBSL223
;
; PURPOSE:
;       make level3 file and associated directories in a
;       tree-structure. Wrapper around iris_make_fits_level3
;
; CATEGORY:
;       IRIS
;
; CALLING SEQUENCE:
;       IRIS_OBSL223,obs ,iwin=iwin,/all,/sp,rootl2=rootl2,rootl3=rootl3,/debug,/silent,/replace
;
; INPUTS:
;       obs    - obs name (=directory name where level2 files reside)
;
; KEYWORD PARAMETERS:
;       version- print version number
;       iwin   - windows to concatenate into level3, if not /all given
;                there is a menu printed of possible windows
;       all    - use all windows
;       rootl2 - root directory of level2 files (defaults to IRIS_DATA/level2)
;       rootl3 - root directory of level3 files (defaults to rootl2
;                with level2 substituted with level3)
;	gw     - level3 directories made with write permission for
;                group if they don't exist before
;       sp     - make also sp level3 file
;       longwsji use longes wavelength SJI image (useful for SST observations)
;       cpsji  - copy SJI images instead of linking them
;       verbose- print informational messages
;       silent - do not print messages or version number, overrides /verbose
;       replace- replace existing level3 data if already existing
;
; OUTPUTS:
;       level3 files written and level2 SJI linked with symbolic links
;       to level3 directory
;
; RESTRICTIONS:
;       does not work for special programs (like 42 series)
;
; PROCEDURE:
;       creates directories in level3 tree if they do not exist
;       runs iris_make_fits_level3
;       makes symbolic links of SJI files from level2 directory to
;       level3 directory
;
; EXAMPLE:
;       iris_obsl223,'20130829_060935_4000005156',/all,/sp,/verbose
;
; MODIFICATION HISTORY:
;       30-Sep-2013: Mats Carlsson - version 1.1
;       1.2  2013/10/04 matsc  - prints version number unless /silent
;       1.3  2013/10/25 matsc  - added keyword /replace
;       1.4  2013/10/31 matsc  - moved replace check to
;                                iris_make_fits_level3
;       1.5  2013/11/20 matsc  - also allows iwin to be given as a
;                                positional argument
;       1.6  2013/11/22 matsc  - passes on extra arguments to iris_make_fits_level3
;       1.7  2014/01/15 matsc  - added keyword /longwsji
;       1.8  2014/01/20 matsc  - added keyword /cpsji
;	1.9  2014/08/13 matsc  - added keyword /gw
;
; $Id: iris_obsl223.pro,v 1.10 2014/09/11 06:27:29 matsc Exp $;
;-
pro iris_obsl223,obs ,wins,iwin=iwin,all=all,sp=sp,rootl2=rootl2,rootl3=rootl3,gw=gw,longwsji=longwsji,cpsji=cpsji,$
 debug=debug,verbose=verbose,version=version,silent=silent,replace=replace,_extra=e

verstring="$Id: iris_obsl223.pro,v 1.10 2014/09/11 06:27:29 matsc Exp $"
if(~keyword_set(silent) or keyword_set(version)) then begin
  ic=strpos(verstring,'pro,v')+6
  message,'version '+strmid(verstring,ic,strlen(verstring)-ic-6),/info
endif
if(n_params() lt 1) then begin
  message,'Syntax: iris_obsl223,obs ,iwin=iwin,/all,/sp,rootl2=rootl2,rootl3=rootl3,/gw,/longwsji,/cpsji,/debug,/verbose,/silent,/replace,_extra=e',/info
  return
endif
if(keyword_set(silent)) then verbose=0
if(n_elements(wins) gt 0) and (n_elements(iwin) eq 0) then iwin=wins

; parameter check

if(strlen(obs) ne 26) then begin
  message,'obs should be of form yyyymmdd_hhmmss_obsid (e.g. 20130829_074924_4003005147)',/info
  return
endif

; default rootl2
if(n_elements(rootl2) eq 0) then begin
  rootl2=getenv('IRIS_DATA')+'/level2'
endif

; default rootl3
if(n_elements(rootl3) eq 0) then begin
  ic=strpos(rootl2,'level2')
  rootl3=rootl2
  strput,rootl3,'3',ic+5
endif

; check that rootl3 directory exists
dum=file_search(rootl3,count=count)
if(count ne 1) then begin
  message,rootl3+' does not exist',/info
  return
endif

; get year,month,date from obs

yyyy=strmid(obs,0,4)
mm=strmid(obs,4,2)
dd=strmid(obs,6,2)

cd,current=cwd   ; save current directory

; construct directory and check that it exists

dirl2=rootl2+'/'+yyyy+'/'+mm+'/'+dd+'/'+obs
dum=file_search(dirl2,count=count)
if(count ne 1) then begin
  message,dirl2+' does not exist',/info
  return
endif

; make level3 directories if they do not exist

cd,rootl3
dum=file_search(yyyy,count=count)
if(count ne 1) then spawn,'mkdir '+yyyy
if(keyword_set(gw)) then spawn,'chmod g+w '+yyyy
cd,yyyy
dum=file_search(mm,count=count)
if(count ne 1) then spawn,'mkdir '+mm
if(keyword_set(gw)) then spawn,'chmod g+w '+mm
cd,mm
dum=file_search(dd,count=count)
if(count ne 1) then spawn,'mkdir '+dd
if(keyword_set(gw)) then spawn,'chmod g+w '+dd
cd,dd
dum=file_search(obs,count=count)
if(count ne 1) then spawn,'mkdir '+obs
if(keyword_set(gw)) then spawn,'chmod g+w '+obs
cd,obs
cd,current=dirl3

if(keyword_set(verbose)) then begin
  message,'directory level2='+dirl2,/info
  message,'directory level3='+dirl3,/info
endif

cd,dirl2
fraster=file_search('iris_l2*raster_t000_*.fits',count=nraster)
if(nraster eq 0) then begin
  message,'No raster files found',/info
  cd,cwd
  return
endif else if(keyword_set(verbose)) then begin
  message,'Raster files included:',/info
  for i=0,nraster-1 do begin
    print,'  '+fraster[i]
  endfor
endif

; check iwin

d=obj_new('iris_data')
d->read,fraster[0]
nwin=d->getnwin()
if(keyword_set(all)) then iwin=indgen(nwin)
if(max(iwin) gt (nwin-1)) or (min(iwin) lt 0) then begin
  message,'iwin outside range [0,'+strtrim(nwin-1,2)+']',/info
  return
endif

fsji=file_search('iris_l2*SJI*.fits',count=nsji)
if(keyword_set(longwsji)) then begin
  clamb=strmid(fsji[nsji-1],39,4)
  f2sji=file_search('iris_l2*SJI_'+clamb+'*fits')
  sji=f2sji[0] ; use longest wavelength sji, align version if existing
endif else begin
  fsji14=file_search('iris_l2*SJI_1400*.fits',count=nsji14)
  if(nsji14 ne 0) then begin
    sji=fsji14[0]
  endif else if(nsji ne 0) then begin
    sji=fsji[0]
  endif
endelse
if(keyword_set(verbose) and (nsji ne 0)) then begin
  message,'SJI files:',/info
  for i=0,nsji-1 do begin
    print,'  '+fsji[i]
  endfor
  message,'SJI file used for iris_make_fits_level3:',/info
  print,'  ',sji
endif

; make level3 files

iris_make_fits_level3,fraster,iwin,sp=sp,wdir=dirl3,sji=sji,silent=silent,replace=replace,_extra=e

; link SJI files from dirl2 to dirl3

cd,dirl3
if(nsji ne 0) then begin
  if(keyword_set(cpsji)) then begin
    for i=0,nsji-1 do begin
      dum=file_search(fsji[i],count=count)
      if(count eq 0) then spawn,'cp '+dirl2+'/'+fsji[i]+' .'
    endfor
  endif else begin
    for i=0,nsji-1 do begin
      dum=file_search(fsji[i],count=count)
      if(count eq 0) then spawn,'ln -s '+dirl2+'/'+fsji[i]+' .'
    endfor
  endelse
endif

cd,cwd
if(keyword_set(debug)) then stop

end
