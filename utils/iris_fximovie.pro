;+
; NAME:
;       IRIS_FXIMOVIE
;
; PURPOSE:
;       Show movie of 3D-data in Fits file. Wrapper around
;       iris_ximovie for fitsfiles
;
; CATEGORY:
;       IRIS
;
; CALLING SEQUENCE:
;       iris_fximovie,filename ,_extra=e,/debug,/silent
;
; INPUTS:
;       filename - name of fitsfile
;
; KEYWORD PARAMETERS:
;       exten_no - number of extension with data
;       _extra   - parameters transferred to iris_ximovie
;       silent   - if set, does not printout version number
;
; RESTRICTIONS:
;       if two files are shown (for blinking) they have to have the
;       same xsize,ysize
;
; EXAMPLE:
;       iris_fximovie,sjifile,afile2=sjifile2     shows two SJI files
;
; MODIFICATION HISTORY:
;       1.1  2013/10/24 matsc   first version
;
;-
pro iris_fximovie,filename ,exten_no=exten_no,_extra=e,debug=debug,silent=silent

verstring="$Id: iris_fximovie.pro,v 1.1 2013/10/26 11:05:41 matsc Exp $"
if(~keyword_set(silent)) then begin
  ic=strpos(verstring,'pro,v')+6
  message,'version '+strmid(verstring,ic,strlen(verstring)-ic-6),/info
endif

if(n_params() lt 1) then begin
  message,'Syntax: iris_fximovie,filename ,ext=ext,_extras=e,/debug,/silent',/info
  return
endif
if N_elements(exten_no) EQ 0 then exten_no = 0
dum=file_search(filename,count=count)
if(count ne 1) then begin
  message,'file does not exist: '+filename,/info
  return
endif

offset=iris_fitspointer(filename,header,exten_no=exten_no,/silent)

if(fxpar(header,'NAXIS') ne 3) then begin
  message,'only works for 3D cubes',/info
  return
endif

xsize=fxpar(header,'NAXIS1')
ysize=fxpar(header,'NAXIS2')
nframes=fxpar(header,'NAXIS3')
bitpix=abs(fxpar(header,'BITPIX'))
bscale=fxpar(header,'BSCALE')
if(bscale eq 0) then bscale=1.
bzero=fxpar(header,'BZERO')
scaling1=[bscale,bzero]
scaling2=[bscale,bzero]
case bitpix of
  8 : byte=1
 16 : int=1
 32 : float=1
 64 : double=1
endcase
if(keyword_set(debug)) then stop
iris_ximovie,filename,xsize,ysize,nframes=nframes,_extra=e,offset=offset,title=file,int=int,float=float,double=double,/swap,imin=imin,imax=imax,$
 scaling1=scaling1,scaling2=scaling2

end
