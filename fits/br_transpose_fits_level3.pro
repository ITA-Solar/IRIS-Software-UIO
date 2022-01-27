pro br_transpose_fits_level3,filename_im,tmp_size=tmp_size,verbose=verbose,filename_sp=filename_sp,silent=silent
;
;+
; NAME:
;	BR_TRANSPOSE_FITS_LEVEL3
;
; PURPOSE:
;       make crispex "spectral" or sp cube based on previously
;       constructed "image" or im cube
;
; CATEGORY:
;       BIFROST
;       IRIS
;       CRISPEX support software
;	
; CALLING SEQUENCE:
;	BR_TRANSPOSE_FITS_LEVEL3,filename_im
;	         ,tmp_size=tmp_size,verbose=verbose,filename_sp=filename_sp,/silent
;
; INPUTS:
;       filename_im - filename of image file
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;       filename_sp - spectral file filename, default is image
;                     filename with "_sp" extension instead of "_im"
;       tmp_size - max temporary memory size
;       verbose  - should I be chatty?
;       silent   - don't be chatty
;
; OUTPUTS:
;       writes data to fits file named filename_sp
;	also copies exten_no=1 and exten_no=2 from image filename
;	(normally lambda/z and t variable)
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURES USED:
;      Procedures: FXHREAD, FXPAR, SXADDPAR, WRITEFITS, READFITS
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;   v1.1 31-Oct-2012 Mats Carlsson - almost identical to br_transpose_crisp_fits
;                    by Viggo Hansteen but copying hdr information instead of
;                    constructing new info
;   v1.1 17-Aug-2013 Viggo Hansteen - copied over from Bifrost tree
;   v1.2 08-Oct-2013 Mats Carlsson - corrected spelling mistake
;   v1.3 09-Oct-2013 Mats Carlsson - added /silent, added version string
;                      removed call to readfits to solve 64-bit problem on early Mac IDL versions
;                      added support for int-type
;   $Id: br_transpose_fits_level3.pro,v 1.3 2013/10/09 10:46:38 matsc Exp $
;-
;
verstring="$Id: br_transpose_fits_level3.pro,v 1.3 2013/10/09 10:46:38 matsc Exp $"
If(~keyword_set(silent)) then begin
  ic=strpos(verstring,'pro,v')+6
  message,'version '+strmid(verstring,ic,strlen(verstring)-ic-6),/info
endif
if n_params() ne 1 then begin 
  message,'br_transpose_fits_level3,filename_im',/cont
  message,' ,tmp_size=tmp_size,verbose=verbose,filename_sp=filename_sp',/cont
  return
endif
;
if n_elements(filename_sp) eq 0 then begin
  filename_sp=filename_im
  strput,filename_sp,'sp',strpos(filename_im,'.fits')-2
endif
;
openr,lur,filename_im,/get_lun,/swap_if_little_endian
;
fxhread,lur,hdr_im,status
hdr_sp=hdr_im
naxis=fxpar(hdr_im,'NAXIS*')
nx=naxis[0]
ny=naxis[1]
nz=naxis[2]
nt=naxis[3]
;
caxis=string(indgen(4)+1,format='(i1)')                 ; axis number as string
oaxis=[2,3,0,1]                                         ; new axis i is old axis oaxis[i]
keys=['NAXIS','CDELT','CRPIX','CRVAL','CTYPE','CUNIT']  ; keywords that should be transposed
for j=0,n_elements(keys)-1 do begin
  cvar=fxpar(hdr_im,keys[j]+'*',comment=comment)
  for i=0,3 do sxaddpar,hdr_sp,keys[j]+caxis[i],cvar[oaxis[i]],comment[oaxis[i]]
endfor
;
; write hdr_sp to filename_sp
;
openw,luw,filename_sp,/get_lun,/swap_if_little_endian
nmax=n_elements(hdr_sp)
endline=nmax-1
nblock=endline*80/2880+1
offset=nblock*2880
bvar=replicate(32b,offset)
for n=0l,nmax-1 do bvar[n*80:n*80+79]=byte(hdr_sp[n])
var=assoc(luw,bvar)
var[0]=bvar
;
; transpose data
; write transposed data to filename_sp
;
gb=1024.^3      ; 1 GB
if(n_elements(tmp_size) eq 0) then tmp_size=12.    ; max temporary storage size in GB
my=long(tmp_size*gb/nz/nt/nx/4) < ny
if(my lt 1) then begin
   message,'max temporary storage ('+strtrim(string(tmp_size),2)+' GB) not big enough for operation',/info
  return
endif
if(keyword_set(verbose)) then begin
  nsweep=(ny*2-1)/my
  print,'my,ny,nt,nsweeps=',my,ny,nt,nsweep
endif
;
bitpix=fxpar(hdr_im,'BITPIX')
case bitpix of
  16: begin
        tmp=intarr(nz,nt,nx,my)  ; temporary array
        imr=intarr(nx,ny,nz)
        imw=intarr(nz,nt,nx)
      end
 -32: begin
        tmp=fltarr(nz,nt,nx,my)  ; temporary array
        imr=fltarr(nx,ny,nz)
        imw=fltarr(nz,nt,nx)
      end
else: begin
        message,'only works for int or float data',/info
        free_lun,lur
        free_lun,luw
        return
      end
endcase
;
recr=assoc(lur,imr,offset)   ; input record
recw=assoc(luw,imw,offset)   ; output record
if(keyword_set(verbose)) then mtimer,/start
irec=0L
for y0=0,ny-1,my do begin ; need to read input file repeated times
  y1=(y0+my-1) < (ny-1)   ; last y index  
  for i=0,nt-1 do begin
    if(keyword_set(verbose)) then mtimer,'br_transpose_fits_level3',irec,nsweep*nt,/remain
    irec=irec+1
    imr=recr[i]
    imt=transpose(imr,[2,0,1])
    tmp[*,i,*,0:y1-y0]=imt[*,*,y0:y1]
  endfor
  for i=y0,y1 do recw[i]=tmp[*,*,*,i-y0]
endfor
if(keyword_set(verbose)) then mtimer,/stop
;
; pad data-records to end on 2880 byte boundary
;
ndata=long64(nx*ny)*long64(nz*nt)
nbytes=long64(ndata)*(abs(bitpix)/8)
offset=nbytes+offset
npad=2880-(nbytes mod 2880)
if(npad eq 2880) then npad=0
if(npad gt 0) then begin
  var=assoc(luw,bytarr(2880-npad),offset)
  var[0]=replicate(0b,2880-npad)
endif
;
; byte copy rest of file (normally extensions - done this way to ;                                               bug in early IDL
;                                               versions on Mac)
;
stat=fstat(lur)
offset=offset+npad
nrest=stat.size-offset
recr=assoc(lur,bytarr(nrest),offset)
recw=assoc(luw,bytarr(nrest),offset)
recw[0]=recr[0]
;
free_lun,lur
free_lun,luw
end
    
