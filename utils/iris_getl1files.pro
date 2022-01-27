;+
; NAME:
;       IRIS_getL1files
;
; PURPOSE:
;
;       IRIS_getL1files returns the level 1 files used in a level 2 file
;
; CATEGORY:
;       Data analysis IRIS
;
; CALLING SEQUENCE:
;       result = IRIS_getL1files(l2file [,/nuv, nfiles=nfiles]
;
; INPUTS:
;       l2file: Level 2 filename
;
; KEYWORD PARAMETERS:
;       nuv: if the file is a raster, the function returns by default the FUV-files,
;         set this keyword to return the NUV-files
;       nfiles: maximum number of files to be returned
;
; OUTPUTS:
;       string array, containing level 1 filenames with full paths
;
; CALLS:
;       mreadifts_header, readfits, gt_tagval
;
; COMMON BLOCKS:
;       NONE
;
; PROCEDURE:
;       IRIS_getL1files reads first the main data header to determine whether it deals
;       with a SJI-file or a rasterfile, and to determine the number of windows,
;       then it loads the respective extension and returns the result
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;       12-Sep-2013: Martin Wiesmann
;
;$Id: iris_getl1files.pro,v 1.2 2013/10/08 15:36:20 mawiesma Exp $

function IRIS_getL1files, l2file, nuv=nuv, nfiles=nfiles
  mreadfits_header, l2file, hdr, only_tags='NWIN,INSTRUME'
  if hdr.instrume eq 'SJI' then begin
    fileind = 'FILES'
    filelen = 'LFILES'
    sji=1
  endif else begin
    if keyword_set(nuv) then begin
      fileind = 'FILEN'
      filelen = 'LFILEN'
    endif else begin
      fileind = 'FILEF'
      filelen = 'LFILEF'
    endelse
    sji=0
  endelse
  d = readfits(l2file, h, exten_no=hdr.NWIN+2-sji)
  ind = gt_tagval(h, fileind)
  len = gt_tagval(h, filelen)
  if len eq 0 then begin
    box_message,'all level 1(.5) files missing'
    l1files=''
  endif else begin
    l1files = string(d[ind:ind+len-1,*])
    if keyword_set(nfiles) then if N_ELEMENTS(l1files) gt nfiles then l1files=l1files[0:nfiles-1]
  endelse
  return, l1files
end
