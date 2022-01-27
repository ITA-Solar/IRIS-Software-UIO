pro IRIS_rdfits_struct, filename, struct, n_extensions=n_extensions, SILENT=silent, $
  HEADER_ONLY=header_only, pointer_array=pointer_array, EXTEN=exten, fitsheadstruct=fitsheadstruct
  ;+
  ; NAME:
  ;      IRIS_rdfits_struct
  ; PURPOSE:
  ;      Read an entire FITS file (all extensions) into a single IDL structure.
  ; EXPLANATION:
  ;      Each header, image or table array is placed in a separate structure
  ;      tag.
  ;
  ; CALLING SEQUENCE:
  ;      IRIS_rdfits_struct, filename, struct [, n_extensions=n_extensions, $
  ;         /SILENT, /HEADER_ONLY, pointer_array=pointer_array, EXTEN=extensions ]
  ;
  ; INPUT:
  ;      FILENAME = Scalar string giving the name of the FITS file.
  ;                 One can also specify a gzip (.gz) compressed file
  ;
  ; OPTIONAL KEYWORD:
  ;      /HEADER_ONLY - If set, then only the FITS headers (and not the data)
  ;                are read into the structure.
  ;      /SILENT - Set this keyword to suppress informational displays at the
  ;               terminal.
  ;      /pointer_array - If set, the output structure will contain to pointer
  ;               arrays: 'hdr' and 'data' only. In each there are number of extensions +1
  ;               pointers. Each pointer in 'hdr' points to a header of the extension
  ;               with the same index. Each pointer in 'data' contains the data
  ;               of its extension.
  ;      /fitsheadstruct = If set, the headers will be converted from string arrays
  ;               into structures using fitshead2struct.
  ; OUTPUT:
  ;      struct = structure into which FITS data is read.   The primary header
  ;             and image are placed into tag names HDR0 and IM0.   The ith
  ;             extension is placed into the tag names HDRi, and either TABi
  ;             (if it is a binary or ASCII table) or IMi (if it is an image
  ;             extension)
  ;
  ;             If /HEADER_ONLY is set, then struct will contain tags HDR0, HDR1
  ;             ....HDRn containing all the headers of a FITS file with n
  ;             extensions
  ;             
  ;             If /pointer_array is set, then struct will contain tags 'HDR'
  ;             and 'DATA' (in case /HEADER_ONLY has not been set). These tags
  ;             contain pointer arrays with as many elements as there are
  ;             extensions in the FITS file +1 for extension=0.
  ;
  ;      n_extensions = number of extensions in the FITS file.
  ; OPTIONAL INPUT KEYWORD:
  ;       EXTEN - positive integer array specifying which extensions to read.
  ;             Default is to read all extensions.
  ; PROCEDURES USED:
  ;       FITS_OPEN, FITS_READ, FITS_CLOSE, FITSHEAD2STRUCT
  ;
  ; METHOD:
  ;       The file is opened with FITS_OPEN which return information on the
  ;       number and type of each extension. The CREATE_STRUCT() function
  ;       is used iteratively, with FITS_READ calls to build the final structure.
  ;       Alternatively, if keyword pointer_array is set, the results of FITS_READ
  ;       are set in a list in form of pointers.
  ;
  ; EXAMPLE:
  ;       Read the FITS file 'm33.fits' into an IDL structure, st
  ;
  ;       IDL> IRIS_rdfits_struct, 'm33.fits', st
  ;       IDL> help, /str, st                   ;Display info about the structure
  ;
  ;       To just read the second and fourth extensions
  ;       IDL> IRIS_rdfits_struct, 'm33.fits', st, exten=[2,4]
  ; RESTRICTIONS:
  ;       Does not handle random groups or variable length binary tables
  ; MODIFICATION HISTORY:
  ;       Written K. Venkatakrishna, STX April 1992
  ;       Code cleaned up a bit  W. Landsman  STX  October 92
  ;       Modified for MacOS     I.  Freedman  HSTX April 1994
  ;       Work under Windows 95  W. Landsman   HSTX  January 1996
  ;       Use anonymous structures, skip extensions without data WBL April 1998
  ;       Converted to IDL V5.0, W. Landsman, April 1998
  ;       OS-independent deletion of temporary file  W. Landsman  Jan 1999
  ;       Major rewrite to use FITS_OPEN and CREATE_STRUCT() W. Landsman Sep 2002
  ;       Added /HEADER_ONLY keyword   W. Landsman  October 2003
  ;       Do not copy primary header into extension headers W. Landsman Dec 2004
  ;       Do not modify NAXIS when using /HEADER_ONLY W. Landsman Jan 2005
  ;       Added EXTEN keyword  W. Landsman July 2009
  ;       Renamed ('Branched') procedure from rdfits_struct to IRIS_rdfits_struct
  ;       Added n_extensions output keyword  M. Wiesmann October 2019
  ;       Added pointer_array keyword  M. Wiesmann October 2019
  ;       Added fitsheadstruct keyword  M. Wiesmann October 2019
  ;-

  compile_opt idl2
  if N_Params() LT 2 then begin
    print,'Syntax - IRIS_rdfits_struct, file, struct, [ /SILENT, /HEADER_ONLY ]'
    return
  endif

  fits_open, filename, fcb                ; Get the description of the file
  n_extensions = fcb.nextend
  if ~keyword_set(silent) then $
    message,/inf,'Now reading file ' + filename + ' with ' + $
    strtrim(n_extensions,2) + ' extensions'

  if n_extensions EQ 0 then begin
    fits_close,fcb
    return
  endif

  n = N_elements(exten)
  if N_elements(exten) EQ 0 then begin
    n = n_extensions
    exten = indgen(n)+1
  endif else begin
    if max(exten) GT n_extensions then message, $
      'ERROR - extension ' + strtrim(max(exten),2) + ' does not exist'
  endelse

  h_only = keyword_set(header_only)
  fheadstruct = keyword_set(fitsheadstruct)
  p_array = keyword_set(pointer_array)
  if p_array then begin
    if h_only then struct = {hdr: ptrarr(n_extensions+1)} $
    else struct = {hdr: ptrarr(n_extensions+1), data: ptrarr(n_extensions+1)}
  endif

  if h_only then begin
    fits_read,fcb,0,h,/header_only,exten_no=0
    if fheadstruct then h=fitshead2struct(h)
    if p_array then struct.hdr[0] = ptr_new(h) $
    else struct = {hdr0:h}
  endif else begin
    fits_read,fcb,d,h,exten_no=0
    if fheadstruct then h=fitshead2struct(h)
    if p_array then begin
      struct.hdr[0] = ptr_new(h)
      struct.data[0] = ptr_new(d)
    endif else begin
      struct = {hdr0:h,im0:temporary(d)}
    endelse
  endelse

  for i= 0, n-1 do begin
    j = exten[i]
    jj  = strtrim(j,2)
    if h_only then begin
      fits_read,fcb,0,h,/header_only,/no_pdu,exten=j
      if fheadstruct then h=fitshead2struct(h)
      if p_array then struct.hdr[j] = ptr_new(h) $
      else struct = create_struct(temporary(struct), 'hdr' + jj, $
        temporary(h))
    endif else begin
      fits_read,fcb,d,h,/no_pdu,exten=j
      if fheadstruct then h=fitshead2struct(h)
      if p_array then begin
        struct.hdr[j] = ptr_new(h)
        struct.data[j] = ptr_new(d)
      endif else begin
        if fcb.xtension[j] EQ 'IMAGE' then tag = 'im' + jj $
        else tag = 'tab' + jj
        struct = create_struct(temporary(struct), 'hdr' + jj, $
          temporary(h),tag, temporary(d))
      endelse
    endelse
  endfor

  fits_close,fcb
  return
end
