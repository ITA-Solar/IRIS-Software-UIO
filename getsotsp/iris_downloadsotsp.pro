;+
; NAME:
;       IRIS_downloadSotSP
;
; PURPOSE:
;       IRIS_downloadSotSP downloads the given files if necessary and if they haven't been downloaded yet
;
; CATEGORY:
;       Martin Wiesmann / IRIS Data processing
;       IRIS_getSotSPdata
;
; CALLING SEQUENCE:
;       IRIS_downloadSotSP, sot_sp_events, outdir [, files=files, debug=debug]
;
; INPUTS:
;       sot_sp_events: modified version of result from event download for SOT SP, provided by IRIS_getSotSPdata
;       outdir: The path into which the resulting files should be saved, downloaded files will be saved int subfolder 'original'
;
; OPTIONAL KEYWORD PARAMETERS:
;
; OUTPUTS:
;       files: a pointer array, one pointer per event, each pointer points to a string array of filenames
;
; CALLS:
;       This process is not called by the user, but by IRIS_processSotSPrequest
;
; COMMON BLOCKS:
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;       31-Oct-2019: Martin Wiesmann (ITA, UIO)
;
; $Id: iris_downloadsotsp.pro,v 1.2 2020/03/23 19:13:27 mawiesma Exp $  ;



pro IRIS_downloadSotSP, sot_sp_events, outdir, files=files, deletetempfiles=deletetempfiles, debug=debug

  if N_ELEMENTS(sot_sp_events) eq 0 then return
  
  dirsep = path_sep()
  if strmid(outdir, 0,1, /reverse_offset) ne dirsep then outdir = outdir+dirsep
  outdirorig = outdir+'original' + dirsep
  file_mkdir, outdirorig
  
  files = ptrarr(N_ELEMENTS(sot_sp_events))

  for ievent=0,N_ELEMENTS(sot_sp_events)-1 do begin
    compurl = sot_sp_events[ievent].philsp_compurl
    if strmatch( compurl,'*lmsal.com*' ) eq 1 then begin
      compurl = str_replace(compurl,'http://','https://')
    endif
    print, 'downloading file: ' + compurl
    tempfile = outdirorig + file_basename(sot_sp_events[ievent].philsp_compurl)
    
    if ~file_test(tempfile) then begin
      sock_list, compurl, zipfile, /buffer
      openw, 1, tempfile
      writeu, 1, zipfile
      close, 1
    endif

    file_untar, tempfile, /verbose, files=files1
    if keyword_set(deletetempfiles) then file_delete, tempfile
    
    files[ievent] = ptr_new(files1)
  endfor
end
