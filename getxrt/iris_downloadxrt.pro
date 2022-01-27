;+
; NAME:
;       IRIS_downloadXRT
;
; PURPOSE:
;       IRIS_downloadXRT downloads the given files if necessary and if they haven't been downloaded yet
;
; CATEGORY:
;       Martin Wiesmann / IRIS Data processing
;       IRIS_getXRTdata
;
; CALLING SEQUENCE:
;       IRIS_downloadXRT, umodes, files, outdir, fileurl, obs2fov [, urls=urls, fulltime=fulltime, debug=debug, redownload=redownload]
;
; INPUTS:
;       umodes: modified version of result from xrt_umodes, provided by IRIS_getXRTdata
;       files: a pointer array, one pointer per umode, each pointer points to a string array of filenames
;       outdir: The path into which the resulting files should be saved, downloaded files will be saved int subfolder 'original'
;       fileurl: boolean, indicates whether filenames are urls
;       urls: the urls of the original files to download
;       obs2fov: An obs2fov object with the IRIS OBS as input
;       fulltime: If set, all HINODE files of the selected umodes will be used, regardless of the time window
;       redownload: If set, all files will be downloaded even if they have been downloaded already,
;           if not set, the program first checks for each file whether it already exist in the outdir
;
; OPTIONAL KEYWORD PARAMETERS:
;       fulltime: If set, all HINODE files of the selected umodes will be used, regardless of the time window
;       redownload: If set, all files will be downloaded even if they have been downloaded already,
;           if not set, the program first checks for each file whether it already exist in the outdir
;
; OUTPUTS:
;
; CALLS:
;       This process is not called by the user, but by IRIS_processXRTrequest
;
; COMMON BLOCKS:
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;       15-Feb-2019: Martin Wiesmann (ITA, UIO)
;
; $Id: iris_downloadxrt.pro,v 1.5 2019/06/04 12:30:35 mawiesma Exp $  ;



pro IRIS_downloadXRT, umodes, files, outdir, fileurl, obs2fov, urls=urls, fulltime=fulltime, debug=debug, redownload=redownload, redol2=redol2

  dirsep = path_sep()
  if strmid(outdir, 0,1, /reverse_offset) ne dirsep then outdir = outdir+dirsep

  obs2fov->get_startendtimes, starttime, endtime
  startdate=anytim2cal(starttime,form=8)
  stopdate=anytim2cal(endtime,form=8)

  nfiles=0
  for iumodes=0,N_ELEMENTS(umodes)-1 do begin

;    doit = 1
;    
;    if doit eq 0 then begin
;      box_message, 'this mode is not (yet) implemented'
;      *files[iumodes] = ''
;      *urls[iumodes] = ''
;      umodes[iumodes].datatype = ''
;      continue
;    endif


    if keyword_set(fulltime) then begin
      count = N_ELEMENTS(*files[iumodes])
    endif else begin
      fitsfiledates = anytim2cal(file2time(file_basename(*files[iumodes])), form=8)
      fileind=where((fitsfiledates ge startdate) AND (fitsfiledates le stopdate) AND $
        (strmatch(*files[iumodes], '*d.fits') eq 0), count)
      if count gt 0 then begin
        *files[iumodes] = (*files[iumodes])[fileind]
        *urls[iumodes] = (*urls[iumodes])[fileind]
      endif else begin
        *files[iumodes] = ''
        *urls[iumodes] = ''
        umodes[iumodes].datatype = ''
      endelse
    endelse
    if keyword_set(debug) then begin
      if count gt 3 then begin
        *files[iumodes] = (*files[iumodes])[0:2]
        *urls[iumodes] = (*urls[iumodes])[0:2]
        count=3
      endif
    endif
    nfiles=nfiles+count
  endfor

  if fileurl then begin
    print,'downloading HINODE XRT files...'
    ;we have to download and save all the files
    outdirorig = outdir+'original/'
    file_mkdir, outdirorig

    nfileread=0
    for iumodes=0,N_ELEMENTS(umodes)-1 do begin
      if umodes[iumodes].datatype ne '' then begin
        for iss=0,N_ELEMENTS(*files[iumodes])-1 do begin
          print, strcompress(string(iumodes+1), /remove_all) + ' of ' + strcompress(string(N_ELEMENTS(umodes)), /remove_all) $
            + ' : ' + string(double(nfileread)/double(nfiles)*100d, format='(F6.2)') + '% ; ' $
          + strcompress(string(iss+1), /remove_all) + ' of ' + strcompress(string(N_ELEMENTS(*files[iumodes])), /remove_all) $
            + ' : ' + string(double(iss)/double(N_ELEMENTS(*files[iumodes]))*100d, format='(F6.2)') + '% : ' $
            + file_basename((*files[iumodes])[iss])

          outfile = outdirorig+file_basename((*urls[iumodes])[iss])
          if ~file_test(outfile) || keyword_set(redownload) || file_test(outfile,/zero_length) then begin
            sock_list, (*urls[iumodes])[iss], bindata, /buffer
            openw,lun, outfile, /get_lun
            writeu, lun, bindata
            free_lun, lun
            redol2 = 1
          endif
          (*files[iumodes])[iss] = outfile
          nfileread = nfileread+1
        endfor
      endif
    endfor

  endif else begin
    file_mkdir, outdir
  endelse

end
