; $Id: iris_downloadhinode.pro,v 1.12 2019/07/04 09:31:04 mawiesma Exp $  ;

pro iris_downloadHINODE, umodes, files, outdir, no_download, obs2fov, fulltime=fulltime, debug=debug

  dirsep = path_sep()
  if strmid(outdir, 0,1, /reverse_offset) ne dirsep then outdir = outdir+dirsep

  obs2fov->get_startendtimes, starttime, endtime
  startdate=anytim2cal(starttime,form=8)
  stopdate=anytim2cal(endtime,form=8)

  nfiles=0
  for iumodes=0,N_ELEMENTS(umodes)-1 do begin

    case umodes[iumodes].obs_type of
      'FG (simple)': begin
        case umodes[iumodes].wave of
          'CN bandhead 3883': doit=1
          'Ca II H line': doit=1
          'G band 4305': doit=1
          'blue cont 4504': doit=1
          'green cont 5550': doit=1
          'red cont 6684': doit=1
          'TF H I 6563': doit=1
          'TF H I 6563 base': doit=1
          else: doit=0
        endcase
      end

      'FG MG4 V/I': begin
        doit=2
        umodes[iumodes].obs_type = 'FG MG4 V-I' ;needs to be changed, because filenames with / are a nuisance
      end

      'FG shuttered I and V': doit=2
      'FG shuttered IV+DG': doit=2
      'FG shutterless I and V': doit=3
      'FG shutterless I and V with 0.2s intervals': doit=3
      'FG shutterless Stokes': doit=3

      else: doit=0
    endcase

    if doit eq 0 then begin
      box_message, 'this mode is not (yet) implemented'
      *files[iumodes] = ''
      umodes[iumodes].obs_type = ''
      continue
    endif


    if keyword_set(fulltime) then begin
      count = N_ELEMENTS(*files[iumodes])
    endif else begin
      fitsfiledates = anytim2cal(file2time(file_basename(*files[iumodes])), form=8)
      fileind=where((fitsfiledates ge startdate) AND (fitsfiledates le stopdate), count)
      if count gt 0 then begin
        *files[iumodes] = (*files[iumodes])[fileind]
      endif else begin
        *files[iumodes] = ''
        umodes[iumodes].obs_type = ''
      endelse
    endelse
    if keyword_set(debug) then begin
      if count gt 3 then begin
        *files[iumodes] = (*files[iumodes])[0:2]
        count=3
      endif
    endif
    nfiles=nfiles+count
  endfor

  file_mkdir, outdir
  outdirorig = outdir+'original/'
  file_mkdir, outdirorig

  if ~no_download then begin
    ;we have to download and save all the files

    nfileread=0
    for iumodes=0,N_ELEMENTS(umodes)-1 do begin
      if umodes[iumodes].obs_type ne '' && umodes[iumodes].fileurl then begin
        print,'downloading HINODE files...'
        for iss=0,N_ELEMENTS(*files[iumodes])-1 do begin
          print, strcompress(string(iumodes+1), /remove_all) + ' of ' + strcompress(string(N_ELEMENTS(umodes)), /remove_all) $
            + ' : ' + string(double(nfileread)/double(nfiles)*100d, format='(F6.2)') + '% ; ' $
          + strcompress(string(iss+1), /remove_all) + ' of ' + strcompress(string(N_ELEMENTS(*files[iumodes])), /remove_all) $
            + ' : ' + string(double(iss)/double(N_ELEMENTS(*files[iumodes]))*100d, format='(F6.2)') + '% : ' $
            + file_basename((*files[iumodes])[iss])

          outfile = outdirorig+file_basename((*files[iumodes])[iss])
          if ~file_test(outfile) then begin
            sock_list, (*files[iumodes])[iss], bindata, /buffer
            openw,lun, outfile, /get_lun
            writeu, lun, bindata
            free_lun, lun
          endif
          (*files[iumodes])[iss] = outdirorig+file_basename((*files[iumodes])[iss])
          nfileread = nfileread+1
        endfor ;iss=0,N_ELEMENTS(*files[iumodes])-1
      endif ;umodes[iumodes].obs_type ne '' && umodes[iumodes].fileurl
    endfor ;iumodes=0,N_ELEMENTS(umodes)-1
  endif
end
