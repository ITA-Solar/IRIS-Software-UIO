function iris_files,filter,subs=subs,path=path
  
  if n_elements(filter) eq 0 then filter='*.fits'
  if ~keyword_set(path) then path='./'
  if strmid(path,0,/reverse) ne path_sep() then path=path+path_sep()
  if keyword_set(subs) then f=file_search(path, filter) $
  else f=file_search(path+filter)
  r=file_info(f)
  if f[0] ne '' then begin
    for i=0,n_elements(f)-1 do begin
      fs=r[i].size
      if fs gt 1024 then begin
        fs=fs/1024d
        unit='kB'
        if fs gt 1024 then begin
          fs=fs/1024d
          unit='MB'
          if fs gt 1024 then begin
            fs=fs/1024d
            unit='GB'
          endif
        endif
      endif else unit='B'
      print,i,'  ',f[i],string(fs,format='(I8)'),' ',unit
    endfor
  endif else message,'no files found',/info
  return,f
end
