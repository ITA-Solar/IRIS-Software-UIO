;+
; NAME:
;       IRISl12_shiftwave
;
; PURPOSE:
;       IRISl12_shiftwave corrects the wavelength information in the main header and the extension headers for the FUV2 (or FUVL) windows
;
; CATEGORY:
;       Martin Wiesmann / IRIS Data processing
;
; CALLING SEQUENCE:
;       IRISl12_shiftwave, files, waveshift
;
; INPUTS:
;       files: list of IRIS l2 files, SJI files will be ignored
;       waveshift: the amount by which the wavelength of FUV2 windows should be corrected (in Angstrom)
;
; OUTPUTS:
;       The headers of the raster files will be modified
;
; PROCEDURE:
;       If this correction already has been applied to an input file (according to HISTORY keyword), the file will be ignored
;       The correction will applied only to files which were created with the iris_mk_pointdb version 8
;       keywords in the main header that will be edited are TWMINx and TWMAXx (x: window numbers of FUV2 windows)
;       keyword in the extensions of the FUV2 windows that will be edited is CRVAL1
;       to all the above keywords waveshift is added
;       TDESCx and TWAVEx will not be edited
;       Information of this correction and which keywords were edited will be written into the HISTORY keyword
;
; MODIFICATION HISTORY:
;       2016-12-12: Martin Wiesmann (ITA, UIO).
;
; $Id: irisl12_shiftwave.pro,v 1.6 2017/01/23 10:42:12 mawiesma Exp $  ;

pro IRISl12_shiftwave, files, waveshift

  ;files = '~/data/modtest/iris_l2_20150412_175043_3893012099_raster_t000_r00000.fits'
  ;waveshift = 10000

  indrast = where(strmatch(files, '*_raster_*.fits') eq 1, count)
  if count eq 0 then begin
    box_message, 'need raster-file(s) in input (*_raster_*.fits)'
    return
  endif
  if ~isnumeric(waveshift) then begin
    box_message, 'waveshift needs to be a number'
    return
  endif

  file = files[indrast]

  for ifile=0,N_ELEMENTS(file)-1 do begin
    ;read mainheader
    h = headfits(file[ifile])
    
    ;check whether change has been applied already
    hist = fxpar(h, 'history')
    ;for i=0,N_ELEMENTS(hist)-1 do print,hist[i]
    ind = where(strmatch(hist, '*FUV2 wavelengths corrected post-level1to2*') eq 1, count)
    if count gt 0 then begin
      box_message, ['wavelength correction already applied', file[ifile]]
      continue
    endif
    
    ;check iris_mk_pointdb version, has to be 8, else ignore
    ind = where(strmatch(hist, '*iris_mk_poin*') eq 1, count)
    if count eq 0 then begin
      box_message, ['cannot find version of iris_mk_pointdb in history', file[ifile]]
      continue
    endif else if count gt 1 then begin
      box_message, ['iris_mk_pointdb version ambiguous', file[ifile]]
      continue
    endif else begin
      verstext = strsplit(hist[ind[0]],/extract)
      ind = where(strmatch(verstext,'ver',/Fold_case) eq 1, count)
      if count ne 1 then begin
        box_message, ['cannot find version of iris_mk_pointdb in history', file[ifile]]
        continue
      endif else begin
        version = fix(verstext[ind[0]+1])
        if version ne 8 then begin
          box_message, ['iris_mk_pointdb version is not 8', file[ifile]]
          continue
        endif
      endelse
    endelse
    
    ;check iris_prep version
    ;prepversion = fxpar(h, 'iprpver')
    ; TODO ; obsolete

    tdet = strtrim(fxpar(h, 'tdet*'), 2)
    indfuv2 = where(tdet eq 'FUV2', count)
    if count eq 0 then begin
      box_message, ['rasterfile does not contain FUV2 window(s)', file[ifile]]
      continue
    endif
    twmin = fxpar(h, 'twmin*')
    twmax = fxpar(h, 'twmax*')
    
    fits_open,file[ifile],io,/update    ;Faster to explicity open (for extensions)
    changedkeywords2=''
    changedext2=''
    for i=0,count-1 do begin
      ;change main header
      number = string(indfuv2[i]+1, format='(I1)')
      newmin = double(twmin[indfuv2[i]]) + double(waveshift)
      newmax = double(twmax[indfuv2[i]]) + double(waveshift)
      sxaddpar, h, 'TWMIN'+number, newmin
      sxaddpar, h, 'TWMAX'+number, newmax
      
      ;read and change header of extensions
      fits_read,io,0,hext,/header_only,exten_no=indfuv2[i]+1,/No_PDU ;Get header (for extensions)
      crval1 = fxpar(hext, 'CRVAL1')
      newcrval1 = double(crval1) + double(waveshift)
      sxaddpar, hext, 'CRVAL1', newcrval1
      modfits,io,0,hext,exten_no=indfuv2[i]+1        ;Update header

      ;compile history addition
      if i eq 0 then changedkeywords1 = 'KW edited: TWMIN'+number + ',' + 'TWMAX'+number $
      else if i gt 0 && i lt 4 then changedkeywords1 = changedkeywords1 + ',TWMIN'+number + ',' + 'TWMAX'+number $
      else if i eq 4 then changedkeywords2 = 'TWMIN'+number + ',' + 'TWMAX'+number $
      else changedkeywords2 = changedkeywords2 + ',TWMIN'+number + ',' + 'TWMAX'+number
      if i eq 0 then changedext1 = 'CRVAL1 (ext'+number+')' $
      else if i gt 0 && i lt 5 then changedext1 = changedext1 + 'CRVAL1 (ext'+number+')' $
      else if i eq 5 then changedext2 = 'CRVAL1 (ext'+number+')' $
      else changedext2 = changedext2 + 'CRVAL1 (ext'+number+')'
    endfor
    fits_close, io
    
    ;add info to history
    get_utc,utc,/ccsds,/date_only
    sxaddhist,['FUV2 wavelengths corrected post-level1to2', $
      'on ' + utc + ' by ' + string(waveshift, format='(f9.6)') + ' Angstrom', $
      changedkeywords1], h
    if changedkeywords2 ne '' then sxaddhist, changedkeywords2, h
    sxaddhist, changedext1, h
    if changedext2 ne '' then sxaddhist, changedext2, h

    ;modify mainheader
    modfits,file[ifile],0,h
    
    print, 'modified header of ' + file[ifile]
  endfor
end
