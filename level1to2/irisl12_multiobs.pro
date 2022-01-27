PRO IRISl12_multiOBS, level1dirs, outputfolders, OBSid=OBSid, l1to2log=l1to2log, $
    scaled=scaled, maxdeviation=maxdeviation, xmlparentfolder=xmlparentfolder, outdir=outdir, $
    SJIonly=SJIonly, spectralonly=spectralonly, preserveINF=preserveINF, preserveNAN=preserveNAN, $
    maxl1files=maxl1files, finalstatus=finalstatus, rollangle=rollangle, $
    debug=debug, _extra=_extra
  ; $Id: irisl12_multiobs.pro,v 1.14 2013/11/18 00:37:06 mawiesma Exp $
    
  for i=0,N_ELEMENTS(level1dirs)-1 do begin
  
    level1files = file_search(level1dirs[i]+'*.fits')
    
    if N_ELEMENTS(outputfolders) eq 1 then begin
      if outputfolders eq '' then begin
        ;construct the destination folder, and create it
        lastpos = STRPOS(level1dirs[i], '_L1', /reverse_search)
        if lastpos eq -1 then begin
          outputfolder = STRMID(level1dirs[i], 0, STRLEN(level1dirs[i])-1) + '_L2/'
        endif else begin
          outputfolder = level1dirs[i]
          STRPUT, outputfolder, '_L2', lastpos
        endelse
      endif else outputfolder=outputfolders
    endif else outputfolder = outputfolders[i]
    
    FILE_MKDIR, outputfolder
    
    iris_level1to2, level1files, outputfolder, OBSid=OBSid, l1to2log=l1to2log, $
      scaled=scaled, maxdeviation=maxdeviation, xmlparentfolder=xmlparentfolder, outdir=outdir, $
      SJIonly=SJIonly, spectralonly=spectralonly, preserveINF=preserveINF, preserveNAN=preserveNAN, $
      maxl1files=maxl1files, finalstatus=finalstatus, rollangle=rollangle, $
      debug=debug, _extra=_extra
  endfor
END
