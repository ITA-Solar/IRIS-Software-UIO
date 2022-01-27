;+
; NAME:
;       IRIS_LEVEL1TO2
;
; PURPOSE:
;
;       IRIS_LEVEL1TO2 is used to transform level 1 or level 1.5 or level 1.6 IRIS data into level 2 data
;
; CATEGORY:
;       Martin Wiesmann / IRIS Data processing
;
; CALLING SEQUENCE:
;       iris_level1to2, level1files, outputfolder[, OBSid=OBSid, l1to2log=l1to2log, $
;       scaled=scaled, maxdeviation=maxdeviation, xmlparentfolder=xmlparentfolder, outdir=outdir, $
;       SJIonly=SJIonly, spectralonly=spectralonly, preserveINF=preserveINF, preserveNAN=preserveNAN, $
;       maxl1files=maxl1files, finalstatus=finalstatus, rollangle=rollangle, $
;       debug=debug, _extra=_extra]
;
; INPUTS:
;       level1files: string array with full paths and names of the level 1(.5/6) files
;       outputfolder: directory in which the level 2 files should be saved (must exist)
;
; OPTIONAL KEYWORD PARAMETERS:
;       OBSid: The OBS ID as a string (if not set, will be derived from header keyword ISQOLTID)
;       l1to2log: Set this keword to a named variable that will contain the log in form of a structure
;       scaled: Set this keyword, if the data should be saved as a scaled integer, instead of floating point number (l2 files appr. half the size)
;       maxdeviation: Sets the number of pixels that the extreme y-values of the windows can deviate from the median of all windows (default: 30)
;       xmlparentfolder: the parent directory in which the xml-files are (doesn't work properly, iris_obstab2depend has to be fixed)
;       outdir: the directory in which to save the xml-files(default is xmlparentfolder if set, otherwise the default of iris_obstab2depend)
;       SJIonly: if set, only the SJI files will be created (recommended to feed the tool all level1(.5/6) files of the whole OBS anyway,
;         to get the correct number for the keyword 'MISSOBS' and a correct log, i.e. without many reported missing files)
;       spectralonly: if set, only the raster files will be created (same recommendation as for SJIonly)
;       preserveINF: if set, INF will be replaced by 16183 before calling poly_2d, and then they will be reset,
;         this will preserve the number INFs, but the pixels around INFs won't be photometrically accurate
;       preserveNAN: if set, NANs will be replaced by -200 before calling poly_2d, and then they will be reset,
;         this will preserve the number NANs, but the pixels around NANs won't be photometrically accurate
;       maxl1files: the maximum number of level 1 or 1.5/6 files that should be loaded in one go (higher = faster, but needs more RAM, default is 100,
;         above 100 the speed gain is minimal)
;       finalstatus: if set, the value of the 'STATUS' keyword will be 'Final' instead of 'Quicklook'
;       rollangle: overwrites the SAT_ROT keyword in level 1(.5/6) files
;       debug: catch, error_status will not be implemented, i.e. procedure crashes when encountering an error (no errorlog file will be written)
;       _extra: will be passed to
;         read_iris (/uncomp_delete, /use_shared and /no_shell are already set)
;         iris_prep (/strict, /uncomp_delete, /use_shared and /no_shell are already set)
;         iris_prep_header (obsolete, using iris_prep, /keywords_only now)
;         iris_obstab2depend
;
; OUTPUTS:
;       level 2 files in outputfolder, as well as a logfile and an errorlogfile if the procedure encountered an error and the debug keyword was not set
;       structure with the content of the logfile will be passed to l1to2log
;
; CALLS:
;
; COMMON BLOCKS:
;       IRIS_simulator_common: a IRISsim_xml2struct object
;
; PROCEDURE:
;       IRIS_LEVEL1TO2
;
; RESTRICTIONS:
;       works so far only on one OBS at a time
;
; MODIFICATION HISTORY:
;       2013: Martin Wiesmann (ITA, UIO). Gradually developed through the year.
;       More detailed changelog can be found in irisl12_changelog.rtf, which is also part of solarsoft
;       /ssw/iris/idl/uio/level1to2/
;
; $Id: iris_level1to2.pro,v 1.168 2019/08/08 08:09:11 mawiesma Exp $


PRO iris_level1to2, level1files, outputfolder, OBSid=OBSid, l1to2log=l1to2log, $
  scaled=scaled, maxdeviation=maxdeviation, xmlparentfolder=xmlparentfolder, outdir=outdir, $
  SJIonly=SJIonly, spectralonly=spectralonly, preserveINF=preserveINF, preserveNAN=preserveNAN, $
  maxl1files=maxl1files, finalstatus=finalstatus, rollangle=rollangle, $
  debug=debug, _extra=_extra
  
  version='L12-2019-08-08'
  ; assuming that all files belong to one OBS execution!!!
  
  ; Get current allocation and reset the high water mark:
  start_mem = MEMORY(/CURRENT)
  
  
  COMMON IRIS_simulator_common, listObject
  IF N_ELEMENTS(listObject) eq 0 THEN listObject = OBJ_NEW('IRISsim_xml2struct') ;create a IRISsim_xml2struct object, which inherits IDLffXMLSAX
  
  timemeasure = {readheader:0d, $
    xmlfiles:0d, $
    simulation:0d, $
    preparation:0d, $
    saveSpectralData:0d, $
    saveSJIData:0d, $
    specStats:0d, $
    SJIStats:0d, $
    readfile:0d, $
    writefile:0d, $
    total:0d}
    
  timemeas = {stats:0d, $
    readfile:0d, $
    writefile:0d}
  
  l1to2log = !NULL 
  
  if N_PARAMS() lt 2 then begin
    box_message, ['usage: iris_level1to2, level1files, outputfolder[, OBSid=OBSid, l1to2log=l1to2log, ', $
       'scaled=scaled, maxdeviation=maxdeviation, xmlparentfolder=xmlparentfolder, outdir=outdir, ', $
       'SJIonly=SJIonly, spectralonly=spectralonly, preserveINF=preserveINF, preserveNAN=preserveNAN, ', $
       'maxl1files=maxl1files, finalstatus=finalstatus, rollangle=rollangle, ', $
       'debug=debug, _extra=_extra]']
    return
  endif else begin
    if size(level1files, /type) ne 7 then begin
      box_message,'level1files needs to be a string-array'
      return
    endif
    if (size(outputfolder, /type) ne 7) || (size(outputfolder, /N_ELEMENTS) ne 1) then begin
      box_message,'outputfolder needs to be a scalar string'
      return
    endif
  endelse
  if keyword_set(OBSid) then begin
    if (size(OBSid, /type) ne 7) || (size(OBSid, /N_ELEMENTS) ne 1) then begin
      box_message,['OBSid needs to be a scalar string', 'converting it']
      OBSid = strcompress(string(OBSid), /remove_all)
    endif
  endif
  if keyword_set(xmlparentfolder) then begin
    if (size(xmlparentfolder, /type) ne 7) || (size(xmlparentfolder, /N_ELEMENTS) ne 1) then begin
      box_message,'xmlparentfolder needs to be a scalar string'
      return
    endif
    if ~keyword_set(outdir) then outdir = xmlparentfolder
  endif
  if keyword_set(outdir) then begin
    if (size(outdir, /type) ne 7) || (size(outdir, /N_ELEMENTS) ne 1) then begin
      box_message,'outdir needs to be a scalar string'
      return
    endif
  endif
  if keyword_set(maxdeviation) then begin
    if ~isnumeric(maxdeviation) || (size(maxdeviation, /N_ELEMENTS) ne 1) then begin
      box_message,'maxdeviation needs to be a scalar number'
      return
    endif
  endif
  if keyword_set(maxl1files) then begin
    if ~isnumeric(maxl1files) || (size(maxl1files, /N_ELEMENTS) ne 1) then begin
      box_message,'maxl1files needs to be a scalar number'
      return
    endif
  endif else maxl1files=100
  if maxl1files lt 1 then maxl1files=1
  if N_ELEMENTS(rollangle) gt 0 then begin
    if ~isnumeric(rollangle) || (size(rollangle, /N_ELEMENTS) ne 1) then begin
      box_message,'rollangle needs to be a scalar number'
      return
    endif
  endif

  
  files = level1files
  level2dir = outputfolder
  dirsep = path_sep()
  if strmid(level2dir, 0,1, /reverse_offset) ne dirsep then level2dir = level2dir+dirsep
  print, 'Processing new OBS'
  print, 'saving data in folder: ', level2dir
  
  except_orig = !except
  if ~keyword_set(debug) then begin
    !except = 0
    ; launch the error handler:
    catch, error_status
    ; begin error handler
    if error_status ne 0 then begin
      catch, /cancel
      catch, error_inerror
      if error_inerror ne 0 then begin
        catch, /cancel
        Help, /Last_Message, Output=traceback
        box_message,traceback
        box_message,['error handler crashed',$
          'exiting without writing an errorfile']
        !except = except_orig
        return
      endif
      Help, /Last_Message, Output=traceback
      box_message,traceback
      box_message,'writing errorlogfile, and returning'
      if N_ELEMENTS(filename) eq 0 then filename=level2dir+'iris_l2'
      IRISl12_writeLogFile, l1to2log, OBSvars, filename, memlog=memlog, timestr=timestr, traceback=traceback, version=version, $
        files=files, obstab=depend
      !except = except_orig
      return
    endif
  endif else !except = 2
  

  GET_UTC, Date_RF2, /CCSDS
  t1=systime(1)
  
  print, 'reading in all fits-headers'
  read_iris, files, hdr, /nodata, /use_shared, /no_shell, /uncomp_delete, _extra=_extra
  if required_tags(hdr[0], /LVL_NUM) then begin
    lvl = IRISl12_mostcommonvalue(hdr.LVL_NUM)
    if (abs(lvl-1.5) lt 0.01) || (abs(lvl-1.6) lt 0.01) then l1p5 = 1 $
    else if abs(lvl-1.0) lt 0.01 then l1p5 = 0 $
    else l1p5 = -1
    if l1p5 eq 1 then begin
      ;input is level 1.5 or 1.6
      box_message, 'input is level ' + string(lvl, format='(f3.1)')
    endif else if l1p5 eq 0 then begin
      box_message, ['input is level 1', 'running iris_prep']
      ;divide files for different INSTRUME
      indFUV = where(hdr.INSTRUME eq 'FUV', countFUV)
      indNUV = where(hdr.INSTRUME eq 'NUV', countNUV)
      indSJI = where(hdr.INSTRUME eq 'SJI', countSJI)
      histo = strarr(50)
      
      if countFUV gt 0 then begin
        print, 'prepping '+strtrim(string(countFUV),2)+' FUV files'
        ;divide files for different CRS
        crsid = hdr[indFUV[UNIQ(hdr[indFUV].IICRSID, SORT(hdr[indFUV].IICRSID))]].IICRSID
        for icrs=0,N_ELEMENTS(crsid)-1 do begin
          crscur = where(hdr[indFUV].IICRSID eq crsid[icrs], count)
          if count gt 0 then begin
            print, 'FUV CRS '+strtrim(string(icrs+1),2)+' of '+strtrim(string(N_ELEMENTS(crsid)),2)+' (CRS ID: '+strtrim(string(crsid[icrs]),2)+')'
            print, 'number of files: '+strtrim(string(count),2)
            iris_prep, hdr[indFUV[crscur]], dummy, h1a, /keyword_only, /strict, /run_time, _extra=_extra, masks=masks
            if N_ELEMENTS(masks) gt 0 then begin
              indbad = where(masks.anybit ne 0, countbad, complement=indgood, ncomplement=countgood)
              if countbad gt 0 then begin
                if countgood gt 0 then begin
                  iris_prep, hdr[indFUV[crscur[indgood]]], dummy, h1agood, /keyword_only, /strict, /run_time, _extra=_extra, masks=masks
                  h1a = make_array(N_ELEMENTS(crscur), value=h1agood[0])
                  h1a[indgood] = h1agood
                  h1a[indbad].ISQOLTID = 0
                  h1a[indbad].DATE_OBS = hdr[indFUV[crscur[indbad]]].DATE_OBS
                  if N_ELEMENTS(hdrtemplate) eq 0 then hdrtemplate=h1agood[0]
                endif else begin
                  if N_ELEMENTS(hdrtemplate) gt 0 then begin
                    h1a = make_array(N_ELEMENTS(crscur), value=hdrtemplate)
                    h1a[indbad].ISQOLTID = 0
                    h1a[indbad].DATE_OBS = hdr[indFUV[crscur[indbad]]].DATE_OBS
                  endif else begin
                    ;???
                    box_message,['no template for the headers','if this occurs please inform','martin.wiesmann@astro.uio.no']
                  endelse
                endelse
              endif else begin
                if N_ELEMENTS(hdrtemplate) eq 0 then hdrtemplate=h1a[0]
              endelse
            endif
            for i=0,N_ELEMENTS(h1a)-1 do begin
              histo[*]=''
              histo[0:N_ELEMENTS(h1a[i].history)-1] = h1a[i].history
              htemp = h1a[i]
              struct_replace_field, htemp, 'HISTORY', histo
              if i eq 0 then hstemp=htemp $
              else hstemp=[hstemp,htemp]
            endfor
            h1a = hstemp
            if N_ELEMENTS(h1) eq 0 then h1 = make_array(N_ELEMENTS(indFUV), value=h1a[0])
            h1[crscur] = h1a
          endif
        endfor
      endif
      
      if countNUV gt 0 then begin
        print, 'prepping '+strtrim(string(countNUV),2)+' NUV files'
        ;divide files for different CRS
        crsid = hdr[indNUV[UNIQ(hdr[indNUV].IICRSID, SORT(hdr[indNUV].IICRSID))]].IICRSID
        for icrs=0,N_ELEMENTS(crsid)-1 do begin
          crscur = where(hdr[indNUV].IICRSID eq crsid[icrs], count)
          if count gt 0 then begin
            print, 'NUV CRS '+strtrim(string(icrs+1),2)+' of '+strtrim(string(N_ELEMENTS(crsid)),2)+' (CRS ID: '+strtrim(string(crsid[icrs]),2)+')'
            print, 'number of files: '+strtrim(string(count),2)
            iris_prep, hdr[indNUV[crscur]], dummy, h2a, /keyword_only, /strict, /run_time, _extra=_extra, masks=masks
            if N_ELEMENTS(masks) gt 0 then begin
              indbad = where(masks.anybit ne 0, countbad, complement=indgood, ncomplement=countgood)
              if countbad gt 0 then begin
                if countgood gt 0 then begin
                  iris_prep, hdr[indNUV[crscur[indgood]]], dummy, h2agood, /keyword_only, /strict, /run_time, _extra=_extra, masks=masks
                  h2a = make_array(N_ELEMENTS(crscur), value=h2agood[0])
                  h2a[indgood] = h2agood
                  h2a[indbad].ISQOLTID = 0
                  h2a[indbad].DATE_OBS = hdr[indNUV[crscur[indbad]]].DATE_OBS
                  if N_ELEMENTS(hdrtemplate) eq 0 then hdrtemplate=h2agood[0]
                endif else begin
                  if N_ELEMENTS(hdrtemplate) gt 0 then begin
                    h2a = make_array(N_ELEMENTS(crscur), value=hdrtemplate)
                    h2a[indbad].ISQOLTID = 0
                    h2a[indbad].DATE_OBS = hdr[indNUV[crscur[indbad]]].DATE_OBS
                  endif else begin
                    ;???
                    box_message,['no template for the headers','if this occurs please inform','martin.wiesmann@astro.uio.no']
                  endelse
                endelse
              endif else begin
                if N_ELEMENTS(hdrtemplate) eq 0 then hdrtemplate=h2a[0]
              endelse
            endif
            for i=0,N_ELEMENTS(h2a)-1 do begin
              histo[*]=''
              histo[0:N_ELEMENTS(h2a[i].history)-1] = h2a[i].history
              htemp = h2a[i]
              struct_replace_field, htemp, 'HISTORY', histo
              if i eq 0 then hstemp=htemp $
              else hstemp=[hstemp,htemp]
            endfor
            h2a = hstemp
            if N_ELEMENTS(h2) eq 0 then h2 = make_array(N_ELEMENTS(indNUV), value=h2a[0])
            h2[crscur] = h2a
          endif
        endfor
      endif
      
      if countSJI gt 0 then begin
        print, 'prepping '+strtrim(string(countSJI),2)+' SJI files'
        ;divide files for different IMG_PATH
        sjifw = hdr[indSJI[UNIQ(hdr[indSJI].IMG_PATH, SORT(hdr[indSJI].IMG_PATH))]].IMG_PATH
        for isjifw=0,N_ELEMENTS(sjifw)-1 do begin
          sjicur = where(hdr[indSJI].IMG_PATH eq sjifw[isjifw], count)
          if count gt 0 then begin
            print, 'SJI FW '+strtrim(string(isjifw+1),2)+' of '+strtrim(string(N_ELEMENTS(sjifw)),2)+' (FW: '+sjifw[isjifw]+')'
            print, 'number of files: '+strtrim(string(count),2)
            ;divide files for different CRS
            indSJIcur=indSJI[sjicur]
            crsid = hdr[indSJIcur[UNIQ(hdr[indSJIcur].IICRSID, SORT(hdr[indSJIcur].IICRSID))]].IICRSID
            for icrs=0,N_ELEMENTS(crsid)-1 do begin
              crscur = where(hdr[indSJIcur].IICRSID eq crsid[icrs], count2)
              if count2 gt 0 then begin
                print, 'SJI CRS '+strtrim(string(icrs+1),2)+' of '+strtrim(string(N_ELEMENTS(crsid)),2)+' (CRS ID: '+strtrim(string(crsid[icrs]),2)+')'
                print, 'number of files: '+strtrim(string(count2),2)
                iris_prep, hdr[indSJIcur[crscur]], dummy, h3a, /keyword_only, /strict, /run_time, _extra=_extra, masks=masks
                if N_ELEMENTS(masks) gt 0 then begin
                  indbad = where(masks.anybit ne 0, countbad, complement=indgood, ncomplement=countgood)
                  if countbad gt 0 then begin
                    if countgood gt 0 then begin
                      iris_prep, hdr[indSJIcur[crscur[indgood]]], dummy, h3agood, /keyword_only, /strict, /run_time, _extra=_extra, masks=masks
                      h3a = make_array(N_ELEMENTS(crscur), value=h3agood[0])
                      h3a[indgood] = h3agood
                      h3a[indbad].ISQOLTID = 0
                      h3a[indbad].DATE_OBS = hdr[indSJIcur[crscur[indbad]]].DATE_OBS
                      if N_ELEMENTS(hdrtemplate) eq 0 then hdrtemplate=h3agood[0]
                    endif else begin
                      if N_ELEMENTS(hdrtemplate) gt 0 then begin
                        h3a = make_array(N_ELEMENTS(crscur), value=hdrtemplate)
                        h3a[indbad].ISQOLTID = 0
                        h3a[indbad].DATE_OBS = hdr[indSJIcur[crscur[indbad]]].DATE_OBS
                      endif else begin
                        ;???
                        box_message,['no template for the headers','if this occurs please inform','martin.wiesmann@astro.uio.no']
                      endelse
                    endelse
                  endif else begin
                    if N_ELEMENTS(hdrtemplate) eq 0 then hdrtemplate=h3a[0]
                  endelse
                endif
                for i=0,N_ELEMENTS(h3a)-1 do begin
                  histo[*]=''
                  histo[0:N_ELEMENTS(h3a[i].history)-1] = h3a[i].history
                  htemp = h3a[i]
                  struct_replace_field, htemp, 'HISTORY', histo
                  if i eq 0 then hstemp=htemp $
                  else hstemp=[hstemp,htemp]
                endfor
                h3a = hstemp
                if N_ELEMENTS(h3) eq 0 then h3 = make_array(N_ELEMENTS(indSJI), value=h3a[0])
                h3[sjicur[crscur]] = h3a
              endif
            endfor
          endif
        endfor
      endif
      
      hdr=!NULL
      if countFUV gt 0 then begin
        hdr = make_array(N_ELEMENTS(files), value=h1[0])
        hdr[indFUV] = h1
      endif
      if countNUV gt 0 then begin
        if N_ELEMENTS(hdr) eq 0 then hdr = make_array(N_ELEMENTS(files), value=h2[0])
        hdr[indNUV] = h2
      endif
      if countSJI gt 0 then begin
        if N_ELEMENTS(hdr) eq 0 then hdr = make_array(N_ELEMENTS(files), value=h3[0])
        hdr[indSJI] = h3
      endif
      hdr[*].LVL_NUM=1.51
    endif else begin
      box_message, 'Level of data is neither 1 nor 1.5 nor 1.6'
      !except = except_orig
      return
    endelse
  endif else begin
    box_message,'Cannot determine data level, keyword LVL_NUM missing'
    !except = except_orig
    return
  endelse
  
  ;get the OBS id, and OBS repetition, so we can load the xml-files
  IRISl12_getkeywords, 0, hdr, OBSvars, OBSid=OBSid, finalstatus=finalstatus
  
  if OBSvars.OBSrep eq 0 then begin
    print, 'one or more essential keywords are missing, exiting...'
    print, 'ISQOLTID, IIOLNRPT, INSTRUME, IIOLRPT, ISQOLTDX, IIFLRPT, ISQFLTDX'
    !except = except_orig
    return
  endif
  
  ;we have to add naxis1 and naxis2 to the structure
  t=tag_names(hdr)
  w=where(t eq 'NAXIS1', c)
  if c eq 0 then begin
    constants = obj_new('IRISsim_constants')
    hdr = add_tag(hdr, constants->get_PixCCDx()/hdr.sumsptrl, 'NAXIS1')
    hdr = add_tag(hdr, constants->get_PixCCDy()/hdr.sumspat, 'NAXIS2')
    ind = where(hdr.instrume eq 'FUV', count)
    if count gt 0 then hdr[ind].naxis1 = hdr[ind].naxis1*2
    obj_destroy, constants
  endif
  if N_ELEMENTS(rollangle) gt 0 then begin
    w=where(t eq 'SAT_ROT', c)
    if c gt 0 then begin
      hdr[*].SAT_ROT=rollangle
    endif else begin
      hdr = add_tag(hdr, rollangle, 'SAT_ROT')
    endelse
  endif
  
  t2=systime(1)
  timemeasure.readheader = t2-t1
  
  
  ;get XML-files
  depend=iris_obstab2depend('OBS-'+OBSvars.OBSid+'.xml', parent=xmlparentfolder, outdir=outdir, /check, nmissing=nmissing, _extra=_extra)
  if size(depend, /type) eq 2 then begin
    print, 'xml-file ' + 'OBS-'+OBSvars.OBSid+'.xml' + ' not found'
    !except = except_orig
    return
  endif
  if nmissing gt 0 then begin
    print,'not all XML files are available, exiting...'
    !except = except_orig
    return
  endif
  
  ;set extra keyword 'outdir' to empty string, otherwise read_iris will crash
  if N_ELEMENTS(_extra) gt 0 then begin
    tags = tag_names(_extra)
    in = where(tags eq 'OUTDIR', count)
    if count gt 0 then _extra.(in[0])=''
  endif
  
  
  ;reading all xml files
  
  listObject->Reset
  listObject->parseFile, depend.obsfiles       ;read and parse file
  OBSList = listObject->getArray()            ;get structure with observation list table
  
  for i=0,N_ELEMENTS(depend.frmfiles)-1 do BEGIN
    listObject->Reset
    listObject->parseFile, depend.frmfiles[i]       ;read and parse file
    temp = listObject->getArray()            ;get structure with observation list table
    if i eq 0 then FRMList = temp $
    else FRMList = [FRMList, temp]
  ENDFOR
  
  for i=0,N_ELEMENTS(depend.fdbfiles)-1 do BEGIN
    listObject->Reset
    listObject->parseFile, depend.fdbfiles[i]       ;read and parse file
    temp = listObject->getArray()            ;get structure with observation list table
    if i eq 0 then FDBList = temp $
    else FDBList = [FDBList, temp]
  ENDFOR
  
  for i=0,N_ELEMENTS(depend.crsfiles)-1 do BEGIN
    listObject->Reset
    listObject->parseFile, depend.crsfiles[i]       ;read and parse file
    temp = listObject->getArray()            ;get structure with observation list table
    if i eq 0 then CRSList = temp $
    else CRSList = [CRSList, temp]
  ENDFOR
  
  
  
  
  
  
  ;mapping FRM to OBS
  FRMinOBS = lonarr(OBSList.NumEntries)
  mapped = intarr(OBSList.NumEntries)
  for nrFRM=0,OBSList.NumEntries-1 do begin
    if ~mapped(nrFRM) then begin
      ind = where(FRMList.ID eq (*(OBSList).FRM_ID)[nrFRM], count)
      if count eq 0 then begin
        print,'did not find a FRM'
        !except = except_orig
        return
      endif
      currentFRM=ind[0]
      ind = where(*(OBSList).FRM_ID eq FRMList[currentFRM].ID)
      FRMinOBS[ind] = currentFRM
      mapped[ind] = 1
    endif
  endfor
  
  
  
  ;print, 'mapping FDB to FRM'
  maxFRMentry = max(FRMList.NumEntries)
  FDBinFRM = lonarr(N_ELEMENTS(FRMList), maxFRMentry, 3)
  for currentFRM=0,N_ELEMENTS(FRMList)-1 do begin
    for nrFDB=0,FRMList[currentFRM].NumEntries-1 do begin
      if (*(FRMList[currentFRM]).SJI_FDB_ID)[nrFDB] ne '0000000000' then begin
        ind = where(FDBList.ID eq (*(FRMList[currentFRM]).SJI_FDB_ID)[nrFDB], count)
        if count eq 0 then begin
          print,'did not find a FDB'
          !except = except_orig
          return
        endif
        FDBinFRM[currentFRM, nrFDB, 0] = ind[0]
      endif else FDBinFRM[currentFRM, nrFDB, 0] = -1
      
      if (*(FRMList[currentFRM]).NUV_SG_FDB_ID)[nrFDB] ne '0000000000' then begin
        ind = where(FDBList.ID eq (*(FRMList[currentFRM]).NUV_SG_FDB_ID)[nrFDB], count)
        if count eq 0 then begin
          print,'did not find a FDB'
          !except = except_orig
          return
        endif
        FDBinFRM[currentFRM, nrFDB, 1] = ind[0]
      endif else FDBinFRM[currentFRM, nrFDB, 1] = -1
      
      if (*(FRMList[currentFRM]).FUV_SG_FDB_ID)[nrFDB] ne '0000000000' then begin
        ind = where(FDBList.ID eq (*(FRMList[currentFRM]).FUV_SG_FDB_ID)[nrFDB], count)
        if count eq 0 then begin
          print,'did not find a FDB'
          !except = except_orig
          return
        endif
        FDBinFRM[currentFRM, nrFDB, 2] = ind[0]
      endif else FDBinFRM[currentFRM, nrFDB, 2] = -1
    endfor
  endfor
  
  
  
  ;print, 'mapping CRS to FDB'
  CRSinFDB = lonarr(N_ELEMENTS(FDBList))
  for currentFDB=0,N_ELEMENTS(FDBList)-1 do begin
    ind = where(CRSList.ID eq FDBList[currentFDB].CRSID, count)
    if count eq 0 then begin
      print,'did not find a CRS'
      !except = except_orig
      return
    endif
    CRSinFDB[currentFDB] = ind[0]
  endfor
  
  
  
  t3=systime(1)
  timemeasure.xmlfiles = t3-t2
  
  
  ;now we create a log of this OBS, which is the basis for the rastering
  IRISsim_syntool, OBSfile=OBSfile, $;The OBS filename (e.g. 'OBS-T-00004.xml')
    OBSrep=OBSvars.OBSrep, $         ;this number overwrites the number of repetitions defined in the OBS (if zero, the original value will be used)
    logonly=1, $        ;set to 1 if you only want the logfiles, without all the other output (savelogcsv/txt still have to be 1)
    logdata=simlog, $
    simendtime=simendtime, $
    OBSList=OBSList, $
    FRMList=FRMList, $
    FDBList=FDBList, $
    CRSList=CRSList, $
    FRMinOBS=FRMinOBS, $
    FDBinFRM=FDBinFRM, $
    CRSinFDB=CRSinFDB
    
    
  t4=systime(1)
  timemeasure.simulation = t4-t3
  
  OBSvars.OBS_Desc = OBSList.Description
  OBSvars.version = version
  OBSvars.Date_RF2 = Date_RF2
  
  IRISl12_hdr2logMapping, simlog, hdr, files, hdrmap, l1to2log, timemeasure, level2dir=level2dir
  l1to2log.date = Date_RF2
  
  IRISl12_log2rasterstruct, simlog, rasters, rastersSJI, l1to2log, simendtime
  
  ;get global keyword values
  IRISl12_getkeywords, 1, hdr, OBSvars, hdrmap=hdrmap, simlog=simlog, level2dir=level2dir, simendtime=simendtime, maxdeviation=maxdeviation
  
  ;all files from this OBS start with this filename
  filename = level2dir+'iris_l2_'+time2file(OBSvars.OBSstartfile,/seconds)+'_'+OBSvars.OBSid
  
  t5=systime(1)
  timemeasure.preparation = t5-t4
  
  if ~keyword_set(SJIonly) then $
    IRISl12_saveSpectralData, hdr, files, hdrmap, rasters, filename, OBSvars, l1to2log, _extra=_extra, $
    timemeas=timemeas, scaled=scaled, l1p5=l1p5, maxl1files=maxl1files, rollangle=rollangle
    
  t6=systime(1)
  timemeasure.saveSpectralData=t6-t5
  timemeasure.specStats = timemeas.stats
  timemeas.stats = 0d
  
  if ~keyword_set(spectralonly) then $
    IRISl12_saveSJIData, hdr, files, hdrmap, rastersSJI, filename, OBSvars, l1to2log, _extra=_extra, $
    timemeas=timemeas, scaled=scaled, preserveNAN=preserveNAN, preserveINF=preserveINF, l1p5=l1p5, maxl1files=maxl1files, rollangle=rollangle
    
  t7=systime(1)
  timemeasure.saveSJIData = t7-t6
  timemeasure.SJIStats = timemeas.stats
  timemeasure.readfile = timemeas.readfile
  timemeasure.writefile = timemeas.writefile
  timemeasure.total = t7-t1
  l1to2log.timemeasure = timemeasure
  
  l1to2log.memory = MEMORY(/HIGHWATER) - start_mem
  
  IRISl12_writeLogFile, l1to2log, OBSvars, filename, memlog=memlog, timestr=timestr, SJIonly=SJIonly, spectralonly=spectralonly
  
  
  !except = except_orig
  
  print,''
  print,'time spent on [s]: OBS ' + OBSvars.OBSid + '  ' + OBSvars.OBSstart
  print,'reading header     :'+string(timemeasure.readheader, format='(f10.3)')
  print,'xml files          :'+string(timemeasure.xmlfiles, format='(f10.3)')
  print,'simulation         :'+string(timemeasure.simulation, format='(f10.3)')
  print,'preparation        :'+string(timemeasure.preparation, format='(f10.3)')
  print,'saveSpectralData   :'+string(timemeasure.saveSpectralData, format='(f10.3)')
  print,'saveSJIData        :'+string(timemeasure.saveSJIData, format='(f10.3)')
  print,'reading/iris_prep  :'+string(timemeasure.readfile, format='(f10.3)')
  print,'writing files      :'+string(timemeasure.writefile, format='(f10.3)')
  print,'spectral statistics:'+string(timemeasure.specStats, format='(f10.3)')
  print,'SJI statistics     :'+string(timemeasure.SJIStats, format='(f10.3)')
  print,timestr
  print,''
  print,'recieved files  :'+STRING(l1to2log.filesreceived, format='(I7)')
  print,'expected files  :'+string(l1to2log.filesexpected, format='(I7)')
  print,'reported missing:'+string(l1to2log.nmissing, format='(I7)')
  print,'unused files    :'+string(l1to2log.nunused, format='(I7)')
  print,'bad files       :'+string(l1to2log.nbadfiles, format='(I7)')
  print,''
  print,memlog
  print,'================================'
  print,''
END
