pro IRISsim_readtables, filenamein, directoryin, ObsList, FRMList, FDBList, CRSList, error, copyxml, destination, $
    FRMdir=FRMdir, FDBdir=FDBdir, CRSdir=CRSdir, $
    FRMinOBS, FDBinFRM, CRSinFDB, $
    quiet=quiet
    
  ; procedure to read the xml-files from Ankurs planning tool
  ; INPUT:
  ;     filename: the name of the Observing List Table
  ;             the other filenames of FRM, FDB and CRS will be taken from there, from the IDs
  ;     directory: the directory, in which all xml-files are in
  ;     destination: the folder in which all results are saved, this procedure will copy the xml-files there
  ;     copyxml: 1 if xml-files should be copied to destination, 0 if not
  ;
  ; OUTPUT:
  ;     ObsList: a structure, derived from IRISsim_structure.pro containing
  ;           all variables of the Observing list table
  ;     FRMList: an array of structures, containing the FRMs
  ;     FDBList: an array of structures, containing the FDBs
  ;     CRSList: an array of structures, containing the CRSs
  ;     error: is 1, if an error occurred
  ;
  ; MODIFICATION HISTORY:
  ; written by Martin Wiesmann (ITA, UIO), 5.6.2012
  ; modified it, so that xml-files will be copied to a destination-folder, 30.8.2012
  ;
  ; $Id: irissim_readtables.pro,v 1.13 2013/07/22 21:02:14 mawiesma Exp $  ;
    
  COMMON IRIS_simulator_common, listObject
  IF N_ELEMENTS(listObject) eq 0 THEN listObject = OBJ_NEW('IRISsim_xml2struct') ;create a IRISsim_xml2struct object, which inherits IDLffXMLSAX
  
  defaultlocation='XMLTest/'
  defaultfilename='OBS-T-00001.xml'
  
  error=0
  
  if N_PARAMS() eq 0 then filename=defaultfilename $
  else filename=filenamein
  if N_PARAMS() le 1 then directory=defaultlocation $
  else directory=directoryin
  sfile = size(filename)
  if sfile[N_ELEMENTS(sfile)-2] ne 7 then begin
    print, 'first parameter (filename) must be a string'
    error=1
    return
  endif
  if filename eq '' then filename=defaultfilename
  file=directory+filename
  if ~FILE_TEST(file) then begin
    print, 'invalid filename, could not load OBS file: ' + file
    error=1
    return
  endif
  
  
  ;copy OBS-table
  if copyxml then file_copy,file,destination, /overwrite
  
  ;read OBS-table
  listObject->Reset
  listObject->parseFile, file       ;read and parse file
  ObsList = listObject->getArray()      ;get structure with observation list table
  
  if ~keyword_set(quiet) then $
    print, 'Observing list table loaded, ID: ' + string(ObsList.ID) + '; ' + string(ObsList.NumEntries) + ' entries'
    
  ; get a possible appendix to the filename plus its extension
  file_app = strsplit(filename, ObsList.ID, /extract, /regex)
  file_app = file_app[1]
  ; if the OBS-file sits in a folder called 'OBS' then go 1 level up
  ; FRM, FDB, CRS will have their own folder, probably
  ; so that means, if this is the case, directory ends with'*/OBS/'
  dir2 = strsplit(directory, '/OBS/', /extract, /regex)
  dir2=dir2[0]
  
  
  ;get FRM-IDs
  if ObsList.NumEntries gt 0 then begin
    for i=0,ObsList.NumEntries-1 do begin
      if i eq 0 then FRMids=(*(ObsList).FRM_ID)[i] else begin
        index = WHERE(FRMids eq (*(ObsList).FRM_ID)[i],count)
        if count eq 0 then FRMids = [FRMids, (*(ObsList).FRM_ID)[i]]
      endelse
    endfor
    if ~keyword_set(quiet) then begin
      print, string(N_ELEMENTS(FRMids)) + ' FRM-IDs found'
      print, 'IDs: ', FRMids
    endif
    
    
    
    
    ;read FRM-tables
    FDBidstemp=0
    for nfrm=0,N_ELEMENTS(FRMids)-1 do begin
      filename='FRM-'+FRMids[nfrm]+file_app
      searchfurther=0
      if keyword_set(FRMdir) then begin
        file = FRMdir + filename
        if ~file_test(file) then searchfurther=1
      endif
      if ~keyword_set(FRMdir) || searchfurther then begin
        file = dir2 + '/FRM/' + filename
        if ~file_test(file) then begin
          file = dir2 + filename
          if ~file_test(file) then begin
            print, 'no file found, could not load FRM: ' + filename
            error=1
            return
          endif
        endif
      endif
      file = file[0]
      
      
      ;copy FRM-table
      if copyxml then file_copy,file,destination, /overwrite
      
      listObject->Reset
      listObject->parseFile, file
      FRMListtemp = listObject->getArray()
      
      if ~keyword_set(quiet) then $
        print, 'Frame List Table loaded, ID: ' + string(FRMListtemp.ID) + '; ' + string(FRMListtemp.NumEntries) + ' entries'
        
      ;create an array of FRM-tables
      if nfrm eq 0 then begin
        FRMList = MAKE_ARRAY(N_ELEMENTS(FRMids), Value=FRMListtemp)
      endif else begin
        FRMList[nfrm] = FRMListtemp
      endelse
      
      ; temporary variable, max number of FDB IDs
      FDBidstemp = FDBidstemp + 3 * FRMListtemp.NumEntries
    endfor ;read FRM-tables
    
    
    ;Get number of FDB IDs and their IDs
    FDBids = MAKE_ARRAY(FDBidstemp, /String)
    k=0
    for i=0,N_ELEMENTS(FRMids)-1 do begin
      for j=0,FRMList[i].NumEntries-1 do begin
        FDBids[k]=(*(FRMList[i]).SJI_FDB_ID)[j]
        k=k+1
        FDBids[k]=(*(FRMList[i]).NUV_SG_FDB_ID)[j]
        k=k+1
        FDBids[k]=(*(FRMList[i]).FUV_SG_FDB_ID)[j]
        k=k+1
      endfor
    endfor
    index = WHERE(FDBids ne '0000000000', count)
    if count eq 0 then begin
      print, 'No FDBs defined'
      return
    endif
    FDBids = FDBids[index]
    
    if N_ELEMENTS(FDBids) gt 1 then begin
      temp = FDBids
      for i=0,N_ELEMENTS(temp)-1 do begin
        if i eq 0 then FDBids = temp[i] else begin
          index = WHERE(temp[i] eq FDBids,count)
          if count eq 0 then FDBids = [FDBids, temp[i]]
        endelse
      endfor
    endif
    if ~keyword_set(quiet) then begin
      print, string(N_ELEMENTS(FDBids)) + ' FDB-IDs found'
      print, 'IDs: ', FDBids
    endif
    
    
    
    
    ;read FDB-table
    if N_ELEMENTS(FDBids) gt 0 then begin
      for nfdb=0,N_ELEMENTS(FDBids)-1 do begin
        filename='FDB-'+FDBids[nfdb]+file_app
        searchfurther=0
        if keyword_set(FDBdir) then begin
          file = FDBdir + filename
          if ~file_test(file) then searchfurther=1
        endif
        if ~keyword_set(FDBdir) || searchfurther then begin
          file = dir2 + '/FDB/' + filename
          if ~file_test(file) then begin
            file = dir2 + filename
            if ~file_test(file) then begin
              print, 'no file found, could not load FDB: ' + filename
              error=1
              return
            endif
          endif
        endif
        file = file[0]
        
        
        ;copy FDB-table
        if copyxml then file_copy,file,destination, /overwrite
        
        listObject->Reset
        listObject->parseFile, file
        FDBListtemp = listObject->getArray()    ;get structure with observation list table
        
        if ~keyword_set(quiet) then $
          print, 'Frame Definition Block Table loaded, ID: ' + string(FDBListtemp.ID)
          
        ;create an array of FDB-tables
        if nfdb eq 0 then begin
          FDBList = MAKE_ARRAY(N_ELEMENTS(FDBids), Value=FDBListtemp)
        endif else begin
          FDBList[nfdb] = FDBListtemp
        endelse
      endfor ;read FDB-table
      
      ;get CRS-IDs
      for i=0,N_ELEMENTS(FDBids)-1 do begin
        if i eq 0 then begin
          CRSids = FDBlist[i].CRSID
          type = FDBlist[i].type
        endif else begin
          index = WHERE(FDBlist[i].CRSID eq CRSids,count)
          if count eq 0 then begin
            CRSids = [CRSids, FDBlist[i].CRSID]
            type = [type, FDBlist[i].type]
          endif
        endelse
      endfor
      ;endif
      if ~keyword_set(quiet) then begin
        print, string(N_ELEMENTS(CRSids)) + ' CRS-IDs found'
        print, 'IDs: ', CRSids
        print, 'Types: ', type
      endif
      
      
      
      ;read CRS-table
      if N_ELEMENTS(CRSids) gt 0 then begin
        for ncrs=0,N_ELEMENTS(CRSids)-1 do begin
          filename='CRS-'+CRSids[ncrs]+'-'+type[ncrs]+file_app
          searchfurther=0
          if keyword_set(CRSdir) then begin
            file = CRSdir + filename
            if ~file_test(file) then searchfurther=1
          endif
          if ~keyword_set(CRSdir) || searchfurther then begin
            file = dir2 + '/CRS/' + filename
            if ~file_test(file) then begin
              file = dir2 + filename
              if ~file_test(file) then begin
                print, 'no file found, could not load CRS: ' + filename
                error=1
                return
              endif
            endif
          endif
          file = file[0]
          
          
          ;copy CRS-table
          if copyxml then file_copy,file,destination, /overwrite
          
          listObject->Reset
          listObject->parseFile, file
          CRSListtemp = listObject->getArray()    ;get structure with observation list table
          
          if ~keyword_set(quiet) then $
            print, 'CCD Readout Spec Table loaded, ID: ' + string(CRSListtemp.ID) + '; ' + string(CRSListtemp.SubRegions) + ' subregions'
            
          ;help,CRSListtemp
            
          ;create an array of CRS-tables
          if ncrs eq 0 then begin
            CRSList = MAKE_ARRAY(N_ELEMENTS(CRSids), Value=CRSListtemp)
          endif else begin
            CRSList[ncrs] = CRSListtemp
          endelse
        endfor ;read CRS-table
        
      endif else error=1 ;N_ELEMENTS(CRSids) gt 0
    endif else error=1 ;N_ELEMENTS(FDBids) gt 0
  endif else error=1 ;ObsList.NumEntries gt 0
  
  
  
  
  
  ;mapping FRM to OBS
  FRMinOBS = lonarr(OBSList.NumEntries)
  mapped = intarr(OBSList.NumEntries)
  for nrFRM=0,OBSList.NumEntries-1 do begin
    if ~mapped(nrFRM) then begin
      ind = where(FRMList.ID eq (*(OBSList).FRM_ID)[nrFRM], count)
      if count eq 0 then begin
        print,'mapping failed, did not find a FRM'
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
          print,'mapping failed, did not find a FDB'
          return
        endif
        FDBinFRM[currentFRM, nrFDB, 0] = ind[0]
      endif else FDBinFRM[currentFRM, nrFDB, 0] = -1
      
      if (*(FRMList[currentFRM]).NUV_SG_FDB_ID)[nrFDB] ne '0000000000' then begin
        ind = where(FDBList.ID eq (*(FRMList[currentFRM]).NUV_SG_FDB_ID)[nrFDB], count)
        if count eq 0 then begin
          print,'mapping failed, did not find a FDB'
          return
        endif
        FDBinFRM[currentFRM, nrFDB, 1] = ind[0]
      endif else FDBinFRM[currentFRM, nrFDB, 1] = -1
      
      if (*(FRMList[currentFRM]).FUV_SG_FDB_ID)[nrFDB] ne '0000000000' then begin
        ind = where(FDBList.ID eq (*(FRMList[currentFRM]).FUV_SG_FDB_ID)[nrFDB], count)
        if count eq 0 then begin
          print,'mapping failed, did not find a FDB'
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
      print,'mapping failed, did not find a CRS'
      return
    endif
    CRSinFDB[currentFDB] = ind[0]
  endfor
  
end