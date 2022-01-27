PRO IRISsim_multiOBS, files=files, OBSdir=OBSdir, inputdirFUV=inputdirFUV, inputdirNUV=inputdirNUV, $
    inputfileFUV=inputfileFUV, inputfileNUV=inputfileNUV, dtimeSIM=dtimeSIM, dtimend=dtimend, $
    SJIframe=SJIframe, rasterSJI=rasterSJI, maxWINsize=maxWINsize, $
    outputfolder=outputfolder, $
    savefitsfiles=savefitsfiles, saveplot=saveplot, noplot=noplot, savelogcsv=savelogcsv, savelogtxt=savelogtxt, $
    copyxml=copyxml, makemovie=makemovie, playmovie=playmovie, quicktimeloc=quicktimeloc, $
    logonly=logonly, logdir=logdir, savelogfull=savelogfull, logfulldir=logfulldir, $
    maxfilesperfolder=maxfilesperfolder, $
    logdata=logdata, getlogstruct=getlogstruct, logall=logall, $
    OBSreps=OBSreps, $
    FRMdir=FRMdir, FDBdir=FDBdir, CRSdir=CRSdir, $
    threshold_Overview_SJI=threshold_Overview_SJI, threshold_SJI=threshold_SJI, threshold_Spec=threshold_Spec, $
    debug=debug, debtime=debtime, $
    timepOBS=timepOBS,$
    quiet=quiet, nopreload=nopreload
    
  ; $Id: irissim_multiobs.pro,v 1.26 2013/09/16 13:41:28 mawiesma Exp $  ;
    
  COMMON IRIS_simulator_common, listObject
  IF N_ELEMENTS(listObject) eq 0 THEN listObject = OBJ_NEW('IRISsim_xml2struct') ;create a IRISsim_xml2struct object, which inherits IDLffXMLSAX
  
  
  ; INPUT
  if ~keyword_set(files) then begin
    ;OBSfile = 'OBS-T-00901_Q.xml'   ; The OBS filename (e.g. 'OBS-T-00004.xml')
    if keyword_set(OBSdir) then path=OBSdir else path=''
    file=dialog_pickfile(/read, filter=['OBS*.xml'], path=path, title='Please select an OBS file')
    if file eq '' then return
    filesIN = file_basename(file)
    OBSdirIN = file_dirname(file, /mark_directory)
  endif else begin
    if ~keyword_set(OBSdir) then begin
      ;OBSdir = 'XML_bart_121025/OBS/'       ; OBS-file directory (e.g. 'XMLfiles/')
      OBSdirIN = file_dirname(files, /mark_directory)
      OBSdirIN = OBSdirIN[0] ;assuming all files are in the same directory!
      filesIN = file_basename(files)
    endif else begin
      OBSdirIN = OBSdir
      filesIN = OBSfile
    endelse
  endelse
  
  if keyword_set(OBSreps) then begin
    OBSreps=intarr(N_ELEMENTS(files))
  endif else begin
    s=size(OBSreps)
    if s[0] eq 0 then begin
      OBSreps=make_array(N_ELEMENTS(files), value=OBSreps)
    endif else begin
      if N_ELEMENTS(OBSreps) lt N_ELEMENTS(files) then begin
        temp=lonarr(files)
        temp[0:N_ELEMENTS(OBSreps)-1] = OBSreps
      endif
    endelse
  endelse
  
  
  
  if ~keyword_set(nopreload) then begin
  
  
  
    dir2 = strsplit(OBSdirIN, '/OBS/', /extract, /regex)
    dir2=dir2[0]
    
    
    ;search FRM files
    searchfurther=0
    if keyword_set(FRMdir) then begin
      frmfiles = find_files('FRM*.xml', FRMdir)
      if (size(frmfiles))[0] eq 0 then searchfurther=1
    endif
    if ~keyword_set(FRMdir) || searchfurther then begin
      frmfiles = find_files('FRM*.xml', dir2+'/FRM/')
      if (size(frmfiles))[0] eq 0 then begin
        frmfiles = find_files('FRM*.xml', dir2)
        if (size(frmfiles))[0] eq 0 then begin
          print, 'No FRM files found'
          return
        endif
      endif
    endif
    
    print, strcompress('reading ' + string(N_ELEMENTS(frmfiles)) + ' FRM files')
    
    
    for i=0,N_ELEMENTS(frmfiles)-1 do BEGIN
      listObject->Reset
      listObject->parseFile, frmfiles[i]       ;read and parse file
      temp = listObject->getArray()            ;get structure with observation list table
      if i eq 0 then FRMListall = temp $
      else FRMListall = [FRMListall, temp]
    ENDFOR
    
    ;search FDB files
    searchfurther=0
    if keyword_set(FDBdir) then begin
      fdbfiles = find_files('FDB*.xml', FDBdir)
      if (size(fdbfiles))[0] eq 0 then searchfurther=1
    endif
    if ~keyword_set(FDBdir) || searchfurther then begin
      fdbfiles = find_files('FDB*.xml', dir2+'/FDB/')
      if (size(fdbfiles))[0] eq 0 then begin
        fdbfiles = find_files('FDB*.xml', dir2)
        if (size(fdbfiles))[0] eq 0 then begin
          print, 'No FDB files found'
          return
        endif
      endif
    endif
    
    print, strcompress('reading ' + string(N_ELEMENTS(fdbfiles)) + ' FDB files')
    
    for i=0,N_ELEMENTS(fdbfiles)-1 do begin
      listObject->Reset
      listObject->parseFile, fdbfiles[i]       ;read and parse file
      temp = listObject->getArray()      ;get structure with observation list table
      if i eq 0 then FDBListall = temp $
      else FDBListall = [FDBListall, temp]
    endfor
    
    
    ;search CRS files
    searchfurther=0
    if keyword_set(CRSdir) then begin
      crsfiles = find_files('CRS*.xml', CRSdir)
      if (size(crsfiles))[0] eq 0 then searchfurther=1
    endif
    if ~keyword_set(CRSdir) || searchfurther then begin
      crsfiles = find_files('CRS*.xml', dir2+'/CRS/')
      if (size(crsfiles))[0] eq 0 then begin
        crsfiles = find_files('CRS*.xml', dir2)
        if (size(crsfiles))[0] eq 0 then begin
          print, 'No CRS files found'
          return
        endif
      endif
    endif
    
    print, strcompress('reading ' + string(N_ELEMENTS(crsfiles)) + ' CRS files')
    for i=0,N_ELEMENTS(crsfiles)-1 do begin
      listObject->Reset
      listObject->parseFile, crsfiles[i]       ;read and parse file
      temp = listObject->getArray()      ;get structure with observation list table
      if i eq 0 then CRSListall = temp $
      else CRSListall = [CRSListall, temp]
    endfor
    
    
  endif ;~keyword_set(nopreload)
  
  
  
  
  
  
  if keyword_set(getlogstruct) then begin
    steptemp = {imnr:0L, $
      time:0.0, $
      type:'', $
      obsrep:0L, $
      obsentry:0L, $
      frmrep:0L, $
      frmentry:0L, $
      pztx:0.0, $
      pzty:0.0, $
      fw:0, $
      focus:0, $
      flush:0, $
      frmid:0UL, $
      fdbid:0UL, $
      crsid:0L, $
      lut:0L, $
      compn:0, $
      compk:0, $
      exp:0L, $
      sumx:0, $
      sumy:0, $
      fovy:0.0, $
      file1f:0L, $
      file2f:0L, $
      file1n:0L, $
      file2n:0L}
    obstemp = {nimages:0L, $
      obsid:0UL, $
      logstep:make_array(3000, value=steptemp)}
    logall = make_array(N_ELEMENTS(filesIN), value=obstemp)
  endif
  
  
  dircounter=-1
  for nfile=0,N_ELEMENTS(filesIN)-1 do begin
    if keyword_set(timepOBS) then t1=systime(1)
    logdircurrent = logdir
    if logonly && (strpos(logdir, '#') ge 0) then begin
      if (nfile MOD maxfilesperfolder) eq 0 then dircounter=dircounter+1
      logdircurrent = fns(logdir, dircounter)
    endif
    
    
    if ~keyword_set(nopreload) then begin
    
    
    
      listObject->Reset
      listObject->parseFile, OBSdirIN+filesIN[nfile]       ;read and parse file
      OBSList = listObject->getArray()      ;get structure with observation list table
      
      
      
      file_app = strsplit(filesIN[nfile], ObsList.ID, /extract, /regex)
      file_app = file_app[1]
      
      
      
      
      
      ;get FRM, which are used by this OBS
      for nrFRM=0,OBSList.NumEntries-1 do begin ;loop over whole OBS, different FRMs
        if nrFRM eq 0 then begin
          ind = where(FRMListall.ID eq (*(OBSList).FRM_ID)[nrFRM], count)
          if count eq 0 then begin
            ;search special FRM
            filename='FRM-'+(*(OBSList).FRM_ID)[nrFRM]+file_app
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
                  break
                endif
              endif
            endif
            file = file[0]
            
            listObject->Reset
            listObject->parseFile, file
            FRMListtemp = listObject->getArray()
            
          endif else FRMListtemp = FRMListall[ind[0]]
          FRMList = FRMListtemp
          
        endif else begin
          ind = where(FRMList.ID eq (*(OBSList).FRM_ID)[nrFRM], count)
          if count eq 0 then begin
            ind = where(FRMListall.ID eq (*(OBSList).FRM_ID)[nrFRM], count)
            if count eq 0 then begin
              ;search special FRM
              filename='FRM-'+(*(OBSList).FRM_ID)[nrFRM]+file_app
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
                    break
                  endif
                endif
              endif
              file = file[0]
              
              listObject->Reset
              listObject->parseFile, file
              FRMListtemp = listObject->getArray()
              
            endif else FRMListtemp = FRMListall[ind[0]]
            FRMList = [FRMList, FRMListtemp]
          endif
        endelse
      endfor
      
      FDBList=0
      for currentFRM=0,N_ELEMENTS(FRMList)-1 do begin
        ;get FDBs of SJI
        for nrFDB=0,FRMList[currentFRM].NumEntries-1 do begin
          if (*(FRMList[currentFRM]).SJI_FDB_ID)[nrFDB] ne '0000000000' then begin
            ;if N_ELEMENTS(FDBList) eq 0 then begin
            if size(FDBList, /Type) ne 8 then begin
              ind = where(FDBListall.ID eq (*(FRMList[currentFRM]).SJI_FDB_ID)[nrFDB], count)
              if count eq 0 then begin
                print,'mapping failed, did not find FDB'
                return
              endif
              FDBList = FDBListall[ind[0]]
            endif else begin
              ind = where(FDBList.ID eq (*(FRMList[currentFRM]).SJI_FDB_ID)[nrFDB], count)
              if count eq 0 then begin
                ind = where(FDBListall.ID eq (*(FRMList[currentFRM]).SJI_FDB_ID)[nrFDB], count)
                if count eq 0 then begin
                  print,'mapping failed, did not find FDB'
                  return
                endif
                FDBList = [FDBList, FDBListall[ind[0]]]
              endif
            endelse
          endif ;(*(FRMList[currentFRM]).SJI_FDB_ID)[nrFDB] ne '0'
          
          
          ;get FDBs of NUV
          if (*(FRMList[currentFRM]).NUV_SG_FDB_ID)[nrFDB] ne '0000000000' then begin
            ;if N_ELEMENTS(FDBList) eq 0 then begin
            if size(FDBList, /Type) ne 8 then begin
              ind = where(FDBListall.ID eq (*(FRMList[currentFRM]).NUV_SG_FDB_ID)[nrFDB], count)
              if count eq 0 then begin
                print,'mapping failed, did not find FDB'
                return
              endif
              FDBList = FDBListall[ind[0]]
            endif else begin
              ind = where(FDBList.ID eq (*(FRMList[currentFRM]).NUV_SG_FDB_ID)[nrFDB], count)
              if count eq 0 then begin
                ind = where(FDBListall.ID eq (*(FRMList[currentFRM]).NUV_SG_FDB_ID)[nrFDB], count)
                if count eq 0 then begin
                  print,'mapping failed, did not find FDB'
                  return
                endif
                FDBList = [FDBList, FDBListall[ind[0]]]
              endif
            endelse
          endif ;(*(FRMList[currentFRM]).NUV_SG_FDB_ID)[nrFDB] ne '0'
          
          
          ;get FDBs of FUV
          if (*(FRMList[currentFRM]).FUV_SG_FDB_ID)[nrFDB] ne '0000000000' then begin
            ;if N_ELEMENTS(FDBList) eq 0 then begin
            if size(FDBList, /Type) ne 8 then begin
              ind = where(FDBListall.ID eq (*(FRMList[currentFRM]).FUV_SG_FDB_ID)[nrFDB], count)
              if count eq 0 then begin
                print,'mapping failed, did not find FDB'
                return
              endif
              FDBList = FDBListall[ind[0]]
            endif else begin
              ind = where(FDBList.ID eq (*(FRMList[currentFRM]).FUV_SG_FDB_ID)[nrFDB], count)
              if count eq 0 then begin
                ind = where(FDBListall.ID eq (*(FRMList[currentFRM]).FUV_SG_FDB_ID)[nrFDB], count)
                if count eq 0 then begin
                  print,'mapping failed, did not find FDB'
                  return
                endif
                FDBList = [FDBList, FDBListall[ind[0]]]
              endif
            endelse
            
          endif ;(*(FRMList[currentFRM]).FUV_SG_FDB_ID)[nrFDB] ne '0'
        endfor ;nrFDB=0,FRMList[currentFRM].NumEntries-1
      endfor ;currentFRM=0,N_ELEMENTS(FRMList)-1
      
      
      
      for currentFDB=0,N_ELEMENTS(FDBList)-1 do begin
        ;get current CRS from FDB
        ;if N_ELEMENTS(CRSList) eq 0 then begin
        if currentFDB eq 0 then begin
          ind = where(CRSListall.ID eq FDBList[currentFDB].CRSID, count)
          if count eq 0 then begin
            print,'mapping failed, did not find CRS'
            return
          endif
          CRSList = CRSListall[ind[0]]
        endif else begin
          ind = where(CRSList.ID eq FDBList[currentFDB].CRSID, count)
          if count eq 0 then begin
            ind = where(CRSListall.ID eq FDBList[currentFDB].CRSID, count)
            if count eq 0 then begin
              print,'mapping failed, did not find CRS'
              return
            endif
            CRSList = [CRSList, CRSListall[ind[0]]]
          endif
        endelse
      endfor ;currentFDB=0,N_ELEMENTS(FDBList)-1
      
      
      
      
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
      
    endif ;~keyword_set(nopreload)
    
    
    
    
    ;print,files[nfile]
    
    IRISsim_syntool, OBSfile=filesIN[nfile], $
      OBSdir=OBSdirIN, $
      OBSrep=OBSreps[nfile], $
      outputfolder=outputfolder, $
      rasterSJI=rasterSJI, $
      savefitsfiles=savefitsfiles, $
      saveplot=saveplot, $
      noplot=noplot, $
      makemovie=makemovie, $
      playmovie=playmovie, $
      savelogcsv=savelogcsv, $
      savelogtxt=savelogtxt, $
      copyxml=copyxml, $
      logonly=logonly, $
      logdir=logdircurrent, $
      logdata=logdata, $
      SJIframe=SJIframe, $
      maxWINsize=maxWINsize, $
      quicktimeloc=quicktimeloc, $
      threshold_Overview_SJI=threshold_Overview_SJI, $
      threshold_SJI=threshold_SJI, $
      threshold_Spec=threshold_Spec, $
      inputdirFUV = inputdirFUV, $
      inputdirNUV = inputdirNUV, $
      inputfileFUV = inputfileFUV, $
      inputfileNUV = inputfileNUV, $
      dtimeSIM = dtimeSIM, $
      dtimend = dtimend, $
      savelogfull = savlogfull, $
      logfulldir = logfulldir, $
      debug=debug, $
      debtime=debtime, $
      OBSList=OBSList, $
      FRMList=FRMList, $
      FDBList=FDBList, $
      CRSList=CRSList, $
      FRMdir=FRMdir, $
      FDBdir=FDBdir, $
      CRSdir=CRSdir, $
      FRMinOBS=FRMinOBS, $
      FDBinFRM=FDBinFRM, $
      CRSinFDB=CRSinFDB, $
      totalsteps=totalsteps, $
      quiet=quiet
      
    if keyword_set(getlogstruct) then begin
      logall[nfile].nimages = N_ELEMENTS(logdata)
      logall[nfile].obsid = ulong(logdata[0].obsid)
      n = logall[nfile].nimages-1
      logall[nfile].logstep[0:n].frmid = ulong(logdata.frmid)
      logall[nfile].logstep[0:n].fdbid = ulong(logdata.fdbid)
      logall[nfile].logstep[0:n].crsid = long(logdata.crsid)
      logall[nfile].logstep[0:n].imnr = logdata.imnr
      logall[nfile].logstep[0:n].time = logdata.simtime/1000.0
      logall[nfile].logstep[0:n].type = logdata.type
      logall[nfile].logstep[0:n].obsrep = logdata.obsrep
      logall[nfile].logstep[0:n].obsentry = logdata.obsentry
      logall[nfile].logstep[0:n].frmrep = logdata.frmrep
      logall[nfile].logstep[0:n].frmentry = logdata.frmentry
      logall[nfile].logstep[0:n].pztx = logdata.pztxy[0]
      logall[nfile].logstep[0:n].pzty = logdata.pztxy[1]
      logall[nfile].logstep[0:n].fw = logdata.fw
      logall[nfile].logstep[0:n].focus = logdata.focus
      logall[nfile].logstep[0:n].flush = logdata.flushcurrent
      logall[nfile].logstep[0:n].lut = logdata.lut
      logall[nfile].logstep[0:n].compn = logdata.compn
      logall[nfile].logstep[0:n].compk = logdata.compk
      logall[nfile].logstep[0:n].exp = logdata.exptime
      logall[nfile].logstep[0:n].sumx = logdata.sumspec
      logall[nfile].logstep[0:n].sumy = logdata.sumspat
      logall[nfile].logstep[0:n].fovy = logdata.fovy
      logall[nfile].logstep[0:n].file1f = logdata.sim1
      logall[nfile].logstep[0:n].file2f = logdata.sim2
      logall[nfile].logstep[0:n].file1n = logdata.sim3
      logall[nfile].logstep[0:n].file1n = logdata.sim4
    endif
    
    
    if keyword_set(timepOBS) then begin
      t2=systime(1)
      dt=(t2-t1)
      if nfile eq 0 then begin
        dta=dt
        st=totalsteps
      endif else begin
        dta=[dta,dt]
        st=[st,totalsteps]
      endelse
    endif
  endfor
  if keyword_set(timepOBS) then timepOBS=[[dta], [st]]
END
