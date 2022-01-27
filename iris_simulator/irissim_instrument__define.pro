FUNCTION IRISsim_instrument::init, OBSList, FRMList, FDBList, CRSList, Date_OBS, $
    FRMinOBS, FDBinFRM, CRSinFDB, $
    savelogcsv, savelogtxt, datestamp, savelogfull, logfulldir, logonly, $
    filedirfuv, filedirnuv, $
    version
    
  ; $Id: irissim_instrument__define.pro,v 1.15 2013/09/25 09:18:06 mawiesma Exp $  ;
    
  self.OBSList=ptr_new(/allocate_heap)
  *(self).OBSList = OBSList
  self.FRMList=ptr_new(/allocate_heap)
  *(self).FRMList = FRMList
  self.FDBList=ptr_new(/allocate_heap)
  *(self).FDBList = FDBList
  self.CRSList=ptr_new(/allocate_heap)
  *(self).CRSList = CRSList
  
  self.FRMinOBS=ptr_new(/allocate_heap)
  *(self).FRMinOBS = FRMinOBS
  self.FDBinFRM=ptr_new(/allocate_heap)
  *(self).FDBinFRM = FDBinFRM
  self.CRSinFDB=ptr_new(/allocate_heap)
  *(self).CRSinFDB = CRSinFDB
  
  self.Date_OBS_utc = str2utc(Date_OBS)
  
  self.logonly = logonly
  self.savelogcsv = savelogcsv
  self.savelogtxt = savelogtxt
  self.savelogfull = savelogfull
  if N_ELEMENTS(datestamp) then $
    self.datestamp = datestamp
  if N_ELEMENTS(logfulldir) then $
    self.logfulldir = logfulldir
  self.filedirfuv = filedirfuv
  self.filedirnuv = filedirnuv
  self.version = version
  
  constants = obj_new('IRISsim_constants')
  self.PZTnoChangeValue = constants->get_PZTnoChangeValue()
  self.FWnoChangeValue = constants->get_FWnoChangeValue()
  self.FW = constants->get_FWinitial()
  obj_destroy, constants
  
  return, 1
END



PRO IRISsim_instrument::cleanup
  if ptr_valid(self.OBSList) then begin
    ;heap_free, *(self).OBSList
    ptr_free, self.OBSList
  endif
  if ptr_valid(self.FRMList) then begin
    ;heap_free, *(self).FRMList
    ptr_free, self.FRMList
  endif
  if ptr_valid(self.FDBList) then ptr_free, self.FDBList
  if ptr_valid(self.CRSList) then begin
    ;heap_free, *(self).CRSList
    ptr_free, self.CRSList
  endif
  if ptr_valid(self.FRMinOBS) then ptr_free, self.FRMinOBS
  if ptr_valid(self.FDBinFRM) then ptr_free, self.FDBinFRM
  if ptr_valid(self.CRSinFDB) then ptr_free, self.CRSinFDB
  if ptr_valid(self.logdata) then ptr_free, self.logdata
END



PRO IRISsim_instrument::newOBSrepeat, nrOBSrepeats
  self.nrOBSrepeats = nrOBSrepeats
  self.PZT_total = IRIS_xy2pzt(0,0)
END



PRO IRISsim_instrument::newFRM, nrFRM, currentFRM
  self.nrFRM = nrFRM
  
  OBSList = *(self).OBSList
  FRMList = *(self).FRMList
  
  ;check PZT offset of OBS
  if (*(OBSList).PZT_A_Abs)[nrFRM] ne self.PZTnoChangeValue || $
    (*(OBSList).PZT_B_Abs)[nrFRM] ne self.PZTnoChangeValue || $
    (*(OBSList).PZT_C_Abs)[nrFRM] ne self.PZTnoChangeValue then begin
    
    self.PZT_OBS = [(*(OBSList).PZT_A_Abs)[nrFRM], $
      (*(OBSList).PZT_B_Abs)[nrFRM], $
      (*(OBSList).PZT_C_Abs)[nrFRM]]
    self.add1Step=0
  endif else begin
    self.PZT_OBS = self.PZT_total
    self.add1Step=1
  endelse
  
  self.PZT_Step = [(*(OBSList).PZT_A_Step)[nrFRM], $
    (*(OBSList).PZT_B_Step)[nrFRM], $
    (*(OBSList).PZT_C_Step)[nrFRM]]
    
  ;get current FRM index
  currentFRM = (*(self).FRMinOBS)[nrFRM]
  
  self.currentFRM = currentFRM
END



PRO IRISsim_instrument::newFRMrepeat, nrFRMrepeats
  self.nrFRMrepeats = nrFRMrepeats
END



PRO IRISsim_instrument::newFDB, nrFDB, time, step, sim1, sim2, sim3, sim4, $
    PZTcurrent, filterwheel, currentFDB, currentCRS, error, init=init, $
    Date_OBS, T_OBS
    
  self.nrFDB = nrFDB
  
  OBSList = *(self).OBSList
  FRMList = *(self).FRMList
  FDBList = *(self).FDBList
  CRSList = *(self).CRSList
  
  ;calculate total PZT offset
  PZT_Off = [(*(FRMList[self.currentFRM]).PZT_A_Rel)[nrFDB], $
    (*(FRMList[self.currentFRM]).PZT_B_Rel)[nrFDB], $
    (*(FRMList[self.currentFRM]).PZT_C_Rel)[nrFDB]]
  self.PZT_Total = self.PZT_OBS + self.PZT_Step * (self.nrFRMrepeats + self.add1Step) + PZT_Off
  PZTcurrent = IRIS_pzt2xy(self.PZT_Total[0], self.PZT_Total[1], self.PZT_Total[2])
  
  ;change filterwheel value if necessary
  if (*(FRMList[self.currentFRM]).FW)[nrFDB] ne self.FWnoChangeValue then $
    self.FW = (*(FRMList[self.currentFRM]).FW)[nrFDB]
  filterwheel = self.FW
  
  ;calculate flush
  if (*(FRMList[self.currentFRM]).Flush)[nrFDB] eq -1 then flushc = (*(OBSList).Flush)[self.nrFRM] $
  else self.flushc = (*(FRMList[self.currentFRM]).Flush)[nrFDB]
  ;flushc = self.flushc
  
  ;calculate inhibitskip
  if (*(FRMList[self.currentFRM]).InhibitSkip)[nrFDB] eq -1 then flushc = (*(OBSList).InhibitSkip)[self.nrFRM] $
  else self.InhibitSkip = (*(FRMList[self.currentFRM]).InhibitSkip)[nrFDB]
  
  ;get current FDB indices
  currentFDB = reform((*(self).FDBinFRM)[self.currentFRM, nrFDB, *])
  
  ;get current CRS indices
  currentCRS = (*(self).CRSinFDB)[currentFDB]
  ind=where(currentFDB eq -1, count)
  if count gt 0 then currentCRS[ind]=-1
  
  self.currentFDB = currentFDB
  self.currentCRS = currentCRS
  
  ;calculate new time of this exposure
  self.simtime = time
  Date_OBS = utc2str(IRISsim_addTime(self.Date_OBS_utc, time))
  
  ;calculate midpoint of exposure
  T_OBS = strarr(3)
  ext=0L
  if currentFDB[0] ge 0 then $
    ext=[ext, FDBList[currentFDB[0]].Exp_Duration]
  if currentFDB[1] ge 0 then $
    ext=[ext, FDBList[currentFDB[1]].Exp_Duration]
  if currentFDB[2] ge 0 then $
    ext=[ext, FDBList[currentFDB[2]].Exp_Duration]
  maxex=max(ext)
  if currentFDB[0] ge 0 then $
    T_OBS[0] = utc2str(IRISsim_addTime(self.Date_OBS_utc, time+maxex-FDBList[currentFDB[0]].Exp_Duration/2d))
  if currentFDB[1] ge 0 then $
    T_OBS[1] = utc2str(IRISsim_addTime(self.Date_OBS_utc, time+maxex-FDBList[currentFDB[1]].Exp_Duration/2d))
  if currentFDB[2] ge 0 then $
    T_OBS[2] = utc2str(IRISsim_addTime(self.Date_OBS_utc, time+maxex-FDBList[currentFDB[2]].Exp_Duration/2d))
    
    
    
  ;log-files
  if ~keyword_set(init) then begin
  
    if step eq 0 then begin
      ;create logfiles
      if self.savelogcsv || self.savelogtxt || self.savelogfull then begin
        self.imNR=0
        if self.logonly then begin
          dirfuv='-'
          dirnuv='-'
        endif else begin
          dirfuv=self.filedirfuv
          dirnuv=self.filedirnuv
        endelse
        IRISsim_writeLogFile, self.savelogcsv, self.savelogtxt, self.savelogfull, logFilecsvName, logFiletxtName, logFilefullName, $
          csvUnit, txtUnit, fullUnit, 1, $
          filedirfuv=dirfuv, filedirnuv=dirnuv, datestamp=self.datestamp, OBSid=OBSList.ID, version=self.version, $
          logfulldir=self.logfulldir
        if self.savelogfull then begin
          self.logFilefullName=logFilefullName
          self.fullUnit=fullUnit
        endif
        if self.savelogcsv then begin
          self.logFilecsvName=logFilecsvName
          self.csvUnit=csvUnit
        endif
        if self.savelogtxt then begin
          self.logFiletxtName=logFiletxtName
          self.txtUnit=txtUnit
        endif
      endif
      
      
    endif
    
    
    ;create log structure
    ;some values are the same for all possible 3 exposures (FUV, NUV, SJI)
    ;some are different
    logtemp = {step:step, $
      imNR:0L, $
      SimTime:time, $
      Type:'', $
      OBSreptot:OBSList.Repeat_Obs, $
      OBSrep:self.nrOBSrepeats, $
      OBSentrytot:OBSList.NumEntries, $
      OBSentry:self.nrFRM, $
      FRMreptot:(*(OBSList).Repeat_FRM)[self.nrFRM], $
      FRMrep:self.nrFRMrepeats, $
      FRMentrytot:FRMList[self.currentFRM].NumEntries, $
      FRMentry:self.nrFDB, $
      PZTabc:self.PZT_Total, $
      PZTxy:PZTcurrent, $
      FW:self.FW, $
      Focus:(*(FRMList[self.currentFRM]).Focus)[self.nrFDB], $
      Flushcurrent:self.Flushc, $
      Inhibitskip:self.InhibitSkip, $
      OBSid:OBSList.ID, $
      FRMid:FRMList[self.currentFRM].ID, $
      FDBid:'', $
      CRSid:'', $
      LUT:'', $
      CompN:0L, $
      CompK:0L, $
      Exptime:0L, $
      ExpType:0L, $
      FOVy:0.0, $
      sim1:sim1, $
      sim2:sim2, $
      sim3:sim3, $
      sim4:sim4, $
      sumspat:0, $
      sumspec:0, $
      numreg:0, $
      startrow:intarr(8), $
      endrow:intarr(8), $
      startcol:intarr(8), $
      endcol:intarr(8)}
      
    ;SJI
    if currentFDB[0] ge 0 then begin
      log1=logtemp
      self.imNR = self.imNR + 1
      log1.imNR = self.imNR
      log1.type = 'SJI'
      log1.FDBid = FDBList[currentFDB[0]].ID
      log1.CRSid = CRSList[currentCRS[0]].ID
      log1.LUT = FDBList[currentFDB[0]].LookupTableID
      log1.CompN = FDBList[currentFDB[0]].CompN
      log1.CompK = FDBList[currentFDB[0]].CompK
      log1.exptime = FDBList[currentFDB[0]].Exp_Duration
      log1.ExpType = FDBList[currentFDB[0]].Exp_Type
      log1.sumspat = CRSList[currentCRS[0]].Spatial
      log1.sumspec = CRSList[currentCRS[0]].Spectral
      log1.numreg = CRSList[currentCRS[0]].SubRegions
      for i=0,7 do begin
        if i lt CRSList[currentCRS[0]].SubRegions then begin
          log1.startrow[i] = (*(CRSList[currentCRS[0]]).Startrow)[i]
          log1.endrow[i] = (*(CRSList[currentCRS[0]]).Startrow)[i] + (*(CRSList[currentCRS[0]]).Numrows)[i] - 1
          log1.startcol[i] = (*(CRSList[currentCRS[0]]).Startcol)[i]
          log1.endcol[i] = (*(CRSList[currentCRS[0]]).Startcol)[i] + (*(CRSList[currentCRS[0]]).Numcols)[i] - 1
        endif
      endfor
      log1.fovy = max(log1.endcol-log1.startcol)/6.
      log = log1
    endif
    
    ;NUV
    if currentFDB[1] ge 0 then begin
      log1=logtemp
      self.imNR = self.imNR + 1
      log1.imNR = self.imNR
      log1.type = 'NUV'
      log1.FDBid = FDBList[currentFDB[1]].ID
      log1.CRSid = CRSList[currentCRS[1]].ID
      log1.LUT = FDBList[currentFDB[1]].LookupTableID
      log1.CompN = FDBList[currentFDB[1]].CompN
      log1.CompK = FDBList[currentFDB[1]].CompK
      log1.exptime = FDBList[currentFDB[1]].Exp_Duration
      log1.ExpType = FDBList[currentFDB[1]].Exp_Type
      log1.sumspat = CRSList[currentCRS[1]].Spatial
      log1.sumspec = CRSList[currentCRS[1]].Spectral
      log1.numreg = CRSList[currentCRS[1]].SubRegions
      for i=0,7 do begin
        if i lt CRSList[currentCRS[1]].SubRegions then begin
          log1.startrow[i] = (*(CRSList[currentCRS[1]]).Startrow)[i]
          log1.endrow[i] = (*(CRSList[currentCRS[1]]).Startrow)[i] + (*(CRSList[currentCRS[1]]).Numrows)[i] - 1
          log1.startcol[i] = (*(CRSList[currentCRS[1]]).Startcol)[i]
          log1.endcol[i] = (*(CRSList[currentCRS[1]]).Startcol)[i] + (*(CRSList[currentCRS[1]]).Numcols)[i] - 1
        endif
      endfor
      log1.fovy = max(log1.endcol-log1.startcol)/6.
      if N_ELEMENTS(log) eq 0 then log = log1 $
      else log = [log, log1]
    endif
    
    ;FUV
    if currentFDB[2] ge 0 then begin
      log1=logtemp
      self.imNR = self.imNR + 1
      log1.imNR = self.imNR
      log1.type = 'FUV'
      log1.FDBid = FDBList[currentFDB[2]].ID
      log1.CRSid = CRSList[currentCRS[2]].ID
      log1.LUT = FDBList[currentFDB[2]].LookupTableID
      log1.CompN = FDBList[currentFDB[2]].CompN
      log1.CompK = FDBList[currentFDB[2]].CompK
      log1.exptime = FDBList[currentFDB[2]].Exp_Duration
      log1.ExpType = FDBList[currentFDB[2]].Exp_Type
      log1.sumspat = CRSList[currentCRS[2]].Spatial
      log1.sumspec = CRSList[currentCRS[2]].Spectral
      log1.numreg = CRSList[currentCRS[2]].SubRegions
      for i=0,7 do begin
        if i lt CRSList[currentCRS[2]].SubRegions then begin
          log1.startrow[i] = (*(CRSList[currentCRS[2]]).Startrow)[i]
          log1.endrow[i] = (*(CRSList[currentCRS[2]]).Startrow)[i] + (*(CRSList[currentCRS[2]]).Numrows)[i] - 1
          log1.startcol[i] = (*(CRSList[currentCRS[2]]).Startcol)[i]
          log1.endcol[i] = (*(CRSList[currentCRS[2]]).Startcol)[i] + (*(CRSList[currentCRS[2]]).Numcols)[i] - 1
        endif
      endfor
      log1.fovy = max(log1.endcol-log1.startcol)/6.
      if N_ELEMENTS(log) eq 0 then log = log1 $
      else log = [log, log1]
    endif
    
    
    
    
    ;write to the log file
    if ~self.logonly then $
      IRISsim_writeLogFile, self.savelogcsv, self.savelogtxt, self.savelogfull, self.logFilecsvName, self.logFiletxtName, self.logFilefullName, $
      self.csvUnit, self.txtUnit, self.fullUnit, 0, 0, $
      log
      
    if step eq 0 then begin
      self.logdata = ptr_new(/allocate_heap)
      *(self).logdata = log
    endif else begin
      *(self).logdata = [*(self).logdata, log]
    endelse
    
  endif ;~keyword_set(init) :log-files
  
END


PRO IRISsim_instrument::endsim, log
  ;write to the log file, all in one piece, logonly has been set
  log=*(self).logdata
  if self.logonly then $
    IRISsim_writeLogFile, self.savelogcsv, self.savelogtxt, self.savelogfull, self.logFilecsvName, self.logFiletxtName, self.logFilefullName, $
    self.csvUnit, self.txtUnit, self.fullUnit, 0, 0, $
    *(self).logdata
    
  IRISsim_writeLogFile, self.savelogcsv, self.savelogtxt, self.savelogfull, self.logFilecsvName, self.logFiletxtName, self.logFilefullName, $
    self.csvUnit, self.txtUnit, self.fullUnit, 0, 1
END



PRO IRISsim_instrument__define

  void = {IRISsim_instrument, $
    OBSList:ptr_new(), $
    FRMList:ptr_new(), $
    FDBList:ptr_new(), $
    CRSList:ptr_new(), $
    FRMinOBS:ptr_new(), $
    FDBinFRM:ptr_new(), $
    CRSinFDB:ptr_new(), $
    Date_OBS_utc:{CDS_INT_TIME, MJD: 0L, TIME: 0L}, $
    simtime:0L, $
    PZT_OBS:intarr(3), $
    PZT_Total:intarr(3), $
    PZT_Step:intarr(3), $
    add1Step:0, $
    FW:0, $
    Flushc:0, $
    InhibitSkip:0, $
    currentFRM:0, $
    currentFDB:intarr(3), $
    currentCRS:intarr(3), $
    savelogcsv:0, $
    logFilecsvName:'', $
    csvUnit:0L, $
    savelogtxt:0, $
    logFiletxtName:'', $
    txtUnit:0L, $
    savelogfull:0, $
    logFilefullName:'', $
    fullUnit:0L, $
    logonly:0, $
    logdata:ptr_new(), $
    datestamp:'', $
    logfulldir:'', $
    filedirfuv:'', $
    filedirnuv:'', $
    version:'', $
    nrOBSrepeats:0L, $
    nrFRM:0L, $
    nrFRMrepeats:0L, $
    nrFDB:0L, $
    imNR:0L, $
    PZTnoChangeValue:0L, $
    FWnoChangeValue:0L $
    }
    
END
