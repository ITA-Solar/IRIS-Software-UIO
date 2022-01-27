pro IRISsim_writeLogFile, savelogcsv, savelogtxt, savelogfull, logFilecsvName, logFiletxtName, logFilefullName, $
    csvUnit, txtUnit, fullUnit, header, endline, $
    logdata, $
    filedirfuv=filedirfuv, filedirnuv=filedirnuv, datestamp=datestamp, OBSid=OBSid, version=version, $
    logfulldir=logfulldir
    
; $Id: irissim_writelogfile.pro,v 1.10 2013/09/16 13:20:00 mawiesma Exp $  ;

  if header then begin
    ;create header of logfiles
  
    ;create csv-logfile
    if savelogcsv then begin
      logFilecsvName=datestamp+OBSid+'_log.csv'
      OpenW, csvUnit, logFilecsvName, /get_lun
      log='Log of IRIS simulator,Version:,'+version
      printf, csvUnit, log
      log='OBS ID:,'+OBSid
      printf, csvUnit, log
      log='Path of input files,FUV:,'+filedirfuv
      printf, csvUnit, log
      log=',NUV:,'+filedirnuv
      printf, csvUnit, log
      log='Image nr,Time [s],Type,OBSrep,OBSentry,FRMrep,FRMentry,'+$
        'PZTx,PZTy,PZTA,PZTB,PZTC,'+$
        'FW,Focus,Flush,'+$
        'FRM ID,FDB ID,CRS ID,'+$
        'LUT,CompN,CompK,Exp [ms],'+$
        'SumX,SumY,FovY,'+$
        'File 1 FUV,File 2 FUV,File 1 NUV,File 2 NUV'
      printf, csvUnit, log
      Close, csvUnit
    endif
    
    ;create txt-logfile
    if savelogtxt then begin
      logFiletxtName=datestamp+OBSid+'_log.txt'
      OpenW, txtUnit, logFiletxtName, /get_lun
      log='Log of IRIS simulator; Version: '+version
      printf, txtUnit, log
      log='OBS ID: '+OBSid
      printf, txtUnit, log
      log='Path of input files; FUV: '+filedirfuv
      printf, txtUnit, log
      log='                     NUV: '+filedirnuv
      printf, txtUnit, log
      log=' IM nr |   Time [s] | Type | OBSrep | OBSentry | FRMrep | FRMentry | '+$
        '   PZTx |    PZTy | '+$
        '  FW | Focus | Flush | '+$
        '    FRM ID |     FDB ID |     CRS ID | '+$
        ' LUT | CompN | CompK | Exp [ms] | '+$
        'SumX | SumY |   FovY | '+$
        'File1 F | File2 F | File1 N | File2 N'
      l=strlen(log)
      log2='='
      for i=0,l-2 do log2=log2+'='
      printf, txtUnit, log2
      printf, txtUnit, log
      printf, txtUnit, log2
      Close, txtUnit
    endif
    
    ;create full-logfile (used to create level 2 fits files)
    if savelogfull then begin
      logFilefullName=logfulldir+OBSid+'_FULLlog.csv'
      OpenW, fullUnit, logFilefullName, /get_lun
      log='Image nr,Time [s],Type,OBSrep,OBSentry,FRMrep,FRMentry,'+$
        'PZTx,PZTy,PZTA,PZTB,PZTC,'+$
        'FW,Focus,Flush,'+$
        'FRM ID,FDB ID,CRS ID,'+$
        'LUT,CompN,CompK,Exp [ms],'+$
        'SumX,SumY'+$
        'File 1 FUV,File 2 FUV,File 1 NUV,File 2 NUV,'+$
        'Number of regions,Startrow,Endrow,Startcol,Endcol (last 4: 8times),0'
      printf, fullUnit, log
      Close, fullUnit
    endif
    
  endif else begin
  
    if endline then begin
      ;close the files
    
      if savelogcsv then $
        free_lun, csvUnit
        
      if savelogtxt then $
        free_lun, txtUnit
        
      if savelogfull then $
        free_lun, fullUnit
        
    endif else begin
      ;write data into files
          
      ;write to the csv-log file
      if savelogcsv then begin
        OpenU, csvUnit, logFilecsvName, /append
        for i=0,N_ELEMENTS(logdata)-1 do begin
          log=string(logdata[i].imNR)+','+$
            string(logdata[i].SimTime/1000.)+','+$
            logdata[i].Type+','+$
            string(logdata[i].OBSrep+1)+','+$
            string(logdata[i].OBSentry+1)+','+$
            string(logdata[i].FRMrep+1)+','+$
            string(logdata[i].FRMentry+1)+','+$
            string(logdata[i].PZTxy[0])+','+$
            string(logdata[i].PZTxy[1])+','+$
            string(logdata[i].pztabc[0])+','+$
            string(logdata[i].pztabc[1])+','+$
            string(logdata[i].pztabc[2])+','+$
            string(logdata[i].FW)+','+$
            string(logdata[i].Focus)+','+$
            string(logdata[i].Flushcurrent)+','+$
            logdata[i].FRMid+','+$
            logdata[i].FDBid+','+$
            logdata[i].CRSid+','+$
            logdata[i].LUT+','+$
            string(logdata[i].CompN)+','+$
            string(logdata[i].CompK)+','+$
            string(logdata[i].Exptime)+','+$
            string(logdata[i].sumspec)+','+$
            string(logdata[i].sumspat)+','+$
            string(logdata[i].fovy)+','+$
            string(logdata[i].sim1)+','+$
            string(logdata[i].sim2)+','+$
            string(logdata[i].sim3)+','+$
            string(logdata[i].sim4)
          log=strcompress(log,/remove_all)
          printf, csvUnit, log
        endfor ;i
        Close, csvUnit
      endif
      
      ;write to the txt-log file
      if savelogtxt then begin
        OpenU, txtUnit, logFiletxtName, /append
        for i=0,N_ELEMENTS(logdata)-1 do begin
          FRMid=logdata[i].FRMid
          for m=strlen(FRMid),9 do FRMid=' '+FRMid
          FDBid=logdata[i].FDBid
          for m=strlen(FDBid),9 do FDBid=' '+FDBid
          CRSid=logdata[i].CRSid
          for m=strlen(CRSid),9 do CRSid=' '+CRSid
          LUT=logdata[i].LUT
          for m=strlen(logdata[i].LUT),3 do LUT=' '+LUT
          
          log=string(logdata[i].imNR, format='(I6)')+' | '+$
            string(logdata[i].SimTime/1000., format='(f10.3)')+' | '+$
            ' '+logdata[i].Type+' | '+$
            string(logdata[i].OBSrep+1, format='(I6)')+' | '+$
            string(logdata[i].OBSentry+1, format='(I8)')+' | '+$
            string(logdata[i].FRMrep+1, format='(I6)')+' | '+$
            string(logdata[i].FRMentry+1, format='(I8)')+' | '+$
            string(logdata[i].PZTxy[0], format='(f7.3)')+' | '+$
            string(logdata[i].PZTxy[1], format='(f7.3)')+' | '+$
            string(logdata[i].FW, format='(I4)')+' | '+$
            string(logdata[i].Focus, format='(I5)')+' | '+$
            string(logdata[i].Flushcurrent, format='(I5)')+' | '+$
            FRMid+' | '+$
            FDBid+' | '+$
            CRSid+' | '+$
            LUT+' | '+$
            string(logdata[i].CompN, format='(I5)')+' | '+$
            string(logdata[i].CompK, format='(I5)')+' | '+$
            string(logdata[i].Exptime, format='(I8)')+' | '+$
            string(logdata[i].sumspec, format='(I4)')+' | '+$
            string(logdata[i].sumspat, format='(I4)')+' | '+$
            string(logdata[i].fovy, format='(f6.2)')+' | '+$
            string(logdata[i].sim1, format='(I7)')+' | '+$
            string(logdata[i].sim2, format='(I7)')+' | '+$
            string(logdata[i].sim3, format='(I7)')+' | '+$
            string(logdata[i].sim4, format='(I7)')
          printf, txtUnit, log
        endfor ;i
        Close, txtUnit
      endif
      
      ;write to the FULL csv-log file
      if savelogfull then begin
        OpenU, fullUnit, logFilefullName, /append
        for i=0,N_ELEMENTS(logdata)-1 do begin
          log=string(logdata[i].imNR)+','+$
            string(logdata[i].SimTime/1000.)+','+$
            logdata[i].type+','+$
            string(logdata[i].OBSrep+1)+','+$
            string(logdata[i].OBSentry+1)+','+$
            string(logdata[i].FRMrep+1)+','+$
            string(logdata[i].FRMentry+1)+','+$
            string(logdata[i].PZTxy[0])+','+$
            string(logdata[i].PZTxy[1])+','+$
            string(logdata[i].pztabc[0])+','+$
            string(logdata[i].pztabc[1])+','+$
            string(logdata[i].pztabc[2])+','+$
            string(logdata[i].FW)+','+$
            string(logdata[i].Focus)+','+$
            string(logdata[i].Flushcurrent)+','+$
            logdata[i].FRMid+','+$
            logdata[i].FDBid+','+$
            logdata[i].CRSid+','+$
            logdata[i].LUT+','+$
            string(logdata[i].CompN)+','+$
            string(logdata[i].CompK)+','+$
            string(logdata[i].Exptime)+','+$
            string(logdata[i].sumspec)+','+$
            string(logdata[i].sumspat)+','+$
            string(logdata[i].sim1)+','+$
            string(logdata[i].sim2)+','+$
            string(logdata[i].sim3)+','+$
            string(logdata[i].sim4)+','+$
            string(logdata[i].numreg)+','
          for m=0,7 do begin
            log=log+string(logdata[i].startrow[m])+','+$
              string(logdata[i].endrow[m])+','+$
              string(logdata[i].startcol[m])+','+$
              string(logdata[i].endcol[m])+','
          endfor
          log=log+'0'
          log=strcompress(log,/remove_all)
          printf, fullUnit, log
        endfor ;i
        Close, fullUnit
      endif
      
    endelse
    
  endelse
  
end
