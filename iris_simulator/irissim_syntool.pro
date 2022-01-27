pro IRISsim_syntool_event, event
  uname = widget_info(event.id,/uname)
  if uname eq 'ABORT' then begin
    widget_control, event.id, set_uvalue=1
    progress_id = widget_info(event.top,find_by_uname = 'PROGRESS')
    widget_control,progress_id,get_uvalue = o
    o->set_property,label = 'Aborting...'
  endif
  
end








PRO IRISsim_syntool, OBSfile=OBSfile, OBSdir=OBSdir, inputdirFUV=inputdirFUV, inputdirNUV=inputdirNUV, $
    inputfileFUV=inputfileFUV, inputfileNUV=inputfileNUV, dtimeSIM=dtimeSIM, dtimend=dtimend, $
    SJIframe=SJIframe, rasterSJI=rasterSJI, maxWINsize=maxWINsize, $
    outputfolder=outputfolder, $
    savefitsfiles=savefitsfiles, saveplot=saveplot, noplot=noplot, savelogcsv=savelogcsv, savelogtxt=savelogtxt, $
    copyxml=copyxml, makemovie=makemovie, playmovie=playmovie, quicktimeloc=quicktimeloc, $
    logonly=logonly, logdir=logdir, savelogfull=savelogfull, logfulldir=logfulldir, $
    logdata=logdata, simendtime=simendtime, $
    OBSrep=OBSrep, $
    threshold_Overview_SJI=threshold_Overview_SJI, threshold_SJI=threshold_SJI, threshold_Spec=threshold_Spec, $
    OBSList=OBSList, FRMList=FRMList, FDBList=FDBList, CRSList=CRSList, $
    FRMinOBS=FRMinOBS, FDBinFRM=FDBinFRM, CRSinFDB=CRSinFDB, $
    FRMdir=FRMdir, FDBdir=FDBdir, CRSdir=CRSdir, $
    quiet=quiet, $
    debug=debug, debtime=debtime, totalsteps=totalsteps
  ;+
  ; NAME:
  ;       IRISsim_SYNTOOL
  ;
  ; PURPOSE:
  ;       simulates an IRIS observation series
  ;       logics included:
  ;         -reads xml-input files, and copies these files into target-folder if desired
  ;         -reads data input files from simulation; SJI: FUV1, FUV2, NUV1, NUV2; Spectras: FUV1, FUV2, NUV
  ;         -handles the PZT-logic of the instrument
  ;         -interpolation of input data in all dimensions, except wavelength (i.e. time, x, y)
  ;         -binning of data
  ;         -input data is multiplied by exposure time (assumed input: datacount/s, output: datacount)
  ;         -fits-files are created, data is zeroed outside of CRS-windows
  ;         -FUV, NUV and SJI get each a separate file
  ;         -keywords are included in header
  ;
  ; CALLING SEQUENCE:
  ;       syntool (see synt.pro)
  ;
  ; INPUT PARAMETER:
  ;       see manual
  ;
  ; KEYWORDS:
  ;       see manual
  ;
  ; OUTPUTS:
  ;       see manual
  ;
  ; COMMON BLOCKS:
  ;       none
  ;
  ; MODIFICATION HISTORY:
  ;       11-Jun-2012  M. Wiesmann, UIO
  ;       works best with xml-files created by irisplantool_20121017.jar by Ankur Somani (LMSAL)
  ;       works also with later versions, it was tested also with build irisplantool_20130109
  ; the full changelog can be found in a separate document called 'Changelog_syntool.rtf'
  ;
  ; $Id: irissim_syntool.pro,v 1.52 2019/08/08 08:09:11 mawiesma Exp $  ;
  ;-
    
  version='SIM-2013-09-25'
  
  
  ; INPUT
  if ~keyword_set(OBSList) then begin
    if ~keyword_set(OBSfile) then begin
      ;OBSfile = 'OBS-T-00901_Q.xml'   ; The OBS filename (e.g. 'OBS-T-00004.xml')
      if keyword_set(OBSdir) then path=OBSdir else path=''
      file=dialog_pickfile(/read, filter=['OBS*.xml'], path=path, title='Please select an OBS file')
      if file eq '' then return
      OBSfileIN = file_basename(file)
      OBSdirIN = file_dirname(file, /mark_directory)
    endif else begin
      if ~keyword_set(OBSdir) then begin
        ;OBSdir = 'XML_bart_121025/OBS/'       ; OBS-file directory (e.g. 'XMLfiles/')
        OBSdirIN = file_dirname(OBSfile, /mark_directory)
        OBSfileIN = file_basename(OBSfile)
      endif else begin
        OBSdirIN = OBSdir
        OBSfileIN = OBSfile
      endelse
    endelse
  endif else begin
    if ~keyword_set(OBSfile) then begin
      OBSfileIN = ''
      OBSdirIN = ''
    endif else begin
      if ~keyword_set(OBSdir) then begin
        OBSdirIN = file_dirname(OBSfile, /mark_directory)
        OBSfileIN = file_basename(OBSfile)
      endif else begin
        OBSdirIN = OBSdir
        OBSfileIN = OBSfile
      endelse
    endelse
  endelse
  if ~N_ELEMENTS(inputdirFUV) then $
    inputdirFUV = '';'Data_cb24bih/' ; directory of input FUV files (e.g. 'Data_cb24bih/')
  if ~N_ELEMENTS(inputdirNUV) then $
    inputdirNUV = '';'Data_cb24bih/' ; directory of input NUV files (e.g. 'Data_cb24bih/')
  if ~N_ELEMENTS(inputfileFUV) then $
    inputfileFUV = 'iris_synthetic_fuv_*.fits' ; format of input FUV files (as used in wildcards or strmatch, where * is placed where the file number is, e.g. 'iris_synthetic_fuv_*.fits')
  if ~N_ELEMENTS(inputfileNUV) then $
    inputfileNUV = 'iris_synthetic_NUVlines_cb24bih_s*.fits' ; format of input FUV files (as used in wildcards or strmatch, where * is placed where the file number is, e.g. 'iris_synthetic_nuv_cb24bih_*.fits')
  ; assumptions about the input files:
  ; - they all have the same filename length (that is, FUV and NUV can have different filename lengths, but all FUV, or NUV respectively, have the same length)
  ; - file numbers are between 100 and 999
  ; - there are no other 3- or more digit numbers in the filename
  ; - dtimeSIM, defined below, is valid for consecutive numbers for all files (time-equidistance)
  if ~N_ELEMENTS(dtimeSIM) then $
    dtimeSIM = 10000L         ; time steps between simulation files (in ms)
  if ~N_ELEMENTS(dtimend) then $
    dtimeSIMend = 30000L $     ; time step between last and first file (in case OBS is longer than the time series from the simulation)
  else dtimeSIMend=dtimend
  if ~N_ELEMENTS(SJIframe) then $
    SJIframe=2d               ; visible data around overview SJI (in arcsec), 'Frame'
  if ~N_ELEMENTS(maxWINsize) then $
    maxWINsize=[1400, 1000]   ; max size of plot window [x, y]
  if ~N_ELEMENTS(rasterSJI) then $
    rasterSJI='fu1'           ; ('fu1', 'fu2', 'nu1', 'nu2') choose what SJI data is shown in the overview SJI graph (which will be updated on every step), NUV2 not available yet
  if ~N_ELEMENTS(outputfolder) then $
    outputfolder=''           ; select the folder, in which to save the data
    
  ;some keywords
  if ~N_ELEMENTS(savefitsfiles) then $
    savefitsfiles=0 ;set to 1 if you want to save the fits files
  if ~N_ELEMENTS(saveplot) then $
    saveplot=0      ;set to 1 if you want to save the images, otherwise they will be plotted on screen
  if ~N_ELEMENTS(makemovie) then $
    makemovie=0     ;set to 1 if you want to create the movie out of the pictures (saveplot should be 1), qt-tools required (http://omino.com/sw/qt_tools/), MAC only
  if ~N_ELEMENTS(playmovie) then $
    playmovie=0     ;set to 1 if you want to play the movie immediately after creating, MAC only
  if ~N_ELEMENTS(quicktimeloc) then $
    quicktimeloc='/Applications/QuickTime\ Player.app/Contents/MacOS/QuickTime\'  ;location of your quicktime application, required for playmovie, MAC only
  if ~N_ELEMENTS(savelogcsv) then $
    savelogcsv=0    ;set to 1 if you want to save a logfile in csv-format (excel-readable)
  if ~N_ELEMENTS(savelogtxt) then $
    savelogtxt=0    ;set to 1 if you want to save a logfile in txt-format (human-readable)
  if ~N_ELEMENTS(savelogfull) then $
    savelogfull=0    ;set to 1 if you want to save a full logfile for level 2 processing
  if ~keyword_set(logonly) then $
    logonly=0        ;set to 1 if you only want the logfiles, without all the other output (savelogcsv/txt still have to be 1)
  if ~N_ELEMENTS(copyxml) then $
    copyxml=0       ;set to 1 if you want to copy the xml-files to the destination folder
  ; logonly: if keyword is set, the program produces only logfiles, and doesn't run the simulation
    
  ;for debugging:
  if ~N_ELEMENTS(debug) then begin
    debug=0         ;set to 1 to control directly, how many pictures will be calculated
    nrpics=1        ;set the number of pictures to be calculated in debug-mode (program stops when this number is reached)
  endif else nrpics=debug
  if ~N_ELEMENTS(debtime) then $
    debugtime=0 $    ;this time will be added to the real time in the plot output, when in debug-mode (in ms)
  else debugtime=debtime
  
  ;graph variables
  if ~N_ELEMENTS(threshold_Overview_SJI) then $
    threshold_SJI_Overview=0.0002 $;threshold for display used in histo_opt for overview SJI
  else threshold_SJI_Overview=threshold_Overview_SJI
  if ~N_ELEMENTS(threshold_SJI) then $
    threshold_SJI=0.002          ;threshold for display used in histo_opt for SJI(s)
  if ~N_ELEMENTS(threshold_Spec) then $
    threshold_Spec=0.02         ;threshold for display used in histo_opt for spectral images
    
    
  constants = obj_new('IRISsim_constants')
  
  ;more graph variables
  pcharsize = constants->get_pcharsize()
  pcharthick = constants->get_pcharthick()
  color_active = constants->get_color_active()
  color_inactive = constants->get_color_inactive()
  
  ;sizes of CCDs
  PixCCDxfuv1 = constants->get_PixCCDx()
  PixCCDyfuv1 = constants->get_PixCCDy()
  PixCCDxfuv2=PixCCDxfuv1 ;number of pixels on FUV2 CCD in x-direction
  PixCCDyfuv2=PixCCDyfuv1 ;number of pixels on FUV2 CCD in y-direction
  PixCCDxnuv=PixCCDxfuv1 ;number of pixels on NUV CCD in x-direction
  PixCCDynuv=PixCCDyfuv1 ;number of pixels on NUV CCD in y-direction
  PixCCDxfuvSJI = constants->get_PixCCDxSJI()
  PixCCDyfuvSJI=PixCCDyfuv1 ;number of pixels on FUV SJI CCD in y-direction
  PixCCDxnuvSJI = constants->get_PixCCDxSJI()
  PixCCDynuvSJI=PixCCDyfuv1 ;number of pixels on NUV SJI CCD in y-direction
  PZTnoChangeValue = constants->get_PZTnoChangeValue()
  
  
  oldexcept=!except
  !except=0
  
  
  
  ;get data from XML-files, if we don't have it yet
  if ~keyword_set(OBSList) then begin
    IRISsim_readtables, OBSfileIN, OBSdirIN, OBSList, FRMList, FDBList, CRSList, error, 0, '', $
      FRMdir=FRMdir, FDBdir=FDBdir, CRSdir=CRSdir, $
      FRMinOBS, FDBinFRM, CRSinFDB, $
      quiet=quiet
      
    ; check for errors while reading the XML-files
    if error then begin
      print,'There is something wrong with the given OBS file, exiting program'
      return
    endif
  endif
  
  
  
  ; get current date and time to create a new folder
  GET_UTC, Date_OBS, /CCSDS
  if savefitsfiles || saveplot || makemovie || savelogcsv || savelogtxt || copyxml || $
    (keyword_set(savelogfull) && ~keyword_set(logfulldir)) then begin
    if keyword_set(logonly) && keyword_set(logdir) then $
      datestamp=outputfolder+logdir $
    else $
      datestamp=outputfolder+time2file(Date_OBS,/seconds)+'_'+OBSList.ID+'_L1/'
    if ~keyword_set(logonly) then $
      print, 'saving data in folder: ', datestamp
    FILE_MKDIR, datestamp
    if (keyword_set(savelogfull) && ~keyword_set(logfulldir)) then logfulldir=datestamp
  endif
  
  
  ;save the xml-files to the above created folder if desired
  if copyxml && keyword_set(OBSfile) then $
    IRISsim_readtables, OBSfileIN, OBSdirIN, OBSList, FRMList, FDBList, CRSList, error, copyxml, datestamp, $
    FRMdir=FRMdir, FDBdir=FDBdir, CRSdir=CRSdir, $
    FRMinOBS, FDBinFRM, CRSinFDB, $
    /quiet
    
  if keyword_set(OBSrep) then OBSList.Repeat_Obs = OBSrep
  
  
  filedirfuv=inputdirFUV+inputfileFUV
  filedirnuv=inputdirNUV+inputfileNUV
  
  instrument = obj_new('IRISsim_instrument', OBSList, FRMList, FDBList, CRSList, Date_OBS, $
    FRMinOBS, FDBinFRM, CRSinFDB, $
    savelogcsv, savelogtxt, datestamp, savelogfull, logfulldir, logonly, $
    filedirfuv, filedirnuv, $
    version)
    
    
    
    
  ; get list of input files
  if ~keyword_set(logonly) then begin
    fuvfileList = find_file(filedirfuv)
    if (size(fuvfileList))[0] eq 0 then begin
      print, 'No FUV input files found'
      return
    endif
    if (size(fuvfileList))[1] eq 1 then begin
      print, 'Found only 1 FUV input file'
      return
    endif
    f=file_basename(fuvfileList)
    ff=strmid(f,stregex(f[0],'[1-9][0-9][0-9]'),3)
    fuvfilenumbers=uint(ff)
  endif else begin
    fuvfilenumbers = indgen(200)+385
  endelse
  
  if ~keyword_set(logonly) then begin
    nuvfileList = find_file(filedirnuv)
    if (size(nuvfileList))[0] eq 0 then begin
      print, 'No NUV input files found'
      return
    endif
    if (size(nuvfileList))[1] eq 1 then begin
      print, 'Found only 1 NUV input file'
      return
    endif
    f=file_basename(nuvfileList)
    ff=strmid(f,stregex(f[0],'[1-9][0-9][0-9]'),3)
    nuvfilenumbers=uint(ff)
    dummy = readfits(nuvfileList[0], header2, /silent)
    naxis1 = gt_tagval(header2, 'NAXIS1', missing=0)
    if naxis1 lt 2000 then begin
      box_message,'| Found the old NUV files, please download the NUV files from IRIS team page |'
      return
    endif
  endif else begin
    nuvfilenumbers = indgen(200)+385
  endelse
  
  
  
  
  if ~keyword_set(logonly) then begin
    ;set up progress bar
    progressbase = widget_base(/col, /tlb_frame_attr, XOffset=100, YOffset=100, title=OBSList.ID)
    progressbar =  cw_progress(progressbase, value = 0.0, /blue, uname = 'PROGRESS',    $
      xsize = 200, label = 'IRIS Simulation running', /frame, obj_ref = progressObject,  $
      bg_color = 'white')
    abortbutton = widget_button(progressbase, value = 'Abort', uname = 'ABORT', uvalue=0)
    
    widget_control, progressbase, /realize
    widget_control, progressbar, set_uvalue = progressObject
    xmanager,'IRISsim_syntool', progressbase, /no_block
  endif
  
  
  
  
  
  
  
  ;start simulation
  oldtime = 0
  localtimefuv = 0
  localtimenuv = 0
  step = 0L
  for nrOBSrepeats=0,OBSList.Repeat_Obs-1 do begin ; loop over OBS repeats
    if ~keyword_set(logonly) && ~keyword_set(quiet) then $
      print, 'OBS repeat ', nrOBSrepeats+1, ' of ', OBSList.Repeat_Obs
      
    instrument->newOBSrepeat, nrOBSrepeats
    
    OLreptStartTime = OBSList.Tref_Obs + (nrOBSrepeats * OBSList.Cadence_Obs)
    
    
    
    for nrFRM=0,OBSList.NumEntries-1 do begin ;loop over whole OBS, different FRMs
    
      instrument->newFRM, nrFRM, currentFRM
      
      if ~keyword_set(logonly) && ~keyword_set(quiet) then $
        print, 'current FRM ID: ', FRMList[currentFRM].ID
        
      OlentryStartTime = OlreptStartTime + (*(OBSList).Tref_FRM)[nrFRM]
      
      
      
      for nrFRMrepeats=0,(*(OBSList).Repeat_FRM)[nrFRM]-1 do begin ; loop over FRM repeats
      
        instrument->newFRMrepeat, nrFRMrepeats
        
        FlreptStartTime = OlentryStartTime + (nrFRMrepeats * (*(OBSList).Cadence_FRM)[nrFRM])
        if (nrOBSrepeats eq OBSList.Repeat_OBS-1) && $
          (nrFRM eq OBSList.NumEntries-1) && $
          (nrFRMrepeats eq (*(OBSList).Repeat_FRM)[nrFRM]-1) then $
            simendtime = OlentryStartTime + ((nrFRMrepeats+1) * (*(OBSList).Cadence_FRM)[nrFRM])
        
        
        
        for nrFDB=0,FRMList[currentFRM].NumEntries-1 do begin ; loop over current FRM, different entries
        
          time = FlreptStartTime + (*(FRMList[currentFRM]).Tref)[nrFDB]
          
          dt = time - oldtime
          oldtime = time
          localtimefuv = localtimefuv + dt
          localtimenuv = localtimenuv + dt
          
          
          
          
          
          
          if step eq 0 then begin
            ;load initial values, files
            ;FUV
            fuvfileind1=0
            fuvfileind2=1
            dtimeSIMfuv = dtimeSIM * (fuvfilenumbers[fuvfileind2]-fuvfilenumbers[fuvfileind1])
            while localtimefuv gt dtimeSIMfuv do begin
              localtimefuv = localtimefuv - dtimeSIMfuv
              fuvfileind1=fuvfileind2
              fuvfileind2=fuvfileind2+1
              if fuvfileind2 eq N_ELEMENTS(fuvfilenumbers) then begin
                fuvfileind2 = 0
                dtimeSIMfuv = dtimeSIMend
              endif else begin
                dtimeSIMfuv = dtimeSIM * (fuvfilenumbers[fuvfileind2]-fuvfilenumbers[fuvfileind1])
              endelse
            endwhile
            sim1=fuvfilenumbers[fuvfileind1]
            if ~keyword_set(logonly) then begin
              simfile1=file_basename(fuvfileList[fuvfileind1])
              if ~keyword_set(quiet) then $
                print, 'loading new data file: ', fuvfileList[fuvfileind1]
              fxread, fuvfileList[fuvfileind1], fuvSJI1, header
              fxbopen,unit,fuvfileList[fuvfileind1],1,header
              fxbread,unit,fuv11,'FUV 1'
              fxbread,unit,fuv12,'FUV 2'
              fxbclose,unit
            endif ;~keyword_set(logonly)
            
            sim2=fuvfilenumbers[fuvfileind2]
            if ~keyword_set(logonly) then begin
              simfile2=file_basename(fuvfileList[fuvfileind2])
              if ~keyword_set(quiet) then $
                print, 'loading new data file: ', fuvfileList[fuvfileind2]
              fxread, fuvfileList[fuvfileind2], fuvSJI2, header
              fxbopen,unit,fuvfileList[fuvfileind2],1,header
              fxbread,unit,fuv21,'FUV 1'
              fxbread,unit,fuv22,'FUV 2'
              fxbclose,unit
            endif ;~keyword_set(logonly)
            
            ;NUV
            nuvfileind1=0
            nuvfileind2=1
            dtimeSIMnuv = dtimeSIM * (nuvfilenumbers[nuvfileind2]-nuvfilenumbers[nuvfileind1])
            while localtimenuv gt dtimeSIMnuv do begin
              localtimenuv = localtimenuv - dtimeSIMnuv
              nuvfileind1=nuvfileind2
              nuvfileind2=nuvfileind2+1
              if nuvfileind2 eq N_ELEMENTS(nuvfilenumbers) then begin
                nuvfileind2 = 0
                dtimeSIMnuv = dtimeSIMend
              endif else begin
                dtimeSIMnuv = dtimeSIM * (nuvfilenumbers[nuvfileind2]-nuvfilenumbers[nuvfileind1])
              endelse
            endwhile
            sim3=nuvfilenumbers[nuvfileind1]
            if ~keyword_set(logonly) then begin
              simfile3=file_basename(nuvfileList[nuvfileind1])
              if ~keyword_set(quiet) then $
                print, 'loading new data file: ', nuvfileList[nuvfileind1]
              nuv1 = readfits(nuvfileList[nuvfileind1], header2, /silent)
              nuvSJI1 = readfits(nuvfileList[nuvfileind1], header22, exten_no=1, /silent)
              nuvSJI12 = readfits(nuvfileList[nuvfileind1], header22, exten_no=2, /silent)
              nuvSJI1 = [[[nuvSJI1]], [[nuvSJI12]]]
            ;nuv1lambda = readfits(nuvfileList[nuvfileind1], header21, exten_no=3, /silent)
            endif ;~keyword_set(logonly)
            
            sim4=nuvfilenumbers[nuvfileind2]
            if ~keyword_set(logonly) then begin
              simfile4=file_basename(nuvfileList[nuvfileind2])
              if ~keyword_set(quiet) then $
                print, 'loading new data file: ', nuvfileList[nuvfileind2]
              nuv2 = readfits(nuvfileList[nuvfileind2], header2, /silent)
              nuvSJI2 = readfits(nuvfileList[nuvfileind2], header22, exten_no=1, /silent)
              nuvSJI22 = readfits(nuvfileList[nuvfileind2], header22, exten_no=2, /silent)
              nuvSJI2 = [[[nuvSJI2]], [[nuvSJI22]]]
            ;nuv2lambda = readfits(nuvfileList[nuvfileind1], header21, exten_no=3, /silent)
            endif ;~keyword_set(logonly)
            
          endif else begin
            ;check if we need to load a new simulation file
            ;FUV
            if localtimefuv ge dtimeSIMfuv then begin
              localtimefuv = localtimefuv-dtimeSIMfuv
              fuvfileind1=fuvfileind2
              fuvfileind2=fuvfileind2+1
              if fuvfileind2 eq N_ELEMENTS(fuvfilenumbers) then begin
                fuvfileind2 = 0
                dtimeSIMfuv = dtimeSIMend
              endif else begin
                dtimeSIMfuv = dtimeSIM * (fuvfilenumbers[fuvfileind2]-fuvfilenumbers[fuvfileind1])
              endelse
              load2=0
              while localtimefuv gt dtimeSIMfuv do begin
                load2=1
                localtimefuv = localtimefuv - dtimeSIMfuv
                fuvfileind1=fuvfileind2
                fuvfileind2=fuvfileind2+1
                if fuvfileind2 eq N_ELEMENTS(fuvfilenumbers) then begin
                  fuvfileind2 = 0
                  dtimeSIMfuv = dtimeSIMend
                endif else begin
                  dtimeSIMfuv = dtimeSIM * (fuvfilenumbers[fuvfileind2]-fuvfilenumbers[fuvfileind1])
                endelse
              endwhile
              
              if load2 then begin ;time step is too big, we need to skip a file (or more)
                sim1=fuvfilenumbers[fuvfileind1]
                if ~keyword_set(logonly) then begin
                  simfile1=file_basename(fuvfileList[fuvfileind1])
                  if ~keyword_set(quiet) then $
                    print, 'loading new data file: ', fuvfileList[fuvfileind1]
                  fxread, fuvfileList[fuvfileind1], fuvSJI1, header
                  fxbopen,unit,fuvfileList[fuvfileind1],1,header
                  fxbread,unit,fuv11,'FUV 1'
                  fxbread,unit,fuv12,'FUV 2'
                  fxbclose,unit
                endif ;~keyword_set(logonly)
              endif else begin
                sim1=sim2
                if ~keyword_set(logonly) then begin
                  simfile1=simfile2
                  fuvSJI1=fuvSJI2
                  fuv11=fuv21
                  fuv12=fuv22
                endif ;~keyword_set(logonly)
              endelse
              sim2=fuvfilenumbers[fuvfileind2]
              if ~keyword_set(logonly) then begin
                simfile2=file_basename(fuvfileList[fuvfileind2])
                if ~keyword_set(quiet) then $
                  print, 'loading new data file: ', fuvfileList[fuvfileind2]
                fxread, fuvfileList[fuvfileind2], fuvSJI2, header
                fxbopen,unit,fuvfileList[fuvfileind2],1,header
                fxbread,unit,fuv21,'FUV 1'
                fxbread,unit,fuv22,'FUV 2'
                fxbclose,unit
              endif ;~keyword_set(logonly)
            endif ;FUV
            
            
            if localtimenuv ge dtimeSIMnuv then begin
              ;NUV
              localtimenuv = localtimenuv-dtimeSIMnuv
              nuvfileind1=nuvfileind2
              nuvfileind2=nuvfileind2+1
              if nuvfileind2 eq N_ELEMENTS(nuvfilenumbers) then begin
                nuvfileind2 = 0
                dtimeSIMnuv = dtimeSIMend
              endif else begin
                dtimeSIMnuv = dtimeSIM * (nuvfilenumbers[nuvfileind2]-nuvfilenumbers[nuvfileind1])
              endelse
              load2=0
              while localtimenuv gt dtimeSIMnuv do begin
                load2=1
                localtimenuv = localtimenuv - dtimeSIMnuv
                nuvfileind1=nuvfileind2
                nuvfileind2=nuvfileind2+1
                if nuvfileind2 eq N_ELEMENTS(nuvfilenumbers) then begin
                  nuvfileind2 = 0
                  dtimeSIMnuv = dtimeSIMend
                endif else begin
                  dtimeSIMnuv = dtimeSIM * (nuvfilenumbers[nuvfileind2]-nuvfilenumbers[nuvfileind1])
                endelse
              endwhile
              
              if load2 then begin ;time step is too big, we need to skip a file (or more)
                sim3=nuvfilenumbers[nuvfileind1]
                if ~keyword_set(logonly) then begin
                  simfile3=file_basename(nuvfileList[nuvfileind1])
                  if ~keyword_set(quiet) then $
                    print, 'loading new data file: ', nuvfileList[nuvfileind1]
                  nuv1 = readfits(nuvfileList[nuvfileind1], header2, /silent)
                  nuvSJI1 = readfits(nuvfileList[nuvfileind1], header22, exten_no=1, /silent)
                  nuvSJI12 = readfits(nuvfileList[nuvfileind1], header22, exten_no=2, /silent)
                  nuvSJI1 = [[[nuvSJI1]], [[nuvSJI12]]]
                ;nuv1lambda = readfits(nuvfileList[nuvfileind1], header21, exten_no=3, /silent)
                endif ;~keyword_set(logonly)
              endif else begin
                sim3=sim4
                if ~keyword_set(logonly) then begin
                  simfile3=simfile4
                  nuvSJI1=nuvSJI2
                  nuv1=nuv2
                ;nuv1lambda=nuv2lambda
                endif ;~keyword_set(logonly)
              endelse
              sim4=nuvfilenumbers[nuvfileind2]
              if ~keyword_set(logonly) then begin
                simfile4=file_basename(nuvfileList[nuvfileind2])
                if ~keyword_set(quiet) then $
                  print, 'loading new data file: ', nuvfileList[nuvfileind2]
                nuv2 = readfits(nuvfileList[nuvfileind2], header2, /silent)
                nuvSJI2 = readfits(nuvfileList[nuvfileind2], header22, exten_no=1, /silent)
                nuvSJI22 = readfits(nuvfileList[nuvfileind2], header22, exten_no=2, /silent)
                nuvSJI2 = [[[nuvSJI2]], [[nuvSJI22]]]
              ;nuv2lambda = readfits(nuvfileList[nuvfileind1], header21, exten_no=3, /silent)
              endif ;~keyword_set(logonly)
            endif
          endelse
          
          
          
          
          instrument->newFDB, nrFDB, time, step, sim1, sim2, sim3, sim4, $
            PZTcurrent, filterwheel, currentFDB, currentCRS, error, $
            Date_OBS, T_OBS
            
            
            
          if ~keyword_set(logonly) then begin
          
          
          
            if step eq 0 then begin
              ; initialize window parameters
              !p.charsize=pcharsize
              !p.charthick=pcharthick
              
              IRISsim_InitGraph, OBSList, FRMList, FDBList, CRSList, $
                FRMinOBS, FDBinFRM, CRSinFDB, $
                round(SJIframe*6), PZTnoChangeValue, PixCCDxfuv1, PixCCDxfuvSJI, $
                SpecData, SJIData, rasterSJI, WINsize, PosClock, totalsteps, xmax=maxWINsize[0], ymax=maxWINsize[1], debug=debug
                
              frozenSlit=intarr(N_ELEMENTS(SJIData))
            endif
            
            
            
            
            
            
            
            
            ;Actual simulation
            
            ;timefactor for interpolation between time steps
            timefacfuv = double(localtimefuv)/dtimeSIMfuv
            timefacnuv = double(localtimenuv)/dtimeSIMnuv
            
            if N_ELEMENTS(SpecData) gt 0 then $
              SpecwinUpdate=intarr(N_ELEMENTS(SpecData))
              
            ;calculate FUV if necessary
            if currentCRS[2] ge 0 then begin
              ;interpolation in time, x- and y direction
              fuv1 = IRISsim_DataPrepareSpec(fuv11, fuv21, PixCCDxfuv1, PixCCDyfuv1, PZTcurrent, timefacfuv, /rightpad)
              fuv2 = IRISsim_DataPrepareSpec(fuv12, fuv22, PixCCDxfuv2, PixCCDyfuv2, PZTcurrent, timefacfuv)
              ;put fuv1 and fuv2 into 1 matrix
              fuv = fltarr((size(fuv1))[1]+(size(fuv2))[1], max([(size(fuv1))[2], (size(fuv2))[2]]))
              fuv[0:(size(fuv1))[1]-1, 0: (size(fuv1))[2]-1] = fuv1
              fuv[(size(fuv1))[1]:(size(fuv1))[1]+(size(fuv2))[1]-1, 0:(size(fuv2))[2]-1] = fuv2
              
              ;check if we need to rebin the data
              sumspec=CRSList[currentCRS[2]].Spectral
              sumspat=CRSList[currentCRS[2]].Spatial
              if (sumspec gt 1) || (sumspat gt 1) then $
                fuv = rebin(fuv, (PixCCDxfuv1+PixCCDxfuv2)/sumspec, max([PixCCDyfuv1, PixCCDyfuv2])/sumspat) * sumspec * sumspat
                
              ;multiply with defined exposure duratioin
              fuv = fuv * FDBList[currentFDB[2]].Exp_Duration/1000d
              
              ; set all data to zero, except for CRS windows
              fuvtemp = fltarr((size(fuv))[1],(size(fuv))[2])
              for i=0,CRSList[currentCRS[2]].SubRegions-1 do begin
                for pda=0,N_ELEMENTS(SpecData)-1 do begin
                  if (SpecData[pda].CRSindex eq currentCRS[2]) && (SpecData[pda].SubRegionID eq (*(CRSList[currentCRS[2]]).SubRegionID)[i]) then begin
                    startR = SpecData[pda].SIMcoords[0]
                    endR = SpecData[pda].SIMcoords[4]
                    startC = SpecData[pda].SIMcoords[2]
                    endC = SpecData[pda].SIMcoords[5]
                    ;adjust for rebinning
                    if sumspec gt 1 then begin
                      startR = startR / sumspec
                      endR = endR / sumspec
                    endif
                    if sumspat gt 1 then begin
                      startC = startC / sumspat
                      endC = endC / sumspat
                    endif
                    fuvtemp[startR:endR,startC:endC] = fuv[startR:endR,startC:endC]
                    
                    ;set data of region to structure SpecData for later plotting
                    *(SpecData[pda]).data = fuv[startR:endR,startC:endC]
                    SpecwinUpdate[pda]=1
                    
                    ;the first time this spectral window is populated, we need to set the minium and maximum value for scaling
                    if (SpecData[pda].min_show eq 0) && (SpecData[pda].max_show eq 0) then begin
                      temp = Histo_Opt(*(SpecData[pda]).data, threshold_Spec)
                      SpecData[pda].max_show = max(temp, min=m)
                      SpecData[pda].min_show = m
                    endif
                    
                    ;we save the PZT offset for each SJI indivually, for the axis of the plot
                    SpecData[pda].PZTx = PZTcurrent[0]
                    SpecData[pda].PZTy = PZTcurrent[1]
                    
                    break
                  endif
                endfor
              endfor
              fuv = fuvtemp
              
            endif ;FUV calculation
            
            
            
            ;calculate NUV if necessary
            if currentCRS[1] ge 0 then begin
              ;interpolation in time, x- and y direction
              nuv = IRISsim_DataPrepareSpec(nuv1, nuv2, PixCCDxnuv, PixCCDynuv, PZTcurrent, timefacnuv, /rightpad, /nuv)
              
              ;check if we need to rebin the data
              sumspec=CRSList[currentCRS[1]].Spectral
              sumspat=CRSList[currentCRS[1]].Spatial
              if (sumspec gt 1) || (sumspat gt 1) then $
                nuv = rebin(nuv, PixCCDxnuv/sumspec, PixCCDynuv/sumspat) * sumspec * sumspat
                
              ;multiply with defined exposure duration
              nuv = nuv * FDBList[currentFDB[1]].Exp_Duration/1000d
              
              ; set all data to zero, except for CRS windows
              nuvtemp = fltarr((size(nuv))[1],(size(nuv))[2])
              for i=0,CRSList[currentCRS[1]].SubRegions-1 do begin
                for pda=0,N_ELEMENTS(SpecData)-1 do begin
                  if (SpecData[pda].CRSindex eq currentCRS[1]) && (SpecData[pda].SubRegionID eq (*(CRSList[currentCRS[1]]).SubRegionID)[i]) then begin
                    startR = SpecData[pda].SIMcoords[0]
                    if startR ge PixCCDxfuv1 then startR = startR - PixCCDxfuv1
                    endR = SpecData[pda].SIMcoords[4]
                    if endR ge PixCCDxfuv1 then endR = endR - PixCCDxfuv1
                    startC = SpecData[pda].SIMcoords[2]
                    endC = SpecData[pda].SIMcoords[5]
                    ;adjust for rebinning
                    if sumspec gt 1 then begin
                      startR = startR / sumspec
                      endR = endR / sumspec
                    endif
                    if sumspat gt 1 then begin
                      startC = startC / sumspat
                      endC = endC / sumspat
                    endif
                    nuvtemp[startR:endR,startC:endC] = nuv[startR:endR,startC:endC]
                    
                    ;set data of region to structure PlotData for later plotting
                    *(SpecData[pda]).data = nuv[startR:endR,startC:endC]
                    SpecwinUpdate[pda]=1
                    
                    ;the first time this spectral window is populated, we need to set the minium and maximum value for scaling
                    if (SpecData[pda].min_show eq 0) && (SpecData[pda].max_show eq 0) then begin
                      temp = Histo_Opt(*(SpecData[pda]).data, threshold_Spec)
                      SpecData[pda].max_show = max(temp, min=m)
                      SpecData[pda].min_show = m
                    endif
                    
                    ;we save the PZT offset for each SJI indivually, for the axis of the plot
                    SpecData[pda].PZTx = PZTcurrent[0]
                    SpecData[pda].PZTy = PZTcurrent[1]
                    
                    break
                  endif
                endfor
              endfor
              nuv = nuvtemp
              
            endif ;NUV calculation
            
            
            
            ;SJI calculation
            SJIwinUpdate=intarr(N_ELEMENTS(SJIData)) ;indicates whether a specific SJI window is updated
            SJIwinUpdate[0]=1 ;Overview SJI window is always updated
            fuvSlit=0
            if currentCRS[0] ge 0 then begin
              for i=1,N_ELEMENTS(SJIData)-1 do begin
                if SJIData[i].CRSindex eq currentCRS[0] then begin
                  SJIwinUpdate[i]=1
                  frozenSlit[i]=PixCCDxfuvSJI/2
                  if SJIData[i].type eq 'fuv' then frozenSlit[i]=frozenSlit[i]+PixCCDxfuvSJI
                  if SJIData[i].type eq 'both' then begin
                    if ((*(FRMList[currentFRM]).FW)[nrFDB] eq 31) || ((*(FRMList[currentFRM]).FW)[nrFDB] eq 91) then begin
                      frozenSlit[i]=frozenSlit[i]+PixCCDxfuvSJI
                      fuvSlit=PixCCDxfuvSJI
                    endif
                  endif
                  if SJIData[i].sum_spatX gt 1 then $
                    frozenSlit[i]=SJIData[i].Boxcoords[0]+1+(frozenSlit[i]-SJIData[i].Boxcoords[0]-1)/SJIData[i].sum_spatX
                endif
              endfor
            endif
            IRISsim_DataPrepareSJI, fuvSJI1, fuvSJI2, nuvSJI1, nuvSJI2, rasterSJI, PixCCDxfuvSJI, PixCCDyfuvSJI, timefacfuv, timefacnuv, PZTcurrent, SJIData, $
              FRMList[currentFRM], filterwheel, nrFDB, FDBList[currentFDB[0]], CRSList[currentCRS[0]], SJI, SJIwinUpdate, $
              threshold_SJI_Overview, threshold_SJI
            currentBox=SJIData[0].Showcoords
            currentBox[0]=currentBox[0]+PZTcurrent[0]*6
            currentBox[4]=currentBox[4]+PZTcurrent[0]*6
            currentBox[2]=currentBox[2]+PZTcurrent[1]*6
            currentBox[5]=currentBox[5]+PZTcurrent[1]*6
            currentSlit=PixCCDxfuvSJI/2+PZTcurrent[0]*6+fuvSlit
            ;currentMiddle=PixCCDyfuvSJI/2+PZTcurrent[1]*6
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            if savefitsfiles then begin
            
              if currentCRS[0] ge 0 then begin
                file=datestamp + fns(OBSList.ID+'_####_SJI_L1.fits',step)
                fxhmake, header, SJI, /initialize, /date
                IRISsim_AddFitsKeywords, header, OBSList, FRMList, FDBList, CRSList, SpecData, $
                  0, currentFRM, currentFDB[0], currentCRS[0], $
                  nrOBSrepeats, nrFRM, nrFRMrepeats, nrFDB, $
                  time, PZTcurrent, filterwheel, Date_OBS, T_OBS[0], $
                  simfile1, simfile2, simfile3, simfile4
                fxwrite, file, header, SJI
              endif
              
              if currentCRS[1] ge 0 then begin
                file=datestamp + fns(OBSList.ID+'_####_NUV_L1.fits',step)
                fxhmake, header, nuv, /initialize, /date
                IRISsim_AddFitsKeywords, header, OBSList, FRMList, FDBList, CRSList, SpecData, $
                  1, currentFRM, currentFDB[1], currentCRS[1], $
                  nrOBSrepeats, nrFRM, nrFRMrepeats, nrFDB, $
                  time, PZTcurrent, filterwheel, Date_OBS, T_OBS[1], $
                  simfile1, simfile2, simfile3, simfile4
                fxwrite, file, header, nuv
              endif
              
              if currentCRS[2] ge 0 then begin
                file=datestamp + fns(OBSList.ID+'_####_FUV_L1.fits',step)
                ; Create a primary header and write it out
                fxhmake, header, fuv, /initialize, /date
                IRISsim_AddFitsKeywords, header, OBSList, FRMList, FDBList, CRSList, SpecData, $
                  2, currentFRM, currentFDB[2], currentCRS[2], $
                  nrOBSrepeats, nrFRM, nrFRMrepeats, nrFDB, $
                  time, PZTcurrent, filterwheel, Date_OBS, T_OBS[2], $
                  simfile1, simfile2, simfile3, simfile4
                fxwrite, file, header, fuv
              endif
              
            endif ;save fits files
            
            
            
            
            
            ;;; graphical output
            if ~keyword_set(noplot) then begin
            
              ;if step eq 0 then begin
              if saveplot then set_plot,'z' else set_plot,'x'
              !p.background=253
              !p.color=0
              !p.ticklen=-.02
              ;!p.charsize=1  ;those 2 are defined above
              ;!p.charthick=1
              if saveplot then device,set_res=WINsize $
              else window, 16, xs=WINsize[0], ys=WINsize[1]
              
              if step eq 0 then begin
                ;load color table, works only with set_plot,'x', or see 3 lines below
                loadct,0
                ;reverse black and white, works like this only when plotting directly on screen, not in file
                ;but like this it works: tvrd(),r,g,b
                tvlct,r,g,b,/get
                ;r=reverse(r)
                ;g=reverse(g)
                ;b=reverse(b)
                ;active color
                r[255]=color_active[0]
                g[255]=color_active[1]
                b[255]=color_active[2]
                ;non-active color
                r[254]=color_inactive[0]
                g[254]=color_inactive[1]
                b[254]=color_inactive[2]
                tvlct,r,g,b
                
                bold=1
                titlesize=1.0*!p.charsize
                titleposy=titlesize*!d.y_ch_size/WINsize[1]
                datasize=0.9d*!p.CHARSIZE
                dataposy=datasize*!d.y_ch_size/WINsize[1]
                leftpos=0.1*!d.x_ch_size/WINsize[0]
              endif
              
              
              
              
              ;plot SJI
              shiftxaxis = (-SJIData[0].Boxcoords[1])/2 - SJIData[0].Boxcoords[0]
              shiftyaxis = (-SJIData[0].Boxcoords[3])/2 - SJIData[0].Boxcoords[2]
              for i=0,N_ELEMENTS(SJIData)-1 do begin
                if SJIwinUpdate[i] then colortext=255 else colortext=254
                if SJIData[i].min_show eq SJIData[i].max_show then maxshow=SJIData[i].max_show+1e-10 else maxshow=SJIData[i].max_show
                
                if i eq 0 then begin
                  ;Overview SJI
                  pih, *(SJIData[i]).data, threshold_SJI_Overview, position=SJIData[i].WINcoords, $
                    origin=[SJIData[i].Boxcoords[0]+1, SJIData[i].Boxcoords[2]+1], $
                    min=SJIData[i].min_show, max=maxshow, $
                    xstyle=5, ystyle=5, top=253
                  ;axis, xaxis=1, xrange=[SJIData[i].Boxcoords[0]+1, SJIData[i].Boxcoords[4]+1], xtit='Solar X [Px]', xstyle=1
                  axis, xaxis=0, xrange=[(-SJIData[i].Boxcoords[1])/2/6.0, (SJIData[i].Boxcoords[1])/2/6.0], xtit='Solar X [arcsec]', xstyle=1
                  ;axis, yaxis=1, yrange=[SJIData[i].Boxcoords[2]+1, SJIData[i].Boxcoords[5]+1], ytit='Solar Y [Px]', ystyle=1
                  axis, yaxis=0, yrange=[(-SJIData[i].Boxcoords[3])/2/6.0, (SJIData[i].Boxcoords[3])/2/6.0], ytit='Solar Y [arcsec]', ystyle=1
                  if N_ELEMENTS(SJIData) gt 1 then begin
                    oplot, [currentBox[0], currentBox[0]], [currentBox[2], currentBox[5]], thick=2, col=128
                    oplot, [currentBox[4], currentBox[4]], [currentBox[2], currentBox[5]], thick=2, col=128
                    oplot, [currentBox[0], currentBox[4]], [currentBox[2], currentBox[2]], thick=2, col=128
                    oplot, [currentBox[0], currentBox[4]], [currentBox[5], currentBox[5]], thick=2, col=128
                  endif
                  oplot, [currentSlit, currentSlit], [SJIData[i].Showcoords[2]+PZTcurrent[1]*6, SJIData[i].Showcoords[5]+PZTcurrent[1]*6], thick=2, col=128
                endif else begin
                  pih,*(SJIData[i]).data, threshold_SJI, position=SJIData[i].WINcoords, $
                    origin=[SJIData[i].Boxcoords[0]+1, SJIData[i].Boxcoords[2]+1], $
                    min=SJIData[i].min_show, max=maxshow, $
                    xstyle=5, ystyle=5, /noerase, top=253
                  ;print, SJIData[i].min_show, maxshow, max(*(SJIData[i]).data,min=a),a
                    
                  ;upper x-axis
                  ;if SJIData[i].type eq 'fuv' then shiftpx=PixCCDxnuvSJI else shiftPx=0
                  axis, xaxis=1, xrange=[SJIData[i].Boxcoords[0]+1, SJIData[i].Boxcoords[4]+1], $
                    xtit='Solar X [Px]', xstyle=1, xticks=SJIData[i].nrticksx, xtickformat='(i4)'
                  ;lower x-axis
                  if SJIData[i].Boxcoords[0] ge PixCCDxfuvSJI then shiftxarc=PixCCDxfuvSJI else shiftxarc=0
                  axis, xaxis=0, xrange=[((SJIData[i].Boxcoords[0]+shiftxaxis-shiftxarc))/6.0+SJIData[i].PZTx, $
                    ((SJIData[i].Boxcoords[4]+shiftxaxis-shiftxarc))/6.0+SJIData[i].PZTx], $
                    xtit='Solar X [arcsec]', xstyle=1, xticks=SJIData[i].nrticksx, xtickformat='(i4)'
                  ;right y-axis
                  if SJIData[i].axisR then $
                    axis, yaxis=1, yrange=[SJIData[i].Boxcoords[2]+1, SJIData[i].Boxcoords[5]+1], $
                    ytit='Solar Y [Px]', ystyle=1, yticks=SJIData[i].nrticksy, ytickformat='(i4)'
                  ;left y-axis
                  if SJIData[i].axisL then $
                    axis, yaxis=0, yrange=[(SJIData[i].Boxcoords[2]+shiftyaxis)/6.0+SJIData[i].PZTy, $
                    (SJIData[i].Boxcoords[5]+shiftyaxis)/6.0+SJIData[i].PZTy], $
                    ytit='Solar Y [arcsec]', ystyle=1, yticks=SJIData[i].nrticksy, ytickformat='(i4)'
                endelse
                
                
                ;draw slit if necessary
                if frozenSlit[i] gt 0 then begin
                  oplot, [frozenSlit[i], frozenSlit[i]], [SJIData[i].BOXcoords[2], SJIData[i].BOXcoords[5]], thick=2, col=128
                endif
                
                ;write title and some values
                xyouts, SJIData[i].WINcoords[0]+leftpos, SJIData[i].WINcoords[3]-1.2*titleposy, SJIData[i].name, color=colortext, charsize=titlesize, charthick=bold, /normal
                xyouts, SJIData[i].WINcoords[0]+leftpos, SJIData[i].WINcoords[3]-2.4*titleposy, SJIData[i].name2, color=colortext, charsize=titlesize, charthick=bold, /normal
                if i eq 0 then begin
                  text1='X '+string(PZTcurrent[0],format='(f6.2)')
                  text2='Y '+string(PZTcurrent[1],format='(f6.2)')
                endif else begin
                  maxval=string(max(*(SJIData[i]).data),format='(e7.1)')
                  meanval=string(mean(*(SJIData[i]).data),format='(e7.1)')
                  text1='A '+meanval
                  text2='M '+maxval
                endelse
                xyouts, SJIData[i].WINcoords[0]+leftpos, SJIData[i].WINcoords[1]+1.5*dataposy, text1, color=colortext, charsize=datasize, charthick=bold, /normal
                xyouts, SJIData[i].WINcoords[0]+leftpos, SJIData[i].WINcoords[1]+0.3*dataposy, text2, color=colortext, charsize=datasize, charthick=bold, /normal
              endfor
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              ;plot Spectra
              for i=0,N_ELEMENTS(SpecData)-1 do begin
                if SpecwinUpdate[i] then colortext=255 else colortext=254
                if SpecData[i].min_show eq SpecData[i].max_show then maxshow=SpecData[i].max_show+1e-10 else maxshow=SpecData[i].max_show
                maxval=string(max(*(SpecData[i]).data),format='(e7.1)')
                meanval=string(mean(*(SpecData[i]).data),format='(e7.1)')
                case SpecData[i].type of
                  'fu1': begin
                    displambda = constants->get_fuv1displambda()
                    startlambda = constants->get_fuv1startlambda()
                  end
                  'fu2': begin
                    displambda = constants->get_fuv2displambda()
                    startlambda = constants->get_fuv2startlambda() - displambda * PixCCDxfuv1
                  end
                  'nuv': begin
                    displambda = constants->get_nuvdisplambda()
                    startlambda = constants->get_nuvstartlambda() - displambda * PixCCDxfuv1
                  end
                endcase
                pih,*(SpecData[i]).data*(-1d), threshold_Spec, position=SpecData[i].WINcoords, $
                  max=SpecData[i].min_show*(-1d), min=maxshow*(-1d), $
                  xstyle=5, ystyle=5, $
                  /noerase, top=253;, noscale=1
                ;upper x-axis
                axis, xaxis=1, xrange=[SpecData[i].SIMcoords[0]+1, SpecData[i].SIMcoords[4]+1], $
                  xtit='!4k!3'+' [Px]', xstyle=1, xticks=SpecData[i].nrticksx, xtickformat='(i4)'
                ;lower x-axis
                axis, xaxis=0, xrange=[startlambda+displambda*SpecData[i].SIMcoords[0], startlambda+displambda*SpecData[i].SIMcoords[4]], $
                  xtit='!4k!3'+' [!3' + String("305B) + '!X]', xstyle=1, xticks=SpecData[i].nrticksx, xtickformat='(i4)'
                ;right y-axis
                if SpecData[i].axisR then $
                  axis, yaxis=1, yrange=[SpecData[i].SIMcoords[2]+1, SpecData[i].SIMcoords[5]+1], $
                  ytit='Solar Y [Px]', ystyle=1, yticks=SpecData[i].nrticksy, ytickformat='(i4)'
                ;left y-axis
                if SpecData[i].axisL then $
                  axis, yaxis=0, yrange=[(SpecData[i].SIMcoords[2]+shiftyaxis)/6.0+SpecData[i].PZTy, $
                  (SpecData[i].SIMcoords[5]+shiftyaxis)/6.0+SpecData[i].PZTy], $
                  ytit='Solar Y [arcsec]', ystyle=1, yticks=SpecData[i].nrticksy, ytickformat='(i4)'
                  
                xyouts, SpecData[i].WINcoords[0]+leftpos, SpecData[i].WINcoords[3]-1.2*titleposy, SpecData[i].name, color=colortext, charsize=titlesize, charthick=bold, /normal
                xyouts, SpecData[i].WINcoords[0]+leftpos, SpecData[i].WINcoords[3]-2.4*titleposy, SpecData[i].name2, color=colortext, charsize=titlesize, charthick=bold, /normal
                xyouts, SpecData[i].WINcoords[0]+leftpos, SpecData[i].WINcoords[1]+1.5*dataposy, 'A '+meanval, color=colortext, charsize=datasize, charthick=bold, /normal
                xyouts, SpecData[i].WINcoords[0]+leftpos, SpecData[i].WINcoords[1]+0.3*dataposy, 'M '+maxval, color=colortext, charsize=datasize, charthick=bold, /normal
              endfor
              
              
              ;display time, digitally and analogously, digitally, now in title of SJI
              if debug then stringtime = IRISsim_strtime(time+debugtime) else stringtime = IRISsim_strtime(time)
              if debug then stringtimesec = IRISsim_strtime(time+debugtime,/seconds) else stringtimesec = IRISsim_strtime(time,/seconds)
              xyouts, 0.99, 1-1.7*titleposy, 'Time: '+stringtime, color=0, /normal, charsize=titlesize, charthick=bold, alignment=1, width=w1
              xyouts, 0.98-w1, 1-1.7*titleposy, 'Time [s]: '+stringtimesec, color=0, /normal, charsize=titlesize, charthick=bold, alignment=1, width=w2
              xyouts, 0.97-w1-w2, 1-1.7*titleposy, 'Index: '+string(step,format='(I6.4)'), color=0, /normal, charsize=titlesize, charthick=bold, alignment=1
              clock, stringtime, pos=PosClock[0:1], size=PosClock[2:3], col=255
              
              
              
              
              ; save image
              if saveplot then begin
                file=datestamp + fns(OBSList.ID+'_pic_####.png',step)
                write_png,file,tvrd(),r,g,b
              endif
              
              
            endif ;~keyword_set(noplot)
            
            
            if ~keyword_set(quiet) then $
              print, 'step: ', strcompress(string(step), /remove_all), $
              '  time: ', strcompress(string(time), /remove_all), $
              ;'  localtimefuv: ', strcompress(string(localtimefuv), /remove_all), $
              ;'  localtimenuv: ', strcompress(string(localtimenuv), /remove_all), $
              '  PZT: ', strcompress(string(PZTcurrent[0]), /remove_all), $
              ' / ', strcompress(string(PZTcurrent[1]), /remove_all)
              
              
          endif ;~keyword_set(logonly)
          
          step = step + 1
          
          if ~keyword_set(logonly) then begin
            void=widget_event(/nowait);see if the abort-button was pressed
            
            set_plot,'x'
            widget_control,progressbar,set_value = double(step) / totalsteps
            ;if saveplot then set_plot,'z' else set_plot,'x'
            
            widget_control, abortbutton, get_uvalue=abort
            if abort then print, 'Aborting simulation...'
          endif else abort=0
          
          
          if (debug && (step eq nrpics)) || abort then break
        endfor ;nrFDB=0,FRMList[useFRM].NumEntries-1
        
        if (debug && (step eq nrpics)) || abort then break
      endfor ;nrFRMrepeats=0,(*(OBSList).Repeat_FRM)[nrFRM]-1
      
      if (debug && (step eq nrpics)) || abort then break
    endfor ;nrFRM=0,OBSList.NumEntries-1
    
    if (debug && (step eq nrpics)) || abort then break
  endfor ;nrOBSrepeats=0,OBSList.Repeat_Obs-1
  
  instrument->endsim, logdata
  
  set_plot,'x'
  !P.Multi=0
  
  if ~keyword_set(logonly) && ~keyword_set(noplot) then begin
    if makemovie then begin
      file=fns(OBSList.ID+'_pic_####.png',0)
      cd,datestamp, current=old_dir
      framerate='--sequencerate=30'
      print,'/usr/local/bin/qt_export --replacefile --video=png,30,100 '+framerate+' '+file+' '+OBSList.ID+'_movie.mov'
      spawn,'/usr/local/bin/qt_export --replacefile --video=png,30,100 '+framerate+' '+file+' '+OBSList.ID+'_movie.mov'
      if playmovie then begin
        print,'opening movie...'
        print, quicktimeloc + ' *.mov &'
        spawn, quicktimeloc + ' *.mov &'
      endif
      cd,old_dir
    endif
  endif ;~keyword_set(logonly)
  
  if ~keyword_set(logonly) then $
    widget_control,progressbase, /destroy
    
    
  obj_destroy, constants
  obj_destroy, instrument
  
  
  if keyword_set(logonly) then totalsteps=step
  
  
  ;if ~keyword_set(logonly) && ~keywords_set(quiet) then $
  print, 'done with OBS '+OBSList.ID
  
  !except=oldexcept
end
