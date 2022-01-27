  ; $Id: iris_simstarter.pro,v 1.7 2013/09/16 13:41:28 mawiesma Exp $
  
  
  files = ['~/iris/input/XML/OBS-T-00001.xml', $
    '~/iris/input/XML/OBS-T-00002.xml', $
    '~/iris/input/XML/OBS-T-00003.xml', $
    '~/iris/input/XML/OBS-T-00004.xml', $
    '~/iris/input/XML/OBS-T-00005.xml']
    
    
    
  ;;;;;; or ;;;;;;;
    
    
    
  folder = '~/iris/input/XML/'
  files = find_files('OBS*.xml',folder)
  ;files = files[0:1]
  
  OBSdir = '' ;the directory where the xml-files reside, if this information is not included in the files-variable, optional
  FRMdir = '' ;the directory where the FRM-files reside, optional
  FDBdir = '' ;the directory where the FDB-files reside, optional
  CRSdir = '' ;the directory where the CRS-files reside, optional
  
  
  ;uncomment the next line, if you want to see the content of a specific OBS
  ;IRISsim_showXML, OBSfilein=files[0], FRMdir=FRMdir, FDBdir=FDBdir, CRSdir=CRSdir
  ;stop
  
  
  
  OBSreps = make_array(N_ELEMENTS(files), value=0) ;this number overwrites the number of repetitions defined in the OBS (if zero, the original value will be used), one number per file
  
  
  ; output folder destination
  outputfolder = '~/iris/output/' ;directory in which the results should be saved, a subfolder with date and time will be created there
  
  ; log file
  quiet = 0          ;set to 1 if you don't want any output on the console
  copyxml = 1        ;set to 1 if you want to copy the xml-files to the destination folder
  savelogcsv = 0     ;set to 1 if you want to save a logfile in csv-format (excel-readable)
  savelogtxt = 1     ;set to 1 if you want to save a logfile in txt-format (human-readable)
  savelogfull = 0    ;if set, a logfile will be created in csv-format, with information for the level 1-to-2 procedure (not maintained)
  logfulldir = ''    ;the logl2-file will be saved into this given directory (must already exist), if not given (or empty), it will be written into the same directory as the other output of the simulator
  logonly = 0        ;set to 1 if you only want the logfiles, without all the other output (savelogcsv/txt still have to be 1 for a log to be produced)
  nopreload = 1      ;set to 1 if you don't want to preload all xml files (speeds things up, if only a few OBS are run)
  getlogstruct = 0   ;set to 1 if you want an array of log structures returned (maximum of 3000 images per OBS), alternativ to writing log to files, set all savelogxxx to 0 for speed
                     ;structure is returned in the keyword logall
                     ;the keyword logdata returns the log of the last OBS
                     
  ; if logonly is set, you can set which directory the logs should be saved into
  logdir = 'logs#####/'        ;the directory in which to save the logs (as an extension to outputfolder!), if logonly is set
  ; use #-character in the folder name, if you want to set a maximum number of files for one folder
  ; the # will then be translated into numbers, using the function fns.pro
  maxfilesperfolder = 1000  ;defines the maximum numbers of OBS output per folder (output will be logs and xml-files if appropriate keywords are set)
  
  
  ; if logonly is set, then the parameters below this line can be ignored
  
  ; output
  savefitsfiles = 1  ;set to 1 if you want to save the fits files
  saveplot = 1       ;set to 1 if you want to save the images, otherwise they will be plotted on screen
  noplot = 0         ;set to 1 if you don't want any graphical output (saveplot will be ignored)
  makemovie = 0      ;set to 1 if you want to create the movie out of the pictures (saveplot should be 1), qt-tools required (http://omino.com/sw/qt_tools/), MAC only
  playmovie = 0      ;set to 1 if you want to play the movie immediately after creating (ignored if makemovie is not set), MAC only
  quicktimeloc = '/Applications/QuickTime\ Player.app/Contents/MacOS/QuickTime\ Player' ;location of your quicktime application, required for playmovie, MAC only
  
  ; graphics parameters
  rasterSJI = 'nu2'  ;('fu1', 'fu2', 'nu1', 'nu2') choose what SJI data is shown in the overview SJI graph (which will be updated on every step), NUV2 not available yet
  SJIframe = 2d      ;visible data around overview SJI (in arcsec), 'Frame'
  maxWINsize = [1400, 1000]       ;max size of plot window [x, y]
  threshold_Overview_SJI = 0.0002 ;threshold for display used in histo_opt for overview SJI
  threshold_SJI = 0.002           ;threshold for display used in histo_opt for SJI(s)
  threshold_Spec = 0.02           ;threshold for display used in histo_opt for spectral images
  
  ; input
  inputdirFUV = '~/iris/input/SIMData_fuv/' ;directory of input FUV files (e.g. 'Data_cb24bih/')
  inputdirNUV = '~/iris/input/SIMData_nuv/' ;directory of input NUV files (e.g. 'Data_cb24bih/')
  inputfileFUV = 'iris_synthetic_fuv_*.fits' ;format of input FUV files (as used in wildcards or strmatch, where * is placed where the file number is, e.g. 'iris_synthetic_fuv_*.fits')
  inputfileNUV = 'iris_synthetic_NUVlines_cb24bih_s*.fits' ;format of input FUV files (as used in wildcards or strmatch, where * is placed where the file number is, e.g. 'iris_synthetic_nuv_cb24bih_*.fits')
  dtimeSIM = 10000L;time steps between simulation files (in ms)
  dtimend = 30000L ;time step between last and first file (in case OBS is longer than the time series from the simulation)
  
  ; debugging
  debug = 0          ;set to positive non-zero number to control directly how many pictures will be calculated
  debtime = 0        ;this time will be added to the real time in the plot output, when in debug-mode (in ms)
  timepOBS = 0      ;set to non-zero, if you want to get the times used for each OBS (for debugging purposes)
  ;these times are returned on timepOBS in the following format: [*,0]=number steps [*,1]=time in seconds
  
  
  IRISsim_multiOBS, files=files, $
    OBSdir=OBSdir, $
    OBSreps=OBSreps, $
    FRMdir=FRMdir, $
    FDBdir=FDBdir, $
    CRSdir=CRSdir, $
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
    logdir=logdir, $
    maxfilesperfolder=maxfilesperfolder, $
    logdata=logdata, $
    getlogstruct=getlogstruct, $
    logall=logall, $
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
    timepOBS=timepOBS,$
    quiet=quiet, $
    nopreload=nopreload
    
    
  if keyword_set(timepOBS) gt 0 then plot, timepOBS[*,1], timepOBS[*,0], psym=4
  if keyword_set(timepOBS) gt 0 then print, 'total time ', total(timepOBS[*,0])
  
  print, 'done with all OBS'
  
