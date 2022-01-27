; $Id: simrep.pro,v 1.6 2013/04/26 03:28:17 mawiesma Exp $  ;

  files = ['/Users/mawiesma/my_idl/syntoolDATA/XML_bart_121025/OBS/OBS-T-00901_Q.xml', $
    '/Users/mawiesma/my_idl/syntoolDATA/XML_bart_121025/OBS/OBS-T-00902_Q.xml', $
    '/Users/mawiesma/my_idl/syntoolDATA/XML_bart_121025/OBS/OBS-T-00903_Q.xml', $
    '/Users/mawiesma/my_idl/syntoolDATA/XML_bart_121025/OBS/OBS-T-00904_Q.xml', $
    '/Users/mawiesma/my_idl/syntoolDATA/XML_bart_121025/OBS/OBS-T-00905_Q.xml']
    
  ;;;;;; or ;;;;;;;
    
  folder = '/Users/mawiesma/my_idl/syntoolDATA/XMLankur_new/'
  files = find_files('OBS*.xml',folder)

  OBSdir = '' ;the directory where the xml-files reside, if this information is not included in the files-variable
  FRMdir = '' ;the directory where the FRM-files reside, optional
  FDBdir = '' ;the directory where the FDB-files reside, optional
  CRSdir = '' ;the directory where the CRS-files reside, optional
    
  OBSreps = intarr(N_ELEMENTS(files)) ;this number overwrites the number of repetitions defined in the OBS (if zero, the original value will be used), one number per file
    
  ; output folder destination
  outputfolder = '/Users/mawiesma/my_idl/syntoolDATA/SIMresults/' ;directory in which the results should be saved, a subfolder with date and time will be created there
  
  ; log file
  copyxml = 0        ;set to 1 if you want to copy the xml-files to the destination folder
  savelogcsv = 0     ;set to 1 if you want to save a logfile in csv-format (excel-readable)
  savelogtxt = 1     ;set to 1 if you want to save a logfile in txt-format (human-readable)
  savelogfull = 0    ;if set, a logfile will be created in csv-format, with information for the level 1-to-2 procedure
  logfulldir = ''    ;the logl2-file will be saved into this given directory (must already exist), if not given (or empty), it will be written into the same directory as the other output of the simulator
  logonly = 1        ;set to 1 if you only want the logfiles, without all the other output (savelogcsv/txt still have to be 1)
  
  ; if logonly is set, you can set which directory the logs should be saved into
  logdir = 'logs#####/'        ;the directory in which to save the logs (as an extension to outputfolder!), if logonly is set
  ; use #-character in the folder name, if you want to set a maximum number of files for one folder
  ; the # will then be translated into numbers, using the function fns.pro
  maxfilesperfolder = 100  ;defines the maximum numbers of files per folder
  
  timepOBS = 0      ;set to non-zero, if you want to get the times used for each OBS
        ;these times are returned on timepOBS in the following format: [*,0]=time in seconds; [*,1]=total number of steps 
  
  
  ; if logonly is set, then the parameters below this line can be ignored
  
  ; output
  quiet = 0          ;set to 1 to suppress most of the output to the command line
  savefitsfiles = 1  ;set to 1 if you want to save the fits files
  saveplot = 1       ;set to 1 if you want to save the images, otherwise they will be plotted on screen
  noplot = 0         ;set to 1 if you don't want any graphical output
  makemovie = 1      ;set to 1 if you want to create the movie out of the pictures (saveplot should be 1), qt-tools required (http://omino.com/sw/qt_tools/), MAC only
  playmovie = 0      ;set to 1 if you want to play the movie immediately after creating, MAC only
  quicktimeloc = '/Applications/QuickTime\ Player.app/Contents/MacOS/QuickTime\ Player' ;location of your quicktime application, required for playmovie, MAC only
  
  ; graphics parameters
  rasterSJI = 'nu2'  ;('fu1', 'fu2', 'nu1', 'nu2') choose what SJI data is shown in the overview SJI graph (which will be updated on every step), NUV2 not available yet
  SJIframe = 2d      ;visible data around overview SJI (in arcsec), 'Frame'
  maxWINsize = [1400, 1000]       ;max size of plot window [x, y]
  threshold_Overview_SJI = 0.0002 ;threshold for display used in histo_opt for overview SJI
  threshold_SJI = 0.002           ;threshold for display used in histo_opt for SJI(s)
  threshold_Spec = 0.02           ;threshold for display used in histo_opt for spectral images
  
  ; input
  inputdirFUV = '/Users/mawiesma/my_idl/syntoolDATA/Data_cb24bih/' ;directory of input FUV files (e.g. 'Data_cb24bih/')
  inputdirNUV = '/Users/mawiesma/my_idl/syntoolDATA/Data_cb24bih/' ;directory of input NUV files (e.g. 'Data_cb24bih/')
  inputfileFUV = 'iris_synthetic_fuv_*.fits' ;format of input FUV files (as used in wildcards or strmatch, where * is placed where the file number is, e.g. 'iris_synthetic_fuv_*.fits')
  inputfileNUV = 'iris_synthetic_nuv_cb24bih_*.fits' ;format of input FUV files (as used in wildcards or strmatch, where * is placed where the file number is, e.g. 'iris_synthetic_nuv_cb24bih_*.fits')
  dtimeSIM = 10000L;time steps between simulation files (in ms)
  dtimend = 30000L ;time step between last and first file (in case OBS is longer than the time series from the simulation)
  
  ; debugging
  debug = 0          ;set to positive non-zero number to control directly how many pictures will be calculated
  debtime = 0        ;this time will be added to the real time in the plot output, when in debug-mode (in ms)
  
  
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
    logdata=log, $
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
    timepOBS=timepOBS, $
    quiet=quiet, $
    debug=debug, $
    debtime=debtime
    