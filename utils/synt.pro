; $Id: synt.pro,v 1.7 2013/05/05 14:53:45 mawiesma Exp $  ;

file = '/Users/mawiesma/my_idl/syntoolDATA/XML_130109/OBS-T-00004.xml'

FRMdir = ''
FDBdir = ''
CRSdir = ''

;IRIS_simulator, file

IRISsim_showXML, OBSfilein=file, FRMdir=FRMdir, FDBdir=FDBdir, CRSdir=CRSdir
stop

IRISsim_syntool, OBSfile=file, $ ;The OBS filename (e.g. 'OBS-T-00004.xml')
    OBSdir='', $        ;the directory where the xml-files reside, if this information is not included in the files-variable
    OBSrep=0, $         ;this number overwrites the number of repetitions defined in the OBS (if zero, the original value will be used)
    FRMdir=FRMdir, $        ;the directory where the FRM-files reside, optional
    FDBdir=FDBdir, $        ;the directory where the FDB-files reside, optional
    CRSdir=CRSdir, $        ;the directory where the CRS-files reside, optional
    outputfolder='/Users/mawiesma/my_idl/syntoolDATA/SIMresults/', $ ;directory in which the results should be saved, a subfolder with date and time will be created there
    rasterSJI='nu2', $  ;('fu1', 'fu2', 'nu1', 'nu2') choose what SJI data is shown in the overview SJI graph (which will be updated on every step), NUV2 not available yet
    savefitsfiles=1, $  ;set to 1 if you want to save the fits files
    saveplot=1, $       ;set to 1 if you want to save the images, otherwise they will be plotted on screen
    noplot=0, $         ;set to 1 if you don't want any graphical output
    makemovie=1, $      ;set to 1 if you want to create the movie out of the pictures (saveplot should be 1), qt-tools required (http://omino.com/sw/qt_tools/), MAC only
    playmovie=1, $      ;set to 1 if you want to play the movie immediately after creating, MAC only
    savelogcsv=1, $     ;set to 1 if you want to save a logfile in csv-format (excel-readable)
    savelogtxt=1, $     ;set to 1 if you want to save a logfile in txt-format (human-readable)
    copyxml=1, $        ;set to 1 if you want to copy the xml-files to the destination folder
    logonly=0, $        ;set to 1 if you only want the logfiles, without all the other output (savelogcsv/txt still have to be 1)
    logdir='logs/', $   ;the directory in which to save the logs (as an extension to outputfolder!), if logonly is set
    logdata=log, $      ;this variable returns the full log as a structure
    SJIframe=2d, $      ;visible data around overview SJI (in arcsec), 'Frame'
    maxWINsize=[1400, 1000], $       ;max size of plot window [x, y]
    quicktimeloc='/Applications/QuickTime\ Player.app/Contents/MacOS/QuickTime\ Player', $ ;location of your quicktime application, required for playmovie, MAC only
    threshold_Overview_SJI=0.0002, $ ;threshold for display used in histo_opt for overview SJI
    threshold_SJI=0.002, $           ;threshold for display used in histo_opt for SJI(s)
    threshold_Spec=0.02, $           ;threshold for display used in histo_opt for spectral images
    inputdirFUV = '/Users/mawiesma/my_idl/syntoolDATA/Data_cb24bih/', $ ;directory of input FUV files (e.g. 'Data_cb24bih/')
    inputdirNUV = '/Users/mawiesma/my_idl/syntoolDATA/Data_cb24bih/', $ ;directory of input NUV files (e.g. 'Data_cb24bih/')
    inputfileFUV = 'iris_synthetic_fuv_*.fits', $ ;format of input FUV files (as used in wildcards or strmatch, where * is placed where the file number is, e.g. 'iris_synthetic_fuv_*.fits')
    inputfileNUV = 'iris_synthetic_nuv_cb24bih_*.fits', $ ;format of input FUV files (as used in wildcards or strmatch, where * is placed where the file number is, e.g. 'iris_synthetic_nuv_cb24bih_*.fits')
    dtimeSIM = 10000L, $;time steps between simulation files (in ms)
    dtimend = 30000L, $ ;time step between last and first file (in case OBS is longer than the time series from the simulation)
    savelogfull=0, $    ;if set, a logfile will be created in csv-format, with information for the level 1-to-2 procedure
    logfulldir=''       ;the logl2-file will be saved into this given directory (must already exist), if not given, it will be written into the same directory as the other output of the simulator
    quiet=0, $          ;set to 1 if you don't want any output on commandline
    totalsteps=steps, $ ;returns the total numbers of steps
    debug=0, $          ;set to positive non-zero number to control directly how many pictures will be calculated
    debtime=0           ;this time will be added to the real time in the plot output, when in debug-mode (in ms)
       