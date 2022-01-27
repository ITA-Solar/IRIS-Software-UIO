pro IRISsim_Starter_event, event
  widget_control, event.top, get_UValue=info, /No_Copy
  uname = widget_info(event.id,/uname)
  liveon=1
  
  ;print, 'event', event.id, '  ', tag_names(event, /structure_name), '  ', uname
  
  case tag_names(event, /structure_name) of
  
    'WIDGET_BUTTON': begin
      cd, current=cur_dir
      case uname of
        'loadparam': IRISsim_paramload, 0, info
        'loaddefault': IRISsim_paramload, 1, info
        'saveparam': IRISsim_paramsave, 0, info
        'savedefault': IRISsim_paramsave, 1, info
        
        'OBSfile': begin
          file=dialog_pickfile(/read, filter=['OBS*.xml'], path=cur_dir, title='Please select an OBS file')
          if file ne '' then widget_control, info.textOBSfile, set_value=file
        end
        
        'FUVdir': begin
          dir=dialog_pickfile(/directory, path=cur_dir, title='Please select a directory, which contains the FUV data')
          if dir ne '' then widget_control, info.textfuvdir, set_value=dir
        end
        
        'NUVdir': begin
          dir=dialog_pickfile(/directory, path=cur_dir, title='Please select a directory, which contains the NUV data')
          if dir ne '' then widget_control, info.textnuvdir, set_value=dir
        end
        
        'outfolder': begin
          dir=dialog_pickfile(/directory, path=cur_dir, title='Please select a directory, where the data should be saved to')
          if dir ne '' then widget_control, info.textdest, set_value=dir
        end
        
        'qtloc': begin
          dir=dialog_pickfile(path='/Applications', title='Please locate your QuickTime installation')
          if dir ne '' then widget_control, info.textqtloc, set_value=dir
        end
        
        'startsim': begin
          widget_control, info.textOBSfile, get_value=OBSfile
          widget_control, info.textfuvdir, get_value=inputdirFUV
          widget_control, info.textfuvfile, get_value=inputfileFUV
          widget_control, info.textnuvdir, get_value=inputdirNUV
          widget_control, info.textnuvfile, get_value=inputfileNUV
          widget_control, info.textdtime, get_value=dtimeSIM
          widget_control, info.textdtimend, get_value=dtimend
          widget_control, info.bgoutput1, get_value=output1
          widget_control, info.bgoutput2, get_value=output2
          widget_control, info.textx, get_value=maxWINsizeX
          widget_control, info.texty, get_value=maxWINsizeY
          widget_control, info.textdest, get_value=outputfolder
          widget_control, info.textqtloc, get_value=quicktimeloc
          widget_control, info.textovsji, get_value=SJIframe
          widget_control, info.textovtype, get_value=rasterSJI
          widget_control, info.textthresov, get_value=threshold_Overview_SJI
          widget_control, info.textthressji, get_value=threshold_SJI
          widget_control, info.textthresspec, get_value=threshold_Spec
;          widget_control, info.bgdebug, get_value=debug
;          widget_control, info.textdebug, get_value=debugnr
;          widget_control, info.textdebugt, get_value=debtime
          widget_control, info.textobsrep, get_value=OBSrep
          
          widget_control, event.top, /destroy
          liveon=0
          
          OBSrep = long(OBSrep)
          dtimeSIM = long(dtimeSIM)*1000
          dtimend = long(dtimend)*1000
          maxWINsize=[long(maxWINsizeX[0]), long(maxWINsizeY[0])]
          SJIframe = double(SJIframe)
          saveplot = output1[0]
          makemovie = output1[1]
          playmovie = output1[2]
          savefitsfiles = output2[0]
          copyxml = output2[1]
          savelogcsv = output2[2]
          savelogtxt = output2[3]
          
;          threshold_Overview_SJI=double(threshold_Overview_SJI)
;          threshold_SJI=double(threshold_SJI)
;          threshold_Spec=double(threshold_Spec)
          
;          debug = long(debug)
;          if debug then debug=long(debugnr)
;          debtime=long(debtime)
          
          IRISsim_syntool, OBSfile=OBSfile[0], $
            OBSrep=OBSrep[0], $
            inputdirFUV=inputdirFUV[0], $
            inputdirNUV=inputdirNUV[0], $
            inputfileFUV=inputfileFUV[0], $
            inputfileNUV=inputfileNUV[0], $
            dtimeSIM=dtimeSIM[0], $
            dtimend=dtimend[0], $
            SJIframe=SJIframe[0], $
            rasterSJI=rasterSJI[0], $
            maxWINsize=maxWINsize, $
            outputfolder=outputfolder[0], $
            savefitsfiles=savefitsfiles[0], $
            saveplot=saveplot[0], $
            savelogcsv=savelogcsv[0], $
            savelogtxt=savelogtxt[0], $
            copyxml=copyxml[0], $
            makemovie=makemovie[0], $
            playmovie=playmovie[0], $
            quicktimeloc=quicktimeloc[0], $
            threshold_Overview_SJI=double(threshold_Overview_SJI[0]), $
            threshold_SJI=double(threshold_SJI[0]), $
            threshold_Spec=double(threshold_Spec[0])
        end
        
        else: ;some other button
      endcase
    end
    
    else: ;some other event
  endcase
  
  if liveon then widget_control, event.top, set_UValue=info, /No_Copy
end





pro IRISsim_Starter, OBSfile=OBSfile, OBSdir=OBSdir

  ;  ; INPUT
  if keyword_set(OBSfile) then begin
    if keyword_set(OBSdir) then OBSfile=OBSdir+OBSfile
  endif else begin
    if keyword_set(OBSdir) then OBSfile=OBSdir $
    else OBSfile=''
  endelse



  labelsize=130
  fieldsize=70
  
  
  
  MainWindow = WIDGET_BASE(/COLUMN, title='IRIS Simulator', XOffset=0, YOffset=0, MBAR=mbar)
  menufile = WIDGET_BUTTON(mbar, Value='File', /MENU)
  loadparam = WIDGET_BUTTON(menufile, Value='Load Parameters', uname='loadparam')
  loaddefault = WIDGET_BUTTON(menufile, Value='Load Default Parameters', uname='loaddefault')
  saveparam = WIDGET_BUTTON(menufile, Value='Save Parameters', uname='saveparam')
  savedefault = WIDGET_BUTTON(menufile, Value='Save as Default Parameters', uname='savedefault')
  
  inputbase = WIDGET_BASE(MainWindow, /Column, frame=2)
  labelinput = WIDGET_LABEL(inputbase, value='INPUT', /align_center, frame=1)
  
  baseOBSfile = WIDGET_BASE(inputbase, /Row)
  labelOBSfile = WIDGET_LABEL(baseOBSfile, value='OBS input file', /align_left, xsize=labelsize)
  textOBSfile = WIDGET_TEXT(baseOBSfile, value=OBSfile, /editable, xsize=fieldsize)
  buttonOBSfile = WIDGET_BUTTON(baseOBSfile, value='Change', uname='OBSfile')
  
  basefuvdir = WIDGET_BASE(inputbase, /Row)
  labelfuvdir = WIDGET_LABEL(basefuvdir, value='FUV sim input folder', /align_left, xsize=labelsize)
  textfuvdir = WIDGET_TEXT(basefuvdir, value='params.fuvdir', /editable, xsize=fieldsize)
  buttonfuvdir = WIDGET_BUTTON(basefuvdir, value='Change', uname='FUVdir')
  
  basefuvfile = WIDGET_BASE(inputbase, /Row)
  labelfuvfile = WIDGET_LABEL(basefuvfile, value='FUV sim input format', /align_left, xsize=labelsize)
  textfuvfile = WIDGET_TEXT(basefuvfile, value='params.fuvfile', /editable, xsize=fieldsize)
  
  basenuvdir = WIDGET_BASE(inputbase, /Row)
  labelnuvdir = WIDGET_LABEL(basenuvdir, value='NUV sim input folder', /align_left, xsize=labelsize)
  textnuvdir = WIDGET_TEXT(basenuvdir, value='params.nuvdir', /editable, xsize=fieldsize)
  buttonnuvdir = WIDGET_BUTTON(basenuvdir, value='Change', uname='NUVdir')
  
  basenuvfile = WIDGET_BASE(inputbase, /Row)
  labelnuvfile = WIDGET_LABEL(basenuvfile, value='NUV sim input format', /align_left, xsize=labelsize)
  textnuvfile = WIDGET_TEXT(basenuvfile, value='params.nuvfile', /editable, xsize=fieldsize)
  
  basedtime = WIDGET_BASE(inputbase, /Row)
  labeldtime = WIDGET_LABEL(basedtime, value='dtime [s]', /align_left, xsize=labelsize)
  textdtime = WIDGET_TEXT(basedtime, value='params.dtime', /editable, xsize=20)
  labeldtspace = WIDGET_LABEL(basedtime, value='', /align_left, xsize=60)
  labeldtimend = WIDGET_LABEL(basedtime, value='dtime wrap [s]', /align_left, xsize=90)
  textdtimend = WIDGET_TEXT(basedtime, value='params.dtimend', /editable, xsize=20)
  
  baseobsrep = WIDGET_BASE(inputbase, /Row)
  labelobsrep = WIDGET_LABEL(baseobsrep, value='OBS repetitions', /align_left, xsize=labelsize)
  textobsrep = WIDGET_TEXT(baseobsrep, value='0', /editable, xsize=20)
  
  
  outputbase = WIDGET_BASE(MainWindow, /Column, frame=2)
  labeloutput = WIDGET_LABEL(outputbase, value='OUTPUT', /align_center, frame=1)
  
  basefiles = WIDGET_BASE(outputbase, /Row)
  labeloutspace1 = WIDGET_LABEL(basefiles, value='', /align_left, xsize=40)
  output1 = ['save graphs', 'make movie', 'play movie']
  bgoutput1 = CW_BGROUP(basefiles, output1, /Column, /Nonexclusive, uname='bgoutput1')
  labeloutspace2 = WIDGET_LABEL(basefiles, value='', /align_left, xsize=60)
  output2 = ['fitsfiles', 'copy xml-files', 'csv logfile', 'txt logfile']
  bgoutput2 = CW_BGROUP(basefiles, output2, /Column, /Nonexclusive, uname='bgoutput2')
  labeloutspace3 = WIDGET_LABEL(basefiles, value='', /align_left, xsize=80)
  
  basewin0 = WIDGET_BASE(basefiles, /Column)
  labelwin = WIDGET_LABEL(basewin0, value='max window size [pixels]', /align_left, xsize=150)
  basewin1 = WIDGET_BASE(basewin0, /Row)
  labelx = WIDGET_LABEL(basewin1, value='X', /align_left, xsize=20)
  textx = WIDGET_TEXT(basewin1, value='maxWINsize[0]', /editable, xsize=10)
  basewin2 = WIDGET_BASE(basewin0, /Row)
  labely = WIDGET_LABEL(basewin2, value='Y', /align_left, xsize=20)
  texty = WIDGET_TEXT(basewin2, value='maxWINsize[1]', /editable, xsize=10)
  
  basendest = WIDGET_BASE(outputbase, /Row)
  labedest = WIDGET_LABEL(basendest, value='Output folder', /align_left, xsize=labelsize)
  textdest = WIDGET_TEXT(basendest, value='params.dest', /editable, xsize=fieldsize)
  buttondest = WIDGET_BUTTON(basendest, value='Change', uname='outfolder')
  
  basenqtloc = WIDGET_BASE(outputbase, /Row)
  labelqtloc = WIDGET_LABEL(basenqtloc, value='QuickTime location', /align_left, xsize=labelsize)
  textqtloc = WIDGET_TEXT(basenqtloc, value='params.qtloc', /editable, xsize=fieldsize)
  buttonqtloc = WIDGET_BUTTON(basenqtloc, value='Change', uname='qtloc')
  
  
  
  graphbase = WIDGET_BASE(MainWindow, /Column, frame=2)
  labelgraph = WIDGET_LABEL(graphbase, value='GRAPH', /align_center, frame=1)
  
  baseovsji = WIDGET_BASE(graphbase, /Row)
  labelovsji = WIDGET_LABEL(baseovsji, value='Overview SJI:  frame [arcsec]', /align_left, xsize=180)
  textovsji = WIDGET_TEXT(baseovsji, value='params.ovsji', /editable, xsize=20)
  labelovtype = WIDGET_LABEL(baseovsji, value='Type (fu1, fu2, nu1, nu2)', /align_left, xsize=150)
  textovtype = WIDGET_TEXT(baseovsji, value='params.ovtype', /editable, xsize=12)
  
  basethres = WIDGET_BASE(graphbase, /Row)
  labelthresov = WIDGET_LABEL(basethres, value='scaling threshold:  Overview', /align_left, xsize=180)
  textthresov = WIDGET_TEXT(basethres, value='params.thresov', /editable, xsize=10)
  labelthressji = WIDGET_LABEL(basethres, value='SJI', /align_right, xsize=60)
  textthressji = WIDGET_TEXT(basethres, value='params.thressji', /editable, xsize=10)
  labelthresspec = WIDGET_LABEL(basethres, value='Spec', /align_right, xsize=65)
  textthresspec = WIDGET_TEXT(basethres, value='params.thresspec', /editable, xsize=10)
  
  
  
;  debugbase = WIDGET_BASE(MainWindow, /Column, frame=2)
;  labeldebug = WIDGET_LABEL(debugbase, value='DEBUGGING', /align_center, frame=1)
;  
;  basedebug = WIDGET_BASE(debugbase, /Row)
;  debug1 = ['Debugging']
;  bgdebug = CW_BGROUP(basedebug, debug1, /Column, /Nonexclusive, uname='bgdebug')
;  labeldebugspace1 = WIDGET_LABEL(basedebug, value='', /align_left, xsize=30)
;  labeldebug = WIDGET_LABEL(basedebug, value='nr. steps', /align_left, xsize=60)
;  textdebug = WIDGET_TEXT(basedebug, value='params.debugnr', /editable, xsize=20)
;  labeldebugspace2 = WIDGET_LABEL(basedebug, value='', /align_left, xsize=40)
;  labeldebugt = WIDGET_LABEL(basedebug, value='add. time [ms]', /align_left, xsize=60)
;  textdebugt = WIDGET_TEXT(basedebug, value='params.debugt', /editable, xsize=20)
  
  
  
  startbutton = WIDGET_BUTTON(MainWindow, value='Start Simulation', uname='startsim')
  
  
  
  ;define structure with information to be passed around
  info = { $
    textOBSfile:textOBSfile, $
    textfuvdir:textfuvdir, $
    textfuvfile:textfuvfile, $
    textnuvdir:textnuvdir, $
    textnuvfile:textnuvfile, $
    textdtime:textdtime, $
    textdtimend:textdtimend, $
    bgoutput1:bgoutput1, $
    bgoutput2:bgoutput2, $
    textx:textx, $
    texty:texty, $
    textdest:textdest, $
    textqtloc:textqtloc, $
    textovsji:textovsji, $
    textovtype:textovtype, $
    textthresov:textthresov, $
    textthressji:textthressji, $
    textthresspec:textthresspec, $
    textobsrep:textobsrep $
;    bgdebug:bgdebug, $
;    textdebug:textdebug, $
;    textdebugt:textdebugt $
    }
    
  IRISsim_paramload, 1, info
  
  ;set this structure as the user-defined value of mainwindow, and give the other windows access to mainwindow
  widget_control, MainWindow, set_UValue=info, /No_Copy
  WIDGET_CONTROL, MainWindow, /realize
  xmanager, 'IRISsim_Starter', MainWindow, /no_block
end
