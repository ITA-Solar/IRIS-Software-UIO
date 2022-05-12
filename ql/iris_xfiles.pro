;+
; NAME:
;       IRIS_XFILES
;
; PURPOSE:
;
;       IRIS_XFILES is used to select data files from data bases.  
;       IRIS_XFILES defines the data objects, header objects and
;       auxiliary objects and sends them to XDISPLAY or
;       IRIS_XDISPLAY. The XDISPLAY window is opened when the 
;       user selects a data file in IRIS_XFILES.
;
;
; CATEGORY:
;       Hansteen/Wikst�l Data analysis SW
;
; CALLING SEQUENCE:
;       iris_xfiles
;
; INPUTS:
;       none
;
; KEYWORD PARAMETERS:
;       none
;
;
; OUTPUTS:
;       Opens the XDISPLAY widget
;
; CALLS:
;
;
; COMMON BLOCKS:
;
;
; PROCEDURE:
;       IRIS_XFILES searches through data bases (directories) for data
;       files. Which data base and directory can be selected from the
;       various data sources. The user can provide other data sources
;       under the "other" button, in which case the directory of data
;       must be specified, along with the routines to read the data.
;
; RESTRICTIONS:
;
;
; MODIFICATION HISTORY:
;       2001: Oivind Wikstol. Gradually developed through the year.
;       19-Apr-2004: Oivind Wikstol - Cleaned up.
;       06-May-2004: Oivind Wikstol. Changed call to xcontrol for
;                    ccsds type. 
;       18-Nov-2006: Viggo H. Cleaned up. Made fits default file type, 
;                    activated date filter, added filename filter.
;       29-Sep-2007: Alessandro Gardini. Added the Confirmation button. Set 
;                    the image device to Pixmap. Freed (*info).filelist 
;                    each time it is redefined, and at the end. Renamed the
;                    various Row# in the widget according to their order.
;                    The function "findfile" was replaced by "file_search"
;                    already on 19-Jun-2007.
;       18-Mar-2008: A. Gardini. Check on level 2 FITS files, and call of
;                    xmap instead of xcontrol.
;       24-May-2013: Viggo H. IRIS version
;       2014-2016:   Martin Wiesmann, added new features, e.g. showing
;                    OBS and corresponding files separately, made it faster
;   
;$Id: iris_xfiles.pro,v 1.81 2022/05/12 11:50:20 mawiesma Exp $
;-
;
; Start Xfiles:
; xfiles exit:
pro iris_xfiles_exit, event
 widget_control, event.top, /destroy
end

; iris_xfiles cleanup
pro iris_xfiles_cleanup, tlb
  widget_control, tlb, get_uvalue = info
  sdir=(*info).sdir
  ignoretime=(*info).ignoretime
  tstartval=(*info).tstartval
  tstopval=(*info).tstopval
  spatterns = *(*info).spatterns
  xmlfolder = (*info).xmlfolder
  (*info).recentwindows->gettimes, starttimes, endtimes
  if valid_time(tstartval) && valid_time(tstopval) then $
    save, sdir, tstartval, tstopval, spatterns, ignoretime, starttimes, endtimes, xmlfolder, $
      filename=IRISxfiles_appReadme()+'/iris_xfiles_searches.sav'
  ptr_free, (*info).filelistall
  ptr_free, (*info).filelist
  ptr_free, (*info).file2obsmap
  ptr_free, (*info).OBSids
  ptr_free, (*info).OBSreps
  ptr_free, (*info).spatterns
  ptr_free, info
end

; determine data source and search directory
function iris_xfiles_source, event
  widget_control, event.top, get_uvalue = info
  (*info).sdir = (*info).dsource_dir[event.value]
  (*info).datatype=(*info).dsource[event.value]
  sensitive = 1
  (*info).filter = (*info).sfilters[event.value]
  widget_control, (*info).filtercw, set_value = (*info).filter
  widget_control, (*info).searchdir, set_value = (*info).sdir,  $
                  sensitive = sensitive
  widget_control, (*info).foundfiles, set_value = filelist
  return, 0
end


;this procedure opens a new widget in modal mode, in which one can edit
;the search patterns, which will be saved in the user's .idl directory
pro iris_xfiles_editpattern, event
  widget_control, event.top, get_uvalue = info
  temp = iris_xfiles_editpatternwidget(*(*info).spatterns, event.top)
  if temp.names[0] ne 'Cancel' then begin
    widget_control, (*info).tstart, get_value=tstartval
    (*info).tstartval=tstartval
    widget_control, (*info).tstop, get_value=tstopval
    (*info).tstopval=tstopval
    *(*info).spatterns = temp
    sdir=(*info).sdir
    ignoretime=(*info).ignoretime
    spatterns = *(*info).spatterns
    xmlfolder = (*info).xmlfolder
    (*info).recentwindows->gettimes, starttimes, endtimes
    if valid_time(tstartval) && valid_time(tstopval) then $
      save, sdir, tstartval, tstopval, spatterns, ignoretime, starttimes, endtimes, xmlfolder, $
        filename=IRISxfiles_appReadme()+'/iris_xfiles_searches.sav'
    
    widget_control, (*info).searchdroplist, set_value=spatterns.names, SET_DROPLIST_SELECT=spatterns.defaul
    widget_control, (*info).searchdir, set_value=spatterns.paths[spatterns.defaul]
  endif
end


pro iris_xfiles_startsearch, event
  widget_control, event.top, get_uvalue = info
  widget_control, (*info).tstart, get_value=tstartval
  (*info).tstartval=tstartval
  widget_control, (*info).tstop, get_value=tstopval
  (*info).tstopval=tstopval
  if valid_time(tstartval) && valid_time(tstopval) then begin
    sdir=(*info).sdir
    ignoretime=(*info).ignoretime
    spatternind = widget_info((*info).searchdroplist, /droplist_select)
    spatterns = *(*info).spatterns
    xmlfolder = (*info).xmlfolder
    
    ;update recent time window list
    if spatterns.usetree[spatternind] || ~ignoretime then begin
      (*info).recentwindows->newsearch, tstartval, tstopval
      widget_control, (*info).recentdroplist, set_value=(*info).recentwindows->getwindows()
    endif
    (*info).recentwindows->gettimes, starttimes, endtimes

    save, sdir, tstartval, tstopval, spatterns, ignoretime, starttimes, endtimes, xmlfolder, $
      filename=IRISxfiles_appReadme()+'/iris_xfiles_searches.sav'
      
    iris_xfiles_searchdir, info
  endif else box_message,'invalid time format(s)'
end

;this procedure searches a directory(-tree) for files using the filter and the start- and stoptimes
pro iris_xfiles_searchdir, info
  widget_control, /hourglass
  dirsep = path_sep()
  spatternind = widget_info((*info).searchdroplist, /droplist_select)
  spatterns = *(*info).spatterns
  
  ;we have to make sure that we have the correct dates
  widget_control, (*info).tstart, get_value=tstartval
  (*info).tstartval=tstartval
  widget_control, (*info).tstop, get_value=tstopval
  (*info).tstopval=tstopval
  ;and the correct filter
  widget_control,(*info).filtercw, get_value = filter
  filter = strtrim(filter, 2)
  if filter eq '' then begin
    filter='*'
    widget_control,(*info).filtercw, set_value = filter
  endif
  (*info).filter=filter
  
  if valid_time(tstartval) && valid_time(tstopval) then begin
    startdate=anytim2cal(tstartval,form=8)
    stopdate=anytim2cal(tstopval,form=8)
    
    if spatterns.usetree[spatternind] then paths = ssw_time2paths(tstartval, tstopval, spatterns.paths[spatternind]) $
    else paths = spatterns.paths[spatternind]
    if paths[0] eq '' then paths = (*info).sdir
    
    ;tic
    for p=0,N_ELEMENTS(paths)-1 do begin
      stopevent = widget_event((*info).searchstopbutton, /nowait)
      widget_control, /hourglass
      if stopevent.id gt 0 then begin
        box_message,'Canceling search'
        break
      endif
      if file_test(paths[p]) then begin
        if strmid(paths[p], 0,1, /reverse_offset) ne dirsep then paths[p] = paths[p]+dirsep
        if spatterns.searchsubdir[spatternind] then begin
          ;normal case
          ;file_search is slow, use this for windows
          if !version.os_family ne 'unix' then begin
            temp = file_search(paths[p], (*info).filter, count=fcount)
          endif else begin
            if ~spatterns.usetree[spatternind] then begin
              temp = file_search(paths[p], (*info).filter, count=fcount)
            endif else begin
              temp=!NULL
              spawn, 'ls ' + paths[p], temp0
              dum = extract_fids(temp0, fidsfound=fidsfound)
              dirgood = where(fidsfound, fcount2)
              fcount=0
              if fcount2 gt 0 then begin
                temp0=temp0[dirgood]
                dirdates=anytim2cal(file2time(temp0), form=8)
                dirind=where((dirdates ge startdate) AND (dirdates le stopdate), count)
                if count gt 0 then begin
                  for idir=0,count-1 do begin
                    idir_cur = dirind[idir]
                    if strmid(temp0[idir_cur], 0,1, /reverse_offset) ne dirsep then temp0[idir_cur] = temp0[idir_cur]+dirsep
                    spawn, 'ls ' + paths[p] + temp0[idir_cur], temp1
                    if (*info).filter ne '' then begin
                      findin = where(strmatch(temp1, (*info).filter, /fold_case) eq 1, fcount0)
                      if fcount0 gt 0 then temp1 = temp1[findin] $
                      else temp1=''
                    endif
                    if temp1[0] ne '' then temp1 = paths[p]+temp0[idir_cur]+temp1
                    fcount = fcount + fcount0
                    if fcount0 gt 0 then begin
                      if N_ELEMENTS(temp) eq 0 then temp=temp1 $
                      else temp=[temp, temp1]
                    endif
                  endfor ;idir=0,count-1
                endif ;count gt 0
              endif ;fcount2 gt 0
            endelse ;~spatterns.usetree[spatternind]
          endelse ;test purpose, activates the old version; which is now the version for windows ;!version.os_family ne 'unix'
        endif else begin ;spatterns.searchsubdir[spatternind]
          temp = file_search(paths[p]+(*info).filter, count=fcount)
        endelse

        if fcount gt 0 then begin
          dum = extract_fids(file_basename(temp), fidsfound=fidsfound)
          fgood = where(fidsfound, fcount2)
          if fcount2 gt 0 then begin
            temp=temp[fgood]
            ;if spatterns.usetree[spatternind] || ~(*info).ignoretime then begin ;old version
            if ~spatterns.usetree[spatternind] && ~(*info).ignoretime then begin
              filedates=anytim2cal(file2time(file_basename(temp)), form=8)
              fileind=where((filedates ge startdate) AND (filedates le stopdate), count)
              if count gt 0 then begin
                if N_ELEMENTS(files) eq 0 then files=temp[fileind] $
                else files=[files, temp[fileind]]
              endif
            endif else begin
              if N_ELEMENTS(files) eq 0 then files=temp $
              else files=[files, temp]
            endelse
          endif
        endif
      endif
    endfor
;        print,'days: ', N_ELEMENTS(paths), 'files: ', fcount
;        toc
;        tic

    ;now we search the headers for different runs of OBS to display
    OBSdesc=''
    file2obsmap=0
    if N_ELEMENTS(files) gt 0 then begin
      fitsind = where(strmatch(files, '*.fits') eq 1, count)
      if count gt 0 then begin
        fitsfiles = files[fitsind]
        fitsfiledates = anytim2cal(file2time(file_basename(fitsfiles)), form=8)
        uniqin = UNIQ(fitsfiledates, sort(fitsfiledates))
        template={STARTOBS:'', OBSID:'', OBS_DEC:'', OBS_DESC:'', XCEN:0.0, YCEN:0.0, SAT_ROT:0.0, OBSREP:0L} 
        for fit=0,N_ELEMENTS(uniqin)-1 do begin
          ind = where(fitsfiledates eq fitsfiledates[uniqin[fit]], count)
          if count gt 0 then begin
            mreadfits_header, fitsfiles[ind[0]], hdrtemp, only_tags='STARTOBS,OBSID,OBS_DEC,OBS_DESC,XCEN,YCEN,SAT_ROT,OBSREP', template=template
            ;before 5.9.13 the keyword OBS_DESC was misspelled as OBS_DEC
            ;so we have to check if it is populated
            if strcompress(hdrtemp.OBS_DESC, /remove_all) eq '' then hdrtemp.OBS_DESC=hdrtemp.OBS_DEC
            if N_ELEMENTS(hdr) eq 0 then hdr=hdrtemp $
            else hdr=[hdr,hdrtemp]
          endif
        endfor
        OBSdesc = get_infox(hdr, 'STARTOBS, OBSID, OBS_DESC, XCEN, YCEN, SAT_ROT', header=header, $
          format='a,a,a,(f7.1),(f7.1),(f7.1)')
        OBSdesc = [header, OBSdesc]
        OBSids = get_infox(hdr, 'OBSID', format='a')
        OBSreps = get_infox(hdr, 'OBSREP', format='a')
        OBSreps = long(OBSreps)
        ;need to map the files to the OBS
        filedates=long64(anytim2cal(file2time(file_basename(files)), form=8))
        file2obsmap = make_array(N_ELEMENTS(files), value=-1L)
        for i=0,N_ELEMENTS(uniqin)-1 do begin
          OBSdate = long64(fitsfiledates[uniqin[i]])
          fileind = where(abs(filedates-OBSdate) le 2, count)
          if count gt 0 then file2obsmap[fileind]=i+1
        endfor
      endif
    endif else files=''
    ptr_free, (*info).filelistall
    (*info).filelistall = ptr_new(files)
    ptr_free, (*info).file2obsmap
    (*info).file2obsmap = ptr_new(file2obsmap)
    ptr_free, (*info).OBSids
    (*info).OBSids = ptr_new(OBSids)
    ptr_free, (*info).OBSreps
    (*info).OBSreps = ptr_new(OBSreps)
    widget_control, (*info).foundOBS, set_value = OBSdesc
    widget_control, (*info).foundOBS, set_list_select = 1
    ind=where(file2obsmap eq 1, count)
    if count gt 0 then displayfiles=files[ind] $
    else displayfiles=''
    ptr_free, (*info).filelist
    (*info).filelist = ptr_new(displayfiles)
    widget_control, (*info).foundfiles, set_value = displayfiles
  endif else box_message,'invalid time format(s)'
  ;toc
end


function iris_xfiles_stopsearch, event
  return, {widget_stopsearch, id:1L, top:0L, handler:0L}
end

pro iris_xfiles_event, event
;this is just here for the stop button, because apparently I can't define an event_func and event_pro at the same time
;when there is an event_func defined, it ignores event_pro and searches for iris_xfiles_event
end


pro iris_xfiles_changepattern, event
  widget_control, event.top, get_uvalue = info
  if event.index gt 0 then widget_control, (*info).searchdir, set_value = (*(*info).spatterns).paths[event.index] $
  else widget_control, (*info).searchdir, set_value = (*info).sdir
end


; set search filter
pro iris_xfiles_filter, event
  dirsep = path_sep()
  widget_control, event.top, get_uvalue = info
  filter = strtrim(event.value, 2)
  if filter eq '' then begin
    filter='*'
    widget_control,(*info).filtercw,set_value = filter
  endif
  (*info).filter=filter
end


; filters files according to date
pro iris_xfiles_date, event
  widget_control, event.top, get_uvalue = info
  case event.id of 
    (*info).tstart: begin
      if valid_time(event.value) then begin
        (*info).tstartval=event.value
      endif else begin
        box_message,'invalid time format in start time'
        return
      endelse
    end
    (*info).tstop: begin
      if valid_time(event.value) then begin
        (*info).tstopval=event.value
      endif else begin
        box_message,'invalid time format in stop time'
        return
      endelse
    end
    (*info).ignoredatebg: begin
      widget_control, (*info).ignoredatebg, get_value=ignoretime
      (*info).ignoretime=ignoretime[0]
    end
    (*info).recentdroplist:begin
      recentind = event.index
      (*info).recentwindows->gettimes, starttimes, endtimes, index=recentind
      widget_control, (*info).tstart, set_value=starttimes
      (*info).tstartval = starttimes
      widget_control, (*info).tstop, set_value=endtimes
      (*info).tstopval = endtimes
    end
  endcase
end

pro iris_xfiles_currentdate, event
  widget_control, event.top, get_uvalue = info
  GET_UTC, tstopval, /stime, /truncate
  widget_control, (*info).tstop, set_value = tstopval
  if event.id eq (*info).getlast5days then begin
    tstartval = str2utc(tstopval)
    tstartval.mjd = tstartval.mjd-5
    tstartval = utc2str(tstartval, /STIME, /truncate)
    widget_control, (*info).tstart, set_value = tstartval
  endif else widget_control, (*info).tstart, get_value = tstartval
end

; list files in directory
pro iris_xfiles_dir, event
  dirsep = path_sep()
  widget_control, event.top, get_uvalue = info
  sdir = strtrim(event.value, 2)
  if strmid(sdir, 0,1, /reverse_offset) ne dirsep then sdir = sdir+dirsep
  (*info).sdir=sdir
  widget_control, (*info).searchdir, set_value=sdir
  widget_control, (*info).searchdroplist, set_droplist_select = 0
end

pro iris_xfiles_changesdir, event
  widget_control, event.top, get_uvalue = info
  sfile=dialog_pickfile(path=(*info).sdir, title='Please select a directory', get_path=sdir)
  if sdir ne '' then begin
    (*info).sdir=sdir
    widget_control, (*info).searchdir, set_value=sdir
    widget_control, (*info).searchdroplist, set_droplist_select = 0
  endif
end


;user selected an OBS, we have to display to files which go with it
pro iris_xfiles_selectOBS, event
  widget_control, event.top, get_uvalue = info
  ind=where(*(*info).file2obsmap eq event.index, count)
  if count gt 0 then displayfiles=(*(*info).filelistall)[ind] $
  else displayfiles=''
  ptr_free, (*info).filelist
  (*info).filelist = ptr_new(displayfiles)
  widget_control, (*info).foundfiles, set_value = displayfiles
end


; print filename to console
pro iris_xfiles_printfilename, event
  widget_control, event.top, get_uvalue = info
  print,(*info).fileselect
end


; download xml-files and show the tables
pro iris_xfiles_showtables, event
  widget_control, event.top, get_uvalue = info
  num=widget_info((*info).foundOBS, /list_number)
  if num gt 1 then begin
    ind=widget_info((*info).foundOBS, /list_select)
    OBS=(*(*info).OBSids)[ind-1]
    OBSrep=(*(*info).OBSreps)[ind-1]
    desc = [ $
      '0, LABEL, Show OBS tables for, CENTER', $
      '0, LABEL, OBS '+OBS+', CENTER', $
      '0, TEXT, '+(*info).xmlfolder+', LABEL_LEFT=Directory for XML-files: , WIDTH=80, TAG=folder', $
      '1, BASE,, ROW', $
      '0, BUTTON, OK, QUIT, TAG=OK', $
      '2, BUTTON, CANCEL, QUIT']
    answer = CW_FORM(desc, /COLUMN, title='Show OBS tables')

    if answer.ok then begin
      file_mkdir, answer.folder
      (*info).xmlfolder=answer.folder
      sdir=(*info).sdir
      ignoretime=(*info).ignoretime
      tstartval=(*info).tstartval
      tstopval=(*info).tstopval
      spatterns = *(*info).spatterns
      xmlfolder = (*info).xmlfolder
      (*info).recentwindows->gettimes, starttimes, endtimes
      if valid_time(tstartval) && valid_time(tstopval) then $
        save, sdir, tstartval, tstopval, spatterns, ignoretime, starttimes, endtimes, xmlfolder, $
          filename=IRISxfiles_appReadme()+'/iris_xfiles_searches.sav'
      OBS='OBS-'+OBS+'.xml'
      depend=iris_obstab2depend(OBS, parent=xmlfolder, outdir=xmlfolder, /check, nmissing=nmissing, _extra=_extra)
      if size(depend, /type) eq 2 then begin
        print, 'xml-file ' + OBS + ' not found'
      endif else if nmissing gt 0 then begin
        print,'not all XML files are available, exiting...'
      endif else begin
        IRISsim_showXML, OBSfilein=depend.obsfiles, OBSrep=OBSrep
      endelse
    endif
  endif
end


; save the selected file
pro iris_xfiles_select, event
  widget_control, event.top, get_uvalue = info
; first check if this is the second click of a double click
; ...if so call iris_xfiles_read
  if event.clicks eq 2 then begin
    pseudoevent={widget_button,id:0L, $
                 top:event.top, handler:0l, select:1}
    iris_xfiles_read,pseudoevent
    return
  endif
; first click, so figure out file and/or directory required...
  findx = event.index
  flist = *(*info).filelist
  sdir=(*info).sdir

  ; add full path to filenames in subdirectories, so that these 
  ; files can be selected directly
  ; first check if the first entry is a subdirectory (it ends with a ':')

  last_char=strmid(flist(0),0,/reverse_offset)
  
  ; find the indexes of the rest of the subdirectories
  subdirindx=where(flist eq '',count)+1
  nsub = n_elements(subdirindx) ; number of subdirectories
  if count eq 0 then nsub=-1 
  ;check if first entry in flist also is subdirectory 
  ;(special case since it is then not lead by an empty entry)
  if last_char eq ':' then begin
    subdir=flist[0]
    slen=strlen(subdir)  ; length of string
    ;take out ':' at the end and add dirsep    
    subdir=strmid(subdir,0,slen-1)+(*info).dirsep
    flist[0]=subdir
    start=1
    stop=subdirindx[0]-2
    if stop gt start then flist(start:stop)=subdir+flist[start:stop]
  endif
  ; then add path to the rest of the files in subdirectories
  for i=0,nsub-1 do begin
    subdir=flist[subdirindx[i]]
    slen=strlen(subdir)  ; length of string
    ;take out ':' at the end and add dirsep    
    subdir=strmid(subdir,0,slen-1)+(*info).dirsep
    flist[subdirindx[i]]=subdir
    start = subdirindx[i]+1
    if i eq nsub-1 then stop=n_elements(flist)-1 else $
                        stop=subdirindx[i+1]-2
    if stop gt start then flist[start:stop]=subdir+flist[start:stop]
  endfor
  (*info).fileselect = flist[findx]      ; selected file
;  if the file is a directory change sdir and return
  if (file_info((*info).fileselect)).directory then begin
    dirsep = path_sep()
    sdir=(*info).fileselect
    if strmid(sdir, 0,1, /reverse_offset) ne dirsep then sdir = sdir+dirsep
    (*info).sdir = sdir
    sstr = (*info).sdir + (*info).filter
    filelist = file_search(sstr, count = fcount)
    if fcount ne 0 then begin
      ptr_free, (*info).filelist 
      (*info).filelist = ptr_new(strarr(fcount))
      *(*info).filelist = filelist
    endif else filelist=' '
    widget_control, (*info).searchdir, set_value = sdir
    widget_control, (*info).foundfiles, set_value = filelist
    return
  end
end


;destroy log-window
pro iris_xfiles_logdisp_destroy, event
 widget_control, event.top,/destroy
end


;show logfile
pro iris_xfiles_logdisplay, info
  case !version.os of
    'MacOS': cr=string(13b)
    'Win32': cr=string(13b)+string(10b)
    else: cr=string(10b)
  endcase  

  OPENR, inunit, (*info).fileselect, /GET_LUN
  textall=''
  line = ''
  ; While there is text left, read it:
  WHILE ~ EOF(inunit) DO BEGIN
    READF, inunit, line
    textall=textall+line+cr
  ENDWHILE
  ; Close the files and deallocate the units:
  FREE_LUN, inunit

  ; open text widget window to dump bytes in
  log_widget = widget_base(title = 'Logfile contents',  $
                group_leader = (*info).tlb,/row)
  closefield = widget_base(log_widget,/column)
  closebutton = widget_button(closefield, value = 'Close', $
                              event_pro = 'iris_xfiles_logdisp_destroy')
  disp_base = widget_base(log_widget,/column)
  disp_field = widget_text(disp_base, group_leader = (*info).tlb, /scroll, $
  xs=160, ysize = 50, value=textall)

  widget_control, disp_field, set_text_top_line=0
  widget_control, log_widget, /realize
  xmanager, 'Display Logfile Contents', log_widget, /no_block, $
             group_leader = (*info).tlb
end


; read the selected file and call xcontrol
pro iris_xfiles_read, event
; define data object and read file
; ...but first a consistency check on the file name  
  widget_control, event.top, get_uvalue = info
  widget_control,/hourglass

  case (*info).datatype of
    'IRIS' : begin
        if not stregex((*info).fileselect,/fold_case,'.fits',/bool) then begin 
          if not stregex((*info).fileselect,/fold_case,'.*log',/bool) then begin
            ok=dialog_message('FITS file must have ".fits" extension!!', $
                            /center,title='Iris_Xfiles warning',/information, $
                            dialog_parent=(*info).tlb)
            return
          endif else begin
            iris_xfiles_logdisplay, info
            return
          endelse
        endif
        if stregex((*info).fileselect,/fold_case,'raster',/bool) then begin 
; find eventual slit jaw images
          ff=((*info).fileselect)
          file_head=strmid(ff,0,strpos(ff,'raster')-1)
          fsij=file_search(file_head+'_SJI*.fits')
          d=iris_obj([(*info).fileselect,fsij])
          iris_xcontrol,d,group_leader=(*info).tlb
        endif else begin
; should be a slit jaw image file...
          d=iris_sji((*info).fileselect)
          if not d->badfile() then begin
            d->ximovie,min(where(d->lwin_read())),group_leader=(*info).tlb
          endif else begin
           error=['Dimensions of SJI file not consistent', $
                  'cannot run ximovie']
           continue=dialog_message(error,dialog_parent=(*info).tlb)
          endelse
        endelse               
             end
    'EIS/CCSDS' : begin
        if stregex((*info).fileselect,/fold_case,'.fits',/bool) then begin 
          ok=dialog_message('FITS file cannot be read as CCSDS file!!', $
                            /center,title='Iris_Xfiles warning',/information)
          return
        endif
        aux_obj=obj_new('eis_aux',(*info).fileselect)
        data_obj= obj_new('eis_data',(*info).fileselect, datasource='ccsds', hdr = hdr)
        xcontrol, data_obj, hdr, aux_obj, $
                  group_leader = (*info).tlb, filename = (*info).fileselect
        end
    'EIS/FITS'  : begin
        if not stregex((*info).fileselect,/fold_case,'.fits',/bool) then begin 
          ok=dialog_message('FITS file must have ".fits" extension!!', $
                            /center,title='Iris_Xfiles warning',/information)
          return
        endif
        aux_obj = obj_new('eis_aux',(*info).fileselect)
        data_obj = obj_new('eis_data',(*info).fileselect, datasource='fits')
        if data_obj->getfitslev() eq 2 then begin 
          data_obj->setcomment,'moments'
          linelist = indgen(data_obj->getnwin())          
          xmap, data_obj, data_obj->gethdr(), aux_obj, $
                linelist=linelist, group_leader = (*info).tlb
        endif else begin
          xcontrol, data_obj, (data_obj->gethdr()), aux_obj, $
                group_leader = (*info).tlb, filename = (*info).fileselect 
        endelse
        end
    'EIS/HK'    : begin
        hk_packet, (*info).fileselect
        end
    else:
  endcase
end

pro iris_xfiles, dsrc=dsrc, dir=dir
;
  if 1 then begin
  ; launch the error handler:
  catch, error_status
  ; begin error handler
  if error_status ne 0 then begin
    void=error_message([!error_state.msg, $
      'Please report this error to the DAI system', $
      'https://www.lmsal.com/iris_science/DAI/requestSearch', $
      'including the traceback report, printed on the commandline after clicking OK'], $
      /traceback, /center)
    return
  endif
  endif
  
  dsource = ['IRIS', 'EIS/CCSDS', 'EIS/FITS', 'EIS/HK']
  home = getenv ('HOME')
  eis_inst = getenv('EIS_INST')
  eis_data = getenv('EIS_DATA')
  iris_data = getenv('IRIS_DATA')
  eis_ccsds_data = getenv('EIS_CCSDS_DATA')
  eis_fits_data = getenv('EIS_FITS_DATA')
  eis_hk_data = getenv('EIS_HK_DATA')
  eis_cal_data = getenv('EIS_CAL_DATA')

  cdir = local_name (eis_ccsds_data)
  fdir = local_name (eis_fits_data)
  idir = local_name (iris_data)
  hdir = local_name (eis_hk_data)

  if not is_dir (cdir) then cdir = "."
  if not is_dir (fdir) then fdir = "."
  if not is_dir (idir) then idir = "."
  if not is_dir (hdir) then hdir = "."
  
  sdirfile=IRISxfiles_appReadme()+'/iris_xfiles_searches.sav'
  if file_test(sdirfile) then begin
    restore,sdirfile
    ;sdirfile contains the variables: sdir, tstartval, tstopval
    ;and if it's made after 14-Aug-2013 it contains also the structure: spatterns
    ;and ignoretime and the recent start and stop dates
    ;and the folder for the xml-files
    idir=sdir
  endif

  dsource_dir =  [idir, cdir, fdir, hdir]
  if n_elements(dsrc) ne 0 then begin
    dsource=dsrc
  endif 
  
  sfilters = ['iris_l2*', '*', '*', '*']

; initialize
  if n_elements(tstartval) eq 0 then begin
    tstartval = '17-Jun-13 18:14:05' ;IRIS door opens
    tstopval = '22-Jun-13 23:59:59'
  endif else begin
    if ~valid_time(tstartval) || ~valid_time(tstopval) then begin
      GET_UTC, tstopval, /stime, /truncate
      tstartval = str2utc(tstopval)
      tstartval.mjd = tstartval.mjd-5
      tstartval = utc2str(tstartval, /STIME, /truncate)
    endif
  endelse
  
  if n_elements(spatterns) eq 0 then begin
    names = ['free search', 'LMSAL', 'UIO', 'local']
    paths = ['', $
      '/irisa/data/level2/', $
      '/mn/stornext/d10/HDC2/iris/data/level2/', $
      '~']
    usetree = [0, 1, 1, 0]
    searchsubdir = [0, 1, 1, 0]
    defaul = 2
    spatterns = {names:names, paths:paths, usetree:usetree, searchsubdir:searchsubdir, defaul:defaul}
  endif else begin
    if N_ELEMENTS(spatterns.names) gt 2 then begin
      if spatterns.names[2] eq 'archive' then begin
        spatterns.names[2] = 'LMSAL'
        spatterns.paths[2] = '/irisa/data/level2/'
        spatterns.usetree[2] = 1
        spatterns.searchsubdir[2] = 1
        spatterns.defaul = 2
      endif
    endif
  endelse
  
  if N_ELEMENTS(xmlfolder) eq 0 then cd, current=xmlfolder
  
  if n_elements(ignoretime) eq 0 then ignoretime=0
  
  if (N_ELEMENTS(starttimes) eq 0) || (N_ELEMENTS(endtimes) eq 0) then begin
    starttimes = tstartval
    endtimes = tstopval
  endif
  recentwindows = OBJ_NEW('IRIS_recent_timewindows', starttimes, endtimes)

  def=0 ;0=IRIS
  datatype=dsource[def]
  if n_elements(dir) eq 0 then begin
    if N_ELEMENTS(sdir) eq 0 then sdir = dsource_dir[def]
;   if n_elements(sdir) eq 0 then sdir='~/solarb/data/eis/'
  endif else sdir=dir
  if spatterns.paths[spatterns.defaul] eq '' then sdircur=sdir else sdircur=spatterns.paths[spatterns.defaul]
  if n_elements(sfilter) eq 0 then sfilter = sfilters[def]
  dirsep = path_sep()


; top level base widget:
  tlb = widget_base(/column, title='Iris_Xfiles - QL Control Window', $
                    xoffset=200,yoffset=200)

; first row contains exit button
  exitbase = widget_base(tlb, /row, /frame)
  exitb = widget_button(exitbase, value = 'Exit', event_pro = 'iris_xfiles_exit')

  eis_icon_base=widget_base(exitbase, /col, /align_right)
  eis_icon = widget_draw(eis_icon_base, retain = 2, $ 
                       XSize = 120, YSize = 60, frame = 1)  

  iris_icon_size=120
  iris_icon_aspect=146./200.
  iris_icon_base=widget_base(exitbase, /col, /align_right)
  iris_icon = widget_draw(iris_icon_base, retain = 2, $ 
                       XSize = iris_icon_size, YSize =iris_icon_size*iris_icon_aspect , frame = 1)  

; determine data source
  row2 = widget_base(tlb, /row, /frame) ; data source label field
  sls = 'Select data source         '    ; data source string
  slabel = widget_label(row2, value=sls)
  nsource = n_elements(dsource)
  datasource = cw_bgroup(row2, dsource, event_func = 'iris_xfiles_source', $
                       /exclusive, column = nsource )
                       
; date/time fields
  row3=widget_base(tlb, /row, /frame)
  tlabelfield = widget_base(row3,/column)
  tls = 'Start/Stop for file search. Time Units: [D]D-MON-[YR]YR HH:MM:SS[.MS]'
  tlabel = widget_label(tlabelfield, value=tls, /align_left)
  tfield=widget_base(tlabelfield, /row, event_pro='iris_xfiles_date')
  tstart=cw_field(tfield, Title='Start Time:  ', value=tstartval, /string ,/return_events)
  tstop =cw_field(tfield, Title='Stop Time:   ', value=tstopval, /string ,/return_events)
  tfieldbuttons = widget_base(row3, /Column, event_pro='iris_xfiles_currentdate')
  getlast5days = widget_button(tfieldbuttons, value='Last 5 days')
  getcurrentdate = widget_button(tfieldbuttons, value='Up until now')
  tfield2 = widget_base(row3, /column, event_pro='iris_xfiles_date')
  recentdroplist = widget_droplist(tfield2, value=recentwindows->getwindows(), title='Recent time-windows')
  ignoredatebg = cw_bgroup(tfield2, ['ignore times (only if no tree structure)'], set_value=[ignoretime], /column, /nonexclusive) 

  rowheight=29
; search filter
  row4=widget_base(tlb, /column, /frame)
  filterfield = widget_base(row4, /row, event_pro = 'iris_xfiles_filter', ysize=rowheight)
  sls = 'Set search filter '
  filtercw=cw_field(filterfield, title=sls, value = ' ', $
                   /string, /return_events, xsize = 100)
                   
; search file fields
  searchpatternfield = widget_base(row4, /row, ysize=rowheight)
  label = widget_label(searchpatternfield, value='Search Pattern:  ')
  searchdroplist = widget_droplist(searchpatternfield, value=spatterns.names, event_pro='iris_xfiles_changepattern')
  widget_control, searchdroplist, SET_DROPLIST_SELECT=spatterns.defaul
  label = widget_label(searchpatternfield, value='          ')
  searchpatternbutton = widget_button(searchpatternfield, value='Edit', event_pro='iris_xfiles_editpattern')
  label = widget_label(searchpatternfield, value='                         ')
  searchstopbutton = widget_button(searchpatternfield, value='Start Search', event_pro='iris_xfiles_startsearch')
  label = widget_label(searchpatternfield, value='     ')
  searchstopbutton = widget_button(searchpatternfield, value='Stop Search', event_func='iris_xfiles_stopsearch')
  searchdirfield = widget_base(row4, /row, event_pro = 'iris_xfiles_dir', ysize=rowheight)
  searchdir=cw_field(searchdirfield, title='Search Directory  ', value = ' ', $
                   /string, /return_events, xsize = 100)
  changesdir=widget_button(searchdirfield, value='Change', event_pro='iris_xfiles_changesdir')
                   
  case !version.os of
    'MacOS': begin
       xsize = 150
       ysize = 4
     end
     else: begin
       xsize = 150 ; in cm as units=2
       ysize = 4  ; in cm as units=2
     end
  endcase

  foundOBS=widget_list(row4, value='', /frame, xsize = xsize $
                         , scr_ysize = 0, units = 2, $
                         event_pro = 'iris_xfiles_selectOBS')
  foundfiles=widget_list(row4, value='', /frame, xsize = xsize $
                         , scr_ysize = 0, units = 2 $
                         , event_pro = 'iris_xfiles_select')
  confbase = widget_base(row4, /row, /align_left)
  confb = widget_button(confbase, value = 'Confirm selection' $
                        , event_pro = 'iris_xfiles_read')
  label = widget_label(confbase, value='                 ')
  printfile = widget_button(confbase, value = 'Print filename to console' $
                        , event_pro = 'iris_xfiles_printfilename')
  label = widget_label(confbase, value='                 ')
  showtables = widget_button(confbase, value = 'Show OBS tables' $
                        , event_pro = 'iris_xfiles_showtables')

; set up default directory
  widget_control, datasource, set_value = def
  widget_control, searchdir, set_value = sdircur, sensitive = 1
  widget_control, filtercw, set_value = sfilter

  geometry = widget_info(tlb,/geometry)
  screen = get_screen_size()
  space = float(screen[1]) - float(geometry.scr_ysize) - 30
  if space lt 1100 then widget_control,tlb,yoffset=0
  if space gt 900 then space = 900
  space = space / 5.0
  widget_control,foundOBS,scr_ysize=space*2
  widget_control,foundfiles,scr_ysize=space*3

; realize the top level base widget
  widget_control, tlb, /realize
  

; Define the info structure, used to send information around
  info= { tlb:tlb, $
          tstart:tstart, $
          tstop:tstop, $
          tstartval:tstartval, $
          tstopval:tstopval, $
          ignoretime:ignoretime, $
          ignoredatebg:ignoredatebg, $
          getlast5days:getlast5days, $
          filter:sfilter, $
          sfilters:sfilters, $
          dsource:dsource, $
          nsource:nsource, $
          dsource_dir:dsource_dir, $
          datatype:datatype, $
          dirsep:dirsep, $
          searchdir:searchdir, $
          filtercw:filtercw, $
          sdir:sdir, $
          filelist:ptr_new(), $
          filelistall:ptr_new(), $
          file2obsmap:ptr_new(), $
          OBSids:ptr_new(), $
          OBSreps:ptr_new(), $
          xmlfolder:xmlfolder, $
          fileselect:'', $
          foundOBS:foundOBS, $
          foundfiles:foundfiles, $
          spatterns:ptr_new(spatterns), $
          searchdroplist:searchdroplist, $
          searchpatternbutton:searchpatternbutton, $
          searchstopbutton:searchstopbutton, $
          recentdroplist:recentdroplist, $
          recentwindows:recentwindows}
  info=ptr_new(info,/no_copy)
  

; Set the info ptr to be the user value of the tlb widget
  widget_control,tlb, set_uvalue=info      

  widget_control, eis_icon , get_value = drawID
  wset,drawID
  fileName = concat_dir(GETENV('ancillary') , 'eis_logo_sarah_small.jpg')
  if (file_info(fileName)).exists then begin
    read_jpeg , filename , icon
    icon_resized = CONGRID(icon,3,120,60)
    tvscl,icon_resized,true = 1
  endif else begin
    xyouts,0.5,0.5,'EIS',chars=chars,/normal,alignment=0.5
  endelse
;
  widget_control, iris_icon , get_value = drawID1
  wset,drawID1
  if getenv('IRIS_ANCILLARY') eq '' then $
    set_logenv,'IRIS_ANCILLARY',concat_dir(getenv('SSW'),'iris/idl/uio/ancillary/')
  fileName = concat_dir(getenv('IRIS_ANCILLARY'),'iris_logo.jpg')
  if (file_info(fileName)).exists then begin
    read_jpeg,filename,icon
    icon_resized = congrid(icon,3,iris_icon_size,iris_icon_size*iris_icon_aspect)
    tvscl,icon_resized,true=1
  endif else begin
    xyouts,0.5,0.5,'IRIS',chars=chars,/normal,alignment=0.5
  endelse                       

  xmanager, 'iris_xfiles', tlb, /no_block, $
            group_leader = group, cleanup = 'iris_xfiles_cleanup'

end

