;+
; NAME:
;       iris_xfiles_editpatternwidget
;
; PURPOSE:
;
;       iris_xfiles_editpatternwidget is used by iris_xfiles.
;       It is a modal dialog, in which the user can choose a mode
;       to search for iris files. Or it is possible to edit or add 
;       new modes.
;
;
; CATEGORY:
;       IRIS Data analysis 
;
; CALLING SEQUENCE:
;       result = iris_xfiles_editpatternwidget(spatterns, mainwindow)
;
; INPUTS:
;       spatterns: search pattern structure as defined in iris_xfiles
;       mainwindow: ID of main window
;
; KEYWORD PARAMETERS:
;       none
;
;
; OUTPUTS:
;       Returns the updated search pattern structure
;
; CALLS:
;
;
; COMMON BLOCKS:
;
;
;
; RESTRICTIONS:
;
;
; MODIFICATION HISTORY:
;       2013:   Martin Wiesmann
;
; $Id: iris_xfiles_editpatternwidget.pro,v 1.7 2020/01/30 09:07:20 mawiesma Exp $

function iris_xfiles_editpatternwidget_tablevals, sp, alignments=alignments, ColumnLabels=ColumnLabels, widths=widths
  ColumnLabels = ['Name', 'Default']
  widths = [200, 45]
  aligntemp = [0, 1]
  void = {ptable, name:'', default:''}
  showData = make_array(N_ELEMENTS(sp.names), value={ptable})
  showData[sp.defaul].default='X'
  for i=0,N_ELEMENTS(sp.names)-1 do begin
    showData[i].name = sp.names[i]
    if i eq 0 then alignments = aligntemp $
    else alignments = [[alignments], [aligntemp]]
  endfor
  return, showData
end


pro iris_xfiles_editpatternwidget_event, event
  widget_control, event.top, get_Uvalue=info, /No_Copy
  destroyed=0
  case event.id of
    ;cancel, we do nothing, just destroy this widget
    ;the returned structure is the same as from the input
    ;except that the first name is 'Cancel'
    info.cancel: begin
      widget_control, event.top, /destroy
      destroyed=1
    end
    
    ;ok, set the current fieldvalues to the respective variable
    ;then set the new structure to the result ouptut pointer
    ;and destroy the widget
    info.ok: begin
      widget_control, info.namefield, get_value=name
      widget_control, info.pathfield, get_value=path
      sp = *(info).sp
      sp.names[info.tablesel] = name
      sp.paths[info.tablesel] = path
      *(info).result = sp
      widget_control, event.top, /destroy
      destroyed=1
    end
    
    ;user clicked the table, set fieldvalues and change fields afterwards
    info.ptable: begin
      if tag_names(event, /structure_name) eq 'WIDGET_TABLE_CELL_SEL' then begin
        if (event.sel_top ge 0) && (event.sel_top ne info.tablesel) then begin
          widget_control, info.namefield, get_value=name
          widget_control, info.pathfield, get_value=path
          sp = *(info).sp
          sp.names[info.tablesel] = name
          sp.paths[info.tablesel] = path
          *(info).sp = sp
          showData = iris_xfiles_editpatternwidget_tablevals(sp, alignments=alignments)
          widget_control, info.ptable, set_value=showData, alignment=alignments
          info.tablesel = event.sel_top
          widget_control, info.namefield, set_value=sp.names[event.sel_top], sensitive=event.sel_top
          widget_control, info.pathfield, set_value=sp.paths[event.sel_top], sensitive=event.sel_top
          widget_control, info.pathstruct, set_value=[sp.usetree[event.sel_top], sp.searchsubdir[event.sel_top]], sensitive=event.sel_top
          widget_control, info.makedefault, set_value=[event.sel_top eq sp.defaul]
        endif
      endif
    end
    
    info.additem: begin
      widget_control, info.namefield, get_value=name
      widget_control, info.pathfield, get_value=path
      sp = *(info).sp
      sp.names[info.tablesel] = name
      sp.paths[info.tablesel] = path
      defaul=sp.defaul
      names=[sp.names, '']
      paths=[sp.paths, '']
      usetree=[sp.usetree, 0]
      searchsubdir=[sp.searchsubdir, 0]
      sp = {names:names, paths:paths, usetree:usetree, searchsubdir:searchsubdir, defaul:defaul}
      ptr_free, info.sp
      info.sp = ptr_new(sp)
      info.tablesel=N_ELEMENTS(sp.names)-1
      showData = iris_xfiles_editpatternwidget_tablevals(sp, alignments=alignments)
      widget_control, info.ptable, set_value=showData, table_ysize=N_ELEMENTS(sp.names), $
        set_table_select=[0,info.tablesel,0,info.tablesel], alignment=alignments
      widget_control, info.namefield, set_value=sp.names[info.tablesel], sensitive=info.tablesel
      widget_control, info.pathfield, set_value=sp.paths[info.tablesel], sensitive=info.tablesel
      widget_control, info.pathstruct, set_value=[sp.usetree[info.tablesel], sp.searchsubdir[info.tablesel]], sensitive=info.tablesel
      widget_control, info.makedefault, set_value=[info.tablesel eq sp.defaul]
    end
    
    info.delitem: begin
      if info.tablesel gt 0 then begin
        sp = *(info).sp
        if sp.defaul eq info.tablesel then sp.defaul=0
        if sp.defaul gt info.tablesel then sp.defaul=sp.defaul-1
        defaul=sp.defaul
        n=N_ELEMENTS(sp.names)
        names=sp.names[0:info.tablesel-1]
        paths=sp.paths[0:info.tablesel-1]
        usetree=sp.usetree[0:info.tablesel-1]
        searchsubdir=sp.searchsubdir[0:info.tablesel-1]
        if info.tablesel lt n-1 then begin
          names=[names,sp.names[info.tablesel+1:n-1]]
          paths=[paths,sp.paths[info.tablesel+1:n-1]]
          usetree=[usetree,sp.usetree[info.tablesel+1:n-1]]
          searchsubdir=[searchsubdir,sp.searchsubdir[info.tablesel+1:n-1]]
        endif
        sp = {names:names, paths:paths, usetree:usetree, searchsubdir:searchsubdir, defaul:defaul}
        ptr_free, info.sp
        info.sp = ptr_new(sp)
        info.tablesel=info.tablesel-1
        showData = iris_xfiles_editpatternwidget_tablevals(sp, alignments=alignments)
        widget_control, info.ptable, set_value=showData, table_ysize=N_ELEMENTS(sp.names), $
          set_table_select=[0,info.tablesel,0,info.tablesel], alignment=alignments
        widget_control, info.namefield, set_value=sp.names[info.tablesel], sensitive=info.tablesel
        widget_control, info.pathfield, set_value=sp.paths[info.tablesel], sensitive=info.tablesel
        widget_control, info.pathstruct, set_value=[sp.usetree[info.tablesel], sp.searchsubdir[info.tablesel]], sensitive=info.tablesel
        widget_control, info.makedefault, set_value=[info.tablesel eq sp.defaul]
      endif
    end
    
    info.namefield: begin
      widget_control, info.namefield, get_value=name
      sp = *(info).sp
      sp.names[info.tablesel] = name
      *(info).sp = sp
      showData = iris_xfiles_editpatternwidget_tablevals(sp, alignments=alignments)
      widget_control, info.ptable, set_value=showData, alignment=alignments
    end
    
    info.pathfield: begin
      widget_control, info.pathfield, get_value=path
      sp = *(info).sp
      sp.paths[info.tablesel] = path
      *(info).sp = sp
    end
    
    info.pathstruct: begin
      widget_control, info.pathstruct, get_value=struct
      sp = *(info).sp
      sp.usetree[info.tablesel] = struct[0]
      sp.searchsubdir[info.tablesel] = struct[1]
      *(info).sp = sp
      widget_control, info.namefield, get_value=name
      widget_control, info.pathfield, get_value=path
      sp.names[info.tablesel] = name
      sp.paths[info.tablesel] = path
      showData = iris_xfiles_editpatternwidget_tablevals(sp, alignments=alignments)
      widget_control, info.ptable, set_value=showData, alignment=alignments
    end
    
    info.makedefault: begin
      widget_control, info.makedefault, get_value=def
      sp = *(info).sp
      if def then begin
        sp.defaul = info.tablesel
        *(info).sp = sp
      endif else begin
        widget_control, info.makedefault, set_value=[1]
      endelse
      widget_control, info.namefield, get_value=name
      widget_control, info.pathfield, get_value=path
      sp.names[info.tablesel] = name
      sp.paths[info.tablesel] = path
      showData = iris_xfiles_editpatternwidget_tablevals(sp, alignments=alignments)
      widget_control, info.ptable, set_value=showData, alignment=alignments
    end
    
    else:
  endcase
  if ~destroyed then widget_control, event.top, set_Uvalue=info, /No_Copy
end



function iris_xfiles_editpatternwidget, spatterns, mainwindow

  sp=spatterns
  
  base = widget_base(title='IRIS-XFILES SEARCH PATTERNS', group=mainwindow, /modal, /column)
  lab = widget_label(base,value='Edit the search patterns')
  
  base2 = widget_base(base, /row)
  
  base2a = widget_base(base2, /column)
  showData = iris_xfiles_editpatternwidget_tablevals(sp, alignments=alignments, ColumnLabels=ColumnLabels, widths=widths)
  ptable = widget_table(base2a, Column_Labels=ColumnLabels, /No_Row_Headers, $
    Value=showData, Column_Widths=widths, Alignment=alignments, /All_Events, $
    y_scroll_size=20, scr_ysize=300)
  widget_control, ptable, set_table_select=[0,sp.defaul,0,sp.defaul]
  
  base2aa = widget_base(base2a, /row)
  lab = widget_label(base2aa, value='', xsize=40)
  additem = widget_button(base2aa, value='+', scr_xsize=50)
  lab = widget_label(base2aa, value='', xsize=20)
  delitem = widget_button(base2aa, value='-', xsize=50)
  
  base2b = widget_base(base2, /column)
  namefield = CW_FIELD(base2b, title='Name', value=sp.names[sp.defaul], xsize=80, /Return_events)
  pathfield = CW_FIELD(base2b, title='Path', value=sp.paths[sp.defaul], xsize=80, /Return_events)
  pathstruct = CW_BGROUP(base2b, ['Use Date-Tree-Structure (e.g. 2013/06/17/ added to path)', 'Search subdirectories'], $
    /column, /nonexclusive, set_value=[sp.usetree[sp.defaul], sp.searchsubdir[sp.defaul]])
  makedefault = CW_BGROUP(base2b, ['Set this as the default pattern'], $
    /column, /nonexclusive, set_value=[1])
    
  base3 = widget_base(base, /row)
  lab = widget_label(base3, value='', xsize=300)
  cancel = widget_button(base3, value='Cancel', scr_xsize=80)
  lab = widget_label(base3, value='', xsize=100)
  ok = widget_button(base3, value='OK', scr_xsize=80)
  
  if sp.defaul eq 0 then begin
    widget_control, namefield, sensitive=0
    widget_control, pathfield, sensitive=0
    widget_control, pathstruct, sensitive=0
  endif
  
  sptemp = sp
  sptemp.names[0] = 'Cancel'
  result = ptr_new(sptemp)
  
  info = {sp:ptr_new(sp), $
    result:result, $
    ptable:ptable, $
    tablesel:sp.defaul, $
    additem:additem, $
    delitem:delitem, $
    namefield:namefield, $
    pathfield:pathfield, $
    pathstruct:pathstruct, $
    makedefault:makedefault, $
    cancel:cancel, $
    ok:ok}
    
  widget_control, base, set_Uvalue=info, /No_Copy
  widget_control, base, /realize
  xmanager, 'iris_xfiles_editpatternwidget', base, event_handler='iris_xfiles_editpatternwidget_event'
  
  res = *result
  ptr_free, result
  return, res
end