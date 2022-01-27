;+
; Project     : IRIS
;
; Name        : OBJ_HELP
;
; Purpose     : find methods & one-liner description of them, and print them to console
;
; Category    : utility objects
;
; Explanation : checks CLASS name and CLASS__DEFINE procedure and prints out methods which have a comment line after its declaration
;
; Syntax      : IDL>obj_help, class [, methods=methods, infos=infos, description=description, methodsall=methodsall, $
;                 quiet=quiet, err=err]
;
; Inputs      : CLASS = class name or object variable name
;
; Outputs     : prints methods with their one-line explanation to console
;               See keywords
;
; Keywords    : QUIET = inhibit printing
;               METHODS = string array of method calls, which have a comment-line after declaration
;               INFOS = string array of comment-line after declarations
;               DESCRIPTION = string array of comments at beginning of file
;               METHODSALL = string array of all methods in this class
;               ERR = string with error message
;               
; Usage       : This is convenient to include in an object as a method of this object, i.e.:
;               pro MyObject::help, _extra=_extra
;                 ;prints out this help, setting the 'description' keyword will also print the header info
;                 obj_help, self, _extra=_extra
;               end
;               
;               make sure to have one line of comment right after the declaration for public methods,
;               only those will be printed.
;
; History     : Written 31 Jan 2014, M.Wiesmann, ITA, UIO
;               First part is a modified version from obj_dissect
;               and strip_arg has been reused here
;               4. Dezember 2019, Martin Wiesmann, ITA, UIO
;               added check whether 'version' is defined if not, set it to last modified date of the class definition file
;               header info will also be printed out if description keyword is set like this /description.
;
; Contact     : martin.wiesmann@astro.uio.no
;
; $Id: obj_help.pro,v 1.2 2019/12/04 13:59:15 mawiesma Exp $
;
;-

pro obj_help, class, methods=methods, infos=infos, description=description, methodsall=methodsall, $
    quiet=quiet, err=err, _extra=_extra
    
  verbose = ~keyword_set(quiet)
  find_desc = arg_present(description) || keyword_set(description)
  methods=!NULL
  infos=!NULL
  description=!NULL
  methodsall=!NULL
  
  err=''
  if n_elements(class) ne 1 then begin
    err='Input argument must be scalar class name or object reference'
    message,err,/cont
    return
  endif
  
  valid_obj=0b
  if size(class,/tname) eq 'OBJREF' then begin
    valid_obj=obj_valid(class)
    if ~valid_obj then begin
      err='Input object is null'
      message,err,/cont
      return
    endif
    class_name=obj_class(class)
  endif else begin
    if size(class,/tname) ne 'STRING' then begin
      if find_props then $
        pr_syntax,'props=obj_props(class_name)'
      if find_methods then $
        pr_syntax,'methods=obj_methods(class_name [,/super])'
      err='Invalid input'
      return
    endif
    class_name=class
  endelse
  
  ;-- error catch
  
  class_err='"'+class_name+'" is probably not a valid class name'
  error=0
  catch,error
  if error ne 0 then begin
    err=class_err
    message,err,/cont
    return
  endif
  
  
  ;-- extract methods calls
  ;-- look for __define constructor procedure.
  ;   If found, avoid the overhead of creating a temporary object
  
  class_def=strlowcase(trim(class_name))+'__define'
  have_con=have_proc(class_def,out=fname)
  
  if ~have_con or ~valid_obj then begin
    chk=valid_class(class_name)
    if ~chk then begin
      err='Invalid class name - '+class_name
      message,err,/cont
      return
    endif
  endif
  
  if fname eq '' then begin
    def_err='Could not locate "'+class_def+'" constructor'
    err=def_err
    message,err,/cont
    return
  endif
  
  
  proc = get_proc(file_dirname(fname),file_basename(fname))
    
  ;-- strip off comment lines and blanks
  
  tproc=strtrim(proc,2)
  ok=where(tproc ne '',nlines)
  header=1
  if nlines gt 0 then begin
    tproc=tproc(ok)
    for i=0,nlines-1 do begin
      line=strcompress(tproc(i))
      procd=strpos(strupcase(line),'PRO ')
      func=strpos(strupcase(line),'FUNCTION ')
      pfound=((procd eq 0) or (func eq 0))
      if pfound then begin
        header=0
        semi=strpos(line,';')
        if semi gt 0 then line=strmid(line,0,semi)
        out=line
        repeat begin
          doll=strpos(line,'$')
          contin=(doll gt -1)
          if contin then begin
            i=i+1
            line=tproc(i)
            semi=strpos(line,';')
            if semi gt 0 then line=strmid(line,0,semi)
            out=out+' '+line
            out=str_replace(out,'$','')
          endif
        endrep until not contin
        doll=strpos(out,'::')
        if doll gt 0 then begin
          if procd eq 0 then pmeth='PRO ' else pmeth='FUNC'
          out = pmeth+' '+strmid(out,doll+2)
          methodsall=append_arr(methodsall,out,/no_copy)
          i=i+1
          line=tproc(i)
          semi=strpos(line,';')
          if semi eq 0 then begin
            methods=append_arr(methods,out,/no_copy)
            infos=append_arr(infos,strtrim(strmid(line,1),2),/no_copy)
          endif
        endif
      endif else if header then begin
        vers=strpos(strupcase(line),'$ID: ')
        if vers gt 0 then version=strmid(line,vers)
        description=append_arr(description,line,/no_copy)
      endif
    endfor
  endif
  
  if n_elements(version) eq 0 then begin
    mod_date = file_info(fname)
    version='unknown - last modified: ' + systime(0, mod_date.mtime)
  endif

  
  if ~exist(methods) then methods=''
  if ~exist(infos) then infos=''
  if ~exist(methodsall) then methodsall=''
  if ~exist(description) then description=''
  methods = FIX_STRLEN_ARR(methods, max(strlen(methods)))
  
  if verbose then print, 'Class: '+class_name
  if verbose then print, 'file: '+fname
  if verbose then print, 'version: '+version
  
  if find_desc then for i=0,N_ELEMENTS(description)-1 do print,description[i]
  
  for i=0,N_ELEMENTS(methods)-1 do $
    print, methods[i] + ' : ' + infos[i]

end
