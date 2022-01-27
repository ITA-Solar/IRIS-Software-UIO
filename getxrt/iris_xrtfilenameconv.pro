;+
; NAME:
;       IRIS_xrtFileNameConv
;
; PURPOSE:
;       IRIS_xrtFileNameConv converts filenames given by xrt_cat to point to level 1 files on SAO server
;
; CATEGORY:
;       Martin Wiesmann / IRIS Data processing
;       IRIS_getXRTdata
;
; CALLING SEQUENCE:
;       IRIS_xrtFileNameConv, files
;
; INPUTS:
;       files: a list of filenames
;
; OPTIONAL KEYWORD PARAMETERS:
;
; OUTPUTS:
;       a list of converted filenames
;
; CALLS:
;       files = IRIS_xrtFileNameConv(files)
;
; COMMON BLOCKS:
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;       15-Feb-2019: Martin Wiesmann (ITA, UIO)
;
; $Id: iris_xrtfilenameconv.pro,v 1.1 2019/02/15 13:35:45 mawiesma Exp $  ;



function IRIS_xrtFileNameConv, files

  dirPrefix = 'http://sao.virtualsolar.org/VSO/DataProvider/SAO/hinode/xrt/level1/'
  filePrefix = 'L1_'

  for i=0,N_ELEMENTS(files)-1 do begin
    filename = filePrefix + file_basename(files[i])
    path = file_dirname(files[i])
    path = strsplit(path, path_sep(), /extract)
    path = dirPrefix + strjoin(path[N_ELEMENTS(path)-4:N_ELEMENTS(path)-1], path_sep())
    newfile = path + path_sep() + filename
    if i eq 0 then newfiles = newfile $
      else newfiles = [newfiles, newfile]
  endfor

  return, newfiles
end
