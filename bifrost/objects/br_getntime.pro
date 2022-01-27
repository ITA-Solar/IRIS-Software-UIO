;+
; NAME:
;	BR_GETNTIME
;
;
; PURPOSE:
;	Get number of snap files for given idlparam root
;
;
; CATEGORY:
;	Oslo Stagger Code
;
;
; CALLING SEQUENCE:
;	BR_GETNTIME,idlparam,ntime
;
;
; INPUTS:
;	idlparam - root name of snap files
;
;
; OUTPUTS:
;	ntime - number of snap files
;
;
; RESTRICTIONS:
;	will not include the isnap=0 file (idlparam.idl file)
;
;
; PROCEDURE:
;	uses file_search
;
;
; MODIFICATION HISTORY:
;v1.0	14-Aug-2008 Mats Carlsson
;-
pro br_getntime,idlparam,ntime

if(n_params() lt 2) then begin
  message,'syntax: br_getntime,idlparam,ntime',/info
  return
endif

f=file_search(idlparam+'[0-9][0-9][0-9].idl',count=ntime)

end
