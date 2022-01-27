;+
; NAME:
;       IRIS_obs2fov
;
; PURPOSE:
;
;       IRIS_obs2fov returns the x/ycen, fovx/y and the start and end time for a specific IRIS OBS
;         the results are to be used to create a cutout from another source, speficially for the AIA data
;         and its results can be used to call ssw_cutout_service
;
; CATEGORY:
;       Martin Wiesmann / IRIS Data processing
;
; CALLING SEQUENCE:
;       IRIS_obs2fov, obs [, xycen, fovxy, startendtime, $
;         fovexpand=fovexpand, timeexpand=timeexpand]
;
; INPUTS:
;       obs: where obs is the OBS ID of the IRIS OBS. It can be given in 3 different styles:
;         1) Just the OBS ID (e.g. '20130829_005500_4203300028')
;         2) The directory in which the IRIS files reside (e.g. '/mn/xsan/d2/iris/data/level2/2014/01/01/20140101_000431_3840257196/')
;         3) The full path of one of the IRIS files, which you can print out from iris_xfiles (e.g. '/mn/xsan/d2/iris/data/level2/2014/01/01/20140101_000431_3840257196/iris_l2_20140101_000431_3840257196_SJI_1400_t000.fits')
;
;
; OPTIONAL KEYWORD PARAMETERS:
;       fovexpand: defines by how much the FOV should be expanded, as compared to IRIS FOV, in arcsecond, can be scalar,
;         then the same expansion will be applied to both the x- and y-axis. It can also be a 2-element vector, one element for
;         x- and one for y-axis, respectively.
;         (default: 100.0 arcsec)
;       timeexpand: defines by how much the time window of the IRIS OBS is expanded for AIA data, in minutes. If scalar,
;         the value will be applied to the start and the end time. Can be also a 2-element vector, for different expansions
;         at the start and at the end, respectively. (Positive numbers result in a wider time-window)
;         (default: 10.0 minutes)
;
; OUTPUTS:
;       xycen: a two-element vector, containing [XCEN, YCEN] (float)
;       fovxy: a two-element vector, containing [FOVX, FOVY] (float)
;       startendtime: a two-element vector, containing [starttime, endtime] (string)
;
; CALLS:
;
; COMMON BLOCKS:
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;       2016-04-21: Martin Wiesmann (ITA, UIO).
;
; $Id: iris_obs2fov.pro,v 1.9 2016/04/21 12:45:11 mawiesma Exp $  ;


pro IRIS_obs2fov, obs, xycen, fovxy, startendtime, $
  fovexpand=fovexpand, timeexpand=timeexpand
  
  safeadd = 50
  
  obs2fov = obj_new('IRIS_obs2fov', obs, usehcr=usehcr)
  if obs2fov->get_error() then begin
    print, 'Invalid OBS'
    return
  endif

  fovexpanddefault = 100.0 ;arcseconds ;;;;;;;;;;;;;;;;;DEFAULT
  timeexpanddefault = 10.0 ;minutes ;;;;;;;;;;;;;;;;;DEFAULT
  if N_ELEMENTS(fovexpand) eq 0 then obs2fov->set_fovexpand, fovexpanddefault $
  else obs2fov->set_fovexpand, fovexpand
  if N_ELEMENTS(timeexpand) eq 0 then obs2fov->set_timeexpand, timeexpanddefault $
  else obs2fov->set_timeexpand, timeexpand

  xycen = [obs2fov->get_xcen(), obs2fov->get_ycen()]
  fovxy = [obs2fov->get_fovx(/expsatrot)+safeadd, obs2fov->get_fovy(/expsatrot)+safeadd]
  obs2fov->get_startendtimes, starttime, endtime
  startendtime = [starttime, endtime]

end
