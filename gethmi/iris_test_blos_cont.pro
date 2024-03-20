pro iris_test_blos_cont

  obs = '20230315_044803_3660259102'
  outdir='/Users/mawiesma/data/test/blos/'
  jobid='ssw_service_230321_081413_24541_ssmq_blos_cont_test_nodl'
  waves = ['blos','cont']

  IF 0 THEN BEGIN
    ssw_service_get_data,jobid,out_dir=outdir,/loud    
  ENDIF

  fovexpanddefault = 100.0 ;arcseconds ;;;;;;;;;;;;;;;;;DEFAULT
  timeexpanddefault = 10.0 ;minutes ;;;;;;;;;;;;;;;;;DEFAULT
  wavesdefault = ['94','131','171','193','211','304','335','1600','1700','4500'] ;;;;;;;;;;;;DEFAULT
  maxframesdefault = 50    ;;;;;;;;;;;;DEFAULT
  maxframes = maxframesdefault

  constants = obj_new('IRISsim_constants')

  t1=systime(1)

  obs2fov = obj_new('IRIS_obs2fov', obs, usehcr=usehcr)
  if obs2fov->get_error() then begin
    print, 'Invalid OBS'
    return
  endif


  obs2fov->get_startendtimes, starttime, endtime
  datarequested=1
  downloadfinished=1
  l2created=0
  query_source = 'iris_test_blos_cont'
  email = ''
  instrument = ''
  deletetempfiles = 0
  tres = 10
  method = 0

  filesfound = ptrarr(N_ELEMENTS(waves))
  nfilesl1 = lonarr(N_ELEMENTS(waves))
  aiafilesl1 = ptrarr(N_ELEMENTS(waves))

print,outdir+'cutout_params.sav'

  save, starttime, endtime, waves, obs2fov, $
    maxframes, query_source, email, instrument, _extra, deletetempfiles, $
    jobID, datarequested, downloadfinished, l2created, $
    tres, method, aiafilesl1, nfilesl1, filesfound, $
    filename=outdir+'cutout_params.sav'


IRIS_processAIArequest, outdir=outdir
end
