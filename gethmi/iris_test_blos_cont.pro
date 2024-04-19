pro iris_test_blos_cont

  test = 2

  case test of
    1: begin
      obs = '20230315_044803_3660259102'
      outdir='/Users/mawiesma/data/test/blos/'
      jobid='ssw_service_230321_081413_24541_ssmq_blos_cont_test_nodl'
    end
    2: begin
      obs = '20240306_130020_3660259103'
      outdir='/Users/mawiesma/data/test/blos_2/'
      jobid='ssw_service_240312_081123_1737_ssmq_blos_cont_TEST_1p5'
    end
  endcase
  waves = ['blos','cont']


  IF 1 THEN BEGIN
    ;run this part only once
    ssw_service_get_data,jobid,out_dir=outdir,/loud
  ENDIF

  fovexpanddefault = 100.0 ;arcseconds ;;;;;;;;;;;;;;;;;DEFAULT
  timeexpanddefault = 10.0 ;minutes ;;;;;;;;;;;;;;;;;DEFAULT
  maxframesdefault = 50    ;;;;;;;;;;;;DEFAULT
  maxframes = maxframesdefault

  obs2fov = obj_new('IRIS_obs2fov', obs, usehcr=usehcr)
  if obs2fov->get_error() then begin
    print, 'Invalid OBS'
    return
  endif
  if N_ELEMENTS(fovexpand) eq 0 then obs2fov->set_fovexpand, fovexpanddefault $
  else obs2fov->set_fovexpand, fovexpand
  if N_ELEMENTS(timeexpand) eq 0 then obs2fov->set_timeexpand, timeexpanddefault $
  else obs2fov->set_timeexpand, timeexpand


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
