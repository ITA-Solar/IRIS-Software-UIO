pro iris_test_blos_cont

  outdir='/Users/mawiesma/data/test/blos'
  jobid='ssw_service_230321_081413_24541_ssmq_blos_cont_test_nodl'
  waves = ['blos','cont']

  IF 0 THEN BEGIN
    ssw_service_get_data,jobid,out_dir=outdir,/loud    
  ENDIF

  save, starttime, endtime, waves, obs2fov, $
    maxframes, query_source, email, instrument, _extra, deletetempfiles, $
    jobID, datarequested, downloadfinished, l2created, $
    tres, method, aiafilesl1, nfilesl1, filesfound, $
    filename=outdir+'cutout_params.sav'


end
