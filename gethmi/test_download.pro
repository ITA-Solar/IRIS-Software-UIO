file = '/mn/stornext/d10/PRITS/iris/data/level2/2023/03/15/20230315_042004_3882010194/iris_l2_20230315_042004_3882010194_raster_t000_r00000.fits'

iris_getaiadata, file

jobid = 'ssw_service_240313_060532_6344'
destination = '/Users/mawiesma/data/test/20230315_042004_3882010194/manual/' + jobid
ssw_service_get_data, jobID, out_dir=destination, /loud



;old

;jobid = 'ssw_service_240312_070214_42933'
;destination = '/Users/mawiesma/data/test/manual/' + jobid
;ssw_service_get_data, jobID, out_dir=destination, /loud

