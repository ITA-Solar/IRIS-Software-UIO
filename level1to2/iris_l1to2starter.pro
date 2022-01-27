  ; $Id: iris_l1to2starter.pro,v 1.2 2013/11/18 00:37:06 mawiesma Exp $

;level1dirs = '~/iris/output/20130718_002452_4204000003_L1/'
;level1dirs = '/Users/mawiesma/my_idl/syntoolDATA/SIMresults/20130604_141652_3800000010_L1/'
;level1dirs = '~/my_idl/syntoolDATA/SIMresults/aa_missing1/'
level1dirs = '~/iris/data/level1/20131024_025657_3840363643/'

;outputfolders = '~/my_idl/syntoolDATA/L2/'
outputfolders='~/iris/data/level2/20131024_025657_3840363643/'

;xmlparentfolder='/archive/iris/ops/tables/'
xmlparentfolder='~/iris/input/XML/'
;xmlparentfolder='~/iris/input/XML38/'
;sock_list,'http://www.lmsal.com/cgi-ssw/ssw_service_iris.sh?obsid=OBS-4002751935&ssw_service=iris_obstab2depend',out

;;; or ;;;

;level1dir = file_list('~/my_idl/syntoolDATA/SIMresults/', '2013*')
;level1dir = level1dir + '/'
;print,level1dir
;level1dirs = find_all_dir('~/iris/output/20130716_21*L1')
;level1dirs[0]=level1dirs[0]+'/'
;level1dir=level1dir[0:5]

;xmlfiles = '/Users/mawiesma/my_idl/syntoolDATA/XMLl12tests/'
;xmlfiles = '/Users/mawiesma/my_idl/syntoolDATA/SIMresults/logs00000/'

;print,level1dir
;print,xmlfiles

;correct first level1dir, seems that there is always the directory marker missing
;level1dir[0]=level1dir[0]+'/'
;print,level1dir

OBSid=''
scaled=0
SJIonly=0
spectralonly=0
preserveNAN=0
preserveINF=0
finalstatus=1
rollangle=!NULL  ;set to !NULL, if you don't want to overwrite the SAT_ROT value in the level 1(.5) files
outdir=xmlparentfolder

IRISl12_multiOBS, level1dirs, outputfolders, OBSid=OBSid, l1to2log=l1to2log, $
  scaled=scaled, maxdeviation=maxdeviation, xmlparentfolder=xmlparentfolder, outdir=outdir, $
  SJIonly=SJIonly, spectralonly=spectralonly, preserveINF=preserveINF, preserveNAN=preserveNAN, $
  maxl1files=maxl1files, finalstatus=finalstatus, rollangle=rollangle, $
  debug=debug, _extra=_extra
