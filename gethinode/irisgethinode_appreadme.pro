function IRISgetHINODE_appReadme
  ;This function initializes some readme-files for the user, which are saved in the hidden .idl folder in the home
  ;directory of the user
  ;It returns the path to that directory, which is used to save the default parameters for the IRIS_getHINODEdata procedure
  ;
  ; $Id: irisgethinode_appreadme.pro,v 1.2 2015/12/01 13:15:06 mawiesma Exp $  ;

  AuthorDirname = 'mwiesmann'
  AuthorDesc = 'Martin Wiesmann, Institute of Theoretical Astrophysics, University of Oslo'
  AppDirname = 'IRIS_getHINDOEdata'
  AppDesc = 'IRIS Get HINODE data'
  AppReadmeText = 'IRIS Get HINODE data, written by Martin Wiesmann, ITA, University of Oslo (martin.wiesmann@astro.uio.no)' + $
    '/n these files can be deleted without restrictions.'
  AppReadmeVersion=1
  return, APP_USER_DIR(AuthorDirname, AuthorDesc, AppDirname, AppDesc, AppReadmeText, AppReadmeVersion)
end