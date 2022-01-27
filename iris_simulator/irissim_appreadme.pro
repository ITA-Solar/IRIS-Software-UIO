function IRISsim_appReadme
;This function initializes some readme-files for the user, which are saved in the hidden .idl folder in the home 
;directory of the user
;It returns the path to that directory, which is used to save the default parameters for IRIS simulator
; $Id: irissim_appreadme.pro,v 1.2 2013/04/25 20:19:46 mawiesma Exp $  ;

    AuthorDirname = 'mwiesmann'
    AuthorDesc = 'Martin Wiesmann, Institute of Theoretical Astrophysics, University of Oslo'
    AppDirname = 'iris_simulator'
    AppDesc = 'IRIS Simulator'
    AppReadmeText = 'IRIS Simulator, written by Martin Wiesmann, ITA, University of Oslo (martin.wiesmann@astro.uio.no)' + $
      '/n these files can be deleted without restrictions.'
    AppReadmeVersion=1
    return, APP_USER_DIR(AuthorDirname, AuthorDesc, AppDirname, AppDesc, AppReadmeText, AppReadmeVersion)
end